/* Rumprun x86-64 test peer. COM1 is the console; COM2 is the broker wire.
 * This deliberately privileged guest is not an authorization boundary.
 * The host owns deadlines and must kill guests which wait or flood forever.
 */
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#if !defined(__x86_64__)
#error "This probe requires an x86-64 guest with port I/O privileges"
#endif

enum {
    UART = 0x2f8,
    HEADER_SIZE = 8,
    MAX_DATA = 4096,
    READ = 1,
    WRITE = 2,
    FINISH = 3,
    OK = 0,
    DENIED = 1,
    STALE = 2,
    TOOLARGE = 3,
    MALFORMED = 4
};

static uint8_t request[4 + HEADER_SIZE + MAX_DATA];
static uint8_t response[HEADER_SIZE + MAX_DATA];

static uint8_t
port_read(uint16_t port)
{
    uint8_t value;
    __asm__ volatile ("inb %w1, %0" : "=a" (value) : "Nd" (port));
    return value;
}

static void
port_write(uint16_t port, uint8_t value)
{
    __asm__ volatile ("outb %0, %w1" : : "a" (value), "Nd" (port));
}

static void
uart_initialize(void)
{
    port_write(UART + 1, 0);       /* Disable interrupts. */
    port_write(UART + 3, 0x80);    /* Divisor latch. */
    port_write(UART, 1);          /* 115200 baud. */
    port_write(UART + 1, 0);
    port_write(UART + 3, 0x03);    /* Eight data bits, no parity, one stop bit. */
    port_write(UART + 2, 0x07);    /* Enable and clear FIFOs, one-byte trigger. */
    port_write(UART + 4, 0x03);    /* DTR and RTS. */
}

static void
uart_send(const uint8_t *bytes, size_t length)
{
    for (size_t i = 0; i < length; ++i) {
        while (!(port_read(UART + 5) & 0x20))
            __asm__ volatile ("pause");
        port_write(UART, bytes[i]);
    }
    /* Drain the transmitter before returning, including truncated probes. */
    while (!(port_read(UART + 5) & 0x40))
        __asm__ volatile ("pause");
}

static int
uart_receive(uint8_t *bytes, size_t length)
{
    for (size_t i = 0; i < length; ++i) {
        uint8_t status;
        do {
            status = port_read(UART + 5);
            if (status & 0x1e) {
                fprintf(stderr, "artifact-guest: UART receive error 0x%02x\n",
                        (unsigned)status);
                return 0;
            }
            __asm__ volatile ("pause");
        } while (!(status & 1));
        bytes[i] = port_read(UART);
    }
    return 1;
}

static void
put_u16(uint8_t *bytes, uint16_t value)
{
    bytes[0] = (uint8_t)(value >> 8);
    bytes[1] = (uint8_t)value;
}

static void
put_u32(uint8_t *bytes, uint32_t value)
{
    bytes[0] = (uint8_t)(value >> 24);
    bytes[1] = (uint8_t)(value >> 16);
    bytes[2] = (uint8_t)(value >> 8);
    bytes[3] = (uint8_t)value;
}

static uint16_t
get_u16(const uint8_t *bytes)
{
    return (uint16_t)(((uint16_t)bytes[0] << 8) | bytes[1]);
}

static uint32_t
get_u32(const uint8_t *bytes)
{
    return ((uint32_t)bytes[0] << 24) | ((uint32_t)bytes[1] << 16) |
           ((uint32_t)bytes[2] << 8) | bytes[3];
}

/* Check the complete response, including empty-data and revision invariants. */
static int
exchange(const char *label, uint8_t opcode, uint16_t slot, uint32_t revision,
         const void *data, size_t length, uint8_t expected_status,
         uint32_t expected_revision, const void *expected_data,
         size_t expected_length)
{
    uint8_t prefix[4];
    uint32_t payload_length;

    if (length > MAX_DATA || expected_length > MAX_DATA) {
        fprintf(stderr, "artifact-guest: invalid local test length: %s\n", label);
        return 0;
    }
    put_u32(request, (uint32_t)(HEADER_SIZE + length));
    request[4] = 1;
    request[5] = opcode;
    put_u16(request + 6, slot);
    put_u32(request + 8, revision);
    if (length)
        memcpy(request + 12, data, length);
    uart_send(request, 12 + length);

    if (!uart_receive(prefix, sizeof(prefix)))
        return 0;
    payload_length = get_u32(prefix);
    if (payload_length < HEADER_SIZE || payload_length > sizeof(response)) {
        fprintf(stderr, "artifact-guest: %s: invalid response length %lu\n",
                label, (unsigned long)payload_length);
        return 0;
    }
    if (!uart_receive(response, payload_length))
        return 0;
    if (response[0] != 1 || response[1] != expected_status ||
        get_u16(response + 2) != slot ||
        get_u32(response + 4) != expected_revision ||
        payload_length != HEADER_SIZE + expected_length ||
        (expected_length &&
         memcmp(response + HEADER_SIZE, expected_data, expected_length))) {
        fprintf(stderr,
                "artifact-guest: %s: unexpected response "
                "version=%u status=%u slot=%u revision=%lu length=%lu\n",
                label, (unsigned)response[0], (unsigned)response[1],
                (unsigned)get_u16(response + 2),
                (unsigned long)get_u32(response + 4),
                (unsigned long)payload_length);
        return 0;
    }
    return 1;
}

static int
write_output(void)
{
    static const char output[] = "approved output";
    return exchange("write output", WRITE, 2, 0, output, sizeof(output) - 1,
                    OK, 1, NULL, 0);
}

static int
normal(void)
{
    static const char input[] = "input snapshot";
    static const char oversized[] = "123456789012345678901234567890123";

    if (!exchange("read input", READ, 1, 0, NULL, 0,
                  OK, 1, input, sizeof(input) - 1) ||
        !exchange("write input denied", WRITE, 1, 1, "x", 1,
                  DENIED, 0, NULL, 0) ||
        !exchange("unknown read denied", READ, UINT16_MAX, 0, NULL, 0,
                  DENIED, 0, NULL, 0) ||
        !exchange("unknown write denied", WRITE, UINT16_MAX, 0, "x", 1,
                  DENIED, 0, NULL, 0) ||
        !exchange("output read denied", READ, 2, 0, NULL, 0,
                  DENIED, 0, NULL, 0) ||
        !exchange("unknown opcode", 255, 2, 0, NULL, 0,
                  MALFORMED, 0, NULL, 0) ||
        !exchange("output too large", WRITE, 2, 0,
                  oversized, sizeof(oversized) - 1, TOOLARGE, 0, NULL, 0) ||
        !write_output() ||
        !exchange("stale output", WRITE, 2, 0, "replacement", 11,
                  STALE, 1, NULL, 0) ||
        !exchange("finish", FINISH, 0, 0, NULL, 0, OK, 0, NULL, 0))
        return 1;
    puts("artifact-guest: normal passed");
    return 0;
}

int
main(int argc, char **argv)
{
    const char *mode = argc == 1 ? "normal" : argv[1];

    if (argc > 2) {
        fprintf(stderr, "usage: artifact-guest [normal|oversize|truncated|"
                        "flood|stall|rollback]\n");
        return 1;
    }
    uart_initialize();
    if (!strcmp(mode, "normal"))
        return normal();
    if (!strcmp(mode, "oversize")) {
        put_u32(request, HEADER_SIZE + MAX_DATA + 1);
        uart_send(request, 4);
        return 0;
    }
    if (!strcmp(mode, "truncated")) {
        put_u32(request, HEADER_SIZE);
        request[4] = 1;
        request[5] = READ;
        request[6] = 0;
        uart_send(request, 7);
        return 0;
    }
    if (!strcmp(mode, "flood")) {
        for (;;)
            if (!exchange("flood", READ, UINT16_MAX, 0, NULL, 0,
                          DENIED, 0, NULL, 0))
                return 1;
    }
    if (!strcmp(mode, "stall")) {
        for (;;)
            (void)port_read(UART + 5);
    }
    if (!strcmp(mode, "rollback"))
        return write_output() ? 0 : 1;
    fprintf(stderr, "artifact-guest: unknown mode: %s\n", mode);
    return 1;
}
