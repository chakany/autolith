/* Process calls for the single-process rumprun guest.
 *
 * Rumprun's stubs for fork, kill, and waitpid return ENOTSUP as a positive
 * result, which its libc_stubs.c itself marks as an incorrect return value:
 * fork appears to create child 86. The final unikernel link wraps each stub,
 * so every caller, C or Lisp, sees POSIX behavior instead.
 *
 * The guest cannot create a process itself. When the host passes
 * RUMPRUN_BROKER, ADDRESS:PORT:TOKEN in the guest's JSON "env", SBCL's spawn
 * asks the host broker to run the program instead, over one TCP connection
 * per process, and relays the process's standard streams. The remote process
 * has a guest pid that waitpid reaps and kill signals, and its exit raises
 * SIGCHLD, as a child's would. The broker decides what may run and how.
 *
 * Each frame, in either direction, is a 32-bit big-endian length that counts
 * a type octet and a payload following it. Payloads use XDR encoding. Signal
 * numbers travel in NetBSD's numbering; the broker translates them. */
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include "sbcl-machine.h"

#define FRAME_SPAWN 1
#define FRAME_STARTED 2
#define FRAME_FAILED 3
#define FRAME_STDIN 4
#define FRAME_STDIN_EOF 5
#define FRAME_STDOUT 6
#define FRAME_STDERR 7
#define FRAME_STDOUT_EOF 8
#define FRAME_STDERR_EOF 9
#define FRAME_SIGNAL 10
#define FRAME_EXIT 11
#define FRAME_LIMIT (1u << 20)
#define DATA_LIMIT 65536
#define REMOTE_LIMIT 64
#define REMOTE_PID_BASE 100000

/* FAILED's first word: why the broker ran nothing. */
#define FAILURE_REFUSED 1
#define FAILURE_MISSING 2

/* EXIT's first word: how the process ended. */
#define EXIT_CODE 0
#define EXIT_SIGNAL 1

/* The guest cannot fork. */
pid_t __wrap___fork(void)
{
    errno = ENOTSUP;
    return -1;
}

pid_t __wrap___vfork14(void)
{
    errno = ENOTSUP;
    return -1;
}


/* -- Remote Process Table -- */

struct remote {
    int used;
    pid_t pid;
    int socket;
    int references;          /* relay threads that still use the socket */
    int input, output, error; /* guest descriptors relayed, or -1 */
    int exited;
    int status;              /* a NetBSD wait status once exited */
    pthread_mutex_t write_lock;
};

static struct remote remotes[REMOTE_LIMIT];
static pthread_mutex_t remotes_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t remotes_changed = PTHREAD_COND_INITIALIZER;
static pid_t next_remote_pid = REMOTE_PID_BASE;

/* Return the used entry for PID, with remotes_lock held, or NULL. */
static struct remote *remote_find(pid_t pid)
{
    for (int i = 0; i < REMOTE_LIMIT; ++i)
        if (remotes[i].used && remotes[i].pid == pid) return &remotes[i];
    return NULL;
}

/* Claim a table entry for a process relaying SOCKET, or return NULL. */
static struct remote *remote_claim(int socket)
{
    pthread_mutex_lock(&remotes_lock);
    struct remote *remote = NULL;
    for (int i = 0; !remote && i < REMOTE_LIMIT; ++i)
        if (!remotes[i].used) remote = &remotes[i];
    if (remote) {
        remote->used = 1;
        remote->pid = next_remote_pid++;
        remote->socket = socket;
        remote->references = 1;
        remote->input = remote->output = remote->error = -1;
        remote->exited = 0;
        remote->status = 0;
        pthread_mutex_init(&remote->write_lock, NULL);
    }
    pthread_mutex_unlock(&remotes_lock);
    return remote;
}

/* Drop one reference to REMOTE's socket, closing it after the last, when
 * waitpid may reap the process. */
static void remote_release(struct remote *remote)
{
    pthread_mutex_lock(&remotes_lock);
    if (--remote->references == 0 && remote->socket >= 0) {
        close(remote->socket);
        remote->socket = -1;
    }
    pthread_cond_broadcast(&remotes_changed);
    pthread_mutex_unlock(&remotes_lock);
}

/* Record REMOTE's exit with wait STATUS, wake waiters, and signal the guest. */
static void remote_exited(struct remote *remote, int status)
{
    pthread_mutex_lock(&remotes_lock);
    int first = !remote->exited;
    if (first) {
        remote->exited = 1;
        remote->status = status;
    }
    pthread_cond_broadcast(&remotes_changed);
    pthread_mutex_unlock(&remotes_lock);
    if (first) rumprun_raise_process_signal(SIGCHLD);
}


/* -- Frames -- */

struct buffer { unsigned char *data; size_t length, capacity; };

static int buffer_reserve(struct buffer *buffer, size_t more)
{
    if (buffer->length + more <= buffer->capacity) return 0;
    size_t capacity = buffer->capacity ? buffer->capacity : 256;
    while (capacity < buffer->length + more) capacity *= 2;
    unsigned char *grown = realloc(buffer->data, capacity);
    if (!grown) return -1;
    buffer->data = grown;
    buffer->capacity = capacity;
    return 0;
}

static int put_word(struct buffer *buffer, unsigned value)
{
    if (buffer_reserve(buffer, 4)) return -1;
    unsigned char *at = buffer->data + buffer->length;
    at[0] = value >> 24; at[1] = value >> 16; at[2] = value >> 8; at[3] = value;
    buffer->length += 4;
    return 0;
}

/* Append an XDR string: its length, its octets, and padding to four. */
static int put_string(struct buffer *buffer, const char *string)
{
    size_t length = strlen(string), padded = (length + 3) & ~(size_t)3;
    if (put_word(buffer, (unsigned)length) || buffer_reserve(buffer, padded)) return -1;
    memcpy(buffer->data + buffer->length, string, length);
    memset(buffer->data + buffer->length + length, 0, padded - length);
    buffer->length += padded;
    return 0;
}

/* Append a counted array of the NULL-terminated STRINGS. */
static int put_strings(struct buffer *buffer, char *const *strings)
{
    unsigned count = 0;
    while (strings && strings[count]) ++count;
    if (put_word(buffer, count)) return -1;
    for (unsigned i = 0; i < count; ++i)
        if (put_string(buffer, strings[i])) return -1;
    return 0;
}

static unsigned get_word(const unsigned char *data)
{
    return (unsigned)data[0] << 24 | (unsigned)data[1] << 16 | (unsigned)data[2] << 8 | data[3];
}

static int write_fully(int descriptor, const void *data, size_t length)
{
    const unsigned char *at = data;
    while (length) {
        ssize_t count = write(descriptor, at, length);
        if (count < 0) { if (errno == EINTR) continue; return -1; }
        at += count;
        length -= count;
    }
    return 0;
}

static int read_fully(int descriptor, void *data, size_t length)
{
    unsigned char *at = data;
    while (length) {
        ssize_t count = read(descriptor, at, length);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) { if (!count) errno = ECONNRESET; return -1; }
        at += count;
        length -= count;
    }
    return 0;
}

/* Send a frame of TYPE with LENGTH octets of PAYLOAD on REMOTE's socket. */
static int send_frame(struct remote *remote, int type, const void *payload, size_t length)
{
    unsigned total = (unsigned)length + 1;
    unsigned char header[5] = { total >> 24, total >> 16, total >> 8, total, (unsigned char)type };
    pthread_mutex_lock(&remote->write_lock);
    int failed = remote->socket < 0 || write_fully(remote->socket, header, 5)
                 || (length && write_fully(remote->socket, payload, length));
    pthread_mutex_unlock(&remote->write_lock);
    return failed ? -1 : 0;
}

/* Read one frame from SOCKET. Return its type and set *PAYLOAD to a new
 * allocation of *LENGTH octets, or return -1 at a closed or malformed
 * stream. */
static int receive_frame(int socket, unsigned char **payload, size_t *length)
{
    unsigned char header[5];
    if (read_fully(socket, header, 4)) return -1;
    unsigned total = get_word(header);
    if (!total || total > FRAME_LIMIT || read_fully(socket, header + 4, 1)) return -1;
    unsigned char *data = malloc(total > 1 ? total - 1 : 1);
    if (!data || (total > 1 && read_fully(socket, data, total - 1))) { free(data); return -1; }
    *payload = data;
    *length = total - 1;
    return header[4];
}


/* -- Relays -- */

/* Pump the remote process's standard input from its guest descriptor. The
 * guest's own standard input, the console, is never read. */
static void *relay_input(void *argument)
{
    struct remote *remote = argument;
    unsigned char *data = remote->input > 0 ? malloc(DATA_LIMIT) : NULL;
    if (data) {
        for (ssize_t count; (count = read(remote->input, data, DATA_LIMIT)) != 0; ) {
            if (count < 0) { if (errno == EINTR) continue; break; }
            if (send_frame(remote, FRAME_STDIN, data, count)) break;
        }
        free(data);
    }
    if (remote->input >= 0) close(remote->input);
    send_frame(remote, FRAME_STDIN_EOF, NULL, 0);
    remote_release(remote);
    return NULL;
}

/* Write the remote process's output to its guest descriptors, and record
 * its exit. A connection lost before EXIT counts as the process being
 * killed. */
static void *relay_output(void *argument)
{
    struct remote *remote = argument;
    int status = SIGKILL;
    for (;;) {
        unsigned char *payload;
        size_t length;
        int type = receive_frame(remote->socket, &payload, &length);
        if (type < 0) break;
        int done = 0;
        switch (type) {
        case FRAME_STDOUT:
            if (remote->output >= 0 && write_fully(remote->output, payload, length)) {
                close(remote->output);
                remote->output = -1;
            }
            break;
        case FRAME_STDERR:
            if (remote->error >= 0 && write_fully(remote->error, payload, length)) {
                close(remote->error);
                remote->error = -1;
            }
            break;
        case FRAME_STDOUT_EOF:
            if (remote->output >= 0) { close(remote->output); remote->output = -1; }
            break;
        case FRAME_STDERR_EOF:
            if (remote->error >= 0) { close(remote->error); remote->error = -1; }
            break;
        case FRAME_EXIT:
            if (length == 8) {
                unsigned how = get_word(payload), code = get_word(payload + 4);
                status = how == EXIT_SIGNAL ? (int)(code & 0x7f) : (int)((code & 0xff) << 8);
            }
            done = 1;
            break;
        default:
            done = 1;
            break;
        }
        free(payload);
        if (done) break;
    }
    if (remote->output >= 0) { close(remote->output); remote->output = -1; }
    if (remote->error >= 0) { close(remote->error); remote->error = -1; }
    remote_exited(remote, status);
    remote_release(remote);
    return NULL;
}


/* -- Spawning -- */

/* Return the broker's address and copy its token into TOKEN, or return -1
 * when the guest has no well-formed RUMPRUN_BROKER. */
static int broker_address(struct sockaddr_in *address, char *token, size_t size)
{
    const char *broker = getenv("RUMPRUN_BROKER");
    char copy[256];
    if (!broker || strlen(broker) >= sizeof(copy)) return -1;
    strcpy(copy, broker);
    char *port = strchr(copy, ':');
    char *secret = port ? strchr(port + 1, ':') : NULL;
    if (!secret) return -1;
    *port++ = 0;
    *secret++ = 0;
    char *end;
    long number = strtol(port, &end, 10);
    memset(address, 0, sizeof(*address));
    address->sin_len = sizeof(*address);
    address->sin_family = AF_INET;
    address->sin_port = htons((unsigned short)number);
    if (*end || number <= 0 || number > 65535 || inet_pton(AF_INET, copy, &address->sin_addr) != 1
        || !*secret || strlen(secret) >= size)
        return -1;
    strcpy(token, secret);
    return 0;
}

/* Encode the SPAWN request, which names the process by the broker TOKEN,
 * PROGRAM with ARGUMENTS and ENVIRONMENT, whether to SEARCH the path, and
 * the working DIRECTORY, empty for the broker's choice. */
static int encode_spawn(struct buffer *buffer, const char *token, const char *program,
                        char *const *arguments, char *const *environment, int search,
                        const char *directory)
{
    return put_string(buffer, token) || put_string(buffer, program)
           || put_strings(buffer, arguments) || put_strings(buffer, environment)
           || put_word(buffer, search ? 1 : 0) || put_string(buffer, directory ? directory : "");
}

/* SBCL's child exits with this status when its exec fails. */
#define EXEC_FAILURE_STATUS 2

/* Report a failed start through SBCL's exec status pipe, as a child whose
 * exec failed would, with a pid that waitpid reaps with SBCL's exec
 * failure status. */
static int report_failure(struct remote *remote, int channel[2], int error)
{
    remote_exited(remote, EXEC_FAILURE_STATUS << 8);
    remote_release(remote);
    if (channel && channel[1] >= 0) write_fully(channel[1], &error, sizeof(error));
    return remote->pid;
}

extern int __real_spawn(char *, char *[], int, int, int, int, char *[], char *, int[2], char *, int *);

/* SBCL's spawn, which run-program calls with the child's standard streams
 * SIN, SOUT, and SERR and the exec status CHANNEL. */
int __wrap_spawn(char *program, char *argv[], int sin, int sout, int serr, int search,
                 char *envp[], char *pty_name, int channel[2], char *pwd, int *dont_close)
{
    struct sockaddr_in address;
    char token[128];
    if (broker_address(&address, token, sizeof(token)))
        return __real_spawn(program, argv, sin, sout, serr, search, envp, pty_name,
                            channel, pwd, dont_close);
    if (pty_name) { errno = ENOTSUP; return -1; }
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) return -1;
    if (connect(sock, (struct sockaddr *)&address, sizeof(address))) {
        int saved = errno;
        close(sock);
        errno = saved;
        return -1;
    }
    struct remote *remote = remote_claim(sock);
    if (!remote) { close(sock); errno = EAGAIN; return -1; }
    struct buffer request = { NULL, 0, 0 };
    int failed = encode_spawn(&request, token, program, argv, envp, search, pwd)
                 || send_frame(remote, FRAME_SPAWN, request.data, request.length);
    free(request.data);
    unsigned char *payload = NULL;
    size_t length = 0;
    int type = failed ? -1 : receive_frame(sock, &payload, &length);
    if (type == FRAME_FAILED) {
        int why = length >= 4 ? (int)get_word(payload) : 0;
        free(payload);
        return report_failure(remote, channel, why == FAILURE_MISSING ? ENOENT
                                               : why == FAILURE_REFUSED ? EACCES : EIO);
    }
    free(payload);
    if (type != FRAME_STARTED) return report_failure(remote, channel, EIO);
    remote->input = sin > 0 ? dup(sin) : -1;
    remote->output = sout >= 0 ? dup(sout) : -1;
    remote->error = serr >= 0 ? dup(serr) : -1;
    pthread_mutex_lock(&remotes_lock);
    remote->references = 2;
    pthread_mutex_unlock(&remotes_lock);
    pthread_t input, output;
    if (pthread_create(&output, NULL, relay_output, remote)) {
        if (remote->input >= 0) close(remote->input);
        if (remote->output >= 0) close(remote->output);
        if (remote->error >= 0) close(remote->error);
        remote->input = remote->output = remote->error = -1;
        remote->references = 1;
        return report_failure(remote, channel, EAGAIN);
    }
    pthread_detach(output);
    if (pthread_create(&input, NULL, relay_input, remote)) {
        if (remote->input >= 0) close(remote->input);
        remote->input = -1;
        send_frame(remote, FRAME_STDIN_EOF, NULL, 0);
        remote_release(remote);
    } else {
        pthread_detach(input);
    }
    return remote->pid;
}


/* -- Signals and Waiting -- */

/* The guest itself receives a signal sent to it as a kernel would deliver
 * it; a remote process, or its process group, has the signal forwarded to
 * the broker; every other process does not exist. */
int __wrap_kill(pid_t pid, int signal)
{
    if (signal < 0 || signal >= NSIG) {
        errno = EINVAL;
        return -1;
    }
    if (pid == getpid() || pid == 0) return rumprun_raise_process_signal(signal);
    pthread_mutex_lock(&remotes_lock);
    struct remote *remote = remote_find(pid < 0 ? -pid : pid);
    int exited = remote ? remote->exited : 1;
    pthread_mutex_unlock(&remotes_lock);
    if (!remote) {
        errno = ESRCH;
        return -1;
    }
    if (signal == 0 || exited) return 0;
    unsigned char payload[8] = { 0, 0, 0, (unsigned char)signal, 0, 0, 0, pid < 0 ? 1 : 0 };
    send_frame(remote, FRAME_SIGNAL, payload, sizeof(payload));
    return 0;
}

/* Reap a remote process as waitpid does. PID -1, 0, or a negative process
 * group matches every remote process, since they form one group. */
pid_t __wrap__sys___wait450(pid_t pid, int *status, int options, struct rusage *usage)
{
    if (usage) memset(usage, 0, sizeof(*usage));
    pthread_mutex_lock(&remotes_lock);
    for (;;) {
        int candidates = 0;
        for (int i = 0; i < REMOTE_LIMIT; ++i) {
            struct remote *remote = &remotes[i];
            if (!remote->used || (pid > 0 && remote->pid != pid)) continue;
            ++candidates;
            if (remote->exited && remote->references == 0) {
                pid_t reaped = remote->pid;
                if (status) *status = remote->status;
                remote->used = 0;
                pthread_mutex_unlock(&remotes_lock);
                return reaped;
            }
        }
        if (!candidates) {
            pthread_mutex_unlock(&remotes_lock);
            errno = ECHILD;
            return -1;
        }
        if (options & WNOHANG) {
            pthread_mutex_unlock(&remotes_lock);
            return 0;
        }
        pthread_cond_wait(&remotes_changed, &remotes_lock);
    }
}
