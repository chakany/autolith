/* SBCL 2.6.6 runtime adapters for the experimental rumprun guest. */
#include "genesis/sbcl.h"
#include "runtime.h"
#include "os.h"
#include "interr.h"
#include "sbcl-machine.h"
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <dlfcn.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <fs/tmpfs/tmpfs_args.h>

os_vm_address_t os_alloc_gc_space(int space, int attributes,
                                 os_vm_address_t address, os_vm_size_t length)
{
    (void)space;
    if (attributes & ALLOCATE_LOW) lose("rumprun: low memory allocation requested");
    void *result = rumprun_vm_map((attributes & MOVABLE) ? NULL : address, length);
    if (result && (attributes & IS_GUARD_PAGE)
        && rumprun_vm_protect(result, length, PROT_NONE))
        lose("rumprun: guard protection failed");
    return result;
}

void *rumprun_load_core(int fd, os_vm_offset_t offset,
                        os_vm_address_t address, os_vm_size_t length)
{
    if (!address) address = rumprun_vm_map(NULL, length);
    if (!address) lose("rumprun: core allocation failed");
    size_t done = 0;
    while (done < length) {
        ssize_t count = pread(fd, address + done, length - done, offset + done);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) lose("rumprun: truncated core at %lu", (unsigned long)(offset + done));
        done += count;
    }
    return address;
}

extern void *rumprun_symbol(const char *);
static char *lookup_error;
void *__wrap_dlsym(void *handle, const char *name)
{
    (void)handle;
    void *address = rumprun_symbol(name);
    if (!address) {
        fprintf(stderr, "rumprun: missing static foreign symbol %s\n", name);
        lookup_error = "rumprun: symbol absent from static runtime manifest";
    }
    return address;
}

void *__wrap_dlopen(const char *path, int flags)
{
    (void)flags;
    /* Only the statically linked runtime has a lookup namespace. */
    if (path) {
        lookup_error = "rumprun: dynamic shared objects are unavailable";
        return NULL;
    }
    return (void *)1;
}

char *__wrap_dlerror(void)
{
    char *error = lookup_error;
    lookup_error = NULL;
    return error;
}

/* Test harness only: QEMU isa-debug-exit reports (status << 1) | 1.
 * Without that device, use rumprun's normal halt path. */
extern void __real__exit(int) __attribute__((noreturn));
void __wrap__exit(int status)
{
    printf("SBCL-GUEST-EXIT %d\n", status);
    fflush(NULL);
    __asm__ volatile("outl %0, %1" :: "a"((unsigned)status), "Nd"((unsigned short)0xf4));
    __real__exit(status);
}
extern const unsigned char rumprun_core_start[], rumprun_core_end[];
extern const unsigned char rumprun_script_start[], rumprun_script_end[];
extern const unsigned char rumprun_sources_start[], rumprun_sources_end[];
extern int initialize_lisp(int, char **, char **);
extern char **environ;

static int write_guest_file(const char *path, const unsigned char *start,
                            const unsigned char *end)
{
    int fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, 0600);
    if (fd < 0) { perror(path); return -1; }
    while (start < end) {
        ssize_t count = write(fd, start, end - start);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) { perror(path); close(fd); return -1; }
        start += count;
    }
    return close(fd);
}

static unsigned read_u32(const unsigned char *bytes)
{
    return (unsigned)bytes[0]<<24 | (unsigned)bytes[1]<<16
         | (unsigned)bytes[2]<<8 | bytes[3];
}

/* This archive is a build input embedded in the executable, not guest traffic. */
static int unpack_sources(void)
{
    const unsigned char *position = rumprun_sources_start;
    const unsigned char *end = rumprun_sources_end;
    while (end - position >= 8) {
        unsigned namesize = read_u32(position), size = read_u32(position + 4);
        position += 8;
        if (!namesize) return size == 0 && position == end ? 0 : -1;
        if (namesize >= 256 || (size_t)(end-position) < (size_t)namesize + size) return -1;
        char path[256];
        memcpy(path, position, namesize); path[namesize] = 0; position += namesize;
        if (path[0] == '/' || strstr(path, "..") || strlen(path) != namesize) return -1;
        for (char *slash = path; *slash; ++slash) {
            if (*slash != '/') continue;
            *slash = 0;
            if (mkdir(path, 0700) && errno != EEXIST) return -1;
            *slash = '/';
        }
        if (write_guest_file(path, position, position + size)) return -1;
        position += size;
    }
    return -1;
}

int main(void)
{
    rumprun_machine_init();
    /* Verify namespace error handling before the first Lisp foreign lookup. */
    if (__wrap_dlerror() || !__wrap_dlopen(NULL, RTLD_NOW) || __wrap_dlerror()
        || !__wrap_dlsym((void *)1, "lisp_init_time") || __wrap_dlerror()
        || __wrap_dlopen("unavailable.so", RTLD_NOW) || !__wrap_dlerror()
        || __wrap_dlerror()) {
        fprintf(stderr, "static foreign namespace check failed\n");
        return 1;
    }
    struct tmpfs_args tmpfs = {
        .ta_version = TMPFS_ARGS_VERSION, .ta_size_max = 256UL*1024*1024,
        .ta_root_mode = 0700
    };
    if (mkdir("/core", 0700) || mount(MOUNT_TMPFS, "/core", 0, &tmpfs, sizeof(tmpfs))) {
        perror("private core tmpfs"); return 1;
    }
    size_t length = rumprun_core_end - rumprun_core_start;
    printf("SBCL rumprun: embedding %lu core bytes\n", (unsigned long)length);
    if (write_guest_file("/core/cold.core", rumprun_core_start, rumprun_core_end)
        || write_guest_file("/core/smoke.lisp", rumprun_script_start, rumprun_script_end))
        return 1;
    if (chdir("/core") || unpack_sources()) { perror("warm sources"); return 1; }
    char *arguments[] = { "sbcl", "--core", "/core/cold.core", "--noinform",
                          "--dynamic-space-size", "512",
                          "--disable-debugger", "--no-sysinit", "--no-userinit",
                          "--script", "/core/smoke.lisp", NULL };
    return initialize_lisp(11, arguments, environ);
}
