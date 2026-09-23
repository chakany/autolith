/* SBCL 2.6.6 runtime adapters for the experimental rumprun guest. */
#include "genesis/sbcl.h"
#include "runtime.h"
#include "os.h"
#include "interr.h"
#include "sbcl-machine.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>
#include <stdint.h>
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

/* Every distinct name Lisp looked up, in request order. A saved core
 * re-resolves these at startup, so later link stages must provide them. */
static struct lookup { char *name; int found; } *lookups;
static size_t lookup_count, lookup_capacity;

static void lookup_record(const char *name, int found)
{
    for (size_t i = 0; i < lookup_count; ++i)
        if (!strcmp(lookups[i].name, name)) return;
    if (lookup_count == lookup_capacity) {
        size_t capacity = lookup_capacity ? 2 * lookup_capacity : 256;
        struct lookup *grown = realloc(lookups, capacity * sizeof(*grown));
        if (!grown) { fprintf(stderr, "rumprun: lookup manifest exhausted memory\n"); abort(); }
        lookups = grown;
        lookup_capacity = capacity;
    }
    char *copy = strdup(name);
    if (!copy) { fprintf(stderr, "rumprun: lookup manifest exhausted memory\n"); abort(); }
    lookups[lookup_count++] = (struct lookup){ copy, found };
}

void *__wrap_dlsym(void *handle, const char *name)
{
    (void)handle;
    void *address = rumprun_symbol(name);
    lookup_record(name, address != NULL);
    if (!address) {
        fprintf(stderr, "rumprun: missing static foreign symbol %s\n", name);
        lookup_error = "rumprun: symbol absent from static runtime manifest";
    }
    return address;
}

/* Write "found NAME" or "missing NAME" lines for every recorded lookup. */
static int write_lookup_manifest(const char *path)
{
    FILE *stream = fopen(path, "w");
    if (!stream) return -1;
    for (size_t i = 0; i < lookup_count; ++i)
        fprintf(stream, "%s %s\n", lookups[i].found ? "found" : "missing", lookups[i].name);
    return fclose(stream) ? -1 : 0;
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

/* Guest-to-host export channel. QEMU's isa-debugcon copies every byte
 * written to port 0xe9 into a host file. After a header line, each regular
 * file below /core/export is framed with its relative path as
 *   FILE <path> <length> <FNV-1a 64-bit hex>\n<bytes>\n
 * and an END line closes the stream. This detects transport damage; the
 * guest is not a trusted source of these files. */
static void debugcon_write(const void *data, size_t length)
{
    __asm__ volatile("rep outsb" : "+S"(data), "+c"(length)
                     : "d"((unsigned short)0xe9) : "memory");
}

/* Accept one path component of letters, digits, '.', '-', and '_' that
 * does not begin with a dot. */
static int export_component_p(const char *name)
{
    if (!*name || *name == '.') return 0;
    for (const char *c = name; *c; ++c)
        if (!((*c >= 'a' && *c <= 'z') || (*c >= 'A' && *c <= 'Z')
              || (*c >= '0' && *c <= '9') || *c == '.' || *c == '-' || *c == '_'))
            return 0;
    return 1;
}

/* Hash the file, then send it; the size must not change between passes. */
static int export_file(const char *path, const char *name)
{
    static unsigned char buffer[65536];
    char line[1100];
    int fd = open(path, O_RDONLY);
    struct stat status;
    if (fd < 0 || fstat(fd, &status) || !S_ISREG(status.st_mode)) {
        if (fd >= 0) close(fd);
        return -1;
    }
    uint64_t hash = 0xcbf29ce484222325ULL;
    off_t total = 0;
    for (ssize_t count; (count = read(fd, buffer, sizeof(buffer))) != 0; ) {
        if (count < 0) { if (errno == EINTR) continue; close(fd); return -1; }
        for (ssize_t i = 0; i < count; ++i)
            hash = (hash ^ buffer[i]) * 0x100000001b3ULL;
        total += count;
    }
    if (total != status.st_size || lseek(fd, 0, SEEK_SET)) { close(fd); return -1; }
    snprintf(line, sizeof(line), "FILE %s %lld %016llx\n", name,
             (long long)total, (unsigned long long)hash);
    debugcon_write(line, strlen(line));
    for (ssize_t count; (count = read(fd, buffer, sizeof(buffer))) != 0; ) {
        if (count < 0) { if (errno == EINTR) continue; close(fd); return -1; }
        debugcon_write(buffer, count);
        total -= count;
    }
    debugcon_write("\n", 1);
    return close(fd) || total ? -1 : 0;
}

/* Export every regular file below PATH, naming it by NAME, its path
 * relative to /core/export. Other file types are refused. */
static int export_tree(const char *path, const char *name)
{
    DIR *directory = opendir(path);
    if (!directory) return -1;
    int failed = 0;
    for (struct dirent *entry; !failed && (entry = readdir(directory)); ) {
        if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
        char child[512], child_name[512];
        struct stat status;
        if (!export_component_p(entry->d_name)
            || snprintf(child, sizeof(child), "%s/%s", path, entry->d_name) >= (int)sizeof(child)
            || snprintf(child_name, sizeof(child_name), "%s%s%s", name, *name ? "/" : "",
                        entry->d_name) >= (int)sizeof(child_name)
            || lstat(child, &status)) {
            fprintf(stderr, "rumprun: cannot export %s/%s\n", path, entry->d_name);
            failed = 1;
        } else if (S_ISDIR(status.st_mode)) {
            failed = export_tree(child, child_name);
        } else if (!S_ISREG(status.st_mode) || export_file(child, child_name)) {
            fprintf(stderr, "rumprun: cannot export %s\n", child_name);
            failed = 1;
        }
    }
    closedir(directory);
    return failed ? -1 : 0;
}

/* Export nothing when the guest created no /core/export directory. An
 * export always carries the foreign lookup manifest. */
static int export_files(void)
{
    struct stat status;
    if (stat("/core/export", &status)) return errno == ENOENT ? 0 : -1;
    if (write_lookup_manifest("/core/export/foreign-lookups.txt")) return -1;
    debugcon_write("RUMPRUN-EXPORT 1\n", 17);
    if (export_tree("/core/export", "")) return -1;
    debugcon_write("END\n", 4);
    return 0;
}

/* Runs once on the first exit path taken, whether Lisp exits through the
 * static table or C calls exit(), which in libc bypasses the _exit wrap.
 * A successful guest first exports its files. QEMU's isa-debug-exit then
 * reports (status << 1) | 1; without that device the exit continues through
 * rumprun's normal halt path. */
static int guest_exit(int status)
{
    static int exiting;
    if (exiting) return status;
    exiting = 1;
    fflush(NULL);
    if (status == 0 && export_files()) status = 3;
    printf("SBCL-GUEST-EXIT %d\n", status);
    fflush(NULL);
    __asm__ volatile("outl %0, %1" :: "a"((unsigned)status), "Nd"((unsigned short)0xf4));
    return status;
}

extern void __real_exit(int) __attribute__((noreturn));
void __wrap_exit(int status)
{
    __real_exit(guest_exit(status));
}

extern void __real__exit(int) __attribute__((noreturn));
void __wrap__exit(int status)
{
    __real__exit(guest_exit(status));
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

/* The guest command line is "sbcl [MEGABYTES]", naming the dynamic space size. */
int main(int argc, char **argv)
{
    char *heap = argc > 1 ? argv[1] : "512";
    if (argc > 2 || !*heap || strspn(heap, "0123456789") != strlen(heap) || strlen(heap) > 6) {
        fprintf(stderr, "usage: sbcl [DYNAMIC-SPACE-MEGABYTES]\n");
        return 1;
    }
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
        .ta_version = TMPFS_ARGS_VERSION, .ta_size_max = 512UL*1024*1024,
        .ta_root_mode = 0700
    };
    if (mkdir("/core", 0700) || mount(MOUNT_TMPFS, "/core", 0, &tmpfs, sizeof(tmpfs))) {
        perror("private core tmpfs"); return 1;
    }
    size_t length = rumprun_core_end - rumprun_core_start;
    printf("SBCL rumprun: embedding %lu core bytes\n", (unsigned long)length);
    if (write_guest_file("/core/lisp.core", rumprun_core_start, rumprun_core_end)
        || write_guest_file("/core/script.lisp", rumprun_script_start, rumprun_script_end))
        return 1;
    if (chdir("/core") || unpack_sources()) { perror("embedded sources"); return 1; }
    /* Contribs, when embedded, live in SBCL's standard SBCL_HOME layout. */
    if (setenv("SBCL_HOME", "/core/sbcl-home/", 1)) { perror("SBCL_HOME"); return 1; }
    char *arguments[] = { "sbcl", "--core", "/core/lisp.core", "--noinform",
                          "--dynamic-space-size", heap,
                          "--disable-debugger", "--no-sysinit", "--no-userinit",
                          "--script", "/core/script.lisp", NULL };
    return initialize_lisp(11, arguments, environ);
}
