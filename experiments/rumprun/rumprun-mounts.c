/* NFS mounts of host directories for the rumprun guest.
 * RUMPRUN_NFS_MOUNTS, which the host passes in the guest's JSON "env", is a
 * space-separated list of ADDRESS:PORT:EXPORT:MOUNTPOINT entries: an IPv4
 * address and port serving both MOUNT and NFS version 3 over TCP, as the
 * host broker does, the export to mount, and the absolute guest directory
 * to mount it on. There is no portmapper, so the MOUNT call goes directly to
 * PORT. Each mount is hard and version 3 over TCP. */
#include "rumprun-mounts.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsproto.h>
#define NFS_ARGS_ONLY
#include <nfs/nfsmount.h>

#define MOUNT_PROGRAM 100005
#define MOUNT_VERSION 3
#define MOUNT_PROCEDURE_MNT 1
#define TRANSFER_SIZE 65536

/* The successful part of a MOUNT v3 MNT reply. */
struct mount_reply {
    unsigned status;
    unsigned handle_length;
    char handle[NFSX_V3FHMAX];
};

/* Create DIRECTORY and its missing parents. */
static int make_directories(const char *directory)
{
    char path[1024];
    if (strlen(directory) >= sizeof(path)) { errno = ENAMETOOLONG; return -1; }
    strcpy(path, directory);
    for (char *slash = path + 1; ; ++slash) {
        char saved = *slash;
        if (saved != '/' && saved) continue;
        *slash = 0;
        if (mkdir(path, 0755) && errno != EEXIST) return -1;
        *slash = saved;
        if (!saved) return 0;
    }
}

/* Append the big-endian 32-bit VALUE to BUFFER at *LENGTH. */
static void put_word(unsigned char *buffer, size_t *length, unsigned value)
{
    buffer[(*length)++] = value >> 24; buffer[(*length)++] = value >> 16;
    buffer[(*length)++] = value >> 8;  buffer[(*length)++] = value;
}

static unsigned get_word(const unsigned char *buffer, size_t *offset)
{
    unsigned value = (unsigned)buffer[*offset] << 24 | (unsigned)buffer[*offset + 1] << 16
                   | (unsigned)buffer[*offset + 2] << 8 | buffer[*offset + 3];
    *offset += 4;
    return value;
}

/* Read exactly LENGTH octets from SOCKET. */
static int read_fully(int sock, unsigned char *buffer, size_t length)
{
    while (length) {
        ssize_t count = read(sock, buffer, length);
        if (count <= 0) { if (count < 0 && errno == EINTR) continue; return -1; }
        buffer += count; length -= count;
    }
    return 0;
}

/* Ask the MOUNT service at SERVER for EXPORT's root file handle with one
 * MNT call, encoded here because NetBSD's RPC client needs /etc/netconfig,
 * which rumprun does not provide. */
static int mount_handle(struct sockaddr_in *server, const char *export, struct mount_reply *reply)
{
    size_t export_length = strlen(export);
    if (export_length > 1024) { fprintf(stderr, "rumprun: export path too long\n"); return -1; }
    unsigned char call[4 + 64 + 1024 + 4];
    size_t length = 4;
    put_word(call, &length, 1);                   /* xid */
    put_word(call, &length, 0);                   /* CALL */
    put_word(call, &length, 2);                   /* RPC version */
    put_word(call, &length, MOUNT_PROGRAM);
    put_word(call, &length, MOUNT_VERSION);
    put_word(call, &length, MOUNT_PROCEDURE_MNT);
    put_word(call, &length, 1);                   /* AUTH_SYS */
    put_word(call, &length, 20);                  /* its body length */
    put_word(call, &length, 0);                   /* stamp */
    put_word(call, &length, 0);                   /* empty machine name */
    put_word(call, &length, 0);                   /* uid */
    put_word(call, &length, 0);                   /* gid */
    put_word(call, &length, 0);                   /* no groups */
    put_word(call, &length, 0);                   /* AUTH_NONE verifier */
    put_word(call, &length, 0);
    put_word(call, &length, export_length);
    memcpy(call + length, export, export_length);
    length += export_length;
    while (length % 4) call[length++] = 0;
    size_t mark = 0;
    put_word(call, &mark, 0x80000000u | (unsigned)(length - 4));
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0 || connect(sock, (struct sockaddr *)server, sizeof(*server))
        || write(sock, call, length) != (ssize_t)length) {
        fprintf(stderr, "rumprun: MOUNT call for %s: %s\n", export, strerror(errno));
        if (sock >= 0) close(sock);
        return -1;
    }
    unsigned char header[4], answer[256];
    size_t offset = 0;
    int failed = read_fully(sock, header, 4);
    unsigned answer_length = failed ? 0 : get_word(header, &offset) & 0x7fffffffu;
    failed = failed || answer_length > sizeof(answer) || answer_length < 24
             || read_fully(sock, answer, answer_length);
    close(sock);
    if (failed) { fprintf(stderr, "rumprun: malformed MOUNT reply for %s\n", export); return -1; }
    offset = 0;
    unsigned xid = get_word(answer, &offset), type = get_word(answer, &offset);
    unsigned accepted = get_word(answer, &offset);
    get_word(answer, &offset);
    unsigned verifier_length = get_word(answer, &offset);
    if (xid != 1 || type != 1 || accepted != 0 || verifier_length > 400
        || offset + ((verifier_length + 3) & ~3u) + 8 > answer_length) goto malformed;
    offset += (verifier_length + 3) & ~3u;
    if (get_word(answer, &offset) != 0) goto malformed;
    reply->status = get_word(answer, &offset);
    if (reply->status) {
        fprintf(stderr, "rumprun: MOUNT of %s refused with status %u\n", export, reply->status);
        return -1;
    }
    if (offset + 4 > answer_length) goto malformed;
    reply->handle_length = get_word(answer, &offset);
    if (reply->handle_length > NFSX_V3FHMAX || offset + reply->handle_length > answer_length)
        goto malformed;
    memcpy(reply->handle, answer + offset, reply->handle_length);
    return 0;
malformed:
    fprintf(stderr, "rumprun: malformed MOUNT reply for %s\n", export);
    return -1;
}

/* Mount one ADDRESS:PORT:EXPORT:MOUNTPOINT ENTRY. */
static int mount_entry(char *entry)
{
    char *port_text = strchr(entry, ':');
    char *export = port_text ? strchr(port_text + 1, ':') : NULL;
    char *mountpoint = export ? strchr(export + 1, ':') : NULL;
    if (!mountpoint) goto malformed;
    *port_text++ = 0; *export++ = 0; *mountpoint++ = 0;
    char *end;
    long port = strtol(port_text, &end, 10);
    struct sockaddr_in server = { .sin_len = sizeof(server), .sin_family = AF_INET,
                                  .sin_port = htons((unsigned short)port) };
    if (*end || port <= 0 || port > 65535 || inet_pton(AF_INET, entry, &server.sin_addr) != 1
        || *export != '/' || *mountpoint != '/')
        goto malformed;
    struct mount_reply reply;
    if (mount_handle(&server, export, &reply)) return -1;
    if (make_directories(mountpoint)) {
        fprintf(stderr, "rumprun: mount point %s: %s\n", mountpoint, strerror(errno));
        return -1;
    }
    struct nfs_args args = {
        .version = NFS_ARGSVERSION, .addr = (struct sockaddr *)&server,
        .addrlen = sizeof(server), .sotype = SOCK_STREAM, .proto = IPPROTO_TCP,
        .fh = (u_char *)reply.handle, .fhsize = reply.handle_length,
        .flags = NFSMNT_NFSV3 | NFSMNT_WSIZE | NFSMNT_RSIZE | NFSMNT_READDIRSIZE | NFSMNT_INT,
        .wsize = TRANSFER_SIZE, .rsize = TRANSFER_SIZE, .readdirsize = TRANSFER_SIZE,
        .hostname = entry
    };
    if (mount(MOUNT_NFS, mountpoint, 0, &args, sizeof(args))) {
        fprintf(stderr, "rumprun: NFS mount of %s on %s: %s\n", export, mountpoint, strerror(errno));
        return -1;
    }
    printf("rumprun: mounted %s:%s on %s\n", entry, export, mountpoint);
    return 0;
malformed:
    fprintf(stderr, "rumprun: malformed RUMPRUN_NFS_MOUNTS entry\n");
    return -1;
}

int rumprun_mount_nfs_from_environment(void)
{
    const char *list = getenv("RUMPRUN_NFS_MOUNTS");
    if (!list) return 0;
    char copy[4096];
    if (strlen(list) >= sizeof(copy)) {
        fprintf(stderr, "rumprun: RUMPRUN_NFS_MOUNTS is too long\n");
        return -1;
    }
    strcpy(copy, list);
    char *state;
    for (char *entry = strtok_r(copy, " ", &state); entry; entry = strtok_r(NULL, " ", &state))
        if (mount_entry(entry)) return -1;
    return 0;
}
