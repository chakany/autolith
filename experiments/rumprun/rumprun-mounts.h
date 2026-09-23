/* NFS mounts the rumprun guest makes at startup. */
#ifndef RUMPRUN_MOUNTS_H
#define RUMPRUN_MOUNTS_H
/* Mount every entry of RUMPRUN_NFS_MOUNTS. Return 0 when all mounted or
 * none were requested, or -1 after reporting the first failure. */
int rumprun_mount_nfs_from_environment(void);
#endif
