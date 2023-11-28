/* Basic syscall test, see memcheck/tests/x86-linux/scalar.c for more info. */

#include "scalar.h"
#include "config.h"

#include <bsm/audit.h>
#include <nfs/nfs.h>
#include <nfs/nfssys.h>
#include <sys/acl.h>
#include <sys/door.h>
#include <sys/fcntl.h>
#include <sys/fstyp.h>
#include <sys/lgrp_user.h>
#if defined(HAVE_SYS_LGRP_USER_IMPL_H)
#include <sys/lgrp_user_impl.h>
#endif /* HAVE_SYS_LGRP_USER_IMPL_H */
#include <sys/mman.h>
#include <sys/modctl.h>
#include <sys/mount.h>
#include <sys/port_impl.h>
#include <sys/priocntl.h>
#include <sys/priv.h>
#include <sys/sem_impl.h>
#include <sys/sendfile.h>
#include <sys/shm_impl.h>
#include <sys/termios.h>
#include <ucontext.h>
#include <sys/utsname.h>
#include <sys/tsol/tndb.h>
#include <sys/tsol/tsyscall.h>

/* Helper functions.  These are necessary if we've got two tests for a single
   syscall.  In that case, Memcheck can sometimes merge error messages.  Doing
   each test in its own function prevents that. */
__attribute__((noinline))
static void sys_mount(void)
{
   GO(SYS_mount, "(4-arg, table variant) 4s 2m");
   SY(SYS_mount, x0 + 1, x0, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_mount2(void)
{
   GO(SYS_mount, "(4-arg) 4s 3m");
   SY(SYS_mount, x0 + 1, x0, x0, x0 + 256); FAIL;
}

__attribute__((noinline))
static void sys_mount3(void)
{
   GO(SYS_mount, "(6-arg) 6s 4m");
   SY(SYS_mount, x0 + 1, x0, x0 | MS_DATA, x0 + 256, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_mount4(void)
{
   GO(SYS_mount, "(8-arg) 8s 5m");
   SY(SYS_mount, x0 + 1, x0, x0 | MS_OPTIONSTR, x0 + 256, x0 + 1, x0 + 1,
                 x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pgrpsys(void)
{
   GO(SYS_pgrpsys, "(GETPGRP) 1s 0m");
   SY(SYS_pgrpsys, x0); SUCC;
}

__attribute__((noinline))
static void sys_pgrpsys2(void)
{
   GO(SYS_pgrpsys, "(GETSID) 2s 0m");
   SY(SYS_pgrpsys, x0 + 2, x0); SUCC;
}

__attribute__((noinline))
static void sys_pgrpsys3(void)
{
   GO(SYS_pgrpsys, "(GETPGID) 2s 0m");
   SY(SYS_pgrpsys, x0 + 4, x0); SUCC;
}

__attribute__((noinline))
static void sys_shmsys(void)
{
   GO(SYS_shmsys, "(SHMAT) 4s 0m");
   SY(SYS_shmsys, x0 + SHMAT, x0, x0 - 1, x0); FAIL;
}

__attribute__((noinline))
static void sys_shmsys2(void)
{
   GO(SYS_shmsys, "(SHMCTL,SHM_LOCK) 3s 0m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + SHM_LOCK); FAIL;
}

__attribute__((noinline))
static void sys_shmsys3(void)
{
   GO(SYS_shmsys, "(SHMCTL,SHM_UNLOCK) 3s 0m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + SHM_UNLOCK); FAIL;
}

__attribute__((noinline))
static void sys_shmsys4(void)
{
   GO(SYS_shmsys, "(SHMCTL,IPC_RMID) 3s 0m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_RMID); FAIL;
}

__attribute__((noinline))
static void sys_shmsys5(void)
{
   GO(SYS_shmsys, "(SHMCTL,IPC_SET) 4s 3m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_SET, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys6(void)
{
   struct shmid_ds buf;
   buf.shm_perm.uid = x0 + 1;
   buf.shm_perm.gid = x0 + 1;
   buf.shm_perm.mode = x0 + 1;

   GO(SYS_shmsys, "(SHMCTL,IPC_SET) 6s 0m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_SET, &buf); FAIL;
}

__attribute__((noinline))
static void sys_shmsys7(void)
{
   GO(SYS_shmsys, "(SHMCTL,IPC_STAT) 4s 1m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_STAT, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys8(void)
{
   GO(SYS_shmsys, "(SHMCTL,IPC_SET64) 4s 3m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_SET64, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys9(void)
{
   struct shmid_ds64 buf;
   buf.shmx_perm.ipcx_uid = x0 + 1;
   buf.shmx_perm.ipcx_gid = x0 + 1;
   buf.shmx_perm.ipcx_mode = x0 + 1;

   GO(SYS_shmsys, "(SHMCTL,IPC_SET64) 6s 0m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_SET64, &buf); FAIL;
}

__attribute__((noinline))
static void sys_shmsys10(void)
{
   GO(SYS_shmsys, "(SHMCTL,IPC_STAT64) 4s 1m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_STAT64, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys11(void)
{
   GO(SYS_shmsys, "(SHMDT) 2s 0m");
   SY(SYS_shmsys, x0 + SHMDT, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys12(void)
{
   GO(SYS_shmsys, "(SHMGET) 4s 0m");
   SY(SYS_shmsys, x0 + SHMGET, x0, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_shmsys13(void)
{
   GO(SYS_shmsys, "(SHMIDS) 4s 2m");
   SY(SYS_shmsys, x0 + SHMIDS, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys(void)
{
   GO(SYS_semsys, "(SEMCTL,IPC_STAT) 5s 1m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + IPC_STAT, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys2(void)
{
   GO(SYS_semsys, "(SEMCTL,IPC_SET) 5s 1m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + IPC_SET, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys3(void)
{
   GO(SYS_semsys, "(SEMCTL,IPC_STAT64) 5s 1m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + IPC_STAT64, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys4(void)
{
   GO(SYS_semsys, "(SEMCTL,IPC_SET64) 5s 1m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + IPC_SET64, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys5(void)
{
   GO(SYS_semsys, "(SEMCTL,IPC_RMID) 3s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + IPC_RMID); FAIL;
}

__attribute__((noinline))
static void sys_semsys6(void)
{
   GO(SYS_semsys, "(SEMCTL,GETALL) 4s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + GETALL, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys7(void)
{
   GO(SYS_semsys, "(SEMCTL,SETALL) 4s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + SETALL, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys8(void)
{
   GO(SYS_semsys, "(SEMCTL,GETVAL) 4s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + GETVAL); FAIL;
}

__attribute__((noinline))
static void sys_semsys9(void)
{
   GO(SYS_semsys, "(SEMCTL,SETVAL) 5s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + SETVAL, x0 + 2); FAIL;
}

__attribute__((noinline))
static void sys_semsys10(void)
{
   GO(SYS_semsys, "(SEMCTL,GETPID) 4s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + GETPID); FAIL;
}

__attribute__((noinline))
static void sys_semsys11(void)
{
   GO(SYS_semsys, "(SEMCTL,GETNCNT) 4s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + GETNCNT); FAIL;
}

__attribute__((noinline))
static void sys_semsys12(void)
{
   GO(SYS_semsys, "(SEMCTL,GETZCNT) 4s 0m");
   SY(SYS_semsys, x0 + SEMCTL, x0, x0, x0 + GETZCNT); FAIL;
}

__attribute__((noinline))
static void sys_semsys13(void)
{
   GO(SYS_semsys, "(SEMGET) 4s 0m");
   SY(SYS_semsys, x0 + SEMGET, x0, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_semsys14(void)
{
   GO(SYS_semsys, "(SEMOP) 4s 1m");
   SY(SYS_semsys, x0 + SEMOP, x0, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys15(void)
{
   GO(SYS_semsys, "(SEMIDS) 4s 2m");
   SY(SYS_semsys, x0 + SEMIDS, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_semsys16(void)
{
   GO(SYS_semsys, "(SEMTIMEDOP) 5s 2m");
   SY(SYS_semsys, x0 + SEMTIMEDOP, x0, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_fcntl(void)
{
   GO(SYS_fcntl, "(GETFD) 2s 0m");
   SY(SYS_fcntl, x0 - 1, x0 + F_GETFD); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_fcntl2(void)
{
   GO(SYS_fcntl, "(DUPFD) 3s 0m");
   SY(SYS_fcntl, x0 - 1, x0 + F_DUPFD, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_fcntl3(void)
{
   GO(SYS_fcntl, "(DUPFD_CLOEXEC) 3s 0m");
   SY(SYS_fcntl, x0 - 1, x0 + F_DUPFD_CLOEXEC, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_fcntl4(void)
{
   GO(SYS_fcntl, "(GETLK) 3s 5m");
   SY(SYS_fcntl, x0 - 1, x0 + F_GETLK, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_openat(void)
{
   GO(SYS_openat, "(3-args) 3s 1m");
   SY(SYS_openat, x0 - 1, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_openat2(void)
{
   GO(SYS_openat, "(4-args) 4s 1m");
   SY(SYS_openat, x0 - 1, x0, x0 | O_CREAT, x0); FAIL;
}

__attribute__((noinline))
static void sys_tasksys(void)
{
   GO(SYS_tasksys, "(settaskid) 3s 0m");
   SY(SYS_tasksys, x0 + 0, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_tasksys2(void)
{
   GO(SYS_tasksys, "(gettaskid) 1s 0m");
   SY(SYS_tasksys, x0 + 1); SUCC;
}

__attribute__((noinline))
static void sys_tasksys3(void)
{
   GO(SYS_tasksys, "(getprojid) 1s 0m");
   SY(SYS_tasksys, x0 + 2); SUCC;
}

__attribute__((noinline))
static void sys_tasksys4(void)
{
   GO(SYS_tasksys, "(projlist) 3s 1m");
   /* ARG2 and ARG3 are ignored */
   SY(SYS_tasksys, x0 + 3, x0, x0, x0 + 1, x0 + 2); FAIL;
}

__attribute__((noinline))
static void sys_sendfilev(void)
{
   GO(SYS_sendfilev, "(SENDFILEV) 5s 2m");
   SY(SYS_sendfilev, x0 + SENDFILEV, x0 - 1, x0 + 1, x0 + 2, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_sendfilev2(void)
{
   struct sendfilevec vec[2];
   vec[0].sfv_fd = SFV_FD_SELF;
   vec[0].sfv_off = (off_t)(x0 + 1);
   vec[0].sfv_len = x0 + 1;
   vec[0].sfv_flag = 0; // should be set to zero according to man page
   vec[1].sfv_fd = x0 - 1;
   vec[1].sfv_off = x0;
   vec[1].sfv_len = x0 + 1;
   vec[1].sfv_flag = 0; // should be set to zero according to man page

   GO(SYS_sendfilev, "(SENDFILEV) 4s 2m");
   SY(SYS_sendfilev, x0 + SENDFILEV, x0 - 1, vec, 2, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_sendfilev3(void)
{
   GO(SYS_sendfilev, "(SENDFILEV64) 5s 2m");
   SY(SYS_sendfilev, x0 + SENDFILEV64, x0 - 1, x0 + 1, x0 + 2, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_sendfilev4(void)
{
   struct sendfilevec64 vec[2];
   vec[0].sfv_fd = SFV_FD_SELF;
   vec[0].sfv_off = (off_t)(x0 + 1);
   vec[0].sfv_len = x0 + 1;
   vec[0].sfv_flag = 0; // should be set to zero according to man page
   vec[1].sfv_fd = x0 - 1;
   vec[1].sfv_off = x0;
   vec[1].sfv_len = x0 + 1;
   vec[1].sfv_flag = 0; // should be set to zero according to man page

   GO(SYS_sendfilev, "(SENDFILEV64) 4s 2m");
   SY(SYS_sendfilev, x0 + SENDFILEV64, x0 - 1, vec, 2, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_privsys(void)
{
   GO(SYS_privsys, "(PRIVSYS_SETPPRIV) 5s 1m");
   SY(SYS_privsys, x0 + PRIVSYS_SETPPRIV, x0, x0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_privsys2(void)
{
   GO(SYS_privsys, "(PRIVSYS_GETPPRIV) 5s 1m");
   SY(SYS_privsys, x0 + PRIVSYS_GETPPRIV, x0, x0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_privsys3(void)
{
   GO(SYS_privsys, "(PRIVSYS_GETIMPLINFO) 5s 1m");
   SY(SYS_privsys, x0 + PRIVSYS_GETIMPLINFO, x0, x0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_privsys4(void)
{
   GO(SYS_privsys, "(PRIVSYS_SETPFLAGS) 3s 0m");
   SY(SYS_privsys, x0 + PRIVSYS_SETPFLAGS, x0, x0 + 2); FAIL;
}

__attribute__((noinline))
static void sys_privsys5(void)
{
   GO(SYS_privsys, "(PRIVSYS_GETPFLAGS) 2s 0m");
   SY(SYS_privsys, x0 + PRIVSYS_GETPFLAGS, x0); FAIL;
}

__attribute__((noinline))
static void sys_privsys6(void)
{
   GO(SYS_privsys, "(PRIVSYS_ISSETUGID) 1s 0m");
   SY(SYS_privsys, x0 + PRIVSYS_ISSETUGID); SUCC;
}

__attribute__((noinline))
static void sys_privsys7(void)
{
   GO(SYS_privsys, "(PRIVSYS_PFEXEC_REG) 2s 0m");
   SY(SYS_privsys, x0 + PRIVSYS_PFEXEC_REG, x0 - 1); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_privsys8(void)
{
   GO(SYS_privsys, "(PRIVSYS_PFEXEC_UNREG) 2s 0m");
   SY(SYS_privsys, x0 + PRIVSYS_PFEXEC_UNREG, x0 - 1); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_ucredsys(void)
{
   GO(SYS_ucredsys, "(UCREDSYS_UCREDGET) 3s 1m");
   SY(SYS_ucredsys, x0 + 0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ucredsys2(void)
{
   GO(SYS_ucredsys, "(UCREDSYS_GETPEERUCRED) 3s 1m");
   SY(SYS_ucredsys, x0 + 1, x0 - 1, x0 + 1); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_sysfs(void)
{
   GO(SYS_sysfs, "(GETFSIND) 2s 1m");
   SY(SYS_sysfs, x0 + GETFSIND, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_sysfs2(void)
{
   GO(SYS_sysfs, "(GETFSTYP) 3s 1m");
   SY(SYS_sysfs, x0 + GETFSTYP, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_sysfs3(void)
{
   GO(SYS_sysfs, "(GETNFSTYP) 1s 0m");
   SY(SYS_sysfs, x0 + GETNFSTYP); SUCC;
}

__attribute__((noinline))
static void sys_context(void)
{
   GO(SYS_context, "(GETCONTEXT) 2s 1m");
   SY(SYS_context, x0 + GETCONTEXT, x0); FAILx(EFAULT);
}

__attribute__((noinline))
static void sys_context2(void)
{
   /* The setcontext() wrapper has to call ML_(safe_to_deref) before doing the
      PRE_READ_*() stuff, therefore the 0m parameter. */
   GO(SYS_context, "(SETCONTEXT) 2s 0m");
   SY(SYS_context, x0 + SETCONTEXT, x0 + 1); FAILx(EFAULT);
}

__attribute__((noinline))
static void sys_context3(void)
{
   GO(SYS_context, "(GETUSTACK) 2s 1m");
   SY(SYS_context, x0 + GETUSTACK, x0); FAILx(EFAULT);
}

__attribute__((noinline))
static void sys_context4(void)
{
   GO(SYS_context, "(SETUSTACK) 2s 1m");
   SY(SYS_context, x0 + SETUSTACK, x0); FAILx(EFAULT);
}

__attribute__((noinline))
static void sys_statvfs(void)
{
   GO(SYS_statvfs, "2s 2m");
   SY(SYS_statvfs, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static int sys_statvfs2(void)
{
   const char path[] = "/";
   struct statvfs stats;

   GO(SYS_statvfs, "4s 0m");
   SY(SYS_statvfs, x0 + path, x0 + &stats); SUCC;

   size_t basetype_len = strlen(stats.f_basetype);
   size_t fstr_len = strlen(stats.f_fstr);

   /* Now check that memory after the strings is reported uninitialized. */
   int x = 0;
   if (stats.f_basetype[basetype_len + 2] != ' ') x = -1; else x = -2;
   if (stats.f_fstr[fstr_len + 2] != ' ') x = -3; else x = -4;
   return x;
}

__attribute__((noinline))
static void sys_nfssys(void)
{
   GO(SYS_nfssys, "(NFS_REVAUTH) 2s 1m");
   SY(SYS_nfssys, x0 + NFS_REVAUTH, x0); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys(void)
{
   pcinfo_t pcinfo;
   pcinfo.pc_clname[0] = x0 + 'T';
   pcinfo.pc_clname[1] = x0 + 'S';
   pcinfo.pc_clname[2] = x0;

   GO(SYS_priocntlsys, "(GETCID) 6s 0m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_GETCID, x0 + &pcinfo, x0); SUCC;
}

__attribute__((noinline))
static void sys_priocntlsys2(void)
{
   pcinfo_t pcinfo;
   pcinfo.pc_cid = x0 + 1;

   GO(SYS_priocntlsys, "(GETCLINFO) 6s 0m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_GETCLINFO, x0 + &pcinfo, x0); SUCC;
}

__attribute__((noinline))
static void sys_priocntlsys3(void)
{
   GO(SYS_priocntlsys, "(SETPARMS) 5s 2m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_SETPARMS, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys4(void)
{
   GO(SYS_priocntlsys, "(GETPARMS) 5s 2m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_GETPARMS, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys5(void)
{
   GO(SYS_priocntlsys, "(GETPRIRANGE) 5s 2m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_GETPRIRANGE, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys6(void)
{
   GO(SYS_priocntlsys, "(DONICE) 5s 2m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_DONICE, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys7(void)
{
   GO(SYS_priocntlsys, "(SETXPARMS) 5s 3m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_SETXPARMS, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys8(void)
{
   GO(SYS_priocntlsys, "(GETXPARMS) 5s 3m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_GETXPARMS, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys9(void)
{
   GO(SYS_priocntlsys, "(SETDFLCL) 5s 1m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_SETDFLCL, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys10(void)
{
   GO(SYS_priocntlsys, "(GETDFLCL) 5s 1m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_GETDFLCL, x0 + 1, x0); FAIL;
}

__attribute__((noinline))
static void sys_priocntlsys11(void)
{
   GO(SYS_priocntlsys, "(DOPRIO) 5s 2m");
   SY(SYS_priocntlsys, x0 + 1, x0, x0 + PC_DOPRIO, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_uname(void)
{
   GO(SYS_uname, "1s 1m");
   SY(SYS_uname, x0 + 1); FAIL;
}

__attribute__((noinline))
static int sys_uname2(void)
{
   struct utsname name;

   GO(SYS_uname, "6s 0m");
   SY(SYS_uname, x0 + &name); SUCC;

   size_t sysname_len = strlen(name.sysname);
   size_t nodename_len = strlen(name.nodename);
   size_t release_len = strlen(name.release);
   size_t version_len = strlen(name.version);
   size_t machine_len = strlen(name.machine);

   /* Now check that memory after the strings is reported uninitialized. */
   int x = 0;
   if (name.sysname[sysname_len + 2] != ' ') x = -1; else x = -2;
   if (name.nodename[nodename_len + 2] != ' ') x = -3; else x = -4;
   if (name.release[release_len + 2] != ' ') x = -5; else x = -6;
   if (name.version[version_len + 2] != ' ') x = -7; else x = -8;
   if (name.machine[machine_len + 2] != ' ') x = -9; else x = -10;
   return x;
}

__attribute__((noinline))
static void sys_modctl(void)
{
   GO(SYS_modctl, "(MODLOAD) 4s 1m");
   SY(SYS_modctl, x0 + MODLOAD, x0 + 1, x0 - 1, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_modctl2(void)
{
   GO(SYS_modctl, "(MODUNLOAD) 2s 0m");
   SY(SYS_modctl, x0 + MODUNLOAD, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_modctl3(void)
{
   GO(SYS_modctl, "(MODINFO) 3s 4m");
   SY(SYS_modctl, x0 + MODINFO, x0 + 1, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_lgrpsys(void)
{
   GO(SYS_lgrpsys, "(LGRP_SYS_MEMINFO) 3s 1m");
   SY(SYS_lgrpsys, x0 + LGRP_SYS_MEMINFO, x0 + 0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_lgrpsys2(void)
{
   GO(SYS_lgrpsys, "(LGRP_SYS_MEMINFO) 3s 1m");
   SY(SYS_lgrpsys, x0 + LGRP_SYS_MEMINFO, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_lgrpsys3(void)
{
   meminfo_t minfo;
   minfo.mi_inaddr = (void *)(x0 + 1);
   minfo.mi_info_req = (void *)(x0 + 1);
   minfo.mi_info_count = x0 + 1;
   minfo.mi_outdata = (void *)(x0 + 1);
   minfo.mi_validity = (void *)(x0 + 1);

   GO(SYS_lgrpsys, "(LGRP_SYS_MEMINFO) 4s 4m");
   SY(SYS_lgrpsys, x0 + LGRP_SYS_MEMINFO, x0 + 1, x0 + &minfo); FAIL;
}

__attribute__((noinline))
static void sys_lgrpsys4(void)
{
   GO(SYS_lgrpsys, "(LGRP_SYS_GENERATION) 2s 0m");
   SY(SYS_lgrpsys, x0 + LGRP_SYS_GENERATION, x0 + 0); SUCC;
}

__attribute__((noinline))
static void sys_lgrpsys5(void)
{
   GO(SYS_lgrpsys, "(LGRP_SYS_VERSION) 2s 0m");
   SY(SYS_lgrpsys, x0 + LGRP_SYS_VERSION, x0 + 0); SUCC;
}

__attribute__((noinline))
static void sys_lgrpsys6(void)
{
   GO(SYS_lgrpsys, "(LGRP_SYS_SNAPSHOT) 3s 1m");
   SY(SYS_lgrpsys, x0 + LGRP_SYS_SNAPSHOT, x0 + 10, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_rusagesys(void)
{
   GO(SYS_rusagesys, "(_RUSAGESYS_GETRUSAGE) 2s 1m");
   SY(SYS_rusagesys, x0 + _RUSAGESYS_GETRUSAGE, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_rusagesys2(void)
{
   GO(SYS_rusagesys, "(_RUSAGESYS_GETRUSAGE_CHLD) 2s 1m");
   SY(SYS_rusagesys, x0 + _RUSAGESYS_GETRUSAGE_CHLD, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_rusagesys3(void)
{
   GO(SYS_rusagesys, "(_RUSAGESYS_GETRUSAGE_LWP) 2s 1m");
   SY(SYS_rusagesys, x0 + _RUSAGESYS_GETRUSAGE_LWP, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_rusagesys4(void)
{
   GO(SYS_rusagesys, "(_RUSAGESYS_GETVMUSAGE) 5s 1m");
   SY(SYS_rusagesys, x0 + _RUSAGESYS_GETVMUSAGE, x0, x0, x0 + 1, x0 + 1);
   FAIL;
}

__attribute__((noinline))
static void sys_rusagesys5(void)
{
   size_t nres = 10;

   GO(SYS_rusagesys, "(_RUSAGESYS_GETVMUSAGE) 5s 1m");
   SY(SYS_rusagesys, x0 + _RUSAGESYS_GETVMUSAGE, x0, x0, x0 + 1, x0 + &nres);
   FAIL;
}

__attribute__((noinline))
static void sys_port(void)
{
   GO(SYS_port, "(PORT_CREATE) 1s 0m");
   SY(SYS_port, (x0 + PORT_CREATE) | PORT_SYS_NOPORT); SUCC;
}

__attribute__((noinline))
static void sys_port2(void)
{
   GO(SYS_port, "(PORT_ASSOCIATE,PORT_SOURCE_FD) 6s 0m");
   SY(SYS_port, x0 + PORT_ASSOCIATE, x0 - 1, x0 + PORT_SOURCE_FD, x0, x0,
                x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port3(void)
{
   GO(SYS_port, "(PORT_ASSOCIATE,PORT_SOURCE_FILE) 6s 1m");
   SY(SYS_port, x0 + PORT_ASSOCIATE, x0 - 1, x0 + PORT_SOURCE_FILE, x0, x0,
                x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port4(void)
{
   GO(SYS_port, "(PORT_DISSOCIATE,PORT_SOURCE_FD) 6s 0m");
   SY(SYS_port, x0 + PORT_DISSOCIATE, x0 - 1, x0 + PORT_SOURCE_FD, x0, x0,
                x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port5(void)
{
   GO(SYS_port, "(PORT_DISSOCIATE,PORT_SOURCE_FILE) 6s 1m");
   SY(SYS_port, x0 + PORT_DISSOCIATE, x0 - 1, x0 + PORT_SOURCE_FILE, x0, x0,
                x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port6(void)
{
   GO(SYS_port, "(PORT_SEND) 4s 0m");
   SY(SYS_port, x0 + PORT_SEND, x0 - 1, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port7(void)
{
   GO(SYS_port, "(PORT_SENDN) 6s 2m");
   SY(SYS_port, (x0 + PORT_SENDN) | PORT_SYS_NOPORT, x0, x0, x0 + 1, x0,
                x0); FAIL;
}

__attribute__((noinline))
static void sys_port8(void)
{
   GO(SYS_port, "(PORT_GET) 6s 1m");
   SY(SYS_port, x0 + PORT_GET, x0 - 1, x0, x0, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port9(void)
{
   GO(SYS_port, "(PORT_GETN) 5s 2m");
   SY(SYS_port, x0 + PORT_GETN, x0 - 1, x0, x0 + 1, x0, x0 + 1); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port10(void)
{
   GO(SYS_port, "(PORT_ALERT) 5s 0m");
   SY(SYS_port, x0 + PORT_ALERT, x0 - 1, x0, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_port11(void)
{
   GO(SYS_port, "(PORT_DISPATCH) 6s 0m");
   SY(SYS_port, x0 + PORT_DISPATCH, x0 - 1, x0, x0, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_labelsys(void)
{
   GO(SYS_labelsys, "(TSOL_SYSLABELING) 1s 0m");
   SY(SYS_labelsys, x0 + TSOL_SYSLABELING); SUCC;
}

__attribute__((noinline))
static void sys_labelsys2(void)
{
   GO(SYS_labelsys, "(TSOL_TNRH) 3s 1m");
   SY(SYS_labelsys, x0 + TSOL_TNRH, x0 + TNDB_GET, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_labelsys3(void)
{
   GO(SYS_labelsys, "(TSOL_TNRHTP) 3s 1m");
   SY(SYS_labelsys, x0 + TSOL_TNRHTP, x0 + TNDB_GET, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_labelsys4(void)
{
   GO(SYS_labelsys, "(TSOL_TNMLP) 3s 1m");
   SY(SYS_labelsys, x0 + TSOL_TNMLP, x0 + TNDB_GET, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_labelsys5(void)
{
   GO(SYS_labelsys, "(TSOL_GETLABEL) 3s 2m");
   SY(SYS_labelsys, x0 + TSOL_GETLABEL, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_labelsys6(void)
{
   GO(SYS_labelsys, "(TSOL_FGETLABEL) 3s 1m");
   SY(SYS_labelsys, x0 + TSOL_FGETLABEL, x0 - 1, x0 + 1); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_acl(void)
{
   GO(SYS_acl, "(SETACL) 4s 2m");
   SY(SYS_acl, x0, x0 + SETACL, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_acl2(void)
{
   GO(SYS_acl, "(GETACL) 4s 2m");
   SY(SYS_acl, x0, x0 + GETACL, x0 + 1, x0); FAIL;
}

__attribute__((noinline))
static void sys_acl3(void)
{
   GO(SYS_acl, "(GETACLCNT) 4s 1m");
   SY(SYS_acl, x0, x0 + GETACLCNT, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_acl4(void)
{
   GO(SYS_acl, "(ACE_SETACL) 4s 2m");
   SY(SYS_acl, x0, x0 + ACE_SETACL, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_acl5(void)
{
   GO(SYS_acl, "(ACE_GETACL) 4s 2m");
   SY(SYS_acl, x0, x0 + ACE_GETACL, x0 + 1, x0); FAIL;
}

__attribute__((noinline))
static void sys_acl6(void)
{
   GO(SYS_acl, "(ACE_GETACLCNT) 4s 1m");
   SY(SYS_acl, x0, x0 + ACE_GETACLCNT, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys(void)
{
   GO(SYS_auditsys, "(BSM_GETAUID) 2s 1m");
   SY(SYS_auditsys, x0 + BSM_GETAUID, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys2(void)
{
   GO(SYS_auditsys, "(BSM_SETAUID) 2s 1m");
   SY(SYS_auditsys, x0 + BSM_SETAUID, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys3(void)
{
   GO(SYS_auditsys, "(BSM_GETAUDIT) 2s 1m");
   SY(SYS_auditsys, x0 + BSM_GETAUDIT, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys4(void)
{
   GO(SYS_auditsys, "(BSM_SETAUDIT) 2s 1m");
   SY(SYS_auditsys, x0 + BSM_SETAUDIT, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys5(void)
{
   GO(SYS_auditsys, "(BSM_AUDIT) 3s 1m");
   /* The following syscall can succeed if auditing is not enabled. */
   SY(SYS_auditsys, x0 + BSM_AUDIT, x0, x0 + 1); /*FAIL;*/
}

__attribute__((noinline))
static void sys_auditsys6(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETPOLICY) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETPOLICY, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys7(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETPOLICY) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETPOLICY, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys8(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETKMASK) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETKMASK, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys9(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETKMASK) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETKMASK, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys10(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETQCTRL) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETQCTRL, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys11(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETQCTRL) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETQCTRL, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys12(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETCWD) 4s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETCWD, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys13(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETCAR) 4s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETCAR, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys14(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETUMASK) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETUMASK, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys15(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETSMASK) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETSMASK, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys16(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETCOND) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETCOND, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys17(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETCOND) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETCOND, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys18(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETCLASS) 3s 0m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETCLASS, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys19(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETCLASS) 3s 0m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETCLASS, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys20(void)
{
   au_evclass_map_t classmap;
   classmap.ec_number = x0;
   classmap.ec_class = x0;

   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETCLASS) 4s 0m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETCLASS, &classmap); FAIL;
}

__attribute__((noinline))
static void sys_auditsys21(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETPINFO) 3s 0m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETPINFO, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys22(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETPMASK) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETPMASK, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys23(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETPINFO_ADDR) 4s 0m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETPINFO_ADDR, x0,
                    x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys24(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETKAUDIT) 4s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETKAUDIT, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys25(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETKAUDIT) 4s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETKAUDIT, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys26(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETAMASK) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETAMASK, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys27(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETAMASK) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETAMASK, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys28(void)
{
   GO(SYS_auditsys, "(BSM_GETAUDIT_ADDR) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_GETAUDIT_ADDR, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys29(void)
{
   GO(SYS_auditsys, "(BSM_SETAUDIT_ADDR) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_SETAUDIT_ADDR, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_auditsys30(void)
{
   GO(SYS_auditsys, "(BSM_AUDITDOOR) 2s 0m");
   SY(SYS_auditsys, x0 + BSM_AUDITDOOR, x0); FAIL;
}

__attribute__((noinline))
static void sys_timer_create(void)
{
   GO(SYS_timer_create, "3s 4m");
   SY(SYS_timer_create, x0, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_timer_create2(void)
{
   struct sigevent evp;
   evp.sigev_notify = x0 + SIGEV_THREAD;
   evp.sigev_value.sival_ptr = (void *)(x0 + 1);

   GO(SYS_timer_create, "5s 2m");
   SY(SYS_timer_create, x0, &evp, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_facl(void)
{
   GO(SYS_facl, "(SETACL) 4s 1m");
   SY(SYS_facl, x0 - 1, x0 + SETACL, x0 + 1, x0 + 1); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_facl2(void)
{
   GO(SYS_facl, "(GETACL) 4s 1m");
   SY(SYS_facl, x0 - 1, x0 + GETACL, x0 + 1, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_facl3(void)
{
   GO(SYS_facl, "(GETACLCNT) 4s 0m");
   SY(SYS_facl, x0 - 1, x0 + GETACLCNT, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_facl4(void)
{
   GO(SYS_facl, "(ACE_SETACL) 4s 1m");
   SY(SYS_facl, x0 - 1, x0 + ACE_SETACL, x0 + 1, x0 + 1); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_facl5(void)
{
   GO(SYS_facl, "(ACE_GETACL) 4s 1m");
   SY(SYS_facl, x0 - 1, x0 + ACE_GETACL, x0 + 1, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_facl6(void)
{
   GO(SYS_facl, "(ACE_GETACLCNT) 4s 0m");
   SY(SYS_facl, x0 - 1, x0 + ACE_GETACLCNT, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_door(void)
{
   GO(SYS_door, "(DOOR_CREATE) 4s 0m");
   SY(SYS_door, x0, x0, x0, x0, x0, x0 + DOOR_CREATE); SUCC;
}

__attribute__((noinline))
static void sys_door2(void)
{
   GO(SYS_door, "(DOOR_REVOKE) 2s 0m");
   SY(SYS_door, x0, x0, x0, x0, x0, x0 + DOOR_REVOKE); FAIL;
}

__attribute__((noinline))
static void sys_door3(void)
{
   GO(SYS_door, "(DOOR_INFO) 3s 1m");
   SY(SYS_door, x0, x0 - 1, x0, x0, x0, x0 + DOOR_INFO); FAIL;
}

__attribute__((noinline))
static void sys_door4(void)
{
   GO(SYS_door, "(DOOR_CALL) 3s 6m");
   SY(SYS_door, x0 - 1, x0, x0, x0, x0, x0 + DOOR_CALL); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_door5(void)
{
   door_arg_t params;
   params.data_ptr = (void *)(x0 + 1);
   params.data_size = x0 + 1;
   params.desc_ptr = (void *)(x0 + 1);
   params.desc_num = x0 + 1;
   params.rbuf = (void *)(x0 + 1);
   params.rsize = x0 + 1;

   GO(SYS_door, "(DOOR_CALL) 9s 2m");
   SY(SYS_door, x0, x0 + &params, x0, x0, x0, x0 + DOOR_CALL); FAIL;
}

__attribute__((noinline))
static void sys_door6(void)
{
   GO(SYS_door, "(DOOR_SETPARAM) 4s 0m");
   SY(SYS_door, x0, x0 - 1, x0 + 1, x0, x0, x0 + DOOR_SETPARAM); FAIL;
}

__attribute__((noinline))
static void sys_pset(void)
{
   GO(SYS_pset, "(CREATE) 2s 1m");
   SY(SYS_pset, x0 + PSET_CREATE, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset2(void)
{
   GO(SYS_pset, "(DESTROY) 2s 0m");
   SY(SYS_pset, x0 + PSET_DESTROY, x0); FAIL;
}

__attribute__((noinline))
static void sys_pset3(void)
{
   GO(SYS_pset, "(ASSIGN) 4s 1m");
   SY(SYS_pset, x0 + PSET_ASSIGN, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset4(void)
{
   GO(SYS_pset, "(INFO) 5s 3m");
   SY(SYS_pset, x0 + PSET_INFO, x0 + 1, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset5(void)
{
   int type;
   uint_t numcpus = x0 + 1;

   GO(SYS_pset, "(INFO) 5s 1m");
   SY(SYS_pset, x0 + PSET_INFO, x0 + 1, x0 + &type, x0 + &numcpus,
      x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset6(void)
{
   GO(SYS_pset, "(BIND) 5s 1m");
   SY(SYS_pset, x0 + PSET_BIND, x0 + 1, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset7(void)
{
   GO(SYS_pset, "(BIND_LWP) 5s 1m");
   SY(SYS_pset, x0 + PSET_BIND_LWP, x0 + 1, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset8(void)
{
   GO(SYS_pset, "(GETLOADAVG) 4s 1m");
   SY(SYS_pset, x0 + PSET_GETLOADAVG, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset9(void)
{
   GO(SYS_pset, "(LIST) 3s 1m");
   SY(SYS_pset, x0 + PSET_LIST, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset10(void)
{
   uint_t numpsets = x0 + 1;

   GO(SYS_pset, "(LIST) 3s 1m");
   SY(SYS_pset, x0 + PSET_LIST, x0 + 1, x0 + &numpsets);
}

__attribute__((noinline))
static void sys_pset11(void)
{
   GO(SYS_pset, "(SETATTR) 3s 0m");
   SY(SYS_pset, x0 + PSET_SETATTR, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_pset12(void)
{
   GO(SYS_pset, "(GETATTR) 3s 1m");
   SY(SYS_pset, x0 + PSET_GETATTR, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_pset13(void)
{
   GO(SYS_pset, "(ASSIGN_FORCED) 4s 1m");
   SY(SYS_pset, x0 + PSET_ASSIGN_FORCED, x0 + 1, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_lwp_rwlock(void)
{
   GO(SYS_lwp_rwlock_sys, "(RDLOCK) 3s 8m");
   SY(SYS_lwp_rwlock_sys, x0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_lwp_rwlock2(void)
{
   GO(SYS_lwp_rwlock_sys, "(WRLOCK) 3s 8m");
   SY(SYS_lwp_rwlock_sys, x0 + 1, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_lwp_rwlock3(void)
{
   GO(SYS_lwp_rwlock_sys, "(TRYRDLOCK) 2s 7m");
   SY(SYS_lwp_rwlock_sys, x0 + 2, x0); FAIL;
}

__attribute__((noinline))
static void sys_lwp_rwlock4(void)
{
   GO(SYS_lwp_rwlock_sys, "(TRYWRLOCK) 2s 7m");
   SY(SYS_lwp_rwlock_sys, x0 + 3, x0); FAIL;
}

__attribute__((noinline))
static void sys_lwp_rwlock5(void)
{
   GO(SYS_lwp_rwlock_sys, "(UNLOCK) 2s 2m");
   SY(SYS_lwp_rwlock_sys, x0 + 4, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone(void)
{
   GO(SYS_zone, "(ZONE_CREATE) 2s 12m");
   SY(SYS_zone, x0 + ZONE_CREATE, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone2(void)
{
   zone_def zd;
   zd.zone_name = (void *)(x0 + 1);
   zd.zone_root = (void *)(x0 + 1);
   zd.zone_privs = (void *)x0;
   zd.zone_privssz = x0 + 1;
   zd.rctlbuf = (void *)x0;
   zd.rctlbufsz = x0 + 1;
   zd.extended_error = (void *)x0;
   zd.zfsbuf = (void *)x0;
   zd.zfsbufsz = x0 + 1;
   zd.match = x0;
   zd.doi = x0;
   zd.label = (void *)(x0 + 1);
   zd.flags = x0;

   GO(SYS_zone, "(create) 2s 19m");
   SY(SYS_zone, x0 + ZONE_CREATE, x0 + &zd); FAIL;
}

__attribute__((noinline))
static void sys_zone3(void)
{
   GO(SYS_zone, "(ZONE_DESTROY) 2s 0m");
   SY(SYS_zone, x0 + ZONE_DESTROY, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone4(void)
{
   GO(SYS_zone, "(ZONE_GETATTR) 5s 1m");
   SY(SYS_zone, x0 + ZONE_GETATTR, x0, x0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_zone5(void)
{
   GO(SYS_zone, "(ZONE_ENTER) 2s 0m");
   SY(SYS_zone, x0 + ZONE_ENTER, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone6(void)
{
   GO(SYS_zone, "(ZONE_LIST) 3s 1m");
   SY(SYS_zone, x0 + ZONE_LIST, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_zone7(void)
{
   uint_t numzones = x0 + 1;

   GO(SYS_zone, "(ZONE_LIST) 3s 1m");
   SY(SYS_zone, x0 + ZONE_LIST, x0 + 1, x0 + &numzones); FAIL;
}

__attribute__((noinline))
static void sys_zone8(void)
{
   GO(SYS_zone, "(ZONE_SHUTDOWN) 2s 0m");
   SY(SYS_zone, x0 + ZONE_SHUTDOWN, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone9(void)
{
   GO(SYS_zone, "(ZONE_LOOKUP) 2s 1m");
   SY(SYS_zone, x0 + ZONE_LOOKUP, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_zone10(void)
{
   GO(SYS_zone, "(ZONE_BOOT) 2s 0m");
   SY(SYS_zone, x0 + ZONE_BOOT, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone11(void)
{
   GO(SYS_zone, "(ZONE_SETATTR) 5s 1m");
   SY(SYS_zone, x0 + ZONE_SETATTR, x0, x0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_zone12(void)
{
   GO(SYS_zone, "(ZONE_ADD_DATALINK) 3s 0m");
   SY(SYS_zone, x0 + ZONE_ADD_DATALINK, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone13(void)
{
   GO(SYS_zone, "(ZONE_DEL_DATALINK) 3s 0m");
   SY(SYS_zone, x0 + ZONE_DEL_DATALINK, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone14(void)
{
   GO(SYS_zone, "(ZONE_CHECK_DATALINK) 3s 1m");
   SY(SYS_zone, x0 + ZONE_CHECK_DATALINK, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_zone15(void)
{
   GO(SYS_zone, "(ZONE_LIST_DATALINK) 4s 1m");
   SY(SYS_zone, x0 + ZONE_LIST_DATALINK, x0, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_zone16(void)
{
   int dlnum = x0 + 1;

   GO(SYS_zone, "(ZONE_LIST_DATALINK) 4s 1m");
   SY(SYS_zone, x0 + ZONE_LIST_DATALINK, x0, x0 + &dlnum, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_getpeername(void)
{
   GO(SYS_getpeername, "4s 1m");
   SY(SYS_getpeername, x0 - 1, x0 + 1, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_getpeername2(void)
{
   socklen_t size = x0 + 10;

   GO(SYS_getpeername, "4s 1m");
   SY(SYS_getpeername, x0 - 1, x0 + 1, &size, x0); FAILx(EBADF);
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_syscall                 0 */
   /* SPARC only. */

   /* SYS_exit                    1 */
   /* Tested below. */

   /* SYS_read                    3 */
   GO(SYS_read, "3s 0m");
   SY(SYS_read, x0 - 1, x0, x0 + 1); FAILx(EBADF);
   /* Note that above should be preferably "3s 1m" test.. */

   /* SYS_write                   4 */
   GO(SYS_write, "3s 1m");
   SY(SYS_write, x0 + 1, x0, x0 + 1); FAIL;

   /* SYS_open                    5 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_close                   6 */
   GO(SYS_close, "1s 0m");
   SY(SYS_close, x0 - 1); FAILx(EBADF);

   /* SYS_linkat                  7 */
   GO(SYS_linkat, "5s 2m");
   SY(SYS_linkat, x0 - 1, x0 + 1, x0 - 1, x0 + 1, x0); FAIL;

   /* SYS_link                    9 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_unlink                 10 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_symlinkat              11 */
   GO(SYS_symlinkat, "3s 2m");
   SY(SYS_symlinkat, x0 + 1, x0 - 1, x0 + 1); FAIL;

   /* SYS_chdir                  12 */
   GO(SYS_chdir, "1s 1m");
   SY(SYS_chdir, x0); FAIL;

   /* SYS_time                   13 */
   GO(SYS_time, "0s 0m");
   SY(SYS_time); SUCC;

   /* SYS_mknod                  14 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_chmod                  15 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_chown                  16 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_brk                    17 */
   GO(SYS_brk, "1s 0m");
   SY(SYS_brk, x0); SUCC;

   /* SYS_stat                   18 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_lseek                  19 */
   GO(SYS_lseek, "3s 0m");
   SY(SYS_lseek, x0 - 1, x0, x0); FAILx(EBADF);

   /* SYS_getpid                 20 */
   GO(SYS_getpid, "0s 0m");
   SY(SYS_getpid); SUCC;

   /* SYS_mount                  21 */
   sys_mount();
   sys_mount2();
   sys_mount3();
   sys_mount4();

   /* SYS_readlinkat             22 */
   GO(SYS_readlinkat, "4s 2m");
   SY(SYS_readlinkat, x0 - 1, x0, x0, x0 + 1); FAIL;

   /* SYS_setuid                 23 */
   GO(SYS_setuid, "1s 0m");
   SY(SYS_setuid, x0 - 1); FAIL;

   /* SYS_getuid                 24 */
   GO(SYS_getuid, "0s 0m");
   SY(SYS_getuid); SUCC;

   /* SYS_stime                  25 */
   GO(SYS_stime, "1s 0m");
   SY(SYS_stime, x0); FAIL;

   /* SYS_pcsample               26 */
   /* XXX Missing wrapper. */

   /* SYS_alarm                  27 */
   GO(SYS_alarm, "1s 0m");
   SY(SYS_alarm, x0); SUCC;

   /* SYS_fstat                  28 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_pause                  29 */
   /* Don't bother to test this. */
   GO(SYS_pause, "ignore");

   /* SYS_stty                   31 */
   GO(SYS_stty, "2s 1m");
   SY(SYS_stty, x0 - 1, x0 + 1); FAIL;

   /* SYS_gtty                   32 */
   GO(SYS_gtty, "2s 1m");
   SY(SYS_gtty, x0 - 1, x0 + 1); FAIL;

   /* SYS_access                 33 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_nice                   34 */
   /* XXX Missing wrapper. */

   /* SYS_statfs                 35 */
   /* XXX Missing wrapper. */

   /* SYS_sync                   36 */
   /* XXX Missing wrapper. */

   /* SYS_kill                   37 */
   GO(SYS_kill, "2s 0m");
   SY(SYS_kill, x0, x0); SUCC;

   /* SYS_fstatfs                38 */
   /* XXX Missing wrapper. */

   /* SYS_pgrpsys                39 */
   sys_pgrpsys();
   sys_pgrpsys2();
   sys_pgrpsys3();

   /* SYS_uucopystr              40 */
   /* XXX Missing wrapper. */

   /* SYS_pipe                   42 */
   /* Don't bother to test this. */
   GO(SYS_pipe, "ignore");

   /* SYS_times                  43 */
   GO(SYS_times, "1s 1m");
   SY(SYS_times, x0 + 1); FAIL;

   /* SYS_profil                 44 */
   /* XXX Missing wrapper. */

   /* SYS_faccessat              45 */
   GO(SYS_faccessat, "4s 1m");
   SY(SYS_faccessat, x0 - 1, x0 + 1, x0, x0); FAIL;

   /* SYS_setgid                 46 */
   GO(SYS_setgid, "1s 0m");
   SY(SYS_setgid, x0 - 1); FAIL;

   /* SYS_getgid                 47 */
   GO(SYS_getgid, "0s 0m");
   SY(SYS_getgid); SUCC;

   /* SYS_mknodat                48 */
   GO(SYS_mknodat, "4s 1m");
   SY(SYS_mknodat, x0 - 1, x0 + 1, x0, x0); FAIL;

   /* SYS_msgsys                 49 */
   /* XXX Missing wrapper. */

   /* SYS_sysi86                 50 */
   /* TODO Add test. */
   GO(SYS_sysi86, "incoming");

   /* SYS_acct                   51 */
   /* XXX Missing wrapper. */

   /* SYS_shmsys                 52 */
   sys_shmsys();
   sys_shmsys2();
   sys_shmsys3();
   sys_shmsys4();
   sys_shmsys5();
   sys_shmsys6();
   sys_shmsys7();
   sys_shmsys8();
   sys_shmsys9();
   sys_shmsys10();
   sys_shmsys11();
   sys_shmsys12();
   sys_shmsys13();

   /* SYS_semsys                 53 */
   sys_semsys();
   sys_semsys2();
   sys_semsys3();
   sys_semsys4();
   sys_semsys5();
   sys_semsys6();
   sys_semsys7();
   sys_semsys8();
   sys_semsys9();
   sys_semsys10();
   sys_semsys11();
   sys_semsys12();
   sys_semsys13();
   sys_semsys14();
   sys_semsys15();
   sys_semsys16();

   /* SYS_ioctl                  54 */
   GO(SYS_ioctl, "3s 1m");
   SY(SYS_ioctl, x0, x0 + TCGETS, x0); FAIL;

   /* SYS_uadmin                 55 */
   /* XXX Missing wrapper. */

   /* SYS_fchownat               56 */
   GO(SYS_fchownat, "5s 1m");
   SY(SYS_fchownat, x0 - 1, x0 + 1, x0, x0, x0); FAIL;

   /* SYS_utssys                 57 */
   /* XXX Missing wrapper. */

   /* SYS_fdsync                 58 */
   GO(SYS_fdsync, "2s 0m");
   SY(SYS_fdsync, x0 - 1, x0); FAILx(EBADF);

   /* SYS_execve                 59 */
   /* illumos ignores the fourth argument. */
   GO(SYS_execve, "3s 1m");
   SY(SYS_execve, x0, x0, x0, 0); FAIL;
   /* More cases tested in execx.c */

   /* SYS_umask                  60 */
   GO(SYS_umask, "1s 0m");
   SY(SYS_umask, x0 + 022); SUCC;

   /* SYS_chroot                 61 */
   GO(SYS_chroot, "1s 1m");
   SY(SYS_chroot, x0 + 1); FAIL;

   /* SYS_fcntl                  62 */
   sys_fcntl();
   sys_fcntl2();
   sys_fcntl3();
   sys_fcntl4();

   /* SYS_ulimit                 63 */
   /* XXX Missing wrapper. */

   /* SYS_renameat               64 */
   GO(SYS_renameat, "4s 2m");
   SY(SYS_renameat, x0 - 1, x0, x0, x0); FAIL;

   /* SYS_unlinkat               65 */
   GO(SYS_unlinkat, "3s 1m");
   SY(SYS_unlinkat, x0 - 1, x0, x0); FAIL;

   /* SYS_fstatat                66 */
   GO(SYS_fstatat, "4s 2m");
   SY(SYS_fstatat, x0 - 1, x0 + 1, x0, x0); FAIL;

   /* SYS_fstatat64              67 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_openat                 68 */
   sys_openat();
   sys_openat2();

   /* SYS_openat64               69 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_tasksys                70 */
   sys_tasksys();
   sys_tasksys2();
   sys_tasksys3();
   sys_tasksys4();

   /* SYS_acctctl                71 */
   /* XXX Missing wrapper. */

   /* SYS_exacctsys              72 */
   /* XXX Missing wrapper. */

   /* SYS_getpagesizes           73 */
   GO(SYS_getpagesizes, "3s 1m");
   SY(SYS_getpagesizes, x0, x0 + 1, x0 + 1); FAIL;

   /* SYS_rctlsys                74 */
   /* XXX Missing wrapper. */

   /* SYS_sidsys                 75 */
   /* XXX Missing wrapper. */

   /* SYS_lwp_park               77 */
   /* Don't bother to test this. */
   GO(SYS_lwp_park, "ignore");

   /* SYS_sendfilev              78 */
   sys_sendfilev();
   sys_sendfilev2();
   sys_sendfilev3();
   sys_sendfilev4();

   /* SYS_rmdir                  79 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_mkdir                  80 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_getdents               81 */
   GO(SYS_getdents, "3s 1m");
   SY(SYS_getdents, x0, x0, x0 + 1); FAIL;

   /* SYS_Privsys                82 */
   sys_privsys();
   sys_privsys2();
   sys_privsys3();
   sys_privsys4();
   sys_privsys5();
   sys_privsys6();
   sys_privsys7();
   sys_privsys8();

   /* SYS_ucredsys               83 */
   sys_ucredsys();
   sys_ucredsys2();

   /* SYS_sysfs                  84 */
   sys_sysfs();
   sys_sysfs2();
   sys_sysfs3();

   /* SYS_getmsg                 85 */
   GO(SYS_getmsg, "4s 1m");
   SY(SYS_getmsg, x0, x0, x0, x0); FAIL;

   /* SYS_putmsg                 86 */
   GO(SYS_putmsg, "4s 0m");
   SY(SYS_putmsg, x0, x0, x0, x0);

   /* SYS_lstat                  88 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_symlink                89 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_readlink               90 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_setgroups              91 */
   GO(SYS_setgroups, "2s 1m");
   SY(SYS_setgroups, x0 + 1, x0 + 1); FAIL;

   /* SYS_getgroups              92 */
   GO(SYS_getgroups, "2s 1m");
   SY(SYS_getgroups, x0 + 1, x0 + 1); FAIL;

   /* SYS_fchmod                 93 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_fchown                 94 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_sigprocmask            95 */
   GO(SYS_sigprocmask, "3s 2m");
   SY(SYS_sigprocmask, x0, x0 + 1, x0 + 1); FAILx(EFAULT);

   /* SYS_sigsuspend             96 */
   GO(SYS_sigsuspend, "1s 1m");
   SY(SYS_sigsuspend, x0 + 1); FAILx(EFAULT);

   /* SYS_sigaltstack            97 */
   GO(SYS_sigaltstack, "2s 2m");
   SY(SYS_sigaltstack, x0 + 1, x0 + 1); FAILx(EFAULT);

   /* SYS_sigaction              98 */
   GO(SYS_sigaction, "3s 4m");
   SY(SYS_sigaction, x0, x0 + 1, x0 + 1); FAILx(EFAULT);

   /* SYS_sigpending             99 */
   GO(SYS_sigpending, "2s 1m");
   SY(SYS_sigpending, x0, x0 + 1);

   /* SYS_context               100 */
   sys_context();
   sys_context2();
   sys_context3();
   sys_context4();

   /* SYS_fchmodat              101 */
   GO(SYS_fchmodat, "4s 1m");
   SY(SYS_fchmodat, x0 - 1, x0 + 1, x0, x0); FAIL;

   /* SYS_mkdirat               102 */
   GO(SYS_mkdirat, "3s 1m");
   SY(SYS_mkdirat, x0 - 1, x0, x0); FAIL;

   /* SYS_statvfs               103 */
   sys_statvfs();
   sys_statvfs2();

   /* SYS_fstatvfs              104 */
   GO(SYS_fstatvfs, "2s 1m");
   SY(SYS_fstatvfs, x0 - 1, x0 + 1); FAILx(EBADF);

   /* SYS_getloadavg            105 */
   /* XXX Missing wrapper. */

   /* SYS_nfssys                106 */
   sys_nfssys();
   /* :TODO: Add test cases when other opcodes are implemented. */

   /* SYS_waitid                107 */
   GO(SYS_waitid, "4s 1m");
   SY(SYS_waitid, x0 - 1, x0, x0, x0); FAIL;

   /* SYS_sigsendsys            108 */
   GO(SYS_sigsendsys, "2s 1m");
   SY(SYS_sigsendsys, x0 - 1, x0); FAIL;

   /* SYS_hrtsys                109 */
   /* XXX Missing wrapper. */

   /* SYS_utimesys              110 */
   /* SYS_utimensat             110 */
   /* Tested in scalar_utimesys and scalar_utimensat. */

   /* SYS_sigresend             111 */
   GO(SYS_sigresend, "3s 2m");
   SY(SYS_sigresend, x0, x0 + 1, x0); FAIL;

   /* SYS_priocntlsys           112 */
   sys_priocntlsys();
   sys_priocntlsys2();
   sys_priocntlsys3();
   sys_priocntlsys4();
   sys_priocntlsys5();
   sys_priocntlsys6();
   sys_priocntlsys7();
   sys_priocntlsys8();
   sys_priocntlsys9();
   sys_priocntlsys10();
   sys_priocntlsys11();

   /* SYS_pathconf              113 */
   GO(SYS_pathconf, "2s 1m");
   SY(SYS_pathconf, x0, x0); FAIL;

   /* SYS_mincore               114 */
   /* XXX Missing wrapper. */

   /* SYS_mmap                  115 */
   GO(SYS_mmap, "6s 0m");
   SY(SYS_mmap, x0, x0, x0, x0, x0, x0); FAILx(EINVAL);

   /* SYS_mprotect              116 */
   GO(SYS_mprotect, "3s 0m");
   SY(SYS_mprotect, x0, x0, x0); FAILx(EINVAL);

   /* SYS_munmap                117 */
   GO(SYS_munmap, "2s 0m");
   SY(SYS_munmap, x0, x0); FAILx(EINVAL);

   /* SYS_fpathconf             118 */
   /* XXX Missing wrapper. */

   /* SYS_vfork                 119 */
   /* XXX Missing wrapper. */

   /* SYS_fchdir                120 */
   GO(SYS_fchdir, "1s 0m");
   SY(SYS_fchdir, x0 - 1); FAILx(EBADF);

   /* SYS_readv                 121 */
   GO(SYS_readv, "3s 1m");
   SY(SYS_readv, x0, x0, x0 + 1); FAIL;

   /* SYS_writev                122 */
   GO(SYS_writev, "3s 1m");
   SY(SYS_writev, x0, x0, x0 + 1); FAIL;

   /* SYS_mmapobj               127 */
   GO(SYS_mmapobj, "5s 2m");
   SY(SYS_mmapobj, x0 - 1, x0 | MMOBJ_PADDING, x0, x0, x0); FAILx(EBADF);

   /* SYS_setrlimit             128 */
   GO(SYS_setrlimit, "2s 1m");
   SY(SYS_setrlimit, x0, x0); FAIL;

   /* SYS_getrlimit             129 */
   GO(SYS_getrlimit, "2s 1m");
   SY(SYS_getrlimit, x0, x0); FAIL;

   /* SYS_lchown                130 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_memcntl               131 */
   GO(SYS_memcntl, "6s 1m");
   SY(SYS_memcntl, x0, x0, x0 | MC_HAT_ADVISE, x0, x0, x0); FAIL;

   /* SYS_getpmsg               132 */
   GO(SYS_getpmsg, "5s 2m");
   SY(SYS_getpmsg, x0, x0, x0, x0, x0); FAIL;

   /* SYS_putpmsg               133 */
   GO(SYS_putpmsg, "5s 0m");
   SY(SYS_putpmsg, x0, x0, x0, x0, x0); FAIL;

   /* SYS_rename                134 */
   /* Tested in scalar_obsolete.c. */

   /* SYS_uname                 135 */
   sys_uname();
   sys_uname2();

   /* SYS_setegid               136 */
   GO(SYS_setegid, "1s 0m");
   SY(SYS_setegid, x0 - 1); FAIL;

   /* SYS_sysconfig             137 */
   GO(SYS_sysconfig, "1s 0m");
   SY(SYS_sysconfig, x0 - 1); FAIL;

   /* SYS_adjtime               138 */
   /* XXX Missing wrapper. */

   /* SYS_systeminfo            139 */
   GO(SYS_systeminfo, "3s 1m");
   SY(SYS_systeminfo, x0 + 1, x0, x0 + 1); FAIL;

   /* SYS_sharefs               140 */
   /* XXX Missing wrapper. */

   /* SYS_seteuid               141 */
   GO(SYS_seteuid, "1s 0m");
   SY(SYS_seteuid, x0 - 1); FAIL;

   /* SYS_forksys               142 */
   GO(SYS_forksys, "2s 0m");
   SY(SYS_forksys, x0, x0 - 1); FAIL;

   /* SYS_sigtimedwait          144 */
   GO(SYS_sigtimedwait, "3s 3m");
   SY(SYS_sigtimedwait, x0 - 1, x0 - 1, x0 - 1); FAIL;

   /* SYS_lwp_info              145 */
   /* XXX Missing wrapper. */

   /* SYS_yield                 146 */
   GO(SYS_yield, "0s 0m");
   SY(SYS_yield); SUCC;

   /* SYS_lwp_sema_post         148 */
   GO(SYS_lwp_sema_post, "1s 3m");
   SY(SYS_lwp_sema_post, x0); FAIL;

   /* SYS_lwp_sema_trywait      149 */
   /* XXX Missing wrapper. */

   /* SYS_lwp_detach            150 */
   GO(SYS_lwp_detach, "1s 0m");
   SY(SYS_lwp_detach, x0); FAIL;

   /* SYS_corectl               151 */
   /* XXX Missing wrapper. */

   /* SYS_modctl                152 */
   sys_modctl();
   sys_modctl2();
   sys_modctl3();

   /* SYS_fchroot               153 */
   GO(SYS_fchroot, "1s 0m");
   SY(SYS_fchroot, x0 - 1); FAILx(EBADF);

   /* SYS_vhangup               155 */
   /* XXX Missing wrapper. */

   /* SYS_gettimeofday          156 */
   GO(SYS_gettimeofday, "1s 1m");
   SY(SYS_gettimeofday, x0 + 1); FAIL;

   /* SYS_getitimer             157 */
   GO(SYS_getitimer, "2s 2m");
   SY(SYS_getitimer, x0, x0 + 1); FAIL;

   /* SYS_setitimer             158 */
   GO(SYS_setitimer, "3s 4m");
   SY(SYS_setitimer, x0, x0 + 1, x0 + 1); FAIL;

   /* SYS_lwp_create            159 */
   /* In the lwp_create() wrapper, we unfortunately have to call
      ML_(safe_to_deref) before doing the PRE_READ_*() stuff, therefore only 1m
      parameter. */
   GO(SYS_lwp_create, "3s 1m");
   SY(SYS_lwp_create, x0, x0, x0 + 1); FAILx(EINVAL);

   /* SYS_lwp_exit              160 */
   /* Don't bother to test this. */
   GO(SYS_lwp_exit, "ignore");

   /* SYS_lwp_suspend           161 */
   GO(SYS_lwp_suspend, "1s 0m");
   SY(SYS_lwp_suspend, x0 - 1); FAIL;

   /* SYS_lwp_continue          162 */
   GO(SYS_lwp_continue, "1s 0m");
   SY(SYS_lwp_continue, x0 - 1); FAIL;

   /* SYS_lwp_kill              163 */
   /* SYS_lwp_sigqueue          163 */
   /* Tested in scalar_lwp_kill and scalar_lwp_sigqueue. */

   /* SYS_lwp_self              164 */
   GO(SYS_lwp_self, "0s 0m");
   SY(SYS_lwp_self); SUCC;

   /* SYS_lwp_sigmask           165 */
   GO(SYS_lwp_sigmask, "5s 0m");
   SY(SYS_lwp_sigmask, x0, x0, x0, x0, x0); FAIL;

   /* SYS_lwp_private           166 */
   /* Tested in amd64-solaris/scalar and x86-solaris/scalar */

   /* SYS_lwp_wait              167 */
   GO(SYS_lwp_wait, "2s 1m");
   SY(SYS_lwp_wait, x0 - 1, x0 + 1); FAIL;

   /* SYS_lwp_mutex_wakeup      168 */
   GO(SYS_lwp_mutex_wakeup, "2s 2m");
   SY(SYS_lwp_mutex_wakeup, x0, x0); FAIL;

   /* SYS_lwp_cond_wait         170 */
   GO(SYS_lwp_cond_wait, "4s 5m");
   SY(SYS_lwp_cond_wait, x0 + 1, x0 + 1, x0 + 1, x0); FAIL;

   /* SYS_lwp_cond_signal       171 */
   GO(SYS_lwp_cond_signal, "1s 2m");
   SY(SYS_lwp_cond_signal, x0); FAIL;

   /* SYS_lwp_cond_broadcast    172 */
   GO(SYS_lwp_cond_broadcast, "1s 2m");
   SY(SYS_lwp_cond_broadcast, x0); FAIL;

   /* SYS_pread                 173 */
   GO(SYS_pread, "4s 1m");
   SY(SYS_pread, x0 - 1, x0, x0 + 1, x0); FAILx(EBADF);

   /* SYS_pwrite                174 */
   GO(SYS_pwrite, "4s 1m");
   SY(SYS_pwrite, x0 - 1, x0, x0 + 1, x0); FAILx(EBADF);

   /* SYS_llseek                175 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_inst_sync             176 */
   /* XXX Missing wrapper. */

   /* SYS_brand                 177 */
   /* XXX Missing wrapper. */

   /* SYS_kaio                  178 */
   /* XXX Missing wrapper. */

   /* SYS_cpc                   179 */
   /* XXX Missing wrapper. */

   /* SYS_lgrpsys               180 */
   sys_lgrpsys();
   sys_lgrpsys2();
   sys_lgrpsys3();
   sys_lgrpsys4();
   sys_lgrpsys5();
   sys_lgrpsys6();

   /* SYS_rusagesys             181 */
   sys_rusagesys();
   sys_rusagesys2();
   sys_rusagesys3();
   sys_rusagesys4();
   sys_rusagesys5();

   /* SYS_port                  182 */
   sys_port();
   sys_port2();
   sys_port3();
   sys_port4();
   sys_port5();
   sys_port6();
   sys_port7();
   sys_port8();
   sys_port9();
   sys_port10();
   sys_port11();

   /* SYS_pollsys               183 */
   GO(SYS_pollsys, "4s 5m");
   SY(SYS_pollsys, x0, x0 + 1, x0 + 1, x0 + 1); FAIL;

   /* SYS_labelsys              184 */
   sys_labelsys();
   sys_labelsys2();
   sys_labelsys3();
   sys_labelsys4();
   sys_labelsys5();
   sys_labelsys6();

   /* SYS_acl                   185 */
   sys_acl();
   sys_acl2();
   sys_acl3();
   sys_acl4();
   sys_acl5();
   sys_acl6();

   /* SYS_auditsys              186 */
   sys_auditsys();
   sys_auditsys2();
   sys_auditsys3();
   sys_auditsys4();
   sys_auditsys5();
   sys_auditsys6();
   sys_auditsys7();
   sys_auditsys8();
   sys_auditsys9();
   sys_auditsys10();
   sys_auditsys11();
   sys_auditsys12();
   sys_auditsys13();
   sys_auditsys14();
   sys_auditsys15();
   sys_auditsys16();
   sys_auditsys17();
   sys_auditsys18();
   sys_auditsys19();
   sys_auditsys20();
   sys_auditsys21();
   sys_auditsys22();
   sys_auditsys23();
   sys_auditsys24();
   sys_auditsys25();
   sys_auditsys26();
   sys_auditsys27();
   sys_auditsys28();
   sys_auditsys29();
   sys_auditsys30();

   /* SYS_processor_bind        187 */
   /* XXX Missing wrapper. */

   /* SYS_processor_info        188 */
   /* XXX Missing wrapper. */

   /* SYS_p_online              189 */
   GO(SYS_p_online, "2s 0m");
   SY(SYS_p_online, x0, x0); FAILx(EINVAL);

   /* SYS_sigqueue              190 */
   GO(SYS_sigqueue, "5s 1m");
   SY(SYS_sigqueue, x0 - 1, x0, x0 + 1, x0, x0 - 1); FAIL;

   /* SYS_clock_gettime         191 */
   GO(SYS_clock_gettime, "2s 1m");
   SY(SYS_clock_gettime, x0, x0); FAIL;

   /* SYS_clock_settime         192 */
   GO(SYS_clock_settime, "2s 1m");
   SY(SYS_clock_settime, x0, x0);  FAIL;

   /* SYS_clock_getres          193 */
   GO(SYS_clock_getres, "2s 1m");
   SY(SYS_clock_getres, x0 + 1, x0 + 1); FAIL;

   /* SYS_timer_create          194 */
   sys_timer_create();
   sys_timer_create2();

   /* SYS_timer_delete          195 */
   GO(SYS_timer_delete, "1s 0m");
   SY(SYS_timer_delete, x0 + 1); FAIL;

   /* SYS_timer_settime         196 */
   GO(SYS_timer_settime, "4s 2m");
   SY(SYS_timer_settime, x0, x0, x0 + 1, x0 + 2); FAIL;

   /* SYS_timer_gettime         197 */
   GO(SYS_timer_gettime, "2s 1m");
   SY(SYS_timer_gettime, x0, x0 + 1); FAIL;

   /* SYS_timer_getoverrun      198 */
   GO(SYS_timer_getoverrun, "1s 0m");
   SY(SYS_timer_getoverrun, x0); FAIL;

   /* SYS_nanosleep             199 */
   GO(SYS_nanosleep, "2s 2m");
   SY(SYS_nanosleep, x0, x0 + 1); FAIL;

   /* SYS_facl                  200 */
   sys_facl();
   sys_facl2();
   sys_facl3();
   sys_facl4();
   sys_facl5();
   sys_facl6();

   /* SYS_door                  201 */
   sys_door();
   sys_door2();
   sys_door3();
   sys_door4();
   sys_door5();
   sys_door6();
   /* XXX Additional sys_door variants still unimplemented. */

   /* SYS_setreuid              202 */
   GO(SYS_setreuid, "2s 0m");
   SY(SYS_setreuid, x0 - 1, x0 - 1); SUCC;

   /* SYS_setregid              203 */
   GO(SYS_setregid, "2s 0m");
   SY(SYS_setregid, x0 - 1, x0 - 1); SUCC;

   /* SYS_install_utrap         204 */
   /* XXX Missing wrapper. */

   /* SYS_signotify             205 */
   /* XXX Missing wrapper. */

   /* SYS_schedctl              206 */
   GO(SYS_schedctl, "0s 0m");
   SY(SYS_schedctl); SUCC;

   /* SYS_pset                  207 */
   sys_pset();
   sys_pset2();
   sys_pset3();
   sys_pset4();
   sys_pset5();
   sys_pset6();
   sys_pset7();
   sys_pset8();
   sys_pset9();
   sys_pset10();
   sys_pset11();
   sys_pset12();
   sys_pset13();

   /* SYS_sparc_utrap_install   208 */
   /* XXX Missing wrapper. */

   /* SYS_resolvepath           209 */
   GO(SYS_resolvepath, "3s 2m");
   SY(SYS_resolvepath, x0, x0, x0 + 1); FAIL;

   /* SYS_lwp_mutex_timedlock   210 */
   GO(SYS_lwp_mutex_timedlock, "3s 7m");
   SY(SYS_lwp_mutex_timedlock, x0, x0 + 1, x0); FAIL;

   /* SYS_lwp_sema_timedwait    211 */
   GO(SYS_lwp_sema_timedwait, "3s 4m");
   SY(SYS_lwp_sema_timedwait, x0, x0 + 1, x0); FAIL;

   /* SYS_lwp_rwlock_sys        212 */
   sys_lwp_rwlock();
   sys_lwp_rwlock2();
   sys_lwp_rwlock3();
   sys_lwp_rwlock4();
   sys_lwp_rwlock5();

   /* SYS_getdents64            213 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_mmap64                214 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_stat64                215 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_lstat64               216 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_fstat64               217 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_statvfs64             218 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_fstatvfs64            219 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_setrlimit64           220 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_getrlimit64           221 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_pread64               222 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_pwrite64              223 */
   /* Tested in x86-solaris/scalar.c. */

   /* SYS_open64                225 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_rpcsys                226 */
   /* XXX Missing wrapper. */

   /* SYS_zone                  227 */
   sys_zone();
   sys_zone2();
   sys_zone3();
   sys_zone4();
   sys_zone5();
   sys_zone6();
   sys_zone7();
   sys_zone8();
   sys_zone9();
   sys_zone10();
   sys_zone11();
   /*
   2013-09-22 Petr Pavlu -- The following test crashes my system because of
   a kernel bug (illumos), commenting it out for now.
   sys_zone12();
   */
   sys_zone13();
   /*
   2013-09-22 Petr Pavlu -- The following test provides an incorrect result
   because of a kernel bug (illumos), commenting it out for now.
   sys_zone14();
   */
   sys_zone15();
   /*
   2013-09-22 Petr Pavlu -- The following test provides an incorrect result
   because of a kernel bug (illumos), commenting it out for now.
   sys_zone16();
   */

   /* SYS_autofssys             228 */
   /* XXX Missing wrapper. */

   /* SYS_getcwd                229 */
   GO(SYS_getcwd, "2s 1m");
   SY(SYS_getcwd, x0, x0 + 1); FAIL;

   /* SYS_so_socket             230 */
   GO(SYS_so_socket, "5s 1m");
   SY(SYS_so_socket, x0, x0, x0, x0 + 1, x0); FAIL;

   /* SYS_so_socketpair         231 */
   GO(SYS_so_socketpair, "1s 1m");
   SY(SYS_so_socketpair, x0); FAIL;

   /* SYS_bind                  232 */
   GO(SYS_bind, "4s 0m");
   SY(SYS_bind, x0, x0, x0, x0); FAIL;

   /* SYS_listen                233 */
   GO(SYS_listen, "3s 0m");
   SY(SYS_listen, x0, x0, x0); FAIL;

   /* SYS_accept                234 */
   /* Illumos added a new version of the accept() syscall which takes an extra
      flag parameter.  This is trivially handled in the syscall wrapper but it
      is not tested here so it is not necessary to have two versions of the
      stderr.exp file. */
   GO(SYS_accept, "4s 0m");
   SY(SYS_accept, x0, x0, x0, x0, 0); FAIL;

   /* SYS_connect               235 */
   GO(SYS_connect, "4s 0m");
   SY(SYS_connect, x0, x0, x0, x0); FAIL;

   /* SYS_shutdown              236 */
   GO(SYS_shutdown, "3s 0m");
   SY(SYS_shutdown, x0 - 1, x0, x0); FAILx(EBADF);

   /* SYS_recv                  237 */
   GO(SYS_recv, "4s 1m");
   SY(SYS_recv, x0, x0, x0 + 1, x0); FAIL;

   /* SYS_recvfrom              238 */
   GO(SYS_recvfrom, "6s 1m");
   SY(SYS_recvfrom, x0, x0, x0 + 1, x0, x0, x0); FAIL;

   /* SYS_recvmsg               239 */
   GO(SYS_recvmsg, "3s 0m");
   SY(SYS_recvmsg, x0, x0, x0); FAIL;

   /* SYS_send                  240 */
   GO(SYS_send, "4s 1m");
   SY(SYS_send, x0, x0, x0 + 1, x0); FAIL;

   /* SYS_sendmsg               241 */
   GO(SYS_sendmsg, "3s 0m");
   SY(SYS_sendmsg, x0, x0, x0); FAIL;

   /* SYS_sendto                242 */
   GO(SYS_sendto, "6s 1m");
   SY(SYS_sendto, x0, x0, x0 + 1, x0, x0, x0); FAIL;

   /* SYS_getpeername           243 */
   sys_getpeername();
   sys_getpeername2();

   /* SYS_getsockname           244 */
   GO(SYS_getsockname, "4s 1m");
   SY(SYS_getsockname, x0, x0, x0, x0); FAIL;

   /* SYS_getsockopt            245 */
   GO(SYS_getsockopt, "6s 0m");
   SY(SYS_getsockopt, x0, x0, x0, x0, x0, x0); FAIL;

   /* SYS_setsockopt            246 */
   GO(SYS_setsockopt, "6s 1m");
   SY(SYS_setsockopt, x0, x0, x0, x0, x0 + 1, x0); FAIL;

   /* SYS_sockconfig            247 */
   /* XXX Missing wrapper. */

   /* SYS_ntp_gettime           248 */
   /* XXX Missing wrapper. */

   /* SYS_ntp_adjtime           249 */
   /* XXX Missing wrapper. */

   /* SYS_lwp_mutex_unlock      250 */
   /* XXX Missing wrapper. */

   /* SYS_lwp_mutex_trylock     251 */
   /* XXX Missing wrapper. */

   /* SYS_lwp_mutex_register    252 */
   GO(SYS_lwp_mutex_register, "2s 1m");
   SY(SYS_lwp_mutex_register, x0, x0); FAIL;

   /* SYS_cladm                 253 */
   /* XXX Missing wrapper. */

   /* SYS_uucopy                254 */
   GO(SYS_uucopy, "3s 2m");
   SY(SYS_uucopy, x0, x0, x0 + 1); FAIL;

   /* SYS_umount2               255 */
   GO(SYS_umount2, "2s 1m");
   SY(SYS_umount2, x0, x0); FAIL;

   /* No such syscall... */
#if 0
   GO(9999, "1e");
   SY(9999); FAIL;
#endif

   /* SYS_exit                    1 */
   GO(SYS_exit, "1s 0m");
   SY(SYS_exit, x0); FAIL;

   assert(0);
   return 0;
}

