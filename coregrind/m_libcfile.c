
/*--------------------------------------------------------------------*/
/*--- File- and socket-related libc stuff.            m_libcfile.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"     // VG_(sprintf)
#include "pub_core_libcproc.h"      // VG_(getpid), VG_(getppid)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"   // VG_(fd_hard_limit)
#include "pub_core_syscall.h"

/* IMPORTANT: on Darwin it is essential to use the _nocancel versions
   of syscalls rather than the vanilla version, if a _nocancel version
   is available.  See docs/internals/Darwin-notes.txt for the reason
   why. */

/* ---------------------------------------------------------------------
   File stuff
   ------------------------------------------------------------------ */

static inline Bool fd_exists(Int fd)
{
   struct vg_stat st;
   return VG_(fstat)(fd, &st) == 0;
}

/* Move an fd into the Valgrind-safe range */
Int VG_(safe_fd)(Int oldfd)
{
   Int newfd;

   vg_assert(VG_(fd_hard_limit) != -1);

   newfd = VG_(fcntl)(oldfd, VKI_F_DUPFD, VG_(fd_hard_limit));
   if (newfd != -1)
      VG_(close)(oldfd);

   /* Set the close-on-exec flag for this fd. */
   VG_(fcntl)(newfd, VKI_F_SETFD, VKI_FD_CLOEXEC);

   vg_assert(newfd >= VG_(fd_hard_limit));
   return newfd;
}

/* Given a file descriptor, attempt to deduce its filename.  To do
   this, we use /proc/self/fd/<FD>.  If this doesn't point to a file,
   or if it doesn't exist, we return False. */
Bool VG_(resolve_filename) ( Int fd, HChar* buf, Int n_buf )
{
#  if defined(VGO_linux)
   HChar tmp[64];
   VG_(sprintf)(tmp, "/proc/self/fd/%d", fd);
   VG_(memset)(buf, 0, n_buf);
   if (VG_(readlink)(tmp, buf, n_buf) > 0 && buf[0] == '/')
      return True;
   else
      return False;

#  elif defined(VGO_darwin)
   HChar tmp[VKI_MAXPATHLEN+1];
   if (0 == VG_(fcntl)(fd, VKI_F_GETPATH, (UWord)tmp)) {
      if (n_buf > 0) {
         VG_(strncpy)( buf, tmp, n_buf < sizeof(tmp) ? n_buf : sizeof(tmp) );
         buf[n_buf-1] = 0;
      }
      if (tmp[0] == '/') return True;
   }
   return False;

#  else
#     error Unknown OS
#  endif
}

SysRes VG_(mknod) ( const HChar* pathname, Int mode, UWord dev )
{  
#  if defined(VGP_arm64_linux)
   /* ARM64 wants to use __NR_mknodat rather than __NR_mknod. */
   SysRes res = VG_(do_syscall4)(__NR_mknodat,
                                 VKI_AT_FDCWD, (UWord)pathname, mode, dev);
#  elif defined(VGO_linux) || defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_mknod,
                                 (UWord)pathname, mode, dev);
#  else
#    error Unknown OS
#  endif
   return res;
}

SysRes VG_(open) ( const HChar* pathname, Int flags, Int mode )
{
#  if defined(VGP_arm64_linux)
   /* ARM64 wants to use __NR_openat rather than __NR_open. */
   SysRes res = VG_(do_syscall4)(__NR_openat,
                                 VKI_AT_FDCWD, (UWord)pathname, flags, mode);
#  elif defined(VGO_linux)
   SysRes res = VG_(do_syscall3)(__NR_open,
                                 (UWord)pathname, flags, mode);
#  elif defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_open_nocancel,
                                 (UWord)pathname, flags, mode);
#  else
#    error Unknown OS
#  endif
   return res;
}

Int VG_(fd_open) (const HChar* pathname, Int flags, Int mode)
{
   SysRes sr;
   sr = VG_(open) (pathname, flags, mode);
   if (sr_isError (sr))
      return -1;
   else
      return sr_Res (sr);
}

void VG_(close) ( Int fd )
{
   /* Hmm.  Return value is not checked.  That's uncool. */
#  if defined(VGO_linux)
   (void)VG_(do_syscall1)(__NR_close, fd);
#  elif defined(VGO_darwin)
   (void)VG_(do_syscall1)(__NR_close_nocancel, fd);
#  else
#    error Unknown OS
#  endif
}

Int VG_(read) ( Int fd, void* buf, Int count)
{
   Int    ret;
#  if defined(VGO_linux)
   SysRes res = VG_(do_syscall3)(__NR_read, fd, (UWord)buf, count);
#  elif defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_read_nocancel, fd, (UWord)buf, count);
#  else
#    error Unknown OS
#  endif
   if (sr_isError(res)) {
      ret = - (Int)(Word)sr_Err(res);
      vg_assert(ret < 0);
   } else {
      ret = (Int)(Word)sr_Res(res);
      vg_assert(ret >= 0);
   }
   return ret;
}

Int VG_(write) ( Int fd, const void* buf, Int count)
{
   Int    ret;
#  if defined(VGO_linux)
   SysRes res = VG_(do_syscall3)(__NR_write, fd, (UWord)buf, count);
#  elif defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_write_nocancel, fd, (UWord)buf, count);
#  else
#    error "Unknown OS"
#  endif
   if (sr_isError(res)) {
      ret = - (Int)(Word)sr_Err(res);
      vg_assert(ret < 0);
   } else {
      ret = (Int)(Word)sr_Res(res);
      vg_assert(ret >= 0);
   }
   return ret;
}


Int VG_(pipe) ( Int fd[2] )
{
#  if defined(VGP_mips32_linux) || defined(VGP_mips64_linux)
   /* __NR_pipe has a strange return convention on mips32-linux. */
   SysRes res = VG_(do_syscall1)(__NR_pipe, (UWord)fd);
   if (!sr_isError(res)) {
      fd[0] = (Int)sr_Res(res);
      fd[1] = (Int)sr_ResEx(res);
      return 0;
   } else {
      return -1;
   }
#  elif defined(VGP_arm64_linux)
   SysRes res = VG_(do_syscall2)(__NR_pipe2, (UWord)fd, 0);
   return sr_isError(res) ? -1 : 0;
#  elif defined(VGO_linux)
   SysRes res = VG_(do_syscall1)(__NR_pipe, (UWord)fd);
   return sr_isError(res) ? -1 : 0;
#  elif defined(VGO_darwin)
   /* __NR_pipe is UX64, so produces a double-word result */
   SysRes res = VG_(do_syscall0)(__NR_pipe);
   if (!sr_isError(res)) {
      fd[0] = (Int)sr_Res(res);
      fd[1] = (Int)sr_ResHI(res);
   }
   return sr_isError(res) ? -1 : 0;
#  else
#    error "Unknown OS"
#  endif
}

Off64T VG_(lseek) ( Int fd, Off64T offset, Int whence )
{
#  if defined(VGO_linux) || defined(VGP_amd64_darwin)
#  if defined(__NR__llseek)
   Off64T result;
   SysRes res = VG_(do_syscall5)(__NR__llseek, fd,
                                 offset >> 32, offset & 0xffffffff,
                                 (UWord)&result, whence);
   return sr_isError(res) ? (-1) : result;
#  else
   SysRes res = VG_(do_syscall3)(__NR_lseek, fd, offset, whence);
   vg_assert(sizeof(Off64T) == sizeof(Word));
   return sr_isError(res) ? (-1) : sr_Res(res);
#  endif
#  elif defined(VGP_x86_darwin)
   SysRes res = VG_(do_syscall4)(__NR_lseek, fd, 
                                 offset & 0xffffffff, offset >> 32, whence);
   return sr_isError(res) ? (-1) : sr_Res(res);
#  else
#    error "Unknown plat"
#  endif
   /* if you change the error-reporting conventions of this, also
      change all usage points. */
}


/* stat/fstat support.  It's uggerly.  We have impedance-match into a
   'struct vg_stat' in order to have a single structure that callers
   can use consistently on all platforms. */

#define TRANSLATE_TO_vg_stat(_p_vgstat, _p_vkistat) \
   do { \
      (_p_vgstat)->dev        = (ULong)( (_p_vkistat)->st_dev ); \
      (_p_vgstat)->ino        = (ULong)( (_p_vkistat)->st_ino ); \
      (_p_vgstat)->nlink      = (ULong)( (_p_vkistat)->st_nlink ); \
      (_p_vgstat)->mode       = (UInt) ( (_p_vkistat)->st_mode ); \
      (_p_vgstat)->uid        = (UInt) ( (_p_vkistat)->st_uid ); \
      (_p_vgstat)->gid        = (UInt) ( (_p_vkistat)->st_gid ); \
      (_p_vgstat)->rdev       = (ULong)( (_p_vkistat)->st_rdev ); \
      (_p_vgstat)->size       = (Long) ( (_p_vkistat)->st_size ); \
      (_p_vgstat)->blksize    = (ULong)( (_p_vkistat)->st_blksize ); \
      (_p_vgstat)->blocks     = (ULong)( (_p_vkistat)->st_blocks ); \
      (_p_vgstat)->atime      = (ULong)( (_p_vkistat)->st_atime ); \
      (_p_vgstat)->atime_nsec = (ULong)( (_p_vkistat)->st_atime_nsec ); \
      (_p_vgstat)->mtime      = (ULong)( (_p_vkistat)->st_mtime ); \
      (_p_vgstat)->mtime_nsec = (ULong)( (_p_vkistat)->st_mtime_nsec ); \
      (_p_vgstat)->ctime      = (ULong)( (_p_vkistat)->st_ctime ); \
      (_p_vgstat)->ctime_nsec = (ULong)( (_p_vkistat)->st_ctime_nsec ); \
   } while (0)

SysRes VG_(stat) ( const HChar* file_name, struct vg_stat* vgbuf )
{
   SysRes res;
   VG_(memset)(vgbuf, 0, sizeof(*vgbuf));

#  if defined(VGO_linux) || defined(VGO_darwin)
   /* First try with stat64.  If that doesn't work out, fall back to
      the vanilla version. */
#  if defined(__NR_stat64)
   { struct vki_stat64 buf64;
     res = VG_(do_syscall2)(__NR_stat64, (UWord)file_name, (UWord)&buf64);
     if (!(sr_isError(res) && sr_Err(res) == VKI_ENOSYS)) {
        /* Success, or any failure except ENOSYS */
        if (!sr_isError(res))
           TRANSLATE_TO_vg_stat(vgbuf, &buf64);
        return res;
     }
   }
#  endif /* defined(__NR_stat64) */
   /* This is the fallback ("vanilla version"). */
   { struct vki_stat buf;
#    if defined(VGP_arm64_linux)
     res = VG_(do_syscall3)(__NR3264_fstatat, VKI_AT_FDCWD,
                                              (UWord)file_name, (UWord)&buf);
#    else
     res = VG_(do_syscall2)(__NR_stat, (UWord)file_name, (UWord)&buf);
#    endif
     if (!sr_isError(res))
        TRANSLATE_TO_vg_stat(vgbuf, &buf);
     return res;
   }

#  else
#    error Unknown OS
#  endif
}

Int VG_(fstat) ( Int fd, struct vg_stat* vgbuf )
{
   SysRes res;
   VG_(memset)(vgbuf, 0, sizeof(*vgbuf));

#  if defined(VGO_linux)  ||  defined(VGO_darwin)
   /* First try with fstat64.  If that doesn't work out, fall back to
      the vanilla version. */
#  if defined(__NR_fstat64)
   { struct vki_stat64 buf64;
     res = VG_(do_syscall2)(__NR_fstat64, (UWord)fd, (UWord)&buf64);
     if (!(sr_isError(res) && sr_Err(res) == VKI_ENOSYS)) {
        /* Success, or any failure except ENOSYS */
        if (!sr_isError(res))
           TRANSLATE_TO_vg_stat(vgbuf, &buf64);
        return sr_isError(res) ? (-1) : 0;
     }
   }
#  endif /* if defined(__NR_fstat64) */
   { struct vki_stat buf;
     res = VG_(do_syscall2)(__NR_fstat, (UWord)fd, (UWord)&buf);
     if (!sr_isError(res))
        TRANSLATE_TO_vg_stat(vgbuf, &buf);
     return sr_isError(res) ? (-1) : 0;
   }

#  else
#    error Unknown OS
#  endif
}

#undef TRANSLATE_TO_vg_stat


Long VG_(fsize) ( Int fd )
{
   struct vg_stat buf;
   Int res = VG_(fstat)( fd, &buf );
   return (res == -1) ? (-1LL) : buf.size;
}

SysRes VG_(getxattr) ( const HChar* file_name, const HChar* attr_name, Addr attr_value, SizeT attr_value_len )
{
   SysRes res;
#if defined(VGO_linux)
   res = VG_(do_syscall4)(__NR_getxattr, (UWord)file_name, (UWord)attr_name,
                          attr_value, attr_value_len);
#else
   res = VG_(mk_SysRes_Error)(VKI_ENOSYS);
#endif
   return res;
}

Bool VG_(is_dir) ( const HChar* f )
{
   struct vg_stat buf;
   SysRes res = VG_(stat)(f, &buf);
   return sr_isError(res) ? False
                      : VKI_S_ISDIR(buf.mode) ? True : False;
}

SysRes VG_(dup) ( Int oldfd )
{
   return VG_(do_syscall1)(__NR_dup, oldfd);
}

SysRes VG_(dup2) ( Int oldfd, Int newfd )
{
#  if defined(VGO_linux) || defined(VGO_darwin)
   return VG_(do_syscall2)(__NR_dup2, oldfd, newfd);
#  else
#    error Unknown OS
#  endif
}

/* Returns -1 on error. */
Int VG_(fcntl) ( Int fd, Int cmd, Addr arg )
{
#  if defined(VGO_linux)
   SysRes res = VG_(do_syscall3)(__NR_fcntl, fd, cmd, arg);
#  elif defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_fcntl_nocancel, fd, cmd, arg);
#  else
#    error "Unknown OS"
#  endif
   return sr_isError(res) ? -1 : sr_Res(res);
}

Int VG_(rename) ( const HChar* old_name, const HChar* new_name )
{
   SysRes res = VG_(do_syscall2)(__NR_rename, (UWord)old_name, (UWord)new_name);
   return sr_isError(res) ? (-1) : 0;
}

Int VG_(unlink) ( const HChar* file_name )
{
#  if defined(VGP_arm64_linux)
   SysRes res = VG_(do_syscall2)(__NR_unlinkat, VKI_AT_FDCWD,
                                                (UWord)file_name);
#  else
   SysRes res = VG_(do_syscall1)(__NR_unlink, (UWord)file_name);
#  endif
   return sr_isError(res) ? (-1) : 0;
}

/* The working directory at startup.  AIX doesn't provide an easy
   system call to do getcwd, but fortunately we don't need arbitrary
   getcwd support.  All that is really needed is to note the cwd at
   process startup.  Hence VG_(record_startup_wd) notes it (in a
   platform dependent way) and VG_(get_startup_wd) produces the noted
   value.  Hence: */
static HChar startup_wd[VKI_PATH_MAX];
static Bool  startup_wd_acquired = False;

/* Record the process' working directory at startup.  Is intended to
   be called exactly once, at startup, before the working directory
   changes.  Return True for success, False for failure, so that the
   caller can bomb out suitably without creating module cycles if
   there is a problem. */
Bool VG_(record_startup_wd) ( void )
{
   const Int szB = sizeof(startup_wd);
   vg_assert(!startup_wd_acquired);
   vg_assert(szB >= 512 && szB <= 16384/*let's say*/); /* stay sane */
   VG_(memset)(startup_wd, 0, szB);
#  if defined(VGO_linux)
   /* Simple: just ask the kernel */
   { SysRes res
        = VG_(do_syscall2)(__NR_getcwd, (UWord)startup_wd, szB-1);
     vg_assert(startup_wd[szB-1] == 0);
     if (sr_isError(res)) {
        return False;
     } else {
        startup_wd_acquired = True;
        return True;
     }
   }
#  elif defined(VGO_darwin)
   /* We can't ask the kernel, so instead rely on launcher-*.c to
      tell us the startup path.  Note the env var is keyed to the
      parent's PID, not ours, since our parent is the launcher
      process. */
   { HChar  envvar[100];
     HChar* wd = NULL;
     VG_(memset)(envvar, 0, sizeof(envvar));
     VG_(sprintf)(envvar, "VALGRIND_STARTUP_PWD_%d_XYZZY", 
                          (Int)VG_(getppid)());
     wd = VG_(getenv)( envvar );
     if (wd == NULL || (1+VG_(strlen)(wd) >= szB))
        return False;
     VG_(strncpy_safely)(startup_wd, wd, szB);
     vg_assert(startup_wd[szB-1] == 0);
     startup_wd_acquired = True;
     return True;
   }
#  else
#    error Unknown OS
#  endif
}

/* Copy the previously acquired startup_wd into buf[0 .. size-1],
   or return False if buf isn't big enough. */
Bool VG_(get_startup_wd) ( HChar* buf, SizeT size )
{
   vg_assert(startup_wd_acquired);
   vg_assert(startup_wd[ sizeof(startup_wd)-1 ] == 0);
   if (1+VG_(strlen)(startup_wd) >= size)
      return False;
   VG_(strncpy_safely)(buf, startup_wd, size);
   return True;
}

SysRes VG_(poll) (struct vki_pollfd *fds, Int nfds, Int timeout)
{
   SysRes res;
#  if defined(VGP_arm64_linux)
   /* ARM64 wants to use __NR_ppoll rather than __NR_poll. */
   struct vki_timespec timeout_ts;
   if (timeout >= 0) {
      timeout_ts.tv_sec = timeout / 1000;
      timeout_ts.tv_nsec = ((long)timeout % 1000) * 1000000;
   }
   res = VG_(do_syscall4)(__NR_ppoll,
                          (UWord)fds, nfds, 
                          (UWord)(timeout >= 0 ? &timeout_ts : NULL),
                          (UWord)NULL);
#  elif defined(VGO_linux)
   res = VG_(do_syscall3)(__NR_poll, (UWord)fds, nfds, timeout);
#  elif defined(VGO_darwin)
   res = VG_(do_syscall3)(__NR_poll_nocancel, (UWord)fds, nfds, timeout);
#  else
#    error "Unknown OS"
#  endif
   return res;
}


Int VG_(readlink) (const HChar* path, HChar* buf, UInt bufsiz)
{
   SysRes res;
   /* res = readlink( path, buf, bufsiz ); */
#  if defined(VGP_arm64_linux)
   res = VG_(do_syscall4)(__NR_readlinkat, VKI_AT_FDCWD,
                                           (UWord)path, (UWord)buf, bufsiz);
#  else
   res = VG_(do_syscall3)(__NR_readlink, (UWord)path, (UWord)buf, bufsiz);
#  endif
   return sr_isError(res) ? -1 : sr_Res(res);
}

Int VG_(getdents) (Int fd, struct vki_dirent *dirp, UInt count)
{
#  if defined(VGO_linux)
   SysRes res;
   /* res = getdents( fd, dirp, count ); */
   res = VG_(do_syscall3)(__NR_getdents, fd, (UWord)dirp, count);
   return sr_isError(res) ? -1 : sr_Res(res);
#  elif defined(VGO_darwin)
   I_die_here;
#  else
#    error "Unknown OS"
#  endif
}

/* Check accessibility of a file.  Returns zero for access granted,
   nonzero otherwise. */
Int VG_(access) ( const HChar* path, Bool irusr, Bool iwusr, Bool ixusr )
{
#  if defined(VGO_linux)
   /* Very annoyingly, I cannot find any definition for R_OK et al in
      the kernel interfaces.  Therefore I reluctantly resort to
      hardwiring in these magic numbers that I determined by
      experimentation. */
#  define VKI_R_OK 4
#  define VKI_W_OK 2
#  define VKI_X_OK 1
#  endif

   UWord w = (irusr ? VKI_R_OK : 0)
             | (iwusr ? VKI_W_OK : 0)
             | (ixusr ? VKI_X_OK : 0);
#  if defined(VGP_arm64_linux)
   SysRes res = VG_(do_syscall3)(__NR_faccessat, VKI_AT_FDCWD, (UWord)path, w);
#  else
   SysRes res = VG_(do_syscall2)(__NR_access, (UWord)path, w);
#  endif
   return sr_isError(res) ? 1 : 0;   

#  if defined(VGO_linux)
#  undef VKI_R_OK
#  undef VKI_W_OK
#  undef VKI_X_OK
#  endif
}

/* 
   Emulate the normal Unix permissions checking algorithm.

   If owner matches, then use the owner permissions, else
   if group matches, then use the group permissions, else
   use other permissions.

   Note that we can't deal properly with SUID/SGID.  By default
   (allow_setuid == False), we refuse to run them (otherwise the
   executable may misbehave if it doesn't have the permissions it
   thinks it does).  However, the caller may indicate that setuid
   executables are allowed, for example if we are going to exec them
   but not trace into them (iow, client sys_execve when
   clo_trace_children == False).

   If VKI_EACCES is returned (iow, permission was refused), then
   *is_setuid is set to True iff permission was refused because the
   executable is setuid.
*/
/* returns: 0 = success, non-0 is failure */
Int VG_(check_executable)(/*OUT*/Bool* is_setuid,
                          const HChar* f, Bool allow_setuid)
{
   struct vg_stat st;
   SysRes res = VG_(stat)(f, &st);

   if (is_setuid)
      *is_setuid = False;

   if (sr_isError(res)) {
      return sr_Err(res);
   }

   if ( (st.mode & (VKI_S_ISUID | VKI_S_ISGID)) && !allow_setuid ) {
      if (is_setuid)
         *is_setuid = True;
      return VKI_EACCES;
   }

   res = VG_(getxattr)(f, "security.capability", (Addr)0, 0);
   if (!sr_isError(res) && !allow_setuid) {
      if (is_setuid)
         *is_setuid = True;
      return VKI_EACCES;
   }

   if (VG_(geteuid)() == st.uid) {
      if (!(st.mode & VKI_S_IXUSR))
         return VKI_EACCES;
   } else {
      Int grpmatch = 0;

      if (VG_(getegid)() == st.gid)
	 grpmatch = 1;
      else {
	 UInt groups[32];
	 Int ngrp = VG_(getgroups)(32, groups);
	 Int i;
         /* ngrp will be -1 if VG_(getgroups) failed. */
         for (i = 0; i < ngrp; i++) {
	    if (groups[i] == st.gid) {
	       grpmatch = 1;
	       break;
	    }
         }
      }

      if (grpmatch) {
	 if (!(st.mode & VKI_S_IXGRP)) {
            return VKI_EACCES;
         }
      } else if (!(st.mode & VKI_S_IXOTH)) {
         return VKI_EACCES;
      }
   }

   return 0;
}

SysRes VG_(pread) ( Int fd, void* buf, Int count, OffT offset )
{
   SysRes res;
   // on 32 bits platforms, we receive a 32 bits OffT but
   // we must extend it to pass a long long 64 bits.
#  if defined(VGP_x86_linux)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall5)(__NR_pread64, fd, (UWord)buf, count, 
                          offset, 0); // Little endian long long
   return res;
#  elif defined(VGP_arm_linux)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall5)(__NR_pread64, fd, (UWord)buf, count, 
                          0, offset); // Big endian long long
   return res;
#  elif defined(VGP_ppc32_linux)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall6)(__NR_pread64, fd, (UWord)buf, count, 
                          0, // Padding needed on PPC32
                          0, offset); // Big endian long long
   return res;
#  elif defined(VGP_mips32_linux) && (VKI_LITTLE_ENDIAN)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall6)(__NR_pread64, fd, (UWord)buf, count, 
                          0, offset, 0);
   return res;
#  elif defined(VGP_mips32_linux) && (VKI_BIG_ENDIAN)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall6)(__NR_pread64, fd, (UWord)buf, count, 
                          0, 0, offset);
   return res;
#  elif defined(VGP_amd64_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux) \
      || defined(VGP_mips64_linux) \
      || defined(VGP_arm64_linux)
   res = VG_(do_syscall4)(__NR_pread64, fd, (UWord)buf, count, offset);
   return res;
#  elif defined(VGP_amd64_darwin)
   vg_assert(sizeof(OffT) == 8);
   res = VG_(do_syscall4)(__NR_pread_nocancel, fd, (UWord)buf, count, offset);
   return res;
#  elif defined(VGP_x86_darwin)
   vg_assert(sizeof(OffT) == 8);
   res = VG_(do_syscall5)(__NR_pread_nocancel, fd, (UWord)buf, count, 
                          offset & 0xffffffff, offset >> 32);
   return res;
#  else
#    error "Unknown platform"
#  endif
}

/* Return the name of a directory for temporary files. */
const HChar *VG_(tmpdir)(void)
{
   const HChar *tmpdir;

   tmpdir = VG_(getenv)("TMPDIR");
   if (tmpdir == NULL || *tmpdir == '\0') tmpdir = VG_TMPDIR;
   if (tmpdir == NULL || *tmpdir == '\0') tmpdir = "/tmp";    /* fallback */

   return tmpdir;
}

static const HChar *mkstemp_format = "%s/valgrind_%s_%08x";

SizeT VG_(mkstemp_fullname_bufsz) ( SizeT part_of_name_len )
{
   return VG_(strlen)(mkstemp_format)
      + VG_(strlen)(VG_(tmpdir)()) - 2 // %s tmpdir
      + part_of_name_len - 2           // %s part_of_name
      + 8 - 4                          // %08x
      + 1;                             // trailing 0
}


/* Create and open (-rw------) a tmp file name incorporating said arg.
   Returns -1 on failure, else the fd of the file.  If fullname is
   non-NULL, the file's name is written into it.  The number of bytes
   written is equal to VG_(mkstemp_fullname_bufsz)(part_of_name). */

Int VG_(mkstemp) ( HChar* part_of_name, /*OUT*/HChar* fullname )
{
   HChar  buf[VG_(mkstemp_fullname_bufsz)(VG_(strlen)(part_of_name))];
   Int    n, tries, fd;
   UInt   seed;
   SysRes sres;
   const HChar *tmpdir;

   vg_assert(part_of_name);
   n = VG_(strlen)(part_of_name);
   vg_assert(n > 0 && n < 100);

   seed = (VG_(getpid)() << 9) ^ VG_(getppid)();

   /* Determine sensible location for temporary files */
   tmpdir = VG_(tmpdir)();

   tries = 0;
   while (True) {
      if (tries++ > 10) 
         return -1;
      VG_(sprintf)( buf, "%s/valgrind_%s_%08x",
                    tmpdir, part_of_name, VG_(random)( &seed ));
      if (0)
         VG_(printf)("VG_(mkstemp): trying: %s\n", buf);

      sres = VG_(open)(buf,
                       VKI_O_CREAT|VKI_O_RDWR|VKI_O_EXCL|VKI_O_TRUNC,
                       VKI_S_IRUSR|VKI_S_IWUSR);
      if (sr_isError(sres)) {
         VG_(umsg)("VG_(mkstemp): failed to create temp file: %s\n", buf);
         continue;
      }
      /* VG_(safe_fd) doesn't return if it fails. */
      fd = VG_(safe_fd)( sr_Res(sres) );
      if (fullname)
         VG_(strcpy)( fullname, buf );
      return fd;
   }
   /* NOTREACHED */
}


/* ---------------------------------------------------------------------
   Socket-related stuff.
   ------------------------------------------------------------------ */

static
Int parse_inet_addr_and_port ( const HChar* str, UInt* ip_addr, UShort* port );

static
Int my_connect ( Int sockfd, struct vki_sockaddr_in* serv_addr, Int addrlen );

UInt VG_(htonl) ( UInt x )
{
#  if defined(VG_BIGENDIAN)
   return x;
#  else
   return
      (((x >> 24) & 0xFF) << 0) | (((x >> 16) & 0xFF) << 8)
      | (((x >> 8) & 0xFF) << 16) | (((x >> 0) & 0xFF) << 24);
#  endif
}

UInt VG_(ntohl) ( UInt x )
{
#  if defined(VG_BIGENDIAN)
   return x;
#  else
   return
      (((x >> 24) & 0xFF) << 0) | (((x >> 16) & 0xFF) << 8)
      | (((x >> 8) & 0xFF) << 16) | (((x >> 0) & 0xFF) << 24);
#  endif
}

UShort VG_(htons) ( UShort x )
{
#  if defined(VG_BIGENDIAN)
   return x;
#  else
   return
      (((x >> 8) & 0xFF) << 0) | (((x >> 0) & 0xFF) << 8);
#  endif
}

UShort VG_(ntohs) ( UShort x )
{
#  if defined(VG_BIGENDIAN)
   return x;
#  else
   return
      (((x >> 8) & 0xFF) << 0) | (((x >> 0) & 0xFF) << 8);
#  endif
}


/* The main function. 

   Supplied string contains either an ip address "192.168.0.1" or
   an ip address and port pair, "192.168.0.1:1500".  Parse these,
   and return:
     -1 if there is a parse error
     -2 if no parse error, but specified host:port cannot be opened
     the relevant file (socket) descriptor, otherwise.
 is used.
*/
Int VG_(connect_via_socket)( const HChar* str )
{
#  if defined(VGO_linux) || defined(VGO_darwin)
   Int sd, res;
   struct vki_sockaddr_in servAddr;
   UInt   ip   = 0;
   UShort port = VG_CLO_DEFAULT_LOGPORT;
   Bool   ok   = parse_inet_addr_and_port(str, &ip, &port);
   if (!ok) 
      return -1;

   //if (0)
   //   VG_(printf)("ip = %d.%d.%d.%d, port %d\n",
   //               (ip >> 24) & 0xFF, (ip >> 16) & 0xFF, 
   //               (ip >> 8) & 0xFF, ip & 0xFF, 
   //               (UInt)port );

   servAddr.sin_family = VKI_AF_INET;
   servAddr.sin_addr.s_addr = VG_(htonl)(ip);
   servAddr.sin_port = VG_(htons)(port);

   /* create socket */
   sd = VG_(socket)(VKI_AF_INET, VKI_SOCK_STREAM, 0 /* IPPROTO_IP ? */);
   if (sd < 0) {
      /* this shouldn't happen ... nevertheless */
      return -2;
   }
		
   /* connect to server */
   res = my_connect(sd, &servAddr, sizeof(servAddr));
   if (res < 0) {
      /* connection failed */
      return -2;
   }

   return sd;

#  else
#    error "Unknown OS"
#  endif
}


/* Let d = one or more digits.  Accept either:
   d.d.d.d  or  d.d.d.d:d
*/
static Int parse_inet_addr_and_port ( const HChar* str, UInt* ip_addr, UShort* port )
{
#  define GET_CH ((*str) ? (*str++) : 0)
   UInt ipa, i, j, c, any;
   ipa = 0;
   for (i = 0; i < 4; i++) {
      j = 0;
      any = 0;
      while (1) {
         c = GET_CH; 
         if (c < '0' || c > '9') break;
         j = 10 * j + (int)(c - '0');
         any = 1;
      }
      if (any == 0 || j > 255) goto syntaxerr;
      ipa = (ipa << 8) + j;
      if (i <= 2 && c != '.') goto syntaxerr;
   }
   if (c == 0 || c == ':') 
      *ip_addr = ipa;
   if (c == 0) goto ok;
   if (c != ':') goto syntaxerr;
   j = 0;
   any = 0;
   while (1) {
      c = GET_CH; 
      if (c < '0' || c > '9') break;
      j = j * 10 + (int)(c - '0');
      any = 1;
      if (j > 65535) goto syntaxerr;
   }
   if (any == 0 || c != 0) goto syntaxerr;
   if (j < 1024) goto syntaxerr;
   *port = (UShort)j;
 ok:
   return 1;
 syntaxerr:
   return 0;
#  undef GET_CH
}

// GrP fixme safe_fd?
Int VG_(socket) ( Int domain, Int type, Int protocol )
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux)
   SysRes res;
   UWord  args[3];
   args[0] = domain;
   args[1] = type;
   args[2] = protocol;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_SOCKET, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
        || defined(VGP_arm64_linux)
   SysRes res;
   res = VG_(do_syscall3)(__NR_socket, domain, type, protocol );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall3)(__NR_socket, domain, type, protocol);
   if (!sr_isError(res)) {
       // Set SO_NOSIGPIPE so write() returns EPIPE instead of raising SIGPIPE
       Int optval = 1;
       SysRes res2;
       res2 = VG_(do_syscall5)(__NR_setsockopt, sr_Res(res), VKI_SOL_SOCKET, 
                               VKI_SO_NOSIGPIPE, (UWord)&optval, 
                               sizeof(optval));
       // ignore setsockopt() error
   }
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown arch"
#  endif
}


static
Int my_connect ( Int sockfd, struct vki_sockaddr_in* serv_addr, Int addrlen )
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux)
   SysRes res;
   UWord  args[3];
   args[0] = sockfd;
   args[1] = (UWord)serv_addr;
   args[2] = addrlen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_CONNECT, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
        || defined(VGP_arm64_linux)
   SysRes res;
   res = VG_(do_syscall3)(__NR_connect, sockfd, (UWord)serv_addr, addrlen);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall3)(__NR_connect_nocancel,
                          sockfd, (UWord)serv_addr, addrlen);
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown arch"
#  endif
}

Int VG_(write_socket)( Int sd, const void *msg, Int count )
{
   /* This is actually send(). */

   /* For Linux, VKI_MSG_NOSIGNAL is a request not to send SIGPIPE on 
      errors on stream oriented sockets when the other end breaks the
      connection. The EPIPE error is still returned.

      For Darwin, VG_(socket)() sets SO_NOSIGPIPE to get EPIPE instead of 
      SIGPIPE */

#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux)
   SysRes res;
   UWord  args[4];
   args[0] = sd;
   args[1] = (UWord)msg;
   args[2] = count;
   args[3] = VKI_MSG_NOSIGNAL;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_SEND, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
        || defined(VGP_arm64_linux)
   SysRes res;
   res = VG_(do_syscall6)(__NR_sendto, sd, (UWord)msg, 
                                       count, VKI_MSG_NOSIGNAL, 0,0);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
   SysRes res;
   res = VG_(do_syscall3)(__NR_write_nocancel, sd, (UWord)msg, count);
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}

Int VG_(getsockname) ( Int sd, struct vki_sockaddr *name, Int *namelen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux) \
      || defined(VGP_mips32_linux)
   SysRes res;
   UWord  args[3];
   args[0] = sd;
   args[1] = (UWord)name;
   args[2] = (UWord)namelen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETSOCKNAME, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips64_linux) || defined(VGP_arm64_linux)
   SysRes res;
   res = VG_(do_syscall3)( __NR_getsockname,
                           (UWord)sd, (UWord)name, (UWord)namelen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall3)( __NR_getsockname,
                           (UWord)sd, (UWord)name, (UWord)namelen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}

Int VG_(getpeername) ( Int sd, struct vki_sockaddr *name, Int *namelen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux) \
      || defined(VGP_mips32_linux)
   SysRes res;
   UWord  args[3];
   args[0] = sd;
   args[1] = (UWord)name;
   args[2] = (UWord)namelen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETPEERNAME, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips64_linux) || defined(VGP_arm64_linux)
   SysRes res;
   res = VG_(do_syscall3)( __NR_getpeername,
                           (UWord)sd, (UWord)name, (UWord)namelen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall3)( __NR_getpeername,
                           (UWord)sd, (UWord)name, (UWord)namelen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}

Int VG_(getsockopt) ( Int sd, Int level, Int optname, void *optval,
                      Int *optlen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux)
   SysRes res;
   UWord  args[5];
   args[0] = sd;
   args[1] = level;
   args[2] = optname;
   args[3] = (UWord)optval;
   args[4] = (UWord)optlen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETSOCKOPT, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
        || defined(VGP_arm64_linux)
   SysRes res;
   res = VG_(do_syscall5)( __NR_getsockopt,
                           (UWord)sd, (UWord)level, (UWord)optname, 
                           (UWord)optval, (UWord)optlen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall5)( __NR_getsockopt,
                           (UWord)sd, (UWord)level, (UWord)optname, 
                           (UWord)optval, (UWord)optlen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}


Int VG_(setsockopt) ( Int sd, Int level, Int optname, void *optval,
                      Int optlen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64_linux) || defined(VGP_s390x_linux)
   SysRes res;
   UWord  args[5];
   args[0] = sd;
   args[1] = level;
   args[2] = optname;
   args[3] = (UWord)optval;
   args[4] = (UWord)optlen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_SETSOCKOPT, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
        || defined(VGP_arm64_linux)
   SysRes res;
   res = VG_(do_syscall5)( __NR_setsockopt,
                           (UWord)sd, (UWord)level, (UWord)optname, 
                           (UWord)optval, (UWord)optlen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall5)( __NR_setsockopt,
                           (UWord)sd, (UWord)level, (UWord)optname, 
                           (UWord)optval, (UWord)optlen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}


const HChar *VG_(basename)(const HChar *path)
{
   static HChar buf[VKI_PATH_MAX];
   
   const HChar *p, *end;

   if (path == NULL  ||  
       0 == VG_(strcmp)(path, ""))
   {
      return ".";
   }

   p = path + VG_(strlen)(path);
   while (p > path  &&  *p == '/') {
      // skip all trailing '/'
      p--;
   }

   if (p == path  &&  *p == '/') return "/"; // all slashes

   end = p;

   while (p > path  &&  *p != '/') {
      // now skip non '/'
      p--;
   }

   if (*p == '/') p++;

   VG_(strncpy)(buf, p, end-p+1);
   buf[end-p+1] = '\0';

   return buf;
}


const HChar *VG_(dirname)(const HChar *path)
{
   static HChar buf[VKI_PATH_MAX];
    
   const HChar *p;

   if (path == NULL  ||  
       0 == VG_(strcmp)(path, "")  ||  
       0 == VG_(strcmp)(path, "/"))
   {
      return ".";
   }

   p = path + VG_(strlen)(path);
   while (p > path  &&  *p == '/') {
      // skip all trailing '/'
      p--;
   }

   while (p > path  &&  *p != '/') {
      // now skip non '/'
      p--;
   }

   if (p == path) {
      if (*p == '/') return "/"; // all slashes
      else return "."; // no slashes
   } 

   while (p > path  &&  *p == '/') {
      // skip '/' again
      p--;
   }

   VG_(strncpy)(buf, path, p-path+1);
   buf[p-path+1] = '\0';

   return buf;
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
