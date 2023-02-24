/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- File- and socket-related libc stuff.            m_libcfile.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

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
#include "pub_core_clientstate.h"   // VG_(fd_hard_limit)
#include "pub_core_mallocfree.h"    // VG_(realloc)
#include "pub_core_syscall.h"

/* IMPORTANT: on Darwin it is essential to use the _nocancel versions
   of syscalls rather than the vanilla version, if a _nocancel version
   is available.  See docs/internals/Darwin-notes.txt for the reason
   why. */

/* ---------------------------------------------------------------------
   File stuff
   ------------------------------------------------------------------ */

/* Move an fd into the Valgrind-safe range */
Int VG_(safe_fd)(Int oldfd)
{
   Int newfd;

   vg_assert(VG_(fd_hard_limit) != -1);

   newfd = VG_(fcntl)(oldfd, VKI_F_DUPFD, VG_(fd_hard_limit));

   if (newfd == -1) {
      VG_(debugLog)(0, "libcfile", "Valgrind: FATAL: "
                       "Private file creation failed.\n"
                       "   The current file descriptor limit is %d.\n"
                       "   If you are running in Docker please consider\n"
                       "   lowering this limit with the shell built-in limit command.\n",
                       VG_(fd_hard_limit));
      VG_(debugLog)(0, "libcfile", "Exiting now.\n");
      VG_(exit)(1);
   }

   vg_assert(newfd >= VG_(fd_hard_limit));

   VG_(close)(oldfd);

   /* Set the close-on-exec flag for this fd. */
   VG_(fcntl)(newfd, VKI_F_SETFD, VKI_FD_CLOEXEC);

   return newfd;
}

#if defined(VGO_freebsd)
#define M_FILEDESC_BUF  1000000
static Char filedesc_buf[M_FILEDESC_BUF];
#endif

/* Given a file descriptor, attempt to deduce its filename.  To do
   this, we use /proc/self/fd/<FD>.  If this doesn't point to a file,
   or if it doesn't exist, we return False. 
   Upon successful completion *result contains the filename. The
   filename will be overwritten with the next invocation so callers
   need to copy the filename if needed. *result is NULL if the filename
   cannot be deduced. */
Bool VG_(resolve_filename) ( Int fd, const HChar** result )
{
#  if defined(VGO_linux) || defined(VGO_solaris)
   static HChar *buf = NULL;
   static SizeT  bufsiz = 0;

   if (buf == NULL) {   // first time
      bufsiz = 500;
      buf = VG_(malloc)("resolve_filename", bufsiz);
   }

   HChar tmp[64];   // large enough
   {
#     if defined(VGO_linux)
      VG_(sprintf)(tmp, "/proc/self/fd/%d", fd);
#     elif defined(VGO_solaris)
      VG_(sprintf)(tmp, "/proc/self/path/%d", fd);
#     endif
   }

   while (42) {
      SSizeT res = VG_(readlink)(tmp, buf, bufsiz);
      if (res < 0) break;
      if (res == bufsiz) {  // buffer too small; increase and retry
         bufsiz += 500;
         buf = VG_(realloc)("resolve_filename", buf, bufsiz);
         continue;
      }
      vg_assert(bufsiz > res);  // paranoia
      if (buf[0] != '/') break;

      buf[res] = '\0';
      *result = buf;
      return True;
   }
   // Failure
   *result = NULL;
   return False;

#elif defined(VGO_freebsd)

#if (1)
   Int mib[4];
   SysRes sres;
   vki_size_t len;
   Char *bp;
   Char *eb;
   struct vki_kinfo_file *kf;
   static HChar *buf = NULL;
   static SizeT  bufsiz = 0;

   if (buf == NULL) {   // first time
      bufsiz = 500;
      buf = VG_(malloc)("resolve_filename", bufsiz);
   }

   mib[0] = VKI_CTL_KERN;
   mib[1] = VKI_KERN_PROC;
   mib[2] = VKI_KERN_PROC_FILEDESC;
   mib[3] = sr_Res(VG_(do_syscall0)(__NR_getpid));
   len = sizeof(filedesc_buf);
   sres = VG_(do_syscall6)(__NR___sysctl, (UWord)mib, 4, (UWord)filedesc_buf,
      (UWord)&len, 0, 0);
   if (sr_isError(sres)) {
       VG_(debugLog)(0, "sysctl(kern.proc.filedesc)", "%s\n", VG_(strerror)(sr_Err(sres)));
       return False;
   }
   /* Walk though the list. */
   bp = filedesc_buf;
   eb = filedesc_buf + len;
   while (bp < eb) {
      kf = (struct vki_kinfo_file *)bp;
      if (kf->vki_kf_fd == fd) {
         break;
      }
      bp += kf->vki_kf_structsize;
   }
   if (bp >= eb || *kf->vki_kf_path == '\0') {
     VG_(strncpy)( buf, "[unknown]", bufsiz );
   } else {
     VG_(strncpy)( buf, kf->vki_kf_path, bufsiz );
   }
   *result = buf;
   return True;
#else
   // PJF it will be a relief to get rid of the above bit of ugliness
   // however it does not seem to have the same functionality
   // regarding pipes where it profuces just an empty string
   struct vki_kinfo_file kinfo_file;
   kinfo_file.vki_kf_structsize = VKI_KINFO_FILE_SIZE;
   if (0 == VG_(fcntl) ( fd, VKI_F_KINFO, (Addr)&kinfo_file )) {
      static HChar *buf = NULL;

      if (buf == NULL)
         buf = VG_(malloc)("resolve_filename", VKI_PATH_MAX);

      *result = buf;
      VG_(strcpy)( buf, kinfo_file.vki_kf_path );
      if (buf[0] == '/') return True;
   }
   *result = NULL;
   return False;
#endif

#  elif defined(VGO_darwin)
   HChar tmp[VKI_MAXPATHLEN+1];
   if (0 == VG_(fcntl)(fd, VKI_F_GETPATH, (UWord)tmp)) {
      static HChar *buf = NULL;

      if (buf == NULL) 
         buf = VG_(malloc)("resolve_filename", VKI_MAXPATHLEN+1);
      VG_(strcpy)( buf, tmp );

      *result = buf;
      if (buf[0] == '/') return True;
   }
   // Failure
   *result = NULL;
   return False;

#  else
#     error Unknown OS
#  endif
}

#if defined(VGO_freebsd)


#if (1)
/* This should only be called after a successful call to
 * Bool VG_(resolve_filename) ( Int fd, const HChar** result )
 * so that filedesc_buf is still valid for fd */
Bool VG_(resolve_filemode) ( Int fd, Int * result )
{
   Char *bp;
   Char *eb;
   struct vki_kinfo_file *kf;

   /* Walk though the list. */
   bp = filedesc_buf;
   eb = filedesc_buf + sizeof(filedesc_buf);
   while (bp < eb) {
      kf = (struct vki_kinfo_file *)bp;
      if (kf->vki_kf_fd == fd) {
         break;
      }
      bp += kf->vki_kf_structsize;
   }
   if (bp >= eb) {
     *result = -1;
   } else {
     *result = kf->vki_kf_flags;
   }
   return True;
}
#else
/* less ugly version, no dependency on resolve_filename */
Bool VG_(resolve_filemode) ( Int fd, Int * result )
{
   struct vki_kinfo_file kinfo_file;
   kinfo_file.vki_kf_structsize = VKI_KINFO_FILE_SIZE;
   if (0 == VG_(fcntl) ( fd, VKI_F_KINFO, (Addr)&kinfo_file )) {
      *result = kinfo_file.vki_kf_flags;
      return True;
   }
   return False;
}
#endif
#endif


SysRes VG_(mknod) ( const HChar* pathname, Int mode, UWord dev )
{
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   /* ARM64 wants to use __NR_mknodat rather than __NR_mknod. */
   SysRes res = VG_(do_syscall4)(__NR_mknodat,
                                 VKI_AT_FDCWD, (UWord)pathname, mode, dev);
#  elif defined(VGO_linux) || defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_mknod,
                                 (UWord)pathname, mode, dev);
#  elif defined(VGO_freebsd)
#if (FREEBSD_VERS < FREEBSD_12)
   SysRes res = VG_(do_syscall3)(__NR_freebsd11_mknod,
                                 (UWord)pathname, mode, dev);
#else
   SysRes res = VG_(do_syscall4)(__NR_mknodat, VKI_AT_FDCWD,
                                 (UWord)pathname, mode, dev);
#endif
#  elif defined(VGO_solaris)
   SysRes res = VG_(do_syscall4)(__NR_mknodat,
                                 VKI_AT_FDCWD, (UWord)pathname, mode, dev);
#  else
#    error Unknown OS
#  endif
   return res;
}

SysRes VG_(open) ( const HChar* pathname, Int flags, Int mode )
{
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   /* ARM64 wants to use __NR_openat rather than __NR_open. */
   SysRes res = VG_(do_syscall4)(__NR_openat,
                                 VKI_AT_FDCWD, (UWord)pathname, flags, mode);
#  elif defined(VGO_linux) || defined(VGO_freebsd)
   SysRes res = VG_(do_syscall3)(__NR_open,
                                 (UWord)pathname, flags, mode);
#  elif defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_open_nocancel,
                                 (UWord)pathname, flags, mode);
#  elif defined(VGO_solaris)
   SysRes res = VG_(do_syscall4)(__NR_openat,
                                 VKI_AT_FDCWD, (UWord)pathname, flags, mode);
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
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
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
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
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
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
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
#  elif defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   SysRes res = VG_(do_syscall2)(__NR_pipe2, (UWord)fd, 0);
   return sr_isError(res) ? -1 : 0;
#  elif defined(VGO_linux)
   SysRes res = VG_(do_syscall1)(__NR_pipe, (UWord)fd);
   return sr_isError(res) ? -1 : 0;
#  elif defined(VGO_freebsd)
#if defined(__NR_pipe2)
   SysRes res = VG_(do_syscall2)(__NR_pipe2, (UWord)fd, 0);
#else
   SysRes res = VG_(do_syscall0)(__NR_freebsd10_pipe);
   if (!sr_isError(res)) {
      fd[0] = sr_Res(res);
      fd[1] = sr_ResHI(res);
   }
#endif
   return sr_isError(res) ? -1 : 0;
#  elif defined(VGO_darwin)
   /* __NR_pipe is UX64, so produces a double-word result */
   SysRes res = VG_(do_syscall0)(__NR_pipe);
   if (!sr_isError(res)) {
      fd[0] = (Int)sr_Res(res);
      fd[1] = (Int)sr_ResHI(res);
   }
   return sr_isError(res) ? -1 : 0;
#  elif defined(VGO_solaris)
#  if defined(SOLARIS_NEW_PIPE_SYSCALL)
   SysRes res = VG_(do_syscall2)(__NR_pipe, (UWord)fd, 0);
   return sr_isError(res) ? -1 : 0;
#  else
   SysRes res = VG_(do_syscall0)(__NR_pipe);
   if (!sr_isError(res)) {
      fd[0] = (Int)sr_Res(res);
      fd[1] = (Int)sr_ResHI(res);
   }
   return sr_isError(res) ? -1 : 0;
#  endif
#  else
#    error "Unknown OS"
#  endif
}

Off64T VG_(lseek) ( Int fd, Off64T offset, Int whence )
{
#  if defined(VGO_linux) || defined(VGP_amd64_darwin) || defined(VGP_amd64_freebsd)
#  if defined(__NR__llseek)
   Off64T result;
   SysRes res = VG_(do_syscall5)(__NR__llseek, fd,
                                 offset >> 32, offset & 0xffffffff,
                                 (UWord)&result, whence);
   return sr_isError(res) ? (-1) : result;
#  else
   SysRes res = VG_(do_syscall3)(__NR_lseek, fd, offset, whence);
   vg_assert(sizeof(Off64T) == sizeof(sr_Res(res)));
   return sr_isError(res) ? (-1) : sr_Res(res);
#  endif
#  elif defined(VGP_x86_darwin) || defined(VGP_x86_freebsd)
   SysRes res = VG_(do_syscall4)(__NR_lseek, fd, 
                                 offset & 0xffffffff, offset >> 32, whence);
   return sr_isError(res) ? (-1) : sr_Res(res);
#  elif defined(VGP_x86_solaris)
   SysRes res = VG_(do_syscall4)(__NR_llseek, fd,
                                 offset & 0xffffffff, offset >> 32, whence);
   return sr_isError(res) ? (-1) : ((ULong)sr_ResHI(res) << 32 | sr_Res(res));
#  elif defined(VGP_amd64_solaris)
   SysRes res = VG_(do_syscall3)(__NR_lseek, fd, offset, whence);
   vg_assert(sizeof(Off64T) == sizeof(Word));
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

#define TRANSLATE_statx_TO_vg_stat(_p_vgstat, _p_vkistat)                   \
   do {                                                                     \
      (_p_vgstat)->dev        = VG_MAKEDEV( (_p_vkistat)->stx_dev_major,    \
                                            (_p_vkistat)->stx_dev_minor );  \
      (_p_vgstat)->ino        = (ULong)( (_p_vkistat)->stx_ino );           \
      (_p_vgstat)->nlink      = (ULong)( (_p_vkistat)->stx_nlink );         \
      (_p_vgstat)->mode       = (UInt) ( (_p_vkistat)->stx_mode );          \
      (_p_vgstat)->uid        = (UInt) ( (_p_vkistat)->stx_uid );           \
      (_p_vgstat)->gid        = (UInt) ( (_p_vkistat)->stx_gid );           \
      (_p_vgstat)->rdev       = VG_MAKEDEV( (_p_vkistat)->stx_rdev_major,   \
                                            (_p_vkistat)->stx_rdev_minor ); \
      (_p_vgstat)->size       = (Long) ( (_p_vkistat)->stx_size );          \
      (_p_vgstat)->blksize    = (ULong)( (_p_vkistat)->stx_blksize );       \
      (_p_vgstat)->blocks     = (ULong)( (_p_vkistat)->stx_blocks );        \
      (_p_vgstat)->atime      = (ULong)( (_p_vkistat)->stx_atime.tv_sec );  \
      (_p_vgstat)->atime_nsec = (ULong)( (_p_vkistat)->stx_atime.tv_nsec ); \
      (_p_vgstat)->mtime      = (ULong)( (_p_vkistat)->stx_mtime.tv_sec );  \
      (_p_vgstat)->mtime_nsec = (ULong)( (_p_vkistat)->stx_mtime.tv_nsec ); \
      (_p_vgstat)->ctime      = (ULong)( (_p_vkistat)->stx_ctime.tv_sec );  \
      (_p_vgstat)->ctime_nsec = (ULong)( (_p_vkistat)->stx_ctime.tv_nsec ); \
   } while (0)

SysRes VG_(stat) ( const HChar* file_name, struct vg_stat* vgbuf )
{
   SysRes res;
   VG_(memset)(vgbuf, 0, sizeof(*vgbuf));

#  if defined(VGO_linux)
   /* On Linux, first try with statx. If that doesn't work out, fall back to
      the stat64 or vanilla version. */
   { struct vki_statx buf;
     res = VG_(do_syscall5)(__NR_statx, VKI_AT_FDCWD, (UWord)file_name, 0,
                            VKI_STATX_ALL, (UWord)&buf);
     if (!(sr_isError(res) && sr_Err(res) == VKI_ENOSYS)) {
        /* Success, or any failure except ENOSYS */
        if (!sr_isError(res))
           TRANSLATE_statx_TO_vg_stat(vgbuf, &buf);
        return res;
     }
   }
#  endif
#  if defined(VGO_linux) || defined(VGO_darwin)
   /* Try with stat64. This is the second candidate on Linux, and the first
      one on Darwin. If that doesn't work out, fall back to vanilla version.
    */
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
#  if defined(__NR_stat) || defined(VGP_arm64_linux)
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
#  endif
#  elif defined(VGO_solaris)
   {
#     if defined(VGP_x86_solaris)
      struct vki_stat64 buf64;
      res = VG_(do_syscall4)(__NR_fstatat64, VKI_AT_FDCWD, (UWord)file_name,
                             (UWord)&buf64, 0);
#     elif defined(VGP_amd64_solaris)
      struct vki_stat buf64;
      res = VG_(do_syscall4)(__NR_fstatat, VKI_AT_FDCWD, (UWord)file_name,
                             (UWord)&buf64, 0);
#     else
#        error "Unknown platform"
#     endif
      if (!sr_isError(res))
         TRANSLATE_TO_vg_stat(vgbuf, &buf64);
      return res;
   }
#  elif defined(VGO_freebsd)
   {
#if (FREEBSD_VERS < FREEBSD_12)
      struct vki_freebsd11_stat buf;
      res = VG_(do_syscall2)(__NR_stat, (UWord)file_name, (UWord)&buf);
#else
      struct vki_stat buf;
      res = VG_(do_syscall4)(__NR_fstatat, VKI_AT_FDCWD, (UWord)file_name, (UWord)&buf, 0);
#endif
      if (!sr_isError(res)) {
         TRANSLATE_TO_vg_stat(vgbuf, &buf);
      }
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

#  if defined(VGO_linux)
   /* On Linux, first try with statx. If that doesn't work out, fall back to
      the fstat64 or vanilla version. */
   { struct vki_statx buf;
     const char* file_name = "";
     res = VG_(do_syscall5)(__NR_statx, fd, (RegWord)file_name,
                            VKI_AT_EMPTY_PATH, VKI_STATX_ALL, (RegWord)&buf);
     if (!(sr_isError(res) && sr_Err(res) == VKI_ENOSYS)) {
        /* Success, or any failure except ENOSYS */
        if (!sr_isError(res))
           TRANSLATE_statx_TO_vg_stat(vgbuf, &buf);
        return sr_isError(res) ? (-1) : 0;
     }
   }
#endif
#  if defined(VGO_linux) || defined(VGO_darwin)
   /* Try with fstat64. This is the second candidate on Linux, and the first
      one on Darwin. If that doesn't work out, fall back to vanilla version.
    */
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
#  endif /* defined(__NR_fstat64) */
#  if defined(__NR_fstat)
   { struct vki_stat buf;
     res = VG_(do_syscall2)(__NR_fstat, (RegWord)fd, (RegWord)(Addr)&buf);
     if (!sr_isError(res))
        TRANSLATE_TO_vg_stat(vgbuf, &buf);
     return sr_isError(res) ? (-1) : 0;
   }
#  endif
#  elif defined(VGO_solaris)
   { 
#     if defined(VGP_x86_solaris)
      struct vki_stat64 buf64;
      res = VG_(do_syscall4)(__NR_fstatat64, (UWord)fd, 0, (UWord)&buf64, 0);
#     elif defined(VGP_amd64_solaris)
      struct vki_stat buf64;
      res = VG_(do_syscall4)(__NR_fstatat, (UWord)fd, 0, (UWord)&buf64, 0);
#     else
#        error "Unknown platform"
#     endif
      if (!sr_isError(res))
         TRANSLATE_TO_vg_stat(vgbuf, &buf64);
      return sr_isError(res) ? (-1) : 0;
   }
#  elif defined(VGO_freebsd)
   {
#if (FREEBSD_VERS < FREEBSD_12)
     struct vki_freebsd11_stat buf;
     res = VG_(do_syscall2)(__NR_fstat, (RegWord)fd, (RegWord)(Addr)&buf);
#else
      struct vki_stat buf;
     res = VG_(do_syscall2)(__NR_fstat, (RegWord)fd, (RegWord)(Addr)&buf);
#endif
     if (!sr_isError(res)) {
        TRANSLATE_TO_vg_stat(vgbuf, &buf);
     }
     return sr_isError(res) ? (-1) : 0;
   }
#  else
#    error Unknown OS
#  endif
}

#if defined(VGO_freebsd)
/* extend this to other OSes as and when needed */
SysRes VG_(lstat) ( const HChar* file_name, struct vg_stat* vgbuf )
{
   SysRes res;
   VG_(memset)(vgbuf, 0, sizeof(*vgbuf));

#if (FREEBSD_VERS < FREEBSD_12)
   struct vki_freebsd11_stat buf;
   res = VG_(do_syscall2)(__NR_lstat, (UWord)file_name, (UWord)&buf);
#else
   struct vki_stat buf;
   res = VG_(do_syscall4)(__NR_fstatat, VKI_AT_FDCWD, (UWord)file_name, (UWord)&buf, VKI_AT_SYMLINK_NOFOLLOW);
#endif
   if (!sr_isError(res)) {
      TRANSLATE_TO_vg_stat(vgbuf, &buf);
   }
   return res;
}
#endif

#undef TRANSLATE_TO_vg_stat
#undef TRANSLATE_statx_TO_vg_stat

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
#  if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   return VG_(do_syscall1)(__NR_dup, oldfd);
#  elif defined(VGO_solaris)
   return VG_(do_syscall3)(__NR_fcntl, oldfd, F_DUPFD, 0);
#  else
#    error Unknown OS
#  endif
}

SysRes VG_(dup2) ( Int oldfd, Int newfd )
{
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   /* We only have dup3, that means we have to mimic dup2.
      The only real difference is when oldfd == newfd.
      dup3 always returns an error, but dup2 returns only an
      error if the fd is invalid, otherwise it returns newfd. */
   if (oldfd == newfd) {
      if (VG_(fcntl)(oldfd, VKI_F_GETFL, 0) == -1)
         return VG_(mk_SysRes_Error)(VKI_EBADF);
      return VG_(mk_SysRes_Success)(newfd);
   }
   return VG_(do_syscall3)(__NR_dup3, oldfd, newfd, 0);
#  elif defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   return VG_(do_syscall2)(__NR_dup2, oldfd, newfd);
#  elif defined(VGO_solaris)
   return VG_(do_syscall3)(__NR_fcntl, oldfd, F_DUP2FD, newfd);
#  else
#    error Unknown OS
#  endif
}

/* Returns -1 on error. */
Int VG_(fcntl) ( Int fd, Int cmd, Addr arg )
{
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
#  if defined(VGP_nanomips_linux)
   SysRes res = VG_(do_syscall3)(__NR_fcntl64, fd, cmd, arg);
#  else
   SysRes res = VG_(do_syscall3)(__NR_fcntl, fd, cmd, arg);
#  endif
#  elif defined(VGO_darwin)
   SysRes res = VG_(do_syscall3)(__NR_fcntl_nocancel, fd, cmd, arg);
#  else
#    error "Unknown OS"
#  endif
   if (sr_isError(res)) {
      VG_(debugLog)(1, "VG_(fcntl)", "fcntl error %lu %s\n", sr_Err(res), VG_(strerror)(sr_Err(res)));
      return -1;
   }
   return (Int)sr_Res(res);
}

Int VG_(rename) ( const HChar* old_name, const HChar* new_name )
{
#  if defined(VGO_solaris) || defined(VGP_arm64_linux)
   SysRes res = VG_(do_syscall4)(__NR_renameat, VKI_AT_FDCWD, (UWord)old_name,
                                 VKI_AT_FDCWD, (UWord)new_name);
#  elif defined(VGP_nanomips_linux)
   SysRes res = VG_(do_syscall5)(__NR_renameat2, VKI_AT_FDCWD, (UWord)old_name,
                                 VKI_AT_FDCWD, (UWord)new_name, 0);

#  elif defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   SysRes res = VG_(do_syscall2)(__NR_rename, (UWord)old_name, (UWord)new_name);
#  else
#    error "Unknown OS"
#  endif
   return sr_isError(res) ? (-1) : 0;
}

Int VG_(unlink) ( const HChar* file_name )
{
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   SysRes res = VG_(do_syscall2)(__NR_unlinkat, VKI_AT_FDCWD,
                                                (UWord)file_name);
#  elif defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   SysRes res = VG_(do_syscall1)(__NR_unlink, (UWord)file_name);
#  elif defined(VGO_solaris)
   SysRes res = VG_(do_syscall3)(__NR_unlinkat, VKI_AT_FDCWD,
                                 (UWord)file_name, 0);
#  else
#    error "Unknown OS"
#  endif
   return sr_isError(res) ? (-1) : 0;
}

/* The working directory at startup.
   All that is really needed is to note the cwd at process startup.
   Hence VG_(record_startup_wd) notes it (in a platform dependent way)
   and VG_(get_startup_wd) produces the noted value. */
static HChar *startup_wd;

/* Record the process' working directory at startup.  Is intended to
   be called exactly once, at startup, before the working directory
   changes. */
void VG_(record_startup_wd) ( void )
{
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
   /* Simple: just ask the kernel */
   SysRes res;
   SizeT szB = 0;
   do { 
      szB += 500;
      startup_wd = VG_(realloc)("startup_wd", startup_wd, szB);
      VG_(memset)(startup_wd, 0, szB);
#   if defined(VGO_linux) || defined(VGO_solaris)
      res = VG_(do_syscall2)(__NR_getcwd, (UWord)startup_wd, szB-1);
#   elif defined(VGO_freebsd)
      res = VG_(do_syscall2)(__NR___getcwd, (UWord)startup_wd, szB-1);
#   endif
   } while (sr_isError(res) && sr_Err(res) == VKI_ERANGE);

   if (sr_isError(res)) {
      VG_(free)(startup_wd);
      startup_wd = NULL;
      return;
   }

   vg_assert(startup_wd[szB-1] == 0);

#  elif defined(VGO_darwin)
   /* We can't ask the kernel, so instead rely on launcher-*.c to
      tell us the startup path.  Note the env var is keyed to the
      parent's PID, not ours, since our parent is the launcher
      process. */
   { HChar  envvar[100];   // large enough
     HChar* wd;
     VG_(memset)(envvar, 0, sizeof(envvar));
     VG_(sprintf)(envvar, "VALGRIND_STARTUP_PWD_%d_XYZZY", 
                          (Int)VG_(getppid)());
     wd = VG_(getenv)( envvar );
     if (wd == NULL)
        return;
     SizeT need = VG_(strlen)(wd) + 1;
     startup_wd = VG_(malloc)("startup_wd", need);
     VG_(strcpy)(startup_wd, wd);
   }
#  else
#    error Unknown OS
#  endif
}

/* Return the previously acquired startup_wd or NULL. */
const HChar *VG_(get_startup_wd) ( void )
{
   return startup_wd;
}

SysRes VG_(poll) (struct vki_pollfd *fds, Int nfds, Int timeout)
{
   SysRes res;
#  if defined(VGP_arm64_linux)  || defined(VGP_nanomips_linux)
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
#  elif defined(VGO_freebsd)
   res = VG_(do_syscall3)(__NR_poll, (UWord)fds, nfds, timeout);
#  elif defined(VGO_darwin)
   res = VG_(do_syscall3)(__NR_poll_nocancel, (UWord)fds, nfds, timeout);
#  elif defined(VGO_solaris)
   struct vki_timespec ts;
   struct vki_timespec *tsp;

   if (timeout < 0)
      tsp = NULL;
   else {  
      ts.tv_sec = timeout / 1000;
      ts.tv_nsec = (timeout % 1000) * 1000000;
      tsp = &ts;
   }

   res = VG_(do_syscall4)(__NR_pollsys, (UWord)fds, nfds, (UWord)tsp, 0);
#  else
#    error "Unknown OS"
#  endif
   return res;
}


/* Performs the readlink operation and puts the result into 'buf'.
   Note, that the string in 'buf' is *not* null-terminated. The function
   returns the number of characters put into 'buf' or -1 if an error
   occurred. */
SSizeT VG_(readlink) (const HChar* path, HChar* buf, SizeT bufsiz)
{
   SysRes res;
   /* res = readlink( path, buf, bufsiz ); */
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   res = VG_(do_syscall4)(__NR_readlinkat, VKI_AT_FDCWD,
                                           (UWord)path, (UWord)buf, bufsiz);
#  elif defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   res = VG_(do_syscall3)(__NR_readlink, (UWord)path, (UWord)buf, bufsiz);
#  elif defined(VGO_solaris)
   res = VG_(do_syscall4)(__NR_readlinkat, VKI_AT_FDCWD, (UWord)path,
                          (UWord)buf, bufsiz);
#  else
#    error "Unknown OS"
#  endif
   return sr_isError(res) ? -1 : sr_Res(res);
}

#if defined(VGO_linux) || defined(VGO_solaris)
Int VG_(getdents64) (Int fd, struct vki_dirent64 *dirp, UInt count)
{
   SysRes res;
   /* res = getdents( fd, dirp, count ); */
#  if defined(VGP_amd64_solaris)
   /* This silently assumes that dirent64 and dirent on amd64 are same, which
      they should always be. */
   res = VG_(do_syscall3)(__NR_getdents, fd, (UWord)dirp, count);
#  else
   res = VG_(do_syscall3)(__NR_getdents64, fd, (UWord)dirp, count);
#     if defined(VGA_mips64)
      /* The MIPS64 getdents64() system call is only present in 3.10+ kernels.
         If the getdents64() system call is not available fall back to using
         getdents() and modify the result to be compatible with getdents64(). */
      if (sr_isError(res) && (sr_Err(res) == VKI_ENOSYS)) {
         int r;
         res = VG_(do_syscall3)(__NR_getdents, fd, (UWord)dirp, count);
         r = sr_Res(res);
         if (r > 0) {
            char *p;
            char type;
            union dirents {
               struct vki_dirent m;
               struct vki_dirent64 d;
            } *u;
            p = (char *)dirp;
            do {
               u = (union dirents *)p;
               /* This should not happen, but just in case... */
               if (p + u->m.d_reclen > (char *)dirp + r)
                  break;
               /* shuffle the dirent */
               type = *(p + u->m.d_reclen - 1);
               VG_(memmove)(u->d.d_name, u->m.d_name,
                            u->m.d_reclen - 2
                               - offsetof(struct vki_dirent, d_name) + 1);
               u->d.d_type = type;
               p += u->m.d_reclen;
            } while (p < (char *)dirp + r);
         }
      }
#     endif
#  endif
   return sr_isError(res) ? -1 : sr_Res(res);
}
#endif

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
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   SysRes res = VG_(do_syscall3)(__NR_faccessat, VKI_AT_FDCWD, (UWord)path, w);
#  elif defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   SysRes res = VG_(do_syscall2)(__NR_access, (UWord)path, w);
#  elif defined(VGO_solaris)
   SysRes res = VG_(do_syscall4)(__NR_faccessat, VKI_AT_FDCWD, (UWord)path,
                                 w, 0);
#  else
#    error "Unknown OS"
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

   if ( VKI_S_ISDIR (st.mode) ) {
      return VKI_EACCES;
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
         UInt *groups = NULL;
         Int   ngrp;

         /* Find out # groups, allocate large enough array and fetch groups */
         ngrp = VG_(getgroups)(0, NULL);
         if (ngrp != -1) {
            groups = VG_(malloc)("check_executable", ngrp * sizeof *groups);
            ngrp   = VG_(getgroups)(ngrp, groups);
         }

         Int i;
         /* ngrp will be -1 if VG_(getgroups) failed. */
         for (i = 0; i < ngrp; i++) {
	    if (groups[i] == st.gid) {
	       grpmatch = 1;
	       break;
	    }
         }
         VG_(free)(groups);
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
#  elif (defined(VGP_mips32_linux) || defined(VGP_nanomips_linux)) \
     && (VKI_LITTLE_ENDIAN)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall6)(__NR_pread64, fd, (UWord)buf, count, 
                          0, offset, 0);
   return res;
#  elif (defined(VGP_mips32_linux) || defined(VGP_nanomips_linux)) \
     && (VKI_BIG_ENDIAN)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall6)(__NR_pread64, fd, (UWord)buf, count, 
                          0, 0, offset);
   return res;
#  elif defined(VGP_amd64_linux) || defined(VGP_s390x_linux) \
      || defined(VGP_ppc64be_linux)  || defined(VGP_ppc64le_linux) \
      || defined(VGP_mips64_linux) || defined(VGP_arm64_linux)
   res = VG_(do_syscall4)(__NR_pread64, fd, (UWord)buf, count, offset);
   return res;
#  elif defined(VGP_amd64_freebsd)
   vg_assert(sizeof(OffT) == 8);
   res = VG_(do_syscall4)(__NR_pread, fd, (UWord)buf, count, offset);
   return res;
#  elif defined(VGP_x86_freebsd)
   vg_assert(sizeof(OffT) == 8);
   res = VG_(do_syscall5)(__NR_pread, fd, (UWord)buf, count, 
                          offset & 0xffffffff, offset >> 32);
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
#  elif defined(VGP_x86_solaris)
   vg_assert(sizeof(OffT) == 4);
   res = VG_(do_syscall4)(__NR_pread, fd, (UWord)buf, count, offset);
   return res;
#  elif defined(VGP_amd64_solaris)
   vg_assert(sizeof(OffT) == 8);
   res = VG_(do_syscall4)(__NR_pread, fd, (UWord)buf, count, offset);
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

static const HChar mkstemp_format[] = "%s/valgrind_%s_%08x";

SizeT VG_(mkstemp_fullname_bufsz) ( SizeT part_of_name_len )
{
   return VG_(strlen)(mkstemp_format)
      + VG_(strlen)(VG_(tmpdir)()) - 2 // %s tmpdir
      + part_of_name_len - 2           // %s part_of_name
      + 8 - 4                          // %08x
      + 1;                             // trailing 0
}


Int VG_(mkstemp) ( const HChar* part_of_name, /*OUT*/HChar* fullname )
{
   Int    n, tries;
   UInt   seed;
   SysRes sres;
   const HChar *tmpdir;

   vg_assert(part_of_name);
   vg_assert(fullname);
   n = VG_(strlen)(part_of_name);
   vg_assert(n > 0 && n < 100);

   seed = (VG_(getpid)() << 9) ^ VG_(getppid)();

   /* Determine sensible location for temporary files */
   tmpdir = VG_(tmpdir)();

   tries = 0;
   while (True) {
      if (tries++ > 10) 
         return -1;
      VG_(sprintf)( fullname, mkstemp_format,
                    tmpdir, part_of_name, VG_(random)( &seed ));
      if (0)
         VG_(printf)("VG_(mkstemp): trying: %s\n", fullname);

      sres = VG_(open)(fullname,
                       VKI_O_CREAT|VKI_O_RDWR|VKI_O_EXCL|VKI_O_TRUNC,
                       VKI_S_IRUSR|VKI_S_IWUSR);
      if (sr_isError(sres)) {
         VG_(umsg)("VG_(mkstemp): failed to create temp file: %s\n", fullname);
         continue;
      }
      /* VG_(safe_fd) doesn't return if it fails. */
      return VG_(safe_fd)( sr_Res(sres) );
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
#  if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)
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
      || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux) \
      || defined(VGP_s390x_linux)
   SysRes res;
   UWord  args[3];
   args[0] = domain;
   args[1] = type;
   args[2] = protocol;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_SOCKET, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
        || defined(VGP_arm64_linux) || defined(VGP_nanomips_linux) || defined(VGO_freebsd)
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

#  elif defined(VGO_solaris)
   /* XXX There doesn't seem to be an easy way to convince the send syscall to
      only return EPIPE instead of raising SIGPIPE. EPIPE is only returned if
      SM_KERNEL is set on the socket. Without serious hackery it looks we
      can't set this flag.

      Should we wrap the send syscall below into sigprocmask calls to block
      SIGPIPE?
    */
   SysRes res;
   res = VG_(do_syscall5)(__NR_so_socket, domain, type, protocol,
                          0 /*devpath*/, VKI_SOV_DEFAULT /*version*/);
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown arch"
#  endif
}


static
Int my_connect ( Int sockfd, struct vki_sockaddr_in* serv_addr, Int addrlen )
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux) \
      || defined(VGP_s390x_linux)
   SysRes res;
   UWord  args[3];
   args[0] = sockfd;
   args[1] = (UWord)serv_addr;
   args[2] = addrlen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_CONNECT, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
        || defined(VGP_arm64_linux) || defined(VGP_nanomips_linux) || defined(VGO_freebsd)
   SysRes res;
   res = VG_(do_syscall3)(__NR_connect, sockfd, (UWord)serv_addr, addrlen);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall3)(__NR_connect_nocancel,
                          sockfd, (UWord)serv_addr, addrlen);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_solaris)
   SysRes res;
   res = VG_(do_syscall4)(__NR_connect, sockfd, (UWord)serv_addr, addrlen,
                          VKI_SOV_DEFAULT /*version*/);
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
      || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux) \
      || defined(VGP_s390x_linux)
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
        || defined(VGP_arm64_linux) || defined(VGP_nanomips_linux) || defined(VGO_freebsd)
   SysRes res;
   res = VG_(do_syscall6)(__NR_sendto, sd, (UWord)msg, 
                                       count, VKI_MSG_NOSIGNAL, 0,0);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
   SysRes res;
   res = VG_(do_syscall3)(__NR_write_nocancel, sd, (UWord)msg, count);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_solaris)
   SysRes res;
   res = VG_(do_syscall4)(__NR_send, sd, (UWord)msg, count, 0 /*flags*/);
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}

Int VG_(getsockname) ( Int sd, struct vki_sockaddr *name, Int *namelen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux) \
      || defined(VGP_s390x_linux) \
      || defined(VGP_mips32_linux)
   SysRes res;
   UWord  args[3];
   args[0] = sd;
   args[1] = (UWord)name;
   args[2] = (UWord)namelen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETSOCKNAME, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips64_linux) || defined(VGP_arm64_linux) \
        || defined(VGP_nanomips_linux) || defined(VGO_freebsd) \
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

#  elif defined(VGO_solaris)
   SysRes res;
   res = VG_(do_syscall4)(__NR_getsockname, sd, (UWord)name, (UWord)namelen,
                          VKI_SOV_DEFAULT /*version*/);
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}

Int VG_(getpeername) ( Int sd, struct vki_sockaddr *name, Int *namelen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux) \
      || defined(VGP_s390x_linux) \
      || defined(VGP_mips32_linux)
   SysRes res;
   UWord  args[3];
   args[0] = sd;
   args[1] = (UWord)name;
   args[2] = (UWord)namelen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETPEERNAME, (UWord)&args);
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGP_amd64_linux) || defined(VGP_arm_linux) \
        || defined(VGP_mips64_linux) || defined(VGP_arm64_linux) \
        || defined(VGP_nanomips_linux) || defined(VGO_freebsd)
   SysRes res;
   res = VG_(do_syscall3)( __NR_getpeername,
                           (UWord)sd, (UWord)name, (UWord)namelen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin)
   SysRes res;
   res = VG_(do_syscall3)( __NR_getpeername,
                           (UWord)sd, (UWord)name, (UWord)namelen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_solaris)
   SysRes res;
   res = VG_(do_syscall4)(__NR_getpeername, sd, (UWord)name, (UWord)namelen,
                          VKI_SOV_DEFAULT /*version*/);
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}

Int VG_(getsockopt) ( Int sd, Int level, Int optname, void *optval,
                      Int *optlen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux) \
      || defined(VGP_s390x_linux)
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
        || defined(VGP_arm64_linux) || defined(VGP_nanomips_linux) \
        || defined(VGO_freebsd)
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

#  elif defined(VGO_solaris)
   SysRes res;
   res = VG_(do_syscall6)(__NR_getsockopt, sd, level, optname, (UWord)optval,
                          (UWord)optlen, VKI_SOV_DEFAULT /*version*/);
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}


Int VG_(setsockopt) ( Int sd, Int level, Int optname, void *optval,
                      Int optlen)
{
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
      || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux) \
      || defined(VGP_s390x_linux)
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
        || defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   SysRes res;
   res = VG_(do_syscall5)( __NR_setsockopt,
                           (UWord)sd, (UWord)level, (UWord)optname, 
                           (UWord)optval, (UWord)optlen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_darwin) || defined(VGO_freebsd)
   SysRes res;
   res = VG_(do_syscall5)( __NR_setsockopt,
                           (UWord)sd, (UWord)level, (UWord)optname, 
                           (UWord)optval, (UWord)optlen );
   return sr_isError(res) ? -1 : sr_Res(res);

#  elif defined(VGO_solaris)
   SysRes res;
   res = VG_(do_syscall6)( __NR_setsockopt,
                           (UWord)sd, (UWord)level, (UWord)optname,
                           (UWord)optval, (UWord)optlen,
                           VKI_SOV_DEFAULT /*version*/ );
   return sr_isError(res) ? -1 : sr_Res(res);

#  else
#    error "Unknown platform"
#  endif
}


const HChar *VG_(basename)(const HChar *path)
{
   static HChar *buf = NULL;
   static SizeT  buf_len = 0;
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

   SizeT need = end-p+1 + 1;
   if (need > buf_len) {
      buf_len = (buf_len == 0) ? 500 : need;
      buf = VG_(realloc)("basename", buf, buf_len);
   }
   VG_(strncpy)(buf, p, end-p+1);
   buf[end-p+1] = '\0';

   return buf;
}


const HChar *VG_(dirname)(const HChar *path)
{
   static HChar *buf = NULL;
   static SizeT  buf_len = 0;
    
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

   SizeT need = p-path+1 + 1;
   if (need > buf_len) {
      buf_len = (buf_len == 0) ? 500 : need;
      buf = VG_(realloc)("dirname", buf, buf_len);
   }
   VG_(strncpy)(buf, path, p-path+1);
   buf[p-path+1] = '\0';

   return buf;
}

#if defined(VGO_freebsd)
/*
 * I did look at nicking this from FreeBSD, it's fairly easy to port
 * but I was put off by the copyright and 3-clause licence
 * Then I looked at nicking it from glibc but that is full of
 * macros private functions and conditions for Windows.
 * So I gave up as it is only for FreeBSD 11 and 12.
 *
 * It is somewhat hard-coded for sysctl_kern_proc_pathname
 * and PRE(sys___sysctl) assuming resolved has
 * VKI_PATH_MAX space.
 */
Bool VG_(realpath)(const HChar *path, HChar *resolved)
{
   vg_assert(path);
   vg_assert(resolved);
#if (FREEBSD_VERS >= FREEBSD_13_0)
   return !sr_isError(VG_(do_syscall5)(__NR___realpathat, VKI_AT_FDCWD, (RegWord)path, (RegWord)resolved, VKI_PATH_MAX, 0));
#else
   // poor man's realpath
   const HChar *resolved_name;
   HChar tmp[VKI_PATH_MAX];

   struct vg_stat statbuf;
   SysRes res = VG_(lstat)(path, &statbuf);

   if (sr_isError(res)) {
      return False;
   }

   if (VKI_S_ISLNK(statbuf.mode)) {
      SizeT link_len = VG_(readlink)(path, tmp, VKI_PATH_MAX);
      tmp[link_len] = '\0';
      resolved_name = tmp;
   } else {
      // not a link
      resolved_name = path;
   }

   if (resolved_name[0] != '/') {
      // relative path
      if (resolved_name[0] == '.' && resolved_name[1] == '/') {
         resolved_name += 2;
      }
      VG_(snprintf)(resolved, VKI_PATH_MAX, "%s/%s", VG_(get_startup_wd)(), resolved_name);
   } else {
      VG_(snprintf)(resolved, VKI_PATH_MAX, "%s", resolved_name);
   }

   return True;
#endif
}
#endif


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
