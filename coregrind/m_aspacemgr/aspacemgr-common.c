
/*--------------------------------------------------------------------*/
/*--- The address space manager: stuff common to all platforms     ---*/
/*---                                                              ---*/
/*---                                         m_aspacemgr-common.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2017 OpenWorks LLP
      info@open-works.co.uk

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

/* *************************************************************
   DO NOT INCLUDE ANY OTHER FILES HERE.
   ADD NEW INCLUDES ONLY TO priv_aspacemgr.h
   AND THEN ONLY AFTER READING DIRE WARNINGS THERE TOO.
   ************************************************************* */

#include "priv_aspacemgr.h"
#include "pub_core_libcassert.h"
#include "config.h"


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Stuff to make aspacem almost completely independent of    ---*/
/*--- the rest of Valgrind.                                     ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

//--------------------------------------------------------------
// Simple assert and assert-like fns, which avoid dependence on
// m_libcassert, and hence on the entire debug-info reader swamp

__attribute__ ((noreturn))
void ML_(am_exit)( Int status )
{
   VG_(exit_now) (status);
}

void ML_(am_barf) ( const HChar* what )
{
   VG_(debugLog)(0, "aspacem", "Valgrind: FATAL: %s\n", what);
   VG_(debugLog)(0, "aspacem", "Exiting now.\n");
   ML_(am_exit)(1);
}

void ML_(am_barf_toolow) ( const HChar* what )
{
   VG_(debugLog)(0, "aspacem", 
                    "Valgrind: FATAL: %s is too low.\n", what);
   VG_(debugLog)(0, "aspacem", "  Increase it and rebuild.  "
                               "Exiting now.\n");
   ML_(am_exit)(1);
}

void ML_(am_assert_fail)( const HChar* expr,
                          const HChar* file,
                          Int line, 
                          const HChar* fn )
{
   VG_(debugLog)(0, "aspacem", 
                    "Valgrind: FATAL: aspacem assertion failed:\n");
   VG_(debugLog)(0, "aspacem", "  %s\n", expr);
   VG_(debugLog)(0, "aspacem", "  at %s:%d (%s)\n", file,line,fn);
   VG_(debugLog)(0, "aspacem", "Exiting now.\n");
   ML_(am_exit)(1);
}

Int ML_(am_getpid)( void )
{
   SysRes sres = VG_(do_syscall0)(__NR_getpid);
   aspacem_assert(!sr_isError(sres));
   return sr_Res(sres);
}


//--------------------------------------------------------------
// A simple sprintf implementation, so as to avoid dependence on
// m_libcprint.

static void local_add_to_aspacem_sprintf_buf ( HChar c, void *p )
{
   HChar** aspacem_sprintf_ptr = p;
   *(*aspacem_sprintf_ptr)++ = c;
}

static
UInt local_vsprintf ( HChar* buf, const HChar *format, va_list vargs )
{
   Int ret;
   HChar *aspacem_sprintf_ptr = buf;

   ret = VG_(debugLog_vprintf)
            ( local_add_to_aspacem_sprintf_buf, 
              &aspacem_sprintf_ptr, format, vargs );
   local_add_to_aspacem_sprintf_buf('\0', &aspacem_sprintf_ptr);

   return ret;
}

UInt ML_(am_sprintf) ( HChar* buf, const HChar *format, ... )
{
   UInt ret;
   va_list vargs;

   va_start(vargs,format);
   ret = local_vsprintf(buf, format, vargs);
   va_end(vargs);

   return ret;
}


//--------------------------------------------------------------
// Direct access to a handful of syscalls.  This avoids dependence on
// m_libc*.  THESE DO NOT UPDATE THE aspacem-internal DATA
// STRUCTURES (SEGMENT ARRAY).  DO NOT USE THEM UNLESS YOU KNOW WHAT
// YOU ARE DOING.

/* --- Pertaining to mappings --- */

/* Note: this is VG_, not ML_. */
SysRes VG_(am_do_mmap_NO_NOTIFY)( Addr start, SizeT length, UInt prot, 
                                  UInt flags, Int fd, Off64T offset)
{
   SysRes res;
   aspacem_assert(VG_IS_PAGE_ALIGNED(offset));

#  if defined(VGP_arm64_linux)
   res = VG_(do_syscall6)(__NR3264_mmap, (UWord)start, length, 
                         prot, flags, fd, offset);
#  elif defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
        || defined(VGP_arm_linux) || defined(VGP_nanomips_linux)
   /* mmap2 uses 4096 chunks even if actual page size is bigger. */
   aspacem_assert((offset % 4096) == 0);
   res = VG_(do_syscall6)(__NR_mmap2, (UWord)start, length,
                          prot, flags, fd, offset / 4096);
#  elif defined(VGP_amd64_linux) \
        || defined(VGP_ppc64be_linux)  || defined(VGP_ppc64le_linux) \
        || defined(VGP_s390x_linux) || defined(VGP_mips32_linux) \
        || defined(VGP_mips64_linux) || defined(VGP_arm64_linux)
   res = VG_(do_syscall6)(__NR_mmap, (UWord)start, length, 
                         prot, flags, fd, offset);
#  elif defined(VGP_x86_darwin)
   if (fd == 0  &&  (flags & VKI_MAP_ANONYMOUS)) {
       fd = -1;  // MAP_ANON with fd==0 is EINVAL
   }
   res = VG_(do_syscall7)(__NR_mmap, (UWord)start, length,
                          prot, flags, fd, offset & 0xffffffff, offset >> 32);
#  elif defined(VGP_amd64_darwin)
   if (fd == 0  &&  (flags & VKI_MAP_ANONYMOUS)) {
       fd = -1;  // MAP_ANON with fd==0 is EINVAL
   }
   res = VG_(do_syscall6)(__NR_mmap, (UWord)start, length,
                          prot, flags, (UInt)fd, offset);
#  elif defined(VGP_x86_freebsd)
   if (flags & VKI_MAP_ANONYMOUS && fd == 0)
      fd = -1;
   res = VG_(do_syscall7)(__NR_mmap, (UWord)start, length,
			  prot, flags, fd, offset, offset >> 32ul);
#  elif defined(VGP_amd64_freebsd)
   if ((flags & VKI_MAP_ANONYMOUS) && fd == 0)
      fd = -1;
   res = VG_(do_syscall6)(__NR_mmap, (UWord)start, length,
			  prot, flags, fd, offset);
#  elif defined(VGP_x86_solaris)
   /* MAP_ANON with fd==0 is EINVAL. */
   if (fd == 0 && (flags & VKI_MAP_ANONYMOUS))
      fd = -1;
   res = VG_(do_syscall7)(__NR_mmap64, (UWord)start, length, prot, flags,
                          (UInt)fd, offset & 0xffffffff, offset >> 32);
#  elif defined(VGP_amd64_solaris)
   /* MAP_ANON with fd==0 is EINVAL. */
   if (fd == 0 && (flags & VKI_MAP_ANONYMOUS))
      fd = -1;
   res = VG_(do_syscall6)(__NR_mmap, (UWord)start, length, prot, flags,
                          (UInt)fd, offset);
#  else
#    error Unknown platform
#  endif
   return res;
}

static
SysRes local_do_mprotect_NO_NOTIFY(Addr start, SizeT length, UInt prot)
{
   return VG_(do_syscall3)(__NR_mprotect, (UWord)start, length, prot );
}

SysRes ML_(am_do_munmap_NO_NOTIFY)(Addr start, SizeT length)
{
   return VG_(do_syscall2)(__NR_munmap, (UWord)start, length );
}

#if HAVE_MREMAP
/* The following are used only to implement mremap(). */

SysRes ML_(am_do_extend_mapping_NO_NOTIFY)( 
          Addr  old_addr, 
          SizeT old_len,
          SizeT new_len 
       )
{
   /* Extend the mapping old_addr .. old_addr+old_len-1 to have length
      new_len, WITHOUT moving it.  If it can't be extended in place,
      fail. */
#  if defined(VGO_linux) || defined(VGO_solaris)
   return VG_(do_syscall5)(
             __NR_mremap, 
             old_addr, old_len, new_len, 
             0/*flags, meaning: must be at old_addr, else FAIL */,
             0/*new_addr, is ignored*/
          );
#  else
#    error Unknown OS
#  endif
}

SysRes ML_(am_do_relocate_nooverlap_mapping_NO_NOTIFY)( 
          Addr old_addr, Addr old_len, 
          Addr new_addr, Addr new_len 
       )
{
   /* Move the mapping old_addr .. old_addr+old_len-1 to the new
      location and with the new length.  Only needs to handle the case
      where the two areas do not overlap, neither length is zero, and
      all args are page aligned. */
#  if defined(VGO_linux) || defined(VGO_solaris)
   return VG_(do_syscall5)(
             __NR_mremap, 
             old_addr, old_len, new_len, 
             VKI_MREMAP_MAYMOVE|VKI_MREMAP_FIXED/*move-or-fail*/,
             new_addr
          );
#  else
#    error Unknown OS
#  endif
}

#endif

/* --- Pertaining to files --- */

SysRes ML_(am_open) ( const HChar* pathname, Int flags, Int mode )
{
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   /* ARM64 wants to use __NR_openat rather than __NR_open. */
   SysRes res = VG_(do_syscall4)(__NR_openat,
                                 VKI_AT_FDCWD, (UWord)pathname, flags, mode);
#  elif defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   SysRes res = VG_(do_syscall3)(__NR_open, (UWord)pathname, flags, mode);
#  elif defined(VGO_solaris)
   SysRes res = VG_(do_syscall4)(__NR_openat, VKI_AT_FDCWD, (UWord)pathname,
                                 flags, mode);
#  else
#    error Unknown OS
#  endif
   return res;
}

Int ML_(am_read) ( Int fd, void* buf, Int count)
{
   SysRes res = VG_(do_syscall3)(__NR_read, fd, (UWord)buf, count);
   return sr_isError(res) ? -1 : sr_Res(res);
}

void ML_(am_close) ( Int fd )
{
   (void)VG_(do_syscall1)(__NR_close, fd);
}

Int ML_(am_readlink)(const HChar* path, HChar* buf, UInt bufsiz)
{
   SysRes res;
#  if defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
   res = VG_(do_syscall4)(__NR_readlinkat, VKI_AT_FDCWD,
                                           (UWord)path, (UWord)buf, bufsiz);
#  elif defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   res = VG_(do_syscall3)(__NR_readlink, (UWord)path, (UWord)buf, bufsiz);
#  elif defined(VGO_solaris)
   res = VG_(do_syscall4)(__NR_readlinkat, VKI_AT_FDCWD, (UWord)path,
                          (UWord)buf, bufsiz);
#  else
#    error Unknown OS
#  endif
   return sr_isError(res) ? -1 : sr_Res(res);
}

Int ML_(am_fcntl) ( Int fd, Int cmd, Addr arg )
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
#  error "Unknown OS"
#  endif
   return sr_isError(res) ? -1 : sr_Res(res);
}

/* Get the dev, inode and mode info for a file descriptor, if
   possible.  Returns True on success. */
Bool ML_(am_get_fd_d_i_m)( Int fd, 
                           /*OUT*/ULong* dev, 
                           /*OUT*/ULong* ino, /*OUT*/UInt* mode )
{
#  if defined(VGO_linux) || defined(VGO_darwin)
   SysRes          res;
#  if defined(VGO_linux)
   /* First try with statx. */
   struct vki_statx bufx;
   const char* file_name = "";
   res = VG_(do_syscall5)(__NR_statx, fd, (RegWord)file_name,
                          VKI_AT_EMPTY_PATH, VKI_STATX_ALL, (RegWord)&bufx);
   if (!sr_isError(res)) {
      *dev  = VG_MAKEDEV(bufx.stx_dev_major, bufx.stx_dev_minor);
      *ino  = (ULong)bufx.stx_ino;
      *mode = (UInt)bufx.stx_mode;
      return True;
   }
#  endif // VGO_linux only

#  if defined(VGO_linux) && defined(__NR_fstat64)
   /* fstat64 is second candidate as it can cope with minor and major device
      numbers outside the 0-255 range and it works properly for x86
      binaries on amd64 systems where fstat seems to be broken. */
   struct vki_stat64 buf64;
   res = VG_(do_syscall2)(__NR_fstat64, fd, (UWord)&buf64);
   if (!sr_isError(res)) {
      *dev  = (ULong)buf64.st_dev;
      *ino  = (ULong)buf64.st_ino;
      *mode = (UInt) buf64.st_mode;
      return True;
   }
#  endif // VGO_linux and defined __NR_fstat64

#  if defined(__NR_fstat)
   struct vki_stat buf;
   res = VG_(do_syscall2)(__NR_fstat, fd, (UWord)&buf);
   if (!sr_isError(res)) {
      *dev  = (ULong)buf.st_dev;
      *ino  = (ULong)buf.st_ino;
      *mode = (UInt) buf.st_mode;
      return True;
   }
#  endif // defined __NR_fstat
   return False;
#  elif defined(VGO_solaris)
#  if defined(VGP_x86_solaris)
   struct vki_stat64 buf64;
   SysRes res = VG_(do_syscall4)(__NR_fstatat64, fd, 0, (UWord)&buf64, 0);
#  elif defined(VGP_amd64_solaris)
   struct vki_stat buf64;
   SysRes res = VG_(do_syscall4)(__NR_fstatat, fd, 0, (UWord)&buf64, 0);
#  else
#    error "Unknown platform"
#  endif
   if (!sr_isError(res)) {
      *dev  = (ULong)buf64.st_dev;
      *ino  = (ULong)buf64.st_ino;
      *mode = (UInt) buf64.st_mode;
      return True;
   }
   return False;
#  elif defined(VGO_freebsd)
#if (FREEBSD_VERS < FREEBSD_12)
   struct vki_freebsd11_stat buf;
   SysRes res = VG_(do_syscall2)(__NR_fstat, fd, (UWord)&buf);
#else
   struct vki_stat buf;
   SysRes res = VG_(do_syscall2)(__NR_fstat, fd, (UWord)&buf);
#endif
   if (!sr_isError(res)) {
      *dev  = (ULong)buf.st_dev;
      *ino  = (ULong)buf.st_ino;
      *mode = (UInt) buf.st_mode;
      return True;
   }
   return False;
#  else
#    error Unknown OS
#  endif
}

#if defined(VGO_freebsd)
#define	M_FILEDESC_BUF	1000000
static Char filedesc_buf[M_FILEDESC_BUF];
#endif
   
Bool ML_(am_resolve_filename) ( Int fd, /*OUT*/HChar* buf, Int nbuf )
{
#if defined(VGO_linux)
   Int i;
   HChar tmp[64];    // large enough
   for (i = 0; i < nbuf; i++) buf[i] = 0;
   ML_(am_sprintf)(tmp, "/proc/self/fd/%d", fd);
   if (ML_(am_readlink)(tmp, buf, nbuf) > 0 && buf[0] == '/')
      return True;
   else
      return False;

#elif defined(VGO_freebsd)


#if (1)
   Int mib[4];
   SysRes sres;
   vki_size_t len;
   Char *bp, *eb;
   struct vki_kinfo_file *kf;

   mib[0] = VKI_CTL_KERN;
   mib[1] = VKI_KERN_PROC;
   mib[2] = VKI_KERN_PROC_FILEDESC;
   mib[3] = sr_Res(VG_(do_syscall0)(__NR_getpid));
   len = sizeof(filedesc_buf);
   sres = VG_(do_syscall6)(__NR___sysctl, (UWord)mib, 4, (UWord)filedesc_buf,
      (UWord)&len, 0, 0);
   if (sr_isError(sres)) {
       VG_(debugLog)(0, "sysctl(kern.proc.filedesc)", "%s\n", VG_(strerror)(sr_Err(sres)));
       ML_(am_exit)(1);
   }
   /* Walk though the list. */
   bp = filedesc_buf;
   eb = filedesc_buf + len;
   while (bp < eb) {
      kf = (struct vki_kinfo_file *)bp;
      if (kf->vki_kf_fd == fd)
         break;
      bp += kf->vki_kf_structsize;
   }
   if (bp >= eb || *kf->vki_kf_path == '\0')
     VG_(strncpy)( buf, "[unknown]", nbuf );
   else
     VG_(strncpy)( buf, kf->vki_kf_path, nbuf );
   return True;
#else
   // PJF it will be a relief to get rid of the above bit of ugliness
   struct vki_kinfo_file kinfo_file;
   kinfo_file.vki_kf_structsize = VKI_KINFO_FILE_SIZE;
   if (0 == ML_(am_fcntl) ( fd, VKI_F_KINFO, (Addr)&kinfo_file )) {
      if (nbuf > 0) {
         VG_(strncpy)( buf, kinfo_file.vki_kf_path, nbuf < VKI_PATH_MAX ? nbuf : VKI_PATH_MAX );
         buf[nbuf-1] = 0;
      }
      if (buf[0] == '/') return True;
   }
   return False;
#endif

#elif defined(VGO_darwin)
   HChar tmp[VKI_MAXPATHLEN+1];
   if (0 == ML_(am_fcntl)(fd, VKI_F_GETPATH, (UWord)tmp)) {
      if (nbuf > 0) {
         VG_(strncpy)( buf, tmp, nbuf < sizeof(tmp) ? nbuf : sizeof(tmp) );
         buf[nbuf-1] = 0;
      }
      if (tmp[0] == '/') return True;
   }
   return False;

#elif defined(VGO_solaris)
   Int i;
   HChar tmp[64];
   for (i = 0; i < nbuf; i++) buf[i] = 0;
   ML_(am_sprintf)(tmp, "/proc/self/path/%d", fd);
   if (ML_(am_readlink)(tmp, buf, nbuf) > 0 && buf[0] == '/')
      return True;
   else
      return False;

#  else
#     error Unknown OS
#  endif
}




/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Manage stacks for Valgrind itself.                        ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/
struct _VgStack {
   HChar bytes[1];
   // We use a fake size of 1. A bigger size is allocated
   // by VG_(am_alloc_VgStack).
};

/* Allocate and initialise a VgStack (anonymous valgrind space).
   Protect the stack active area and the guard areas appropriately.
   Returns NULL on failure, else the address of the bottom of the
   stack.  On success, also sets *initial_sp to what the stack pointer
   should be set to. */

VgStack* VG_(am_alloc_VgStack)( /*OUT*/Addr* initial_sp )
{
   Int      szB;
   SysRes   sres;
   VgStack* stack;
   UInt*    p;
   Int      i;

   /* Allocate the stack. */
   szB = VG_STACK_GUARD_SZB 
         + VG_(clo_valgrind_stacksize) + VG_STACK_GUARD_SZB;

   sres = VG_(am_mmap_anon_float_valgrind)( szB );
   if (sr_isError(sres))
      return NULL;

   stack = (VgStack*)(Addr)sr_Res(sres);

   aspacem_assert(VG_IS_PAGE_ALIGNED(szB));
   aspacem_assert(VG_IS_PAGE_ALIGNED(stack));

   /* Protect the guard areas. */
   sres = local_do_mprotect_NO_NOTIFY( 
             (Addr) &stack[0], 
             VG_STACK_GUARD_SZB, VKI_PROT_NONE 
          );
   if (sr_isError(sres)) goto protect_failed;
   VG_(am_notify_mprotect)( 
      (Addr) &stack->bytes[0], 
      VG_STACK_GUARD_SZB, VKI_PROT_NONE 
   );

   sres = local_do_mprotect_NO_NOTIFY( 
             (Addr) &stack->bytes[VG_STACK_GUARD_SZB + VG_(clo_valgrind_stacksize)], 
             VG_STACK_GUARD_SZB, VKI_PROT_NONE 
          );
   if (sr_isError(sres)) goto protect_failed;
   VG_(am_notify_mprotect)( 
      (Addr) &stack->bytes[VG_STACK_GUARD_SZB + VG_(clo_valgrind_stacksize)],
      VG_STACK_GUARD_SZB, VKI_PROT_NONE 
   );

   /* Looks good.  Fill the active area with junk so we can later
      tell how much got used. */

   p = (UInt*)&stack->bytes[VG_STACK_GUARD_SZB];
   for (i = 0; i < VG_(clo_valgrind_stacksize)/sizeof(UInt); i++)
      p[i] = 0xDEADBEEF;

   *initial_sp = (Addr)&stack->bytes[VG_STACK_GUARD_SZB + VG_(clo_valgrind_stacksize)];
   *initial_sp -= 8;
   *initial_sp &= ~((Addr)0x1F); /* 32-align it */

   VG_(debugLog)( 1,"aspacem",
                  "allocated valgrind thread stack at 0x%llx size %d\n",
                  (ULong)(Addr)stack, szB);
   ML_(am_do_sanity_check)();
   return stack;

  protect_failed:
   /* The stack was allocated, but we can't protect it.  Unmap it and
      return NULL (failure). */
   (void)ML_(am_do_munmap_NO_NOTIFY)( (Addr)stack, szB );
   ML_(am_do_sanity_check)();
   return NULL;
}


/* Figure out how many bytes of the stack's active area have not
   been used.  Used for estimating if we are close to overflowing it. */

SizeT VG_(am_get_VgStack_unused_szB)( const VgStack* stack, SizeT limit )
{
   SizeT i;
   const UInt* p;

   p = (const UInt*)&stack->bytes[VG_STACK_GUARD_SZB];
   for (i = 0; i < VG_(clo_valgrind_stacksize)/sizeof(UInt); i++) {
      if (p[i] != 0xDEADBEEF)
         break;
      if (i * sizeof(UInt) >= limit)
         break;
   }

   return i * sizeof(UInt);
}

Addr VG_(am_valgrind_stack_low_addr)( const VgStack* stack)
{
   return (Addr)&stack->bytes[VG_STACK_GUARD_SZB];
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
