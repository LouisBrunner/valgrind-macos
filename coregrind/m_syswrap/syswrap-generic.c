
/*--------------------------------------------------------------------*/
/*--- Wrappers for generic Unix system calls                       ---*/
/*---                                            syswrap-generic.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward 
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

#if defined(VGO_linux) || defined(VGO_darwin)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_debuginfo.h"     // VG_(di_notify_*)
#include "pub_core_aspacemgr.h"
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"   // VG_(brk_base), VG_(brk_limit)
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_tool_gdbserver.h"     // VG_(gdbserver)
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_machine.h"       // VG_(get_SP)
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_ume.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"

#include "config.h"


/* Returns True iff address range is something the client can
   plausibly mess with: all of it is either already belongs to the
   client or is free or a reservation. */

Bool ML_(valid_client_addr)(Addr start, SizeT size, ThreadId tid,
                                   const Char *syscallname)
{
   Bool ret;

   if (size == 0)
      return True;

   ret = VG_(am_is_valid_for_client_or_free_or_resvn)
            (start,size,VKI_PROT_NONE);

   if (0)
      VG_(printf)("%s: test=%#lx-%#lx ret=%d\n",
		  syscallname, start, start+size-1, (Int)ret);

   if (!ret && syscallname != NULL) {
      VG_(message)(Vg_UserMsg, "Warning: client syscall %s tried "
                               "to modify addresses %#lx-%#lx\n",
                               syscallname, start, start+size-1);
      if (VG_(clo_verbosity) > 1) {
         VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      }
   }

   return ret;
}


Bool ML_(client_signal_OK)(Int sigNo)
{
   /* signal 0 is OK for kill */
   Bool ret = sigNo >= 0 && sigNo <= VG_SIGVGRTUSERMAX;

   //VG_(printf)("client_signal_OK(%d) -> %d\n", sigNo, ret);

   return ret;
}


/* Handy small function to help stop wrappers from segfaulting when
   presented with bogus client addresses.  Is not used for generating
   user-visible errors. */

Bool ML_(safe_to_deref) ( void* start, SizeT size )
{
   return VG_(am_is_valid_for_client)( (Addr)start, size, VKI_PROT_READ );
}


/* ---------------------------------------------------------------------
   Doing mmap, mremap
   ------------------------------------------------------------------ */

/* AFAICT from kernel sources (mm/mprotect.c) and general experimentation,
   munmap, mprotect (and mremap??) work at the page level.  So addresses
   and lengths must be adjusted for this. */

/* Mash around start and length so that the area exactly covers
   an integral number of pages.  If we don't do that, memcheck's
   idea of addressible memory diverges from that of the
   kernel's, which causes the leak detector to crash. */
static 
void page_align_addr_and_len( Addr* a, SizeT* len)
{
   Addr ra;
   
   ra = VG_PGROUNDDN(*a);
   *len = VG_PGROUNDUP(*a + *len) - ra;
   *a = ra;
}

static void notify_core_of_mmap(Addr a, SizeT len, UInt prot,
                                UInt flags, Int fd, Off64T offset)
{
   Bool d;

   /* 'a' is the return value from a real kernel mmap, hence: */
   vg_assert(VG_IS_PAGE_ALIGNED(a));
   /* whereas len is whatever the syscall supplied.  So: */
   len = VG_PGROUNDUP(len);

   d = VG_(am_notify_client_mmap)( a, len, prot, flags, fd, offset );

   if (d)
      VG_(discard_translations)( (Addr64)a, (ULong)len,
                                 "notify_core_of_mmap" );
}

static void notify_tool_of_mmap(Addr a, SizeT len, UInt prot, ULong di_handle)
{
   Bool rr, ww, xx;

   /* 'a' is the return value from a real kernel mmap, hence: */
   vg_assert(VG_IS_PAGE_ALIGNED(a));
   /* whereas len is whatever the syscall supplied.  So: */
   len = VG_PGROUNDUP(len);

   rr = toBool(prot & VKI_PROT_READ);
   ww = toBool(prot & VKI_PROT_WRITE);
   xx = toBool(prot & VKI_PROT_EXEC);

   VG_TRACK( new_mem_mmap, a, len, rr, ww, xx, di_handle );
}


/* When a client mmap has been successfully done, this function must
   be called.  It notifies both aspacem and the tool of the new
   mapping.

   JRS 2008-Aug-14: But notice this is *very* obscure.  The only place
   it is called from is POST(sys_io_setup).  In particular,
   ML_(generic_PRE_sys_mmap), in m_syswrap, is the "normal case" handler for
   client mmap.  But it doesn't call this function; instead it does the
   relevant notifications itself.  Here, we just pass di_handle=0 to
   notify_tool_of_mmap as we have no better information.  But really this
   function should be done away with; problem is I don't understand what
   POST(sys_io_setup) does or how it works. 
   
   [However, this function is used lots for Darwin, because
    ML_(generic_PRE_sys_mmap) cannot be used for Darwin.] 
 */
void 
ML_(notify_core_and_tool_of_mmap) ( Addr a, SizeT len, UInt prot, 
                                    UInt flags, Int fd, Off64T offset )
{
   // XXX: unlike the other notify_core_and_tool* functions, this one doesn't
   // do anything with debug info (ie. it doesn't call VG_(di_notify_mmap)).
   // Should it?  --njn
   notify_core_of_mmap(a, len, prot, flags, fd, offset);
   notify_tool_of_mmap(a, len, prot, 0/*di_handle*/);
}

void 
ML_(notify_core_and_tool_of_munmap) ( Addr a, SizeT len )
{
   Bool d;

   page_align_addr_and_len(&a, &len);
   d = VG_(am_notify_munmap)(a, len);
   VG_TRACK( die_mem_munmap, a, len );
   VG_(di_notify_munmap)( a, len );
   if (d)
      VG_(discard_translations)( (Addr64)a, (ULong)len, 
                                 "ML_(notify_core_and_tool_of_munmap)" );
}

void 
ML_(notify_core_and_tool_of_mprotect) ( Addr a, SizeT len, Int prot )
{
   Bool rr = toBool(prot & VKI_PROT_READ);
   Bool ww = toBool(prot & VKI_PROT_WRITE);
   Bool xx = toBool(prot & VKI_PROT_EXEC);
   Bool d;

   page_align_addr_and_len(&a, &len);
   d = VG_(am_notify_mprotect)(a, len, prot);
   VG_TRACK( change_mem_mprotect, a, len, rr, ww, xx );
   VG_(di_notify_mprotect)( a, len, prot );
   if (d)
      VG_(discard_translations)( (Addr64)a, (ULong)len, 
                                 "ML_(notify_core_and_tool_of_mprotect)" );
}



#if HAVE_MREMAP
/* Expand (or shrink) an existing mapping, potentially moving it at
   the same time (controlled by the MREMAP_MAYMOVE flag).  Nightmare.
*/
static
SysRes do_mremap( Addr old_addr, SizeT old_len, 
                  Addr new_addr, SizeT new_len,
                  UWord flags, ThreadId tid )
{
#  define MIN_SIZET(_aa,_bb) (_aa) < (_bb) ? (_aa) : (_bb)

   Bool      ok, d;
   NSegment const* old_seg;
   Addr      advised;
   Bool      f_fixed   = toBool(flags & VKI_MREMAP_FIXED);
   Bool      f_maymove = toBool(flags & VKI_MREMAP_MAYMOVE);

   if (0)
      VG_(printf)("do_remap (old %#lx %ld) (new %#lx %ld) %s %s\n",
                  old_addr,old_len,new_addr,new_len, 
                  flags & VKI_MREMAP_MAYMOVE ? "MAYMOVE" : "",
                  flags & VKI_MREMAP_FIXED ? "FIXED" : "");
   if (0)
      VG_(am_show_nsegments)(0, "do_remap: before");

   if (flags & ~(VKI_MREMAP_FIXED | VKI_MREMAP_MAYMOVE))
      goto eINVAL;

   if (!VG_IS_PAGE_ALIGNED(old_addr))
      goto eINVAL;

   old_len = VG_PGROUNDUP(old_len);
   new_len = VG_PGROUNDUP(new_len);

   if (new_len == 0)
      goto eINVAL;

   /* kernel doesn't reject this, but we do. */
   if (old_len == 0)
      goto eINVAL;

   /* reject wraparounds */
   if (old_addr + old_len < old_addr)
      goto eINVAL;
   if (f_fixed == True && new_addr + new_len < new_len)
      goto eINVAL;

   /* kernel rejects all fixed, no-move requests (which are
      meaningless). */
   if (f_fixed == True && f_maymove == False)
      goto eINVAL;

   /* Stay away from non-client areas. */
   if (!ML_(valid_client_addr)(old_addr, old_len, tid, "mremap(old_addr)"))
      goto eINVAL;

   /* In all remaining cases, if the old range does not fall within a
      single segment, fail. */
   old_seg = VG_(am_find_nsegment)( old_addr );
   if (old_addr < old_seg->start || old_addr+old_len-1 > old_seg->end)
      goto eINVAL;
   if (old_seg->kind != SkAnonC && old_seg->kind != SkFileC)
      goto eINVAL;

   vg_assert(old_len > 0);
   vg_assert(new_len > 0);
   vg_assert(VG_IS_PAGE_ALIGNED(old_len));
   vg_assert(VG_IS_PAGE_ALIGNED(new_len));
   vg_assert(VG_IS_PAGE_ALIGNED(old_addr));

   /* There are 3 remaining cases:

      * maymove == False

        new space has to be at old address, so:
            - shrink    -> unmap end
            - same size -> do nothing
            - grow      -> if can grow in-place, do so, else fail

      * maymove == True, fixed == False

        new space can be anywhere, so:
            - shrink    -> unmap end
            - same size -> do nothing
            - grow      -> if can grow in-place, do so, else 
                           move to anywhere large enough, else fail

      * maymove == True, fixed == True

        new space must be at new address, so:

            - if new address is not page aligned, fail
            - if new address range overlaps old one, fail
            - if new address range cannot be allocated, fail
            - else move to new address range with new size
            - else fail
   */

   if (f_maymove == False) {
      /* new space has to be at old address */
      if (new_len < old_len)
         goto shrink_in_place;
      if (new_len > old_len)
         goto grow_in_place_or_fail;
      goto same_in_place;
   }

   if (f_maymove == True && f_fixed == False) {
      /* new space can be anywhere */
      if (new_len < old_len)
         goto shrink_in_place;
      if (new_len > old_len)
         goto grow_in_place_or_move_anywhere_or_fail;
      goto same_in_place;
   }

   if (f_maymove == True && f_fixed == True) {
      /* new space can only be at the new address */
      if (!VG_IS_PAGE_ALIGNED(new_addr)) 
         goto eINVAL;
      if (new_addr+new_len-1 < old_addr || new_addr > old_addr+old_len-1) {
         /* no overlap */
      } else {
         goto eINVAL;
      }
      if (new_addr == 0) 
         goto eINVAL; 
         /* VG_(am_get_advisory_client_simple) interprets zero to mean
            non-fixed, which is not what we want */
      advised = VG_(am_get_advisory_client_simple)(new_addr, new_len, &ok);
      if (!ok || advised != new_addr)
         goto eNOMEM;
      ok = VG_(am_relocate_nooverlap_client)
              ( &d, old_addr, old_len, new_addr, new_len );
      if (ok) {
         VG_TRACK( copy_mem_remap, old_addr, new_addr, 
                                   MIN_SIZET(old_len,new_len) );
         if (new_len > old_len)
            VG_TRACK( new_mem_mmap, new_addr+old_len, new_len-old_len,
                      old_seg->hasR, old_seg->hasW, old_seg->hasX,
                      0/*di_handle*/ );
         VG_TRACK(die_mem_munmap, old_addr, old_len);
         if (d) {
            VG_(discard_translations)( old_addr, old_len, "do_remap(1)" );
            VG_(discard_translations)( new_addr, new_len, "do_remap(2)" );
         }
         return VG_(mk_SysRes_Success)( new_addr );
      }
      goto eNOMEM;
   }

   /* end of the 3 cases */
   /*NOTREACHED*/ vg_assert(0);

  grow_in_place_or_move_anywhere_or_fail: 
   { 
   /* try growing it in-place */
   Addr   needA = old_addr + old_len;
   SSizeT needL = new_len - old_len;

   vg_assert(needL > 0);
   if (needA == 0)
      goto eINVAL; 
      /* VG_(am_get_advisory_client_simple) interprets zero to mean
         non-fixed, which is not what we want */
   advised = VG_(am_get_advisory_client_simple)( needA, needL, &ok );
   if (ok) {
      /* Fixes bug #129866. */
      ok = VG_(am_covered_by_single_free_segment) ( needA, needL );
   }
   if (ok && advised == needA) {
      ok = VG_(am_extend_map_client)( &d, (NSegment*)old_seg, needL );
      if (ok) {
         VG_TRACK( new_mem_mmap, needA, needL, 
                                 old_seg->hasR, 
                                 old_seg->hasW, old_seg->hasX,
                                 0/*di_handle*/ );
         if (d) 
            VG_(discard_translations)( needA, needL, "do_remap(3)" );
         return VG_(mk_SysRes_Success)( old_addr );
      }
   }

   /* that failed.  Look elsewhere. */
   advised = VG_(am_get_advisory_client_simple)( 0, new_len, &ok );
   if (ok) {
      Bool oldR = old_seg->hasR;
      Bool oldW = old_seg->hasW;
      Bool oldX = old_seg->hasX;
      /* assert new area does not overlap old */
      vg_assert(advised+new_len-1 < old_addr 
                || advised > old_addr+old_len-1);
      ok = VG_(am_relocate_nooverlap_client)
              ( &d, old_addr, old_len, advised, new_len );
      if (ok) {
         VG_TRACK( copy_mem_remap, old_addr, advised, 
                                   MIN_SIZET(old_len,new_len) );
         if (new_len > old_len)
            VG_TRACK( new_mem_mmap, advised+old_len, new_len-old_len,
                      oldR, oldW, oldX, 0/*di_handle*/ );
         VG_TRACK(die_mem_munmap, old_addr, old_len);
         if (d) {
            VG_(discard_translations)( old_addr, old_len, "do_remap(4)" );
            VG_(discard_translations)( advised, new_len, "do_remap(5)" );
         }
         return VG_(mk_SysRes_Success)( advised );
      }
   }
   goto eNOMEM;
   }
   /*NOTREACHED*/ vg_assert(0);

  grow_in_place_or_fail:
   {
   Addr  needA = old_addr + old_len;
   SizeT needL = new_len - old_len;
   if (needA == 0) 
      goto eINVAL;
      /* VG_(am_get_advisory_client_simple) interprets zero to mean
         non-fixed, which is not what we want */
   advised = VG_(am_get_advisory_client_simple)( needA, needL, &ok );
   if (ok) {
      /* Fixes bug #129866. */
      ok = VG_(am_covered_by_single_free_segment) ( needA, needL );
   }
   if (!ok || advised != needA)
      goto eNOMEM;
   ok = VG_(am_extend_map_client)( &d, (NSegment*)old_seg, needL );
   if (!ok)
      goto eNOMEM;
   VG_TRACK( new_mem_mmap, needA, needL, 
                           old_seg->hasR, old_seg->hasW, old_seg->hasX,
                           0/*di_handle*/ );
   if (d)
      VG_(discard_translations)( needA, needL, "do_remap(6)" );
   return VG_(mk_SysRes_Success)( old_addr );
   }
   /*NOTREACHED*/ vg_assert(0);

  shrink_in_place:
   {
   SysRes sres = VG_(am_munmap_client)( &d, old_addr+new_len, old_len-new_len );
   if (sr_isError(sres))
      return sres;
   VG_TRACK( die_mem_munmap, old_addr+new_len, old_len-new_len );
   if (d)
      VG_(discard_translations)( old_addr+new_len, old_len-new_len, 
                                 "do_remap(7)" );
   return VG_(mk_SysRes_Success)( old_addr );
   }
   /*NOTREACHED*/ vg_assert(0);

  same_in_place:
   return VG_(mk_SysRes_Success)( old_addr );
   /*NOTREACHED*/ vg_assert(0);

  eINVAL:
   return VG_(mk_SysRes_Error)( VKI_EINVAL );
  eNOMEM:
   return VG_(mk_SysRes_Error)( VKI_ENOMEM );

#  undef MIN_SIZET
}
#endif /* HAVE_MREMAP */


/* ---------------------------------------------------------------------
   File-descriptor tracking
   ------------------------------------------------------------------ */

/* One of these is allocated for each open file descriptor.  */
typedef struct OpenFd
{
   Int fd;                        /* The file descriptor */
   Char *pathname;                /* NULL if not a regular file or unknown */
   ExeContext *where;             /* NULL if inherited from parent */
   struct OpenFd *next, *prev;
} OpenFd;

/* List of allocated file descriptors. */
static OpenFd *allocated_fds = NULL;

/* Count of open file descriptors. */
static Int fd_count = 0;


/* Note the fact that a file descriptor was just closed. */
static
void record_fd_close(Int fd)
{
   OpenFd *i = allocated_fds;

   if (fd >= VG_(fd_hard_limit))
      return;			/* Valgrind internal */

   while(i) {
      if(i->fd == fd) {
         if(i->prev)
            i->prev->next = i->next;
         else
            allocated_fds = i->next;
         if(i->next)
            i->next->prev = i->prev;
         if(i->pathname) 
            VG_(arena_free) (VG_AR_CORE, i->pathname);
         VG_(arena_free) (VG_AR_CORE, i);
         fd_count--;
         break;
      }
      i = i->next;
   }
}

/* Note the fact that a file descriptor was just opened.  If the
   tid is -1, this indicates an inherited fd.  If the pathname is NULL,
   this either indicates a non-standard file (i.e. a pipe or socket or
   some such thing) or that we don't know the filename.  If the fd is
   already open, then we're probably doing a dup2() to an existing fd,
   so just overwrite the existing one. */
void ML_(record_fd_open_with_given_name)(ThreadId tid, Int fd, char *pathname)
{
   OpenFd *i;

   if (fd >= VG_(fd_hard_limit))
      return;			/* Valgrind internal */

   /* Check to see if this fd is already open. */
   i = allocated_fds;
   while (i) {
      if (i->fd == fd) {
         if (i->pathname) VG_(arena_free)(VG_AR_CORE, i->pathname);
         break;
      }
      i = i->next;
   }

   /* Not already one: allocate an OpenFd */
   if (i == NULL) {
      i = VG_(arena_malloc)(VG_AR_CORE, "syswrap.rfdowgn.1", sizeof(OpenFd));

      i->prev = NULL;
      i->next = allocated_fds;
      if(allocated_fds) allocated_fds->prev = i;
      allocated_fds = i;
      fd_count++;
   }

   i->fd = fd;
   i->pathname = VG_(arena_strdup)(VG_AR_CORE, "syswrap.rfdowgn.2", pathname);
   i->where = (tid == -1) ? NULL : VG_(record_ExeContext)(tid, 0/*first_ip_delta*/);
}

// Record opening of an fd, and find its name.
void ML_(record_fd_open_named)(ThreadId tid, Int fd)
{
   static HChar buf[VKI_PATH_MAX];
   Char* name;
   if (VG_(resolve_filename)(fd, buf, VKI_PATH_MAX))
      name = buf;
   else
      name = NULL;
   
   ML_(record_fd_open_with_given_name)(tid, fd, name);
}

// Record opening of a nameless fd.
void ML_(record_fd_open_nameless)(ThreadId tid, Int fd)
{
   ML_(record_fd_open_with_given_name)(tid, fd, NULL);
}

static
Char *unix2name(struct vki_sockaddr_un *sa, UInt len, Char *name)
{
   if (sa == NULL || len == 0 || sa->sun_path[0] == '\0') {
      VG_(sprintf)(name, "<unknown>");
   } else {
      VG_(sprintf)(name, "%s", sa->sun_path);
   }

   return name;
}

static
Char *inet2name(struct vki_sockaddr_in *sa, UInt len, Char *name)
{
   if (sa == NULL || len == 0) {
      VG_(sprintf)(name, "<unknown>");
   } else {
      UInt addr = VG_(ntohl)(sa->sin_addr.s_addr);
      if (addr == 0) {
         VG_(sprintf)(name, "<unbound>");
      } else {
         VG_(sprintf)(name, "%u.%u.%u.%u:%u",
                      (addr>>24) & 0xFF, (addr>>16) & 0xFF,
                      (addr>>8) & 0xFF, addr & 0xFF,
                      VG_(ntohs)(sa->sin_port));
      }
   }

   return name;
}

/*
 * Try get some details about a socket.
 */
static void
getsockdetails(Int fd)
{
   union u {
      struct vki_sockaddr a;
      struct vki_sockaddr_in in;
      struct vki_sockaddr_un un;
   } laddr;
   UInt llen;

   llen = sizeof(laddr);
   VG_(memset)(&laddr, 0, llen);

   if(VG_(getsockname)(fd, (struct vki_sockaddr *)&(laddr.a), &llen) != -1) {
      switch(laddr.a.sa_family) {
      case VKI_AF_INET: {
         static char lname[32];
         static char pname[32];
         struct vki_sockaddr_in paddr;
         UInt plen = sizeof(struct vki_sockaddr_in);

         if (VG_(getpeername)(fd, (struct vki_sockaddr *)&paddr, &plen) != -1) {
            VG_(message)(Vg_UserMsg, "Open AF_INET socket %d: %s <-> %s\n", fd,
                         inet2name(&(laddr.in), llen, lname),
                         inet2name(&paddr, plen, pname));
         } else {
            VG_(message)(Vg_UserMsg, "Open AF_INET socket %d: %s <-> unbound\n",
                         fd, inet2name(&(laddr.in), llen, lname));
         }
         return;
         }
      case VKI_AF_UNIX: {
         static char lname[256];
         VG_(message)(Vg_UserMsg, "Open AF_UNIX socket %d: %s\n", fd,
                      unix2name(&(laddr.un), llen, lname));
         return;
         }
      default:
         VG_(message)(Vg_UserMsg, "Open pf-%d socket %d:\n",
                      laddr.a.sa_family, fd);
         return;
      }
   }

   VG_(message)(Vg_UserMsg, "Open socket %d:\n", fd);
}


/* Dump out a summary, and a more detailed list, of open file descriptors. */
void VG_(show_open_fds) (void)
{
   OpenFd *i = allocated_fds;

   VG_(message)(Vg_UserMsg, "FILE DESCRIPTORS: %d open at exit.\n", fd_count);

   while (i) {
      if (i->pathname) {
         VG_(message)(Vg_UserMsg, "Open file descriptor %d: %s\n", i->fd,
                      i->pathname);
      } else {
         Int val;
         UInt len = sizeof(val);

         if (VG_(getsockopt)(i->fd, VKI_SOL_SOCKET, VKI_SO_TYPE, &val, &len)
             == -1) {
            VG_(message)(Vg_UserMsg, "Open file descriptor %d:\n", i->fd);
         } else {
            getsockdetails(i->fd);
         }
      }

      if(i->where) {
         VG_(pp_ExeContext)(i->where);
         VG_(message)(Vg_UserMsg, "\n");
      } else {
         VG_(message)(Vg_UserMsg, "   <inherited from parent>\n");
         VG_(message)(Vg_UserMsg, "\n");
      }

      i = i->next;
   }

   VG_(message)(Vg_UserMsg, "\n");
}

/* If /proc/self/fd doesn't exist (e.g. you've got a Linux kernel that doesn't
   have /proc support compiled in, or a non-Linux kernel), then we need to
   find out what file descriptors we inherited from our parent process the
   hard way - by checking each fd in turn. */
static
void init_preopened_fds_without_proc_self_fd(void)
{
   struct vki_rlimit lim;
   UInt count;
   Int i;

   if (VG_(getrlimit) (VKI_RLIMIT_NOFILE, &lim) == -1) {
      /* Hmm.  getrlimit() failed.  Now we're screwed, so just choose
         an arbitrarily high number.  1024 happens to be the limit in
         the 2.4 Linux kernels. */
      count = 1024;
   } else {
      count = lim.rlim_cur;
   }

   for (i = 0; i < count; i++)
      if (VG_(fcntl)(i, VKI_F_GETFL, 0) != -1)
         ML_(record_fd_open_named)(-1, i);
}

/* Initialize the list of open file descriptors with the file descriptors
   we inherited from out parent process. */

void VG_(init_preopened_fds)(void)
{
// DDD: should probably use HAVE_PROC here or similar, instead.
#if defined(VGO_linux)
   Int ret;
   struct vki_dirent d;
   SysRes f;

   f = VG_(open)("/proc/self/fd", VKI_O_RDONLY, 0);
   if (sr_isError(f)) {
      init_preopened_fds_without_proc_self_fd();
      return;
   }

   while ((ret = VG_(getdents)(sr_Res(f), &d, sizeof(d))) != 0) {
      if (ret == -1)
         goto out;

      if (VG_(strcmp)(d.d_name, ".") && VG_(strcmp)(d.d_name, "..")) {
         Char* s;
         Int fno = VG_(strtoll10)(d.d_name, &s);
         if (*s == '\0') {
            if (fno != sr_Res(f))
               if (VG_(clo_track_fds))
                  ML_(record_fd_open_named)(-1, fno);
         } else {
            VG_(message)(Vg_DebugMsg, 
               "Warning: invalid file name in /proc/self/fd: %s\n",
               d.d_name);
         }
      }

      VG_(lseek)(sr_Res(f), d.d_off, VKI_SEEK_SET);
   }

  out:
   VG_(close)(sr_Res(f));

#elif defined(VGO_darwin)
   init_preopened_fds_without_proc_self_fd();

#else
#  error Unknown OS
#endif
}

static
Char *strdupcat ( HChar* cc, const Char *s1, const Char *s2, ArenaId aid )
{
   UInt len = VG_(strlen) ( s1 ) + VG_(strlen) ( s2 ) + 1;
   Char *result = VG_(arena_malloc) ( aid, cc, len );
   VG_(strcpy) ( result, s1 );
   VG_(strcat) ( result, s2 );
   return result;
}

static 
void pre_mem_read_sendmsg ( ThreadId tid, Bool read,
                            Char *msg, Addr base, SizeT size )
{
   Char *outmsg = strdupcat ( "di.syswrap.pmrs.1",
                              "sendmsg", msg, VG_AR_CORE );
   PRE_MEM_READ( outmsg, base, size );
   VG_(arena_free) ( VG_AR_CORE, outmsg );
}

static 
void pre_mem_write_recvmsg ( ThreadId tid, Bool read,
                             Char *msg, Addr base, SizeT size )
{
   Char *outmsg = strdupcat ( "di.syswrap.pmwr.1",
                              "recvmsg", msg, VG_AR_CORE );
   if ( read )
      PRE_MEM_READ( outmsg, base, size );
   else
      PRE_MEM_WRITE( outmsg, base, size );
   VG_(arena_free) ( VG_AR_CORE, outmsg );
}

static
void post_mem_write_recvmsg ( ThreadId tid, Bool read,
                              Char *fieldName, Addr base, SizeT size )
{
   if ( !read )
      POST_MEM_WRITE( base, size );
}
 
static
void msghdr_foreachfield ( 
        ThreadId tid,
        Char *name,
        struct vki_msghdr *msg,
        UInt length,
        void (*foreach_func)( ThreadId, Bool, Char *, Addr, SizeT ) 
     )
{
   Char *fieldName;

   if ( !msg )
      return;

   fieldName = VG_(arena_malloc) ( VG_AR_CORE, "di.syswrap.mfef", VG_(strlen)(name) + 32 );

   VG_(sprintf) ( fieldName, "(%s)", name );

   foreach_func ( tid, True, fieldName, (Addr)&msg->msg_name, sizeof( msg->msg_name ) );
   foreach_func ( tid, True, fieldName, (Addr)&msg->msg_namelen, sizeof( msg->msg_namelen ) );
   foreach_func ( tid, True, fieldName, (Addr)&msg->msg_iov, sizeof( msg->msg_iov ) );
   foreach_func ( tid, True, fieldName, (Addr)&msg->msg_iovlen, sizeof( msg->msg_iovlen ) );
   foreach_func ( tid, True, fieldName, (Addr)&msg->msg_control, sizeof( msg->msg_control ) );
   foreach_func ( tid, True, fieldName, (Addr)&msg->msg_controllen, sizeof( msg->msg_controllen ) );
   foreach_func ( tid, False, fieldName, (Addr)&msg->msg_flags, sizeof( msg->msg_flags ) );

   if ( msg->msg_name ) {
      VG_(sprintf) ( fieldName, "(%s.msg_name)", name );
      foreach_func ( tid, False, fieldName, 
                     (Addr)msg->msg_name, msg->msg_namelen );
   }

   if ( msg->msg_iov ) {
      struct vki_iovec *iov = msg->msg_iov;
      UInt i;

      VG_(sprintf) ( fieldName, "(%s.msg_iov)", name );

      foreach_func ( tid, True, fieldName, 
                     (Addr)iov, msg->msg_iovlen * sizeof( struct vki_iovec ) );

      for ( i = 0; i < msg->msg_iovlen; ++i, ++iov ) {
         UInt iov_len = iov->iov_len <= length ? iov->iov_len : length;
         VG_(sprintf) ( fieldName, "(%s.msg_iov[%u])", name, i );
         foreach_func ( tid, False, fieldName, 
                        (Addr)iov->iov_base, iov_len );
         length = length - iov_len;
      }
   }

   if ( msg->msg_control ) 
   {
      VG_(sprintf) ( fieldName, "(%s.msg_control)", name );
      foreach_func ( tid, False, fieldName, 
                     (Addr)msg->msg_control, msg->msg_controllen );
   }

   VG_(arena_free) ( VG_AR_CORE, fieldName );
}

static void check_cmsg_for_fds(ThreadId tid, struct vki_msghdr *msg)
{
   struct vki_cmsghdr *cm = VKI_CMSG_FIRSTHDR(msg);

   while (cm) {
      if (cm->cmsg_level == VKI_SOL_SOCKET &&
          cm->cmsg_type == VKI_SCM_RIGHTS ) {
         Int *fds = (Int *) VKI_CMSG_DATA(cm);
         Int fdc = (cm->cmsg_len - VKI_CMSG_ALIGN(sizeof(struct vki_cmsghdr)))
                         / sizeof(int);
         Int i;

         for (i = 0; i < fdc; i++)
            if(VG_(clo_track_fds))
               // XXX: must we check the range on these fds with
               //      ML_(fd_allowed)()?
               ML_(record_fd_open_named)(tid, fds[i]);
      }

      cm = VKI_CMSG_NXTHDR(msg, cm);
   }
}

/* GrP kernel ignores sa_len (at least on Darwin); this checks the rest */
static
void pre_mem_read_sockaddr ( ThreadId tid,
                             Char *description,
                             struct vki_sockaddr *sa, UInt salen )
{
   Char *outmsg;
   struct vki_sockaddr_un*  sun  = (struct vki_sockaddr_un *)sa;
   struct vki_sockaddr_in*  sin  = (struct vki_sockaddr_in *)sa;
   struct vki_sockaddr_in6* sin6 = (struct vki_sockaddr_in6 *)sa;

   /* NULL/zero-length sockaddrs are legal */
   if ( sa == NULL || salen == 0 ) return;

   outmsg = VG_(arena_malloc) ( VG_AR_CORE, "di.syswrap.pmr_sockaddr.1",
                                VG_(strlen)( description ) + 30 );

   VG_(sprintf) ( outmsg, description, "sa_family" );
   PRE_MEM_READ( outmsg, (Addr) &sa->sa_family, sizeof(vki_sa_family_t));

   switch (sa->sa_family) {
                  
      case VKI_AF_UNIX:
         VG_(sprintf) ( outmsg, description, "sun_path" );
         PRE_MEM_RASCIIZ( outmsg, (Addr) sun->sun_path );
         // GrP fixme max of sun_len-2? what about nul char?
         break;
                     
      case VKI_AF_INET:
         VG_(sprintf) ( outmsg, description, "sin_port" );
         PRE_MEM_READ( outmsg, (Addr) &sin->sin_port, sizeof (sin->sin_port) );
         VG_(sprintf) ( outmsg, description, "sin_addr" );
         PRE_MEM_READ( outmsg, (Addr) &sin->sin_addr, sizeof (sin->sin_addr) );
         break;
                           
      case VKI_AF_INET6:
         VG_(sprintf) ( outmsg, description, "sin6_port" );
         PRE_MEM_READ( outmsg,
            (Addr) &sin6->sin6_port, sizeof (sin6->sin6_port) );
         VG_(sprintf) ( outmsg, description, "sin6_flowinfo" );
         PRE_MEM_READ( outmsg,
            (Addr) &sin6->sin6_flowinfo, sizeof (sin6->sin6_flowinfo) );
         VG_(sprintf) ( outmsg, description, "sin6_addr" );
         PRE_MEM_READ( outmsg,
            (Addr) &sin6->sin6_addr, sizeof (sin6->sin6_addr) );
         VG_(sprintf) ( outmsg, description, "sin6_scope_id" );
         PRE_MEM_READ( outmsg,
            (Addr) &sin6->sin6_scope_id, sizeof (sin6->sin6_scope_id) );
         break;
               
      default:
         VG_(sprintf) ( outmsg, description, "" );
         PRE_MEM_READ( outmsg, (Addr) sa, salen );
         break;
   }
   
   VG_(arena_free) ( VG_AR_CORE, outmsg );
}

/* Dereference a pointer to a UInt. */
static UInt deref_UInt ( ThreadId tid, Addr a, Char* s )
{
   UInt* a_p = (UInt*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(UInt) );
   if (a_p == NULL)
      return 0;
   else
      return *a_p;
}

void ML_(buf_and_len_pre_check) ( ThreadId tid, Addr buf_p, Addr buflen_p,
                                  Char* buf_s, Char* buflen_s )
{
   if (VG_(tdict).track_pre_mem_write) {
      UInt buflen_in = deref_UInt( tid, buflen_p, buflen_s);
      if (buflen_in > 0) {
         VG_(tdict).track_pre_mem_write(
            Vg_CoreSysCall, tid, buf_s, buf_p, buflen_in );
      }
   }
}

void ML_(buf_and_len_post_check) ( ThreadId tid, SysRes res,
                                   Addr buf_p, Addr buflen_p, Char* s )
{
   if (!sr_isError(res) && VG_(tdict).track_post_mem_write) {
      UInt buflen_out = deref_UInt( tid, buflen_p, s);
      if (buflen_out > 0 && buf_p != (Addr)NULL) {
         VG_(tdict).track_post_mem_write( Vg_CoreSysCall, tid, buf_p, buflen_out );
      }
   }
}

/* ---------------------------------------------------------------------
   Data seg end, for brk()
   ------------------------------------------------------------------ */

/*   +--------+------------+
     | anon   |    resvn   |
     +--------+------------+

     ^     ^  ^
     |     |  boundary is page aligned
     |     VG_(brk_limit) -- no alignment constraint
     VG_(brk_base) -- page aligned -- does not move

     Both the anon part and the reservation part are always at least
     one page.  
*/

/* Set the new data segment end to NEWBRK.  If this succeeds, return
   NEWBRK, else return the current data segment end. */

static Addr do_brk ( Addr newbrk )
{
   NSegment const* aseg;
   NSegment const* rseg;
   Addr newbrkP;
   SizeT delta;
   Bool ok;
   Bool debug = False;

   if (debug)
      VG_(printf)("\ndo_brk: brk_base=%#lx brk_limit=%#lx newbrk=%#lx\n",
		  VG_(brk_base), VG_(brk_limit), newbrk);

#  if 0
   if (0) show_segments("in_brk");
#  endif

   if (newbrk < VG_(brk_base))
      /* Clearly impossible. */
      goto bad;

   if (newbrk >= VG_(brk_base) && newbrk < VG_(brk_limit)) {
      /* shrinking the data segment.  Be lazy and don't munmap the
         excess area. */
      NSegment const * seg = VG_(am_find_nsegment)(newbrk);
      if (seg && seg->hasT)
         VG_(discard_translations)( newbrk, VG_(brk_limit) - newbrk, 
                                    "do_brk(shrink)" );
      /* Since we're being lazy and not unmapping pages, we have to
         zero out the area, so that if the area later comes back into
         circulation, it will be filled with zeroes, as if it really
         had been unmapped and later remapped.  Be a bit paranoid and
         try hard to ensure we're not going to segfault by doing the
         write - check both ends of the range are in the same segment
         and that segment is writable. */
      if (seg) {
         /* pre: newbrk < VG_(brk_limit) 
              => newbrk <= VG_(brk_limit)-1 */
         NSegment const * seg2;
         vg_assert(newbrk < VG_(brk_limit));
         seg2 = VG_(am_find_nsegment)( VG_(brk_limit)-1 );
         if (seg2 && seg == seg2 && seg->hasW)
            VG_(memset)( (void*)newbrk, 0, VG_(brk_limit) - newbrk );
      }

      VG_(brk_limit) = newbrk;
      return newbrk;
   }

   /* otherwise we're expanding the brk segment. */
   if (VG_(brk_limit) > VG_(brk_base))
      aseg = VG_(am_find_nsegment)( VG_(brk_limit)-1 );
   else
      aseg = VG_(am_find_nsegment)( VG_(brk_limit) );
   rseg = VG_(am_next_nsegment)( (NSegment*)aseg, True/*forwards*/ );

   /* These should be assured by setup_client_dataseg in m_main. */
   vg_assert(aseg);
   vg_assert(rseg);
   vg_assert(aseg->kind == SkAnonC);
   vg_assert(rseg->kind == SkResvn);
   vg_assert(aseg->end+1 == rseg->start);

   vg_assert(newbrk >= VG_(brk_base));
   if (newbrk <= rseg->start) {
      /* still fits within the anon segment. */
      VG_(brk_limit) = newbrk;
      return newbrk;
   }

   if (newbrk > rseg->end+1 - VKI_PAGE_SIZE) {
      /* request is too large -- the resvn would fall below 1 page,
         which isn't allowed. */
      goto bad;
   }

   newbrkP = VG_PGROUNDUP(newbrk);
   vg_assert(newbrkP > rseg->start && newbrkP <= rseg->end+1 - VKI_PAGE_SIZE);
   delta = newbrkP - rseg->start;
   vg_assert(delta > 0);
   vg_assert(VG_IS_PAGE_ALIGNED(delta));
   
   ok = VG_(am_extend_into_adjacent_reservation_client)( (NSegment*)aseg, delta );
   if (!ok) goto bad;

   VG_(brk_limit) = newbrk;
   return newbrk;

  bad:
   return VG_(brk_limit);
}


/* ---------------------------------------------------------------------
   Vet file descriptors for sanity
   ------------------------------------------------------------------ */
/* 
> - what does the "Bool soft" parameter mean?

(Tom Hughes, 3 Oct 05):

Whether or not to consider a file descriptor invalid if it is above
the current soft limit.

Basically if we are testing whether a newly created file descriptor is
valid (in a post handler) then we set soft to true, and if we are
testing whether a file descriptor that is about to be used (in a pre
handler) is valid [viz, an already-existing fd] then we set it to false.

The point is that if the (virtual) soft limit is lowered then any
existing descriptors can still be read/written/closed etc (so long as
they are below the valgrind reserved descriptors) but no new
descriptors can be created above the new soft limit.

(jrs 4 Oct 05: in which case, I've renamed it "isNewFd")
*/

/* Return true if we're allowed to use or create this fd */
Bool ML_(fd_allowed)(Int fd, const Char *syscallname, ThreadId tid, Bool isNewFd)
{
   Bool allowed = True;

   /* hard limits always apply */
   if (fd < 0 || fd >= VG_(fd_hard_limit))
      allowed = False;

   /* hijacking the output fds is never allowed */
   if (fd == VG_(log_output_sink).fd || fd == VG_(xml_output_sink).fd)
      allowed = False;

   /* if creating a new fd (rather than using an existing one), the
      soft limit must also be observed */
   if (isNewFd && fd >= VG_(fd_soft_limit))
      allowed = False;

   /* this looks like it ought to be included, but causes problems: */
   /*
   if (fd == 2 && VG_(debugLog_getLevel)() > 0)
      allowed = False;
   */
   /* The difficulty is as follows: consider a program P which expects
      to be able to mess with (redirect) its own stderr (fd 2).
      Usually to deal with P we would issue command line flags to send
      logging somewhere other than stderr, so as not to disrupt P.
      The problem is that -d unilaterally hijacks stderr with no
      consultation with P.  And so, if this check is enabled, P will
      work OK normally but fail if -d is issued.

      Basically -d is a hack and you take your chances when using it.
      It's very useful for low level debugging -- particularly at
      startup -- and having its presence change the behaviour of the
      client is exactly what we don't want.  */

   /* croak? */
   if ((!allowed) && VG_(showing_core_errors)() ) {
      VG_(message)(Vg_UserMsg, 
         "Warning: invalid file descriptor %d in syscall %s()\n",
         fd, syscallname);
      if (fd == VG_(log_output_sink).fd && VG_(log_output_sink).fd >= 0)
	 VG_(message)(Vg_UserMsg, 
            "   Use --log-fd=<number> to select an alternative log fd.\n");
      if (fd == VG_(xml_output_sink).fd && VG_(xml_output_sink).fd >= 0)
	 VG_(message)(Vg_UserMsg, 
            "   Use --xml-fd=<number> to select an alternative XML "
            "output fd.\n");
      // DDD: consider always printing this stack trace, it's useful.
      // Also consider also making this a proper core error, ie.
      // suppressible and all that.
      if (VG_(clo_verbosity) > 1) {
         VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      }
   }

   return allowed;
}


/* ---------------------------------------------------------------------
   Deal with a bunch of socket-related syscalls
   ------------------------------------------------------------------ */

/* ------ */

void 
ML_(generic_PRE_sys_socketpair) ( ThreadId tid,
                                  UWord arg0, UWord arg1, 
                                  UWord arg2, UWord arg3 )
{
   /* int socketpair(int d, int type, int protocol, int sv[2]); */
   PRE_MEM_WRITE( "socketcall.socketpair(sv)", 
                  arg3, 2*sizeof(int) );
}

SysRes
ML_(generic_POST_sys_socketpair) ( ThreadId tid,
                                   SysRes res,
                                   UWord arg0, UWord arg1, 
                                   UWord arg2, UWord arg3 )
{
   SysRes r = res;
   Int fd1 = ((Int*)arg3)[0];
   Int fd2 = ((Int*)arg3)[1];
   vg_assert(!sr_isError(res)); /* guaranteed by caller */
   POST_MEM_WRITE( arg3, 2*sizeof(int) );
   if (!ML_(fd_allowed)(fd1, "socketcall.socketpair", tid, True) ||
       !ML_(fd_allowed)(fd2, "socketcall.socketpair", tid, True)) {
      VG_(close)(fd1);
      VG_(close)(fd2);
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      POST_MEM_WRITE( arg3, 2*sizeof(int) );
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, fd1);
         ML_(record_fd_open_nameless)(tid, fd2);
      }
   }
   return r;
}

/* ------ */

SysRes 
ML_(generic_POST_sys_socket) ( ThreadId tid, SysRes res )
{
   SysRes r = res;
   vg_assert(!sr_isError(res)); /* guaranteed by caller */
   if (!ML_(fd_allowed)(sr_Res(res), "socket", tid, True)) {
      VG_(close)(sr_Res(res));
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless)(tid, sr_Res(res));
   }
   return r;
}

/* ------ */

void 
ML_(generic_PRE_sys_bind) ( ThreadId tid,
                            UWord arg0, UWord arg1, UWord arg2 )
{
   /* int bind(int sockfd, struct sockaddr *my_addr, 
               int addrlen); */
   pre_mem_read_sockaddr( 
      tid, "socketcall.bind(my_addr.%s)",
      (struct vki_sockaddr *) arg1, arg2 
   );
}

/* ------ */

void 
ML_(generic_PRE_sys_accept) ( ThreadId tid,
                              UWord arg0, UWord arg1, UWord arg2 )
{
   /* int accept(int s, struct sockaddr *addr, int *addrlen); */
   Addr addr_p     = arg1;
   Addr addrlen_p  = arg2;
   if (addr_p != (Addr)NULL) 
      ML_(buf_and_len_pre_check) ( tid, addr_p, addrlen_p,
                                   "socketcall.accept(addr)",
                                   "socketcall.accept(addrlen_in)" );
}

SysRes 
ML_(generic_POST_sys_accept) ( ThreadId tid,
                               SysRes res,
                               UWord arg0, UWord arg1, UWord arg2 )
{
   SysRes r = res;
   vg_assert(!sr_isError(res)); /* guaranteed by caller */
   if (!ML_(fd_allowed)(sr_Res(res), "accept", tid, True)) {
      VG_(close)(sr_Res(res));
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      Addr addr_p     = arg1;
      Addr addrlen_p  = arg2;
      if (addr_p != (Addr)NULL) 
         ML_(buf_and_len_post_check) ( tid, res, addr_p, addrlen_p,
                                       "socketcall.accept(addrlen_out)" );
      if (VG_(clo_track_fds))
          ML_(record_fd_open_nameless)(tid, sr_Res(res));
   }
   return r;
}

/* ------ */

void 
ML_(generic_PRE_sys_sendto) ( ThreadId tid, 
                              UWord arg0, UWord arg1, UWord arg2,
                              UWord arg3, UWord arg4, UWord arg5 )
{
   /* int sendto(int s, const void *msg, int len, 
                 unsigned int flags, 
                 const struct sockaddr *to, int tolen); */
   PRE_MEM_READ( "socketcall.sendto(msg)",
                 arg1, /* msg */
                 arg2  /* len */ );
   pre_mem_read_sockaddr( 
      tid, "socketcall.sendto(to.%s)",
      (struct vki_sockaddr *) arg4, arg5
   );
}

/* ------ */

void 
ML_(generic_PRE_sys_send) ( ThreadId tid,
                            UWord arg0, UWord arg1, UWord arg2 )
{
   /* int send(int s, const void *msg, size_t len, int flags); */
   PRE_MEM_READ( "socketcall.send(msg)",
                  arg1, /* msg */
                  arg2  /* len */ );

}

/* ------ */

void 
ML_(generic_PRE_sys_recvfrom) ( ThreadId tid, 
                                UWord arg0, UWord arg1, UWord arg2,
                                UWord arg3, UWord arg4, UWord arg5 )
{
   /* int recvfrom(int s, void *buf, int len, unsigned int flags,
                   struct sockaddr *from, int *fromlen); */
   Addr buf_p      = arg1;
   Int  len        = arg2;
   Addr from_p     = arg4;
   Addr fromlen_p  = arg5;
   PRE_MEM_WRITE( "socketcall.recvfrom(buf)", buf_p, len );
   if (from_p != (Addr)NULL) 
      ML_(buf_and_len_pre_check) ( tid, from_p, fromlen_p, 
                                   "socketcall.recvfrom(from)",
                                   "socketcall.recvfrom(fromlen_in)" );
}

void 
ML_(generic_POST_sys_recvfrom) ( ThreadId tid,
                                 SysRes res,
                                 UWord arg0, UWord arg1, UWord arg2,
                                 UWord arg3, UWord arg4, UWord arg5 )
{
   Addr buf_p      = arg1;
   Int  len        = arg2;
   Addr from_p     = arg4;
   Addr fromlen_p  = arg5;

   vg_assert(!sr_isError(res)); /* guaranteed by caller */
   if (from_p != (Addr)NULL) 
      ML_(buf_and_len_post_check) ( tid, res, from_p, fromlen_p,
                                    "socketcall.recvfrom(fromlen_out)" );
   POST_MEM_WRITE( buf_p, len );
}

/* ------ */

void 
ML_(generic_PRE_sys_recv) ( ThreadId tid,
                            UWord arg0, UWord arg1, UWord arg2 )
{
   /* int recv(int s, void *buf, int len, unsigned int flags); */
   /* man 2 recv says:
      The  recv call is normally used only on a connected socket
      (see connect(2)) and is identical to recvfrom with a  NULL
      from parameter.
   */
   PRE_MEM_WRITE( "socketcall.recv(buf)", 
                  arg1, /* buf */
                  arg2  /* len */ );
}

void 
ML_(generic_POST_sys_recv) ( ThreadId tid, 
                             UWord res,
                             UWord arg0, UWord arg1, UWord arg2 )
{
   if (res >= 0 && arg1 != 0) {
      POST_MEM_WRITE( arg1, /* buf */
                      arg2  /* len */ );
   }
}

/* ------ */

void 
ML_(generic_PRE_sys_connect) ( ThreadId tid,
                               UWord arg0, UWord arg1, UWord arg2 )
{
   /* int connect(int sockfd, 
                  struct sockaddr *serv_addr, int addrlen ); */
   pre_mem_read_sockaddr( tid,
                          "socketcall.connect(serv_addr.%s)",
                          (struct vki_sockaddr *) arg1, arg2);
}

/* ------ */

void 
ML_(generic_PRE_sys_setsockopt) ( ThreadId tid, 
                                  UWord arg0, UWord arg1, UWord arg2,
                                  UWord arg3, UWord arg4 )
{
   /* int setsockopt(int s, int level, int optname, 
                     const void *optval, int optlen); */
   PRE_MEM_READ( "socketcall.setsockopt(optval)",
                 arg3, /* optval */
                 arg4  /* optlen */ );
}

/* ------ */

void 
ML_(generic_PRE_sys_getsockname) ( ThreadId tid,
                                   UWord arg0, UWord arg1, UWord arg2 )
{
   /* int getsockname(int s, struct sockaddr* name, int* namelen) */
   Addr name_p     = arg1;
   Addr namelen_p  = arg2;
   /* Nb: name_p cannot be NULL */
   ML_(buf_and_len_pre_check) ( tid, name_p, namelen_p,
                                "socketcall.getsockname(name)",
                                "socketcall.getsockname(namelen_in)" );
}

void 
ML_(generic_POST_sys_getsockname) ( ThreadId tid,
                                    SysRes res,
                                    UWord arg0, UWord arg1, UWord arg2 )
{
   Addr name_p     = arg1;
   Addr namelen_p  = arg2;
   vg_assert(!sr_isError(res)); /* guaranteed by caller */
   ML_(buf_and_len_post_check) ( tid, res, name_p, namelen_p,
                                 "socketcall.getsockname(namelen_out)" );
}

/* ------ */

void 
ML_(generic_PRE_sys_getpeername) ( ThreadId tid,
                                   UWord arg0, UWord arg1, UWord arg2 )
{
   /* int getpeername(int s, struct sockaddr* name, int* namelen) */
   Addr name_p     = arg1;
   Addr namelen_p  = arg2;
   /* Nb: name_p cannot be NULL */
   ML_(buf_and_len_pre_check) ( tid, name_p, namelen_p,
                                "socketcall.getpeername(name)",
                                "socketcall.getpeername(namelen_in)" );
}

void 
ML_(generic_POST_sys_getpeername) ( ThreadId tid,
                                    SysRes res,
                                    UWord arg0, UWord arg1, UWord arg2 )
{
   Addr name_p     = arg1;
   Addr namelen_p  = arg2;
   vg_assert(!sr_isError(res)); /* guaranteed by caller */
   ML_(buf_and_len_post_check) ( tid, res, name_p, namelen_p,
                                 "socketcall.getpeername(namelen_out)" );
}

/* ------ */

void 
ML_(generic_PRE_sys_sendmsg) ( ThreadId tid, Char *name, struct vki_msghdr *msg )
{
   msghdr_foreachfield ( tid, name, msg, ~0, pre_mem_read_sendmsg );
}

/* ------ */

void
ML_(generic_PRE_sys_recvmsg) ( ThreadId tid, Char *name, struct vki_msghdr *msg )
{
   msghdr_foreachfield ( tid, name, msg, ~0, pre_mem_write_recvmsg );
}

void 
ML_(generic_POST_sys_recvmsg) ( ThreadId tid, Char *name, struct vki_msghdr *msg, UInt length )
{
   msghdr_foreachfield( tid, name, msg, length, post_mem_write_recvmsg );
   check_cmsg_for_fds( tid, msg );
}


/* ---------------------------------------------------------------------
   Deal with a bunch of IPC related syscalls
   ------------------------------------------------------------------ */

/* ------ */

void
ML_(generic_PRE_sys_semop) ( ThreadId tid,
                             UWord arg0, UWord arg1, UWord arg2 )
{
   /* int semop(int semid, struct sembuf *sops, unsigned nsops); */
   PRE_MEM_READ( "semop(sops)", arg1, arg2 * sizeof(struct vki_sembuf) );
}

/* ------ */

void
ML_(generic_PRE_sys_semtimedop) ( ThreadId tid,
                                  UWord arg0, UWord arg1,
                                  UWord arg2, UWord arg3 )
{
   /* int semtimedop(int semid, struct sembuf *sops, unsigned nsops,
                     struct timespec *timeout); */
   PRE_MEM_READ( "semtimedop(sops)", arg1, arg2 * sizeof(struct vki_sembuf) );
   if (arg3 != 0)
      PRE_MEM_READ( "semtimedop(timeout)", arg3, sizeof(struct vki_timespec) );
}

/* ------ */

static
UInt get_sem_count( Int semid )
{
   struct vki_semid_ds buf;
   union vki_semun arg;
   SysRes res;

   /* Doesn't actually seem to be necessary, but gcc-4.4.0 20081017
      (experimental) otherwise complains that the use in the return
      statement below is uninitialised. */
   buf.sem_nsems = 0;

   arg.buf = &buf;

#  ifdef __NR_semctl
   res = VG_(do_syscall4)(__NR_semctl, semid, 0, VKI_IPC_STAT, *(UWord *)&arg);
#  else
   res = VG_(do_syscall5)(__NR_ipc, 3 /* IPCOP_semctl */, semid, 0,
                          VKI_IPC_STAT, (UWord)&arg);
#  endif
   if (sr_isError(res))
      return 0;

   return buf.sem_nsems;
}

void
ML_(generic_PRE_sys_semctl) ( ThreadId tid,
                              UWord arg0, UWord arg1,
                              UWord arg2, UWord arg3 )
{
   /* int semctl(int semid, int semnum, int cmd, ...); */
   union vki_semun arg = *(union vki_semun *)&arg3;
   UInt nsems;
   switch (arg2 /* cmd */) {
#if defined(VKI_IPC_INFO)
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_SEM_INFO|VKI_IPC_64:
      PRE_MEM_WRITE( "semctl(IPC_INFO, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_seminfo) );
      break;
#endif

   case VKI_IPC_STAT:
#if defined(VKI_SEM_STAT)
   case VKI_SEM_STAT:
#endif
      PRE_MEM_WRITE( "semctl(IPC_STAT, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_semid_ds) );
      break;

#if defined(VKI_IPC_64)
   case VKI_IPC_STAT|VKI_IPC_64:
#if defined(VKI_SEM_STAT)
   case VKI_SEM_STAT|VKI_IPC_64:
#endif
      PRE_MEM_WRITE( "semctl(IPC_STAT, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_semid64_ds) );
      break;
#endif

   case VKI_IPC_SET:
      PRE_MEM_READ( "semctl(IPC_SET, arg.buf)",
                    (Addr)arg.buf, sizeof(struct vki_semid_ds) );
      break;

#if defined(VKI_IPC_64)
   case VKI_IPC_SET|VKI_IPC_64:
      PRE_MEM_READ( "semctl(IPC_SET, arg.buf)",
                    (Addr)arg.buf, sizeof(struct vki_semid64_ds) );
      break;
#endif

   case VKI_GETALL:
#if defined(VKI_IPC_64)
   case VKI_GETALL|VKI_IPC_64:
#endif
      nsems = get_sem_count( arg0 );
      PRE_MEM_WRITE( "semctl(IPC_GETALL, arg.array)",
                     (Addr)arg.array, sizeof(unsigned short) * nsems );
      break;

   case VKI_SETALL:
#if defined(VKI_IPC_64)
   case VKI_SETALL|VKI_IPC_64:
#endif
      nsems = get_sem_count( arg0 );
      PRE_MEM_READ( "semctl(IPC_SETALL, arg.array)",
                    (Addr)arg.array, sizeof(unsigned short) * nsems );
      break;
   }
}

void
ML_(generic_POST_sys_semctl) ( ThreadId tid,
                               UWord res,
                               UWord arg0, UWord arg1,
                               UWord arg2, UWord arg3 )
{
   union vki_semun arg = *(union vki_semun *)&arg3;
   UInt nsems;
   switch (arg2 /* cmd */) {
#if defined(VKI_IPC_INFO)
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_SEM_INFO|VKI_IPC_64:
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_seminfo) );
      break;
#endif

   case VKI_IPC_STAT:
#if defined(VKI_SEM_STAT)
   case VKI_SEM_STAT:
#endif
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_semid_ds) );
      break;

#if defined(VKI_IPC_64)
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_SEM_STAT|VKI_IPC_64:
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_semid64_ds) );
      break;
#endif

   case VKI_GETALL:
#if defined(VKI_IPC_64)
   case VKI_GETALL|VKI_IPC_64:
#endif
      nsems = get_sem_count( arg0 );
      POST_MEM_WRITE( (Addr)arg.array, sizeof(unsigned short) * nsems );
      break;
   }
}

/* ------ */

/* ------ */

static
SizeT get_shm_size ( Int shmid )
{
#ifdef __NR_shmctl
#  ifdef VKI_IPC_64
   struct vki_shmid64_ds buf;
#    ifdef VGP_amd64_linux
     /* See bug 222545 comment 7 */
     SysRes __res = VG_(do_syscall3)(__NR_shmctl, shmid, 
                                     VKI_IPC_STAT, (UWord)&buf);
#    else
     SysRes __res = VG_(do_syscall3)(__NR_shmctl, shmid,
                                     VKI_IPC_STAT|VKI_IPC_64, (UWord)&buf);
#    endif
#  else /* !def VKI_IPC_64 */
   struct vki_shmid_ds buf;
   SysRes __res = VG_(do_syscall3)(__NR_shmctl, shmid, VKI_IPC_STAT, (UWord)&buf);
#  endif /* def VKI_IPC_64 */
#else
   struct vki_shmid_ds buf;
   SysRes __res = VG_(do_syscall5)(__NR_ipc, 24 /* IPCOP_shmctl */, shmid,
                                 VKI_IPC_STAT, 0, (UWord)&buf);
#endif
   if (sr_isError(__res))
      return 0;
 
   return (SizeT) buf.shm_segsz;
}

UWord
ML_(generic_PRE_sys_shmat) ( ThreadId tid,
                             UWord arg0, UWord arg1, UWord arg2 )
{
   /* void *shmat(int shmid, const void *shmaddr, int shmflg); */
   SizeT  segmentSize = get_shm_size ( arg0 );
   UWord tmp;
   Bool  ok;
   if (arg1 == 0) {
      /* arm-linux only: work around the fact that
         VG_(am_get_advisory_client_simple) produces something that is
         VKI_PAGE_SIZE aligned, whereas what we want is something
         VKI_SHMLBA aligned, and VKI_SHMLBA >= VKI_PAGE_SIZE.  Hence
         increase the request size by VKI_SHMLBA - VKI_PAGE_SIZE and
         then round the result up to the next VKI_SHMLBA boundary.
         See bug 222545 comment 15.  So far, arm-linux is the only
         platform where this is known to be necessary. */
      vg_assert(VKI_SHMLBA >= VKI_PAGE_SIZE);
      if (VKI_SHMLBA > VKI_PAGE_SIZE) {
         segmentSize += VKI_SHMLBA - VKI_PAGE_SIZE;
      }
      tmp = VG_(am_get_advisory_client_simple)(0, segmentSize, &ok);
      if (ok) {
         if (VKI_SHMLBA > VKI_PAGE_SIZE) {
            arg1 = VG_ROUNDUP(tmp, VKI_SHMLBA);
         } else {
            arg1 = tmp;
         }
      }
   }
   else if (!ML_(valid_client_addr)(arg1, segmentSize, tid, "shmat"))
      arg1 = 0;
   return arg1;
}

void
ML_(generic_POST_sys_shmat) ( ThreadId tid,
                              UWord res,
                              UWord arg0, UWord arg1, UWord arg2 )
{
   SizeT segmentSize = VG_PGROUNDUP(get_shm_size(arg0));
   if ( segmentSize > 0 ) {
      UInt prot = VKI_PROT_READ|VKI_PROT_WRITE;
      Bool d;

      if (arg2 & VKI_SHM_RDONLY)
         prot &= ~VKI_PROT_WRITE;
      /* It isn't exactly correct to pass 0 for the fd and offset
         here.  The kernel seems to think the corresponding section
         does have dev/ino numbers:
         
         04e52000-04ec8000 rw-s 00000000 00:06 1966090  /SYSV00000000 (deleted)

         However there is no obvious way to find them.  In order to
         cope with the discrepancy, aspacem's sync checker omits the
         dev/ino correspondence check in cases where V does not know
         the dev/ino. */
      d = VG_(am_notify_client_shmat)( res, segmentSize, prot );

      /* we don't distinguish whether it's read-only or
       * read-write -- it doesn't matter really. */
      VG_TRACK( new_mem_mmap, res, segmentSize, True, True, False,
                              0/*di_handle*/ );
      if (d)
         VG_(discard_translations)( (Addr64)res, 
                                    (ULong)VG_PGROUNDUP(segmentSize),
                                    "ML_(generic_POST_sys_shmat)" );
   }
}

/* ------ */

Bool
ML_(generic_PRE_sys_shmdt) ( ThreadId tid, UWord arg0 )
{
   /* int shmdt(const void *shmaddr); */
   return ML_(valid_client_addr)(arg0, 1, tid, "shmdt");
}

void
ML_(generic_POST_sys_shmdt) ( ThreadId tid, UWord res, UWord arg0 )
{
   NSegment const* s = VG_(am_find_nsegment)(arg0);

   if (s != NULL) {
      Addr  s_start = s->start;
      SizeT s_len   = s->end+1 - s->start;
      Bool  d;

      vg_assert(s->kind == SkShmC);
      vg_assert(s->start == arg0);

      d = VG_(am_notify_munmap)(s_start, s_len);
      s = NULL; /* s is now invalid */
      VG_TRACK( die_mem_munmap, s_start, s_len );
      if (d)
         VG_(discard_translations)( (Addr64)s_start,
                                    (ULong)s_len,
                                    "ML_(generic_POST_sys_shmdt)" );
   }
}
/* ------ */

void
ML_(generic_PRE_sys_shmctl) ( ThreadId tid,
                              UWord arg0, UWord arg1, UWord arg2 )
{
   /* int shmctl(int shmid, int cmd, struct shmid_ds *buf); */
   switch (arg1 /* cmd */) {
#if defined(VKI_IPC_INFO)
   case VKI_IPC_INFO:
      PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_shminfo) );
      break;
#if defined(VKI_IPC_64)
   case VKI_IPC_INFO|VKI_IPC_64:
      PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_shminfo64) );
      break;
#endif
#endif

#if defined(VKI_SHM_INFO)
   case VKI_SHM_INFO:
#if defined(VKI_IPC_64)
   case VKI_SHM_INFO|VKI_IPC_64:
#endif
      PRE_MEM_WRITE( "shmctl(SHM_INFO, buf)",
                     arg2, sizeof(struct vki_shm_info) );
      break;
#endif

   case VKI_IPC_STAT:
#if defined(VKI_SHM_STAT)
   case VKI_SHM_STAT:
#endif
      PRE_MEM_WRITE( "shmctl(IPC_STAT, buf)",
                     arg2, sizeof(struct vki_shmid_ds) );
      break;

#if defined(VKI_IPC_64)
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_SHM_STAT|VKI_IPC_64:
      PRE_MEM_WRITE( "shmctl(IPC_STAT, arg.buf)",
                     arg2, sizeof(struct vki_shmid64_ds) );
      break;
#endif

   case VKI_IPC_SET:
      PRE_MEM_READ( "shmctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_shmid_ds) );
      break;

#if defined(VKI_IPC_64)
   case VKI_IPC_SET|VKI_IPC_64:
      PRE_MEM_READ( "shmctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_shmid64_ds) );
      break;
#endif
   }
}

void
ML_(generic_POST_sys_shmctl) ( ThreadId tid,
                               UWord res,
                               UWord arg0, UWord arg1, UWord arg2 )
{
   switch (arg1 /* cmd */) {
#if defined(VKI_IPC_INFO)
   case VKI_IPC_INFO:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shminfo) );
      break;
   case VKI_IPC_INFO|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shminfo64) );
      break;
#endif

#if defined(VKI_SHM_INFO)
   case VKI_SHM_INFO:
   case VKI_SHM_INFO|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shm_info) );
      break;
#endif

   case VKI_IPC_STAT:
#if defined(VKI_SHM_STAT)
   case VKI_SHM_STAT:
#endif
      POST_MEM_WRITE( arg2, sizeof(struct vki_shmid_ds) );
      break;

#if defined(VKI_IPC_64)
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_SHM_STAT|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shmid64_ds) );
      break;
#endif


   }
}


/* ---------------------------------------------------------------------
   Generic handler for mmap
   ------------------------------------------------------------------ */

/*
 * Although mmap is specified by POSIX and the argument are generally
 * consistent across platforms the precise details of the low level
 * argument passing conventions differ. For example:
 *
 * - On x86-linux there is mmap (aka old_mmap) which takes the
 *   arguments in a memory block and the offset in bytes; and
 *   mmap2 (aka sys_mmap2) which takes the arguments in the normal
 *   way and the offset in pages.
 *
 * - On ppc32-linux there is mmap (aka sys_mmap) which takes the
 *   arguments in the normal way and the offset in bytes; and
 *   mmap2 (aka sys_mmap2) which takes the arguments in the normal
 *   way and the offset in pages.
 *
 * - On amd64-linux everything is simple and there is just the one
 *   call, mmap (aka sys_mmap)  which takes the arguments in the
 *   normal way and the offset in bytes.
 *
 * - On s390x-linux there is mmap (aka old_mmap) which takes the
 *   arguments in a memory block and the offset in bytes. mmap2
 *   is also available (but not exported via unistd.h) with
 *   arguments in a memory block and the offset in pages.
 *
 * To cope with all this we provide a generic handler function here
 * and then each platform implements one or more system call handlers
 * which call this generic routine after extracting and normalising
 * the arguments.
 */

SysRes
ML_(generic_PRE_sys_mmap) ( ThreadId tid,
                            UWord arg1, UWord arg2, UWord arg3,
                            UWord arg4, UWord arg5, Off64T arg6 )
{
   Addr       advised;
   SysRes     sres;
   MapRequest mreq;
   Bool       mreq_ok;

#if defined(VGO_darwin)
   // Nb: we can't use this on Darwin, it has races:
   // * needs to RETRY if advisory succeeds but map fails  
   //   (could have been some other thread in a nonblocking call)
   // * needs to not use fixed-position mmap() on Darwin
   //   (mmap will cheerfully smash whatever's already there, which might 
   //   be a new mapping from some other thread in a nonblocking call)
   VG_(core_panic)("can't use ML_(generic_PRE_sys_mmap) on Darwin");
#endif

   if (arg2 == 0) {
      /* SuSV3 says: If len is zero, mmap() shall fail and no mapping
         shall be established. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   if (!VG_IS_PAGE_ALIGNED(arg1)) {
      /* zap any misaligned addresses. */
      /* SuSV3 says misaligned addresses only cause the MAP_FIXED case
         to fail.   Here, we catch them all. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   if (!VG_IS_PAGE_ALIGNED(arg6)) {
      /* zap any misaligned offsets. */
      /* SuSV3 says: The off argument is constrained to be aligned and
         sized according to the value returned by sysconf() when
         passed _SC_PAGESIZE or _SC_PAGE_SIZE. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Figure out what kind of allocation constraints there are
      (fixed/hint/any), and ask aspacem what we should do. */
   mreq.start = arg1;
   mreq.len   = arg2;
   if (arg4 & VKI_MAP_FIXED) {
      mreq.rkind = MFixed;
   } else
   if (arg1 != 0) {
      mreq.rkind = MHint;
   } else {
      mreq.rkind = MAny;
   }

   /* Enquire ... */
   advised = VG_(am_get_advisory)( &mreq, True/*client*/, &mreq_ok );
   if (!mreq_ok) {
      /* Our request was bounced, so we'd better fail. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Otherwise we're OK (so far).  Install aspacem's choice of
      address, and let the mmap go through.  */
   sres = VG_(am_do_mmap_NO_NOTIFY)(advised, arg2, arg3,
                                    arg4 | VKI_MAP_FIXED,
                                    arg5, arg6);

   /* A refinement: it may be that the kernel refused aspacem's choice
      of address.  If we were originally asked for a hinted mapping,
      there is still a last chance: try again at any address.
      Hence: */
   if (mreq.rkind == MHint && sr_isError(sres)) {
      mreq.start = 0;
      mreq.len   = arg2;
      mreq.rkind = MAny;
      advised = VG_(am_get_advisory)( &mreq, True/*client*/, &mreq_ok );
      if (!mreq_ok) {
         /* Our request was bounced, so we'd better fail. */
         return VG_(mk_SysRes_Error)( VKI_EINVAL );
      }
      /* and try again with the kernel */
      sres = VG_(am_do_mmap_NO_NOTIFY)(advised, arg2, arg3,
                                       arg4 | VKI_MAP_FIXED,
                                       arg5, arg6);
   }

   if (!sr_isError(sres)) {
      ULong di_handle;
      /* Notify aspacem. */
      notify_core_of_mmap(
         (Addr)sr_Res(sres), /* addr kernel actually assigned */
         arg2, /* length */
         arg3, /* prot */
         arg4, /* the original flags value */
         arg5, /* fd */
         arg6  /* offset */
      );
      /* Load symbols? */
      di_handle = VG_(di_notify_mmap)( (Addr)sr_Res(sres), 
                                       False/*allow_SkFileV*/, (Int)arg5 );
      /* Notify the tool. */
      notify_tool_of_mmap(
         (Addr)sr_Res(sres), /* addr kernel actually assigned */
         arg2, /* length */
         arg3, /* prot */
         di_handle /* so the tool can refer to the read debuginfo later,
                      if it wants. */
      );
   }

   /* Stay sane */
   if (!sr_isError(sres) && (arg4 & VKI_MAP_FIXED))
      vg_assert(sr_Res(sres) == arg1);

   return sres;
}


/* ---------------------------------------------------------------------
   The Main Entertainment ... syscall wrappers
   ------------------------------------------------------------------ */

/* Note: the PRE() and POST() wrappers are for the actual functions
   implementing the system calls in the OS kernel.  These mostly have
   names like sys_write();  a few have names like old_mmap().  See the
   comment for ML_(syscall_table)[] for important info about the __NR_foo
   constants and their relationship to the sys_foo() functions.

   Some notes about names used for syscalls and args:
   - For the --trace-syscalls=yes output, we use the sys_foo() name to avoid
     ambiguity.
      
   - For error messages, we generally use a somewhat generic name
     for the syscall (eg. "write" rather than "sys_write").  This should be
     good enough for the average user to understand what is happening,
     without confusing them with names like "sys_write".
     
   - Also, for error messages the arg names are mostly taken from the man
     pages (even though many of those man pages are really for glibc
     functions of the same name), rather than from the OS kernel source,
     for the same reason -- a user presented with a "bogus foo(bar)" arg
     will most likely look at the "foo" man page to see which is the "bar"
     arg.

   Note that we use our own vki_* types.  The one exception is in
   PRE_REG_READn calls, where pointer types haven't been changed, because
   they don't need to be -- eg. for "foo*" to be used, the type foo need not
   be visible.

   XXX: some of these are arch-specific, and should be factored out.
*/

#define PRE(name)      DEFN_PRE_TEMPLATE(generic, name)
#define POST(name)     DEFN_POST_TEMPLATE(generic, name)

// Macros to support 64-bit syscall args split into two 32 bit values
#if defined(VG_LITTLEENDIAN)
#define MERGE64(lo,hi)   ( ((ULong)(lo)) | (((ULong)(hi)) << 32) )
#define MERGE64_FIRST(name) name##_low
#define MERGE64_SECOND(name) name##_high
#elif defined(VG_BIGENDIAN)
#define MERGE64(hi,lo)   ( ((ULong)(lo)) | (((ULong)(hi)) << 32) )
#define MERGE64_FIRST(name) name##_high
#define MERGE64_SECOND(name) name##_low
#else
#error Unknown endianness
#endif

PRE(sys_exit)
{
   ThreadState* tst;
   /* simple; just make this thread exit */
   PRINT("exit( %ld )", ARG1);
   PRE_REG_READ1(void, "exit", int, status);
   tst = VG_(get_ThreadState)(tid);
   /* Set the thread's status to be exiting, then claim that the
      syscall succeeded. */
   tst->exitreason = VgSrc_ExitThread;
   tst->os_state.exitcode = ARG1;
   SET_STATUS_Success(0);
}

PRE(sys_ni_syscall)
{
   PRINT("unimplemented (by the kernel) syscall: %s! (ni_syscall)\n",
      VG_SYSNUM_STRING(SYSNO));
   PRE_REG_READ0(long, "ni_syscall");
   SET_STATUS_Failure( VKI_ENOSYS );
}

PRE(sys_iopl)
{
   PRINT("sys_iopl ( %ld )", ARG1);
   PRE_REG_READ1(long, "iopl", unsigned long, level);
}

PRE(sys_fsync)
{
   *flags |= SfMayBlock;
   PRINT("sys_fsync ( %ld )", ARG1);
   PRE_REG_READ1(long, "fsync", unsigned int, fd);
}

PRE(sys_fdatasync)
{
   *flags |= SfMayBlock;
   PRINT("sys_fdatasync ( %ld )", ARG1);
   PRE_REG_READ1(long, "fdatasync", unsigned int, fd);
}

PRE(sys_msync)
{
   *flags |= SfMayBlock;
   PRINT("sys_msync ( %#lx, %llu, %ld )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "msync",
                 unsigned long, start, vki_size_t, length, int, flags);
   PRE_MEM_READ( "msync(start)", ARG1, ARG2 );
}

// Nb: getpmsg() and putpmsg() are special additional syscalls used in early
// versions of LiS (Linux Streams).  They are not part of the kernel.
// Therefore, we have to provide this type ourself, rather than getting it
// from the kernel sources.
struct vki_pmsg_strbuf {
   int     maxlen;         /* no. of bytes in buffer */
   int     len;            /* no. of bytes returned */
   vki_caddr_t buf;        /* pointer to data */
};
PRE(sys_getpmsg)
{
   /* LiS getpmsg from http://www.gcom.com/home/linux/lis/ */
   struct vki_pmsg_strbuf *ctrl;
   struct vki_pmsg_strbuf *data;
   *flags |= SfMayBlock;
   PRINT("sys_getpmsg ( %ld, %#lx, %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "getpmsg",
                 int, fd, struct strbuf *, ctrl, struct strbuf *, data, 
                 int *, bandp, int *, flagsp);
   ctrl = (struct vki_pmsg_strbuf *)ARG2;
   data = (struct vki_pmsg_strbuf *)ARG3;
   if (ctrl && ctrl->maxlen > 0)
      PRE_MEM_WRITE( "getpmsg(ctrl)", (Addr)ctrl->buf, ctrl->maxlen);
   if (data && data->maxlen > 0)
      PRE_MEM_WRITE( "getpmsg(data)", (Addr)data->buf, data->maxlen);
   if (ARG4)
      PRE_MEM_WRITE( "getpmsg(bandp)", (Addr)ARG4, sizeof(int));
   if (ARG5)
      PRE_MEM_WRITE( "getpmsg(flagsp)", (Addr)ARG5, sizeof(int));
}
POST(sys_getpmsg)
{
   struct vki_pmsg_strbuf *ctrl;
   struct vki_pmsg_strbuf *data;
   vg_assert(SUCCESS);
   ctrl = (struct vki_pmsg_strbuf *)ARG2;
   data = (struct vki_pmsg_strbuf *)ARG3;
   if (RES == 0 && ctrl && ctrl->len > 0) {
      POST_MEM_WRITE( (Addr)ctrl->buf, ctrl->len);
   }
   if (RES == 0 && data && data->len > 0) {
      POST_MEM_WRITE( (Addr)data->buf, data->len);
   }
}

PRE(sys_putpmsg)
{
   /* LiS putpmsg from http://www.gcom.com/home/linux/lis/ */
   struct vki_pmsg_strbuf *ctrl;
   struct vki_pmsg_strbuf *data;
   *flags |= SfMayBlock;
   PRINT("sys_putpmsg ( %ld, %#lx, %#lx, %ld, %ld )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "putpmsg",
                 int, fd, struct strbuf *, ctrl, struct strbuf *, data, 
                 int, band, int, flags);
   ctrl = (struct vki_pmsg_strbuf *)ARG2;
   data = (struct vki_pmsg_strbuf *)ARG3;
   if (ctrl && ctrl->len > 0)
      PRE_MEM_READ( "putpmsg(ctrl)", (Addr)ctrl->buf, ctrl->len);
   if (data && data->len > 0)
      PRE_MEM_READ( "putpmsg(data)", (Addr)data->buf, data->len);
}

PRE(sys_getitimer)
{
   struct vki_itimerval *value = (struct vki_itimerval*)ARG2;
   PRINT("sys_getitimer ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "getitimer", int, which, struct itimerval *, value);

   PRE_timeval_WRITE( "getitimer(&value->it_interval)", &(value->it_interval));
   PRE_timeval_WRITE( "getitimer(&value->it_value)",    &(value->it_value));
}

POST(sys_getitimer)
{
   if (ARG2 != (Addr)NULL) {
      struct vki_itimerval *value = (struct vki_itimerval*)ARG2;
      POST_timeval_WRITE( &(value->it_interval) );
      POST_timeval_WRITE( &(value->it_value) );
   }
}

PRE(sys_setitimer)
{
   PRINT("sys_setitimer ( %ld, %#lx, %#lx )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "setitimer", 
                 int, which,
                 struct itimerval *, value, struct itimerval *, ovalue);
   if (ARG2 != (Addr)NULL) {
      struct vki_itimerval *value = (struct vki_itimerval*)ARG2;
      PRE_timeval_READ( "setitimer(&value->it_interval)",
                         &(value->it_interval));
      PRE_timeval_READ( "setitimer(&value->it_value)",
                         &(value->it_value));
   }
   if (ARG3 != (Addr)NULL) {
      struct vki_itimerval *ovalue = (struct vki_itimerval*)ARG3;
      PRE_timeval_WRITE( "setitimer(&ovalue->it_interval)",
                         &(ovalue->it_interval));
      PRE_timeval_WRITE( "setitimer(&ovalue->it_value)",
                         &(ovalue->it_value));
   }
}

POST(sys_setitimer)
{
   if (ARG3 != (Addr)NULL) {
      struct vki_itimerval *ovalue = (struct vki_itimerval*)ARG3;
      POST_timeval_WRITE( &(ovalue->it_interval) );
      POST_timeval_WRITE( &(ovalue->it_value) );
   }
}

PRE(sys_chroot)
{
   PRINT("sys_chroot ( %#lx )", ARG1);
   PRE_REG_READ1(long, "chroot", const char *, path);
   PRE_MEM_RASCIIZ( "chroot(path)", ARG1 );
}

PRE(sys_madvise)
{
   *flags |= SfMayBlock;
   PRINT("sys_madvise ( %#lx, %llu, %ld )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "madvise",
                 unsigned long, start, vki_size_t, length, int, advice);
}

#if HAVE_MREMAP
PRE(sys_mremap)
{
   // Nb: this is different to the glibc version described in the man pages,
   // which lacks the fifth 'new_address' argument.
   if (ARG4 & VKI_MREMAP_FIXED) {
      PRINT("sys_mremap ( %#lx, %llu, %ld, 0x%lx, %#lx )",
            ARG1, (ULong)ARG2, ARG3, ARG4, ARG5);
      PRE_REG_READ5(unsigned long, "mremap",
                    unsigned long, old_addr, unsigned long, old_size,
                    unsigned long, new_size, unsigned long, flags,
                    unsigned long, new_addr);
   } else {
      PRINT("sys_mremap ( %#lx, %llu, %ld, 0x%lx )",
            ARG1, (ULong)ARG2, ARG3, ARG4);
      PRE_REG_READ4(unsigned long, "mremap",
                    unsigned long, old_addr, unsigned long, old_size,
                    unsigned long, new_size, unsigned long, flags);
   }
   SET_STATUS_from_SysRes( 
      do_mremap((Addr)ARG1, ARG2, (Addr)ARG5, ARG3, ARG4, tid) 
   );
}
#endif /* HAVE_MREMAP */

PRE(sys_nice)
{
   PRINT("sys_nice ( %ld )", ARG1);
   PRE_REG_READ1(long, "nice", int, inc);
}

PRE(sys_mlock)
{
   *flags |= SfMayBlock;
   PRINT("sys_mlock ( %#lx, %llu )", ARG1, (ULong)ARG2);
   PRE_REG_READ2(long, "mlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_munlock)
{
   *flags |= SfMayBlock;
   PRINT("sys_munlock ( %#lx, %llu )", ARG1, (ULong)ARG2);
   PRE_REG_READ2(long, "munlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_mlockall)
{
   *flags |= SfMayBlock;
   PRINT("sys_mlockall ( %lx )", ARG1);
   PRE_REG_READ1(long, "mlockall", int, flags);
}

PRE(sys_setpriority)
{
   PRINT("sys_setpriority ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setpriority", int, which, int, who, int, prio);
}

PRE(sys_getpriority)
{
   PRINT("sys_getpriority ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "getpriority", int, which, int, who);
}

PRE(sys_pwrite64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_pwrite64 ( %ld, %#lx, %llu, %lld )",
         ARG1, ARG2, (ULong)ARG3, MERGE64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pwrite64",
                 unsigned int, fd, const char *, buf, vki_size_t, count,
                 vki_u32, MERGE64_FIRST(offset), vki_u32, MERGE64_SECOND(offset));
#elif VG_WORDSIZE == 8
   PRINT("sys_pwrite64 ( %ld, %#lx, %llu, %lld )",
         ARG1, ARG2, (ULong)ARG3, (Long)ARG4);
   PRE_REG_READ4(ssize_t, "pwrite64",
                 unsigned int, fd, const char *, buf, vki_size_t, count,
                 Word, offset);
#else
#  error Unexpected word size
#endif
   PRE_MEM_READ( "pwrite64(buf)", ARG2, ARG3 );
}

PRE(sys_sync)
{
   *flags |= SfMayBlock;
   PRINT("sys_sync ( )");
   PRE_REG_READ0(long, "sync");
}

PRE(sys_fstatfs)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fstatfs ( %ld, %#lx )",ARG1,ARG2);
   PRE_REG_READ2(long, "fstatfs",
                 unsigned int, fd, struct statfs *, buf);
   PRE_MEM_WRITE( "fstatfs(buf)", ARG2, sizeof(struct vki_statfs) );
}

POST(sys_fstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_statfs) );
}

PRE(sys_fstatfs64)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fstatfs64 ( %ld, %llu, %#lx )",ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "fstatfs64",
                 unsigned int, fd, vki_size_t, size, struct statfs64 *, buf);
   PRE_MEM_WRITE( "fstatfs64(buf)", ARG3, ARG2 );
}
POST(sys_fstatfs64)
{
   POST_MEM_WRITE( ARG3, ARG2 );
}

PRE(sys_getsid)
{
   PRINT("sys_getsid ( %ld )", ARG1);
   PRE_REG_READ1(long, "getsid", vki_pid_t, pid);
}

PRE(sys_pread64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_pread64 ( %ld, %#lx, %llu, %lld )",
         ARG1, ARG2, (ULong)ARG3, MERGE64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pread64",
                 unsigned int, fd, char *, buf, vki_size_t, count,
                 vki_u32, MERGE64_FIRST(offset), vki_u32, MERGE64_SECOND(offset));
#elif VG_WORDSIZE == 8
   PRINT("sys_pread64 ( %ld, %#lx, %llu, %lld )",
         ARG1, ARG2, (ULong)ARG3, (Long)ARG4);
   PRE_REG_READ4(ssize_t, "pread64",
                 unsigned int, fd, char *, buf, vki_size_t, count,
                 Word, offset);
#else
#  error Unexpected word size
#endif
   PRE_MEM_WRITE( "pread64(buf)", ARG2, ARG3 );
}
POST(sys_pread64)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
   }
}

PRE(sys_mknod)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_mknod ( %#lx(%s), 0x%lx, 0x%lx )", ARG1, (char*)ARG1, ARG2, ARG3 );
   PRE_REG_READ3(long, "mknod",
                 const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknod(pathname)", ARG1 );
}

PRE(sys_flock)
{
   *flags |= SfMayBlock;
   PRINT("sys_flock ( %ld, %ld )", ARG1, ARG2 );
   PRE_REG_READ2(long, "flock", unsigned int, fd, unsigned int, operation);
}

// Pre_read a char** argument.
static void pre_argv_envp(Addr a, ThreadId tid, Char* s1, Char* s2)
{
   while (True) {
      Addr a_deref;
      Addr* a_p = (Addr*)a;
      PRE_MEM_READ( s1, (Addr)a_p, sizeof(Addr) );
      a_deref = *a_p;
      if (0 == a_deref)
         break;
      PRE_MEM_RASCIIZ( s2, a_deref );
      a += sizeof(char*);
   }
}

static Bool i_am_the_only_thread ( void )
{
   Int c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */
   return c == 1;
}

/* Wait until all other threads disappear. */
void VG_(reap_threads)(ThreadId self)
{
   while (!i_am_the_only_thread()) {
      /* Let other thread(s) run */
      VG_(vg_yield)();
      VG_(poll_signals)(self);
   }
   vg_assert(i_am_the_only_thread());
}

// XXX: prototype here seemingly doesn't match the prototype for i386-linux,
// but it seems to work nonetheless...
PRE(sys_execve)
{
   Char*        path = NULL;       /* path to executable */
   Char**       envp = NULL;
   Char**       argv = NULL;
   Char**       arg2copy;
   Char*        launcher_basename = NULL;
   ThreadState* tst;
   Int          i, j, tot_args;
   SysRes       res;
   Bool         setuid_allowed, trace_this_child;

   PRINT("sys_execve ( %#lx(%s), %#lx, %#lx )", ARG1, (char*)ARG1, ARG2, ARG3);
   PRE_REG_READ3(vki_off_t, "execve",
                 char *, filename, char **, argv, char **, envp);
   PRE_MEM_RASCIIZ( "execve(filename)", ARG1 );
   if (ARG2 != 0)
      pre_argv_envp( ARG2, tid, "execve(argv)", "execve(argv[i])" );
   if (ARG3 != 0)
      pre_argv_envp( ARG3, tid, "execve(envp)", "execve(envp[i])" );

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Erk.  If the exec fails, then the following will have made a
      mess of things which makes it hard for us to continue.  The
      right thing to do is piece everything together again in
      POST(execve), but that's close to impossible.  Instead, we make
      an effort to check that the execve will work before actually
      doing it. */

   /* Check that the name at least begins in client-accessible storage. */
   if (ARG1 == 0 /* obviously bogus */
       || !VG_(am_is_valid_for_client)( ARG1, 1, VKI_PROT_READ )) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
   }

   // debug-only printing
   if (0) {
      VG_(printf)("ARG1 = %p(%s)\n", (void*)ARG1, (HChar*)ARG1);
      if (ARG2) {
         VG_(printf)("ARG2 = ");
         Int q;
         HChar** vec = (HChar**)ARG2;
         for (q = 0; vec[q]; q++)
            VG_(printf)("%p(%s) ", vec[q], vec[q]);
         VG_(printf)("\n");
      } else {
         VG_(printf)("ARG2 = null\n");
      }
   }

   // Decide whether or not we want to follow along
   { // Make 'child_argv' be a pointer to the child's arg vector
     // (skipping the exe name)
     HChar** child_argv = (HChar**)ARG2;
     if (child_argv && child_argv[0] == NULL)
        child_argv = NULL;
     trace_this_child = VG_(should_we_trace_this_child)( (HChar*)ARG1, child_argv );
   }

   // Do the important checks:  it is a file, is executable, permissions are
   // ok, etc.  We allow setuid executables to run only in the case when
   // we are not simulating them, that is, they to be run natively.
   setuid_allowed = trace_this_child  ? False  : True;
   res = VG_(pre_exec_check)((const Char*)ARG1, NULL, setuid_allowed);
   if (sr_isError(res)) {
      SET_STATUS_Failure( sr_Err(res) );
      return;
   }

   /* If we're tracing the child, and the launcher name looks bogus
      (possibly because launcher.c couldn't figure it out, see
      comments therein) then we have no option but to fail. */
   if (trace_this_child 
       && (VG_(name_of_launcher) == NULL
           || VG_(name_of_launcher)[0] != '/')) {
      SET_STATUS_Failure( VKI_ECHILD ); /* "No child processes" */
      return;
   }

   /* After this point, we can't recover if the execve fails. */
   VG_(debugLog)(1, "syswrap", "Exec of %s\n", (Char*)ARG1);

   
   // Terminate gdbserver if it is active.
   if (VG_(clo_vgdb)  != Vg_VgdbNo) {
      // If the child will not be traced, we need to terminate gdbserver
      // to cleanup the gdbserver resources (e.g. the FIFO files).
      // If child will be traced, we also terminate gdbserver: the new 
      // Valgrind will start a fresh gdbserver after exec.
      VG_(gdbserver) (0);
   }

   /* Resistance is futile.  Nuke all other threads.  POSIX mandates
      this. (Really, nuke them all, since the new process will make
      its own new thread.) */
   VG_(nuke_all_threads_except)( tid, VgSrc_ExitThread );
   VG_(reap_threads)(tid);

   // Set up the child's exe path.
   //
   if (trace_this_child) {

      // We want to exec the launcher.  Get its pre-remembered path.
      path = VG_(name_of_launcher);
      // VG_(name_of_launcher) should have been acquired by m_main at
      // startup.
      vg_assert(path);

      launcher_basename = VG_(strrchr)(path, '/');
      if (launcher_basename == NULL || launcher_basename[1] == 0) {
         launcher_basename = path;  // hmm, tres dubious
      } else {
         launcher_basename++;
      }

   } else {
      path = (Char*)ARG1;
   }

   // Set up the child's environment.
   //
   // Remove the valgrind-specific stuff from the environment so the
   // child doesn't get vgpreload_core.so, vgpreload_<tool>.so, etc.  
   // This is done unconditionally, since if we are tracing the child,
   // the child valgrind will set up the appropriate client environment.
   // Nb: we make a copy of the environment before trying to mangle it
   // as it might be in read-only memory (this was bug #101881).
   //
   // Then, if tracing the child, set VALGRIND_LIB for it.
   //
   if (ARG3 == 0) {
      envp = NULL;
   } else {
      envp = VG_(env_clone)( (Char**)ARG3 );
      if (envp == NULL) goto hosed;
      VG_(env_remove_valgrind_env_stuff)( envp );
   }

   if (trace_this_child) {
      // Set VALGRIND_LIB in ARG3 (the environment)
      VG_(env_setenv)( &envp, VALGRIND_LIB, VG_(libdir));
   }

   // Set up the child's args.  If not tracing it, they are
   // simply ARG2.  Otherwise, they are
   //
   // [launcher_basename] ++ VG_(args_for_valgrind) ++ [ARG1] ++ ARG2[1..]
   //
   // except that the first VG_(args_for_valgrind_noexecpass) args
   // are omitted.
   //
   if (!trace_this_child) {
      argv = (Char**)ARG2;
   } else {
      vg_assert( VG_(args_for_valgrind) );
      vg_assert( VG_(args_for_valgrind_noexecpass) >= 0 );
      vg_assert( VG_(args_for_valgrind_noexecpass) 
                   <= VG_(sizeXA)( VG_(args_for_valgrind) ) );
      /* how many args in total will there be? */
      // launcher basename
      tot_args = 1;
      // V's args
      tot_args += VG_(sizeXA)( VG_(args_for_valgrind) );
      tot_args -= VG_(args_for_valgrind_noexecpass);
      // name of client exe
      tot_args++;
      // args for client exe, skipping [0]
      arg2copy = (Char**)ARG2;
      if (arg2copy && arg2copy[0]) {
         for (i = 1; arg2copy[i]; i++)
            tot_args++;
      }
      // allocate
      argv = VG_(malloc)( "di.syswrap.pre_sys_execve.1",
                          (tot_args+1) * sizeof(HChar*) );
      if (argv == 0) goto hosed;
      // copy
      j = 0;
      argv[j++] = launcher_basename;
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         if (i < VG_(args_for_valgrind_noexecpass))
            continue;
         argv[j++] = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      }
      argv[j++] = (Char*)ARG1;
      if (arg2copy && arg2copy[0])
         for (i = 1; arg2copy[i]; i++)
            argv[j++] = arg2copy[i];
      argv[j++] = NULL;
      // check
      vg_assert(j == tot_args+1);
   }

   /* restore the DATA rlimit for the child */
   VG_(setrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));

   /*
      Set the signal state up for exec.

      We need to set the real signal state to make sure the exec'd
      process gets SIG_IGN properly.

      Also set our real sigmask to match the client's sigmask so that
      the exec'd child will get the right mask.  First we need to
      clear out any pending signals so they they don't get delivered,
      which would confuse things.

      XXX This is a bug - the signals should remain pending, and be
      delivered to the new process after exec.  There's also a
      race-condition, since if someone delivers us a signal between
      the sigprocmask and the execve, we'll still get the signal. Oh
      well.
   */
   {
      vki_sigset_t allsigs;
      vki_siginfo_t info;

      /* What this loop does: it queries SCSS (the signal state that
         the client _thinks_ the kernel is in) by calling
         VG_(do_sys_sigaction), and modifies the real kernel signal
         state accordingly. */
      for (i = 1; i < VG_(max_signal); i++) {
         vki_sigaction_fromK_t sa_f;
         vki_sigaction_toK_t   sa_t;
         VG_(do_sys_sigaction)(i, NULL, &sa_f);
         VG_(convert_sigaction_fromK_to_toK)(&sa_f, &sa_t);
         if (sa_t.ksa_handler == VKI_SIG_IGN)
            VG_(sigaction)(i, &sa_t, NULL);
         else {
            sa_t.ksa_handler = VKI_SIG_DFL;
            VG_(sigaction)(i, &sa_t, NULL);
         }
      }

      VG_(sigfillset)(&allsigs);
      while(VG_(sigtimedwait_zero)(&allsigs, &info) > 0)
         ;

      VG_(sigprocmask)(VKI_SIG_SETMASK, &tst->sig_mask, NULL);
   }

   if (0) {
      Char **cpp;
      VG_(printf)("exec: %s\n", path);
      for (cpp = argv; cpp && *cpp; cpp++)
         VG_(printf)("argv: %s\n", *cpp);
      if (0)
         for (cpp = envp; cpp && *cpp; cpp++)
            VG_(printf)("env: %s\n", *cpp);
   }

   SET_STATUS_from_SysRes( 
      VG_(do_syscall3)(__NR_execve, (UWord)path, (UWord)argv, (UWord)envp) 
   );

   /* If we got here, then the execve failed.  We've already made way
      too much of a mess to continue, so we have to abort. */
  hosed:
   vg_assert(FAILURE);
   VG_(message)(Vg_UserMsg, "execve(%#lx(%s), %#lx, %#lx) failed, errno %ld\n",
                ARG1, (char*)ARG1, ARG2, ARG3, ERR);
   VG_(message)(Vg_UserMsg, "EXEC FAILED: I can't recover from "
                            "execve() failing, so I'm dying.\n");
   VG_(message)(Vg_UserMsg, "Add more stringent tests in PRE(sys_execve), "
                            "or work out how to recover.\n");
   VG_(exit)(101);
}

PRE(sys_access)
{
   PRINT("sys_access ( %#lx(%s), %ld )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "access", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "access(pathname)", ARG1 );
}

PRE(sys_alarm)
{
   PRINT("sys_alarm ( %ld )", ARG1);
   PRE_REG_READ1(unsigned long, "alarm", unsigned int, seconds);
}

PRE(sys_brk)
{
   Addr brk_limit = VG_(brk_limit);
   Addr brk_new; 

   /* libc   says: int   brk(void *end_data_segment);
      kernel says: void* brk(void* end_data_segment);  (more or less)

      libc returns 0 on success, and -1 (and sets errno) on failure.
      Nb: if you ask to shrink the dataseg end below what it
      currently is, that always succeeds, even if the dataseg end
      doesn't actually change (eg. brk(0)).  Unless it seg faults.

      Kernel returns the new dataseg end.  If the brk() failed, this
      will be unchanged from the old one.  That's why calling (kernel)
      brk(0) gives the current dataseg end (libc brk() just returns
      zero in that case).

      Both will seg fault if you shrink it back into a text segment.
   */
   PRINT("sys_brk ( %#lx )", ARG1);
   PRE_REG_READ1(unsigned long, "brk", unsigned long, end_data_segment);

   brk_new = do_brk(ARG1);
   SET_STATUS_Success( brk_new );

   if (brk_new == ARG1) {
      /* brk() succeeded */
      if (brk_new < brk_limit) {
         /* successfully shrunk the data segment. */
         VG_TRACK( die_mem_brk, (Addr)ARG1,
		   brk_limit-ARG1 );
      } else
      if (brk_new > brk_limit) {
         /* successfully grew the data segment */
         VG_TRACK( new_mem_brk, brk_limit,
                   ARG1-brk_limit, tid );
      }
   } else {
      /* brk() failed */
      vg_assert(brk_limit == brk_new);
   }
}

PRE(sys_chdir)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_chdir ( %#lx(%s) )", ARG1,(char*)ARG1);
   PRE_REG_READ1(long, "chdir", const char *, path);
   PRE_MEM_RASCIIZ( "chdir(path)", ARG1 );
}

PRE(sys_chmod)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_chmod ( %#lx(%s), %ld )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "chmod", const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "chmod(path)", ARG1 );
}

PRE(sys_chown)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_chown ( %#lx(%s), 0x%lx, 0x%lx )", ARG1,(char*)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "chown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "chown(path)", ARG1 );
}

PRE(sys_lchown)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_lchown ( %#lx(%s), 0x%lx, 0x%lx )", ARG1,(char*)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "lchown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "lchown(path)", ARG1 );
}

PRE(sys_close)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_close ( %ld )", ARG1);
   PRE_REG_READ1(long, "close", unsigned int, fd);

   /* Detect and negate attempts by the client to close Valgrind's log fd */
   if ( (!ML_(fd_allowed)(ARG1, "close", tid, False))
        /* If doing -d style logging (which is to fd=2), don't
           allow that to be closed either. */
        || (ARG1 == 2/*stderr*/ && VG_(debugLog_getLevel)() > 0) )
      SET_STATUS_Failure( VKI_EBADF );
}

POST(sys_close)
{
   if (VG_(clo_track_fds)) record_fd_close(ARG1);
}

PRE(sys_dup)
{
   PRINT("sys_dup ( %ld )", ARG1);
   PRE_REG_READ1(long, "dup", unsigned int, oldfd);
}

POST(sys_dup)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "dup", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_named)(tid, RES);
   }
}

PRE(sys_dup2)
{
   PRINT("sys_dup2 ( %ld, %ld )", ARG1,ARG2);
   PRE_REG_READ2(long, "dup2", unsigned int, oldfd, unsigned int, newfd);
   if (!ML_(fd_allowed)(ARG2, "dup2", tid, True))
      SET_STATUS_Failure( VKI_EBADF );
}

POST(sys_dup2)
{
   vg_assert(SUCCESS);
   if (VG_(clo_track_fds))
      ML_(record_fd_open_named)(tid, RES);
}

PRE(sys_fchdir)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fchdir ( %ld )", ARG1);
   PRE_REG_READ1(long, "fchdir", unsigned int, fd);
}

PRE(sys_fchown)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fchown ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "fchown",
                 unsigned int, fd, vki_uid_t, owner, vki_gid_t, group);
}

PRE(sys_fchmod)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fchmod ( %ld, %ld )", ARG1,ARG2);
   PRE_REG_READ2(long, "fchmod", unsigned int, fildes, vki_mode_t, mode);
}

PRE(sys_newfstat)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_newfstat ( %ld, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "fstat", unsigned int, fd, struct stat *, buf);
   PRE_MEM_WRITE( "fstat(buf)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_newfstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}

static vki_sigset_t fork_saved_mask;

// In Linux, the sys_fork() function varies across architectures, but we
// ignore the various args it gets, and so it looks arch-neutral.  Hmm.
PRE(sys_fork)
{
   Bool is_child;
   Int child_pid;
   vki_sigset_t mask;

   PRINT("sys_fork ( )");
   PRE_REG_READ0(long, "fork");

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   SET_STATUS_from_SysRes( VG_(do_syscall0)(__NR_fork) );

   if (!SUCCESS) return;

#if defined(VGO_linux)
   // RES is 0 for child, non-0 (the child's PID) for parent.
   is_child = ( RES == 0 ? True : False );
   child_pid = ( is_child ? -1 : RES );
#elif defined(VGO_darwin)
   // RES is the child's pid.  RESHI is 1 for child, 0 for parent.
   is_child = RESHI;
   child_pid = RES;
#else
#  error Unknown OS
#endif

   VG_(do_atfork_pre)(tid);

   if (is_child) {
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);

      /* If --child-silent-after-fork=yes was specified, set the
         output file descriptors to 'impossible' values.  This is
         noticed by send_bytes_to_logging_sink in m_libcprint.c, which
         duly stops writing any further output. */
      if (VG_(clo_child_silent_after_fork)) {
         if (!VG_(log_output_sink).is_socket)
            VG_(log_output_sink).fd = -1;
         if (!VG_(xml_output_sink).is_socket)
            VG_(xml_output_sink).fd = -1;
      }

   } else {
      VG_(do_atfork_parent)(tid);

      PRINT("   fork: process %d created child %d\n", VG_(getpid)(), child_pid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }
}

PRE(sys_ftruncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_ftruncate ( %ld, %ld )", ARG1,ARG2);
   PRE_REG_READ2(long, "ftruncate", unsigned int, fd, unsigned long, length);
}

PRE(sys_truncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_truncate ( %#lx(%s), %ld )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "truncate", 
                 const char *, path, unsigned long, length);
   PRE_MEM_RASCIIZ( "truncate(path)", ARG1 );
}

PRE(sys_ftruncate64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_ftruncate64 ( %ld, %lld )", ARG1, MERGE64(ARG2,ARG3));
   PRE_REG_READ3(long, "ftruncate64",
                 unsigned int, fd,
                 UWord, MERGE64_FIRST(length), UWord, MERGE64_SECOND(length));
#else
   PRINT("sys_ftruncate64 ( %ld, %lld )", ARG1, (Long)ARG2);
   PRE_REG_READ2(long, "ftruncate64",
                 unsigned int,fd, UWord,length);
#endif
}

PRE(sys_truncate64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_truncate64 ( %#lx, %lld )", ARG1, (Long)MERGE64(ARG2, ARG3));
   PRE_REG_READ3(long, "truncate64",
                 const char *, path,
                 UWord, MERGE64_FIRST(length), UWord, MERGE64_SECOND(length));
#else
   PRINT("sys_truncate64 ( %#lx, %lld )", ARG1, (Long)ARG2);
   PRE_REG_READ2(long, "truncate64",
                 const char *,path, UWord,length);
#endif
   PRE_MEM_RASCIIZ( "truncate64(path)", ARG1 );
}

PRE(sys_getdents)
{
   *flags |= SfMayBlock;
   PRINT("sys_getdents ( %ld, %#lx, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getdents",
                 unsigned int, fd, struct linux_dirent *, dirp,
                 unsigned int, count);
   PRE_MEM_WRITE( "getdents(dirp)", ARG2, ARG3 );
}

POST(sys_getdents)
{
   vg_assert(SUCCESS);
   if (RES > 0)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_getdents64)
{
   *flags |= SfMayBlock;
   PRINT("sys_getdents64 ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getdents64",
                 unsigned int, fd, struct linux_dirent64 *, dirp,
                 unsigned int, count);
   PRE_MEM_WRITE( "getdents64(dirp)", ARG2, ARG3 );
}

POST(sys_getdents64)
{
   vg_assert(SUCCESS);
   if (RES > 0)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_getgroups)
{
   PRINT("sys_getgroups ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "getgroups", int, size, vki_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_WRITE( "getgroups(list)", ARG2, ARG1 * sizeof(vki_gid_t) );
}

POST(sys_getgroups)
{
   vg_assert(SUCCESS);
   if (ARG1 > 0 && RES > 0)
      POST_MEM_WRITE( ARG2, RES * sizeof(vki_gid_t) );
}

PRE(sys_getcwd)
{
   // Comment from linux/fs/dcache.c:
   //   NOTE! The user-level library version returns a character pointer.
   //   The kernel system call just returns the length of the buffer filled
   //   (which includes the ending '\0' character), or a negative error
   //   value.
   // Is this Linux-specific?  If so it should be moved to syswrap-linux.c.
   PRINT("sys_getcwd ( %#lx, %llu )", ARG1,(ULong)ARG2);
   PRE_REG_READ2(long, "getcwd", char *, buf, unsigned long, size);
   PRE_MEM_WRITE( "getcwd(buf)", ARG1, ARG2 );
}

POST(sys_getcwd)
{
   vg_assert(SUCCESS);
   if (RES != (Addr)NULL)
      POST_MEM_WRITE( ARG1, RES );
}

PRE(sys_geteuid)
{
   PRINT("sys_geteuid ( )");
   PRE_REG_READ0(long, "geteuid");
}

PRE(sys_getegid)
{
   PRINT("sys_getegid ( )");
   PRE_REG_READ0(long, "getegid");
}

PRE(sys_getgid)
{
   PRINT("sys_getgid ( )");
   PRE_REG_READ0(long, "getgid");
}

PRE(sys_getpid)
{
   PRINT("sys_getpid ()");
   PRE_REG_READ0(long, "getpid");
}

PRE(sys_getpgid)
{
   PRINT("sys_getpgid ( %ld )", ARG1);
   PRE_REG_READ1(long, "getpgid", vki_pid_t, pid);
}

PRE(sys_getpgrp)
{
   PRINT("sys_getpgrp ()");
   PRE_REG_READ0(long, "getpgrp");
}

PRE(sys_getppid)
{
   PRINT("sys_getppid ()");
   PRE_REG_READ0(long, "getppid");
}

static void common_post_getrlimit(ThreadId tid, UWord a1, UWord a2)
{
   POST_MEM_WRITE( a2, sizeof(struct vki_rlimit) );

#ifdef _RLIMIT_POSIX_FLAG
   // Darwin will sometimes set _RLIMIT_POSIX_FLAG on getrlimit calls.
   // Unset it here to make the switch case below work correctly.
   a1 &= ~_RLIMIT_POSIX_FLAG;
#endif

   switch (a1) {
   case VKI_RLIMIT_NOFILE:
      ((struct vki_rlimit *)a2)->rlim_cur = VG_(fd_soft_limit);
      ((struct vki_rlimit *)a2)->rlim_max = VG_(fd_hard_limit);
      break;

   case VKI_RLIMIT_DATA:
      *((struct vki_rlimit *)a2) = VG_(client_rlimit_data);
      break;

   case VKI_RLIMIT_STACK:
      *((struct vki_rlimit *)a2) = VG_(client_rlimit_stack);
      break;
   }
}

PRE(sys_old_getrlimit)
{
   PRINT("sys_old_getrlimit ( %ld, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "old_getrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_WRITE( "old_getrlimit(rlim)", ARG2, sizeof(struct vki_rlimit) );
}

POST(sys_old_getrlimit)
{
   common_post_getrlimit(tid, ARG1, ARG2);
}

PRE(sys_getrlimit)
{
   PRINT("sys_getrlimit ( %ld, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "getrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_WRITE( "getrlimit(rlim)", ARG2, sizeof(struct vki_rlimit) );
}

POST(sys_getrlimit)
{
   common_post_getrlimit(tid, ARG1, ARG2);
}

PRE(sys_getrusage)
{
   PRINT("sys_getrusage ( %ld, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "getrusage", int, who, struct rusage *, usage);
   PRE_MEM_WRITE( "getrusage(usage)", ARG2, sizeof(struct vki_rusage) );
}

POST(sys_getrusage)
{
   vg_assert(SUCCESS);
   if (RES == 0)
      POST_MEM_WRITE( ARG2, sizeof(struct vki_rusage) );
}

PRE(sys_gettimeofday)
{
   PRINT("sys_gettimeofday ( %#lx, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "gettimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   // GrP fixme does darwin write to *tz anymore?
   if (ARG1 != 0)
      PRE_timeval_WRITE( "gettimeofday(tv)", ARG1 );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "gettimeofday(tz)", ARG2, sizeof(struct vki_timezone) );
}

POST(sys_gettimeofday)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      if (ARG1 != 0)
         POST_timeval_WRITE( ARG1 );
      if (ARG2 != 0)
	 POST_MEM_WRITE( ARG2, sizeof(struct vki_timezone) );
   }
}

PRE(sys_settimeofday)
{
   PRINT("sys_settimeofday ( %#lx, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "settimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   if (ARG1 != 0)
      PRE_timeval_READ( "settimeofday(tv)", ARG1 );
   if (ARG2 != 0) {
      PRE_MEM_READ( "settimeofday(tz)", ARG2, sizeof(struct vki_timezone) );
      /* maybe should warn if tz->tz_dsttime is non-zero? */
   }
}

PRE(sys_getuid)
{
   PRINT("sys_getuid ( )");
   PRE_REG_READ0(long, "getuid");
}

void ML_(PRE_unknown_ioctl)(ThreadId tid, UWord request, UWord arg)
{         
   /* We don't have any specific information on it, so
      try to do something reasonable based on direction and
      size bits.  The encoding scheme is described in
      /usr/include/asm/ioctl.h or /usr/include/sys/ioccom.h .
      
      According to Simon Hausmann, _IOC_READ means the kernel
      writes a value to the ioctl value passed from the user
      space and the other way around with _IOC_WRITE. */
   
   UInt dir  = _VKI_IOC_DIR(request);
   UInt size = _VKI_IOC_SIZE(request);
   if (VG_(strstr)(VG_(clo_sim_hints), "lax-ioctls") != NULL) {
      /* 
       * Be very lax about ioctl handling; the only
       * assumption is that the size is correct. Doesn't
       * require the full buffer to be initialized when
       * writing.  Without this, using some device
       * drivers with a large number of strange ioctl
       * commands becomes very tiresome.
       */
   } else if (/* size == 0 || */ dir == _VKI_IOC_NONE) {
      //VG_(message)(Vg_UserMsg, "UNKNOWN ioctl %#lx\n", request);
      //VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      static Int moans = 3;
      if (moans > 0 && !VG_(clo_xml)) {
         moans--;
         VG_(umsg)("Warning: noted but unhandled ioctl 0x%lx"
                   " with no size/direction hints\n", request); 
         VG_(umsg)("   This could cause spurious value errors to appear.\n");
         VG_(umsg)("   See README_MISSING_SYSCALL_OR_IOCTL for "
                   "guidance on writing a proper wrapper.\n" );
      }
   } else {
      //VG_(message)(Vg_UserMsg, "UNKNOWN ioctl %#lx\n", request);
      //VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      if ((dir & _VKI_IOC_WRITE) && size > 0)
         PRE_MEM_READ( "ioctl(generic)", arg, size);
      if ((dir & _VKI_IOC_READ) && size > 0)
         PRE_MEM_WRITE( "ioctl(generic)", arg, size);
   }
}

void ML_(POST_unknown_ioctl)(ThreadId tid, UInt res, UWord request, UWord arg)
{
   /* We don't have any specific information on it, so
      try to do something reasonable based on direction and
      size bits.  The encoding scheme is described in
      /usr/include/asm/ioctl.h or /usr/include/sys/ioccom.h .
      
      According to Simon Hausmann, _IOC_READ means the kernel
      writes a value to the ioctl value passed from the user
      space and the other way around with _IOC_WRITE. */
   
   UInt dir  = _VKI_IOC_DIR(request);
   UInt size = _VKI_IOC_SIZE(request);
   if (size > 0 && (dir & _VKI_IOC_READ)
       && res == 0 
       && arg != (Addr)NULL)
   {
      POST_MEM_WRITE(arg, size);
   }
}

/* 
   If we're sending a SIGKILL to one of our own threads, then simulate
   it rather than really sending the signal, so that the target thread
   gets a chance to clean up.  Returns True if we did the killing (or
   no killing is necessary), and False if the caller should use the
   normal kill syscall.
   
   "pid" is any pid argument which can be passed to kill; group kills
   (< -1, 0), and owner kills (-1) are ignored, on the grounds that
   they'll most likely hit all the threads and we won't need to worry
   about cleanup.  In truth, we can't fully emulate these multicast
   kills.

   "tgid" is a thread group id.  If it is not -1, then the target
   thread must be in that thread group.
 */
Bool ML_(do_sigkill)(Int pid, Int tgid)
{
   ThreadState *tst;
   ThreadId tid;

   if (pid <= 0)
      return False;

   tid = VG_(lwpid_to_vgtid)(pid);
   if (tid == VG_INVALID_THREADID)
      return False;		/* none of our threads */

   tst = VG_(get_ThreadState)(tid);
   if (tst == NULL || tst->status == VgTs_Empty)
      return False;		/* hm, shouldn't happen */

   if (tgid != -1 && tst->os_state.threadgroup != tgid)
      return False;		/* not the right thread group */

   /* Check to see that the target isn't already exiting. */
   if (!VG_(is_exiting)(tid)) {
      if (VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg,
                      "Thread %d being killed with SIGKILL\n", 
                      tst->tid);
      
      tst->exitreason = VgSrc_FatalSig;
      tst->os_state.fatalsig = VKI_SIGKILL;
      
      if (!VG_(is_running_thread)(tid))
	 VG_(get_thread_out_of_syscall)(tid);
   }
   
   return True;
}

PRE(sys_kill)
{
   PRINT("sys_kill ( %ld, %ld )", ARG1,ARG2);
   PRE_REG_READ2(long, "kill", int, pid, int, sig);
   if (!ML_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* If we're sending SIGKILL, check to see if the target is one of
      our threads and handle it specially. */
   if (ARG2 == VKI_SIGKILL && ML_(do_sigkill)(ARG1, -1))
      SET_STATUS_Success(0);
   else
      /* re syscall3: Darwin has a 3rd arg, which is a flag (boolean)
         affecting how posix-compliant the call is.  I guess it is
         harmless to pass the 3rd arg on other platforms; hence pass
         it on all. */
      SET_STATUS_from_SysRes( VG_(do_syscall3)(SYSNO, ARG1, ARG2, ARG3) );

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "kill: sent signal %ld to pid %ld\n",
		   ARG2, ARG1);

   /* This kill might have given us a pending signal.  Ask for a check once 
      the syscall is done. */
   *flags |= SfPollAfter;
}

PRE(sys_link)
{
   *flags |= SfMayBlock;
   PRINT("sys_link ( %#lx(%s), %#lx(%s) )", ARG1,(char*)ARG1,ARG2,(char*)ARG2);
   PRE_REG_READ2(long, "link", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "link(oldpath)", ARG1);
   PRE_MEM_RASCIIZ( "link(newpath)", ARG2);
}

PRE(sys_newlstat)
{
   PRINT("sys_newlstat ( %#lx(%s), %#lx )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "lstat", char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "lstat(file_name)", ARG1 );
   PRE_MEM_WRITE( "lstat(buf)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_newlstat)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}

PRE(sys_mkdir)
{
   *flags |= SfMayBlock;
   PRINT("sys_mkdir ( %#lx(%s), %ld )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "mkdir", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "mkdir(pathname)", ARG1 );
}

PRE(sys_mprotect)
{
   PRINT("sys_mprotect ( %#lx, %llu, %ld )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "mprotect",
                 unsigned long, addr, vki_size_t, len, unsigned long, prot);

   if (!ML_(valid_client_addr)(ARG1, ARG2, tid, "mprotect")) {
      SET_STATUS_Failure( VKI_ENOMEM );
   } 
#if defined(VKI_PROT_GROWSDOWN)
   else 
   if (ARG3 & (VKI_PROT_GROWSDOWN|VKI_PROT_GROWSUP)) {
      /* Deal with mprotects on growable stack areas.

         The critical files to understand all this are mm/mprotect.c
         in the kernel and sysdeps/unix/sysv/linux/dl-execstack.c in
         glibc.

         The kernel provides PROT_GROWSDOWN and PROT_GROWSUP which
         round the start/end address of mprotect to the start/end of
         the underlying vma and glibc uses that as an easy way to
         change the protection of the stack by calling mprotect on the
         last page of the stack with PROT_GROWSDOWN set.

         The sanity check provided by the kernel is that the vma must
         have the VM_GROWSDOWN/VM_GROWSUP flag set as appropriate.  */
      UInt grows = ARG3 & (VKI_PROT_GROWSDOWN|VKI_PROT_GROWSUP);
      NSegment const *aseg = VG_(am_find_nsegment)(ARG1);
      NSegment const *rseg;

      vg_assert(aseg);

      if (grows == VKI_PROT_GROWSDOWN) {
         rseg = VG_(am_next_nsegment)( (NSegment*)aseg, False/*backwards*/ );
         if (rseg &&
             rseg->kind == SkResvn &&
             rseg->smode == SmUpper &&
             rseg->end+1 == aseg->start) {
            Addr end = ARG1 + ARG2;
            ARG1 = aseg->start;
            ARG2 = end - aseg->start;
            ARG3 &= ~VKI_PROT_GROWSDOWN;
         } else {
            SET_STATUS_Failure( VKI_EINVAL );
         }
      } else if (grows == VKI_PROT_GROWSUP) {
         rseg = VG_(am_next_nsegment)( (NSegment*)aseg, True/*forwards*/ );
         if (rseg &&
             rseg->kind == SkResvn &&
             rseg->smode == SmLower &&
             aseg->end+1 == rseg->start) {
            ARG2 = aseg->end - ARG1 + 1;
            ARG3 &= ~VKI_PROT_GROWSUP;
         } else {
            SET_STATUS_Failure( VKI_EINVAL );
         }
      } else {
         /* both GROWSUP and GROWSDOWN */
         SET_STATUS_Failure( VKI_EINVAL );
      }
   }
#endif   // defined(VKI_PROT_GROWSDOWN)
}

POST(sys_mprotect)
{
   Addr a    = ARG1;
   SizeT len = ARG2;
   Int  prot = ARG3;

   ML_(notify_core_and_tool_of_mprotect)(a, len, prot);
}

PRE(sys_munmap)
{
   if (0) VG_(printf)("  munmap( %#lx )\n", ARG1);
   PRINT("sys_munmap ( %#lx, %llu )", ARG1,(ULong)ARG2);
   PRE_REG_READ2(long, "munmap", unsigned long, start, vki_size_t, length);

   if (!ML_(valid_client_addr)(ARG1, ARG2, tid, "munmap"))
      SET_STATUS_Failure( VKI_EINVAL );
}

POST(sys_munmap)
{
   Addr  a   = ARG1;
   SizeT len = ARG2;

   ML_(notify_core_and_tool_of_munmap)( (Addr64)a, (ULong)len );
}

PRE(sys_mincore)
{
   PRINT("sys_mincore ( %#lx, %llu, %#lx )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "mincore",
                 unsigned long, start, vki_size_t, length,
                 unsigned char *, vec);
   PRE_MEM_WRITE( "mincore(vec)", ARG3, VG_PGROUNDUP(ARG2) / VKI_PAGE_SIZE );
}
POST(sys_mincore)
{
   POST_MEM_WRITE( ARG3, VG_PGROUNDUP(ARG2) / VKI_PAGE_SIZE );  
}

PRE(sys_nanosleep)
{
   *flags |= SfMayBlock|SfPostOnFail;
   PRINT("sys_nanosleep ( %#lx, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "nanosleep", 
                 struct timespec *, req, struct timespec *, rem);
   PRE_MEM_READ( "nanosleep(req)", ARG1, sizeof(struct vki_timespec) );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "nanosleep(rem)", ARG2, sizeof(struct vki_timespec) );
}

POST(sys_nanosleep)
{
   vg_assert(SUCCESS || FAILURE);
   if (ARG2 != 0 && FAILURE && ERR == VKI_EINTR)
      POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_open)
{
   if (ARG2 & VKI_O_CREAT) {
      // 3-arg version
      PRINT("sys_open ( %#lx(%s), %ld, %ld )",ARG1,(char*)ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "open",
                    const char *, filename, int, flags, int, mode);
   } else {
      // 2-arg version
      PRINT("sys_open ( %#lx(%s), %ld )",ARG1,(char*)ARG1,ARG2);
      PRE_REG_READ2(long, "open",
                    const char *, filename, int, flags);
   }
   PRE_MEM_RASCIIZ( "open(filename)", ARG1 );

#if defined(VGO_linux)
   /* Handle the case where the open is of /proc/self/cmdline or
      /proc/<pid>/cmdline, and just give it a copy of the fd for the
      fake file we cooked up at startup (in m_main).  Also, seek the
      cloned fd back to the start. */
   {
      HChar  name[30];
      Char*  arg1s = (Char*) ARG1;
      SysRes sres;

      VG_(sprintf)(name, "/proc/%d/cmdline", VG_(getpid)());
      if (ML_(safe_to_deref)( arg1s, 1 ) &&
          (VG_STREQ(arg1s, name) || VG_STREQ(arg1s, "/proc/self/cmdline"))
         )
      {
         sres = VG_(dup)( VG_(cl_cmdline_fd) );
         SET_STATUS_from_SysRes( sres );
         if (!sr_isError(sres)) {
            OffT off = VG_(lseek)( sr_Res(sres), 0, VKI_SEEK_SET );
            if (off < 0)
               SET_STATUS_Failure( VKI_EMFILE );
         }
         return;
      }
   }
#endif // defined(VGO_linux)

   /* Otherwise handle normally */
   *flags |= SfMayBlock;
}

POST(sys_open)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (Char*)ARG1);
   }
}

PRE(sys_read)
{
   *flags |= SfMayBlock;
   PRINT("sys_read ( %ld, %#lx, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "read",
                 unsigned int, fd, char *, buf, vki_size_t, count);

   if (!ML_(fd_allowed)(ARG1, "read", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_WRITE( "read(buf)", ARG2, ARG3 );
}

POST(sys_read)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_write)
{
   Bool ok;
   *flags |= SfMayBlock;
   PRINT("sys_write ( %ld, %#lx, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "write",
                 unsigned int, fd, const char *, buf, vki_size_t, count);
   /* check to see if it is allowed.  If not, try for an exemption from
      --sim-hints=enable-outer (used for self hosting). */
   ok = ML_(fd_allowed)(ARG1, "write", tid, False);
   if (!ok && ARG1 == 2/*stderr*/ 
           && VG_(strstr)(VG_(clo_sim_hints),"enable-outer"))
      ok = True;
   if (!ok)
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_READ( "write(buf)", ARG2, ARG3 );
}

PRE(sys_creat)
{
   *flags |= SfMayBlock;
   PRINT("sys_creat ( %#lx(%s), %ld )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "creat", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "creat(pathname)", ARG1 );
}

POST(sys_creat)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "creat", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (Char*)ARG1);
   }
}

PRE(sys_poll)
{
   /* struct pollfd {
        int fd;           -- file descriptor
        short events;     -- requested events
        short revents;    -- returned events
      };
      int poll(struct pollfd *ufds, unsigned int nfds, int timeout) 
   */
   UInt i;
   struct vki_pollfd* ufds = (struct vki_pollfd *)ARG1;
   *flags |= SfMayBlock;
   PRINT("sys_poll ( %#lx, %ld, %ld )\n", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "poll",
                 struct vki_pollfd *, ufds, unsigned int, nfds, long, timeout);

   for (i = 0; i < ARG2; i++) {
      PRE_MEM_READ( "poll(ufds.fd)",
                    (Addr)(&ufds[i].fd), sizeof(ufds[i].fd) );
      PRE_MEM_READ( "poll(ufds.events)",
                    (Addr)(&ufds[i].events), sizeof(ufds[i].events) );
      PRE_MEM_WRITE( "poll(ufds.reventss)",
                     (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }
}

POST(sys_poll)
{
   if (RES >= 0) {
      UInt i;
      struct vki_pollfd* ufds = (struct vki_pollfd *)ARG1;
      for (i = 0; i < ARG2; i++)
	 POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }
}

PRE(sys_readlink)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   Word saved = SYSNO;

   PRINT("sys_readlink ( %#lx(%s), %#lx, %llu )", ARG1,(char*)ARG1,ARG2,(ULong)ARG3);
   PRE_REG_READ3(long, "readlink",
                 const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlink(path)", ARG1 );
   PRE_MEM_WRITE( "readlink(buf)", ARG2,ARG3 );

   {
#if defined(VGO_linux)
      /*
       * Handle the case where readlink is looking at /proc/self/exe or
       * /proc/<pid>/exe.
       */
      HChar name[25];
      Char* arg1s = (Char*) ARG1;
      VG_(sprintf)(name, "/proc/%d/exe", VG_(getpid)());
      if (ML_(safe_to_deref)(arg1s, 1) &&
          (VG_STREQ(arg1s, name) || VG_STREQ(arg1s, "/proc/self/exe"))
         )
      {
         VG_(sprintf)(name, "/proc/self/fd/%d", VG_(cl_exec_fd));
         SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, (UWord)name, 
                                                         ARG2, ARG3));
      } else
#endif // defined(VGO_linux)
      {
         /* Normal case */
         SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, ARG1, ARG2, ARG3));
      }
   }

   if (SUCCESS && RES > 0)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_readv)
{
   Int i;
   struct vki_iovec * vec;
   *flags |= SfMayBlock;
   PRINT("sys_readv ( %ld, %#lx, %llu )",ARG1,ARG2,(ULong)ARG3);
   PRE_REG_READ3(ssize_t, "readv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count);
   if (!ML_(fd_allowed)(ARG1, "readv", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "readv(vector)", ARG2, ARG3 * sizeof(struct vki_iovec) );

      if (ARG2 != 0) {
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct vki_iovec *)ARG2;
         for (i = 0; i < (Int)ARG3; i++)
            PRE_MEM_WRITE( "readv(vector[...])",
                           (Addr)vec[i].iov_base, vec[i].iov_len );
      }
   }
}

POST(sys_readv)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      Int i;
      struct vki_iovec * vec = (struct vki_iovec *)ARG2;
      Int remains = RES;

      /* RES holds the number of bytes read. */
      for (i = 0; i < (Int)ARG3; i++) {
	 Int nReadThisBuf = vec[i].iov_len;
	 if (nReadThisBuf > remains) nReadThisBuf = remains;
	 POST_MEM_WRITE( (Addr)vec[i].iov_base, nReadThisBuf );
	 remains -= nReadThisBuf;
	 if (remains < 0) VG_(core_panic)("readv: remains < 0");
      }
   }
}

PRE(sys_rename)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_rename ( %#lx(%s), %#lx(%s) )", ARG1,(char*)ARG1,ARG2,(char*)ARG2);
   PRE_REG_READ2(long, "rename", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "rename(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "rename(newpath)", ARG2 );
}

PRE(sys_rmdir)
{
   *flags |= SfMayBlock;
   PRINT("sys_rmdir ( %#lx(%s) )", ARG1,(char*)ARG1);
   PRE_REG_READ1(long, "rmdir", const char *, pathname);
   PRE_MEM_RASCIIZ( "rmdir(pathname)", ARG1 );
}

PRE(sys_select)
{
   *flags |= SfMayBlock;
   PRINT("sys_select ( %ld, %#lx, %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "select",
                 int, n, vki_fd_set *, readfds, vki_fd_set *, writefds,
                 vki_fd_set *, exceptfds, struct vki_timeval *, timeout);
   // XXX: this possibly understates how much memory is read.
   if (ARG2 != 0)
      PRE_MEM_READ( "select(readfds)",   
		     ARG2, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG3 != 0)
      PRE_MEM_READ( "select(writefds)",  
		     ARG3, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG4 != 0)
      PRE_MEM_READ( "select(exceptfds)", 
		     ARG4, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG5 != 0)
      PRE_timeval_READ( "select(timeout)", ARG5 );
}

PRE(sys_setgid)
{
   PRINT("sys_setgid ( %ld )", ARG1);
   PRE_REG_READ1(long, "setgid", vki_gid_t, gid);
}

PRE(sys_setsid)
{
   PRINT("sys_setsid ( )");
   PRE_REG_READ0(long, "setsid");
}

PRE(sys_setgroups)
{
   PRINT("setgroups ( %llu, %#lx )", (ULong)ARG1, ARG2);
   PRE_REG_READ2(long, "setgroups", int, size, vki_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_READ( "setgroups(list)", ARG2, ARG1 * sizeof(vki_gid_t) );
}

PRE(sys_setpgid)
{
   PRINT("setpgid ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "setpgid", vki_pid_t, pid, vki_pid_t, pgid);
}

PRE(sys_setregid)
{
   PRINT("sys_setregid ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "setregid", vki_gid_t, rgid, vki_gid_t, egid);
}

PRE(sys_setreuid)
{
   PRINT("sys_setreuid ( 0x%lx, 0x%lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "setreuid", vki_uid_t, ruid, vki_uid_t, euid);
}

PRE(sys_setrlimit)
{
   UWord arg1 = ARG1;
   PRINT("sys_setrlimit ( %ld, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "setrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_READ( "setrlimit(rlim)", ARG2, sizeof(struct vki_rlimit) );

#ifdef _RLIMIT_POSIX_FLAG
   // Darwin will sometimes set _RLIMIT_POSIX_FLAG on setrlimit calls.
   // Unset it here to make the if statements below work correctly.
   arg1 &= ~_RLIMIT_POSIX_FLAG;
#endif

   if (ARG2 &&
       ((struct vki_rlimit *)ARG2)->rlim_cur > ((struct vki_rlimit *)ARG2)->rlim_max) {
      SET_STATUS_Failure( VKI_EINVAL );
   }
   else if (arg1 == VKI_RLIMIT_NOFILE) {
      if (((struct vki_rlimit *)ARG2)->rlim_cur > VG_(fd_hard_limit) ||
          ((struct vki_rlimit *)ARG2)->rlim_max != VG_(fd_hard_limit)) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         VG_(fd_soft_limit) = ((struct vki_rlimit *)ARG2)->rlim_cur;
         SET_STATUS_Success( 0 );
      }
   }
   else if (arg1 == VKI_RLIMIT_DATA) {
      if (((struct vki_rlimit *)ARG2)->rlim_cur > VG_(client_rlimit_data).rlim_max ||
          ((struct vki_rlimit *)ARG2)->rlim_max > VG_(client_rlimit_data).rlim_max) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         VG_(client_rlimit_data) = *(struct vki_rlimit *)ARG2;
         SET_STATUS_Success( 0 );
      }
   }
   else if (arg1 == VKI_RLIMIT_STACK && tid == 1) {
      if (((struct vki_rlimit *)ARG2)->rlim_cur > VG_(client_rlimit_stack).rlim_max ||
          ((struct vki_rlimit *)ARG2)->rlim_max > VG_(client_rlimit_stack).rlim_max) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         VG_(threads)[tid].client_stack_szB = ((struct vki_rlimit *)ARG2)->rlim_cur;
         VG_(client_rlimit_stack) = *(struct vki_rlimit *)ARG2;
         SET_STATUS_Success( 0 );
      }
   }
}

PRE(sys_setuid)
{
   PRINT("sys_setuid ( %ld )", ARG1);
   PRE_REG_READ1(long, "setuid", vki_uid_t, uid);
}

PRE(sys_newstat)
{
   PRINT("sys_newstat ( %#lx(%s), %#lx )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "stat", char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "stat(file_name)", ARG1 );
   PRE_MEM_WRITE( "stat(buf)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_newstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}

PRE(sys_statfs)
{
   PRINT("sys_statfs ( %#lx(%s), %#lx )",ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "statfs", const char *, path, struct statfs *, buf);
   PRE_MEM_RASCIIZ( "statfs(path)", ARG1 );
   PRE_MEM_WRITE( "statfs(buf)", ARG2, sizeof(struct vki_statfs) );
}
POST(sys_statfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_statfs) );
}

PRE(sys_statfs64)
{
   PRINT("sys_statfs64 ( %#lx(%s), %llu, %#lx )",ARG1,(char*)ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "statfs64",
                 const char *, path, vki_size_t, size, struct statfs64 *, buf);
   PRE_MEM_RASCIIZ( "statfs64(path)", ARG1 );
   PRE_MEM_WRITE( "statfs64(buf)", ARG3, ARG2 );
}
POST(sys_statfs64)
{
   POST_MEM_WRITE( ARG3, ARG2 );
}

PRE(sys_symlink)
{
   *flags |= SfMayBlock;
   PRINT("sys_symlink ( %#lx(%s), %#lx(%s) )",ARG1,(char*)ARG1,ARG2,(char*)ARG2);
   PRE_REG_READ2(long, "symlink", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "symlink(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "symlink(newpath)", ARG2 );
}

PRE(sys_time)
{
   /* time_t time(time_t *t); */
   PRINT("sys_time ( %#lx )",ARG1);
   PRE_REG_READ1(long, "time", int *, t);
   if (ARG1 != 0) {
      PRE_MEM_WRITE( "time(t)", ARG1, sizeof(vki_time_t) );
   }
}

POST(sys_time)
{
   if (ARG1 != 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_time_t) );
   }
}

PRE(sys_times)
{
   PRINT("sys_times ( %#lx )", ARG1);
   PRE_REG_READ1(long, "times", struct tms *, buf);
   if (ARG1 != 0) {
      PRE_MEM_WRITE( "times(buf)", ARG1, sizeof(struct vki_tms) );
   }
}

POST(sys_times)
{
   if (ARG1 != 0) {
      POST_MEM_WRITE( ARG1, sizeof(struct vki_tms) );
   }
}

PRE(sys_umask)
{
   PRINT("sys_umask ( %ld )", ARG1);
   PRE_REG_READ1(long, "umask", int, mask);
}

PRE(sys_unlink)
{
   *flags |= SfMayBlock;
   PRINT("sys_unlink ( %#lx(%s) )", ARG1,(char*)ARG1);
   PRE_REG_READ1(long, "unlink", const char *, pathname);
   PRE_MEM_RASCIIZ( "unlink(pathname)", ARG1 );
}

PRE(sys_newuname)
{
   PRINT("sys_newuname ( %#lx )", ARG1);
   PRE_REG_READ1(long, "uname", struct new_utsname *, buf);
   PRE_MEM_WRITE( "uname(buf)", ARG1, sizeof(struct vki_new_utsname) );
}

POST(sys_newuname)
{
   if (ARG1 != 0) {
      POST_MEM_WRITE( ARG1, sizeof(struct vki_new_utsname) );
   }
}

PRE(sys_waitpid)
{
   *flags |= SfMayBlock;
   PRINT("sys_waitpid ( %ld, %#lx, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "waitpid", 
                 vki_pid_t, pid, unsigned int *, status, int, options);

   if (ARG2 != (Addr)NULL)
      PRE_MEM_WRITE( "waitpid(status)", ARG2, sizeof(int) );
}

POST(sys_waitpid)
{
   if (ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, sizeof(int) );
}

PRE(sys_wait4)
{
   *flags |= SfMayBlock;
   PRINT("sys_wait4 ( %ld, %#lx, %ld, %#lx )", ARG1,ARG2,ARG3,ARG4);

   PRE_REG_READ4(long, "wait4", 
                 vki_pid_t, pid, unsigned int *, status, int, options,
                 struct rusage *, rusage);
   if (ARG2 != (Addr)NULL)
      PRE_MEM_WRITE( "wait4(status)", ARG2, sizeof(int) );
   if (ARG4 != (Addr)NULL)
      PRE_MEM_WRITE( "wait4(rusage)", ARG4, sizeof(struct vki_rusage) );
}

POST(sys_wait4)
{
   if (ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, sizeof(int) );
   if (ARG4 != (Addr)NULL)
      POST_MEM_WRITE( ARG4, sizeof(struct vki_rusage) );
}

PRE(sys_writev)
{
   Int i;
   struct vki_iovec * vec;
   *flags |= SfMayBlock;
   PRINT("sys_writev ( %ld, %#lx, %llu )",ARG1,ARG2,(ULong)ARG3);
   PRE_REG_READ3(ssize_t, "writev",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count);
   if (!ML_(fd_allowed)(ARG1, "writev", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "writev(vector)", 
		     ARG2, ARG3 * sizeof(struct vki_iovec) );
      if (ARG2 != 0) {
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct vki_iovec *)ARG2;
         for (i = 0; i < (Int)ARG3; i++)
            PRE_MEM_READ( "writev(vector[...])",
                           (Addr)vec[i].iov_base, vec[i].iov_len );
      }
   }
}

PRE(sys_utimes)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_utimes ( %#lx(%s), %#lx )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "utimes", char *, filename, struct timeval *, tvp);
   PRE_MEM_RASCIIZ( "utimes(filename)", ARG1 );
   if (ARG2 != 0) {
      PRE_timeval_READ( "utimes(tvp[0])", ARG2 );
      PRE_timeval_READ( "utimes(tvp[1])", ARG2+sizeof(struct vki_timeval) );
   }
}

PRE(sys_acct)
{
   PRINT("sys_acct ( %#lx(%s) )", ARG1,(char*)ARG1);
   PRE_REG_READ1(long, "acct", const char *, filename);
   PRE_MEM_RASCIIZ( "acct(filename)", ARG1 );
}

PRE(sys_pause)
{
   *flags |= SfMayBlock;
   PRINT("sys_pause ( )");
   PRE_REG_READ0(long, "pause");
}

PRE(sys_sigaltstack)
{
   PRINT("sigaltstack ( %#lx, %#lx )",ARG1,ARG2);
   PRE_REG_READ2(int, "sigaltstack",
                 const vki_stack_t *, ss, vki_stack_t *, oss);
   if (ARG1 != 0) {
      const vki_stack_t *ss = (vki_stack_t *)ARG1;
      PRE_MEM_READ( "sigaltstack(ss)", (Addr)&ss->ss_sp, sizeof(ss->ss_sp) );
      PRE_MEM_READ( "sigaltstack(ss)", (Addr)&ss->ss_flags, sizeof(ss->ss_flags) );
      PRE_MEM_READ( "sigaltstack(ss)", (Addr)&ss->ss_size, sizeof(ss->ss_size) );
   }
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "sigaltstack(oss)", ARG2, sizeof(vki_stack_t) );
   }

   SET_STATUS_from_SysRes( 
      VG_(do_sys_sigaltstack) (tid, (vki_stack_t*)ARG1, 
                              (vki_stack_t*)ARG2)
   );
}
POST(sys_sigaltstack)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG2 != 0)
      POST_MEM_WRITE( ARG2, sizeof(vki_stack_t));
}

#undef PRE
#undef POST

#endif // defined(VGO_linux) || defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

