/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Wrappers for generic Unix system calls                       ---*/
/*---                                            syswrap-generic.c ---*/
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

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_debuginfo.h"     // VG_(di_notify_*)
#include "pub_core_aspacemgr.h"
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"   // VG_(brk_base), VG_(brk_limit)
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_core_gdbserver.h"     // VG_(gdbserver)
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
#include "pub_core_stacks.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"

#include "config.h"

void ML_(guess_and_register_stack) (Addr sp, ThreadState* tst)
{
   Bool debug = False;
   NSegment const* seg;

   /* We don't really know where the client stack is, because its
      allocated by the client.  The best we can do is look at the
      memory mappings and try to derive some useful information.  We
      assume that sp starts near its highest possible value, and can
      only go down to the start of the mmaped segment. */
   seg = VG_(am_find_nsegment)(sp);
   if (seg 
       && VG_(am_is_valid_for_client)(sp, 1, VKI_PROT_READ | VKI_PROT_WRITE)) {
      tst->client_stack_highest_byte = (Addr)VG_PGROUNDUP(sp)-1;
      tst->client_stack_szB = tst->client_stack_highest_byte - seg->start + 1;

      tst->os_state.stk_id 
         = VG_(register_stack)(seg->start, tst->client_stack_highest_byte);

      if (debug)
	 VG_(printf)("tid %u: guessed client stack range [%#lx-%#lx]"
                     " as stk_id %lu\n",
		     tst->tid, seg->start, tst->client_stack_highest_byte,
                     tst->os_state.stk_id);
   } else {
      VG_(message)(Vg_UserMsg,
                   "!? New thread %u starts with SP(%#lx) unmapped\n",
		   tst->tid, sp);
      tst->client_stack_highest_byte = 0;
      tst->client_stack_szB  = 0;
   }
}

/* Returns True iff address range is something the client can
   plausibly mess with: all of it is either already belongs to the
   client or is free or a reservation. */

Bool ML_(valid_client_addr)(Addr start, SizeT size, ThreadId tid,
                                   const HChar *syscallname)
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

Bool ML_(safe_to_deref) ( const void *start, SizeT size )
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
      VG_(discard_translations)( a, (ULong)len,
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
      VG_(discard_translations)( a, (ULong)len, 
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
      VG_(discard_translations)( a, (ULong)len, 
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
      VG_(printf)("do_remap (old %#lx %lu) (new %#lx %lu) %s %s\n",
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
   if (old_seg->kind != SkAnonC && old_seg->kind != SkFileC 
       && old_seg->kind != SkShmC)
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
   vg_assert(needA > 0);

   advised = VG_(am_get_advisory_client_simple)( needA, needL, &ok );
   if (ok) {
      /* Fixes bug #129866. */
      ok = VG_(am_covered_by_single_free_segment) ( needA, needL );
   }
   if (ok && advised == needA) {
      const NSegment *new_seg = VG_(am_extend_map_client)( old_addr, needL );
      if (new_seg) {
         VG_TRACK( new_mem_mmap, needA, needL, 
                                 new_seg->hasR, 
                                 new_seg->hasW, new_seg->hasX,
                                 0/*di_handle*/ );
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

   vg_assert(needA > 0);

   advised = VG_(am_get_advisory_client_simple)( needA, needL, &ok );
   if (ok) {
      /* Fixes bug #129866. */
      ok = VG_(am_covered_by_single_free_segment) ( needA, needL );
   }
   if (!ok || advised != needA)
      goto eNOMEM;
   const NSegment *new_seg = VG_(am_extend_map_client)( old_addr, needL );
   if (!new_seg)
      goto eNOMEM;
   VG_TRACK( new_mem_mmap, needA, needL, 
                           new_seg->hasR, new_seg->hasW, new_seg->hasX,
                           0/*di_handle*/ );

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
   HChar *pathname;               /* NULL if not a regular file or unknown */
   ExeContext *where;             /* NULL if inherited from parent */
   struct OpenFd *next, *prev;
} OpenFd;

/* List of allocated file descriptors. */
static OpenFd *allocated_fds = NULL;

/* Count of open file descriptors. */
static Int fd_count = 0;


/* Note the fact that a file descriptor was just closed. */
void ML_(record_fd_close)(Int fd)
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
            VG_(free) (i->pathname);
         VG_(free) (i);
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
void ML_(record_fd_open_with_given_name)(ThreadId tid, Int fd,
                                         const HChar *pathname)
{
   OpenFd *i;

   if (fd >= VG_(fd_hard_limit))
      return;			/* Valgrind internal */

   /* Check to see if this fd is already open. */
   i = allocated_fds;
   while (i) {
      if (i->fd == fd) {
         if (i->pathname) VG_(free)(i->pathname);
         break;
      }
      i = i->next;
   }

   /* Not already one: allocate an OpenFd */
   if (i == NULL) {
      i = VG_(malloc)("syswrap.rfdowgn.1", sizeof(OpenFd));

      i->prev = NULL;
      i->next = allocated_fds;
      if(allocated_fds) allocated_fds->prev = i;
      allocated_fds = i;
      fd_count++;
   }

   i->fd = fd;
   i->pathname = VG_(strdup)("syswrap.rfdowgn.2", pathname);
   i->where = (tid == -1) ? NULL : VG_(record_ExeContext)(tid, 0/*first_ip_delta*/);
}

// Record opening of an fd, and find its name.
void ML_(record_fd_open_named)(ThreadId tid, Int fd)
{
   const HChar* buf;
   const HChar* name;
   if (VG_(resolve_filename)(fd, &buf))
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

// Return if a given file descriptor is already recorded.
Bool ML_(fd_recorded)(Int fd)
{
   OpenFd *i = allocated_fds;
   while (i) {
      if (i->fd == fd)
         return True;
      i = i->next;
   }
   return False;
}

/* Returned string must not be modified nor free'd. */
const HChar *ML_(find_fd_recorded_by_fd)(Int fd)
{
   OpenFd *i = allocated_fds;

   while (i) {
      if (i->fd == fd)
         return i->pathname;
      i = i->next;
   }

   return NULL;
}

static
HChar *unix_to_name(struct vki_sockaddr_un *sa, UInt len, HChar *name)
{
   if (sa == NULL || len == 0 || sa->sun_path[0] == '\0') {
      VG_(sprintf)(name, "<unknown>");
   } else {
      VG_(sprintf)(name, "%s", sa->sun_path);
   }

   return name;
}

static
HChar *inet_to_name(struct vki_sockaddr_in *sa, UInt len, HChar *name)
{
   if (sa == NULL || len == 0) {
      VG_(sprintf)(name, "<unknown>");
   } else if (sa->sin_port == 0) {
      VG_(sprintf)(name, "<unbound>");
   } else {
      UInt addr = VG_(ntohl)(sa->sin_addr.s_addr);
      VG_(sprintf)(name, "%u.%u.%u.%u:%u",
                   (addr>>24) & 0xFF, (addr>>16) & 0xFF,
                   (addr>>8) & 0xFF, addr & 0xFF,
                   VG_(ntohs)(sa->sin_port));
   }

   return name;
}

static
void inet6_format(HChar *s, const UChar ip[16])
{
   static const unsigned char V4mappedprefix[12] = {0,0,0,0,0,0,0,0,0,0,0xff,0xff};

   if (!VG_(memcmp)(ip, V4mappedprefix, 12)) {
      const struct vki_in_addr *sin_addr =
          (const struct vki_in_addr *)(ip + 12);
      UInt addr = VG_(ntohl)(sin_addr->s_addr);

      VG_(sprintf)(s, "::ffff:%u.%u.%u.%u",
                   (addr>>24) & 0xFF, (addr>>16) & 0xFF,
                   (addr>>8) & 0xFF, addr & 0xFF);
   } else {
      Bool compressing = False;
      Bool compressed = False;
      Int len = 0;
      Int i;

      for (i = 0; i < 16; i += 2) {
         UInt word = ((UInt)ip[i] << 8) | (UInt)ip[i+1];
         if (word == 0 && !compressed) {
            compressing = True;
         } else {
            if (compressing) {
               compressing = False;
               compressed = True;
               s[len++] = ':';
            }
            if (i > 0) {
               s[len++] = ':';
            }
            len += VG_(sprintf)(s + len, "%x", word);
         }
      }

      if (compressing) {
         s[len++] = ':';
         s[len++] = ':';
      }

      s[len++] = 0;
   }

   return;
}

static
HChar *inet6_to_name(struct vki_sockaddr_in6 *sa, UInt len, HChar *name)
{
   if (sa == NULL || len == 0) {
      VG_(sprintf)(name, "<unknown>");
   } else if (sa->sin6_port == 0) {
      VG_(sprintf)(name, "<unbound>");
   } else {
      HChar addr[100];    // large enough
      inet6_format(addr, (void *)&(sa->sin6_addr));
      VG_(sprintf)(name, "[%s]:%u", addr, VG_(ntohs)(sa->sin6_port));
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
      struct vki_sockaddr_in6 in6;
      struct vki_sockaddr_un un;
   } laddr;
   Int llen;

   llen = sizeof(laddr);
   VG_(memset)(&laddr, 0, llen);

   if(VG_(getsockname)(fd, (struct vki_sockaddr *)&(laddr.a), &llen) != -1) {
      switch(laddr.a.sa_family) {
      case VKI_AF_INET: {
         HChar lname[32];   // large enough
         HChar pname[32];   // large enough
         struct vki_sockaddr_in paddr;
         Int plen = sizeof(struct vki_sockaddr_in);

         if (VG_(getpeername)(fd, (struct vki_sockaddr *)&paddr, &plen) != -1) {
            VG_(message)(Vg_UserMsg, "Open AF_INET socket %d: %s <-> %s\n", fd,
                         inet_to_name(&(laddr.in), llen, lname),
                         inet_to_name(&paddr, plen, pname));
         } else {
            VG_(message)(Vg_UserMsg, "Open AF_INET socket %d: %s <-> unbound\n",
                         fd, inet_to_name(&(laddr.in), llen, lname));
         }
         return;
         }
      case VKI_AF_INET6: {
         HChar lname[128];  // large enough
         HChar pname[128];  // large enough
         struct vki_sockaddr_in6 paddr;
         Int plen = sizeof(struct vki_sockaddr_in6);

         if (VG_(getpeername)(fd, (struct vki_sockaddr *)&paddr, &plen) != -1) {
            VG_(message)(Vg_UserMsg, "Open AF_INET6 socket %d: %s <-> %s\n", fd,
                         inet6_to_name(&(laddr.in6), llen, lname),
                         inet6_to_name(&paddr, plen, pname));
         } else {
            VG_(message)(Vg_UserMsg, "Open AF_INET6 socket %d: %s <-> unbound\n",
                         fd, inet6_to_name(&(laddr.in6), llen, lname));
         }
         return;
         }
      case VKI_AF_UNIX: {
         static char lname[256];
         VG_(message)(Vg_UserMsg, "Open AF_UNIX socket %d: %s\n", fd,
                      unix_to_name(&(laddr.un), llen, lname));
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
void VG_(show_open_fds) (const HChar* when)
{
   OpenFd *i;
   int non_std = 0;

   for (i = allocated_fds; i; i = i->next) {
      if (i->fd > 2)
         non_std++;
   }

   /* If we are running quiet and there are either no open file descriptors
      or not tracking all fds, then don't report anything.  */
   if ((fd_count == 0
        || ((non_std == 0) && (VG_(clo_track_fds) < 2)))
       && (VG_(clo_verbosity) == 0))
      return;

   VG_(message)(Vg_UserMsg, "FILE DESCRIPTORS: %d open (%d std) %s.\n",
                fd_count, fd_count - non_std, when);

   for (i = allocated_fds; i; i = i->next) {
      if (i->fd <= 2 && VG_(clo_track_fds) < 2)
          continue;

      if (i->pathname) {
         VG_(message)(Vg_UserMsg, "Open file descriptor %d: %s\n", i->fd,
                      i->pathname);
      } else {
         Int val;
         Int len = sizeof(val);

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
   struct vki_dirent64 d;
   SysRes f;

   f = VG_(open)("/proc/self/fd", VKI_O_RDONLY, 0);
   if (sr_isError(f)) {
      init_preopened_fds_without_proc_self_fd();
      return;
   }

   while ((ret = VG_(getdents64)(sr_Res(f), &d, sizeof(d))) != 0) {
      if (ret == -1)
         goto out;

      if (VG_(strcmp)(d.d_name, ".") && VG_(strcmp)(d.d_name, "..")) {
         HChar* s;
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

#elif defined(VGO_darwin) || defined(VGO_freebsd)
   init_preopened_fds_without_proc_self_fd();

#elif defined(VGO_solaris)
   Int ret;
   Char buf[VKI_MAXGETDENTS_SIZE];
   SysRes f;

   f = VG_(open)("/proc/self/fd", VKI_O_RDONLY, 0);
   if (sr_isError(f)) {
      init_preopened_fds_without_proc_self_fd();
      return;
   }

   while ((ret = VG_(getdents64)(sr_Res(f), (struct vki_dirent64 *) buf,
                                 sizeof(buf))) > 0) {
      Int i = 0;
      while (i < ret) {
         /* Proceed one entry. */
         struct vki_dirent64 *d = (struct vki_dirent64 *) (buf + i);
         if (VG_(strcmp)(d->d_name, ".") && VG_(strcmp)(d->d_name, "..")) {
            HChar *s;
            Int fno = VG_(strtoll10)(d->d_name, &s);
            if (*s == '\0') {
               if (fno != sr_Res(f))
                  if (VG_(clo_track_fds))
                     ML_(record_fd_open_named)(-1, fno);
            } else {
               VG_(message)(Vg_DebugMsg,
                     "Warning: invalid file name in /proc/self/fd: %s\n",
                     d->d_name);
            }
         }

         /* Move on the next entry. */
         i += d->d_reclen;
      }
   }

   VG_(close)(sr_Res(f));

#else
#  error Unknown OS
#endif
}

static 
void pre_mem_read_sendmsg ( ThreadId tid, Bool read,
                            const HChar *msg, Addr base, SizeT size )
{
   HChar outmsg[VG_(strlen)(msg) + 10]; // large enough
   VG_(sprintf)(outmsg, "sendmsg%s", msg);
   PRE_MEM_READ( outmsg, base, size );
}

static 
void pre_mem_write_recvmsg ( ThreadId tid, Bool read,
                             const HChar *msg, Addr base, SizeT size )
{
   HChar outmsg[VG_(strlen)(msg) + 10]; // large enough
   VG_(sprintf)(outmsg, "recvmsg%s", msg);
   if ( read )
      PRE_MEM_READ( outmsg, base, size );
   else
      PRE_MEM_WRITE( outmsg, base, size );
}

static
void post_mem_write_recvmsg ( ThreadId tid, Bool read,
                              const HChar *fieldName, Addr base, SizeT size )
{
   if ( !read )
      POST_MEM_WRITE( base, size );
}
 
static
void msghdr_foreachfield ( 
        ThreadId tid,
        const HChar *name,
        struct vki_msghdr *msg,
        UInt length,
        void (*foreach_func)( ThreadId, Bool, const HChar *, Addr, SizeT ),
        Bool rekv /* "recv" apparently shadows some header decl on OSX108 */
     )
{
   HChar fieldName[VG_(strlen)(name) + 32]; // large enough.
   Addr a;
   SizeT s;

   if ( !msg )
      return;

   VG_(sprintf) ( fieldName, "(%s)", name );

   /* FIELDPAIR helps the compiler do one call to foreach_func
      for consecutive (no holes) fields. */
#define FIELDPAIR(f1,f2) \
   if (offsetof(struct vki_msghdr, f1) + sizeof(msg->f1)                \
       == offsetof(struct vki_msghdr, f2))                              \
      s += sizeof(msg->f2);                                             \
   else {                                                               \
      foreach_func (tid, True, fieldName, a, s);                        \
      a = (Addr)&msg->f2;                                               \
      s = sizeof(msg->f2);                                              \
   }

   a = (Addr)&msg->msg_name;
   s = sizeof(msg->msg_name);
   FIELDPAIR(msg_name,    msg_namelen);
   FIELDPAIR(msg_namelen, msg_iov);
   FIELDPAIR(msg_iov,     msg_iovlen);
   FIELDPAIR(msg_iovlen,  msg_control);
   FIELDPAIR(msg_control, msg_controllen);
   foreach_func ( tid, True, fieldName, a, s);
#undef FIELDPAIR

   /* msg_flags is completely ignored for send_mesg, recv_mesg doesn't read
      the field, but does write to it. */
   if ( rekv )
      foreach_func ( tid, False, fieldName, (Addr)&msg->msg_flags, sizeof( msg->msg_flags ) );

   if ( ML_(safe_to_deref)(&msg->msg_name, sizeof (void *))
        && msg->msg_name ) {
      VG_(sprintf) ( fieldName, "(%s.msg_name)", name );
      foreach_func ( tid, False, fieldName, 
                     (Addr)msg->msg_name, msg->msg_namelen );
   }

   if ( ML_(safe_to_deref)(&msg->msg_iov, sizeof (void *))
        && msg->msg_iov ) {
      struct vki_iovec *iov = msg->msg_iov;
      UInt i;

      if (ML_(safe_to_deref)(&msg->msg_iovlen, sizeof (UInt))) {
         VG_(sprintf) ( fieldName, "(%s.msg_iov)", name );
         foreach_func ( tid, True, fieldName, (Addr)iov,
                        msg->msg_iovlen * sizeof( struct vki_iovec ) );

         for ( i = 0; i < msg->msg_iovlen && length > 0; ++i, ++iov ) {
            if (ML_(safe_to_deref)(&iov->iov_len, sizeof (UInt))) {
               UInt iov_len = iov->iov_len <= length ? iov->iov_len : length;
               VG_(sprintf) ( fieldName, "(%s.msg_iov[%u])", name, i );
               foreach_func ( tid, False, fieldName,
                              (Addr)iov->iov_base, iov_len );
               length = length - iov_len;
            }
         }
      }
   }

   if ( ML_(safe_to_deref) (&msg->msg_control, sizeof (void *))
        && msg->msg_control ) {
      VG_(sprintf) ( fieldName, "(%s.msg_control)", name );
      foreach_func ( tid, False, fieldName, 
                     (Addr)msg->msg_control, msg->msg_controllen );
   }

}

static void check_cmsg_for_fds(ThreadId tid, struct vki_msghdr *msg)
{
   struct vki_cmsghdr *cm = VKI_CMSG_FIRSTHDR(msg);

   while (cm) {
      if (cm->cmsg_level == VKI_SOL_SOCKET 
          && cm->cmsg_type == VKI_SCM_RIGHTS ) {
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
void ML_(pre_mem_read_sockaddr) ( ThreadId tid,
                                  const HChar *description,
                                  struct vki_sockaddr *sa, UInt salen )
{
   HChar outmsg[VG_(strlen)( description ) + 30]; // large enough
   struct vki_sockaddr_un*  saun = (struct vki_sockaddr_un *)sa;
   struct vki_sockaddr_in*  sin  = (struct vki_sockaddr_in *)sa;
   struct vki_sockaddr_in6* sin6 = (struct vki_sockaddr_in6 *)sa;
#  ifdef VKI_AF_BLUETOOTH
   struct vki_sockaddr_rc*  rc   = (struct vki_sockaddr_rc *)sa;
#  endif
#  ifdef VKI_AF_NETLINK
   struct vki_sockaddr_nl*  nl   = (struct vki_sockaddr_nl *)sa;
#  endif

   /* NULL/zero-length sockaddrs are legal */
   if ( sa == NULL || salen == 0 ) return;

   VG_(sprintf) ( outmsg, description, "sa_family" );
   PRE_MEM_READ( outmsg, (Addr) &sa->sa_family, sizeof(vki_sa_family_t));
#if defined(VGO_freebsd)
   VG_(sprintf) ( outmsg, description, "sa_len" );
   PRE_MEM_READ( outmsg, (Addr) &sa->sa_len, sizeof(char));
#endif

   /* Don't do any extra checking if we cannot determine the sa_family. */
   if (! ML_(safe_to_deref) (&sa->sa_family, sizeof(vki_sa_family_t)))
      return;

   switch (sa->sa_family) {
                  
      case VKI_AF_UNIX:
         if (ML_(safe_to_deref) (&saun->sun_path, sizeof (Addr))) {
            VG_(sprintf) ( outmsg, description, "sun_path" );
            PRE_MEM_RASCIIZ( outmsg, (Addr) saun->sun_path );
            // GrP fixme max of sun_len-2? what about nul char?
         }
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

#     ifdef VKI_AF_BLUETOOTH
      case VKI_AF_BLUETOOTH:
         VG_(sprintf) ( outmsg, description, "rc_bdaddr" );
         PRE_MEM_READ( outmsg, (Addr) &rc->rc_bdaddr, sizeof (rc->rc_bdaddr) );
         VG_(sprintf) ( outmsg, description, "rc_channel" );
         PRE_MEM_READ( outmsg, (Addr) &rc->rc_channel, sizeof (rc->rc_channel) );
         break;
#     endif

#     ifdef VKI_AF_NETLINK
      case VKI_AF_NETLINK:
         VG_(sprintf)(outmsg, description, "nl_pid");
         PRE_MEM_READ(outmsg, (Addr)&nl->nl_pid, sizeof(nl->nl_pid));
         VG_(sprintf)(outmsg, description, "nl_groups");
         PRE_MEM_READ(outmsg, (Addr)&nl->nl_groups, sizeof(nl->nl_groups));
         break;
#     endif

#     ifdef VKI_AF_UNSPEC
      case VKI_AF_UNSPEC:
         break;
#     endif

      default:
         /* No specific information about this address family.
            Let's just check the full data following the family.
            Note that this can give false positive if this (unknown)
            struct sockaddr_???? has padding bytes between its elements. */
         VG_(sprintf) ( outmsg, description, "sa_data" );
         PRE_MEM_READ( outmsg, (Addr)&sa->sa_family + sizeof(sa->sa_family),
                       salen -  sizeof(sa->sa_family));
         break;
   }
}

/* Dereference a pointer to a UInt. */
static UInt deref_UInt ( ThreadId tid, Addr a, const HChar* s )
{
   UInt* a_p = (UInt*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(UInt) );
   if (a_p == NULL || ! ML_(safe_to_deref) (a_p, sizeof(UInt)))
      return 0;
   else
      return *a_p;
}

void ML_(buf_and_len_pre_check) ( ThreadId tid, Addr buf_p, Addr buflen_p,
                                  const HChar* buf_s, const HChar* buflen_s )
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
                                   Addr buf_p, Addr buflen_p, const HChar* s )
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

static Addr do_brk ( Addr newbrk, ThreadId tid )
{
   NSegment const* aseg;
   Addr newbrkP;
   SizeT delta;
   Bool debug = False;

   if (debug)
      VG_(printf)("\ndo_brk: brk_base=%#lx brk_limit=%#lx newbrk=%#lx\n",
		  VG_(brk_base), VG_(brk_limit), newbrk);

   if (0) VG_(am_show_nsegments)(0, "in_brk");

   if (newbrk < VG_(brk_base))
      /* Clearly impossible. */
      goto bad;

   if (newbrk < VG_(brk_limit)) {
      /* shrinking the data segment.  Be lazy and don't munmap the
         excess area. */
      NSegment const * seg = VG_(am_find_nsegment)(newbrk);
      vg_assert(seg);

      if (seg->hasT)
         VG_(discard_translations)( newbrk, VG_(brk_limit) - newbrk, 
                                    "do_brk(shrink)" );
      /* Since we're being lazy and not unmapping pages, we have to
         zero out the area, so that if the area later comes back into
         circulation, it will be filled with zeroes, as if it really
         had been unmapped and later remapped.  Be a bit paranoid and
         try hard to ensure we're not going to segfault by doing the
         write - check both ends of the range are in the same segment
         and that segment is writable. */
      NSegment const * seg2;

      seg2 = VG_(am_find_nsegment)( VG_(brk_limit) - 1 );
      vg_assert(seg2);

      if (seg == seg2 && seg->hasW)
         VG_(memset)( (void*)newbrk, 0, VG_(brk_limit) - newbrk );

      VG_(brk_limit) = newbrk;
      return newbrk;
   }

   /* otherwise we're expanding the brk segment. */
   if (VG_(brk_limit) > VG_(brk_base))
      aseg = VG_(am_find_nsegment)( VG_(brk_limit)-1 );
   else
      aseg = VG_(am_find_nsegment)( VG_(brk_limit) );

   /* These should be assured by setup_client_dataseg in m_main. */
   vg_assert(aseg);
   vg_assert(aseg->kind == SkAnonC);

   if (newbrk <= aseg->end + 1) {
      /* still fits within the anon segment. */
      VG_(brk_limit) = newbrk;
      return newbrk;
   }

   newbrkP = VG_PGROUNDUP(newbrk);
   delta = newbrkP - (aseg->end + 1);
   vg_assert(delta > 0);
   vg_assert(VG_IS_PAGE_ALIGNED(delta));
   
   Bool overflow = False;
   if (! VG_(am_extend_into_adjacent_reservation_client)( aseg->start, delta,
                                                          &overflow)) {
      if (overflow) {
         static Bool alreadyComplained = False;
         if (!alreadyComplained) {
            alreadyComplained = True;
            if (VG_(clo_verbosity) > 0) {
               VG_(umsg)("brk segment overflow in thread #%u: "
                         "can't grow to %#lx\n",
                         tid, newbrkP);
               VG_(umsg)("(see section Limitations in user manual)\n");
               VG_(umsg)("NOTE: further instances of this message "
                         "will not be shown\n");
            }
         }
      } else {
         if (VG_(clo_verbosity) > 0) {
            VG_(umsg)("Cannot map memory to grow brk segment in thread #%u "
                      "to %#lx\n", tid, newbrkP);
            VG_(umsg)("(see section Limitations in user manual)\n");
         }
      }
      goto bad;
   }

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
Bool ML_(fd_allowed)(Int fd, const HChar *syscallname, ThreadId tid,
                     Bool isNewFd)
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
   ML_(pre_mem_read_sockaddr) (
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
   ML_(pre_mem_read_sockaddr) (
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
   if (arg1 != 0) {
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
   ML_(pre_mem_read_sockaddr) ( tid,
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
ML_(generic_PRE_sys_sendmsg) ( ThreadId tid, const HChar *name,
                               struct vki_msghdr *msg )
{
   msghdr_foreachfield ( tid, name, msg, ~0, pre_mem_read_sendmsg, False );
}

/* ------ */

void
ML_(generic_PRE_sys_recvmsg) ( ThreadId tid, const HChar *name,
                               struct vki_msghdr *msg )
{
   msghdr_foreachfield ( tid, name, msg, ~0, pre_mem_write_recvmsg, True );
}

void 
ML_(generic_POST_sys_recvmsg) ( ThreadId tid, const HChar *name,
                                struct vki_msghdr *msg, UInt length )
{
   msghdr_foreachfield( tid, name, msg, length, post_mem_write_recvmsg, True );
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
   union vki_semun arg;
   SysRes res;

#  if defined(__NR_semctl)
#  if defined(VGO_darwin)
   /* Darwin has no specific 64 bit semid_ds, but has __NR_semctl. */
   struct vki_semid_ds buf;
   arg.buf = &buf;
#  else
   struct vki_semid64_ds buf;
   arg.buf64 = &buf;
#  endif
   res = VG_(do_syscall4)(__NR_semctl, semid, 0, VKI_IPC_STAT, *(UWord *)&arg);
   if (sr_isError(res))
      return 0;

   return buf.sem_nsems;
#  elif defined(__NR___semctl) /* FreeBSD */
   struct vki_semid_ds buf;
   arg.buf = &buf;
   res = VG_(do_syscall4)(__NR___semctl, semid, 0, VKI_IPC_STAT, *(UWord *)&arg);

   if (sr_isError(res))
      return 0;

   // both clang-tidy and coverity complain about this but I think they are both wrong
   return buf.sem_nsems;
#  elif defined(__NR_semsys) /* Solaris */
   struct vki_semid_ds buf;
   arg.buf = &buf;
   res = VG_(do_syscall5)(__NR_semsys, VKI_SEMCTL, semid, 0, VKI_IPC_STAT,
                          *(UWord *)&arg);
   if (sr_isError(res))
      return 0;

   return buf.sem_nsems;

#  else
   struct vki_semid_ds buf;
   arg.buf = &buf;
   res = VG_(do_syscall5)(__NR_ipc, 3 /* IPCOP_semctl */, semid, 0,
                          VKI_IPC_STAT, (UWord)&arg);
   if (sr_isError(res))
      return 0;

   return buf.sem_nsems;
#  endif
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
#if defined(VKI_IPC_64)
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_SEM_INFO|VKI_IPC_64:
#endif
#if defined(VGO_freebsd)
      PRE_MEM_WRITE( "semctl(IPC_INFO, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_semid_ds) );
#else
      PRE_MEM_WRITE( "semctl(IPC_INFO, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_seminfo) );
#endif
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
#endif
#if defined(VKI_IPC_STAT64)
   case VKI_IPC_STAT64:
#endif
#if defined(VKI_IPC_64) || defined(VKI_IPC_STAT64)
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
#endif
#if defined(VKI_IPC_SET64)
   case VKI_IPC_SET64:
#endif
#if defined(VKI_IPC64) || defined(VKI_IPC_SET64)
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
#if defined(VKI_IPC_64)
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_SEM_INFO|VKI_IPC_64:
#endif
#if defined(VGO_freebsd)
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_semid_ds) );
#else
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_seminfo) );
#endif
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
#endif
#if defined(VKI_IPC_STAT64)
   case VKI_IPC_STAT64:
#endif
#if defined(VKI_IPC_64) || defined(VKI_IPC_STAT64)
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
   /*
    * The excluded platforms below gained direct shmctl in Linux 5.1. Keep
    * using ipc-multiplexed shmctl to keep compatibility with older kernel
    * versions.
    */
#if defined(__NR_shmctl) && \
    !defined(VGP_x86_linux) && !defined(VGP_mips32_linux) && \
    !defined(VGP_ppc32_linux) && !defined(VGP_ppc64be_linux) && \
    !defined(VGP_ppc64le_linux) && !defined(VGP_s390x_linux)
#  ifdef VKI_IPC_64
   struct vki_shmid64_ds buf;
     /*
      * On Linux, the following ABIs use old shmid_ds by default with direct
      * shmctl and require IPC_64 for shmid64_ds (i.e. the direct syscall is
      * mapped to sys_old_shmctl):
      *    alpha, arm, microblaze, mips n32/n64, xtensa
      * Other Linux ABIs use shmid64_ds by default and do not recognize IPC_64
      * with the direct shmctl syscall (but still recognize it for the
      * ipc-multiplexed version if that exists for the ABI).
      */
#    if defined(VGO_linux) && !defined(VGP_arm_linux) && !defined(VGP_mips64_linux)
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
#elif defined(__NR_shmsys) /* Solaris */
   struct vki_shmid_ds buf;
   SysRes __res = VG_(do_syscall4)(__NR_shmsys, VKI_SHMCTL, shmid, VKI_IPC_STAT,
                         (UWord)&buf);
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
         VG_(discard_translations)( (Addr)res, 
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
         VG_(discard_translations)( s_start,
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
#   if defined(VGO_freebsd)
      PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_shmid_ds) );
#   else
      PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_shminfo) );
#   endif
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
#   if defined(VGO_freebsd)
      POST_MEM_WRITE( arg2, sizeof(struct vki_shmid_ds) );
#   else
      POST_MEM_WRITE( arg2, sizeof(struct vki_shminfo) );
#   endif
      break;
#if defined(VKI_IPC_64)
   case VKI_IPC_INFO|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shminfo64) );
      break;
#endif
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

#  if defined(VGO_darwin)
   // Nb: we can't use this on Darwin, it has races:
   // * needs to RETRY if advisory succeeds but map fails  
   //   (could have been some other thread in a nonblocking call)
   // * needs to not use fixed-position mmap() on Darwin
   //   (mmap will cheerfully smash whatever's already there, which might 
   //   be a new mapping from some other thread in a nonblocking call)
   VG_(core_panic)("can't use ML_(generic_PRE_sys_mmap) on Darwin");
#  endif

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
#if defined(VKI_MAP_ALIGN) /* Solaris specific */
   if (arg4 & VKI_MAP_ALIGN) {
      mreq.rkind = MAlign;
      if (mreq.start == 0) {
         mreq.start = VKI_PAGE_SIZE;
      }
      /* VKI_MAP_FIXED and VKI_MAP_ALIGN don't like each other. */
      arg4 &= ~VKI_MAP_ALIGN;
   } else
#endif
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

#  if defined(VKI_MAP_32BIT)
   /* MAP_32BIT is royally unportable, so if the client asks for it, try our
      best to make it work (but without complexifying aspacemgr).
      If the user requested MAP_32BIT, the mmap-ed space must be in the
      first 2GB of the address space. So, return ENOMEM if aspacemgr
      advisory is above the first 2GB. If MAP_FIXED is also requested,
      MAP_32BIT has to be ignored.
      Assumption about aspacemgr behaviour: aspacemgr scans the address space
      from low addresses to find a free segment. No special effort is done
      to keep the first 2GB 'free' for this MAP_32BIT. So, this will often
      fail once the program has already allocated significant memory. */
   if ((arg4 & VKI_MAP_32BIT) && !(arg4 & VKI_MAP_FIXED)) {
      if (advised + arg2 >= 0x80000000)
         return VG_(mk_SysRes_Error)( VKI_ENOMEM );
   }
#  endif

   /* Otherwise we're OK (so far).  Install aspacem's choice of
      address, and let the mmap go through.  */
   sres = VG_(am_do_mmap_NO_NOTIFY)(advised, arg2, arg3,
                                    arg4 | VKI_MAP_FIXED,
                                    arg5, arg6);

#  if defined(VKI_MAP_32BIT)
   /* No recovery trial if the advisory was not accepted. */
   if ((arg4 & VKI_MAP_32BIT) && !(arg4 & VKI_MAP_FIXED)
       && sr_isError(sres)) {
      return VG_(mk_SysRes_Error)( VKI_ENOMEM );
   }
#  endif

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

   /* Yet another refinement : sometimes valgrind chooses an address
      which is not acceptable by the kernel. This at least happens
      when mmap-ing huge pages, using the flag MAP_HUGETLB.
      valgrind aspacem does not know about huge pages, and modifying
      it to handle huge pages is not straightforward (e.g. need
      to understand special file system mount options).
      So, let's just redo an mmap, without giving any constraint to
      the kernel. If that succeeds, check with aspacem that the returned
      address is acceptable.
      This will give a similar effect as if the user would have
      hinted that address.
      The aspacem state will be correctly updated afterwards.
      We however cannot do this last refinement when the user asked
      for a fixed mapping, as the user asked a specific address. */
   if (sr_isError(sres) && !(arg4 & VKI_MAP_FIXED)) {
      advised = 0; 
      /* try mmap with NULL address and without VKI_MAP_FIXED
         to let the kernel decide. */
      sres = VG_(am_do_mmap_NO_NOTIFY)(advised, arg2, arg3,
                                       arg4,
                                       arg5, arg6);
      if (!sr_isError(sres)) {
         /* The kernel is supposed to know what it is doing, but let's
            do a last sanity check anyway, as if the chosen address had
            been initially hinted by the client. The whole point of this
            last try was to allow mmap of huge pages to succeed without
            making aspacem understand them, on the other hand the kernel
            does not know about valgrind reservations, so this mapping
            can end up in free space and reservations. */
         mreq.start = (Addr)sr_Res(sres);
         mreq.len   = arg2;
         mreq.rkind = MHint;
         advised = VG_(am_get_advisory)( &mreq, True/*client*/, &mreq_ok );
         vg_assert(mreq_ok && advised == mreq.start);
      }
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

PRE(sys_exit)
{
   ThreadState* tst;
   /* simple; just make this thread exit */
   PRINT("exit( %ld )", SARG1);
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
   PRINT("sys_iopl ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "iopl", unsigned long, level);
}

PRE(sys_fsync)
{
   *flags |= SfMayBlock;
   PRINT("sys_fsync ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "fsync", unsigned int, fd);
}

PRE(sys_fdatasync)
{
   *flags |= SfMayBlock;
   PRINT("sys_fdatasync ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "fdatasync", unsigned int, fd);
}

PRE(sys_msync)
{
   *flags |= SfMayBlock;
   PRINT("sys_msync ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#"
                      FMT_REGWORD "x )", ARG1, ARG2, ARG3);
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
   PRINT("sys_getpmsg ( %ld, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#"
                        FMT_REGWORD "x, %#" FMT_REGWORD "x )", SARG1,
                        ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(int, "getpmsg",
                 int, fd, struct strbuf *, ctrl, struct strbuf *, data, 
                 int *, bandp, int *, flagsp);
   ctrl = (struct vki_pmsg_strbuf *)(Addr)ARG2;
   data = (struct vki_pmsg_strbuf *)(Addr)ARG3;
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
   ctrl = (struct vki_pmsg_strbuf *)(Addr)ARG2;
   data = (struct vki_pmsg_strbuf *)(Addr)ARG3;
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
   PRINT("sys_putpmsg ( %ld, %#" FMT_REGWORD "x, %#" FMT_REGWORD
                        "x, %ld, %ld )", SARG1, ARG2, ARG3, SARG4, SARG5);
   PRE_REG_READ5(int, "putpmsg",
                 int, fd, struct strbuf *, ctrl, struct strbuf *, data, 
                 int, band, int, flags);
   ctrl = (struct vki_pmsg_strbuf *)(Addr)ARG2;
   data = (struct vki_pmsg_strbuf *)(Addr)ARG3;
   if (ctrl && ctrl->len > 0)
      PRE_MEM_READ( "putpmsg(ctrl)", (Addr)ctrl->buf, ctrl->len);
   if (data && data->len > 0)
      PRE_MEM_READ( "putpmsg(data)", (Addr)data->buf, data->len);
}

PRE(sys_getitimer)
{
   struct vki_itimerval *value = (struct vki_itimerval*)(Addr)ARG2;
   PRINT("sys_getitimer ( %ld, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(long, "getitimer", int, which, struct itimerval *, value);

   PRE_timeval_WRITE( "getitimer(&value->it_interval)", &(value->it_interval));
   PRE_timeval_WRITE( "getitimer(&value->it_value)",    &(value->it_value));
}

POST(sys_getitimer)
{
   if (ARG2 != (Addr)NULL) {
      struct vki_itimerval *value = (struct vki_itimerval*)(Addr)ARG2;
      POST_timeval_WRITE( &(value->it_interval) );
      POST_timeval_WRITE( &(value->it_value) );
   }
}

PRE(sys_setitimer)
{
   PRINT("sys_setitimer ( %ld, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
                          SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setitimer", 
                 int, which,
                 struct itimerval *, value, struct itimerval *, ovalue);
   if (ARG2 != (Addr)NULL) {
      struct vki_itimerval *value = (struct vki_itimerval*)(Addr)ARG2;
      PRE_timeval_READ( "setitimer(&value->it_interval)",
                         &(value->it_interval));
      PRE_timeval_READ( "setitimer(&value->it_value)",
                         &(value->it_value));
   }
   if (ARG3 != (Addr)NULL) {
      struct vki_itimerval *ovalue = (struct vki_itimerval*)(Addr)ARG3;
      PRE_timeval_WRITE( "setitimer(&ovalue->it_interval)",
                         &(ovalue->it_interval));
      PRE_timeval_WRITE( "setitimer(&ovalue->it_value)",
                         &(ovalue->it_value));
   }
}

POST(sys_setitimer)
{
   if (ARG3 != (Addr)NULL) {
      struct vki_itimerval *ovalue = (struct vki_itimerval*)(Addr)ARG3;
      POST_timeval_WRITE( &(ovalue->it_interval) );
      POST_timeval_WRITE( &(ovalue->it_value) );
   }
}

PRE(sys_chroot)
{
   PRINT("sys_chroot ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "chroot", const char *, path);
   PRE_MEM_RASCIIZ( "chroot(path)", ARG1 );
}

PRE(sys_madvise)
{
   *flags |= SfMayBlock;
   PRINT("sys_madvise ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %ld )",
                        ARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "madvise",
                 unsigned long, start, vki_size_t, length, int, advice);
}

#if HAVE_MREMAP
PRE(sys_mremap)
{
   // Nb: this is different to the glibc version described in the man pages,
   // which lacks the fifth 'new_address' argument.
   if (ARG4 & VKI_MREMAP_FIXED) {
      PRINT("sys_mremap ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %"
            FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
            ARG1, ARG2, ARG3, ARG4, ARG5);
      PRE_REG_READ5(unsigned long, "mremap",
                    unsigned long, old_addr, unsigned long, old_size,
                    unsigned long, new_size, unsigned long, flags,
                    unsigned long, new_addr);
   } else {
      PRINT("sys_mremap ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %"
            FMT_REGWORD "u, 0x%" FMT_REGWORD "x )",
            ARG1, ARG2, ARG3, ARG4);
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
   PRINT("sys_nice ( %ld )", SARG1);
   PRE_REG_READ1(long, "nice", int, inc);
}

PRE(sys_mlock2)
{
   *flags |= SfMayBlock;
   PRINT("sys_mlock2 ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ2(int, "mlock2", void*, addr, vki_size_t, len);
}

PRE(sys_mlock)
{
   *flags |= SfMayBlock;
   PRINT("sys_mlock ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(long, "mlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_munlock)
{
   *flags |= SfMayBlock;
   PRINT("sys_munlock ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(long, "munlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_mlockall)
{
   *flags |= SfMayBlock;
   PRINT("sys_mlockall ( %" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "mlockall", int, flags);
}

PRE(sys_setpriority)
{
   PRINT("sys_setpriority ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(long, "setpriority", int, which, int, who, int, prio);
}

PRE(sys_getpriority)
{
   PRINT("sys_getpriority ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "getpriority", int, which, int, who);
}

#if !defined(VGO_freebsd)
PRE(sys_pwrite64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_pwrite64 ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "u, %lld )", ARG1, ARG2, ARG3, (Long)MERGE64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pwrite64",
                 unsigned int, fd, const char *, buf, vki_size_t, count,
                 vki_u32, MERGE64_FIRST(offset), vki_u32, MERGE64_SECOND(offset));
#elif VG_WORDSIZE == 8
   PRINT("sys_pwrite64 ( %lu, %#lx, %lu, %ld )",
         ARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(ssize_t, "pwrite64",
                 unsigned int, fd, const char *, buf, vki_size_t, count,
                 Word, offset);
#else
#  error Unexpected word size
#endif
   PRE_MEM_READ( "pwrite64(buf)", ARG2, ARG3 );
}
#endif

PRE(sys_sync)
{
   *flags |= SfMayBlock;
   PRINT("sys_sync ( )");
   PRE_REG_READ0(long, "sync");
}

#if !defined(VGP_nanomips_linux)
PRE(sys_fstatfs)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fstatfs ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2);
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
   PRINT("sys_fstatfs64 ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#"
         FMT_REGWORD "x )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "fstatfs64",
                 unsigned int, fd, vki_size_t, size, struct statfs64 *, buf);
   PRE_MEM_WRITE( "fstatfs64(buf)", ARG3, ARG2 );
}
POST(sys_fstatfs64)
{
   POST_MEM_WRITE( ARG3, ARG2 );
}
#endif

PRE(sys_getsid)
{
   PRINT("sys_getsid ( %ld )", SARG1);
   PRE_REG_READ1(long, "getsid", vki_pid_t, pid);
}

#if !defined(VGO_freebsd)
PRE(sys_pread64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_pread64 ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "u, %lld )", ARG1, ARG2, ARG3, (Long)MERGE64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pread64",
                 unsigned int, fd, char *, buf, vki_size_t, count,
                 vki_u32, MERGE64_FIRST(offset), vki_u32, MERGE64_SECOND(offset));
#elif VG_WORDSIZE == 8
   PRINT("sys_pread64 ( %lu, %#lx, %lu, %ld )",
         ARG1, ARG2, ARG3, SARG4);
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
#endif

PRE(sys_mknod)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_mknod ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %#"
         FMT_REGWORD "x )", ARG1, (HChar*)(Addr)ARG1, ARG2, ARG3 );
   PRE_REG_READ3(long, "mknod",
                 const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknod(pathname)", ARG1 );
}

PRE(sys_flock)
{
   *flags |= SfMayBlock;
   PRINT("sys_flock ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2 );
   PRE_REG_READ2(long, "flock", unsigned int, fd, unsigned int, operation);
}

// Pre_read a char** argument.
void ML_(pre_argv_envp)(Addr a, ThreadId tid, const HChar *s1, const HChar *s2)
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

/* This handles the common part of the PRE macro for execve and execveat. */
void handle_pre_sys_execve(ThreadId tid, SyscallStatus *status, Addr pathname,
                           Addr arg_2, Addr arg_3, ExecveType execveType,
                           Bool check_pathptr)
{
   HChar*       path = NULL;       /* path to executable */
   HChar**      envp = NULL;
   HChar**      argv = NULL;
   HChar**      arg2copy;
   HChar*       launcher_basename = NULL;
   ThreadState* tst;
   Int          i, j, tot_args;
   SysRes       res;
   Bool         setuid_allowed, trace_this_child;
   const char   *str;
   char         str2[30], str3[30];
   Addr         arg_2_check = arg_2;

   switch (execveType) {
   case EXECVE:
      str = "execve";
      break;
   case EXECVEAT:
      str = "execveat";
      break;
   case FEXECVE:
      str = "fexecve";
      break;
   default:
      vg_assert(False);
   }

   VG_(strcpy)(str2, str);
   VG_(strcpy)(str3, str);

   VG_(strcat)(str2, "(argv)");
   VG_(strcat)(str3, "(argv[0])");

   /* argv[] should not be NULL and valid.  */
   PRE_MEM_READ(str2, arg_2_check, sizeof(Addr));

   /* argv[0] should not be NULL and valid.  */
   if (ML_(safe_to_deref)((HChar **) (Addr)arg_2_check, sizeof(HChar *))) {
      Addr argv0 = *(Addr*)arg_2_check;
      PRE_MEM_RASCIIZ( str3, argv0 );
      /* The rest of argv can be NULL or a valid string pointer.  */
      if (VG_(am_is_valid_for_client)(arg_2_check, sizeof(HChar), VKI_PROT_READ)) {
         arg_2_check += sizeof(HChar*);
         str3[VG_(strlen)(str)] = '\0';
         VG_(strcat)(str3, "(argv[i])");
         ML_(pre_argv_envp)( arg_2_check, tid, str2, str3 );
      }
   } else {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }
   // Reset helper strings to syscall name.
   str2[VG_(strlen)(str)] = '\0';
   str3[VG_(strlen)(str)] = '\0';
   if (arg_3 != 0) {
      /* At least the terminating NULL must be addressable. */
      if (!ML_(safe_to_deref)((HChar **) (Addr)arg_3, sizeof(HChar *))) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }
      VG_(strcat)(str2, "(envp)");
      VG_(strcat)(str3, "(envp[i])");
      ML_(pre_argv_envp)( arg_3, tid, str2, str3 );
   }

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Erk.  If the exec fails, then the following will have made a
      mess of things which makes it hard for us to continue.  The
      right thing to do is piece everything together again in
      POST(execve), but that's close to impossible.  Instead, we make
      an effort to check that the execve will work before actually
      doing it. */

   /* Check that the name at least begins in client-accessible storage.
      If we didn't create it ourselves in execveat. */
   if (check_pathptr
       && !VG_(am_is_valid_for_client)( pathname, 1, VKI_PROT_READ )) {
       SET_STATUS_Failure( VKI_EFAULT );
       return;
   }

   // debug-only printing
   if (0) {
      VG_(printf)("pathname = %p(%s)\n", (void*)(Addr)pathname, (HChar*)(Addr)pathname);
      if (arg_2) {
         VG_(printf)("arg_2 = ");
         Int q;
         HChar** vec = (HChar**)(Addr)arg_2;
         for (q = 0; vec[q]; q++)
            VG_(printf)("%p(%s) ", vec[q], vec[q]);
         VG_(printf)("\n");
      } else {
         VG_(printf)("arg_2 = null\n");
      }
   }

   // Decide whether or not we want to follow along
   { // Make 'child_argv' be a pointer to the child's arg vector
     // (skipping the exe name)
     const HChar** child_argv = (const HChar**)(Addr)arg_2;
     if (child_argv && child_argv[0] == NULL)
        child_argv = NULL;
     trace_this_child = VG_(should_we_trace_this_child)( (HChar*)(Addr)pathname,
                                                          child_argv );
   }

   // Do the important checks:  it is a file, is executable, permissions are
   // ok, etc.  We allow setuid executables to run only in the case when
   // we are not simulating them, that is, they to be run natively.
   setuid_allowed = trace_this_child  ? False  : True;
   res = VG_(pre_exec_check)((const HChar *)(Addr)pathname, NULL, setuid_allowed);
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
   VG_(debugLog)(1, "syswrap", "Exec of %s\n", (HChar*)(Addr)pathname);

   
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
      path = (HChar*)(Addr)pathname;
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
   if (arg_3 == 0) {
      envp = NULL;
   } else {
      envp = VG_(env_clone)( (HChar**)(Addr)arg_3 );
      if (envp == NULL) goto hosed;
      VG_(env_remove_valgrind_env_stuff)( envp, True /*ro_strings*/, NULL );
   }

   if (trace_this_child) {
      // Set VALGRIND_LIB in arg_3 (the environment)
      VG_(env_setenv)( &envp, VALGRIND_LIB, VG_(libdir));
   }

   // Set up the child's args.  If not tracing it, they are
   // simply arg_2.  Otherwise, they are
   //
   // [launcher_basename] ++ VG_(args_for_valgrind) ++ [pathname] ++ arg_2[1..]
   //
   // except that the first VG_(args_for_valgrind_noexecpass) args
   // are omitted.
   //
   if (!trace_this_child) {
      argv = (HChar**)(Addr)arg_2;
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
      arg2copy = (HChar**)(Addr)arg_2;
      if (arg2copy && arg2copy[0]) {
         for (i = 1; arg2copy[i]; i++)
            tot_args++;
      }
      // allocate
      argv = VG_(malloc)( "di.syswrap.pre_sys_execve.1",
                          (tot_args+1) * sizeof(HChar*) );
      // copy
      j = 0;
      argv[j++] = launcher_basename;
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         if (i < VG_(args_for_valgrind_noexecpass))
            continue;
         argv[j++] = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      }
      argv[j++] = (HChar*)(Addr)pathname;
      if (arg2copy && arg2copy[0])
         for (i = 1; arg2copy[i]; i++)
            argv[j++] = arg2copy[i];
      argv[j++] = NULL;
      // check
      vg_assert(j == tot_args+1);
   }

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
      HChar **cpp;
      VG_(printf)("exec: %s\n", path);
      for (cpp = argv; cpp && *cpp; cpp++)
         VG_(printf)("argv: %s\n", *cpp);
      if (0)
         for (cpp = envp; cpp && *cpp; cpp++)
            VG_(printf)("env: %s\n", *cpp);
   }

   // always execute this because it's executing valgrind, not the "target" exe
   SET_STATUS_from_SysRes( 
      VG_(do_syscall3)(__NR_execve, (UWord)path, (UWord)argv, (UWord)envp));

   /* If we got here, then the execve failed.  We've already made way
      too much of a mess to continue, so we have to abort. */
  hosed:
   vg_assert(FAILURE);
   VG_(message)(Vg_UserMsg, "execve(%#" FMT_REGWORD "x(%s), %#" FMT_REGWORD
                "x, %#" FMT_REGWORD "x) failed, errno %lu\n",
                pathname, (HChar*)(Addr)pathname, arg_2, arg_3, ERR);
   VG_(message)(Vg_UserMsg, "EXEC FAILED: I can't recover from "
                            "execve() failing, so I'm dying.\n");
   VG_(message)(Vg_UserMsg, "Add more stringent tests in PRE(sys_execve), "
                            "or work out how to recover.\n");
   VG_(exit)(101);

}

// XXX: prototype here seemingly doesn't match the prototype for i386-linux,
// but it seems to work nonetheless...
PRE(sys_execve)
{
   PRINT("sys_execve ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %#"
         FMT_REGWORD "x )", ARG1, (HChar*)(Addr)ARG1, ARG2, ARG3);
   PRE_REG_READ3(vki_off_t, "execve",
                 char *, filename, char **, argv, char **, envp);
   PRE_MEM_RASCIIZ( "execve(filename)", ARG1 );

   char *pathname = (char *)ARG1;
   Addr arg_2 = (Addr)ARG2;
   Addr arg_3 = (Addr)ARG3;

   handle_pre_sys_execve(tid, status, (Addr)pathname, arg_2, arg_3, EXECVE, True);
}

PRE(sys_access)
{
   PRINT("sys_access ( %#" FMT_REGWORD "x(%s), %ld )", ARG1,
         (HChar*)(Addr)ARG1, SARG2);
   PRE_REG_READ2(long, "access", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "access(pathname)", ARG1 );
}

PRE(sys_alarm)
{
   PRINT("sys_alarm ( %" FMT_REGWORD "u )", ARG1);
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
   PRINT("sys_brk ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(unsigned long, "brk", unsigned long, end_data_segment);

   brk_new = do_brk(ARG1, tid);
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
   PRINT("sys_chdir ( %#" FMT_REGWORD "x(%s) )", ARG1,(char*)(Addr)ARG1);
   PRE_REG_READ1(long, "chdir", const char *, path);
   PRE_MEM_RASCIIZ( "chdir(path)", ARG1 );
}

PRE(sys_chmod)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_chmod ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,
         (HChar*)(Addr)ARG1, ARG2);
   PRE_REG_READ2(long, "chmod", const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "chmod(path)", ARG1 );
}

PRE(sys_chown)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_chown ( %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x, 0x%"
         FMT_REGWORD "x )", ARG1,(char*)(Addr)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "chown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "chown(path)", ARG1 );
}

PRE(sys_lchown)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_lchown ( %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x, 0x%"
         FMT_REGWORD "x )", ARG1,(char*)(Addr)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "lchown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "lchown(path)", ARG1 );
}

PRE(sys_close)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_close ( %" FMT_REGWORD "u )", ARG1);
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
   if (VG_(clo_track_fds)) ML_(record_fd_close)(ARG1);
}

PRE(sys_dup)
{
   PRINT("sys_dup ( %" FMT_REGWORD "u )", ARG1);
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
   PRINT("sys_dup2 ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2);
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
   PRINT("sys_fchdir ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "fchdir", unsigned int, fd);
}

PRE(sys_fchown)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fchown ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %"
         FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "fchown",
                 unsigned int, fd, vki_uid_t, owner, vki_gid_t, group);
}

PRE(sys_fchmod)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_fchmod ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(long, "fchmod", unsigned int, fildes, vki_mode_t, mode);
}

#if !defined(VGP_nanomips_linux) && !defined (VGO_freebsd)
PRE(sys_newfstat)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_newfstat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2);
   PRE_REG_READ2(long, "fstat", unsigned int, fd, struct stat *, buf);
   PRE_MEM_WRITE( "fstat(buf)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_newfstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}
#endif

#if !defined(VGO_solaris) && !defined(VGP_arm64_linux) && \
    !defined(VGP_nanomips_linux)
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

   VG_(do_atfork_pre)(tid);

   SET_STATUS_from_SysRes( VG_(do_syscall0)(__NR_fork) );

   if (!SUCCESS) return;

#if defined(VGO_linux) || defined(VGO_freebsd)
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

   if (is_child) {
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } else {
      VG_(do_atfork_parent)(tid);

      PRINT("   fork: process %d created child %d\n", VG_(getpid)(), child_pid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }
}
#endif // !defined(VGO_solaris) && !defined(VGP_arm64_linux)

PRE(sys_ftruncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_ftruncate ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(long, "ftruncate", unsigned int, fd, unsigned long, length);
}

PRE(sys_truncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_truncate ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )",
         ARG1, (HChar*)(Addr)ARG1, ARG2);
   PRE_REG_READ2(long, "truncate", 
                 const char *, path, unsigned long, length);
   PRE_MEM_RASCIIZ( "truncate(path)", ARG1 );
}

PRE(sys_ftruncate64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_ftruncate64 ( %" FMT_REGWORD "u, %llu )", ARG1,
         MERGE64(ARG2,ARG3));
   PRE_REG_READ3(long, "ftruncate64",
                 unsigned int, fd,
                 UWord, MERGE64_FIRST(length), UWord, MERGE64_SECOND(length));
#else
   PRINT("sys_ftruncate64 ( %lu, %lu )", ARG1, ARG2);
   PRE_REG_READ2(long, "ftruncate64",
                 unsigned int,fd, UWord,length);
#endif
}

PRE(sys_truncate64)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_truncate64 ( %#" FMT_REGWORD "x, %lld )", ARG1,
         (Long)MERGE64(ARG2, ARG3));
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
   PRINT("sys_getdents ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD
         "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getdents",
                 unsigned int, fd, struct vki_dirent *, dirp,
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
   PRINT("sys_getdents64 ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "u )",ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getdents64",
                 unsigned int, fd, struct vki_dirent64 *, dirp,
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
   PRINT("sys_getgroups ( %ld, %#" FMT_REGWORD "x )", SARG1, ARG2);
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
   PRINT("sys_getcwd ( %#" FMT_REGWORD "x, %llu )", ARG1,(ULong)ARG2);
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
   PRINT("sys_getpgid ( %ld )", SARG1);
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
   PRINT("sys_old_getrlimit ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",
         ARG1, ARG2);
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
   PRINT("sys_getrlimit ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2);
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
   PRINT("sys_getrusage ( %ld, %#" FMT_REGWORD "x )", SARG1, ARG2);
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
   PRINT("sys_gettimeofday ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         ARG1,ARG2);
   PRE_REG_READ2(long, "gettimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   // GrP fixme does darwin write to *tz anymore?
   if (ARG1 != 0)
      PRE_timeval_WRITE( "gettimeofday(tv)", (Addr)ARG1 );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "gettimeofday(tz)", ARG2, sizeof(struct vki_timezone) );
}

POST(sys_gettimeofday)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      if (ARG1 != 0)
         POST_timeval_WRITE( (Addr)ARG1 );
      if (ARG2 != 0)
	 POST_MEM_WRITE( ARG2, sizeof(struct vki_timezone) );
   }
}

PRE(sys_settimeofday)
{
   PRINT("sys_settimeofday ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         ARG1,ARG2);
   PRE_REG_READ2(long, "settimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   if (ARG1 != 0)
      PRE_timeval_READ( "settimeofday(tv)", (Addr)ARG1 );
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

#if defined(VGO_solaris)
   /* Majority of Solaris ioctl requests does not honour direction hints. */
   UInt dir  = _VKI_IOC_NONE;
#else   
   UInt dir  = _VKI_IOC_DIR(request);
#endif
   UInt size = _VKI_IOC_SIZE(request);

   if (SimHintiS(SimHint_lax_ioctls, VG_(clo_sim_hints))) {
      /* 
       * Be very lax about ioctl handling; the only
       * assumption is that the size is correct. Doesn't
       * require the full buffer to be initialized when
       * writing.  Without this, using some device
       * drivers with a large number of strange ioctl
       * commands becomes very tiresome.
       */
   } else if (dir == _VKI_IOC_NONE && size > 0) {
      static UWord unknown_ioctl[10];
      static Int moans = sizeof(unknown_ioctl) / sizeof(unknown_ioctl[0]);

      if (moans > 0 && !VG_(clo_xml)) {
         /* Check if have not already moaned for this request. */
         UInt i;
         for (i = 0; i < sizeof(unknown_ioctl)/sizeof(unknown_ioctl[0]); i++) {
            if (unknown_ioctl[i] == request)
               break;
            if (unknown_ioctl[i] == 0) {
               unknown_ioctl[i] = request;
               moans--;
               VG_(umsg)("Warning: noted but unhandled ioctl 0x%lx"
                         " with no direction hints.\n", request);
               VG_(umsg)("   This could cause spurious value errors to appear.\n");
               VG_(umsg)("   See README_MISSING_SYSCALL_OR_IOCTL for "
                         "guidance on writing a proper wrapper.\n" );
               //VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
               return;
            }
         }
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
       && arg != (Addr)NULL) {
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

   /* Fatal SIGKILL sent to one of our threads.
      "Handle" the signal ourselves, as trying to have tid
      handling the signal causes termination problems (see #409367
      and #409141).
      Moreover, as a process cannot do anything when receiving SIGKILL,
      it is not particularly crucial that "tid" does the work to
      terminate the process.  */

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "Thread %u %s being killed with SIGKILL, running tid: %u\n",
                   tst->tid, VG_(name_of_ThreadStatus) (tst->status), VG_(running_tid));

   if (!VG_(is_running_thread)(tid))
      tst = VG_(get_ThreadState)(VG_(running_tid));
   VG_(nuke_all_threads_except) (VG_(running_tid), VgSrc_FatalSig);
   VG_(reap_threads)(VG_(running_tid));
   tst->exitreason = VgSrc_FatalSig;
   tst->os_state.fatalsig = VKI_SIGKILL;

   return True;
}

PRE(sys_kill)
{
   PRINT("sys_kill ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "kill", int, pid, int, signal);
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
		   SARG2, SARG1);

   /* This kill might have given us a pending signal.  Ask for a check once 
      the syscall is done. */
   *flags |= SfPollAfter;
}

PRE(sys_link)
{
   *flags |= SfMayBlock;
   PRINT("sys_link ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x(%s) )", ARG1,
         (char*)(Addr)ARG1,ARG2,(char*)(Addr)ARG2);
   PRE_REG_READ2(long, "link", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "link(oldpath)", ARG1);
   PRE_MEM_RASCIIZ( "link(newpath)", ARG2);
}

#if !defined(VGP_nanomips_linux) && !defined(VGO_freebsd)
PRE(sys_newlstat)
{
   PRINT("sys_newlstat ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )", ARG1,
         (char*)(Addr)ARG1,ARG2);
   PRE_REG_READ2(long, "lstat", char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "lstat(file_name)", ARG1 );
   PRE_MEM_WRITE( "lstat(buf)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_newlstat)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}
#endif

PRE(sys_mkdir)
{
   *flags |= SfMayBlock;
   PRINT("sys_mkdir ( %#" FMT_REGWORD "x(%s), %ld )", ARG1,
         (HChar*)(Addr)ARG1, SARG2);
   PRE_REG_READ2(long, "mkdir", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "mkdir(pathname)", ARG1 );
}

PRE(sys_mprotect)
{
   PRINT("sys_mprotect ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %"
         FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "mprotect",
                 unsigned long, addr, vki_size_t, len, unsigned long, prot);

   Addr addr = ARG1;
   SizeT len = ARG2;
   Int prot  = ARG3;

   handle_sys_mprotect (tid, status, &addr, &len, &prot);

   ARG1 = addr;
   ARG2 = len;
   ARG3 = prot;
}
/* This will be called from the generic mprotect, or the linux specific
   pkey_mprotect. Pass pointers to ARG1, ARG2 and ARG3 as addr, len and prot,
   they might be adjusted and have to assigned back to ARG1, ARG2 and ARG3.  */
void handle_sys_mprotect(ThreadId tid, SyscallStatus* status,
                         Addr *addr, SizeT *len, Int *prot)
{
   if (!ML_(valid_client_addr)(*addr, *len, tid, "mprotect")) {
#if defined(VGO_freebsd)
      SET_STATUS_Failure( VKI_EINVAL );
#else
      SET_STATUS_Failure( VKI_ENOMEM );
#endif
   } 
#if defined(VKI_PROT_GROWSDOWN)
   else 
   if (*prot & (VKI_PROT_GROWSDOWN|VKI_PROT_GROWSUP)) {
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
      UInt grows = *prot & (VKI_PROT_GROWSDOWN|VKI_PROT_GROWSUP);
      NSegment const *aseg = VG_(am_find_nsegment)(*addr);
      NSegment const *rseg;

      vg_assert(aseg);

      if (grows == VKI_PROT_GROWSDOWN) {
         rseg = VG_(am_next_nsegment)( aseg, False/*backwards*/ );
         if (rseg
             && rseg->kind == SkResvn
             && rseg->smode == SmUpper
             && rseg->end+1 == aseg->start) {
            Addr end = *addr + *len;
            *addr = aseg->start;
            *len = end - aseg->start;
            *prot &= ~VKI_PROT_GROWSDOWN;
         } else {
            SET_STATUS_Failure( VKI_EINVAL );
         }
      } else if (grows == VKI_PROT_GROWSUP) {
         rseg = VG_(am_next_nsegment)( aseg, True/*forwards*/ );
         if (rseg 
             && rseg->kind == SkResvn
             && rseg->smode == SmLower
             && aseg->end+1 == rseg->start) {
            *len = aseg->end - *addr + 1;
            *prot &= ~VKI_PROT_GROWSUP;
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
   if (0) VG_(printf)("  munmap( %#" FMT_REGWORD "x )\n", ARG1);
   PRINT("sys_munmap ( %#" FMT_REGWORD "x, %llu )", ARG1,(ULong)ARG2);
   PRE_REG_READ2(long, "munmap", unsigned long, start, vki_size_t, length);

   if (!ML_(valid_client_addr)(ARG1, ARG2, tid, "munmap"))
      SET_STATUS_Failure( VKI_EINVAL );
}

POST(sys_munmap)
{
   Addr  a   = ARG1;
   SizeT len = ARG2;

   ML_(notify_core_and_tool_of_munmap)( a, len );
}

PRE(sys_mincore)
{
   PRINT("sys_mincore ( %#" FMT_REGWORD "x, %llu, %#" FMT_REGWORD "x )",
         ARG1, (ULong)ARG2, ARG3);
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
   PRINT("sys_nanosleep ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1,ARG2);
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

#if defined(VGO_linux) || defined(VGO_solaris)
/* Handles the case where the open is of /proc/self/auxv or
   /proc/<pid>/auxv, and just gives out a copy of the fd for the
   fake file we cooked up at startup (in m_main).  Also, seeks the
   cloned fd back to the start.
   Returns True if auxv open was handled (status is set). */
Bool ML_(handle_auxv_open)(SyscallStatus *status, const HChar *filename,
                           int flags)
{
   HChar  name[30];   // large enough

   if (!ML_(safe_to_deref)((const void *) filename, 1))
      return False;

   /* Opening /proc/<pid>/auxv or /proc/self/auxv? */
   VG_(sprintf)(name, "/proc/%d/auxv", VG_(getpid)());
   if (!VG_STREQ(filename, name) && !VG_STREQ(filename, "/proc/self/auxv"))
      return False;

   /* Allow to open the file only for reading. */
   if (flags & (VKI_O_WRONLY | VKI_O_RDWR)) {
      SET_STATUS_Failure(VKI_EACCES);
      return True;
   }

#  if defined(VGO_solaris)
   VG_(sprintf)(name, "/proc/self/fd/%d", VG_(cl_auxv_fd));
   SysRes sres = VG_(open)(name, flags, 0);
   SET_STATUS_from_SysRes(sres);
#  else
   SysRes sres = VG_(dup)(VG_(cl_auxv_fd));
   SET_STATUS_from_SysRes(sres);
   if (!sr_isError(sres)) {
      OffT off = VG_(lseek)(sr_Res(sres), 0, VKI_SEEK_SET);
      if (off < 0)
         SET_STATUS_Failure(VKI_EMFILE);
   }
#  endif

   return True;
}
#endif // defined(VGO_linux) || defined(VGO_solaris)

#if defined(VGO_linux)
Bool ML_(handle_self_exe_open)(SyscallStatus *status, const HChar *filename,
                               int flags)
{
   HChar  name[30];   // large enough for /proc/<int>/exe

   if (!ML_(safe_to_deref)((const void *) filename, 1))
      return False;

   /* Opening /proc/<pid>/exe or /proc/self/exe? */
   VG_(sprintf)(name, "/proc/%d/exe", VG_(getpid)());
   if (!VG_STREQ(filename, name) && !VG_STREQ(filename, "/proc/self/exe"))
      return False;

   /* Allow to open the file only for reading. */
   if (flags & (VKI_O_WRONLY | VKI_O_RDWR)) {
      SET_STATUS_Failure(VKI_EACCES);
      return True;
   }

   SysRes sres = VG_(dup)(VG_(cl_exec_fd));
   SET_STATUS_from_SysRes(sres);
   if (!sr_isError(sres)) {
      OffT off = VG_(lseek)(sr_Res(sres), 0, VKI_SEEK_SET);
      if (off < 0)
         SET_STATUS_Failure(VKI_EMFILE);
   }

   return True;
}
#endif // defined(VGO_linux)

PRE(sys_open)
{
   if (ARG2 & VKI_O_CREAT) {
      // 3-arg version
      PRINT("sys_open ( %#" FMT_REGWORD "x(%s), %ld, %ld )",ARG1,
            (HChar*)(Addr)ARG1, SARG2, SARG3);
      PRE_REG_READ3(long, "open",
                    const char *, filename, int, flags, int, mode);
   } else {
      // 2-arg version
      PRINT("sys_open ( %#" FMT_REGWORD "x(%s), %ld )",ARG1,
            (HChar*)(Addr)ARG1, SARG2);
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
      HChar  name[30];   // large enough
      HChar* arg1s = (HChar*) (Addr)ARG1;
      SysRes sres;

      VG_(sprintf)(name, "/proc/%d/cmdline", VG_(getpid)());
      if (ML_(safe_to_deref)( arg1s, 1 )
          && (VG_STREQ(arg1s, name) || VG_STREQ(arg1s, "/proc/self/cmdline"))) {
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

   /* Handle also the case of /proc/self/auxv or /proc/<pid>/auxv
      or /proc/self/exe or /proc/<pid>/exe. */
   if (ML_(handle_auxv_open)(status, (const HChar *)(Addr)ARG1, ARG2)
       || ML_(handle_self_exe_open)(status, (const HChar *)(Addr)ARG1, ARG2))
      return;
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
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)(Addr)ARG1);
   }
}

PRE(sys_read)
{
   *flags |= SfMayBlock;
   PRINT("sys_read ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(ssize_t, "read",
                 int, fd, char *, buf, vki_size_t, count);

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
   PRINT("sys_write ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(ssize_t, "write",
                 unsigned int, fd, const char *, buf, vki_size_t, count);
   /* check to see if it is allowed.  If not, try for an exemption from
      --sim-hints=enable-outer (used for self hosting). */
   ok = ML_(fd_allowed)(ARG1, "write", tid, False);
   if (!ok && ARG1 == 2/*stderr*/ 
           && SimHintiS(SimHint_enable_outer, VG_(clo_sim_hints)))
      ok = True;
#if defined(VGO_solaris)
   if (!ok && VG_(vfork_fildes_addr) != NULL
       && *VG_(vfork_fildes_addr) >= 0 && *VG_(vfork_fildes_addr) == ARG1)
      ok = True;
#endif
   if (!ok)
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_READ( "write(buf)", ARG2, ARG3 );
}

PRE(sys_creat)
{
   *flags |= SfMayBlock;
   PRINT("sys_creat ( %#" FMT_REGWORD "x(%s), %ld )", ARG1,
         (HChar*)(Addr)ARG1, SARG2);
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
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)(Addr)ARG1);
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
   struct vki_pollfd* ufds = (struct vki_pollfd *)(Addr)ARG1;
   *flags |= SfMayBlock;
   PRINT("sys_poll ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %ld )\n",
         ARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "poll",
                 struct vki_pollfd *, ufds, unsigned int, nfds, long, timeout);

   for (i = 0; i < ARG2; i++) {
      PRE_MEM_READ( "poll(ufds.fd)",
                    (Addr)(&ufds[i].fd), sizeof(ufds[i].fd) );
      if (ML_(safe_to_deref)(&ufds[i].fd, sizeof(ufds[i].fd)) && ufds[i].fd >= 0) {
         PRE_MEM_READ( "poll(ufds.events)",
                       (Addr)(&ufds[i].events), sizeof(ufds[i].events) );
      }
      PRE_MEM_WRITE( "poll(ufds.revents)",
                     (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }
}

POST(sys_poll)
{
   if (SUCCESS) {
      UInt i;
      struct vki_pollfd* ufds = (struct vki_pollfd *)(Addr)ARG1;
      for (i = 0; i < ARG2; i++)
	 POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }
}

PRE(sys_readlink)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   Word saved = SYSNO;

   PRINT("sys_readlink ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %llu )",
         ARG1, (char*)(Addr)ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(long, "readlink",
                 const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlink(path)", ARG1 );
   PRE_MEM_WRITE( "readlink(buf)", ARG2,ARG3 );


   {
#if defined(VGO_linux) || defined(VGO_solaris)
#if defined(VGO_linux)
#define PID_EXEPATH  "/proc/%d/exe"
#define SELF_EXEPATH "/proc/self/exe"
#define SELF_EXEFD   "/proc/self/fd/%d"
#elif defined(VGO_solaris)
#define PID_EXEPATH  "/proc/%d/path/a.out"
#define SELF_EXEPATH "/proc/self/path/a.out"
#define SELF_EXEFD   "/proc/self/path/%d"
#endif
      /*
       * Handle the case where readlink is looking at /proc/self/exe or
       * /proc/<pid>/exe, or equivalent on Solaris.
       */
      HChar  name[30];   // large enough
      HChar* arg1s = (HChar*) (Addr)ARG1;
      VG_(sprintf)(name, PID_EXEPATH, VG_(getpid)());
      if (ML_(safe_to_deref)(arg1s, 1)
          && (VG_STREQ(arg1s, name) || VG_STREQ(arg1s, SELF_EXEPATH))) {
         VG_(sprintf)(name, SELF_EXEFD, VG_(cl_exec_fd));
         SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, (UWord)name, 
                                                         ARG2, ARG3));
      } else
#endif
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
   char buf[sizeof("readv(vector[])") + 11];
   *flags |= SfMayBlock;
   PRINT("sys_readv ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(ssize_t, "readv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count);
   if (!ML_(fd_allowed)(ARG1, "readv", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if ((Int)ARG3 >= 0)
         PRE_MEM_READ( "readv(vector)", ARG2, ARG3 * sizeof(struct vki_iovec) );

      if (ML_(safe_to_deref)((const void*)ARG2, ARG3*sizeof(struct vki_iovec *))) {
         vec = (struct vki_iovec *)(Addr)ARG2;
         for (i = 0; i < (Int)ARG3; i++) {
            VG_(sprintf)(buf, "readv(vector[%d])", i);
            PRE_MEM_WRITE(buf, (Addr)vec[i].iov_base, vec[i].iov_len );
         }
      }
   }
}

POST(sys_readv)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      Int i;
      struct vki_iovec * vec = (struct vki_iovec *)(Addr)ARG2;
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
   PRINT("sys_rename ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x(%s) )", ARG1,
         (char*)(Addr)ARG1,ARG2,(char*)(Addr)ARG2);
   PRE_REG_READ2(long, "rename", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "rename(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "rename(newpath)", ARG2 );
}

PRE(sys_rmdir)
{
   *flags |= SfMayBlock;
   PRINT("sys_rmdir ( %#" FMT_REGWORD "x(%s) )", ARG1,(char*)(Addr)ARG1);
   PRE_REG_READ1(long, "rmdir", const char *, pathname);
   PRE_MEM_RASCIIZ( "rmdir(pathname)", ARG1 );
}

PRE(sys_select)
{
   *flags |= SfMayBlock;
   PRINT("sys_select ( %ld, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#"
         FMT_REGWORD "x, %#" FMT_REGWORD "x )", SARG1, ARG2, ARG3, ARG4, ARG5);
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
      PRE_timeval_READ( "select(timeout)", (Addr)ARG5 );
}

PRE(sys_setgid)
{
   PRINT("sys_setgid ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "setgid", vki_gid_t, gid);
}

PRE(sys_setsid)
{
   PRINT("sys_setsid ( )");
   PRE_REG_READ0(long, "setsid");
}

PRE(sys_setgroups)
{
   PRINT("setgroups ( %llu, %#" FMT_REGWORD "x )", (ULong)ARG1, ARG2);
   PRE_REG_READ2(long, "setgroups", int, size, vki_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_READ( "setgroups(list)", ARG2, ARG1 * sizeof(vki_gid_t) );
}

PRE(sys_setpgid)
{
   PRINT("setpgid ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "setpgid", vki_pid_t, pid, vki_pid_t, pgid);
}

PRE(sys_setregid)
{
   PRINT("sys_setregid ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(long, "setregid", vki_gid_t, rgid, vki_gid_t, egid);
}

PRE(sys_setreuid)
{
   PRINT("sys_setreuid ( 0x%" FMT_REGWORD "x, 0x%" FMT_REGWORD "x )",
         ARG1, ARG2);
   PRE_REG_READ2(long, "setreuid", vki_uid_t, ruid, vki_uid_t, euid);
}

PRE(sys_setrlimit)
{
   UWord arg1 = ARG1;
   PRINT("sys_setrlimit ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2);
   PRE_REG_READ2(long, "setrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_READ( "setrlimit(rlim)", ARG2, sizeof(struct vki_rlimit) );

#ifdef _RLIMIT_POSIX_FLAG
   // Darwin will sometimes set _RLIMIT_POSIX_FLAG on setrlimit calls.
   // Unset it here to make the if statements below work correctly.
   arg1 &= ~_RLIMIT_POSIX_FLAG;
#endif

   if (!VG_(am_is_valid_for_client)(ARG2, sizeof(struct vki_rlimit), 
                                    VKI_PROT_READ)) {
      SET_STATUS_Failure( VKI_EFAULT );
   }
   else if (((struct vki_rlimit *)(Addr)ARG2)->rlim_cur
            > ((struct vki_rlimit *)(Addr)ARG2)->rlim_max) {
#if defined(VGO_freebsd)
      SET_STATUS_Failure( VKI_EPERM );
#else
      SET_STATUS_Failure( VKI_EINVAL );
#endif
   }
   else if (arg1 == VKI_RLIMIT_NOFILE) {
      if (((struct vki_rlimit *)(Addr)ARG2)->rlim_cur > VG_(fd_hard_limit) ||
          ((struct vki_rlimit *)(Addr)ARG2)->rlim_max != VG_(fd_hard_limit)) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         VG_(fd_soft_limit) = ((struct vki_rlimit *)(Addr)ARG2)->rlim_cur;
         SET_STATUS_Success( 0 );
      }
   }
   else if (arg1 == VKI_RLIMIT_DATA) {
      if (((struct vki_rlimit *)(Addr)ARG2)->rlim_cur
           > VG_(client_rlimit_data).rlim_max ||
          ((struct vki_rlimit *)(Addr)ARG2)->rlim_max
           > VG_(client_rlimit_data).rlim_max) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         VG_(client_rlimit_data) = *(struct vki_rlimit *)(Addr)ARG2;
         SET_STATUS_Success( 0 );
      }
   }
   else if (arg1 == VKI_RLIMIT_STACK && tid == 1) {
      if (((struct vki_rlimit *)(Addr)ARG2)->rlim_cur
           > VG_(client_rlimit_stack).rlim_max ||
          ((struct vki_rlimit *)(Addr)ARG2)->rlim_max
           > VG_(client_rlimit_stack).rlim_max) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         /* Change the value of client_stack_szB to the rlim_cur value but
            only if it is smaller than the size of the allocated stack for the
            client.
            TODO: All platforms should set VG_(clstk_max_size) as part of their
                  setup_client_stack(). */
         if ((VG_(clstk_max_size) == 0)
             || (((struct vki_rlimit *) (Addr)ARG2)->rlim_cur <= VG_(clstk_max_size)))
            VG_(threads)[tid].client_stack_szB = ((struct vki_rlimit *)(Addr)ARG2)->rlim_cur;

         VG_(client_rlimit_stack) = *(struct vki_rlimit *)(Addr)ARG2;
         SET_STATUS_Success( 0 );
      }
   }
}

PRE(sys_setuid)
{
   PRINT("sys_setuid ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "setuid", vki_uid_t, uid);
}

#if !defined(VGP_nanomips_linux) && !defined(VGO_freebsd)
PRE(sys_newstat)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_newstat ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",
         ARG1,(char*)(Addr)ARG1,ARG2);
   PRE_REG_READ2(long, "stat", char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "stat(file_name)", ARG1 );
   PRE_MEM_WRITE( "stat(buf)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_newstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}
#endif

#if !defined(VGP_nanomips_linux)
PRE(sys_statfs)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_statfs ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",
         ARG1, (char*)(Addr)ARG1, ARG2);
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
   PRINT("sys_statfs64 ( %#" FMT_REGWORD "x(%s), %llu, %#" FMT_REGWORD "x )",
         ARG1, (char*)(Addr)ARG1, (ULong)ARG2, ARG3);
   PRE_REG_READ3(long, "statfs64",
                 const char *, path, vki_size_t, size, struct statfs64 *, buf);
   PRE_MEM_RASCIIZ( "statfs64(path)", ARG1 );
   PRE_MEM_WRITE( "statfs64(buf)", ARG3, ARG2 );
}
POST(sys_statfs64)
{
   POST_MEM_WRITE( ARG3, ARG2 );
}
#endif

PRE(sys_symlink)
{
   *flags |= SfMayBlock;
   PRINT("sys_symlink ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x(%s) )",
         ARG1, (char*)(Addr)ARG1, ARG2, (char*)(Addr)ARG2);
   PRE_REG_READ2(long, "symlink", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "symlink(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "symlink(newpath)", ARG2 );
}

PRE(sys_time)
{
   /* time_t time(time_t *t); */
   PRINT("sys_time ( %#" FMT_REGWORD "x )",ARG1);
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
   PRINT("sys_times ( %#" FMT_REGWORD "x )", ARG1);
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
   PRINT("sys_umask ( %ld )", SARG1);
   PRE_REG_READ1(long, "umask", int, mask);
}

PRE(sys_unlink)
{
   *flags |= SfMayBlock;
   PRINT("sys_unlink ( %#" FMT_REGWORD "x(%s) )", ARG1,(char*)(Addr)ARG1);
   PRE_REG_READ1(long, "unlink", const char *, pathname);
   PRE_MEM_RASCIIZ( "unlink(pathname)", ARG1 );
}

#if !defined(VGO_freebsd)
PRE(sys_newuname)
{
   PRINT("sys_newuname ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "uname", struct new_utsname *, buf);
   PRE_MEM_WRITE( "uname(buf)", ARG1, sizeof(struct vki_new_utsname) );
}

POST(sys_newuname)
{
   if (ARG1 != 0) {
      POST_MEM_WRITE( ARG1, sizeof(struct vki_new_utsname) );
   }
}
#endif

PRE(sys_waitpid)
{
   *flags |= SfMayBlock;
   PRINT("sys_waitpid ( %ld, %#" FMT_REGWORD "x, %ld )", SARG1, ARG2, SARG3);
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
   PRINT("sys_wait4 ( %ld, %#" FMT_REGWORD "x, %ld, %#" FMT_REGWORD "x )",
         SARG1, ARG2, SARG3, ARG4);

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
   char buf[sizeof("writev(vector[])") + 11];
   *flags |= SfMayBlock;
   PRINT("sys_writev ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(ssize_t, "writev",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count);
   if (!ML_(fd_allowed)(ARG1, "writev", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if ((Int)ARG3 >= 0)
         PRE_MEM_READ( "writev(vector)", 
                       ARG2, ARG3 * sizeof(struct vki_iovec) );

      if (ML_(safe_to_deref)((const void*)ARG2, ARG3*sizeof(struct vki_iovec *))) {
         vec = (struct vki_iovec *)(Addr)ARG2;
         for (i = 0; i < (Int)ARG3; i++) {
            VG_(sprintf)(buf, "writev(vector[%d])", i);
            PRE_MEM_READ( buf, (Addr)vec[i].iov_base, vec[i].iov_len );
         }
      }
   }
}

PRE(sys_utimes)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_utimes ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",
         ARG1, (char*)(Addr)ARG1, ARG2);
   PRE_REG_READ2(long, "utimes", char *, filename, struct timeval *, tvp);
   PRE_MEM_RASCIIZ( "utimes(filename)", ARG1 );
   if (ARG2 != 0) {
      PRE_timeval_READ( "utimes(tvp[0])", (Addr)ARG2 );
      PRE_timeval_READ( "utimes(tvp[1])",
                        (Addr)ARG2+sizeof(struct vki_timeval) );
   }
}

PRE(sys_acct)
{
   PRINT("sys_acct ( %#" FMT_REGWORD "x(%s) )", ARG1,(char*)(Addr)ARG1);
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
   PRINT("sigaltstack ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "sigaltstack",
                 const vki_stack_t *, ss, vki_stack_t *, oss);
   if (ARG1 != 0) {
      const vki_stack_t *ss = (vki_stack_t *)(Addr)ARG1;
      PRE_MEM_READ( "sigaltstack(ss)", (Addr)&ss->ss_sp, sizeof(ss->ss_sp) );
      PRE_MEM_READ( "sigaltstack(ss)", (Addr)&ss->ss_flags, sizeof(ss->ss_flags) );
      PRE_MEM_READ( "sigaltstack(ss)", (Addr)&ss->ss_size, sizeof(ss->ss_size) );
   }
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "sigaltstack(oss)", ARG2, sizeof(vki_stack_t) );
   }

   /* Be safe. */
   if (ARG1 && !ML_(safe_to_deref((void*)(Addr)ARG1, sizeof(vki_stack_t)))) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }
   if (ARG2 && !ML_(safe_to_deref((void*)(Addr)ARG2, sizeof(vki_stack_t)))) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }

   SET_STATUS_from_SysRes( 
      VG_(do_sys_sigaltstack) (tid, (vki_stack_t*)(Addr)ARG1,
                              (vki_stack_t*)(Addr)ARG2)
   );
}
POST(sys_sigaltstack)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG2 != 0)
      POST_MEM_WRITE( ARG2, sizeof(vki_stack_t));
}

PRE(sys_sethostname)
{
   PRINT("sys_sethostname ( %#" FMT_REGWORD "x, %ld )", ARG1, SARG2);
   PRE_REG_READ2(long, "sethostname", char *, name, int, len);
   PRE_MEM_READ( "sethostname(name)", ARG1, ARG2 );
}

#undef PRE
#undef POST

#endif // defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
