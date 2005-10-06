
/*--------------------------------------------------------------------*/
/*--- Wrappers for generic Unix system calls     syswrap-generic.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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
#include "pub_core_threadstate.h"
#include "pub_core_debuginfo.h"     // VG_(di_notify_*)
#include "pub_core_aspacemgr.h"
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "pub_core_clientstate.h"   // VG_(brk_base), VG_(brk_limit)
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"

#include "vki_unistd.h"              /* for the __NR_* constants */


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
      VG_(printf)("%s: test=%p-%p ret=%d\n",
		  syscallname, start, start+size-1, (Int)ret);

   if (!ret && syscallname != NULL) {
      VG_(message)(Vg_UserMsg, "Warning: client syscall %s tried "
                               "to modify addresses %p-%p",
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
   return VG_(am_is_valid_for_client)( (Addr)start, size, VKI_PROT_NONE );
}


/* ---------------------------------------------------------------------
   Doing mmap, mremap
   ------------------------------------------------------------------ */

// Nb: this isn't done as precisely as possible, but it seems that programs
// are usually sufficiently well-behaved that the more obscure corner cases
// aren't important.  Various comments in the few functions below give more
// details... njn 2002-Sep-17

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

/* When a client mmap has been successfully done, this function must
   be called.  It notifies both aspacem and the tool of the new
   mapping.
*/
void 
ML_(notify_aspacem_and_tool_of_mmap) ( Addr a, SizeT len, UInt prot, 
                                       UInt flags, Int fd, Off64T offset )
{
   Bool rr, ww, xx, d;

   /* 'a' is the return value from a real kernel mmap, hence: */
   vg_assert(VG_IS_PAGE_ALIGNED(a));
   /* whereas len is whatever the syscall supplied.  So: */
   len = VG_PGROUNDUP(len);

   d = VG_(am_notify_client_mmap)( a, len, prot, flags, fd, offset );

   rr = toBool(prot & VKI_PROT_READ);
   ww = toBool(prot & VKI_PROT_WRITE);
   xx = toBool(prot & VKI_PROT_EXEC);

   VG_TRACK( new_mem_mmap, a, len, rr, ww, xx );

   if (d)
      VG_(discard_translations)( (Addr64)a, (ULong)len,
                                 "ML_(notify_aspacem_and_tool_of_mmap)" );
}

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
   NSegment* old_seg;
   Addr      advised;
   Bool      f_fixed   = toBool(flags & VKI_MREMAP_FIXED);
   Bool      f_maymove = toBool(flags & VKI_MREMAP_MAYMOVE);

   if (0)
      VG_(printf)("do_remap (old %p %d) (new %p %d) %s %s\n",
                  old_addr,old_len,new_addr,new_len, 
                  flags & VKI_MREMAP_MAYMOVE ? "MAYMOVE" : "",
                  flags & VKI_MREMAP_FIXED ? "FIXED" : "");

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
   if (old_addr + old_len < old_addr
       || new_addr + new_len < new_len)
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
   if (old_seg->kind != SkAnonC && old_seg->kind != SkAnonV)
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
                      old_seg->hasR, old_seg->hasW, old_seg->hasX );
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
   if (ok && advised == needA) {
      ok = VG_(am_extend_map_client)( &d, old_seg, needL );
      if (ok) {
         VG_TRACK( new_mem_mmap, needA, needL, 
                                 old_seg->hasR, 
                                 old_seg->hasW, old_seg->hasX );
         if (d) 
            VG_(discard_translations)( needA, needL, "do_remap(3)" );
         return VG_(mk_SysRes_Success)( old_addr );
      }
   }

   /* that failed.  Look elsewhere. */
   advised = VG_(am_get_advisory_client_simple)( 0, new_len, &ok );
   if (ok) {
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
                      old_seg->hasR, old_seg->hasW, old_seg->hasX );
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
   if (!ok || advised != needA)
      goto eNOMEM;
   ok = VG_(am_extend_map_client)( &d, old_seg, needL );
   if (!ok)
      goto eNOMEM;
   VG_TRACK( new_mem_mmap, needA, needL, 
                           old_seg->hasR, old_seg->hasW, old_seg->hasX );
   if (d)
      VG_(discard_translations)( needA, needL, "do_remap(6)" );
   return VG_(mk_SysRes_Success)( old_addr );
   }
   /*NOTREACHED*/ vg_assert(0);

  shrink_in_place:
   {
   SysRes sres = VG_(am_munmap_client)( &d, old_addr+new_len, old_len-new_len );
   if (sres.isError)
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
static OpenFd *allocated_fds;

/* Count of open file descriptors. */
static Int fd_count = 0;


/* Note the fact that a file descriptor was just closed. */
static
void record_fd_close(ThreadId tid, Int fd)
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
      i = VG_(arena_malloc)(VG_AR_CORE, sizeof(OpenFd));

      i->prev = NULL;
      i->next = allocated_fds;
      if(allocated_fds) allocated_fds->prev = i;
      allocated_fds = i;
      fd_count++;
   }

   i->fd = fd;
   i->pathname = VG_(arena_strdup)(VG_AR_CORE, pathname);
   i->where = (tid == -1) ? NULL : VG_(record_ExeContext)(tid);
}

// Record opening of an fd, and find its name.
static void record_fd_open_named(ThreadId tid, Int fd)
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
      UInt addr = sa->sin_addr.s_addr;

      if (addr == 0) {
         VG_(sprintf)(name, "<unbound>");
      } else {
         VG_(sprintf)(name, "%u.%u.%u.%u:%u",
                      addr & 0xFF, (addr>>8) & 0xFF,
                      (addr>>16) & 0xFF, (addr>>24) & 0xFF,
                      vki_ntohs(sa->sin_port));
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

         if(VG_(getpeername)(fd, (struct vki_sockaddr *)&paddr, &plen) != -1) {
            VG_(message)(Vg_UserMsg, "Open AF_INET socket %d: %s <-> %s", fd,
                         inet2name(&(laddr.in), llen, lname),
                         inet2name(&paddr, plen, pname));
         } else {
            VG_(message)(Vg_UserMsg, "Open AF_INET socket %d: %s <-> unbound",
                         fd, inet2name(&(laddr.in), llen, lname));
         }
         return;
         }
      case VKI_AF_UNIX: {
         static char lname[256];
         VG_(message)(Vg_UserMsg, "Open AF_UNIX socket %d: %s", fd,
                      unix2name(&(laddr.un), llen, lname));
         return;
         }
      default:
         VG_(message)(Vg_UserMsg, "Open pf-%d socket %d:",
                      laddr.a.sa_family, fd);
         return;
      }
   }

   VG_(message)(Vg_UserMsg, "Open socket %d:", fd);
}


/* Dump out a summary, and a more detailed list, of open file descriptors. */
void VG_(show_open_fds) ()
{
   OpenFd *i = allocated_fds;

   VG_(message)(Vg_UserMsg, "FILE DESCRIPTORS: %d open at exit.", fd_count);

   while(i) {
      if(i->pathname) {
         VG_(message)(Vg_UserMsg, "Open file descriptor %d: %s", i->fd,
                      i->pathname);
      } else {
         Int val;
         UInt len = sizeof(val);

         if (VG_(getsockopt)(i->fd, VKI_SOL_SOCKET, VKI_SO_TYPE, &val, &len) == -1) {
            VG_(message)(Vg_UserMsg, "Open file descriptor %d:", i->fd);
         } else {
            getsockdetails(i->fd);
         }
      }

      if(i->where) {
         VG_(pp_ExeContext)(i->where);
         VG_(message)(Vg_UserMsg, "");
      } else {
         VG_(message)(Vg_UserMsg, "   <inherited from parent>");
         VG_(message)(Vg_UserMsg, "");
      }

      i = i->next;
   }

   VG_(message)(Vg_UserMsg, "");
}

/* If /proc/self/fd doesn't exist for some weird reason (like you've
   got a kernel that doesn't have /proc support compiled in), then we
   need to find out what file descriptors we inherited from our parent
   process the hard way - by checking each fd in turn. */

static
void do_hacky_preopened()
{
   struct vki_rlimit lim;
   UInt count;
   Int i;

   if (VG_(getrlimit) (VKI_RLIMIT_NOFILE, &lim) == -1) {
      /* Hmm.  getrlimit() failed.  Now we're screwed, so just choose
         an arbitrarily high number.  1024 happens to be the limit in
         the 2.4 kernels. */
      count = 1024;
   } else {
      count = lim.rlim_cur;
   }

   for (i = 0; i < count; i++)
      if(VG_(fcntl)(i, VKI_F_GETFL, 0) != -1)
         ML_(record_fd_open_nameless)(-1, i);
}

/* Initialize the list of open file descriptors with the file descriptors
   we inherited from out parent process. */

void VG_(init_preopened_fds)()
{
   Int ret;
   struct vki_dirent d;
   SysRes f;

   f = VG_(open)("/proc/self/fd", VKI_O_RDONLY, 0);
   if (f.isError) {
      do_hacky_preopened();
      return;
   }

   while ((ret = VG_(getdents)(f.val, &d, sizeof(d))) != 0) {
      if (ret == -1)
         goto out;

      if (VG_(strcmp)(d.d_name, ".") && VG_(strcmp)(d.d_name, "..")) {
         Int fno = VG_(atoll)(d.d_name);

         if (fno != f.val)
            if (VG_(clo_track_fds))
               record_fd_open_named(-1, fno);
      }

      VG_(lseek)(f.val, d.d_off, VKI_SEEK_SET);
   }

  out:
   VG_(close)(f.val);
}

static
Char *strdupcat ( const Char *s1, const Char *s2, ArenaId aid )
{
   UInt len = VG_(strlen) ( s1 ) + VG_(strlen) ( s2 ) + 1;
   Char *result = VG_(arena_malloc) ( aid, len );
   VG_(strcpy) ( result, s1 );
   VG_(strcat) ( result, s2 );
   return result;
}

static 
void pre_mem_read_sendmsg ( ThreadId tid, Bool read,
                            Char *msg, Addr base, SizeT size )
{
   Char *outmsg = strdupcat ( "socketcall.sendmsg", msg, VG_AR_CORE );
   PRE_MEM_READ( outmsg, base, size );
   VG_(arena_free) ( VG_AR_CORE, outmsg );
}

static 
void pre_mem_write_recvmsg ( ThreadId tid, Bool read,
                             Char *msg, Addr base, SizeT size )
{
   Char *outmsg = strdupcat ( "socketcall.recvmsg", msg, VG_AR_CORE );
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
        struct vki_msghdr *msg, 
        void (*foreach_func)( ThreadId, Bool, Char *, Addr, SizeT ) 
     )
{
   if ( !msg )
      return;

   foreach_func ( tid, True, "(msg)", (Addr)&msg->msg_name, sizeof( msg->msg_name ) );
   foreach_func ( tid, True, "(msg)", (Addr)&msg->msg_namelen, sizeof( msg->msg_namelen ) );
   foreach_func ( tid, True, "(msg)", (Addr)&msg->msg_iov, sizeof( msg->msg_iov ) );
   foreach_func ( tid, True, "(msg)", (Addr)&msg->msg_iovlen, sizeof( msg->msg_iovlen ) );
   foreach_func ( tid, True, "(msg)", (Addr)&msg->msg_control, sizeof( msg->msg_control ) );
   foreach_func ( tid, True, "(msg)", (Addr)&msg->msg_controllen, sizeof( msg->msg_controllen ) );
   foreach_func ( tid, True, "(msg)", (Addr)&msg->msg_flags, sizeof( msg->msg_flags ) );

   if ( msg->msg_name )
      foreach_func ( tid, False,
                     "(msg.msg_name)", 
                     (Addr)msg->msg_name, msg->msg_namelen );

   if ( msg->msg_iov ) {
      struct vki_iovec *iov = msg->msg_iov;
      UInt i;

      foreach_func ( tid, True,
                     "(msg.msg_iov)", 
                     (Addr)iov, msg->msg_iovlen * sizeof( struct vki_iovec ) );

      for ( i = 0; i < msg->msg_iovlen; ++i, ++iov )
         foreach_func ( tid, False,
                        "(msg.msg_iov[i]", 
                        (Addr)iov->iov_base, iov->iov_len );
   }

   if ( msg->msg_control )
      foreach_func ( tid, False,
                     "(msg.msg_control)", 
                     (Addr)msg->msg_control, msg->msg_controllen );
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
               record_fd_open_named(tid, fds[i]);
      }

      cm = VKI_CMSG_NXTHDR(msg, cm);
   }
}

static
void pre_mem_read_sockaddr ( ThreadId tid,
                             Char *description,
                             struct vki_sockaddr *sa, UInt salen )
{
   Char *outmsg;

   /* NULL/zero-length sockaddrs are legal */
   if ( sa == NULL || salen == 0 ) return;

   outmsg = VG_(arena_malloc) ( VG_AR_CORE,
                                VG_(strlen)( description ) + 30 );

   VG_(sprintf) ( outmsg, description, ".sa_family" );
   PRE_MEM_READ( outmsg, (Addr) &sa->sa_family, sizeof(vki_sa_family_t));

   switch (sa->sa_family) {
                  
      case VKI_AF_UNIX:
         VG_(sprintf) ( outmsg, description, ".sun_path" );
         PRE_MEM_RASCIIZ( outmsg,
            (Addr) ((struct vki_sockaddr_un *) sa)->sun_path);
         break;
                     
      case VKI_AF_INET:
         VG_(sprintf) ( outmsg, description, ".sin_port" );
         PRE_MEM_READ( outmsg,
            (Addr) &((struct vki_sockaddr_in *) sa)->sin_port,
            sizeof (((struct vki_sockaddr_in *) sa)->sin_port));
         VG_(sprintf) ( outmsg, description, ".sin_addr" );
         PRE_MEM_READ( outmsg,
            (Addr) &((struct vki_sockaddr_in *) sa)->sin_addr,
            sizeof (struct vki_in_addr));
         break;
                           
      case VKI_AF_INET6:
         VG_(sprintf) ( outmsg, description, ".sin6_port" );
         PRE_MEM_READ( outmsg,
            (Addr) &((struct vki_sockaddr_in6 *) sa)->sin6_port,
            sizeof (((struct vki_sockaddr_in6 *) sa)->sin6_port));
         VG_(sprintf) ( outmsg, description, ".sin6_flowinfo" );
         PRE_MEM_READ( outmsg,
            (Addr) &((struct vki_sockaddr_in6 *) sa)->sin6_flowinfo,
            sizeof (__vki_u32));
         VG_(sprintf) ( outmsg, description, ".sin6_addr" );
         PRE_MEM_READ( outmsg,
            (Addr) &((struct vki_sockaddr_in6 *) sa)->sin6_addr,
            sizeof (struct vki_in6_addr));
         VG_(sprintf) ( outmsg, description, ".sin6_scope_id" );
         PRE_MEM_READ( outmsg,
            (Addr) &((struct vki_sockaddr_in6 *) sa)->sin6_scope_id,
			sizeof (__vki_u32));
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

static 
void buf_and_len_pre_check( ThreadId tid, Addr buf_p, Addr buflen_p,
                            Char* buf_s, Char* buflen_s )
{
   if (VG_(tdict).track_pre_mem_write) {
      UInt buflen_in = deref_UInt( tid, buflen_p, buflen_s);
      if (buflen_in > 0) {
         VG_(tdict).track_pre_mem_write( Vg_CoreSysCall, tid, buf_s, buf_p, buflen_in );
      }
   }
}

static 
void buf_and_len_post_check( ThreadId tid, SysRes res,
                             Addr buf_p, Addr buflen_p, Char* s )
{
   if (!res.isError && VG_(tdict).track_post_mem_write) {
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
   NSegment *aseg, *rseg;
   Addr newbrkP;
   SizeT delta;
   Bool ok;
   Bool debug = False;

   if (debug)
      VG_(printf)("\ndo_brk: brk_base=%p brk_limit=%p newbrk=%p\n",
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
      NSegment* seg = VG_(am_find_nsegment)(newbrk);
      if (seg && seg->hasT)
         VG_(discard_translations)( newbrk, VG_(brk_limit) - newbrk, 
                                    "do_brk(shrink)" );
      VG_(brk_limit) = newbrk;
      return newbrk;
   }

   /* otherwise we're expanding the brk segment. */
   if (VG_(brk_limit) > VG_(brk_base))
      aseg = VG_(am_find_nsegment)( VG_(brk_limit)-1 );
   else
      aseg = VG_(am_find_nsegment)( VG_(brk_limit) );
   rseg = VG_(am_next_nsegment)( aseg, True/*forwards*/ );

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

   if (newbrk >= rseg->end+1 - VKI_PAGE_SIZE) {
      /* request is too large -- the resvn would fall below 1 page,
         which isn't allowed. */
      goto bad;
   }

   newbrkP = VG_PGROUNDUP(newbrk);
   vg_assert(newbrkP > rseg->start && newbrkP < rseg->end+1 - VKI_PAGE_SIZE);
   delta = newbrkP - rseg->start;
   vg_assert(delta > 0);
   vg_assert(VG_IS_PAGE_ALIGNED(delta));
   
   ok = VG_(am_extend_into_adjacent_reservation_client)( aseg, delta );
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

   /* hijacking the logging fd is never allowed */
   if (fd == VG_(clo_log_fd))
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
         "Warning: invalid file descriptor %d in syscall %s()",
         fd, syscallname);
      if (fd == VG_(clo_log_fd))
	 VG_(message)(Vg_UserMsg, 
            "   Use --log-fd=<number> to select an alternative log fd.");
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
   vg_assert(!res.isError); /* guaranteed by caller */
   Int fd1 = ((Int*)arg3)[0];
   Int fd2 = ((Int*)arg3)[1];
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
   vg_assert(!res.isError); /* guaranteed by caller */
   if (!ML_(fd_allowed)(res.val, "socket", tid, True)) {
      VG_(close)(res.val);
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless)(tid, res.val);
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
      buf_and_len_pre_check ( tid, addr_p, addrlen_p,
                              "socketcall.accept(addr)",
                              "socketcall.accept(addrlen_in)" );
}

SysRes 
ML_(generic_POST_sys_accept) ( ThreadId tid,
                               SysRes res,
                               UWord arg0, UWord arg1, UWord arg2 )
{
   SysRes r = res;
   vg_assert(!res.isError); /* guaranteed by caller */
   if (!ML_(fd_allowed)(res.val, "accept", tid, True)) {
      VG_(close)(res.val);
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      Addr addr_p     = arg1;
      Addr addrlen_p  = arg2;
      if (addr_p != (Addr)NULL) 
         buf_and_len_post_check ( tid, res, addr_p, addrlen_p,
                                  "socketcall.accept(addrlen_out)" );
      if (VG_(clo_track_fds))
          ML_(record_fd_open_nameless)(tid, res.val);
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
      buf_and_len_pre_check ( tid, from_p, fromlen_p, 
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

   vg_assert(!res.isError); /* guaranteed by caller */
   if (from_p != (Addr)NULL) 
      buf_and_len_post_check ( tid, res, from_p, fromlen_p,
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
   PRE_MEM_READ( "socketcall.connect(serv_addr.sa_family)",
                 arg1, /* serv_addr */
                 sizeof(vki_sa_family_t));
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
ML_(generic_PRE_sys_getsockopt) ( ThreadId tid, 
                                  UWord arg0, UWord arg1, UWord arg2,
                                  UWord arg3, UWord arg4 )
{
   /* int getsockopt(int s, int level, int optname, 
                     void *optval, socklen_t *optlen); */
   Addr optval_p  = arg3;
   Addr optlen_p  = arg4;
   /* vg_assert(sizeof(socklen_t) == sizeof(UInt)); */
   if (optval_p != (Addr)NULL) {
      buf_and_len_pre_check ( tid, optval_p, optlen_p,
                              "socketcall.getsockopt(optval)",
                              "socketcall.getsockopt(optlen)" );
      if (arg1 == VKI_SOL_SCTP &&
          (arg2 == VKI_SCTP_GET_PEER_ADDRS || arg2 == VKI_SCTP_GET_LOCAL_ADDRS)) {
         struct vki_sctp_getaddrs *ga = (struct vki_sctp_getaddrs*)arg3;
         int address_bytes = sizeof(struct vki_sockaddr_in6) * ga->addr_num;
         PRE_MEM_WRITE( "socketcall.getsockopt(optval.addrs)", (Addr)ga->addrs, address_bytes );
      }
   }
}

void 
ML_(generic_POST_sys_getsockopt) ( ThreadId tid,
                                   SysRes res,
                                   UWord arg0, UWord arg1, UWord arg2,
                                   UWord arg3, UWord arg4 )
{
   Addr optval_p  = arg3;
   Addr optlen_p  = arg4;
   vg_assert(!res.isError); /* guaranteed by caller */
   if (optval_p != (Addr)NULL) {
      buf_and_len_post_check ( tid, res, optval_p, optlen_p,
                               "socketcall.getsockopt(optlen_out)" );
      if (arg1 == VKI_SOL_SCTP &&
          (arg2 == VKI_SCTP_GET_PEER_ADDRS || arg2 == VKI_SCTP_GET_LOCAL_ADDRS)) {
         struct vki_sctp_getaddrs *ga = (struct vki_sctp_getaddrs*)arg3;
         struct vki_sockaddr *a = ga->addrs;
         int i;
         for (i = 0; i < ga->addr_num; i++) {
            int sl = 0;
            if (a->sa_family == VKI_AF_INET)
               sl = sizeof(struct vki_sockaddr_in);
            else if (a->sa_family == VKI_AF_INET6)
               sl = sizeof(struct vki_sockaddr_in6);
            else {
               VG_(message)(Vg_UserMsg, "Warning: getsockopt: unhandled address type %d", a->sa_family);
            }
            a = (struct vki_sockaddr*)((char*)a + sl);
         }
         POST_MEM_WRITE( (Addr)ga->addrs, (char*)a - (char*)ga->addrs );
      }
   }
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
   buf_and_len_pre_check ( tid, name_p, namelen_p,
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
   vg_assert(!res.isError); /* guaranteed by caller */
   buf_and_len_post_check ( tid, res, name_p, namelen_p,
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
   buf_and_len_pre_check ( tid, name_p, namelen_p,
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
   vg_assert(!res.isError); /* guaranteed by caller */
   buf_and_len_post_check ( tid, res, name_p, namelen_p,
                            "socketcall.getpeername(namelen_out)" );
}

/* ------ */

void 
ML_(generic_PRE_sys_sendmsg) ( ThreadId tid,
                               UWord arg0, UWord arg1 )
{
   /* int sendmsg(int s, const struct msghdr *msg, int flags); */
   struct vki_msghdr *msg = (struct vki_msghdr *)arg1;
   msghdr_foreachfield ( tid, msg, pre_mem_read_sendmsg );
}

/* ------ */

void
ML_(generic_PRE_sys_recvmsg) ( ThreadId tid,
                               UWord arg0, UWord arg1 )
{
   /* int recvmsg(int s, struct msghdr *msg, int flags); */
   struct vki_msghdr *msg = (struct vki_msghdr *)arg1;
   msghdr_foreachfield ( tid, msg, pre_mem_write_recvmsg );
}

void 
ML_(generic_POST_sys_recvmsg) ( ThreadId tid,
                                UWord arg0, UWord arg1 )
{
   struct vki_msghdr *msg = (struct vki_msghdr *)arg1;
   msghdr_foreachfield( tid, msg, post_mem_write_recvmsg );
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

   arg.buf = &buf;

#  ifdef __NR_semctl
   res = VG_(do_syscall4)(__NR_semctl, semid, 0, VKI_IPC_STAT, *(UWord *)&arg);
#  else
   res = VG_(do_syscall5)(__NR_ipc, 3 /* IPCOP_semctl */, semid, 0,
                          VKI_IPC_STAT, (UWord)&arg);
# endif
   if (res.isError)
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
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_SEM_INFO|VKI_IPC_64:
      PRE_MEM_WRITE( "semctl(IPC_INFO, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_seminfo) );
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
      PRE_MEM_WRITE( "semctl(IPC_STAT, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_semid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_SEM_STAT|VKI_IPC_64:
      PRE_MEM_WRITE( "semctl(IPC_STAT, arg.buf)",
                     (Addr)arg.buf, sizeof(struct vki_semid64_ds) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "semctl(IPC_SET, arg.buf)",
                    (Addr)arg.buf, sizeof(struct vki_semid_ds) );
      break;
   case VKI_IPC_SET|VKI_IPC_64:
      PRE_MEM_READ( "semctl(IPC_SET, arg.buf)",
                    (Addr)arg.buf, sizeof(struct vki_semid64_ds) );
      break;
   case VKI_GETALL:
   case VKI_GETALL|VKI_IPC_64:
      nsems = get_sem_count( arg0 );
      PRE_MEM_WRITE( "semctl(IPC_GETALL, arg.array)",
                     (Addr)arg.array, sizeof(unsigned short) * nsems );
      break;
   case VKI_SETALL:
   case VKI_SETALL|VKI_IPC_64:
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
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_SEM_INFO|VKI_IPC_64:
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_seminfo) );
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_semid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_SEM_STAT|VKI_IPC_64:
      POST_MEM_WRITE( (Addr)arg.buf, sizeof(struct vki_semid64_ds) );
      break;
   case VKI_GETALL:
   case VKI_GETALL|VKI_IPC_64:
      nsems = get_sem_count( arg0 );
      POST_MEM_WRITE( (Addr)arg.array, sizeof(unsigned short) * nsems );
      break;
   }
}

/* ------ */

/* ------ */

static
UInt get_shm_size ( Int shmid )
{
#  ifdef __NR_shmctl
   struct vki_shmid64_ds buf;
   SysRes __res = VG_(do_syscall3)(__NR_shmctl, shmid, VKI_IPC_STAT, (UWord)&buf);
#  else
   struct vki_shmid_ds buf;
   SysRes __res = VG_(do_syscall5)(__NR_ipc, 24 /* IPCOP_shmctl */, shmid,
                                 VKI_IPC_STAT, 0, (UWord)&buf);
#  endif
   if (__res.isError)
      return 0;
 
   return buf.shm_segsz;
}

UWord
ML_(generic_PRE_sys_shmat) ( ThreadId tid,
                             UWord arg0, UWord arg1, UWord arg2 )
{
   /* void *shmat(int shmid, const void *shmaddr, int shmflg); */
   UInt  segmentSize = get_shm_size ( arg0 );
   UWord tmp;
   Bool  ok;
   if (arg1 == 0) {
      tmp = VG_(am_get_advisory_client_simple)(0, segmentSize, &ok);
      if (ok)
         arg1 = tmp;
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
   UInt segmentSize = get_shm_size ( arg0 );
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
      d = VG_(am_notify_client_shmat)( res, VG_PGROUNDUP(segmentSize), prot );

      /* we don't distinguish whether it's read-only or
       * read-write -- it doesn't matter really. */
      VG_TRACK( new_mem_mmap, res, segmentSize, True, True, False );
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
   NSegment* s = VG_(am_find_nsegment)(arg0);

   if (s != NULL) {
      Addr  s_start = s->start;
      SizeT s_len   = s->end+1 - s->start;
      Bool  d;

      vg_assert(s->kind == SkShmC && s->start == arg0);

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
   case VKI_IPC_INFO:
      PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_shminfo) );
      break;
   case VKI_IPC_INFO|VKI_IPC_64:
      PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_shminfo64) );
      break;
   case VKI_SHM_INFO:
   case VKI_SHM_INFO|VKI_IPC_64:
      PRE_MEM_WRITE( "shmctl(SHM_INFO, buf)",
                     arg2, sizeof(struct vki_shm_info) );
      break;
   case VKI_IPC_STAT:
   case VKI_SHM_STAT:
      PRE_MEM_WRITE( "shmctl(IPC_STAT, buf)",
                     arg2, sizeof(struct vki_shmid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_SHM_STAT|VKI_IPC_64:
      PRE_MEM_WRITE( "shmctl(IPC_STAT, arg.buf)",
                     arg2, sizeof(struct vki_shmid64_ds) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "shmctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_shmid_ds) );
      break;
   case VKI_IPC_SET|VKI_IPC_64:
      PRE_MEM_READ( "shmctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_shmid64_ds) );
      break;
   }
}

void
ML_(generic_POST_sys_shmctl) ( ThreadId tid,
                               UWord res,
                               UWord arg0, UWord arg1, UWord arg2 )
{
   switch (arg1 /* cmd */) {
   case VKI_IPC_INFO:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shminfo) );
      break;
   case VKI_IPC_INFO|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shminfo64) );
      break;
   case VKI_SHM_INFO:
   case VKI_SHM_INFO|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shm_info) );
      break;
   case VKI_IPC_STAT:
   case VKI_SHM_STAT:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shmid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_SHM_STAT|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_shmid64_ds) );
      break;
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

   if (!sres.isError) {
      /* Notify aspacem and the tool. */
      ML_(notify_aspacem_and_tool_of_mmap)( 
         (Addr)sres.val, /* addr kernel actually assigned */
         arg2, arg3, 
         arg4, /* the original flags value */
         arg5, arg6 
      );
      /* Load symbols? */
      VG_(di_notify_mmap)( (Addr)sres.val );
   }

   /* Stay sane */
   if (!sres.isError && (arg4 & VKI_MAP_FIXED))
      vg_assert(sres.val == arg1);

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

// Combine two 32-bit values into a 64-bit value
#define LOHI64(lo,hi)   ( (lo) | ((ULong)(hi) << 32) )

//zz //PRE(sys_exit_group, Special)
//zz //{
//zz //   VG_(core_panic)("syscall exit_group() not caught by the scheduler?!");
//zz //}

PRE(sys_exit)
{
   ThreadState* tst;
   /* simple; just make this thread exit */
   PRINT("exit( %d )", ARG1);
   PRE_REG_READ1(void, "exit", int, exitcode);
   tst = VG_(get_ThreadState)(tid);
   /* Set the thread's status to be exiting, then claim that the
      syscall succeeded. */
   tst->exitreason = VgSrc_ExitSyscall;
   tst->os_state.exitcode = ARG1;
   SET_STATUS_Success(0);
}

PRE(sys_ni_syscall)
{
   PRINT("non-existent syscall! (ni_syscall)");
   PRE_REG_READ0(long, "ni_syscall");
   SET_STATUS_Failure( VKI_ENOSYS );
}

PRE(sys_iopl)
{
   PRINT("sys_iopl ( %d )", ARG1);
   PRE_REG_READ1(long, "iopl", unsigned long, level);
}

// XXX: this wrapper is only suitable for 32-bit platforms
#if defined(VGP_x86_linux)
PRE(sys_lookup_dcookie)
{
   PRINT("sys_lookup_dcookie (0x%llx, %p, %d)", LOHI64(ARG1,ARG2), ARG3, ARG4);
   PRE_REG_READ4(long, "lookup_dcookie",
                 vki_u32, cookie_low32, vki_u32, cookie_high32,
                 char *, buf, vki_size_t, len);
   PRE_MEM_WRITE( "lookup_dcookie(buf)", ARG3, ARG4);
}
POST(sys_lookup_dcookie)
{
   vg_assert(SUCCESS);
   if (ARG3 != (Addr)NULL)
      POST_MEM_WRITE( ARG3, RES);
}
#endif

PRE(sys_fsync)
{
   *flags |= SfMayBlock;
   PRINT("sys_fsync ( %d )", ARG1);
   PRE_REG_READ1(long, "fsync", unsigned int, fd);
}

PRE(sys_fdatasync)
{
   *flags |= SfMayBlock;
   PRINT("sys_fdatasync ( %d )", ARG1);
   PRE_REG_READ1(long, "fdatasync", unsigned int, fd);
}

PRE(sys_msync)
{
   *flags |= SfMayBlock;
   PRINT("sys_msync ( %p, %llu, %d )", ARG1,(ULong)ARG2,ARG3);
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
   PRINT("sys_getpmsg ( %d, %p, %p, %p, %p )", ARG1,ARG2,ARG3,ARG4,ARG5);
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
   PRINT("sys_putpmsg ( %d, %p, %p, %d, %d )", ARG1,ARG2,ARG3,ARG4,ARG5);
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
   PRINT("sys_getitimer ( %d, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "getitimer", int, which, struct itimerval *, value);
   PRE_MEM_WRITE( "getitimer(value)", ARG2, sizeof(struct vki_itimerval) );
}
POST(sys_getitimer)
{
   if (ARG2 != (Addr)NULL) {
      POST_MEM_WRITE(ARG2, sizeof(struct vki_itimerval));
   }
}

PRE(sys_setitimer)
{
   PRINT("sys_setitimer ( %d, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "setitimer", 
                 int, which,
                 struct itimerval *, value, struct itimerval *, ovalue);
   if (ARG2 != (Addr)NULL)
      PRE_MEM_READ( "setitimer(value)", ARG2, sizeof(struct vki_itimerval) );
   if (ARG3 != (Addr)NULL)
      PRE_MEM_WRITE( "setitimer(ovalue)", ARG3, sizeof(struct vki_itimerval));
}

POST(sys_setitimer)
{
   if (ARG3 != (Addr)NULL) {
      POST_MEM_WRITE(ARG3, sizeof(struct vki_itimerval));
   }
}

PRE(sys_chroot)
{
   PRINT("sys_chroot ( %p )", ARG1);
   PRE_REG_READ1(long, "chroot", const char *, path);
   PRE_MEM_RASCIIZ( "chroot(path)", ARG1 );
}

PRE(sys_madvise)
{
   *flags |= SfMayBlock;
   PRINT("sys_madvise ( %p, %llu, %d )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "madvise",
                 unsigned long, start, vki_size_t, length, int, advice);
}

PRE(sys_mremap)
{
   // Nb: this is different to the glibc version described in the man pages,
   // which lacks the fifth 'new_address' argument.
   PRINT("sys_mremap ( %p, %llu, %d, 0x%x, %p )", 
         ARG1, (ULong)ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(unsigned long, "mremap",
                 unsigned long, old_addr, unsigned long, old_size,
                 unsigned long, new_size, unsigned long, flags,
                 unsigned long, new_addr);
   SET_STATUS_from_SysRes( 
      do_mremap((Addr)ARG1, ARG2, (Addr)ARG5, ARG3, ARG4, tid) 
   );
}

PRE(sys_nice)
{
   PRINT("sys_nice ( %d )", ARG1);
   PRE_REG_READ1(long, "nice", int, inc);
}

PRE(sys_mlock)
{
   *flags |= SfMayBlock;
   PRINT("sys_mlock ( %p, %llu )", ARG1, (ULong)ARG2);
   PRE_REG_READ2(long, "mlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_munlock)
{
   *flags |= SfMayBlock;
   PRINT("sys_munlock ( %p, %llu )", ARG1, (ULong)ARG2);
   PRE_REG_READ2(long, "munlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_mlockall)
{
   *flags |= SfMayBlock;
   PRINT("sys_mlockall ( %x )", ARG1);
   PRE_REG_READ1(long, "mlockall", int, flags);
}

PRE(sys_setpriority)
{
   PRINT("sys_setpriority ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setpriority", int, which, int, who, int, prio);
}

PRE(sys_getpriority)
{
   PRINT("sys_getpriority ( %d, %d )", ARG1, ARG2);
   PRE_REG_READ2(long, "getpriority", int, which, int, who);
}

// The actual kernel definition of this routine takes a
// single 64 bit offset argument. This version is for 32 bit
// platforms only and treats the offset as two values - the
// kernel relies on stack based argument passing conventions
// to merge the two together.
PRE(sys_pwrite64)
{
   *flags |= SfMayBlock;
   PRINT("sys_pwrite64 ( %d, %p, %llu, %lld )",
         ARG1, ARG2, (ULong)ARG3, LOHI64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pwrite64",
                 unsigned int, fd, const char *, buf, vki_size_t, count,
                 vki_u32, offset_low32, vki_u32, offset_high32);
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
   PRINT("sys_fstatfs ( %d, %p )",ARG1,ARG2);
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
   PRINT("sys_fstatfs64 ( %d, %llu, %p )",ARG1,(ULong)ARG2,ARG3);
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
   PRINT("sys_getsid ( %d )", ARG1);
   PRE_REG_READ1(long, "getsid", vki_pid_t, pid);
}

// The actual kernel definition of this routine takes a
// single 64 bit offset argument. This version is for 32 bit
// platforms only and treats the offset as two values - the
// kernel relies on stack based argument passing conventions
// to merge the two together.
PRE(sys_pread64)
{
   *flags |= SfMayBlock;
   PRINT("sys_pread64 ( %d, %p, %llu, %lld )",
         ARG1, ARG2, (ULong)ARG3, LOHI64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pread64",
                 unsigned int, fd, char *, buf, vki_size_t, count,
                 vki_u32, offset_low32, vki_u32, offset_high32);
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
   PRINT("sys_mknod ( %p, 0x%x, 0x%x )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(long, "mknod",
                 const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknod(pathname)", ARG1 );
}

PRE(sys_flock)
{
   *flags |= SfMayBlock;
   PRINT("sys_flock ( %d, %d )", ARG1, ARG2 );
   PRE_REG_READ2(long, "flock", unsigned int, fd, unsigned int, operation);
}

/* This surely isn't remotely generic -- move to linux-specifics? */
PRE(sys_init_module)
{
   *flags |= SfMayBlock;
   PRINT("sys_init_module ( %p, %llu, %p )", ARG1, (ULong)ARG2, ARG3 );
   PRE_REG_READ3(long, "init_module",
                 void *, umod, unsigned long, len, const char *, uargs);
   PRE_MEM_READ( "init_module(umod)", ARG1, ARG2 );
   PRE_MEM_RASCIIZ( "init_module(uargs)", ARG3 );
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

   PRINT("sys_execve ( %p(%s), %p, %p )", ARG1, ARG1, ARG2, ARG3);
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
   {
      struct vki_stat st;
      SysRes r = VG_(stat)((Char *)ARG1, &st);

      if (r.isError) {
         /* stat failed */
         SET_STATUS_from_SysRes( r );
	 return;
      }
      /* just look for regular file with any X bit set
	 XXX do proper permissions check?
       */
      if ((st.st_mode & 0100111) == 0100000) {
	 SET_STATUS_Failure( VKI_EACCES );
	 return;
      }
   }

   /* Check more .. that the name at least begins in client-accessible
      storage. */
   if (!VG_(am_is_valid_for_client)( ARG1, 1, VKI_PROT_READ )) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
   }

   /* If we're tracing the child, and the launcher name looks bogus
      (possibly because launcher.c couldn't figure it out, see
      comments therein) then we have no option but to fail. */
   if (VG_(clo_trace_children) 
       && (VG_(name_of_launcher) == NULL
           || VG_(name_of_launcher)[0] != '/')) {
      SET_STATUS_Failure( VKI_ECHILD ); /* "No child processes" */
      return;
   }

   /* After this point, we can't recover if the execve fails. */
   VG_(debugLog)(1, "syswrap", "Exec of %s\n", (Char*)ARG1);

   /* Resistance is futile.  Nuke all other threads.  POSIX mandates
      this. (Really, nuke them all, since the new process will make
      its own new thread.) */
   VG_(nuke_all_threads_except)( tid, VgSrc_ExitSyscall );
   VG_(reap_threads)(tid);

   // Set up the child's exe path.
   //
   if (VG_(clo_trace_children)) {

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

   if (VG_(clo_trace_children)) {
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
   if (!VG_(clo_trace_children)) {
      argv = (Char**)ARG2;
   } else {
      vg_assert( VG_(args_for_valgrind_noexecpass) >= 0 );
      vg_assert( VG_(args_for_valgrind_noexecpass) 
                   <= VG_(args_for_valgrind).used );
      /* how many args in total will there be? */
      // launcher basename
      tot_args = 1;
      // V's args
      tot_args += VG_(args_for_valgrind).used;
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
      argv = VG_(malloc)( (tot_args+1) * sizeof(HChar*) );
      if (argv == 0) goto hosed;
      // copy
      j = 0;
      argv[j++] = launcher_basename;
      for (i = 0; i < VG_(args_for_valgrind).used; i++) {
         if (i < VG_(args_for_valgrind_noexecpass))
            continue;
         argv[j++] = VG_(args_for_valgrind).strs[i];
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
      static const struct vki_timespec zero = { 0, 0 };

      for (i = 1; i < VG_(max_signal); i++) {
         struct vki_sigaction sa;
         VG_(do_sys_sigaction)(i, NULL, &sa);
         if (sa.ksa_handler == VKI_SIG_IGN)
            VG_(sigaction)(i, &sa, NULL);
         else {
            sa.ksa_handler = VKI_SIG_DFL;
            VG_(sigaction)(i, &sa, NULL);
         }
      }

      VG_(sigfillset)(&allsigs);
      while(VG_(sigtimedwait)(&allsigs, &info, &zero) > 0)
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
   VG_(message)(Vg_UserMsg, "execve(%p(%s), %p, %p) failed, errno %d",
                ARG1, ARG1, ARG2, ARG3, RES_unchecked);
   VG_(message)(Vg_UserMsg, "EXEC FAILED: I can't recover from "
                            "execve() failing, so I'm dying.");
   VG_(message)(Vg_UserMsg, "Add more stringent tests in PRE(sys_execve), "
                            "or work out how to recover.");
   VG_(exit)(101);
}

PRE(sys_access)
{
   PRINT("sys_access ( %p(%s), %d )", ARG1,ARG1,ARG2);
   PRE_REG_READ2(long, "access", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "access(pathname)", ARG1 );
}

PRE(sys_alarm)
{
   PRINT("sys_alarm ( %d )", ARG1);
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
   PRINT("sys_brk ( %p )", ARG1);
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
                                ARG1-brk_limit );
      }
   } else {
      /* brk() failed */
      vg_assert(brk_limit == brk_new);
   }
}

PRE(sys_chdir)
{
   PRINT("sys_chdir ( %p )", ARG1);
   PRE_REG_READ1(long, "chdir", const char *, path);
   PRE_MEM_RASCIIZ( "chdir(path)", ARG1 );
}

PRE(sys_chmod)
{
   PRINT("sys_chmod ( %p, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "chmod", const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "chmod(path)", ARG1 );
}

PRE(sys_chown)
{
   PRINT("sys_chown ( %p, 0x%x, 0x%x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "chown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "chown(path)", ARG1 );
}

PRE(sys_lchown)
{
   PRINT("sys_lchown ( %p, 0x%x, 0x%x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "lchown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "lchown(path)", ARG1 );
}

PRE(sys_close)
{
   PRINT("sys_close ( %d )", ARG1);
   PRE_REG_READ1(long, "close", unsigned int, fd);

   /* Detect and negate attempts by the client to close Valgrind's log fd */
   if (!ML_(fd_allowed)(ARG1, "close", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}

POST(sys_close)
{
   if (VG_(clo_track_fds)) record_fd_close(tid, ARG1);
}

PRE(sys_dup)
{
   PRINT("sys_dup ( %d )", ARG1);
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
         record_fd_open_named(tid, RES);
   }
}

PRE(sys_dup2)
{
   PRINT("sys_dup2 ( %d, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "dup2", unsigned int, oldfd, unsigned int, newfd);
   if (!ML_(fd_allowed)(ARG2, "dup2", tid, True))
      SET_STATUS_Failure( VKI_EBADF );
}

POST(sys_dup2)
{
   vg_assert(SUCCESS);
   if (VG_(clo_track_fds))
      record_fd_open_named(tid, RES);
}

PRE(sys_fchdir)
{
   PRINT("sys_fchdir ( %d )", ARG1);
   PRE_REG_READ1(long, "fchdir", unsigned int, fd);
}

PRE(sys_fchown)
{
   PRINT("sys_fchown ( %d, %d, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "fchown",
                 unsigned int, fd, vki_uid_t, owner, vki_gid_t, group);
}

PRE(sys_fchmod)
{
   PRINT("sys_fchmod ( %d, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "fchmod", unsigned int, fildes, vki_mode_t, mode);
}

PRE(sys_fcntl)
{
   switch (ARG2) {
   // These ones ignore ARG3.
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
   case VKI_F_GETSIG:
   case VKI_F_GETLEASE:
      PRINT("sys_fcntl ( %d, %d )", ARG1,ARG2);
      PRE_REG_READ2(long, "fcntl", unsigned int, fd, unsigned int, cmd);
      break;

   // These ones use ARG3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETLEASE:
   case VKI_F_NOTIFY:
   case VKI_F_SETOWN:
   case VKI_F_SETSIG:
      PRINT("sys_fcntl[ARG3=='arg'] ( %d, %d, %d )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      break;

   // These ones use ARG3 as "lock".
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
#  if defined(VGP_x86_linux)
   case VKI_F_GETLK64:
   case VKI_F_SETLK64:
   case VKI_F_SETLKW64:
#  else
#  endif
      PRINT("sys_fcntl[ARG3=='lock'] ( %d, %d, %p )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct flock64 *, lock);
      break;
   }

   if (ARG2 == VKI_F_SETLKW)
      *flags |= SfMayBlock;
}

POST(sys_fcntl)
{
   vg_assert(SUCCESS);
   if (ARG2 == VKI_F_DUPFD) {
      if (!ML_(fd_allowed)(RES, "fcntl(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            record_fd_open_named(tid, RES);
      }
   }
}

// XXX: wrapper only suitable for 32-bit systems
PRE(sys_fcntl64)
{
   switch (ARG2) {
   // These ones ignore ARG3.
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
   case VKI_F_SETOWN:
   case VKI_F_GETSIG:
   case VKI_F_SETSIG:
   case VKI_F_GETLEASE:
      PRINT("sys_fcntl64 ( %d, %d )", ARG1,ARG2);
      PRE_REG_READ2(long, "fcntl64", unsigned int, fd, unsigned int, cmd);
      break;

   // These ones use ARG3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETLEASE:
   case VKI_F_NOTIFY:
      PRINT("sys_fcntl64[ARG3=='arg'] ( %d, %d, %d )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl64",
                    unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      break;

   // These ones use ARG3 as "lock".
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
#  if defined(VGP_x86_linux)
   case VKI_F_GETLK64:
   case VKI_F_SETLK64:
   case VKI_F_SETLKW64:
#  endif
      PRINT("sys_fcntl64[ARG3=='lock'] ( %d, %d, %p )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl64",
                    unsigned int, fd, unsigned int, cmd,
                    struct flock64 *, lock);
      break;
   }
   
#  if defined(VGP_x86_linux)
   if (ARG2 == VKI_F_SETLKW || ARG2 == VKI_F_SETLKW64)
      *flags |= SfMayBlock;
#  else
   if (ARG2 == VKI_F_SETLKW)
      *flags |= SfMayBlock;
#  endif
}

POST(sys_fcntl64)
{
   vg_assert(SUCCESS);
   if (ARG2 == VKI_F_DUPFD) {
      if (!ML_(fd_allowed)(RES, "fcntl64(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            record_fd_open_named(tid, RES);
      }
   }
}

PRE(sys_newfstat)
{
   PRINT("sys_newfstat ( %d, %p )", ARG1,ARG2);
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
   vki_sigset_t mask;

   PRINT("sys_fork ( )");
   PRE_REG_READ0(long, "fork");

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   SET_STATUS_from_SysRes( VG_(do_syscall0)(__NR_fork) );

   if (SUCCESS && RES == 0) {
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } 
   else 
   if (SUCCESS && RES > 0) {
      PRINT("   fork: process %d created child %d\n", VG_(getpid)(), RES);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }
}

PRE(sys_ftruncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_ftruncate ( %d, %ld )", ARG1,ARG2);
   PRE_REG_READ2(long, "ftruncate", unsigned int, fd, unsigned long, length);
}

PRE(sys_truncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_truncate ( %p(%s), %d )", ARG1,ARG1,ARG2);
   PRE_REG_READ2(long, "truncate", 
                 const char *, path, unsigned long, length);
   PRE_MEM_RASCIIZ( "truncate(path)", ARG1 );
}

// XXX: this wrapper is only suitable for 32-bit platforms
#if defined(VGP_x86_linux)
PRE(sys_ftruncate64)
{
   *flags |= SfMayBlock;
   PRINT("sys_ftruncate64 ( %d, %lld )", ARG1, LOHI64(ARG2,ARG3));
   PRE_REG_READ3(long, "ftruncate64",
                 unsigned int, fd,
                 vki_u32, length_low32, vki_u32, length_high32);
}
#endif

// XXX: this wrapper is only suitable for 32-bit platforms
#if defined(VGP_x86_linux)
PRE(sys_truncate64)
{
   *flags |= SfMayBlock;
   PRINT("sys_truncate64 ( %p, %lld )", ARG1, LOHI64(ARG2, ARG3));
   PRE_REG_READ3(long, "truncate64",
                 const char *, path,
                 vki_u32, length_low32, vki_u32, length_high32);
   PRE_MEM_RASCIIZ( "truncate64(path)", ARG1 );
}
#endif

PRE(sys_getdents)
{
   *flags |= SfMayBlock;
   PRINT("sys_getdents ( %d, %p, %d )", ARG1,ARG2,ARG3);
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
   PRINT("sys_getdents64 ( %d, %p, %d )",ARG1,ARG2,ARG3);
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
   PRINT("sys_getgroups ( %d, %p )", ARG1, ARG2);
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
   PRINT("sys_getcwd ( %p, %llu )", ARG1,(ULong)ARG2);
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
   PRINT("sys_getpgid ( %d )", ARG1);
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
   PRINT("sys_old_getrlimit ( %d, %p )", ARG1,ARG2);
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
   PRINT("sys_getrlimit ( %d, %p )", ARG1,ARG2);
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
   PRINT("sys_getrusage ( %d, %p )", ARG1,ARG2);
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
   PRINT("sys_gettimeofday ( %p, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "gettimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   PRE_MEM_WRITE( "gettimeofday(tv)", ARG1, sizeof(struct vki_timeval) );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "gettimeofday(tz)", ARG2, sizeof(struct vki_timezone) );
}

POST(sys_gettimeofday)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(struct vki_timeval) );
      if (ARG2 != 0)
	 POST_MEM_WRITE( ARG2, sizeof(struct vki_timezone) );
   }
}

PRE(sys_settimeofday)
{
   PRINT("sys_settimeofday ( %p, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "settimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   PRE_MEM_READ( "settimeofday(tv)", ARG1, sizeof(struct vki_timeval) );
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

// XXX: I reckon some of these cases must be x86-specific
PRE(sys_ioctl)
{
   *flags |= SfMayBlock;
   PRINT("sys_ioctl ( %d, 0x%x, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "ioctl",
                 unsigned int, fd, unsigned int, request, unsigned long, arg);

   switch (ARG2 /* request */) {
   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
      PRE_MEM_READ( "ioctl(TCSET{S,SW,SF})", ARG3, sizeof(struct vki_termios) );
      break; 
   case VKI_TCGETS:
      PRE_MEM_WRITE( "ioctl(TCGETS)", ARG3, sizeof(struct vki_termios) );
      break;
   case VKI_TCSETA:
   case VKI_TCSETAW:
   case VKI_TCSETAF:
      PRE_MEM_READ( "ioctl(TCSET{A,AW,AF})", ARG3, sizeof(struct vki_termio) );
      break;
   case VKI_TCGETA:
      PRE_MEM_WRITE( "ioctl(TCGETA)", ARG3, sizeof(struct vki_termio) );
      break;
   case VKI_TCSBRK:
   case VKI_TCXONC:
   case VKI_TCSBRKP:
   case VKI_TCFLSH:
      /* These just take an int by value */
      break;
   case VKI_TIOCGWINSZ:
      PRE_MEM_WRITE( "ioctl(TIOCGWINSZ)", ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
      PRE_MEM_READ( "ioctl(TIOCSWINSZ)",  ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCMBIS:
      PRE_MEM_READ( "ioctl(TIOCMBIS)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMBIC:
      PRE_MEM_READ( "ioctl(TIOCMBIC)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMSET:
      PRE_MEM_READ( "ioctl(TIOCMSET)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMGET:
      PRE_MEM_WRITE( "ioctl(TIOCMGET)",   ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCLINUX:
      PRE_MEM_READ( "ioctl(TIOCLINUX)",   ARG3, sizeof(char *) );
      if (*(char *)ARG3 == 11) {
	 PRE_MEM_READ( "ioctl(TIOCLINUX, 11)", ARG3, 2 * sizeof(char *) );
      }
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      PRE_MEM_WRITE( "ioctl(TIOCGPTN)", ARG3, sizeof(int) );
      break;
   case VKI_TIOCSCTTY:
      /* Just takes an int value.  */
      break;
   case VKI_TIOCSPTLCK: /* Lock/unlock Pty */
      PRE_MEM_READ( "ioctl(TIOCSPTLCK)", ARG3, sizeof(int) );
      break;
   case VKI_FIONBIO:
      PRE_MEM_READ( "ioctl(FIONBIO)",    ARG3, sizeof(int) );
      break;
   case VKI_FIOASYNC:
      PRE_MEM_READ( "ioctl(FIOASYNC)",   ARG3, sizeof(int) );
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      PRE_MEM_WRITE( "ioctl(FIONREAD)",  ARG3, sizeof(int) );
      break;

   case VKI_SG_SET_COMMAND_Q:
      PRE_MEM_READ( "ioctl(SG_SET_COMMAND_Q)", ARG3, sizeof(int) );
      break;
   case VKI_SG_IO:
      PRE_MEM_WRITE( "ioctl(SG_IO)", ARG3, sizeof(vki_sg_io_hdr_t) );
      break;
   case VKI_SG_GET_SCSI_ID:
      PRE_MEM_WRITE( "ioctl(SG_GET_SCSI_ID)", ARG3, sizeof(vki_sg_scsi_id_t) );
      break;
   case VKI_SG_SET_RESERVED_SIZE:
      PRE_MEM_READ( "ioctl(SG_SET_RESERVED_SIZE)", ARG3, sizeof(int) );
      break;
   case VKI_SG_SET_TIMEOUT:
      PRE_MEM_READ( "ioctl(SG_SET_TIMEOUT)", ARG3, sizeof(int) );
      break;
   case VKI_SG_GET_RESERVED_SIZE:
      PRE_MEM_WRITE( "ioctl(SG_GET_RESERVED_SIZE)", ARG3, sizeof(int) );
      break;
   case VKI_SG_GET_TIMEOUT:
      PRE_MEM_WRITE( "ioctl(SG_GET_TIMEOUT)", ARG3, sizeof(int) );
      break;
   case VKI_SG_GET_VERSION_NUM:
      PRE_MEM_READ(  "ioctl(SG_GET_VERSION_NUM)",  ARG3, sizeof(int) );
      break;
   case VKI_SG_EMULATED_HOST: /* 0x2203 */
      PRE_MEM_WRITE( "ioctl(SG_EMULATED_HOST)",    ARG3, sizeof(int) );
      break;
   case VKI_SG_GET_SG_TABLESIZE: /* 0x227f */
      PRE_MEM_WRITE( "ioctl(SG_GET_SG_TABLESIZE)", ARG3, sizeof(int) );
      break;

   case VKI_IIOCGETCPS:
      PRE_MEM_WRITE( "ioctl(IIOCGETCPS)", ARG3,
		     VKI_ISDN_MAX_CHANNELS * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      PRE_MEM_READ( "ioctl(IIOCNETGPN)",
		     (Addr)&((vki_isdn_net_ioctl_phone *)ARG3)->name,
		     sizeof(((vki_isdn_net_ioctl_phone *)ARG3)->name) );
      PRE_MEM_WRITE( "ioctl(IIOCNETGPN)", ARG3,
		     sizeof(vki_isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case VKI_SIOCGIFINDEX:        /* get iface index              */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFINDEX)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFINDEX)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFFLAGS)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFFLAGS)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFHWADDR:       /* Get hardware address         */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFHWADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFHWADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMTU)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMTU)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFNETMASK)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNETMASK)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMETRIC)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMETRIC)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMAP:          /* Get device parameters        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMAP)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMAP)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFTXQLEN:       /* Get the tx queue length      */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFTXQLEN)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFTXQLEN)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFDSTADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFDSTADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFBRDADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFBRDADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNAME:         /* get iface name               */
      PRE_MEM_READ( "ioctl(SIOCGIFNAME)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_ifindex,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_ifindex) );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNAME)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGMIIPHY:         /* get hardware entry           */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMIIPHY)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMIIPHY)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGMIIREG:         /* get hardware entry registers */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id) );
      PRE_MEM_READ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num) );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMIIREG)", ARG3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 PRE_MEM_WRITE( "ioctl(SIOCGIFCONF)", ARG3, sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,RES);
	 if (!VG_(is_kerror)(RES) && RES == 0)
	 POST_MEM_WRITE(ARG3, sizeof(struct ifconf));
      */
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)",
                    (Addr)&((struct vki_ifconf *)ARG3)->ifc_len,
                    sizeof(((struct vki_ifconf *)ARG3)->ifc_len));
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)",
                    (Addr)&((struct vki_ifconf *)ARG3)->vki_ifc_buf,
                    sizeof(((struct vki_ifconf *)ARG3)->vki_ifc_buf));
      if ( ARG3 ) {
	 // TODO len must be readable and writable
	 // buf pointer only needs to be readable
	 struct vki_ifconf *ifc = (struct vki_ifconf *) ARG3;
	 PRE_MEM_WRITE( "ioctl(SIOCGIFCONF).ifc_buf",
			(Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
   case VKI_SIOCGSTAMP:
      PRE_MEM_WRITE( "ioctl(SIOCGSTAMP)", ARG3, sizeof(struct vki_timeval));
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case VKI_SIOCOUTQ:
      PRE_MEM_WRITE( "ioctl(SIOCOUTQ)", ARG3, sizeof(int));
      break;
   case VKI_SIOCGRARP:           /* get RARP table entry         */
   case VKI_SIOCGARP:            /* get ARP table entry          */
      PRE_MEM_WRITE( "ioctl(SIOCGARP)", ARG3, sizeof(struct vki_arpreq));
      break;
                    
   case VKI_SIOCSIFFLAGS:        /* set flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_SIOCSIFMAP:          /* Set device parameters        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMAP)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMAP)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_map,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_map) );
      break;
   case VKI_SIOCSIFTXQLEN:       /* Set the tx queue length      */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFTXQLEN)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFTXQLEN)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_qlen,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_qlen) );
      break;
   case VKI_SIOCSIFADDR:         /* set PA address               */
   case VKI_SIOCSIFDSTADDR:      /* set remote PA address        */
   case VKI_SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case VKI_SIOCSIFNETMASK:      /* set network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_addr,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_addr) );
      break;
   case VKI_SIOCSIFMETRIC:       /* set metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_metric,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_metric) );
      break;
   case VKI_SIOCSIFMTU:          /* set MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMTU)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMTU)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_mtu,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_mtu) );
      break;
   case VKI_SIOCSIFHWADDR:       /* set hardware address         */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFHWADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFHWADDR)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_hwaddr,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_hwaddr) );
      break;
   case VKI_SIOCSMIIREG:         /* set hardware entry registers */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSMIIREG)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id) );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num) );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_in,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_in) );
      break;
      /* Routing table calls.  */
   case VKI_SIOCADDRT:           /* add routing table entry      */
   case VKI_SIOCDELRT:           /* delete routing table entry   */
      PRE_MEM_READ( "ioctl(SIOCADDRT/DELRT)", ARG3, 
		    sizeof(struct vki_rtentry));
      break;

      /* RARP cache control calls. */
   case VKI_SIOCDRARP:           /* delete RARP table entry      */
   case VKI_SIOCSRARP:           /* set RARP table entry         */
      /* ARP cache control calls. */
   case VKI_SIOCSARP:            /* set ARP table entry          */
   case VKI_SIOCDARP:            /* delete ARP table entry       */
      PRE_MEM_READ( "ioctl(SIOCSIFFLAGS)", ARG3, sizeof(struct vki_ifreq));
      break;

   case VKI_SIOCGPGRP:
      PRE_MEM_WRITE( "ioctl(SIOCGPGRP)", ARG3, sizeof(int) );
      break;
   case VKI_SIOCSPGRP:
      PRE_MEM_READ( "ioctl(SIOCSPGRP)", ARG3, sizeof(int) );
      //tst->sys_flags &= ~SfMayBlock;
      break;

      /* linux/soundcard interface (OSS) */
   case VKI_SNDCTL_SEQ_GETOUTCOUNT:
   case VKI_SNDCTL_SEQ_GETINCOUNT:
   case VKI_SNDCTL_SEQ_PERCMODE:
   case VKI_SNDCTL_SEQ_TESTMIDI:
   case VKI_SNDCTL_SEQ_RESETSAMPLES:
   case VKI_SNDCTL_SEQ_NRSYNTHS:
   case VKI_SNDCTL_SEQ_NRMIDIS:
   case VKI_SNDCTL_SEQ_GETTIME:
   case VKI_SNDCTL_DSP_GETFMTS:
   case VKI_SNDCTL_DSP_GETTRIGGER:
   case VKI_SNDCTL_DSP_GETODELAY:
   case VKI_SNDCTL_DSP_GETSPDIF:
   case VKI_SNDCTL_DSP_GETCAPS:
   case VKI_SOUND_PCM_READ_RATE:
   case VKI_SOUND_PCM_READ_CHANNELS:
   case VKI_SOUND_PCM_READ_BITS:
#if !defined(VGA_ppc32)
   case (VKI_SOUND_PCM_READ_BITS|0x40000000): /* what the fuck ? */
#endif
   case VKI_SOUND_PCM_READ_FILTER:
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOR, int))", 
		     ARG3, sizeof(int));
      break;
   case VKI_SNDCTL_SEQ_CTRLRATE:
   case VKI_SNDCTL_DSP_SPEED:
   case VKI_SNDCTL_DSP_STEREO:
   case VKI_SNDCTL_DSP_GETBLKSIZE: 
   case VKI_SNDCTL_DSP_CHANNELS:
   case VKI_SOUND_PCM_WRITE_FILTER:
   case VKI_SNDCTL_DSP_SUBDIVIDE:
   case VKI_SNDCTL_DSP_SETFRAGMENT:
   case VKI_SNDCTL_DSP_GETCHANNELMASK:
   case VKI_SNDCTL_DSP_BIND_CHANNEL:
   case VKI_SNDCTL_TMR_TIMEBASE:
   case VKI_SNDCTL_TMR_TEMPO:
   case VKI_SNDCTL_TMR_SOURCE:
   case VKI_SNDCTL_MIDI_PRETIME:
   case VKI_SNDCTL_MIDI_MPUMODE:
      PRE_MEM_READ( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOWR, int))", 
		     ARG3, sizeof(int));
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOWR, int))", 
		     ARG3, sizeof(int));
      break;
   case VKI_SNDCTL_DSP_GETOSPACE:
   case VKI_SNDCTL_DSP_GETISPACE:
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOR, audio_buf_info))",
                     ARG3, sizeof(vki_audio_buf_info));
      break;
   case VKI_SNDCTL_DSP_SETTRIGGER:
      PRE_MEM_READ( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOW, int))", 
		     ARG3, sizeof(int));
      break;

   case VKI_SNDCTL_DSP_POST:
   case VKI_SNDCTL_DSP_RESET:
   case VKI_SNDCTL_DSP_SYNC:
   case VKI_SNDCTL_DSP_SETSYNCRO:
   case VKI_SNDCTL_DSP_SETDUPLEX:
      break;

      /* Real Time Clock (/dev/rtc) ioctls */
   case VKI_RTC_UIE_ON:
   case VKI_RTC_UIE_OFF:
   case VKI_RTC_AIE_ON:
   case VKI_RTC_AIE_OFF:
   case VKI_RTC_PIE_ON:
   case VKI_RTC_PIE_OFF:
   case VKI_RTC_IRQP_SET:
      break;
   case VKI_RTC_RD_TIME:
   case VKI_RTC_ALM_READ:
      PRE_MEM_WRITE( "ioctl(RTC_RD_TIME/ALM_READ)", 
		     ARG3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_ALM_SET:
      PRE_MEM_READ( "ioctl(RTC_ALM_SET)", ARG3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_IRQP_READ:
      PRE_MEM_WRITE( "ioctl(RTC_IRQP_READ)", ARG3, sizeof(unsigned long));
      break;

      /* Block devices */
   case VKI_BLKROSET:
      PRE_MEM_READ( "ioctl(BLKROSET)", ARG3, sizeof(int));
      break;
   case VKI_BLKROGET:
      PRE_MEM_WRITE( "ioctl(BLKROGET)", ARG3, sizeof(int));
      break;
   case VKI_BLKGETSIZE:
      PRE_MEM_WRITE( "ioctl(BLKGETSIZE)", ARG3, sizeof(unsigned long));
      break;
   case VKI_BLKRASET:
      break;
   case VKI_BLKRAGET:
      PRE_MEM_WRITE( "ioctl(BLKRAGET)", ARG3, sizeof(long));
      break;
   case VKI_BLKFRASET:
      break;
   case VKI_BLKFRAGET:
      PRE_MEM_WRITE( "ioctl(BLKFRAGET)", ARG3, sizeof(long));
      break;
   case VKI_BLKSECTGET:
      PRE_MEM_WRITE( "ioctl(BLKSECTGET)", ARG3, sizeof(unsigned short));
      break;
   case VKI_BLKSSZGET:
      PRE_MEM_WRITE( "ioctl(BLKSSZGET)", ARG3, sizeof(int));
      break;
   case VKI_BLKBSZGET:
      PRE_MEM_WRITE( "ioctl(BLKBSZGET)", ARG3, sizeof(int));
      break;
   case VKI_BLKBSZSET:
      PRE_MEM_READ( "ioctl(BLKBSZSET)", ARG3, sizeof(int));
      break;
   case VKI_BLKGETSIZE64:
      PRE_MEM_WRITE( "ioctl(BLKGETSIZE64)", ARG3, sizeof(unsigned long long));
      break;

      /* Hard disks */
   case VKI_HDIO_GETGEO: /* 0x0301 */
      PRE_MEM_WRITE( "ioctl(HDIO_GETGEO)", ARG3, sizeof(struct vki_hd_geometry));
      break;
   case VKI_HDIO_GET_IDENTITY: /* 0x030d */
      PRE_MEM_WRITE( "ioctl(HDIO_GET_IDENTITY)", ARG3,
                     VKI_SIZEOF_STRUCT_HD_DRIVEID );
      break;

      /* CD ROM stuff (??)  */
   case VKI_CDROM_GET_MCN:
      PRE_MEM_READ( "ioctl(CDROM_GET_MCN)", ARG3,
                    sizeof(struct vki_cdrom_mcn) );
      break;
   case VKI_CDROM_SEND_PACKET:
      PRE_MEM_READ( "ioctl(CDROM_SEND_PACKET)", ARG3,
                    sizeof(struct vki_cdrom_generic_command));
      break;
   case VKI_CDROMSUBCHNL:
      PRE_MEM_READ( "ioctl(CDROMSUBCHNL (cdsc_format, char))",
		    (Addr) &(((struct vki_cdrom_subchnl*) ARG3)->cdsc_format),
		    sizeof(((struct vki_cdrom_subchnl*) ARG3)->cdsc_format));
      PRE_MEM_WRITE( "ioctl(CDROMSUBCHNL)", ARG3, 
		     sizeof(struct vki_cdrom_subchnl));
      break;
   case VKI_CDROMREADMODE2:
      PRE_MEM_READ( "ioctl(CDROMREADMODE2)", ARG3, VKI_CD_FRAMESIZE_RAW0 );
      break;
   case VKI_CDROMREADTOCHDR:
      PRE_MEM_WRITE( "ioctl(CDROMREADTOCHDR)", ARG3, 
		     sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMREADTOCENTRY:
      PRE_MEM_READ( "ioctl(CDROMREADTOCENTRY (cdte_format, char))",
		    (Addr) &(((struct vki_cdrom_tocentry*) ARG3)->cdte_format),
		    sizeof(((struct vki_cdrom_tocentry*) ARG3)->cdte_format));
      PRE_MEM_READ( "ioctl(CDROMREADTOCENTRY (cdte_track, char))",
		    (Addr) &(((struct vki_cdrom_tocentry*) ARG3)->cdte_track), 
		    sizeof(((struct vki_cdrom_tocentry*) ARG3)->cdte_track));
      PRE_MEM_WRITE( "ioctl(CDROMREADTOCENTRY)", ARG3, 
		     sizeof(struct vki_cdrom_tocentry));
      break;
   case VKI_CDROMMULTISESSION: /* 0x5310 */
      PRE_MEM_WRITE( "ioctl(CDROMMULTISESSION)", ARG3,
		     sizeof(struct vki_cdrom_multisession));
      break;
   case VKI_CDROMVOLREAD: /* 0x5313 */
      PRE_MEM_WRITE( "ioctl(CDROMVOLREAD)", ARG3,
		     sizeof(struct vki_cdrom_volctrl));
      break;
   case VKI_CDROMREADAUDIO: /* 0x530e */
      PRE_MEM_READ( "ioctl(CDROMREADAUDIO)", ARG3,
		     sizeof (struct vki_cdrom_read_audio));
      if ( ARG3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_cdrom_read_audio *cra = (struct vki_cdrom_read_audio *) ARG3;
	 PRE_MEM_WRITE( "ioctl(CDROMREADAUDIO).buf",
	                (Addr)(cra->buf), cra->nframes * VKI_CD_FRAMESIZE_RAW);
      }
      break;      
   case VKI_CDROMPLAYMSF:
      PRE_MEM_READ( "ioctl(CDROMPLAYMSF)", ARG3, sizeof(struct vki_cdrom_msf));
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case VKI_CDROM_DRIVE_STATUS: /* 0x5326 */
   case VKI_CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

   case VKI_FIGETBSZ:
      PRE_MEM_WRITE( "ioctl(FIGETBSZ)", ARG3, sizeof(unsigned long));
      break;
   case VKI_FIBMAP:
      PRE_MEM_READ( "ioctl(FIBMAP)", ARG3, sizeof(unsigned long));
      break;

   case VKI_FBIOGET_VSCREENINFO: /* 0x4600 */
      PRE_MEM_WRITE( "ioctl(FBIOGET_VSCREENINFO)", ARG3,
                     sizeof(struct vki_fb_var_screeninfo));
      break;
   case VKI_FBIOGET_FSCREENINFO: /* 0x4602 */
      PRE_MEM_WRITE( "ioctl(FBIOGET_FSCREENINFO)", ARG3,
                     sizeof(struct vki_fb_fix_screeninfo));
      break;

   case VKI_PPCLAIM:
   case VKI_PPEXCL:
   case VKI_PPYIELD:
   case VKI_PPRELEASE:
      break;
   case VKI_PPSETMODE:
      PRE_MEM_READ( "ioctl(PPSETMODE)",   ARG3, sizeof(int) );
      break;
   case VKI_PPGETMODE:
      PRE_MEM_WRITE( "ioctl(PPGETMODE)",  ARG3, sizeof(int) );
      break;
   case VKI_PPSETPHASE:
      PRE_MEM_READ(  "ioctl(PPSETPHASE)", ARG3, sizeof(int) );
      break;
   case VKI_PPGETPHASE:
      PRE_MEM_WRITE( "ioctl(PPGETPHASE)", ARG3, sizeof(int) );
      break;
   case VKI_PPGETMODES:
      PRE_MEM_WRITE( "ioctl(PPGETMODES)", ARG3, sizeof(unsigned int) );
      break;
   case VKI_PPSETFLAGS:
      PRE_MEM_READ(  "ioctl(PPSETFLAGS)", ARG3, sizeof(int) );
      break;
   case VKI_PPGETFLAGS:
      PRE_MEM_WRITE( "ioctl(PPGETFLAGS)", ARG3, sizeof(int) );
      break;
   case VKI_PPRSTATUS:
      PRE_MEM_WRITE( "ioctl(PPRSTATUS)",  ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRDATA:
      PRE_MEM_WRITE( "ioctl(PPRDATA)",    ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRCONTROL:
      PRE_MEM_WRITE( "ioctl(PPRCONTROL)", ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPWDATA:
      PRE_MEM_READ(  "ioctl(PPWDATA)",    ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPWCONTROL:
      PRE_MEM_READ(  "ioctl(PPWCONTROL)", ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPFCONTROL:
      PRE_MEM_READ(  "ioctl(PPFCONTROL)", ARG3, 2 * sizeof(unsigned char) );
      break;
   case VKI_PPDATADIR:
      PRE_MEM_READ(  "ioctl(PPDATADIR)",  ARG3, sizeof(int) );
      break;
   case VKI_PPNEGOT:
      PRE_MEM_READ(  "ioctl(PPNEGOT)",    ARG3, sizeof(int) );
      break;
   case VKI_PPWCTLONIRQ:
      PRE_MEM_READ(  "ioctl(PPWCTLONIRQ)",ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPCLRIRQ:
      PRE_MEM_WRITE( "ioctl(PPCLRIRQ)",   ARG3, sizeof(int) );
      break;
   case VKI_PPSETTIME:
      PRE_MEM_READ(  "ioctl(PPSETTIME)",  ARG3, sizeof(struct vki_timeval) );
      break;
   case VKI_PPGETTIME:
      PRE_MEM_WRITE( "ioctl(PPGETTIME)",  ARG3, sizeof(struct vki_timeval) );
      break;

   case VKI_GIO_FONT:
      PRE_MEM_WRITE( "ioctl(GIO_FONT)", ARG3, 32 * 256 );
      break;
   case VKI_PIO_FONT:
      PRE_MEM_READ( "ioctl(PIO_FONT)", ARG3, 32 * 256 );
      break;

   case VKI_GIO_FONTX:
      PRE_MEM_READ( "ioctl(GIO_FONTX)", ARG3, sizeof(struct vki_consolefontdesc) );
      if ( ARG3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_consolefontdesc *cfd = (struct vki_consolefontdesc *)ARG3;
         PRE_MEM_WRITE( "ioctl(GIO_FONTX).chardata", (Addr)cfd->chardata,
                        32 * cfd->charcount );
      }
      break;
   case VKI_PIO_FONTX:
      PRE_MEM_READ( "ioctl(PIO_FONTX)", ARG3, sizeof(struct vki_consolefontdesc) );
      if ( ARG3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_consolefontdesc *cfd = (struct vki_consolefontdesc *)ARG3;
         PRE_MEM_READ( "ioctl(PIO_FONTX).chardata", (Addr)cfd->chardata,
                       32 * cfd->charcount );
      }
      break;

   case VKI_PIO_FONTRESET:
      break;

   case VKI_GIO_CMAP:
      PRE_MEM_WRITE( "ioctl(GIO_CMAP)", ARG3, 16 * 3 );
      break;
   case VKI_PIO_CMAP:
      PRE_MEM_READ( "ioctl(PIO_CMAP)", ARG3, 16 * 3 );
      break;

   case VKI_KIOCSOUND:
   case VKI_KDMKTONE:
      break;

   case VKI_KDGETLED:
      PRE_MEM_WRITE( "ioctl(KDGETLED)", ARG3, sizeof(char) );
      break;
   case VKI_KDSETLED:
      break;

   case VKI_KDGKBTYPE:
      PRE_MEM_WRITE( "ioctl(KDGKBTYPE)", ARG3, sizeof(char) );
      break;

   case VKI_KDADDIO:
   case VKI_KDDELIO:
   case VKI_KDENABIO:
   case VKI_KDDISABIO:
      break;

   case VKI_KDSETMODE:
      break;
   case VKI_KDGETMODE:
      PRE_MEM_WRITE( "ioctl(KDGETMODE)", ARG3, sizeof(int) );
      break;

   case VKI_KDMAPDISP:
   case VKI_KDUNMAPDISP:
      break;

   case VKI_GIO_SCRNMAP:
      PRE_MEM_WRITE( "ioctl(GIO_SCRNMAP)", ARG3, VKI_E_TABSZ );
      break;
   case VKI_PIO_SCRNMAP:
      PRE_MEM_READ( "ioctl(PIO_SCRNMAP)", ARG3, VKI_E_TABSZ  );
      break;
   case VKI_GIO_UNISCRNMAP:
      PRE_MEM_WRITE( "ioctl(GIO_UNISCRNMAP)", ARG3,
                     VKI_E_TABSZ * sizeof(unsigned short) );
      break;
   case VKI_PIO_UNISCRNMAP:
      PRE_MEM_READ( "ioctl(PIO_UNISCRNMAP)", ARG3,
                    VKI_E_TABSZ * sizeof(unsigned short) );
      break;

   case VKI_KDGKBMODE:
      PRE_MEM_WRITE( "ioctl(KDGKBMODE)", ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMODE:
      break;
      
   case VKI_KDGKBMETA:
      PRE_MEM_WRITE( "ioctl(KDGKBMETA)", ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMETA:
      break;
      
   case VKI_KDGKBLED:
      PRE_MEM_WRITE( "ioctl(KDGKBLED)", ARG3, sizeof(char) );
      break;
   case VKI_KDSKBLED:
      break;
      
   case VKI_KDGKBENT:
      PRE_MEM_READ( "ioctl(KDGKBENT).kb_table",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_table,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_table) );
      PRE_MEM_READ( "ioctl(KDGKBENT).kb_index",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_index,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_index) );
      PRE_MEM_WRITE( "ioctl(KDGKBENT).kb_value",
		     (Addr)&((struct vki_kbentry *)ARG3)->kb_value,
		     sizeof(((struct vki_kbentry *)ARG3)->kb_value) );
      break;
   case VKI_KDSKBENT:
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_table",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_table,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_table) );
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_index",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_index,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_index) );
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_value",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_value,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_value) );
      break;
      
   case VKI_KDGKBSENT:
      PRE_MEM_READ( "ioctl(KDGKBSENT).kb_func",
                    (Addr)&((struct vki_kbsentry *)ARG3)->kb_func,
                    sizeof(((struct vki_kbsentry *)ARG3)->kb_func) );
      PRE_MEM_WRITE( "ioctl(KDGKSENT).kb_string",
		     (Addr)((struct vki_kbsentry *)ARG3)->kb_string,
		     sizeof(((struct vki_kbsentry *)ARG3)->kb_string) );
      break;
   case VKI_KDSKBSENT:
      PRE_MEM_READ( "ioctl(KDSKBSENT).kb_func",
                    (Addr)&((struct vki_kbsentry *)ARG3)->kb_func,
                    sizeof(((struct vki_kbsentry *)ARG3)->kb_func) );
      PRE_MEM_RASCIIZ( "ioctl(KDSKBSENT).kb_string",
                       (Addr)((struct vki_kbsentry *)ARG3)->kb_string );
      break;
      
   case VKI_KDGKBDIACR:
      PRE_MEM_WRITE( "ioctl(KDGKBDIACR)", ARG3, sizeof(struct vki_kbdiacrs) );
      break;
   case VKI_KDSKBDIACR:
      PRE_MEM_READ( "ioctl(KDSKBDIACR)", ARG3, sizeof(struct vki_kbdiacrs) );
      break;
      
   case VKI_KDGETKEYCODE:
      PRE_MEM_READ( "ioctl(KDGETKEYCODE).scancode",
                    (Addr)&((struct vki_kbkeycode *)ARG3)->scancode,
                    sizeof(((struct vki_kbkeycode *)ARG3)->scancode) );
      PRE_MEM_WRITE( "ioctl(KDGETKEYCODE).keycode",
		     (Addr)((struct vki_kbkeycode *)ARG3)->keycode,
		     sizeof(((struct vki_kbkeycode *)ARG3)->keycode) );
      break;
   case VKI_KDSETKEYCODE:
      PRE_MEM_READ( "ioctl(KDSETKEYCODE).scancode",
                    (Addr)&((struct vki_kbkeycode *)ARG3)->scancode,
                    sizeof(((struct vki_kbkeycode *)ARG3)->scancode) );
      PRE_MEM_READ( "ioctl(KDSETKEYCODE).keycode",
                    (Addr)((struct vki_kbkeycode *)ARG3)->keycode,
                    sizeof(((struct vki_kbkeycode *)ARG3)->keycode) );
      break;
      
   case VKI_KDSIGACCEPT:
      break;

   case VKI_KDKBDREP:
      PRE_MEM_READ( "ioctl(KBKBDREP)", ARG3, sizeof(struct vki_kbd_repeat) );
      break;
      
      /* We don't have any specific information on it, so
	 try to do something reasonable based on direction and
	 size bits.  The encoding scheme is described in
	 /usr/include/asm/ioctl.h.  

	 According to Simon Hausmann, _IOC_READ means the kernel
	 writes a value to the ioctl value passed from the user
	 space and the other way around with _IOC_WRITE. */
   default: {
      UInt dir  = _VKI_IOC_DIR(ARG2);
      UInt size = _VKI_IOC_SIZE(ARG2);
      if (VG_(strstr)(VG_(clo_weird_hacks), "lax-ioctls") != NULL) {
	 /* 
	  * Be very lax about ioctl handling; the only
	  * assumption is that the size is correct. Doesn't
	  * require the full buffer to be initialized when
	  * writing.  Without this, using some device
	  * drivers with a large number of strange ioctl
	  * commands becomes very tiresome.
	  */
      } else if (/* size == 0 || */ dir == _VKI_IOC_NONE) {
	 static Int moans = 3;
	 if (moans > 0 && !VG_(clo_xml)) {
	    moans--;
	    VG_(message)(Vg_UserMsg, 
			 "Warning: noted but unhandled ioctl 0x%x"
			 " with no size/direction hints",
			 ARG2); 
	    VG_(message)(Vg_UserMsg, 
			 "   This could cause spurious value errors"
			 " to appear.");
	    VG_(message)(Vg_UserMsg, 
			 "   See README_MISSING_SYSCALL_OR_IOCTL for "
			 "guidance on writing a proper wrapper." );
	 }
      } else {
	 if ((dir & _VKI_IOC_WRITE) && size > 0)
	    PRE_MEM_READ( "ioctl(generic)", ARG3, size);
	 if ((dir & _VKI_IOC_READ) && size > 0)
	    PRE_MEM_WRITE( "ioctl(generic)", ARG3, size);
      }
      break;
   }
   }   
}

POST(sys_ioctl)
{
   vg_assert(SUCCESS);
   switch (ARG2 /* request */) {
   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
      break; 
   case VKI_TCGETS:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_termios) );
      break;
   case VKI_TCSETA:
   case VKI_TCSETAW:
   case VKI_TCSETAF:
      break;
   case VKI_TCGETA:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_termio) );
      break;
   case VKI_TCSBRK:
   case VKI_TCXONC:
   case VKI_TCSBRKP:
   case VKI_TCFLSH:
      break;
   case VKI_TIOCGWINSZ:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
   case VKI_TIOCMBIS:
   case VKI_TIOCMBIC:
   case VKI_TIOCMSET:
      break;
   case VKI_TIOCMGET:
      POST_MEM_WRITE( ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCLINUX:
      POST_MEM_WRITE( ARG3, sizeof(char *) );
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      POST_MEM_WRITE( ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      POST_MEM_WRITE( ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      POST_MEM_WRITE( ARG3, sizeof(int));
      break;
   case VKI_TIOCSCTTY:
      break;
   case VKI_TIOCSPTLCK: /* Lock/unlock Pty */
      break;
   case VKI_FIONBIO:
      break;
   case VKI_FIOASYNC:
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;

   case VKI_SG_SET_COMMAND_Q:
      break;
   case VKI_SG_IO:
      POST_MEM_WRITE(ARG3, sizeof(vki_sg_io_hdr_t));
      break;
   case VKI_SG_GET_SCSI_ID:
      POST_MEM_WRITE(ARG3, sizeof(vki_sg_scsi_id_t));
      break;
   case VKI_SG_SET_RESERVED_SIZE:
      break;
   case VKI_SG_SET_TIMEOUT:
      break;
   case VKI_SG_GET_RESERVED_SIZE:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SG_GET_TIMEOUT:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SG_GET_VERSION_NUM:
      break;
   case VKI_SG_EMULATED_HOST:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SG_GET_SG_TABLESIZE:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;      

   case VKI_IIOCGETCPS:
      POST_MEM_WRITE( ARG3, VKI_ISDN_MAX_CHANNELS * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      POST_MEM_WRITE( ARG3, sizeof(vki_isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case VKI_SIOCGIFINDEX:        /* get iface index              */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_ifindex,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_ifindex) );
      break;
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_SIOCGIFHWADDR:       /* Get hardware address         */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->ifr_hwaddr,
                      sizeof(((struct vki_ifreq *)ARG3)->ifr_hwaddr) );
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_mtu,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_mtu) );
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->ifr_addr,
                sizeof(((struct vki_ifreq *)ARG3)->ifr_addr) );
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_metric,
                sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_metric) );
      break;
   case VKI_SIOCGIFMAP:          /* Get device parameters        */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->ifr_map,
                sizeof(((struct vki_ifreq *)ARG3)->ifr_map) );
      break;
     break;
   case VKI_SIOCGIFTXQLEN:       /* Get the tx queue length      */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->ifr_qlen,
                sizeof(((struct vki_ifreq *)ARG3)->ifr_qlen) );
      break;
   case VKI_SIOCGIFNAME:         /* get iface name               */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_name,
                sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_name) );
      break;
   case VKI_SIOCGMIIPHY:         /* get hardware entry           */
      POST_MEM_WRITE(
                (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id,
                sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id) );
      break;
   case VKI_SIOCGMIIREG:         /* get hardware entry registers */
      POST_MEM_WRITE(
                (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_out,
                sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_out) );
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 PRE_MEM_WRITE("ioctl(SIOCGIFCONF)", ARG3, sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,RES);
	 if (!VG_(is_kerror)(RES) && RES == 0)
	 POST_MEM_WRITE(ARG3, sizeof(struct ifconf));
      */
      if (RES == 0 && ARG3 ) {
	 struct vki_ifconf *ifc = (struct vki_ifconf *) ARG3;
	 if (ifc->vki_ifc_buf != NULL)
	    POST_MEM_WRITE( (Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
   case VKI_SIOCGSTAMP:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_timeval) );
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case VKI_SIOCOUTQ:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SIOCGRARP:           /* get RARP table entry         */
   case VKI_SIOCGARP:            /* get ARP table entry          */
      POST_MEM_WRITE(ARG3, sizeof(struct vki_arpreq));
      break;
                    
   case VKI_SIOCSIFFLAGS:        /* set flags                    */
   case VKI_SIOCSIFMAP:          /* Set device parameters        */
   case VKI_SIOCSIFTXQLEN:       /* Set the tx queue length      */
   case VKI_SIOCSIFDSTADDR:      /* set remote PA address        */
   case VKI_SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case VKI_SIOCSIFNETMASK:      /* set network PA mask          */
   case VKI_SIOCSIFMETRIC:       /* set metric                   */
   case VKI_SIOCSIFADDR:         /* set PA address               */
   case VKI_SIOCSIFMTU:          /* set MTU size                 */
   case VKI_SIOCSIFHWADDR:       /* set hardware address         */
   case VKI_SIOCSMIIREG:         /* set hardware entry registers */
      break;
      /* Routing table calls.  */
   case VKI_SIOCADDRT:           /* add routing table entry      */
   case VKI_SIOCDELRT:           /* delete routing table entry   */
      break;

      /* RARP cache control calls. */
   case VKI_SIOCDRARP:           /* delete RARP table entry      */
   case VKI_SIOCSRARP:           /* set RARP table entry         */
      /* ARP cache control calls. */
   case VKI_SIOCSARP:            /* set ARP table entry          */
   case VKI_SIOCDARP:            /* delete ARP table entry       */
      break;

   case VKI_SIOCGPGRP:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SIOCSPGRP:
      break;

      /* linux/soundcard interface (OSS) */
   case VKI_SNDCTL_SEQ_GETOUTCOUNT:
   case VKI_SNDCTL_SEQ_GETINCOUNT:
   case VKI_SNDCTL_SEQ_PERCMODE:
   case VKI_SNDCTL_SEQ_TESTMIDI:
   case VKI_SNDCTL_SEQ_RESETSAMPLES:
   case VKI_SNDCTL_SEQ_NRSYNTHS:
   case VKI_SNDCTL_SEQ_NRMIDIS:
   case VKI_SNDCTL_SEQ_GETTIME:
   case VKI_SNDCTL_DSP_GETFMTS:
   case VKI_SNDCTL_DSP_GETTRIGGER:
   case VKI_SNDCTL_DSP_GETODELAY:
   case VKI_SNDCTL_DSP_GETSPDIF:
   case VKI_SNDCTL_DSP_GETCAPS:
   case VKI_SOUND_PCM_READ_RATE:
   case VKI_SOUND_PCM_READ_CHANNELS:
   case VKI_SOUND_PCM_READ_BITS:
#if !defined(VGA_ppc32)
   case (VKI_SOUND_PCM_READ_BITS|0x40000000): /* what the fuck ? */
#endif
   case VKI_SOUND_PCM_READ_FILTER:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SNDCTL_SEQ_CTRLRATE:
   case VKI_SNDCTL_DSP_SPEED:
   case VKI_SNDCTL_DSP_STEREO:
   case VKI_SNDCTL_DSP_GETBLKSIZE: 
   case VKI_SNDCTL_DSP_CHANNELS:
   case VKI_SOUND_PCM_WRITE_FILTER:
   case VKI_SNDCTL_DSP_SUBDIVIDE:
   case VKI_SNDCTL_DSP_SETFRAGMENT:
   case VKI_SNDCTL_DSP_GETCHANNELMASK:
   case VKI_SNDCTL_DSP_BIND_CHANNEL:
   case VKI_SNDCTL_TMR_TIMEBASE:
   case VKI_SNDCTL_TMR_TEMPO:
   case VKI_SNDCTL_TMR_SOURCE:
   case VKI_SNDCTL_MIDI_PRETIME:
   case VKI_SNDCTL_MIDI_MPUMODE:
      break;
   case VKI_SNDCTL_DSP_GETOSPACE:
   case VKI_SNDCTL_DSP_GETISPACE:
      POST_MEM_WRITE(ARG3, sizeof(vki_audio_buf_info));
      break;
   case VKI_SNDCTL_DSP_SETTRIGGER:
      break;

   case VKI_SNDCTL_DSP_POST:
   case VKI_SNDCTL_DSP_RESET:
   case VKI_SNDCTL_DSP_SYNC:
   case VKI_SNDCTL_DSP_SETSYNCRO:
   case VKI_SNDCTL_DSP_SETDUPLEX:
      break;

      /* Real Time Clock (/dev/rtc) ioctls */
   case VKI_RTC_UIE_ON:
   case VKI_RTC_UIE_OFF:
   case VKI_RTC_AIE_ON:
   case VKI_RTC_AIE_OFF:
   case VKI_RTC_PIE_ON:
   case VKI_RTC_PIE_OFF:
   case VKI_RTC_IRQP_SET:
      break;
   case VKI_RTC_RD_TIME:
   case VKI_RTC_ALM_READ:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_ALM_SET:
      break;
   case VKI_RTC_IRQP_READ:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;

      /* Block devices */
   case VKI_BLKROSET:
      break;
   case VKI_BLKROGET:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_BLKGETSIZE:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;
   case VKI_BLKRASET:
      break;
   case VKI_BLKRAGET:
      POST_MEM_WRITE(ARG3, sizeof(long));
      break;
   case VKI_BLKFRASET:
      break;
   case VKI_BLKFRAGET:
      POST_MEM_WRITE(ARG3, sizeof(long));
      break;
   case VKI_BLKSECTGET:
      POST_MEM_WRITE(ARG3, sizeof(unsigned short));
      break;
   case VKI_BLKSSZGET:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_BLKBSZGET:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_BLKBSZSET:
      break;
   case VKI_BLKGETSIZE64:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long long));
      break;

      /* Hard disks */
   case VKI_HDIO_GETGEO: /* 0x0301 */
      POST_MEM_WRITE(ARG3, sizeof(struct vki_hd_geometry));
      break;
   case VKI_HDIO_GET_IDENTITY: /* 0x030d */
      POST_MEM_WRITE(ARG3, VKI_SIZEOF_STRUCT_HD_DRIVEID );
      break;

      /* CD ROM stuff (??)  */
   case VKI_CDROMSUBCHNL:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_subchnl));
      break;
   case VKI_CDROMREADTOCHDR:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMREADTOCENTRY:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMMULTISESSION:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_multisession));
      break;
   case VKI_CDROMVOLREAD:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_volctrl));
      break;
   case VKI_CDROMREADAUDIO:
   {
      struct vki_cdrom_read_audio *cra = (struct vki_cdrom_read_audio *) ARG3;
      POST_MEM_WRITE( (Addr)(cra->buf), cra->nframes * VKI_CD_FRAMESIZE_RAW);
      break;
   }
      
   case VKI_CDROMPLAYMSF:
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case VKI_CDROM_DRIVE_STATUS: /* 0x5326 */
   case VKI_CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

   case VKI_FIGETBSZ:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;
   case VKI_FIBMAP:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;

   case VKI_FBIOGET_VSCREENINFO: //0x4600
      POST_MEM_WRITE(ARG3, sizeof(struct vki_fb_var_screeninfo));
      break;
   case VKI_FBIOGET_FSCREENINFO: //0x4602
      POST_MEM_WRITE(ARG3, sizeof(struct vki_fb_fix_screeninfo));
      break;

   case VKI_PPCLAIM:
   case VKI_PPEXCL:
   case VKI_PPYIELD:
   case VKI_PPRELEASE:
   case VKI_PPSETMODE:
   case VKI_PPSETPHASE:
   case VKI_PPSETFLAGS:
   case VKI_PPWDATA:
   case VKI_PPWCONTROL:
   case VKI_PPFCONTROL:
   case VKI_PPDATADIR:
   case VKI_PPNEGOT:
   case VKI_PPWCTLONIRQ:
   case VKI_PPSETTIME:
      break;
   case VKI_PPGETMODE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPGETPHASE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPGETMODES:
      POST_MEM_WRITE( ARG3, sizeof(unsigned int) );
      break;
   case VKI_PPGETFLAGS:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPRSTATUS:
      POST_MEM_WRITE( ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRDATA:
      POST_MEM_WRITE( ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRCONTROL:
      POST_MEM_WRITE( ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPCLRIRQ:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPGETTIME:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_timeval) );
      break;

   case VKI_GIO_FONT:
      POST_MEM_WRITE( ARG3, 32 * 256 );
      break;
   case VKI_PIO_FONT:
      break;

   case VKI_GIO_FONTX:
      POST_MEM_WRITE( (Addr)((struct vki_consolefontdesc *)ARG3)->chardata,
                      32 * ((struct vki_consolefontdesc *)ARG3)->charcount );
      break;
   case VKI_PIO_FONTX:
      break;

   case VKI_PIO_FONTRESET:
      break;

   case VKI_GIO_CMAP:
      POST_MEM_WRITE( ARG3, 16 * 3 );
      break;
   case VKI_PIO_CMAP:
      break;

   case VKI_KIOCSOUND:
   case VKI_KDMKTONE:
      break;

   case VKI_KDGETLED:
      POST_MEM_WRITE( ARG3, sizeof(char) );
      break;
   case VKI_KDSETLED:
      break;

   case VKI_KDGKBTYPE:
      POST_MEM_WRITE( ARG3, sizeof(char) );
      break;

   case VKI_KDADDIO:
   case VKI_KDDELIO:
   case VKI_KDENABIO:
   case VKI_KDDISABIO:
      break;

   case VKI_KDSETMODE:
      break;
   case VKI_KDGETMODE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;

   case VKI_KDMAPDISP:
   case VKI_KDUNMAPDISP:
      break;

   case VKI_GIO_SCRNMAP:
      POST_MEM_WRITE( ARG3, VKI_E_TABSZ );
      break;
   case VKI_PIO_SCRNMAP:
      break;
   case VKI_GIO_UNISCRNMAP:
      POST_MEM_WRITE( ARG3, VKI_E_TABSZ * sizeof(unsigned short) );
      break;
   case VKI_PIO_UNISCRNMAP:
      break;

   case VKI_KDGKBMODE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMODE:
      break;
      
   case VKI_KDGKBMETA:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMETA:
      break;
      
   case VKI_KDGKBLED:
      POST_MEM_WRITE( ARG3, sizeof(char) );
      break;
   case VKI_KDSKBLED:
      break;
      
   case VKI_KDGKBENT:
      POST_MEM_WRITE( (Addr)&((struct vki_kbentry *)ARG3)->kb_value,
                      sizeof(((struct vki_kbentry *)ARG3)->kb_value) );
      break;
   case VKI_KDSKBENT:
      break;
      
   case VKI_KDGKBSENT:
      POST_MEM_WRITE( (Addr)((struct vki_kbsentry *)ARG3)->kb_string,
                      sizeof(((struct vki_kbsentry *)ARG3)->kb_string) );
      break;
   case VKI_KDSKBSENT:
      break;
      
   case VKI_KDGKBDIACR:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_kbdiacrs) );
      break;
   case VKI_KDSKBDIACR:
      break;
      
   case VKI_KDGETKEYCODE:
      POST_MEM_WRITE( (Addr)((struct vki_kbkeycode *)ARG3)->keycode,
                      sizeof(((struct vki_kbkeycode *)ARG3)->keycode) );
      break;
   case VKI_KDSETKEYCODE:
      break;
      
   case VKI_KDSIGACCEPT:
      break;

   case VKI_KDKBDREP:
      break;

      /* We don't have any specific information on it, so
	 try to do something reasonable based on direction and
	 size bits.  The encoding scheme is described in
	 /usr/include/asm/ioctl.h.  

	 According to Simon Hausmann, _IOC_READ means the kernel
	 writes a value to the ioctl value passed from the user
	 space and the other way around with _IOC_WRITE. */
   default: {
      UInt dir  = _VKI_IOC_DIR(ARG2);
      UInt size = _VKI_IOC_SIZE(ARG2);
      if (size > 0 && (dir & _VKI_IOC_READ)
	  && RES == 0 
	  && ARG3 != (Addr)NULL)
	 POST_MEM_WRITE(ARG3, size);
      break;
   }
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

   tid = VG_(get_lwp_tid)(pid);
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
	 VG_(message)(Vg_DebugMsg, "Thread %d being killed with SIGKILL", 
                                   tst->tid);
      
      tst->exitreason = VgSrc_FatalSig;
      tst->os_state.fatalsig = VKI_SIGKILL;
      
      if (!VG_(is_running_thread)(tid))
	 VG_(kill_thread)(tid);
   }
   
   return True;
}

PRE(sys_kill)
{
   PRINT("sys_kill ( %d, %d )", ARG1,ARG2);
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
      SET_STATUS_from_SysRes( VG_(do_syscall2)(SYSNO, ARG1, ARG2) );

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "kill: sent signal %d to pid %d",
		   ARG2, ARG1);

   /* This kill might have given us a pending signal.  Ask for a check once 
      the syscall is done. */
   *flags |= SfPollAfter;
}

PRE(sys_link)
{
   *flags |= SfMayBlock;
   PRINT("sys_link ( %p, %p)", ARG1, ARG2);
   PRE_REG_READ2(long, "link", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "link(oldpath)", ARG1);
   PRE_MEM_RASCIIZ( "link(newpath)", ARG2);
}

PRE(sys_newlstat)
{
   PRINT("sys_newlstat ( %p(%s), %p )", ARG1,ARG1,ARG2);
   PRE_REG_READ2(long, "lstat", char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "lstat(file_name)", ARG1 );
   PRE_MEM_WRITE( "lstat(buf)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_newlstat)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
   }
}

PRE(sys_mkdir)
{
   *flags |= SfMayBlock;
   PRINT("sys_mkdir ( %p, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "mkdir", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "mkdir(pathname)", ARG1 );
}

PRE(sys_mprotect)
{
   PRINT("sys_mprotect ( %p, %llu, %d )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "mprotect",
                 unsigned long, addr, vki_size_t, len, unsigned long, prot);

   if (!ML_(valid_client_addr)(ARG1, ARG2, tid, "mprotect"))
      SET_STATUS_Failure( VKI_ENOMEM );
}

POST(sys_mprotect)
{
   Addr a    = ARG1;
   SizeT len = ARG2;
   Int  prot = ARG3;
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
                                 "POST(sys_mprotect)" );
}

PRE(sys_munmap)
{
   if (0) VG_(printf)("  munmap( %p )\n", ARG1);
   PRINT("sys_munmap ( %p, %llu )", ARG1,(ULong)ARG2);
   PRE_REG_READ2(long, "munmap", unsigned long, start, vki_size_t, length);

   if (!ML_(valid_client_addr)(ARG1, ARG2, tid, "munmap"))
      SET_STATUS_Failure( VKI_EINVAL );
}

POST(sys_munmap)
{
   Addr  a   = ARG1;
   SizeT len = ARG2;
   Bool  d;

   page_align_addr_and_len(&a, &len);
   d = VG_(am_notify_munmap)(a, len);
   VG_TRACK( die_mem_munmap, a, len );
   VG_(di_notify_munmap)( a, len );
   if (d)
      VG_(discard_translations)( (Addr64)a, (ULong)len,
                                 "POST(sys_munmap)" );
}

PRE(sys_mincore)
{
   PRINT("sys_mincore ( %p, %llu, %p )", ARG1,(ULong)ARG2,ARG3);
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
   PRINT("sys_nanosleep ( %p, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "nanosleep", 
                 struct timespec *, req, struct timespec *, rem);
   PRE_MEM_READ( "nanosleep(req)", ARG1, sizeof(struct vki_timespec) );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "nanosleep(rem)", ARG2, sizeof(struct vki_timespec) );
}

POST(sys_nanosleep)
{
   vg_assert(SUCCESS || FAILURE);
   if (ARG2 != 0 && FAILURE && RES_unchecked == VKI_EINTR)
      POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_open)
{
   HChar  name[30];
   SysRes sres;

   if (ARG2 & VKI_O_CREAT) {
      // 3-arg version
      PRINT("sys_open ( %p(%s), %d, %d )",ARG1,ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "open",
                    const char *, filename, int, flags, int, mode);
   } else {
      // 2-arg version
      PRINT("sys_open ( %p(%s), %d )",ARG1,ARG1,ARG2);
      PRE_REG_READ2(long, "open",
                    const char *, filename, int, flags);
   }
   PRE_MEM_RASCIIZ( "open(filename)", ARG1 );

   /* Handle the case where the open is of /proc/self/cmdline or
      /proc/<pid>/cmdline, and just give it a copy of the fd for the
      fake file we cooked up at startup (in m_main).  Also, seek the
      cloned fd back to the start. */

   VG_(sprintf)(name, "/proc/%d/cmdline", VG_(getpid)());
   if (ML_(safe_to_deref)( (void*)ARG1, 1 )
       && (VG_(strcmp)((Char *)ARG1, name) == 0 
           || VG_(strcmp)((Char *)ARG1, "/proc/self/cmdline") == 0)) {
      sres = VG_(dup)( VG_(cl_cmdline_fd) );
      SET_STATUS_from_SysRes( sres );
      if (!sres.isError) {
         OffT off = VG_(lseek)( sres.val, 0, VKI_SEEK_SET );
         if (off)
            SET_STATUS_Failure( VKI_EMFILE );
      }
      return;
   }

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
   PRINT("sys_read ( %d, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
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
   PRINT("sys_write ( %d, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "write",
                 unsigned int, fd, const char *, buf, vki_size_t, count);
   /* check to see if it is allowed.  If not, try for an exemption from
      --weird-hacks=enable-outer (used for self hosting). */
   ok = ML_(fd_allowed)(ARG1, "write", tid, False);
   if (!ok && ARG1 == 2/*stderr*/ 
           && VG_(strstr)(VG_(clo_weird_hacks),"enable-outer"))
      ok = True;
   if (!ok)
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_READ( "write(buf)", ARG2, ARG3 );
}

PRE(sys_creat)
{
   *flags |= SfMayBlock;
   PRINT("sys_creat ( %p(%s), %d )", ARG1,ARG1,ARG2);
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

// XXX: x86-specific, due to pollfd struct
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
   *flags |= SfMayBlock;
   struct vki_pollfd* ufds = (struct vki_pollfd *)ARG1;
   PRINT("sys_poll ( %p, %d, %d )\n", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "poll",
                 struct pollfd *, ufds, unsigned int, nfds, long, timeout);
                     
   for (i = 0; i < ARG2; i++) {
      // 'fd' and 'events' field are inputs;  'revents' is output.
      // XXX: this is x86 specific -- the pollfd struct varies across
      // different architectures.
      PRE_MEM_READ( "poll(ufds)",
                    (Addr)(&ufds[i]), sizeof(int) + sizeof(short) );
      PRE_MEM_WRITE( "poll(ufds)", (Addr)(&ufds[i].revents), sizeof(short) );
   }  
}

POST(sys_poll)
{
   if (RES > 0) {
      UInt i;
      struct vki_pollfd* ufds = (struct vki_pollfd *)ARG1;
      // XXX: again, this is x86-specific
      for (i = 0; i < ARG2; i++)
	 POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(Short) );
   }
}

PRE(sys_readlink)
{
   HChar name[25];
   Word  saved = SYSNO;

   PRINT("sys_readlink ( %p, %p, %llu )", ARG1,ARG2,(ULong)ARG3);
   PRE_REG_READ3(long, "readlink",
                 const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlink(path)", ARG1 );
   PRE_MEM_WRITE( "readlink(buf)", ARG2,ARG3 );

   /*
    * Handle the case where readlink is looking at /proc/self/exe or
    * /proc/<pid>/exe.
    */
   VG_(sprintf)(name, "/proc/%d/exe", VG_(getpid)());
   if (ML_(safe_to_deref)((void*)ARG1, 1)
       && (VG_(strcmp)((Char *)ARG1, name) == 0 
           || VG_(strcmp)((Char *)ARG1, "/proc/self/exe") == 0)) {
      VG_(sprintf)(name, "/proc/self/fd/%d", VG_(cl_exec_fd));
      SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, (UWord)name, 
                                                      ARG2, ARG3));
   } else {
      /* Normal case */
      SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, ARG1, ARG2, ARG3));
   }

   if (SUCCESS && RES > 0)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_readv)
{
   Int i;
   struct vki_iovec * vec;
   *flags |= SfMayBlock;
   PRINT("sys_readv ( %d, %p, %llu )",ARG1,ARG2,(ULong)ARG3);
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
   PRINT("sys_rename ( %p, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "rename", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "rename(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "rename(newpath)", ARG2 );
}

PRE(sys_rmdir)
{
   *flags |= SfMayBlock;
   PRINT("sys_rmdir ( %p )", ARG1);
   PRE_REG_READ1(long, "rmdir", const char *, pathname);
   PRE_MEM_RASCIIZ( "rmdir(pathname)", ARG1 );
}

PRE(sys_select)
{
   *flags |= SfMayBlock;
   PRINT("sys_select ( %d, %p, %p, %p, %p )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "select",
                 int, n, vki_fd_set *, readfds, vki_fd_set *, writefds,
                 vki_fd_set *, exceptfds, struct timeval *, timeout);
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
      PRE_MEM_READ( "select(timeout)", ARG5, sizeof(struct vki_timeval) );
}

PRE(sys_setgid)
{
   PRINT("sys_setgid ( %d )", ARG1);
   PRE_REG_READ1(long, "setgid", vki_gid_t, gid);
}

PRE(sys_setsid)
{
   PRINT("sys_setsid ( )");
   PRE_REG_READ0(long, "setsid");
}

PRE(sys_setgroups)
{
   PRINT("setgroups ( %llu, %p )", (ULong)ARG1, ARG2);
   PRE_REG_READ2(long, "setgroups", int, size, vki_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_READ( "setgroups(list)", ARG2, ARG1 * sizeof(vki_gid_t) );
}

PRE(sys_setpgid)
{
   PRINT("setpgid ( %d, %d )", ARG1, ARG2);
   PRE_REG_READ2(long, "setpgid", vki_pid_t, pid, vki_pid_t, pgid);
}

PRE(sys_setregid)
{
   PRINT("sys_setregid ( %d, %d )", ARG1, ARG2);
   PRE_REG_READ2(long, "setregid", vki_gid_t, rgid, vki_gid_t, egid);
}

PRE(sys_setreuid)
{
   PRINT("sys_setreuid ( 0x%x, 0x%x )", ARG1, ARG2);
   PRE_REG_READ2(long, "setreuid", vki_uid_t, ruid, vki_uid_t, euid);
}

PRE(sys_setrlimit)
{
   PRINT("sys_setrlimit ( %d, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "setrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_READ( "setrlimit(rlim)", ARG2, sizeof(struct vki_rlimit) );

   if (ARG1 == VKI_RLIMIT_NOFILE) {
      if (((struct vki_rlimit *)ARG2)->rlim_cur > VG_(fd_hard_limit) ||
          ((struct vki_rlimit *)ARG2)->rlim_max != VG_(fd_hard_limit)) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         VG_(fd_soft_limit) = ((struct vki_rlimit *)ARG2)->rlim_cur;
         SET_STATUS_Success( 0 );
      }
   }
   else if (ARG1 == VKI_RLIMIT_DATA) {
      if (((struct vki_rlimit *)ARG2)->rlim_cur > ((struct vki_rlimit *)ARG2)->rlim_max ||
          ((struct vki_rlimit *)ARG2)->rlim_max > ((struct vki_rlimit *)ARG2)->rlim_max) {
         SET_STATUS_Failure( VKI_EPERM );
      }
      else {
         VG_(client_rlimit_data) = *(struct vki_rlimit *)ARG2;
         SET_STATUS_Success( 0 );
      }
   }
   else if (ARG1 == VKI_RLIMIT_STACK && tid == 1) {
      if (((struct vki_rlimit *)ARG2)->rlim_cur > ((struct vki_rlimit *)ARG2)->rlim_max ||
          ((struct vki_rlimit *)ARG2)->rlim_max > ((struct vki_rlimit *)ARG2)->rlim_max) {
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
   PRINT("sys_setuid ( %d )", ARG1);
   PRE_REG_READ1(long, "setuid", vki_uid_t, uid);
}

PRE(sys_newstat)
{
   PRINT("sys_newstat ( %p(%s), %p )", ARG1,ARG1,ARG2);
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
   PRINT("sys_statfs ( %p, %p )",ARG1,ARG2);
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
   PRINT("sys_statfs64 ( %p, %llu, %p )",ARG1,(ULong)ARG2,ARG3);
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
   PRINT("sys_symlink ( %p, %p )",ARG1,ARG2);
   PRE_REG_READ2(long, "symlink", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "symlink(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "symlink(newpath)", ARG2 );
}

PRE(sys_time)
{
   /* time_t time(time_t *t); */
   PRINT("sys_time ( %p )",ARG1);
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
   PRINT("sys_times ( %p )", ARG1);
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
   PRINT("sys_umask ( %d )", ARG1);
   PRE_REG_READ1(long, "umask", int, mask);
}

PRE(sys_unlink)
{
   *flags |= SfMayBlock;
   PRINT("sys_unlink ( %p(%s) )", ARG1,ARG1);
   PRE_REG_READ1(long, "unlink", const char *, pathname);
   PRE_MEM_RASCIIZ( "unlink(pathname)", ARG1 );
}

PRE(sys_newuname)
{
   PRINT("sys_newuname ( %p )", ARG1);
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
   PRINT("sys_waitpid ( %d, %p, %d )", ARG1,ARG2,ARG3);
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
   PRINT("sys_wait4 ( %d, %p, %d, %p )", ARG1,ARG2,ARG3,ARG4);

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
   PRINT("sys_writev ( %d, %p, %llu )",ARG1,ARG2,(ULong)ARG3);
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
   PRINT("sys_utimes ( %p, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "utimes", char *, filename, struct timeval *, tvp);
   PRE_MEM_RASCIIZ( "utimes(filename)", ARG1 );
   if (ARG2 != 0)
      PRE_MEM_READ( "utimes(tvp)", ARG2, sizeof(struct vki_timeval) );
}

PRE(sys_acct)
{
   PRINT("sys_acct ( %p )", ARG1);
   PRE_REG_READ1(long, "acct", const char *, filename);
   PRE_MEM_RASCIIZ( "acct(filename)", ARG1 );
}

PRE(sys_pause)
{
   *flags |= SfMayBlock;
   PRINT("sys_pause ( )");
   PRE_REG_READ0(long, "pause");
}

//zz // XXX: x86-specific
//zz PRE(sys_sigsuspend, SfMayBlock)
//zz {
//zz    /* The C library interface to sigsuspend just takes a pointer to
//zz       a signal mask but this system call has three arguments - the first
//zz       two don't appear to be used by the kernel and are always passed as
//zz       zero by glibc and the third is the first word of the signal mask
//zz       so only 32 signals are supported.
//zz      
//zz       In fact glibc normally uses rt_sigsuspend if it is available as
//zz       that takes a pointer to the signal mask so supports more signals.
//zz     */
//zz    PRINT("sys_sigsuspend ( %d, %d, %d )", ARG1,ARG2,ARG3 );
//zz    PRE_REG_READ3(int, "sigsuspend",
//zz                  int, history0, int, history1,
//zz                  vki_old_sigset_t, mask);
//zz }

// XXX: x86-specific
PRE(sys_sigaltstack)
{
   PRINT("sigaltstack ( %p, %p )",ARG1,ARG2);
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

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

