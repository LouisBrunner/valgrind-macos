
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

#include "core.h"
#include "pub_core_debuglog.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcmman.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_main.h"
#include "pub_core_mallocfree.h"
#include "pub_core_stacktrace.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"

#include "vki_unistd.h"              /* for the __NR_* constants */


/* return true if address range entirely contained within client
   address space */
Bool VG_(valid_client_addr)(Addr start, SizeT size, ThreadId tid,
                                   const Char *syscallname)
{
   Addr end = start+size;
   Addr cl_base = VG_(client_base);
   Bool ret;

   if (size == 0)
      return True;

   if (0 && cl_base < 0x10000)
      cl_base = 0x10000;

   ret =
      (end >= start) && 
      start >= cl_base && start < VG_(client_end) &&
      (end <= VG_(client_end));

   if (0)
      VG_(printf)("%s: test=%p-%p client=%p-%p ret=%d\n",
		  syscallname, start, end, cl_base, VG_(client_end), ret);

   if (!ret && syscallname != NULL) {
      VG_(message)(Vg_UserMsg, "Warning: client syscall %s tried "
                               "to modify addresses %p-%p",
                               syscallname, start, end);

      if (VG_(clo_verbosity) > 1) {
         VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      }
   }

   return ret;
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
void mash_addr_and_len( Addr* a, SizeT* len)
{
   Addr ra;
   
   ra = VG_PGROUNDDN(*a);
   *len = VG_PGROUNDUP(*a + *len) - ra;
   *a = ra;
}

void VG_(mmap_segment) ( Addr a, SizeT len, UInt prot, 
                         UInt mm_flags, Int fd, ULong offset )
{
   Bool rr, ww, xx;
   UInt flags;

   flags = SF_MMAP;
   
   if (mm_flags & VKI_MAP_FIXED)
      flags |= SF_FIXED;
   if (!(mm_flags & VKI_MAP_PRIVATE))
      flags |= SF_SHARED;

   if (fd != -1)
      flags |= SF_FILE;

   VG_(map_fd_segment)(a, len, prot, flags, fd, offset, NULL);

   rr = prot & VKI_PROT_READ;
   ww = prot & VKI_PROT_WRITE;
   xx = prot & VKI_PROT_EXEC;

   VG_TRACK( new_mem_mmap, a, len, rr, ww, xx );
}

static 
SysRes mremap_segment ( Addr old_addr, SizeT old_size,
                        Addr new_addr, SizeT new_size,
                        UInt flags, ThreadId tid)
{
   SysRes ret;
   Segment *seg, *next;

   old_size = VG_PGROUNDUP(old_size);
   new_size = VG_PGROUNDUP(new_size);

   if (VG_PGROUNDDN(old_addr) != old_addr)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   if (!VG_(valid_client_addr)(old_addr, old_size, tid, "mremap(old_addr)"))
      return VG_(mk_SysRes_Error)( VKI_EFAULT );

   /* fixed at the current address means we don't move it */
   if ((flags & VKI_MREMAP_FIXED) && (old_addr == new_addr))
      flags &= ~(VKI_MREMAP_FIXED|VKI_MREMAP_MAYMOVE);

   if (flags & VKI_MREMAP_FIXED) {
      if (VG_PGROUNDDN(new_addr) != new_addr)
	 return VG_(mk_SysRes_Error)( VKI_EINVAL );

      if (!VG_(valid_client_addr)(new_addr, new_size, tid, "mremap(new_addr)"))
	 return VG_(mk_SysRes_Error)( VKI_ENOMEM );

      /* check for overlaps */
      if ((old_addr < (new_addr+new_size) &&
	   (old_addr+old_size) > new_addr) ||
	  (new_addr < (old_addr+new_size) &&
	   (new_addr+new_size) > old_addr))
	 return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Do nothing */
   if (!(flags & VKI_MREMAP_FIXED) && new_size == old_size)
      return VG_(mk_SysRes_Success)( old_addr );

   seg = VG_(find_segment)(old_addr);

   /* range must be contained within segment */
   if (seg == NULL || !VG_(seg_contains)(seg, old_addr, old_size))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   next = VG_(find_segment_above_mapped)(old_addr);

   if (0)
      VG_(printf)("mremap: old_addr+new_size=%p next->addr=%p flags=%d\n",
		  old_addr+new_size, next->addr, flags);
   
   if ((flags & VKI_MREMAP_FIXED) ||
       (next != NULL && (old_addr+new_size) > next->addr)) {
      /* we're moving the block */
      Addr a;
      
      if ((flags & (VKI_MREMAP_FIXED|VKI_MREMAP_MAYMOVE)) == 0)
         /* not allowed to move */
	 return VG_(mk_SysRes_Error)( VKI_ENOMEM ); 

      if ((flags & VKI_MREMAP_FIXED) == 0)
	  new_addr = 0;

      a = VG_(find_map_space)(new_addr, new_size, True);

      if ((flags & VKI_MREMAP_FIXED) && a != new_addr)
         /* didn't find the place we wanted */
	 return VG_(mk_SysRes_Error)( VKI_ENOMEM );

      new_addr = a;

      /* we've nailed down the location */
      flags |= VKI_MREMAP_FIXED|VKI_MREMAP_MAYMOVE;

      ret = VG_(do_syscall5)(__NR_mremap, old_addr, old_size, new_size, 
			     flags, new_addr);

      if (ret.isError) {
	 return ret;
      }

      VG_TRACK(copy_mem_remap, old_addr, new_addr, 
	       (old_size < new_size) ? old_size : new_size);

      if (new_size > old_size)
	 VG_TRACK(new_mem_mmap, new_addr+old_size, new_size-old_size,
		  seg->prot & VKI_PROT_READ, 
		  seg->prot & VKI_PROT_WRITE, 
		  seg->prot & VKI_PROT_EXEC);
      VG_TRACK(die_mem_munmap, old_addr, old_size);

      VG_(map_file_segment)(new_addr, new_size,
			    seg->prot, 
			    seg->flags,
			    seg->dev, seg->ino,
			    seg->offset, seg->filename);

      VG_(munmap)((void *)old_addr, old_size);
   } else {
      /* staying in place */
      ret = VG_(mk_SysRes_Success)( old_addr );

      if (new_size < old_size) {
	 VG_TRACK(die_mem_munmap, old_addr+new_size, old_size-new_size);
	 VG_(munmap)((void *)(old_addr+new_size), old_size-new_size);
      } else {
	 /* we've nailed down the location */
	 flags &= ~VKI_MREMAP_MAYMOVE;

	 if (0)
	    VG_(printf)("mremap: old_addr=%p old_size=%d new_size=%d flags=%d\n",
			old_addr, old_size, new_size, flags);

	 ret = VG_(do_syscall5)(__NR_mremap, old_addr, old_size, new_size, 
			        flags, 0);

	 if (ret.isError || (!ret.isError && ret.val != old_addr))
	    return ret;

	 VG_TRACK(new_mem_mmap, old_addr+old_size, new_size-old_size,
		  seg->prot & VKI_PROT_READ, 
		  seg->prot & VKI_PROT_WRITE, 
		  seg->prot & VKI_PROT_EXEC);

	 VG_(map_file_segment)(old_addr+old_size, new_size-old_size,
			       seg->prot, 
			       seg->flags,
			       seg->dev, seg->ino,
			       seg->offset, seg->filename);	 
      }
   }

   return ret;
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

static int fd_count = 0;



/* Given a file descriptor, attempt to deduce its filename.  To do
   this, we use /proc/self/fd/<FD>.  If this doesn't point to a file,
   or if it doesn't exist, we just return NULL.  The caller is
   responsible for copying the contents of buf out immediately. */

static HChar resolve_filename_buf[VKI_PATH_MAX];

HChar* VG_(resolve_filename_nodup) ( Int fd )
{
   HChar tmp[64];

   VG_(sprintf)(tmp, "/proc/self/fd/%d", fd);
   VG_(memset)(resolve_filename_buf, 0, VKI_PATH_MAX);

   if (VG_(readlink)(tmp, resolve_filename_buf, VKI_PATH_MAX) == -1)
      return NULL;

   return (resolve_filename_buf[0] == '/') 
             ? resolve_filename_buf 
             : NULL;
}

/* Same as resolve_filename_nodup, except that the result is copied 
   into new memory which the caller is responsible for freeing. */

HChar* VG_(resolve_filename) ( Int fd )
{
   HChar* transient = VG_(resolve_filename_nodup)(fd);
   return transient
             ? VG_(arena_strdup)(VG_AR_CORE, transient)
             : NULL;
}


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

void VG_(record_fd_open)(ThreadId tid, Int fd, char *pathname)
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
   i->pathname = pathname;
   i->where = (tid == -1) ? NULL : VG_(record_ExeContext)(tid);
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
getsockdetails(int fd)
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
         int val;
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
   unsigned int count;
   int i;

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
         VG_(record_fd_open)(-1, i, NULL);
}

/* Initialize the list of open file descriptors with the file descriptors
   we inherited from out parent process. */

void VG_(init_preopened_fds)()
{
   int f, ret;
   struct vki_dirent d;

   f = VG_(open)("/proc/self/fd", VKI_O_RDONLY, 0);
   if(f == -1) {
      do_hacky_preopened();
      return;
   }

   while((ret = VG_(getdents)(f, &d, sizeof(d))) != 0) {
      if(ret == -1)
         goto out;

      if(VG_(strcmp)(d.d_name, ".") && VG_(strcmp)(d.d_name, "..")) {
         int fno = VG_(atoll)(d.d_name);

         if(fno != f)
            if(VG_(clo_track_fds))
               VG_(record_fd_open)(-1, fno, VG_(resolve_filename)(fno));
      }

      VG_(lseek)(f, d.d_off, VKI_SEEK_SET);
   }

out:
   VG_(close)(f);
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
void pre_mem_read_sendmsg ( ThreadId tid,
                            Char *msg, Addr base, SizeT size )
{
   Char *outmsg = strdupcat ( "socketcall.sendmsg", msg, VG_AR_CORE );
   PRE_MEM_READ( outmsg, base, size );

   VG_(arena_free) ( VG_AR_CORE, outmsg );
}

static 
void pre_mem_write_recvmsg ( ThreadId tid,
                             Char *msg, Addr base, SizeT size )
{
   Char *outmsg = strdupcat ( "socketcall.recvmsg", msg, VG_AR_CORE );
   PRE_MEM_WRITE( outmsg, base, size );
   VG_(arena_free) ( VG_AR_CORE, outmsg );
}

static
void post_mem_write_recvmsg ( ThreadId tid,
                              Char *fieldName, Addr base, SizeT size )
{
   POST_MEM_WRITE( base, size );
}
 
static
void msghdr_foreachfield ( 
        ThreadId tid, 
        struct vki_msghdr *msg, 
        void (*foreach_func)( ThreadId, Char *, Addr, SizeT ) 
     )
{
   if ( !msg )
      return;

   foreach_func ( tid, "(msg)", (Addr)msg, sizeof( struct vki_msghdr ) );

   if ( msg->msg_name )
      foreach_func ( tid, 
                     "(msg.msg_name)", 
                     (Addr)msg->msg_name, msg->msg_namelen );

   if ( msg->msg_iov ) {
      struct vki_iovec *iov = msg->msg_iov;
      UInt i;

      foreach_func ( tid, 
                     "(msg.msg_iov)", 
                     (Addr)iov, msg->msg_iovlen * sizeof( struct vki_iovec ) );

      for ( i = 0; i < msg->msg_iovlen; ++i, ++iov )
         foreach_func ( tid, 
                        "(msg.msg_iov[i]", 
                        (Addr)iov->iov_base, iov->iov_len );
   }

   if ( msg->msg_control )
      foreach_func ( tid, 
                     "(msg.msg_control)", 
                     (Addr)msg->msg_control, msg->msg_controllen );
}

static void check_cmsg_for_fds(ThreadId tid, struct vki_msghdr *msg)
{
   struct vki_cmsghdr *cm = VKI_CMSG_FIRSTHDR(msg);

   while (cm) {
      if (cm->cmsg_level == VKI_SOL_SOCKET &&
          cm->cmsg_type == VKI_SCM_RIGHTS ) {
         int *fds = (int *) VKI_CMSG_DATA(cm);
         int fdc = (cm->cmsg_len - VKI_CMSG_ALIGN(sizeof(struct vki_cmsghdr)))
                         / sizeof(int);
         int i;

         for (i = 0; i < fdc; i++)
            if(VG_(clo_track_fds))
               // XXX: must we check the range on these fds with
               //      VG_(fd_allowed)()?
               VG_(record_fd_open) (tid, fds[i], VG_(resolve_filename)(fds[i]));
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

static Addr do_brk(Addr newbrk)
{
   Addr ret = VG_(brk_limit);
   static const Bool debug = False;
   Segment *seg;
   Addr current, newaddr;


   if (debug)
      VG_(printf)("\ndo_brk: brk_base=%p brk_limit=%p newbrk=%p\n",
		  VG_(brk_base), VG_(brk_limit), newbrk);

#  if 0
   if (0) show_segments("in_brk");
#  endif

   if (newbrk < VG_(brk_base) || newbrk >= VG_(client_end))
      return VG_(brk_limit);

   /* brk isn't allowed to grow over anything else */
   seg = VG_(find_segment)(VG_(brk_limit) -1);

   vg_assert(seg != NULL);

   if (0)
      VG_(printf)("brk_limit=%p seg->addr=%p seg->end=%p\n", 
		  VG_(brk_limit), seg->addr, seg->addr+seg->len);
   vg_assert(VG_(brk_limit) >= seg->addr && VG_(brk_limit) 
             <= (seg->addr + seg->len));

   seg = VG_(find_segment_above_mapped)(VG_(brk_limit)-1);
   if (0 && seg) 
      VG_(printf)("NEXT addr = %p\n", seg->addr);
   if (seg != NULL && newbrk > seg->addr)
      return VG_(brk_limit);

   current = VG_PGROUNDUP(VG_(brk_limit));
   newaddr = VG_PGROUNDUP(newbrk);
   if (newaddr != current) {

      /* new brk in a new page - fix the mappings */
      if (newbrk > VG_(brk_limit)) {
	 
	 if (debug)
	    VG_(printf)("  extending brk: current=%p newaddr=%p delta=%d\n",
			current, newaddr, newaddr-current);

	 if (newaddr == current) {
	    ret = newbrk;
         } else if ((void*)-1 != VG_(mmap)((void*)current, newaddr-current,
               VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC,
               VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS|VKI_MAP_FIXED|VKI_MAP_CLIENT,
               SF_FIXED|SF_BRK, -1, 0)) 
         {
	    ret = newbrk;
	 }
      } else {
	 vg_assert(newbrk < VG_(brk_limit));

	 if (debug)
	    VG_(printf)("  shrinking brk: current=%p newaddr=%p delta=%d\n",
			current, newaddr, current-newaddr);

	 if (newaddr != current) {
	    int res = VG_(munmap)((void *)newaddr, current - newaddr);
            vg_assert(0 == res);
	 }
	 ret = newbrk;
      }
   } else
      ret = newbrk;

   VG_(brk_limit) = ret;

   return ret;
}


/* ---------------------------------------------------------------------
   Vet file descriptors for sanity
   ------------------------------------------------------------------ */

/* Return true if we're allowed to use or create this fd */
Bool VG_(fd_allowed)(Int fd, const Char *syscallname, ThreadId tid, Bool soft)
{
   if (fd < 0 || fd >= VG_(fd_hard_limit) || fd == VG_(clo_log_fd)) {
      VG_(message)(Vg_UserMsg, 
         "Warning: invalid file descriptor %d in syscall %s()",
         fd, syscallname);
      if (fd == VG_(clo_log_fd))
	 VG_(message)(Vg_UserMsg, 
            "   Use --log-fd=<number> to select an alternative log fd.");
      if (VG_(clo_verbosity) > 1) {
         VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      }
      return False;
   }
   else 
   if (soft && fd >= VG_(fd_soft_limit)) {
      return False;
   }
   else 
   if (fd == 2 && VG_(debugLog_getLevel)() > 0) {
      return False;
   } 
   else {
      return True;
   }
}


/* ---------------------------------------------------------------------
   Deal with a bunch of socket-related syscalls
   ------------------------------------------------------------------ */

/* ------ */

void 
VG_(generic_PRE_sys_socketpair) ( ThreadId tid,
                                  UWord arg0, UWord arg1, 
                                  UWord arg2, UWord arg3 )
{
   /* int socketpair(int d, int type, int protocol, int sv[2]); */
   PRE_MEM_WRITE( "socketcall.socketpair(sv)", 
                  arg3, 2*sizeof(int) );
}

SysRes
VG_(generic_POST_sys_socketpair) ( ThreadId tid,
                                   SysRes res,
                                   UWord arg0, UWord arg1, 
                                   UWord arg2, UWord arg3 )
{
   SysRes r = res;
   vg_assert(!res.isError); /* guaranteed by caller */
   Int fd1 = ((Int*)arg3)[0];
   Int fd2 = ((Int*)arg3)[1];
   POST_MEM_WRITE( arg3, 2*sizeof(int) );
   if (!VG_(fd_allowed)(fd1, "socketcall.socketpair", tid, True) ||
       !VG_(fd_allowed)(fd2, "socketcall.socketpair", tid, True)) {
      VG_(close)(fd1);
      VG_(close)(fd2);
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      POST_MEM_WRITE( arg3, 2*sizeof(int) );
      if (VG_(clo_track_fds)) {
         VG_(record_fd_open)(tid, fd1, NULL);
         VG_(record_fd_open)(tid, fd2, NULL);
      }
   }
   return r;
}

/* ------ */

SysRes 
VG_(generic_POST_sys_socket) ( ThreadId tid, SysRes res )
{
   SysRes r = res;
   vg_assert(!res.isError); /* guaranteed by caller */
   if (!VG_(fd_allowed)(res.val, "socket", tid, True)) {
      VG_(close)(res.val);
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, res.val, NULL);
   }
   return r;
}

/* ------ */

void 
VG_(generic_PRE_sys_bind) ( ThreadId tid,
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
VG_(generic_PRE_sys_accept) ( ThreadId tid,
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
VG_(generic_POST_sys_accept) ( ThreadId tid,
                               SysRes res,
                               UWord arg0, UWord arg1, UWord arg2 )
{
   SysRes r = res;
   vg_assert(!res.isError); /* guaranteed by caller */
   if (!VG_(fd_allowed)(res.val, "accept", tid, True)) {
      VG_(close)(res.val);
      r = VG_(mk_SysRes_Error)( VKI_EMFILE );
   } else {
      Addr addr_p     = arg1;
      Addr addrlen_p  = arg2;
      if (addr_p != (Addr)NULL) 
         buf_and_len_post_check ( tid, res, addr_p, addrlen_p,
                                  "socketcall.accept(addrlen_out)" );
      if (VG_(clo_track_fds))
          VG_(record_fd_open)(tid, res.val, NULL);
   }
   return r;
}

/* ------ */

void 
VG_(generic_PRE_sys_sendto) ( ThreadId tid, 
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
VG_(generic_PRE_sys_send) ( ThreadId tid,
                            UWord arg0, UWord arg1, UWord arg2 )
{
   /* int send(int s, const void *msg, size_t len, int flags); */
   PRE_MEM_READ( "socketcall.send(msg)",
                  arg1, /* msg */
                  arg2  /* len */ );

}

/* ------ */

void 
VG_(generic_PRE_sys_recvfrom) ( ThreadId tid, 
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
VG_(generic_POST_sys_recvfrom) ( ThreadId tid,
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
VG_(generic_PRE_sys_recv) ( ThreadId tid,
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
VG_(generic_POST_sys_recv) ( ThreadId tid, 
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
VG_(generic_PRE_sys_connect) ( ThreadId tid,
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
VG_(generic_PRE_sys_setsockopt) ( ThreadId tid, 
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
VG_(generic_PRE_sys_getsockopt) ( ThreadId tid, 
                                  UWord arg0, UWord arg1, UWord arg2,
                                  UWord arg3, UWord arg4 )
{
   /* int getsockopt(int s, int level, int optname, 
                     void *optval, socklen_t *optlen); */
   Addr optval_p  = arg3;
   Addr optlen_p  = arg4;
   /* vg_assert(sizeof(socklen_t) == sizeof(UInt)); */
   if (optval_p != (Addr)NULL) 
      buf_and_len_pre_check ( tid, optval_p, optlen_p,
                              "socketcall.getsockopt(optval)",
                              "socketcall.getsockopt(optlen)" );
}

void 
VG_(generic_POST_sys_getsockopt) ( ThreadId tid,
                                   SysRes res,
                                   UWord arg0, UWord arg1, UWord arg2,
                                   UWord arg3, UWord arg4 )
{
   Addr optval_p  = arg3;
   Addr optlen_p  = arg4;
   vg_assert(!res.isError); /* guaranteed by caller */
   if (optval_p != (Addr)NULL) 
      buf_and_len_post_check ( tid, res, optval_p, optlen_p,
                               "socketcall.getsockopt(optlen_out)" );
}

/* ------ */

void 
VG_(generic_PRE_sys_getsockname) ( ThreadId tid,
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
VG_(generic_POST_sys_getsockname) ( ThreadId tid,
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
VG_(generic_PRE_sys_getpeername) ( ThreadId tid,
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
VG_(generic_POST_sys_getpeername) ( ThreadId tid,
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
VG_(generic_PRE_sys_sendmsg) ( ThreadId tid,
                               UWord arg0, UWord arg1 )
{
   /* int sendmsg(int s, const struct msghdr *msg, int flags); */
   struct vki_msghdr *msg = (struct vki_msghdr *)arg1;
   msghdr_foreachfield ( tid, msg, pre_mem_read_sendmsg );
}

/* ------ */

void
VG_(generic_PRE_sys_recvmsg) ( ThreadId tid,
                               UWord arg0, UWord arg1 )
{
   /* int recvmsg(int s, struct msghdr *msg, int flags); */
   struct vki_msghdr *msg = (struct vki_msghdr *)arg1;
   msghdr_foreachfield ( tid, msg, pre_mem_write_recvmsg );
}

void 
VG_(generic_POST_sys_recvmsg) ( ThreadId tid,
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
VG_(generic_PRE_sys_semop) ( ThreadId tid,
                             UWord arg0, UWord arg1, UWord arg2 )
{
   /* int semop(int semid, struct sembuf *sops, unsigned nsops); */
   PRE_MEM_READ( "semop(sops)", arg1, arg2 * sizeof(struct vki_sembuf) );
}

/* ------ */

void
VG_(generic_PRE_sys_semtimedop) ( ThreadId tid,
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
VG_(generic_PRE_sys_semctl) ( ThreadId tid,
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
VG_(generic_POST_sys_semctl) ( ThreadId tid,
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

void
VG_(generic_PRE_sys_msgsnd) ( ThreadId tid,
                              UWord arg0, UWord arg1,
                              UWord arg2, UWord arg3 )
{
   /* int msgsnd(int msqid, struct msgbuf *msgp, size_t msgsz, int msgflg); */
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)arg1;
   PRE_MEM_READ( "msgsnd(msgp->mtype)", (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   PRE_MEM_READ( "msgsnd(msgp->mtext)", (Addr)&msgp->mtext, arg2 );
}

/* ------ */

void
VG_(generic_PRE_sys_msgrcv) ( ThreadId tid,
                              UWord arg0, UWord arg1, UWord arg2,
                              UWord arg3, UWord arg4 )
{
   /* ssize_t msgrcv(int msqid, struct msgbuf *msgp, size_t msgsz,
                     long msgtyp, int msgflg); */
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)arg1;
   PRE_MEM_WRITE( "msgrcv(msgp->mtype)", (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   PRE_MEM_WRITE( "msgrcv(msgp->mtext)", (Addr)&msgp->mtext, arg2 );
}

void
VG_(generic_POST_sys_msgrcv) ( ThreadId tid,
                               UWord res,
                               UWord arg0, UWord arg1, UWord arg2,
                               UWord arg3, UWord arg4 )
{
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)arg1;
   POST_MEM_WRITE( (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   POST_MEM_WRITE( (Addr)&msgp->mtext, res );
}

/* ------ */

void
VG_(generic_PRE_sys_msgctl) ( ThreadId tid,
                              UWord arg0, UWord arg1, UWord arg2 )
{
   /* int msgctl(int msqid, int cmd, struct msqid_ds *buf); */
   switch (arg1 /* cmd */) {
   case VKI_IPC_INFO:
   case VKI_MSG_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_MSG_INFO|VKI_IPC_64:
      PRE_MEM_WRITE( "msgctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_msginfo) );
      break;
   case VKI_IPC_STAT:
   case VKI_MSG_STAT:
      PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)",
                     arg2, sizeof(struct vki_msqid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_MSG_STAT|VKI_IPC_64:
      PRE_MEM_WRITE( "msgctl(IPC_STAT, arg.buf)",
                     arg2, sizeof(struct vki_msqid64_ds) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "msgctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_msqid_ds) );
      break;
   case VKI_IPC_SET|VKI_IPC_64:
      PRE_MEM_READ( "msgctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_msqid64_ds) );
      break;
   }
}

void
VG_(generic_POST_sys_msgctl) ( ThreadId tid,
                               UWord res,
                               UWord arg0, UWord arg1, UWord arg2 )
{
   switch (arg1 /* cmd */) {
   case VKI_IPC_INFO:
   case VKI_MSG_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_MSG_INFO|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_msginfo) );
      break;
   case VKI_IPC_STAT:
   case VKI_MSG_STAT:
      POST_MEM_WRITE( arg2, sizeof(struct vki_msqid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_MSG_STAT|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_msqid64_ds) );
      break;
   }
}

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
VG_(generic_PRE_sys_shmat) ( ThreadId tid,
                             UWord arg0, UWord arg1, UWord arg2 )
{
   /* void *shmat(int shmid, const void *shmaddr, int shmflg); */
   UInt segmentSize = get_shm_size ( arg0 );
   if (arg1 == 0)
      arg1 = VG_(find_map_space)(0, segmentSize, True);
   else if (!VG_(valid_client_addr)(arg1, segmentSize, tid, "shmat"))
      arg1 = 0;
   return arg1;
}

void
VG_(generic_POST_sys_shmat) ( ThreadId tid,
                              UWord res,
                              UWord arg0, UWord arg1, UWord arg2 )
{
   UInt segmentSize = get_shm_size ( arg0 );
   if ( segmentSize > 0 ) {
      UInt prot = VKI_PROT_READ|VKI_PROT_WRITE;
      /* we don't distinguish whether it's read-only or
       * read-write -- it doesn't matter really. */
      VG_TRACK( new_mem_mmap, res, segmentSize, True, True, False );

      if (!(arg2 & 010000)) /* = SHM_RDONLY */
         prot &= ~VKI_PROT_WRITE;
      VG_(map_segment)(res, segmentSize, prot, SF_SHARED|SF_SHM);
   }
}

/* ------ */

Bool
VG_(generic_PRE_sys_shmdt) ( ThreadId tid, UWord arg0 )
{
   /* int shmdt(const void *shmaddr); */
   return VG_(valid_client_addr)(arg0, 1, tid, "shmdt");
}

void
VG_(generic_POST_sys_shmdt) ( ThreadId tid, UWord res, UWord arg0 )
{
   Segment *s = VG_(find_segment)(arg0);

   if (s != NULL && (s->flags & SF_SHM) && VG_(seg_contains)(s, arg0, 1)) {
      VG_TRACK( die_mem_munmap, s->addr, s->len );
      VG_(unmap_range)(s->addr, s->len);
   }
}
/* ------ */

void
VG_(generic_PRE_sys_shmctl) ( ThreadId tid,
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
VG_(generic_POST_sys_shmctl) ( ThreadId tid,
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
   The Main Entertainment ... syscall wrappers
   ------------------------------------------------------------------ */

/* Note: the PRE() and POST() wrappers are for the actual functions
   implementing the system calls in the OS kernel.  These mostly have
   names like sys_write();  a few have names like old_mmap().  See the
   comment for VGA_(syscall_table)[] for important info about the __NR_foo
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

PRE(sys_sched_yield)
{
   *flags |= SfMayBlock;
   PRINT("sched_yield()");
   PRE_REG_READ0(long, "sys_sched_yield");
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

PRE(sys_setxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_setxattr ( %p, %p, %p, %llu, %d )",
         ARG1, ARG2, ARG3, (ULong)ARG4, ARG5);
   PRE_REG_READ5(long, "setxattr",
                 char *, path, char *, name,
                 void *, value, vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "setxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "setxattr(name)", ARG2 );
   PRE_MEM_READ( "setxattr(value)", ARG3, ARG4 );
}

PRE(sys_lsetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_lsetxattr ( %p, %p, %p, %llu, %d )",
         ARG1, ARG2, ARG3, (ULong)ARG4, ARG5);
   PRE_REG_READ5(long, "lsetxattr",
                 char *, path, char *, name,
                 void *, value, vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "lsetxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "lsetxattr(name)", ARG2 );
   PRE_MEM_READ( "lsetxattr(value)", ARG3, ARG4 );
}

PRE(sys_fsetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_fsetxattr ( %d, %p, %p, %llu, %d )",
         ARG1, ARG2, ARG3, (ULong)ARG4, ARG5);
   PRE_REG_READ5(long, "fsetxattr",
                 int, fd, char *, name, void *, value,
                 vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "fsetxattr(name)", ARG2 );
   PRE_MEM_READ( "fsetxattr(value)", ARG3, ARG4 );
}

PRE(sys_getxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_getxattr ( %p, %p, %p, %llu )", ARG1,ARG2,ARG3, (ULong)ARG4);
   PRE_REG_READ4(ssize_t, "getxattr",
                 char *, path, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "getxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "getxattr(name)", ARG2 );
   PRE_MEM_WRITE( "getxattr(value)", ARG3, ARG4 );
}
POST(sys_getxattr)
{
   vg_assert(SUCCESS);
   if (RES > 0 && ARG3 != (Addr)NULL) {
      POST_MEM_WRITE( ARG3, RES );
   }
}

PRE(sys_lgetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_lgetxattr ( %p, %p, %p, %llu )", ARG1,ARG2,ARG3, (ULong)ARG4);
   PRE_REG_READ4(ssize_t, "lgetxattr",
                 char *, path, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "lgetxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "lgetxattr(name)", ARG2 );
   PRE_MEM_WRITE( "lgetxattr(value)", ARG3, ARG4 );
}
POST(sys_lgetxattr)
{
   vg_assert(SUCCESS);
   if (RES > 0 && ARG3 != (Addr)NULL) {
      POST_MEM_WRITE( ARG3, RES );
   }
}

PRE(sys_fgetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_fgetxattr ( %d, %p, %p, %llu )", ARG1, ARG2, ARG3, (ULong)ARG4);
   PRE_REG_READ4(ssize_t, "fgetxattr",
                 int, fd, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "fgetxattr(name)", ARG2 );
   PRE_MEM_WRITE( "fgetxattr(value)", ARG3, ARG4 );
}
POST(sys_fgetxattr)
{
   if (RES > 0 && ARG3 != (Addr)NULL)
      POST_MEM_WRITE( ARG3, RES );
}

PRE(sys_listxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_listxattr ( %p, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "listxattr",
                 char *, path, char *, list, vki_size_t, size);
   PRE_MEM_RASCIIZ( "listxattr(path)", ARG1 );
   PRE_MEM_WRITE( "listxattr(list)", ARG2, ARG3 );
}
POST(sys_listxattr)
{
   if (RES > 0 && ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_llistxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_llistxattr ( %p, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "llistxattr",
                 char *, path, char *, list, vki_size_t, size);
   PRE_MEM_RASCIIZ( "llistxattr(path)", ARG1 );
   PRE_MEM_WRITE( "llistxattr(list)", ARG2, ARG3 );
}
POST(sys_llistxattr)
{
   if (RES > 0 && ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_flistxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_flistxattr ( %d, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "flistxattr",
                 int, fd, char *, list, vki_size_t, size);
   PRE_MEM_WRITE( "flistxattr(list)", ARG2, ARG3 );
}
POST(sys_flistxattr)
{
   if (RES > 0 && ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_removexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_removexattr ( %p, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "removexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "removexattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "removexattr(name)", ARG2 );
}

PRE(sys_lremovexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_lremovexattr ( %p, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "lremovexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "lremovexattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "lremovexattr(name)", ARG2 );
}

PRE(sys_fremovexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_fremovexattr ( %d, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "fremovexattr", int, fd, char *, name);
   PRE_MEM_RASCIIZ( "fremovexattr(name)", ARG2 );
}

PRE(sys_quotactl)
{
   PRINT("sys_quotactl (0x%x, %p, 0x%x, 0x%x )", ARG1,ARG2,ARG3, ARG4);
   PRE_REG_READ4(long, "quotactl",
                 unsigned int, cmd, const char *, special, vki_qid_t, id,
                 void *, addr);
   PRE_MEM_RASCIIZ( "quotactl(special)", ARG2 );
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
      mremap_segment((Addr)ARG1, ARG2, (Addr)ARG5, ARG3, ARG4, tid) 
   );
}

PRE(sys_nice)
{
   PRINT("sys_nice ( %d )", ARG1);
   PRE_REG_READ1(long, "nice", int, inc);
}

PRE(sys_sched_getscheduler)
{
   PRINT("sys_sched_getscheduler ( %d )", ARG1);
   PRE_REG_READ1(long, "sched_getscheduler", vki_pid_t, pid);
}

PRE(sys_sched_setscheduler)
{
   PRINT("sys_sched_setscheduler ( %d, %d, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sched_setscheduler", 
                 vki_pid_t, pid, int, policy, struct sched_param *, p);
   if (ARG3 != 0)
      PRE_MEM_READ( "sched_setscheduler(p)", 
		    ARG3, sizeof(struct vki_sched_param));
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

PRE(sys_munlockall)
{
   *flags |= SfMayBlock;
   PRINT("sys_munlockall ( )");
   PRE_REG_READ0(long, "munlockall");
}

PRE(sys_sched_get_priority_max)
{
   PRINT("sched_get_priority_max ( %d )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_max", int, policy);
}

PRE(sys_sched_get_priority_min)
{
   PRINT("sched_get_priority_min ( %d )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_min", int, policy);
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

PRE(sys_setregid16)
{
   PRINT("sys_setregid16 ( %d, %d )", ARG1, ARG2);
   PRE_REG_READ2(long, "setregid16", vki_old_gid_t, rgid, vki_old_gid_t, egid);
}

// XXX: only for 32-bit archs
#if defined(VGP_x86_linux)
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
#endif

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

// XXX: only for 32-bit archs
// XXX even more: this in fact gets used by amd64-linux.  Someone
// should look into this properly.
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

PRE(sys_capget)
{
   PRINT("sys_capget ( %p, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "capget", 
                 vki_cap_user_header_t, header, vki_cap_user_data_t, data);
   PRE_MEM_READ( "capget(header)", ARG1, 
                  sizeof(struct __vki_user_cap_header_struct) );
   PRE_MEM_WRITE( "capget(data)", ARG2, 
                  sizeof(struct __vki_user_cap_data_struct) );
}
POST(sys_capget)
{
   if (ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, sizeof(struct __vki_user_cap_data_struct) );
}

PRE(sys_capset)
{
   PRINT("sys_capset ( %p, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "capset", 
                 vki_cap_user_header_t, header,
                 const vki_cap_user_data_t, data);
   PRE_MEM_READ( "capset(header)", 
                  ARG1, sizeof(struct __vki_user_cap_header_struct) );
   PRE_MEM_READ( "capset(data)", 
                  ARG2, sizeof(struct __vki_user_cap_data_struct) );
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
   Char*        path;          /* path to executable */
   ThreadState* tst;

   PRINT("sys_execve ( %p(%s), %p, %p )", ARG1, ARG1, ARG2, ARG3);
   PRE_REG_READ3(vki_off_t, "execve",
                 char *, filename, char **, argv, char **, envp);
   PRE_MEM_RASCIIZ( "execve(filename)", ARG1 );
   if (ARG2 != 0)
      pre_argv_envp( ARG2, tid, "execve(argv)", "execve(argv[i])" );
   if (ARG3 != 0)
      pre_argv_envp( ARG3, tid, "execve(envp)", "execve(envp[i])" );

   path = (Char *)ARG1;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Erk.  If the exec fails, then the following will have made a
      mess of things which makes it hard for us to continue.  The
      right thing to do is piece everything together again in
      POST(execve), but that's hard work.  Instead, we make an effort
      to check that the execve will work before actually calling
      exec. */
   {
      struct vki_stat st;
      Int i = VG_(stat)((Char *)ARG1, &st);

      if (i == -1) {
         /* stat failed */
         SET_STATUS_Failure( VKI_EACCES/*really, we should copy stat's result*/ );
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

   /* Resistance is futile.  Nuke all other threads.  POSIX mandates
      this. (Really, nuke them all, since the new process will make
      its own new thread.) */
   VG_(nuke_all_threads_except)( tid, VgSrc_ExitSyscall );
   VG_(reap_threads)(tid);

   { // Remove the valgrind-specific stuff from the environment so the
     // child doesn't get vg_preload_core.so, vg_preload_TOOL.so, etc.  
     // This is done unconditionally, since if we are tracing the child,
     // stage1/2 will set up the appropriate client environment.
     Char** envp = (Char**)ARG3;
     if (envp != NULL) {
        VG_(env_remove_valgrind_env_stuff)( envp );
     }
   }

   if (VG_(clo_trace_children)) {
      Char* optvar = VG_(build_child_VALGRINDCLO)( (Char*)ARG1 );

      // Set VALGRINDCLO and VALGRINDLIB in ARG3 (the environment)
      VG_(env_setenv)( (Char***)&ARG3, VALGRINDCLO, optvar);
      VG_(env_setenv)( (Char***)&ARG3, VALGRINDLIB, VG_(libdir));

      // Create executable name: "/proc/self/fd/<vgexecfd>", update ARG1
      path = VG_(build_child_exename)();
   }

   VG_(debugLog)(1, "syswrap", "Exec of %s\n", (HChar*)ARG1);

   if (0) {
      Char **cpp;

      VG_(printf)("exec: %s\n", (Char *)ARG1);
      for(cpp = (Char **)ARG2; cpp && *cpp; cpp++)
         VG_(printf)("argv: %s\n", *cpp);
      for(cpp = (Char **)ARG3; cpp && *cpp; cpp++)
         VG_(printf)("env: %s\n", *cpp);
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
      Int i;

      for(i = 1; i < VG_(max_signal); i++) {
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

   SET_STATUS_from_SysRes( 
      VG_(do_syscall3)(__NR_execve, (UWord)path, ARG2, ARG3) 
   );

   /* If we got here, then the execve failed.  We've already made too
      much of a mess of ourselves to continue, so we have to abort. */
   VG_(message)(Vg_UserMsg, "execve(%p(%s), %p, %p) failed, errno %d",
                ARG1, ARG1, ARG2, ARG3, -RES);
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

PRE(sys_chown16)
{
   PRINT("sys_chown16 ( %p, 0x%x, 0x%x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "chown16",
                 const char *, path,
                 vki_old_uid_t, owner, vki_old_gid_t, group);
   PRE_MEM_RASCIIZ( "chown16(path)", ARG1 );
}

PRE(sys_chown)
{
   /* int chown(const char *path, uid_t owner, gid_t group); */
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
   if (!VG_(fd_allowed)(ARG1, "close", tid, False))
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
   if (!VG_(fd_allowed)(RES, "dup", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, RES, VG_(resolve_filename)(RES));
   }
}

PRE(sys_dup2)
{
   PRINT("sys_dup2 ( %d, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "dup2", unsigned int, oldfd, unsigned int, newfd);
   if (!VG_(fd_allowed)(ARG2, "dup2", tid, True))
      SET_STATUS_Failure( VKI_EBADF );
}

POST(sys_dup2)
{
   vg_assert(SUCCESS);
   if (VG_(clo_track_fds))
      VG_(record_fd_open)(tid, RES, VG_(resolve_filename)(RES));
}

PRE(sys_fchdir)
{
   PRINT("sys_fchdir ( %d )", ARG1);
   PRE_REG_READ1(long, "fchdir", unsigned int, fd);
}

PRE(sys_fchown16)
{
   PRINT("sys_fchown16 ( %d, %d, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "fchown16",
                 unsigned int, fd, vki_old_uid_t, owner, vki_old_gid_t, group);
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
   case VKI_F_SETOWN:
   case VKI_F_GETSIG:
   case VKI_F_SETSIG:
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
      if (!VG_(fd_allowed)(RES, "fcntl(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, RES, VG_(resolve_filename)(RES));
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
      if (!VG_(fd_allowed)(RES, "fcntl64(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, RES, 
                                VG_(resolve_filename)(RES));
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

   VG_(do_atfork_pre)(tid);

   SET_STATUS_from_SysRes( VG_(do_syscall0)(__NR_fork) );

   if (SUCCESS && RES == 0) {
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } 
   else 
   if (SUCCESS && RES > 0) {
      PRINT("   fork: process %d created child %d\n", VG_(getpid)(), RES);

      VG_(do_atfork_parent)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }
}

PRE(sys_ftruncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_ftruncate ( %d, %lld )", ARG1,(ULong)ARG2);
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

PRE(sys_getgroups16)
{
   PRINT("sys_getgroups16 ( %d, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "getgroups16", int, size, vki_old_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_WRITE( "getgroups16(list)", ARG2, ARG1 * sizeof(vki_old_gid_t) );
}

POST(sys_getgroups16)
{
   vg_assert(SUCCESS);
   if (ARG1 > 0 && RES > 0)
      POST_MEM_WRITE( ARG2, RES * sizeof(vki_old_gid_t) );
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
   // Note that the kernel version of getcwd() behaves quite differently to
   // the glibc one.
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

PRE(sys_geteuid16)
{
   PRINT("sys_geteuid16 ( )");
   PRE_REG_READ0(long, "geteuid16");
}

PRE(sys_geteuid)
{
   PRINT("sys_geteuid ( )");
   PRE_REG_READ0(long, "geteuid");
}

PRE(sys_getegid16)
{
   PRINT("sys_getegid16 ( )");
   PRE_REG_READ0(long, "getegid16");
}

PRE(sys_getegid)
{
   PRINT("sys_getegid ( )");
   PRE_REG_READ0(long, "getegid");
}

PRE(sys_getgid16)
{
   PRINT("sys_getgid16 ( )");
   PRE_REG_READ0(long, "getgid16");
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
   /* int getrusage (int who, struct rusage *usage); */
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

PRE(sys_getuid16)
{
   PRINT("sys_getuid16 ( )");
   PRE_REG_READ0(long, "getuid16");
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
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_ifindex,
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
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)", ARG3, sizeof(struct vki_ifconf));
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
   case (VKI_SOUND_PCM_READ_BITS|0x40000000): /* what the fuck ? */
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

   case VKI_BLKGETSIZE:
      PRE_MEM_WRITE( "ioctl(BLKGETSIZE)", ARG3, sizeof(unsigned long));
      break;

      /* Hard disks */
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
   case (VKI_SOUND_PCM_READ_BITS|0x40000000): /* what the fuck ? */
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

   case VKI_BLKGETSIZE:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;

      /* Hard disks */
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
Bool VG_(do_sigkill)(Int pid, Int tgid)
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
   /* int kill(pid_t pid, int sig); */
   PRINT("sys_kill ( %d, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "kill", int, pid, int, sig);
   if (!VG_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* If we're sending SIGKILL, check to see if the target is one of
      our threads and handle it specially. */
   if (ARG2 == VKI_SIGKILL && VG_(do_sigkill)(ARG1, -1))
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

PRE(sys_lseek)
{
   PRINT("sys_lseek ( %d, %d, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(vki_off_t, "lseek",
                 unsigned int, fd, vki_off_t, offset, unsigned int, whence);
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

PRE(sys_mmap2)
{
   // Exactly like old_mmap() in x86-linux except:
   //  - all 6 args are passed in regs, rather than in a memory-block.
   //  - the file offset is specified in pagesize units rather than bytes,
   //    so that it can be used for files bigger than 2^32 bytes.
   PRINT("sys_mmap2 ( %p, %llu, %d, %d, %d, %d )",
         ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap2",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   if (ARG2 == 0) {
      /* SuSV3 says: If len is zero, mmap() shall fail and no mapping
         shall be established. */
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   if (/*(ARG4 & VKI_MAP_FIXED) && */ (0 != (ARG1 & (VKI_PAGE_SIZE-1)))) {
      /* zap any misaligned addresses. */
      /* SuSV3 says misaligned addresses only cause the MAP_FIXED case
         to fail.   Here, we catch them all. */
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   if (ARG4 & VKI_MAP_FIXED) {
      if (!VG_(valid_client_addr)(ARG1, ARG2, tid, "mmap2"))
	 SET_STATUS_Failure( VKI_ENOMEM );
   } else {
      Addr a = VG_(find_map_space)(ARG1, ARG2, True);
      if (a == 0 && ARG1 != 0)
         ARG1 = VG_(find_map_space)(0, ARG2, True);
      else
         ARG1 = a;
      if (ARG1 == 0)
	 SET_STATUS_Failure( VKI_ENOMEM );
      else 
         ARG4 |= VKI_MAP_FIXED;
   }
}

POST(sys_mmap2)
{
   vg_assert(SUCCESS);
   vg_assert(VG_(valid_client_addr)(RES, ARG2, tid, "mmap2"));
   VG_(mmap_segment)( (Addr)RES, ARG2, ARG3, ARG4, ARG5,
                      ARG6 * (ULong)VKI_PAGE_SIZE );
}

PRE(sys_mprotect)
{
   PRINT("sys_mprotect ( %p, %llu, %d )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "mprotect",
                 unsigned long, addr, vki_size_t, len, unsigned long, prot);

   if (!VG_(valid_client_addr)(ARG1, ARG2, tid, "mprotect"))
      SET_STATUS_Failure( VKI_ENOMEM );
}

POST(sys_mprotect)
{
   Addr a    = ARG1;
   SizeT len = ARG2;
   Int  prot = ARG3;
   Bool rr = prot & VKI_PROT_READ;
   Bool ww = prot & VKI_PROT_WRITE;
   Bool xx = prot & VKI_PROT_EXEC;

   mash_addr_and_len(&a, &len);
   VG_(mprotect_range)(a, len, prot);
   VG_TRACK( change_mem_mprotect, a, len, rr, ww, xx );
}

PRE(sys_munmap)
{
   if (0) VG_(printf)("  munmap( %p )\n", ARG1);
   PRINT("sys_munmap ( %p, %llu )", ARG1,(ULong)ARG2);
   PRE_REG_READ2(long, "munmap", unsigned long, start, vki_size_t, length);

   if (!VG_(valid_client_addr)(ARG1, ARG2, tid, "munmap"))
      SET_STATUS_Failure( VKI_EINVAL );
}

POST(sys_munmap)
{
   Addr  a   = ARG1;
   SizeT len = ARG2;

   mash_addr_and_len(&a, &len);
   VG_(unmap_range)(a, len);
   VG_TRACK( die_mem_munmap, a, len );
}

PRE(sys_mincore)
{
   PRINT("sys_mincore ( %p, %llu, %p )", ARG1,(ULong)ARG2,ARG3);
   PRE_REG_READ3(long, "mincore",
                 unsigned long, start, vki_size_t, length,
                 unsigned char *, vec);
   PRE_MEM_WRITE( "mincore(vec)", ARG3, (ARG2 + 4096 - 1) / 4096);
}
POST(sys_mincore)
{
   POST_MEM_WRITE( ARG3, (ARG2 + 4096 - 1) / 4096 );  
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
   *flags |= SfMayBlock;
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
}

POST(sys_open)
{
   vg_assert(SUCCESS);
   if (!VG_(fd_allowed)(RES, "open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, RES, 
                                  VG_(arena_strdup)(VG_AR_CORE, (Char*)ARG1));
   }
}

PRE(sys_read)
{
   *flags |= SfMayBlock;
   PRINT("sys_read ( %d, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "read",
                 unsigned int, fd, char *, buf, vki_size_t, count);

   if (!VG_(fd_allowed)(ARG1, "read", tid, False))
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
   *flags |= SfMayBlock;
   PRINT("sys_write ( %d, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "write",
                 unsigned int, fd, const char *, buf, vki_size_t, count);
   if (!VG_(fd_allowed)(ARG1, "write", tid, False))
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
   if (!VG_(fd_allowed)(RES, "creat", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, RES, VG_(arena_strdup)(VG_AR_CORE, (Char*)ARG1));
   }
}

// XXX: sort of x86-specific
PRE(sys_pipe)
{
   PRINT("sys_pipe ( %p )", ARG1);
   PRE_REG_READ1(int, "pipe", unsigned long *, filedes);
   PRE_MEM_WRITE( "pipe(filedes)", ARG1, 2*sizeof(long) );
}

POST(sys_pipe)
{
   Int *p = (Int *)ARG1;

   if (!VG_(fd_allowed)(p[0], "pipe", tid, True) ||
       !VG_(fd_allowed)(p[1], "pipe", tid, True)) {
      VG_(close)(p[0]);
      VG_(close)(p[1]);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      POST_MEM_WRITE( ARG1, 2*sizeof(int) );
      if (VG_(clo_track_fds)) {
         VG_(record_fd_open)(tid, p[0], NULL);
         VG_(record_fd_open)(tid, p[1], NULL);
      }
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
   Word saved = SYSNO;
   PRINT("sys_readlink ( %p, %p, %llu )", ARG1,ARG2,(ULong)ARG3);
   PRE_REG_READ3(long, "readlink",
                 const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlink(path)", ARG1 );
   PRE_MEM_WRITE( "readlink(buf)", ARG2,ARG3 );

   /*
    * Handle the case where readlink is looking at /proc/self/exe or
    * /proc/<pid>/exe.
    */

   SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, ARG1, ARG2, ARG3));

   /* jrs 20050604: where does the magic value 2 come from?  It seems
      like it should be a kernel error value, but we don't know of any
      such. */
   if (SWHAT == SsFailure && RES_unchecked == 2) {
      HChar name[25];
      VG_(sprintf)(name, "/proc/%d/exe", VG_(getpid)());
      if (VG_(strcmp)((Char *)ARG1, name) == 0 ||
          VG_(strcmp)((Char *)ARG1, "/proc/self/exe") == 0) {
         VG_(sprintf)(name, "/proc/self/fd/%d", VG_(clexecfd));
         SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, (UWord)name, ARG2, ARG3));
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
   PRINT("sys_readv ( %d, %p, %llu )",ARG1,ARG2,(ULong)ARG3);
   PRE_REG_READ3(ssize_t, "readv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count);
   if (!VG_(fd_allowed)(ARG1, "readv", tid, False)) {
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

PRE(sys_sched_setparam)
{
   PRINT("sched_setparam ( %d, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "sched_setparam", 
                 vki_pid_t, pid, struct sched_param *, p);
   PRE_MEM_READ( "sched_setparam(p)", ARG2, sizeof(struct vki_sched_param) );
}
POST(sys_sched_setparam)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_sched_param) );
}

PRE(sys_sched_getparam)
{
   PRINT("sched_getparam ( %d, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "sched_getparam", 
                 vki_pid_t, pid, struct sched_param *, p);
   PRE_MEM_WRITE( "sched_getparam(p)", ARG2, sizeof(struct vki_sched_param) );
}

POST(sys_sched_getparam)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_sched_param) );
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

PRE(sys_setgid16)
{
   PRINT("sys_setgid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setgid16", vki_old_gid_t, gid);
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

PRE(sys_setgroups16)
{
   PRINT("sys_setgroups16 ( %llu, %p )", (ULong)ARG1, ARG2);
   PRE_REG_READ2(long, "setgroups16", int, size, vki_old_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_READ( "setgroups16(list)", ARG2, ARG1 * sizeof(vki_old_gid_t) );
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

PRE(sys_setreuid16)
{
   PRINT("setreuid16 ( 0x%x, 0x%x )", ARG1, ARG2);
   PRE_REG_READ2(long, "setreuid16", vki_old_uid_t, ruid, vki_old_uid_t, euid);
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

PRE(sys_setuid16)
{
   PRINT("sys_setuid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setuid16", vki_old_uid_t, uid);
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
   PRE_MEM_WRITE( "times(buf)", ARG1, sizeof(struct vki_tms) );
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

PRE(sys_utime)
{
   *flags |= SfMayBlock;
   PRINT("sys_utime ( %p, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "utime", char *, filename, struct utimbuf *, buf);
   PRE_MEM_RASCIIZ( "utime(filename)", ARG1 );
   if (ARG2 != 0)
      PRE_MEM_READ( "utime(buf)", ARG2, sizeof(struct vki_utimbuf) );
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
   if (!VG_(fd_allowed)(ARG1, "writev", tid, False)) {
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

PRE(sys_sched_setaffinity)
{
   PRINT("sched_setaffinity ( %d, %d, %p )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sched_setaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_READ( "sched_setaffinity(mask)", ARG3, ARG2);
}

PRE(sys_sched_getaffinity)
{
   PRINT("sched_getaffinity ( %d, %d, %p )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sched_getaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_WRITE( "sched_getaffinity(mask)", ARG3, ARG2);
}
POST(sys_sched_getaffinity)
{
   POST_MEM_WRITE(ARG3, ARG2);
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
PRE(sys_rt_sigsuspend)
{
   /* The C library interface to sigsuspend just takes a pointer to
      a signal mask but this system call has two arguments - a pointer
      to the mask and the number of bytes used by it. The kernel insists
      on the size being equal to sizeof(sigset_t) however and will just
      return EINVAL if it isn't.
    */
   *flags |= SfMayBlock;
   PRINT("sys_rt_sigsuspend ( %p, %d )", ARG1,ARG2 );
   PRE_REG_READ2(int, "rt_sigsuspend", vki_sigset_t *, mask, vki_size_t, size)
   if (ARG1 != (Addr)NULL) {
      PRE_MEM_READ( "rt_sigsuspend(mask)", ARG1, sizeof(vki_sigset_t) );
   }
}

PRE(sys_rt_sigtimedwait)
{
   *flags |= SfMayBlock;
   PRINT("sys_rt_sigtimedwait ( %p, %p, %p, %lld )",
         ARG1,ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(long, "rt_sigtimedwait", 
                 const vki_sigset_t *, set, vki_siginfo_t *, info,
                 const struct timespec *, timeout, vki_size_t, sigsetsize);
   if (ARG1 != 0) 
      PRE_MEM_READ(  "rt_sigtimedwait(set)",  ARG1, sizeof(vki_sigset_t));
   if (ARG2 != 0)
      PRE_MEM_WRITE( "rt_sigtimedwait(info)", ARG2, sizeof(vki_siginfo_t) );
   PRE_MEM_READ( "rt_sigtimedwait(timeout)",
                 ARG4, sizeof(struct vki_timespec) );
}

POST(sys_rt_sigtimedwait)
{
   if (ARG2 != 0)
      POST_MEM_WRITE( ARG2, sizeof(vki_siginfo_t) );
}

PRE(sys_rt_sigqueueinfo)
{
   PRINT("sys_rt_sigqueueinfo(%d, %d, %p)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "rt_sigqueueinfo", 
                 int, pid, int, sig, vki_siginfo_t *, uinfo);
   if (ARG2 != 0)
      PRE_MEM_READ( "rt_sigqueueinfo(uinfo)", ARG3, sizeof(vki_siginfo_t) );
}
POST(sys_rt_sigqueueinfo)
{
   if (!VG_(client_signal_OK)(ARG2))
      SET_STATUS_Failure( VKI_EINVAL );
}

// XXX: x86-specific
PRE(sys_sigaltstack)
{
   /* int sigaltstack(const stack_t *ss, stack_t *oss); */
   PRINT("sigaltstack ( %p, %p )",ARG1,ARG2);
   PRE_REG_READ2(int, "sigaltstack",
                 const vki_stack_t *, ss, vki_stack_t *, oss);
   if (ARG1 != 0) {
      PRE_MEM_READ( "sigaltstack(ss)", ARG1, sizeof(vki_stack_t) );
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

// XXX: x86-specific
PRE(sys_rt_sigaction)
{
   PRINT("sys_rt_sigaction ( %d, %p, %p, %d )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "rt_sigaction",
                 int, signum, const struct sigaction *, act,
                 struct sigaction *, oldact, vki_size_t, sigsetsize);

   if (ARG2 != 0)
      PRE_MEM_READ( "rt_sigaction(act)", ARG2, sizeof(struct vki_sigaction));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "rt_sigaction(oldact)", ARG3, sizeof(struct vki_sigaction));

   // XXX: doesn't seem right to be calling do_sys_sigaction for
   // sys_rt_sigaction... perhaps this function should be renamed
   // VG_(do_sys_rt_sigaction)()  --njn

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigaction)(ARG1, (const struct vki_sigaction *)ARG2,
                            (struct vki_sigaction *)ARG3)
   );
}
POST(sys_rt_sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_sigaction));
}

// XXX: This syscall is not used on amd64 -- it only provides
//      sys_rt_sigprocmask, which uses sigset_t rather than old_sigset_t.
// This wrapper is only suitable for 32-bit architectures.
#if defined(VGP_x86_linux)
PRE(sys_sigprocmask)
{
   vki_old_sigset_t* set;
   vki_old_sigset_t* oldset;
   vki_sigset_t bigger_set;
   vki_sigset_t bigger_oldset;

   PRINT("sys_sigprocmask ( %d, %p, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sigprocmask", 
                 int, how, vki_old_sigset_t *, set, vki_old_sigset_t *, oldset);
   if (ARG2 != 0)
      PRE_MEM_READ( "sigprocmask(set)", ARG2, sizeof(vki_old_sigset_t));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sigprocmask(oldset)", ARG3, sizeof(vki_old_sigset_t));

   // Nb: We must convert the smaller vki_old_sigset_t params into bigger
   // vki_sigset_t params.
   set    = (vki_old_sigset_t*)ARG2;
   oldset = (vki_old_sigset_t*)ARG3;

   VG_(memset)(&bigger_set,    0, sizeof(vki_sigset_t));
   VG_(memset)(&bigger_oldset, 0, sizeof(vki_sigset_t));
   if (set)
      bigger_set.sig[0] = *(vki_old_sigset_t*)set;

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigprocmask) ( tid, ARG1 /*how*/, 
                                set ? &bigger_set    : NULL,
                             oldset ? &bigger_oldset : NULL)
   );

   if (oldset)
      *oldset = bigger_oldset.sig[0];

   if (SUCCESS)
      *flags |= SfPollAfter;
}

POST(sys_sigprocmask)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_old_sigset_t));
}
#endif

PRE(sys_rt_sigprocmask)
{
   PRINT("sys_rt_sigprocmask ( %d, %p, %p, %llu )",ARG1,ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(long, "rt_sigprocmask", 
                 int, how, vki_sigset_t *, set, vki_sigset_t *, oldset,
                 vki_size_t, sigsetsize);
   if (ARG2 != 0)
      PRE_MEM_READ( "rt_sigprocmask(set)", ARG2, sizeof(vki_sigset_t));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "rt_sigprocmask(oldset)", ARG3, sizeof(vki_sigset_t));

   // Like the kernel, we fail if the sigsetsize is not exactly what we expect.
   if (sizeof(vki_sigset_t) != ARG4)
      SET_STATUS_Failure( VKI_EMFILE );
   else {
      SET_STATUS_from_SysRes( 
                  VG_(do_sys_sigprocmask) ( tid, ARG1 /*how*/, 
                                            (vki_sigset_t*) ARG2,
                                            (vki_sigset_t*) ARG3 )
      );
   }

   if (SUCCESS)
      *flags |= SfPollAfter;
}

POST(sys_rt_sigprocmask)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_sigset_t));
}

PRE(sys_sigpending)
{
   PRINT( "sys_sigpending ( %p )", ARG1 );
   PRE_REG_READ1(long, "sigpending", vki_old_sigset_t *, set);
   PRE_MEM_WRITE( "sigpending(set)", ARG1, sizeof(vki_old_sigset_t));
}

POST(sys_sigpending)
{
   POST_MEM_WRITE( ARG1, sizeof(vki_old_sigset_t) ) ;
}

PRE(sys_rt_sigpending)
{
   PRINT( "sys_rt_sigpending ( %p )", ARG1 );
   PRE_REG_READ2(long, "rt_sigpending", 
                 vki_sigset_t *, set, vki_size_t, sigsetsize);
   PRE_MEM_WRITE( "rt_sigpending(set)", ARG1, sizeof(vki_sigset_t));
}
POST(sys_rt_sigpending)
{
   POST_MEM_WRITE( ARG1, sizeof(vki_sigset_t) ) ;
}

PRE(sys_mq_open)
{
   PRINT("sys_mq_open( %p(%s), %d, %lld, %p )",
         ARG1,ARG1,ARG2,(ULong)ARG3,ARG4);
   PRE_REG_READ4(long, "mq_open",
                 const char *, name, int, oflag, vki_mode_t, mode,
                 struct mq_attr *, attr);
   PRE_MEM_RASCIIZ( "mq_open(name)", ARG1 );
   if ((ARG2 & VKI_O_CREAT) != 0 && ARG4 != 0) {
      const struct vki_mq_attr *attr = (struct vki_mq_attr *)ARG4;
      PRE_MEM_READ( "mq_open(attr->mq_maxmsg)",
                     (Addr)&attr->mq_maxmsg, sizeof(attr->mq_maxmsg) );
      PRE_MEM_READ( "mq_open(attr->mq_msgsize)",
                     (Addr)&attr->mq_msgsize, sizeof(attr->mq_msgsize) );
   }
}

POST(sys_mq_open)
{
   vg_assert(SUCCESS);
   if (!VG_(fd_allowed)(RES, "mq_open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, RES, VG_(arena_strdup)(VG_AR_CORE, (Char*)ARG1));
   }
}

PRE(sys_mq_unlink)
{
   PRINT("sys_mq_unlink ( %p(%s) )", ARG1,ARG1);
   PRE_REG_READ1(long, "mq_unlink", const char *, name);
   PRE_MEM_RASCIIZ( "mq_unlink(name)", ARG1 );
}

PRE(sys_mq_timedsend)
{
   *flags |= SfMayBlock;
   PRINT("sys_mq_timedsend ( %d, %p, %llu, %d, %p )",
         ARG1,ARG2,(ULong)ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "mq_timedsend",
                 vki_mqd_t, mqdes, const char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int, msg_prio, const struct timespec *, abs_timeout);
   if (!VG_(fd_allowed)(ARG1, "mq_timedsend", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "mq_timedsend(msg_ptr)", ARG2, ARG3 );
      if (ARG5 != 0)
         PRE_MEM_READ( "mq_timedsend(abs_timeout)", ARG5,
                        sizeof(struct vki_timespec) );
   }
}

PRE(sys_mq_timedreceive)
{
   *flags |= SfMayBlock;
   PRINT("sys_mq_timedreceive( %d, %p, %llu, %p, %p )",
         ARG1,ARG2,(ULong)ARG3,ARG4,ARG5);
   PRE_REG_READ5(ssize_t, "mq_timedreceive",
                 vki_mqd_t, mqdes, char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int *, msg_prio,
                 const struct timespec *, abs_timeout);
   if (!VG_(fd_allowed)(ARG1, "mq_timedreceive", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_WRITE( "mq_timedreceive(msg_ptr)", ARG2, ARG3 );
      if (ARG4 != 0)
         PRE_MEM_WRITE( "mq_timedreceive(msg_prio)",
                        ARG4, sizeof(unsigned int) );
      if (ARG5 != 0)
         PRE_MEM_READ( "mq_timedreceive(abs_timeout)",
                        ARG5, sizeof(struct vki_timespec) );
   }
}

POST(sys_mq_timedreceive)
{
   POST_MEM_WRITE( ARG2, ARG3 );
   if (ARG4 != 0)
      POST_MEM_WRITE( ARG4, sizeof(unsigned int) );
}

PRE(sys_mq_notify)
{
   PRINT("sys_mq_notify( %d, %p )", ARG1,ARG2 );
   PRE_REG_READ2(long, "mq_notify",
                 vki_mqd_t, mqdes, const struct sigevent *, notification);
   if (!VG_(fd_allowed)(ARG1, "mq_notify", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
   else if (ARG2 != 0)
      PRE_MEM_READ( "mq_notify(notification)",
                    ARG2, sizeof(struct vki_sigevent) );
}

PRE(sys_mq_getsetattr)
{
   PRINT("sys_mq_getsetattr( %d, %p, %p )", ARG1,ARG2,ARG3 );
   PRE_REG_READ3(long, "mq_getsetattr",
                 vki_mqd_t, mqdes, const struct mq_attr *, mqstat,
                 struct mq_attr *, omqstat);
   if (!VG_(fd_allowed)(ARG1, "mq_getsetattr", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if (ARG2 != 0) {
         const struct vki_mq_attr *attr = (struct vki_mq_attr *)ARG2;
         PRE_MEM_READ( "mq_getsetattr(mqstat->mq_flags)",
                        (Addr)&attr->mq_flags, sizeof(attr->mq_flags) );
      }
      if (ARG3 != 0)
         PRE_MEM_WRITE( "mq_getsetattr(omqstat)", ARG3,
                        sizeof(struct vki_mq_attr) );
   }   
}

POST(sys_mq_getsetattr)
{
   if (ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_mq_attr) );
}

PRE(sys_timer_create)
{
   PRINT("sys_timer_create( %d, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "timer_create",
                 vki_clockid_t, clockid, struct sigevent *, evp,
                 vki_timer_t *, timerid);
   if (ARG2 != 0)
      PRE_MEM_READ( "timer_create(evp)", ARG2, sizeof(struct vki_sigevent) );
   PRE_MEM_WRITE( "timer_create(timerid)", ARG3, sizeof(vki_timer_t) );
}
POST(sys_timer_create)
{
   POST_MEM_WRITE( ARG3, sizeof(vki_timer_t) );
}

PRE(sys_timer_settime)
{
   PRINT("sys_timer_settime( %lld, %d, %p, %p )", (ULong)ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "timer_settime", 
                 vki_timer_t, timerid, int, flags,
                 const struct itimerspec *, value,
                 struct itimerspec *, ovalue);
   PRE_MEM_READ( "timer_settime(value)", ARG3,
                  sizeof(struct vki_itimerspec) );
   if (ARG4 != 0)
       PRE_MEM_WRITE( "timer_settime(ovalue)", ARG4,
                      sizeof(struct vki_itimerspec) );
}
POST(sys_timer_settime)
{
   if (ARG4 != 0)
      POST_MEM_WRITE( ARG4, sizeof(struct vki_itimerspec) );
}

PRE(sys_timer_gettime)
{
   PRINT("sys_timer_gettime( %lld, %p )", (ULong)ARG1,ARG2);
   PRE_REG_READ2(long, "timer_gettime", 
                 vki_timer_t, timerid, struct itimerspec *, value);
   PRE_MEM_WRITE( "timer_gettime(value)", ARG2,
                  sizeof(struct vki_itimerspec));
}
POST(sys_timer_gettime)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_itimerspec) );
}

PRE(sys_timer_getoverrun)
{
   PRINT("sys_timer_getoverrun( %p )", ARG1);
   PRE_REG_READ1(long, "timer_getoverrun", vki_timer_t, timerid);
}

PRE(sys_timer_delete)
{
   PRINT("sys_timer_delete( %p )", ARG1);
   PRE_REG_READ1(long, "timer_delete", vki_timer_t, timerid);
}

PRE(sys_clock_settime)
{
   PRINT("sys_clock_settime( %d, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "clock_settime", 
                 vki_clockid_t, clk_id, const struct timespec *, tp);
   PRE_MEM_READ( "clock_settime(tp)", ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_gettime)
{
   PRINT("sys_clock_gettime( %d, %p )" , ARG1,ARG2);
   PRE_REG_READ2(long, "clock_gettime", 
                 vki_clockid_t, clk_id, struct timespec *, tp);
   PRE_MEM_WRITE( "clock_gettime(tp)", ARG2, sizeof(struct vki_timespec) );
}
POST(sys_clock_gettime)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_getres)
{
   PRINT("sys_clock_getres( %d, %p )" , ARG1,ARG2);
   // Nb: we can't use "RES" as the param name because that's a macro
   // defined above!
   PRE_REG_READ2(long, "clock_getres", 
                 vki_clockid_t, clk_id, struct timespec *, res);
   PRE_MEM_WRITE( "clock_getres(res)", ARG2, sizeof(struct vki_timespec) );
}
POST(sys_clock_getres)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
}

#undef PRE
#undef POST

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
