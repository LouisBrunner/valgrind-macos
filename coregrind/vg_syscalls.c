
/*--------------------------------------------------------------------*/
/*--- Handle system calls.                           vg_syscalls.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

/* All system calls are channelled through here, doing two things:

   * notify the tool of the events (mem/reg reads, writes) happening

   * perform the syscall, usually by passing it along to the kernel
     unmodified.

   A magical piece of assembly code, VG_(do_syscall)(), in vg_syscall.S
   does the tricky bit of passing a syscall to the kernel, whilst
   having the simulator retain control.
*/

/* ---------------------------------------------------------------------
   A simple atfork() facility for Valgrind's internal use
   ------------------------------------------------------------------ */

struct atfork {
   vg_atfork_t	pre;
   vg_atfork_t	parent;
   vg_atfork_t	child;
};

#define VG_MAX_ATFORK	10

static struct atfork atforks[VG_MAX_ATFORK];
static Int n_atfork;

void VG_(atfork)(vg_atfork_t pre, vg_atfork_t parent, vg_atfork_t child)
{
   Int i;

   for(i = 0; i < n_atfork; i++) {
      if (atforks[i].pre == pre &&
	  atforks[i].parent == parent &&
	  atforks[i].child == child)
	 return;
   }

   if (n_atfork >= VG_MAX_ATFORK)
      VG_(core_panic)("Too many VG_(atfork) handlers requested: raise VG_MAX_ATFORK");

   atforks[n_atfork].pre    = pre;
   atforks[n_atfork].parent = parent;
   atforks[n_atfork].child  = child;

   n_atfork++;
}

static void do_atfork_pre(ThreadId tid)
{
   Int i;

   for(i = 0; i < n_atfork; i++)
      if (atforks[i].pre != NULL)
	 (*atforks[i].pre)(tid);
}

static void do_atfork_parent(ThreadId tid)
{
   Int i;

   for(i = 0; i < n_atfork; i++)
      if (atforks[i].parent != NULL)
	 (*atforks[i].parent)(tid);
}

static void do_atfork_child(ThreadId tid)
{
   Int i;

   for(i = 0; i < n_atfork; i++)
      if (atforks[i].child != NULL)
	 (*atforks[i].child)(tid);
}

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

   if (cl_base < 0x10000)
      cl_base = 0x10000;

   ret =
      (end >= start) && 
      start >= cl_base && start < VG_(client_end) &&
      (end <= VG_(client_end));

   if (0)
      VG_(printf)("%s: test=%p-%p client=%p-%p ret=%d\n",
		  syscallname, start, end, cl_base, VG_(client_end), ret);

   if (!ret && syscallname != NULL) {
      VG_(message)(Vg_UserMsg, "Warning: client syscall %s tried to modify addresses %p-%p",
		   syscallname, start, end);

      if (VG_(clo_verbosity) > 1) {
	 ExeContext *ec = VG_(get_ExeContext)(tid);
	 VG_(pp_ExeContext)(ec);
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
   
   ra = PGROUNDDN(*a);
   *len = PGROUNDUP(*a + *len) - ra;
   *a = ra;
}

static
void mmap_segment ( Addr a, SizeT len, UInt prot, UInt mm_flags, Int fd, ULong offset )
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
Addr mremap_segment ( Addr old_addr, SizeT old_size,
		      Addr new_addr, SizeT new_size,
		      UInt flags, ThreadId tid)
{
   Addr ret;
   Segment *seg, *next;

   old_size = PGROUNDUP(old_size);
   new_size = PGROUNDUP(new_size);

   if (PGROUNDDN(old_addr) != old_addr)
      return -VKI_EINVAL;

   if (!VG_(valid_client_addr)(old_addr, old_size, tid, "mremap(old_addr)"))
      return -VKI_EFAULT;

   /* fixed at the current address means we don't move it */
   if ((flags & VKI_MREMAP_FIXED) && (old_addr == new_addr))
      flags &= ~(VKI_MREMAP_FIXED|VKI_MREMAP_MAYMOVE);

   if (flags & VKI_MREMAP_FIXED) {
      if (PGROUNDDN(new_addr) != new_addr)
	 return -VKI_EINVAL;

      if (!VG_(valid_client_addr)(new_addr, new_size, tid, "mremap(new_addr)"))
	 return -VKI_ENOMEM;

      /* check for overlaps */
      if ((old_addr < (new_addr+new_size) &&
	   (old_addr+old_size) > new_addr) ||
	  (new_addr < (old_addr+new_size) &&
	   (new_addr+new_size) > old_addr))
	 return -VKI_EINVAL;
   }

   /* Do nothing */
   if (!(flags & VKI_MREMAP_FIXED) && new_size == old_size)
      return old_addr;

   seg = VG_(find_segment)(old_addr);

   /* range must be contained within segment */
   if (seg == NULL || !VG_(seg_contains)(seg, old_addr, old_size))
      return -VKI_EINVAL;

   next = VG_(next_segment)(seg);

   if (0)
      VG_(printf)("mremap: old_addr+new_size=%p next->addr=%p flags=%d\n",
		  old_addr+new_size, next->addr, flags);
   
   if ((flags & VKI_MREMAP_FIXED) ||
       (next != NULL && (old_addr+new_size) > next->addr)) {
      /* we're moving the block */
      Addr a;
      
      if ((flags & (VKI_MREMAP_FIXED|VKI_MREMAP_MAYMOVE)) == 0)
	 return -VKI_ENOMEM;	/* not allowed to move */

      if ((flags & VKI_MREMAP_FIXED) == 0)
	  new_addr = 0;

      a = VG_(find_map_space)(new_addr, new_size, True);

      if ((flags & VKI_MREMAP_FIXED) && a != new_addr)
	 return -VKI_ENOMEM;	/* didn't find the place we wanted */

      new_addr = a;
      ret = a;

      /* we've nailed down the location */
      flags |=  VKI_MREMAP_FIXED|VKI_MREMAP_MAYMOVE;

      ret = VG_(do_syscall)(__NR_mremap, old_addr, old_size, new_size, 
			    flags, new_addr);

      if (ret != new_addr) {
	 vg_assert(VG_(is_kerror)(ret));
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
      ret = old_addr;

      if (new_size < old_size) {
	 VG_TRACK(die_mem_munmap, old_addr+new_size, old_size-new_size);
	 VG_(munmap)((void *)(old_addr+new_size), old_size-new_size);
      } else {
	 /* we've nailed down the location */
	 flags &= ~VKI_MREMAP_MAYMOVE;

	 if (0)
	    VG_(printf)("mremap: old_addr=%p old_size=%d new_size=%d flags=%d\n",
			old_addr, old_size, new_size, flags);

	 ret = VG_(do_syscall)(__NR_mremap, old_addr, old_size, new_size, 
			       flags, 0);

	 if (ret != old_addr)
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


/* Is this a Linux kernel error return value? */
/* From:
   http://sources.redhat.com/cgi-bin/cvsweb.cgi/libc/sysdeps/unix/sysv/
   linux/i386/sysdep.h?
   rev=1.28&content-type=text/x-cvsweb-markup&cvsroot=glibc

   \begin{quote}:

   Linux uses a negative return value to indicate syscall errors,
   unlike most Unices, which use the condition codes' carry flag.

   Since version 2.1 the return value of a system call might be
   negative even if the call succeeded.  E.g., the `lseek' system call
   might return a large offset.  Therefore we must not anymore test
   for < 0, but test for a real error by making sure the value in %eax
   is a real error number.  Linus said he will make sure the no syscall
   returns a value in -1 .. -4095 as a valid result so we can safely
   test with -4095.  

   END QUOTE
*/
Bool VG_(is_kerror) ( Word res )
{
   if (res >= -4095 && res <= -1)
      return True;
   else
      return False;
}

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

/* Given a file descriptor, attempt to deduce its filename.  To do this,
   we use /proc/self/fd/<FD>.  If this doesn't point to a file, or if it
   doesn't exist, we just return NULL.  Otherwise, we return a pointer
   to the file name, which the caller is responsible for freeing. */

Char *VG_(resolve_filename)(Int fd)
{
   char tmp[28], buf[VKI_PATH_MAX];

   VG_(sprintf)(tmp, "/proc/self/fd/%d", fd);
   VG_(memset)(buf, 0, VKI_PATH_MAX);

   if(VG_(readlink)(tmp, buf, VKI_PATH_MAX) == -1)
      return NULL;

   return ((buf[0] == '/') ? VG_(arena_strdup)(VG_AR_CORE, buf) : NULL);
}


/* Note the fact that a file descriptor was just closed. */

static
void record_fd_close(Int tid, Int fd)
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

void VG_(record_fd_open)(Int tid, Int fd, char *pathname)
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
   i->where = (tid == -1) ? NULL : VG_(get_ExeContext)(tid);
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
UInt get_shm_size ( Int shmid )
{
   struct vki_shmid_ds buf;
   long __res = VG_(do_syscall)(__NR_ipc, 24 /* IPCOP_shmctl */, shmid, VKI_IPC_STAT, 0, &buf);
    if ( VG_(is_kerror) ( __res ) )
       return 0;
 
   return buf.shm_segsz;
}

static
UInt get_sem_count( Int semid )
{
  struct vki_semid_ds buf;
  union vki_semun arg;
  long res;

  arg.buf = &buf;
  
  res = VG_(do_syscall)(__NR_ipc, 3 /* IPCOP_semctl */, semid, 0, VKI_IPC_STAT, &arg);
  if ( VG_(is_kerror)(res) )
    return 0;

  return buf.sem_nsems;
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
   Char *outmsg = strdupcat ( "socketcall.sendmsg", msg, VG_AR_TRANSIENT );
   PRE_MEM_READ( outmsg, base, size );

   VG_(arena_free) ( VG_AR_TRANSIENT, outmsg );
}

static 
void pre_mem_write_recvmsg ( ThreadId tid,
                             Char *msg, Addr base, SizeT size )
{
   Char *outmsg = strdupcat ( "socketcall.recvmsg", msg, VG_AR_TRANSIENT );
   PRE_MEM_WRITE( outmsg, base, size );
   VG_(arena_free) ( VG_AR_TRANSIENT, outmsg );
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

void check_cmsg_for_fds(Int tid, struct vki_msghdr *msg)
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

   outmsg = VG_(arena_malloc) ( VG_AR_TRANSIENT,
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
   
   VG_(arena_free) ( VG_AR_TRANSIENT, outmsg );
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

/* Dereference a pointer to a pointer. */
static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
{
   Addr* a_p = (Addr*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
   return *a_p;
}

static 
void buf_and_len_pre_check( ThreadId tid, Addr buf_p, Addr buflen_p,
                            Char* buf_s, Char* buflen_s )
{
   if (VG_(defined_pre_mem_write)()) {
      UInt buflen_in = deref_UInt( tid, buflen_p, buflen_s);
      if (buflen_in > 0) {
         TL_(pre_mem_write) ( Vg_CoreSysCall,
			      tid, buf_s, buf_p, buflen_in );
      }
   }
}

static 
void buf_and_len_post_check( ThreadId tid, Int res,
                             Addr buf_p, Addr buflen_p, Char* s )
{
   if (!VG_(is_kerror)(res) && VG_(defined_post_mem_write)()) {
      UInt buflen_out = deref_UInt( tid, buflen_p, s);
      if (buflen_out > 0 && buf_p != (Addr)NULL) {
         TL_(post_mem_write) ( buf_p, buflen_out );
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
      VG_(printf)("do_brk: brk_base=%p brk_limit=%p newbrk=%p\n",
		  VG_(brk_base), VG_(brk_limit), newbrk);

   if (newbrk < VG_(brk_base) || newbrk >= VG_(client_end))
      return VG_(brk_limit);

   /* brk isn't allowed to grow over anything else */
   seg = VG_(find_segment)(VG_(brk_limit));

   vg_assert(seg != NULL);

   if (0)
      VG_(printf)("brk_limit=%p seg->addr=%p seg->end=%p\n", 
		  VG_(brk_limit), seg->addr, seg->addr+seg->len);
   vg_assert(VG_(brk_limit) >= seg->addr && VG_(brk_limit) <= (seg->addr + seg->len));

   seg = VG_(next_segment)(seg);
   if (seg != NULL && newbrk > seg->addr)
      return VG_(brk_limit);

   current = PGROUNDUP(VG_(brk_limit));
   newaddr = PGROUNDUP(newbrk);
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
	 ExeContext *ec = VG_(get_ExeContext)(tid);
	 VG_(pp_ExeContext)(ec);
      }
      return False;
   }
   else if (soft && fd >= VG_(fd_soft_limit)) {
      return False;
   }
   return True;
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

#define Special    (1 << 0)
#define MayBlock   (1 << 1)
#define NBRunInLWP (1 << 2)   // non-blocking, but must run in LWP context
#define PostOnFail (1 << 3)

#define PRE(x,f) \
   UInt VGA_(gen_##x##_flags) = f; \
   void VGA_(gen_##x##_before)(ThreadId tid, ThreadState *tst)
#define POST(x) \
   void VGA_(gen_##x##_after) (ThreadId tid, ThreadState *tst)

#define SYSNO	PLATFORM_SYSCALL_NUM(tst->arch)    // in PRE(x)
#define res	PLATFORM_SYSCALL_RET(tst->arch)	   // in POST(x)
#define arg1	PLATFORM_SYSCALL_ARG1(tst->arch)
#define arg2	PLATFORM_SYSCALL_ARG2(tst->arch)
#define arg3	PLATFORM_SYSCALL_ARG3(tst->arch)
#define arg4	PLATFORM_SYSCALL_ARG4(tst->arch)
#define arg5	PLATFORM_SYSCALL_ARG5(tst->arch)
#define arg6	PLATFORM_SYSCALL_ARG6(tst->arch)

#define set_result(val) PLATFORM_SET_SYSCALL_RESULT(tst->arch, (val))

#define PRINT(format, args...)  \
   if (VG_(clo_trace_syscalls))        \
      VG_(printf)(format, ## args)

// Combine two 32-bit values into a 64-bit value
#define LOHI64(lo,hi)   ( (lo) | ((ULong)(hi) << 32) )

PRE(sys_exit_group, Special)
{
   VG_(core_panic)("syscall exit_group() not caught by the scheduler?!");
}

PRE(sys_exit, Special)
{
   VG_(core_panic)("syscall exit() not caught by the scheduler?!");
}

PRE(sys_sched_yield, Special)
{
   VG_(core_panic)("syscall sched_yield() not caught by the scheduler?!");
}

PRE(sys_ni_syscall, Special)
{
   PRINT("non-existent syscall! (ni_syscall)");
   PRE_REG_READ0(long, "ni_syscall");
   set_result( -VKI_ENOSYS );
}

// XXX: I think this is x86/linux-specific... at least some of the entries
// are non-generic
// XXX: Why is the memory pointed to by arg3 never checked?
PRE(sys_ptrace, 0)
{
   PRINT("sys_ptrace ( %d, %d, %p, %p )", arg1,arg2,arg3,arg4);
   PRE_REG_READ4(int, "ptrace", 
                 long, request, long, pid, long, addr, long, data);
   switch (arg1) {
   case 12:   /* PTRACE_GETREGS */
      PRE_MEM_WRITE( "ptrace(getregs)", arg4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case 14:   /* PTRACE_GETFPREGS */
      PRE_MEM_WRITE( "ptrace(getfpregs)", arg4, 
		     sizeof (struct vki_user_i387_struct));
      break;
   case 18:   /* PTRACE_GETFPXREGS */
      PRE_MEM_WRITE( "ptrace(getfpxregs)", arg4, 
                     sizeof(struct vki_user_fxsr_struct) );
      break;
   case 1: case 2: case 3:    /* PTRACE_PEEK{TEXT,DATA,USER} */
      PRE_MEM_WRITE( "ptrace(peek)", arg4, 
		     sizeof (long));
      break;
   case 13:   /* PTRACE_SETREGS */
      PRE_MEM_READ( "ptrace(setregs)", arg4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case 15:   /* PTRACE_SETFPREGS */
      PRE_MEM_READ( "ptrace(setfpregs)", arg4, 
		     sizeof (struct vki_user_i387_struct));
      break;
   case 19:   /* PTRACE_SETFPXREGS */
      PRE_MEM_READ( "ptrace(setfpxregs)", arg4, 
                     sizeof(struct vki_user_fxsr_struct) );
      break;
   default:
      break;
   }
}

// XXX: I think this is x86/linux-specific
POST(sys_ptrace)
{
   switch (arg1) {
   case 12:  /* PTRACE_GETREGS */
      POST_MEM_WRITE( arg4, sizeof (struct vki_user_regs_struct));
      break;
   case 14:  /* PTRACE_GETFPREGS */
      POST_MEM_WRITE( arg4, sizeof (struct vki_user_i387_struct));
      break;
   case 18:  /* PTRACE_GETFPXREGS */
      POST_MEM_WRITE( arg4, sizeof(struct vki_user_fxsr_struct) );
      break;
   case 1: case 2: case 3:    /* PTRACE_PEEK{TEXT,DATA,USER} */
      POST_MEM_WRITE( arg4, sizeof (long));
      break;
   default:
      break;
   }
}

PRE(sys_set_tid_address, Special)
{
   // We don't let this syscall run, and don't do anything to simulate it
   // ourselves -- it becomes a no-op!  Why?  Tom says:
   // 
   //   I suspect this is deliberate given that all the user level threads
   //   are running in the same kernel thread under valgrind so we probably
   //   don't want to be calling the actual system call here.
   //
   // Hmm.
   PRINT("sys_set_tid_address ( %p )", arg1);
   PRE_REG_READ1(long, "set_tid_address", int *, tidptr);
}

PRE(sys_iopl, 0)
{
   PRINT("sys_iopl ( %d )", arg1);
   PRE_REG_READ1(long, "iopl", unsigned long, level);
}

PRE(sys_setxattr, MayBlock)
{
   PRINT("sys_setxattr ( %p, %p, %p, %llu, %d )",
         arg1, arg2, arg3, (ULong)arg4, arg5);
   PRE_REG_READ5(long, "setxattr",
                 char *, path, char *, name,
                 void *, value, vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "setxattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "setxattr(name)", arg2 );
   PRE_MEM_READ( "setxattr(value)", arg3, arg4 );
}

PRE(sys_lsetxattr, MayBlock)
{
   PRINT("sys_lsetxattr ( %p, %p, %p, %llu, %d )",
         arg1, arg2, arg3, (ULong)arg4, arg5);
   PRE_REG_READ5(long, "lsetxattr",
                 char *, path, char *, name,
                 void *, value, vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "lsetxattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "lsetxattr(name)", arg2 );
   PRE_MEM_READ( "lsetxattr(value)", arg3, arg4 );
}

PRE(sys_fsetxattr, MayBlock)
{
   PRINT("sys_fsetxattr ( %d, %p, %p, %llu, %d )",
         arg1, arg2, arg3, (ULong)arg4, arg5);
   PRE_REG_READ5(long, "fsetxattr",
                 int, fd, char *, name, void *, value,
                 vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "fsetxattr(name)", arg2 );
   PRE_MEM_READ( "fsetxattr(value)", arg3, arg4 );
}

PRE(sys_getxattr, MayBlock)
{
   PRINT("sys_getxattr ( %p, %p, %p, %llu )", arg1,arg2,arg3, (ULong)arg4);
   PRE_REG_READ4(ssize_t, "getxattr",
                 char *, path, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "getxattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "getxattr(name)", arg2 );
   PRE_MEM_WRITE( "getxattr(value)", arg3, arg4 );
}

POST(sys_getxattr)
{
   if (res > 0 && arg3 != (Addr)NULL) {
      POST_MEM_WRITE( arg3, res );
   }
}

PRE(sys_lgetxattr, MayBlock)
{
   PRINT("sys_lgetxattr ( %p, %p, %p, %llu )", arg1,arg2,arg3, (ULong)arg4);
   PRE_REG_READ4(ssize_t, "lgetxattr",
                 char *, path, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "lgetxattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "lgetxattr(name)", arg2 );
   PRE_MEM_WRITE( "lgetxattr(value)", arg3, arg4 );
}

POST(sys_lgetxattr)
{
   if (res > 0 && arg3 != (Addr)NULL) {
      POST_MEM_WRITE( arg3, res );
   }
}

PRE(sys_fgetxattr, MayBlock)
{
   PRINT("sys_fgetxattr ( %d, %p, %p, %llu )", arg1, arg2, arg3, (ULong)arg4);
   PRE_REG_READ4(ssize_t, "fgetxattr",
                 int, fd, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "fgetxattr(name)", arg2 );
   PRE_MEM_WRITE( "fgetxattr(value)", arg3, arg4 );
}

POST(sys_fgetxattr)
{
   if (res > 0 && arg3 != (Addr)NULL)
      POST_MEM_WRITE( arg3, res );
}

PRE(sys_listxattr, MayBlock)
{
   PRINT("sys_listxattr ( %p, %p, %llu )", arg1, arg2, (ULong)arg3);
   PRE_REG_READ3(ssize_t, "listxattr",
                 char *, path, char *, list, vki_size_t, size);
   PRE_MEM_RASCIIZ( "listxattr(path)", arg1 );
   PRE_MEM_WRITE( "listxattr(list)", arg2, arg3 );
}

POST(sys_listxattr)
{
   if (res > 0 && arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, res );
}

PRE(sys_llistxattr, MayBlock)
{
   PRINT("sys_llistxattr ( %p, %p, %llu )", arg1, arg2, (ULong)arg3);
   PRE_REG_READ3(ssize_t, "llistxattr",
                 char *, path, char *, list, vki_size_t, size);
   PRE_MEM_RASCIIZ( "llistxattr(path)", arg1 );
   PRE_MEM_WRITE( "llistxattr(list)", arg2, arg3 );
}

POST(sys_llistxattr)
{
   if (res > 0 && arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, res );
}

PRE(sys_flistxattr, MayBlock)
{
   PRINT("sys_flistxattr ( %d, %p, %llu )", arg1, arg2, (ULong)arg3);
   PRE_REG_READ3(ssize_t, "flistxattr",
                 int, fd, char *, list, vki_size_t, size);
   PRE_MEM_WRITE( "flistxattr(list)", arg2, arg3 );
}

POST(sys_flistxattr)
{
   if (res > 0 && arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, res );
}

PRE(sys_removexattr, MayBlock)
{
   PRINT("sys_removexattr ( %p, %p )", arg1, arg2);
   PRE_REG_READ2(long, "removexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "removexattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "removexattr(name)", arg2 );
}

PRE(sys_lremovexattr, MayBlock)
{
   PRINT("sys_lremovexattr ( %p, %p )", arg1, arg2);
   PRE_REG_READ2(long, "lremovexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "lremovexattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "lremovexattr(name)", arg2 );
}

PRE(sys_fremovexattr, MayBlock)
{
   PRINT("sys_fremovexattr ( %d, %p )", arg1, arg2);
   PRE_REG_READ2(long, "fremovexattr", int, fd, char *, name);
   PRE_MEM_RASCIIZ( "fremovexattr(name)", arg2 );
}

PRE(sys_quotactl, 0)
{
   PRINT("sys_quotactl (0x%x, %p, 0x%x, 0x%x )", arg1,arg2,arg3, arg4);
   PRE_REG_READ4(long, "quotactl",
                 unsigned int, cmd, const char *, special, vki_qid_t, id,
                 void *, addr);
   PRE_MEM_RASCIIZ( "quotactl(special)", arg2 );
}

// XXX: this wrapper is only suitable for 32-bit platforms
PRE(sys_lookup_dcookie, 0)
{
   PRINT("sys_lookup_dcookie (0x%llx, %p, %d)", LOHI64(arg1,arg2), arg3, arg4);
   PRE_REG_READ4(long, "lookup_dcookie",
                 vki_u32, cookie_low32, vki_u32, cookie_high32,
                 char *, buf, vki_size_t, len);
   PRE_MEM_WRITE( "lookup_dcookie(buf)", arg3, arg4);
}

POST(sys_lookup_dcookie)
{
   if (arg3 != (Addr)NULL)
      POST_MEM_WRITE( arg3, res);
}

PRE(sys_fsync, MayBlock)
{
   PRINT("sys_fsync ( %d )", arg1);
   PRE_REG_READ1(long, "fsync", unsigned int, fd);
}

PRE(sys_fdatasync, MayBlock)
{
   PRINT("sys_fdatasync ( %d )", arg1);
   PRE_REG_READ1(long, "fdatasync", unsigned int, fd);
}

PRE(sys_msync, MayBlock)
{
   PRINT("sys_msync ( %p, %llu, %d )", arg1,(ULong)arg2,arg3);
   PRE_REG_READ3(long, "msync",
                 unsigned long, start, vki_size_t, length, int, flags);
   PRE_MEM_READ( "msync(start)", arg1, arg2 );
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

PRE(sys_getpmsg, MayBlock)
{
   /* LiS getpmsg from http://www.gcom.com/home/linux/lis/ */
   struct vki_pmsg_strbuf *ctrl;
   struct vki_pmsg_strbuf *data;
   PRINT("sys_getpmsg ( %d, %p, %p, %p, %p )", arg1,arg2,arg3,arg4,arg5);
   PRE_REG_READ5(int, "getpmsg",
                 int, fd, struct strbuf *, ctrl, struct strbuf *, data, 
                 int *, bandp, int *, flagsp);
   ctrl = (struct vki_pmsg_strbuf *)arg2;
   data = (struct vki_pmsg_strbuf *)arg3;
   if (ctrl && ctrl->maxlen > 0)
      PRE_MEM_WRITE( "getpmsg(ctrl)", (Addr)ctrl->buf, ctrl->maxlen);
   if (data && data->maxlen > 0)
      PRE_MEM_WRITE( "getpmsg(data)", (Addr)data->buf, data->maxlen);
   if (arg4)
      PRE_MEM_WRITE( "getpmsg(bandp)", (Addr)arg4, sizeof(int));
   if (arg5)
      PRE_MEM_WRITE( "getpmsg(flagsp)", (Addr)arg5, sizeof(int));
}

POST(sys_getpmsg)
{
   struct vki_pmsg_strbuf *ctrl;
   struct vki_pmsg_strbuf *data;

   ctrl = (struct vki_pmsg_strbuf *)arg2;
   data = (struct vki_pmsg_strbuf *)arg3;
   if (res == 0 && ctrl && ctrl->len > 0) {
      POST_MEM_WRITE( (Addr)ctrl->buf, ctrl->len);
   }
   if (res == 0 && data && data->len > 0) {
      POST_MEM_WRITE( (Addr)data->buf, data->len);
   }
}

PRE(sys_putpmsg, MayBlock)
{
   /* LiS putpmsg from http://www.gcom.com/home/linux/lis/ */
   struct vki_pmsg_strbuf *ctrl;
   struct vki_pmsg_strbuf *data;
   PRINT("sys_putpmsg ( %d, %p, %p, %d, %d )", arg1,arg2,arg3,arg4,arg5);
   PRE_REG_READ5(int, "putpmsg",
                 int, fd, struct strbuf *, ctrl, struct strbuf *, data, 
                 int, band, int, flags);
   ctrl = (struct vki_pmsg_strbuf *)arg2;
   data = (struct vki_pmsg_strbuf *)arg3;
   if (ctrl && ctrl->len > 0)
      PRE_MEM_READ( "putpmsg(ctrl)", (Addr)ctrl->buf, ctrl->len);
   if (data && data->len > 0)
      PRE_MEM_READ( "putpmsg(data)", (Addr)data->buf, data->len);
}

PRE(sys_getitimer, NBRunInLWP)
{
   PRINT("sys_getitimer ( %d, %p )", arg1, arg2);
   PRE_REG_READ2(long, "getitimer", int, which, struct itimerval *, value);
   PRE_MEM_WRITE( "getitimer(value)", arg2, sizeof(struct vki_itimerval) );
}

POST(sys_getitimer)
{
   if (arg2 != (Addr)NULL) {
      VG_TRACK( post_mem_write,arg2, sizeof(struct vki_itimerval));
   }
}

PRE(sys_setitimer, NBRunInLWP)
{
   PRINT("sys_setitimer ( %d, %p, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "setitimer", 
                 int, which,
                 struct itimerval *, value, struct itimerval *, ovalue);
   if (arg2 != (Addr)NULL)
      PRE_MEM_READ( "setitimer(value)", arg2, sizeof(struct vki_itimerval) );
   if (arg3 != (Addr)NULL)
      PRE_MEM_WRITE( "setitimer(ovalue)", arg3, sizeof(struct vki_itimerval));
}

POST(sys_setitimer)
{
   if (arg3 != (Addr)NULL) {
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_itimerval));
   }
}

PRE(sys_chroot, 0)
{
   PRINT("sys_chroot ( %p )", arg1);
   PRE_REG_READ1(long, "chroot", const char *, path);
   PRE_MEM_RASCIIZ( "chroot(path)", arg1 );
}

PRE(sys_madvise, MayBlock)
{
   PRINT("sys_madvise ( %p, %llu, %d )", arg1,(ULong)arg2,arg3);
   PRE_REG_READ3(long, "madvise",
                 unsigned long, start, vki_size_t, length, int, advice);
}

PRE(sys_mremap, Special)
{
   // Nb: this is different to the glibc version described in the man pages,
   // which lacks the fifth 'new_address' argument.
   PRINT("sys_mremap ( %p, %llu, %d, 0x%x, %p )", 
         arg1, (ULong)arg2, arg3, arg4, arg5);
   PRE_REG_READ5(unsigned long, "mremap",
                 unsigned long, old_addr, unsigned long, old_size,
                 unsigned long, new_size, unsigned long, flags,
                 unsigned long, new_addr);
   set_result( mremap_segment((Addr)arg1, arg2, (Addr)arg5, arg3, arg4, tid) );
}

PRE(sys_nice, 0)
{
   PRINT("sys_nice ( %d )", arg1);
   PRE_REG_READ1(long, "nice", int, inc);
}

PRE(sys_sched_getscheduler, 0)
{
   PRINT("sys_sched_getscheduler ( %d )", arg1);
   PRE_REG_READ1(long, "sched_getscheduler", vki_pid_t, pid);
}

PRE(sys_sched_setscheduler, 0)
{
   PRINT("sys_sched_setscheduler ( %d, %d, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "sched_setscheduler", 
                 vki_pid_t, pid, int, policy, struct sched_param *, p);
   if (arg3 != 0)
      PRE_MEM_READ( "sched_setscheduler(p)", 
		    arg3, sizeof(struct vki_sched_param));
}

PRE(sys_mlock, MayBlock)
{
   PRINT("sys_mlock ( %p, %llu )", arg1, (ULong)arg2);
   PRE_REG_READ2(long, "mlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_munlock, MayBlock)
{
   PRINT("sys_munlock ( %p, %llu )", arg1, (ULong)arg2);
   PRE_REG_READ2(long, "munlock", unsigned long, addr, vki_size_t, len);
}

PRE(sys_mlockall, MayBlock)
{
   PRINT("sys_mlockall ( %x )", arg1);
   PRE_REG_READ1(long, "mlockall", int, flags);
}

PRE(sys_munlockall, MayBlock)
{
   PRINT("sys_munlockall ( )");
   PRE_REG_READ0(long, "munlockall");
}

PRE(sys_sched_get_priority_max, 0)
{
   PRINT("sched_get_priority_max ( %d )", arg1);
   PRE_REG_READ1(long, "sched_get_priority_max", int, policy);
}

PRE(sys_sched_get_priority_min, 0)
{
   PRINT("sched_get_priority_min ( %d )", arg1);
   PRE_REG_READ1(long, "sched_get_priority_min", int, policy);
}

PRE(sys_setpriority, 0)
{
   PRINT("sys_setpriority ( %d, %d, %d )", arg1, arg2, arg3);
   PRE_REG_READ3(long, "setpriority", int, which, int, who, int, prio);
}

PRE(sys_getpriority, 0)
{
   PRINT("sys_getpriority ( %d, %d )", arg1, arg2);
   PRE_REG_READ2(long, "getpriority", int, which, int, who);
}

PRE(sys_setregid16, 0)
{
   PRINT("sys_setregid16 ( %d, %d )", arg1, arg2);
   PRE_REG_READ2(long, "setregid16", vki_old_gid_t, rgid, vki_old_gid_t, egid);
}

// XXX: only for 32-bit archs
PRE(sys_pwrite64, MayBlock)
{
   PRINT("sys_pwrite64 ( %d, %p, %llu, %lld )",
         arg1, arg2, (ULong)arg3, LOHI64(arg4,arg5));
   PRE_REG_READ5(ssize_t, "pwrite64",
                 unsigned int, fd, const char *, buf, vki_size_t, count,
                 vki_u32, offset_low32, vki_u32, offset_high32);
   PRE_MEM_READ( "pwrite64(buf)", arg2, arg3 );
}

PRE(sys_sync, MayBlock)
{
   PRINT("sys_sync ( )");
   PRE_REG_READ0(long, "sync");
}

PRE(sys_fstatfs, 0)
{
   PRINT("sys_fstatfs ( %d, %p )",arg1,arg2);
   PRE_REG_READ2(long, "fstatfs",
                 unsigned int, fd, struct statfs *, buf);
   PRE_MEM_WRITE( "fstatfs(buf)", arg2, sizeof(struct vki_statfs) );
}

POST(sys_fstatfs)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_statfs) );
}

PRE(sys_fstatfs64, 0)
{
   PRINT("sys_fstatfs64 ( %d, %llu, %p )",arg1,(ULong)arg2,arg3);
   PRE_REG_READ3(long, "fstatfs64",
                 unsigned int, fd, vki_size_t, size, struct statfs64 *, buf);
   PRE_MEM_WRITE( "fstatfs64(buf)", arg3, arg2 );
}

POST(sys_fstatfs64)
{
   POST_MEM_WRITE( arg3, arg2 );
}

PRE(sys_getsid, 0)
{
   PRINT("sys_getsid ( %d )", arg1);
   PRE_REG_READ1(long, "getsid", vki_pid_t, pid);
}

// XXX: only for 32-bit archs
PRE(sys_pread64, MayBlock)
{
   PRINT("sys_pread64 ( %d, %p, %llu, %lld )",
         arg1, arg2, (ULong)arg3, LOHI64(arg4,arg5));
   PRE_REG_READ5(ssize_t, "pread64",
                 unsigned int, fd, char *, buf, vki_size_t, count,
                 vki_u32, offset_low32, vki_u32, offset_high32);
   PRE_MEM_WRITE( "pread64(buf)", arg2, arg3 );
}

POST(sys_pread64)
{
   if (res > 0) {
      POST_MEM_WRITE( arg2, res );
   }
}

PRE(sys_mknod, 0)
{
   PRINT("sys_mknod ( %p, 0x%x, 0x%x )", arg1, arg2, arg3 );
   PRE_REG_READ3(long, "mknod",
                 const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknod(pathname)", arg1 );
}

PRE(sys_flock, MayBlock)
{
   PRINT("sys_flock ( %d, %d )", arg1, arg2 );
   PRE_REG_READ2(long, "flock", unsigned int, fd, unsigned int, operation);
}

PRE(sys_init_module, MayBlock)
{
   PRINT("sys_init_module ( %p, %llu, %p )", arg1, (ULong)arg2, arg3 );
   PRE_REG_READ3(long, "init_module",
                 void *, umod, unsigned long, len, const char *, uargs);
   PRE_MEM_READ( "init_module(umod)", arg1, arg2 );
   PRE_MEM_RASCIIZ( "init_module(uargs)", arg3 );
}

PRE(sys_capget, 0)
{
   PRINT("sys_capget ( %p, %p )", arg1, arg2 );
   PRE_REG_READ2(long, "capget", 
                 vki_cap_user_header_t, header, vki_cap_user_data_t, data);
   PRE_MEM_READ( "capget(header)", arg1, 
                  sizeof(struct __vki_user_cap_header_struct) );
   PRE_MEM_WRITE( "capget(data)", arg2, 
                  sizeof(struct __vki_user_cap_data_struct) );
}

POST(sys_capget)
{
   if (arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, sizeof(struct __vki_user_cap_data_struct) );
}

PRE(sys_capset, 0)
{
   PRINT("sys_capset ( %p, %p )", arg1, arg2 );
   PRE_REG_READ2(long, "capset", 
                 vki_cap_user_header_t, header,
                 const vki_cap_user_data_t, data);
   PRE_MEM_READ( "capset(header)", 
                  arg1, sizeof(struct __vki_user_cap_header_struct) );
   PRE_MEM_READ( "capset(data)", 
                  arg2, sizeof(struct __vki_user_cap_data_struct) );
}

// Pre_read a char** argument.
void pre_argv_envp(Addr a, ThreadId tid, Char* s1, Char* s2)
{
   while (True) {
      Addr a_deref = deref_Addr( tid, a, s1 );
      if (0 == a_deref)
         break;
      PRE_MEM_RASCIIZ( s2, a_deref );
      a += sizeof(char*);
   }
}

// XXX: prototype here seemingly doesn't match the prototype for i386-linux,
// but it seems to work nonetheless...
PRE(sys_execve, Special)
{
   PRINT("sys_execve ( %p(%s), %p, %p )", arg1, arg1, arg2, arg3);
   PRE_REG_READ3(vki_off_t, "execve",
                 char *, filename, char **, argv, char **, envp);
   PRE_MEM_RASCIIZ( "execve(filename)", arg1 );
   if (arg2 != 0)
      pre_argv_envp( arg2, tid, "execve(argv)", "execve(argv[i])" );
   if (arg3 != 0)
      pre_argv_envp( arg3, tid, "execve(envp)", "execve(envp[i])" );

   /* Erk.  If the exec fails, then the following will have made a
      mess of things which makes it hard for us to continue.  The
      right thing to do is piece everything together again in
      POST(execve), but that's hard work.  Instead, we make an effort
      to check that the execve will work before actually calling
      exec. */
   {
      struct vki_stat st;
      Int ret = VG_(stat)((Char *)arg1, &st);

      if (ret < 0) {
	 set_result( ret );
	 return;
      }
      /* just look for regular file with any X bit set
	 XXX do proper permissions check?
       */
      if ((st.st_mode & 0100111) == 0100000) {
	 set_result( -VKI_EACCES );
	 return;
      }
   }

   /* Resistance is futile.  Nuke all other threads.  POSIX mandates
      this. (Really, nuke them all, since the new process will make
      its own new thread.) */
   VG_(nuke_all_threads_except)( VG_INVALID_THREADID );

   {
      // Remove the valgrind-specific stuff from the environment so the
      // child doesn't get our libpthread and other stuff.  This is
      // done unconditionally, since if we are tracing the child,
      // stage1/2 will set up the appropriate client environment.
      Char** envp = (Char**)arg3;

      if (envp != NULL) {
         VG_(env_remove_valgrind_env_stuff)( envp ); 
      }
   }

   if (VG_(clo_trace_children)) {
      Char* optvar = VG_(build_child_VALGRINDCLO)( (Char*)arg1 );

      // Set VALGRINDCLO and VALGRINDLIB in arg3 (the environment)
      VG_(env_setenv)( (Char***)&arg3, VALGRINDCLO, optvar);
      VG_(env_setenv)( (Char***)&arg3, VALGRINDLIB, VG_(libdir));

      // Create executable name: "/proc/self/fd/<vgexecfd>", update arg1
      arg1 = (Addr)VG_(build_child_exename)();
   }

   if (0) {
      Char **cpp;

      VG_(printf)("exec: %s\n", (Char *)arg1);
      for(cpp = (Char **)arg2; cpp && *cpp; cpp++)
         VG_(printf)("argv: %s\n", *cpp);
      for(cpp = (Char **)arg3; cpp && *cpp; cpp++)
         VG_(printf)("env: %s\n", *cpp);
   }

   /* Set our real sigmask to match the client's sigmask so that the
      exec'd child will get the right mask.  First we need to clear
      out any pending signals so they they don't get delivered, which
      would confuse things.

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
    
      VG_(sigfillset)(&allsigs);
      while(VG_(sigtimedwait)(&allsigs, &info, &zero) > 0)
	 ;

      VG_(sigprocmask)(VKI_SIG_SETMASK, &tst->sig_mask, NULL);
   }

   /* restore the DATA rlimit for the child */
   VG_(setrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));

   set_result( VG_(do_syscall)(__NR_execve, arg1, arg2, arg3) );

   /* If we got here, then the execve failed.  We've already made too much of a mess
      of ourselves to continue, so we have to abort. */
   VG_(message)(Vg_UserMsg, "execve(%p(%s), %p, %p) failed, errno %d",
		arg1, arg1, arg2, arg3, -res);
   VG_(core_panic)("EXEC FAILED: I can't recover from execve() failing, so I'm dying.\n"
		   "Add more stringent tests in PRE(execve), or work out how to recover.");   
}

PRE(sys_access, 0)
{
   PRINT("sys_access ( %p(%s), %d )", arg1,arg1,arg2);
   PRE_REG_READ2(long, "access", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "access(pathname)", arg1 );
}

PRE(sys_alarm, NBRunInLWP)
{
   PRINT("sys_alarm ( %d )", arg1);
   PRE_REG_READ1(unsigned long, "alarm", unsigned int, seconds);
}

PRE(sys_brk, Special)
{
   Addr brk_limit = VG_(brk_limit);

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
   PRINT("sys_brk ( %p )", arg1);
   PRE_REG_READ1(unsigned long, "brk", unsigned long, end_data_segment);

   set_result( do_brk(arg1) );

   if (res == arg1) {
      /* brk() succeeded */
      if (res < brk_limit) {
         /* successfully shrunk the data segment. */
         VG_TRACK( die_mem_brk, (Addr)arg1,
		   brk_limit-arg1 );
      } else
      if (res > brk_limit) {
         /* successfully grew the data segment */
         VG_TRACK( new_mem_brk, brk_limit,
                                arg1-brk_limit );
      }
   } else {
      /* brk() failed */
      vg_assert(brk_limit == res);
   }
}

PRE(sys_chdir, 0)
{
   PRINT("sys_chdir ( %p )", arg1);
   PRE_REG_READ1(long, "chdir", const char *, path);
   PRE_MEM_RASCIIZ( "chdir(path)", arg1 );
}

PRE(sys_chmod, 0)
{
   PRINT("sys_chmod ( %p, %d )", arg1,arg2);
   PRE_REG_READ2(long, "chmod", const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "chmod(path)", arg1 );
}

PRE(sys_chown16, 0)
{
   PRINT("sys_chown16 ( %p, 0x%x, 0x%x )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "chown16",
                 const char *, path,
                 vki_old_uid_t, owner, vki_old_gid_t, group);
   PRE_MEM_RASCIIZ( "chown16(path)", arg1 );
}

PRE(sys_chown, 0)
{
   /* int chown(const char *path, uid_t owner, gid_t group); */
   PRINT("sys_chown ( %p, 0x%x, 0x%x )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "chown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "chown(path)", arg1 );
}

PRE(sys_lchown, 0)
{
   PRINT("sys_lchown ( %p, 0x%x, 0x%x )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "lchown",
                 const char *, path, vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "lchown(path)", arg1 );
}

PRE(sys_close, 0)
{
   PRINT("sys_close ( %d )", arg1);
   PRE_REG_READ1(long, "close", unsigned int, fd);

   /* Detect and negate attempts by the client to close Valgrind's log fd */
   if (!VG_(fd_allowed)(arg1, "close", tid, False))
      set_result( -VKI_EBADF );
}

POST(sys_close)
{
   if (VG_(clo_track_fds)) record_fd_close(tid, arg1);
}

PRE(sys_dup, 0)
{
   PRINT("sys_dup ( %d )", arg1);
   PRE_REG_READ1(long, "dup", unsigned int, oldfd);
}

POST(sys_dup)
{
   if (!VG_(fd_allowed)(res, "dup", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, res, VG_(resolve_filename)(res));
   }
}

PRE(sys_dup2, 0)
{
   PRINT("sys_dup2 ( %d, %d )", arg1,arg2);
   PRE_REG_READ2(long, "dup2", unsigned int, oldfd, unsigned int, newfd);
   if (!VG_(fd_allowed)(arg2, "dup2", tid, True))
      set_result( -VKI_EBADF );
}

POST(sys_dup2)
{
   if (VG_(clo_track_fds))
      VG_(record_fd_open)(tid, res, VG_(resolve_filename)(res));
}

PRE(sys_fchdir, 0)
{
   PRINT("sys_fchdir ( %d )", arg1);
   PRE_REG_READ1(long, "fchdir", unsigned int, fd);
}

PRE(sys_fchown16, 0)
{
   PRINT("sys_fchown16 ( %d, %d, %d )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "fchown16",
                 unsigned int, fd, vki_old_uid_t, owner, vki_old_gid_t, group);
}

PRE(sys_fchown, 0)
{
   PRINT("sys_fchown ( %d, %d, %d )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "fchown",
                 unsigned int, fd, vki_uid_t, owner, vki_gid_t, group);
}

PRE(sys_fchmod, 0)
{
   PRINT("sys_fchmod ( %d, %d )", arg1,arg2);
   PRE_REG_READ2(long, "fchmod", unsigned int, fildes, vki_mode_t, mode);
}

void common_pre_fcntl(Bool is64, ThreadId tid, ThreadState* tst)
{
   switch (arg2) {
   // These ones ignore arg3.
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
   case VKI_F_SETOWN:
   case VKI_F_GETSIG:
   case VKI_F_SETSIG:
   case VKI_F_GETLEASE:
      if (is64) {
         PRINT("sys_fcntl64 ( %d, %d )", arg1,arg2);
         PRE_REG_READ2(long, "fcntl64", unsigned int, fd, unsigned int, cmd);
      } else {
         PRINT("sys_fcntl ( %d, %d )", arg1,arg2);
         PRE_REG_READ2(long, "fcntl", unsigned int, fd, unsigned int, cmd);
      }
      break;

   // These ones use arg3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETLEASE:
   case VKI_F_NOTIFY:
      if (is64) {
         PRINT("sys_fcntl64[arg3=='arg'] ( %d, %d, %d )", arg1,arg2,arg3);
         PRE_REG_READ3(long, "fcntl64",
                       unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      } else {
         PRINT("sys_fcntl[arg3=='arg'] ( %d, %d, %d )", arg1,arg2,arg3);
         PRE_REG_READ3(long, "fcntl",
                       unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      }
      break;

   // These ones use arg3 as "lock".
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
   case VKI_F_GETLK64:
   case VKI_F_SETLK64:
   case VKI_F_SETLKW64:
      if (is64) {
         PRINT("sys_fcntl64[arg3=='lock'] ( %d, %d, %p )", arg1,arg2,arg3);
         PRE_REG_READ3(long, "fcntl64",
                       unsigned int, fd, unsigned int, cmd,
                       struct flock64 *, lock);
      } else {
         PRINT("sys_fcntl[arg3=='lock'] ( %d, %d, %p )", arg1,arg2,arg3);
         PRE_REG_READ3(long, "fcntl",
                       unsigned int, fd, unsigned int, cmd,
                       struct flock64 *, lock);
      }
      break;
   }
}

PRE(sys_fcntl, 0)
{
   common_pre_fcntl(/*is64*/False, tid, tst);

   if (arg2 == VKI_F_SETLKW)
      tst->sys_flags |= MayBlock;
}

POST(sys_fcntl)
{
   if (arg2 == VKI_F_DUPFD) {
      if (!VG_(fd_allowed)(res, "fcntl(DUPFD)", tid, True)) {
         VG_(close)(res);
         set_result( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, res, VG_(resolve_filename)(res));
      }
   }
}

// XXX: wrapper only suitable for 32-bit systems
PRE(sys_fcntl64, 0)
{
   common_pre_fcntl(/*is64*/True, tid, tst);
   
   if (arg2 == VKI_F_SETLKW || arg2 == VKI_F_SETLKW64)
      tst->sys_flags |= MayBlock;
}

POST(sys_fcntl64)
{
   if (arg2 == VKI_F_DUPFD) {
      if (!VG_(fd_allowed)(res, "fcntl64(DUPFD)", tid, True)) {
         VG_(close)(res);
         set_result( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, res, VG_(resolve_filename)(res));
      }
   }
}

PRE(sys_newfstat, 0)
{
   PRINT("sys_newfstat ( %d, %p )", arg1,arg2);
   PRE_REG_READ2(long, "fstat", unsigned int, fd, struct stat *, buf);
   PRE_MEM_WRITE( "fstat(buf)", arg2, sizeof(struct vki_stat) );
}

POST(sys_newfstat)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat) );
}

static vki_sigset_t fork_saved_mask;

// In Linux, the sys_fork() function varies across architectures, but we
// ignore the various args it gets, and so it looks arch-neutral.  Hmm.
PRE(sys_fork, 0)
{
   vki_sigset_t mask;

   PRINT("sys_fork ( )");
   PRE_REG_READ0(long, "fork");

   vg_assert(VG_(gettid)() == VG_(main_pid));

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   do_atfork_pre(tid);
}

POST(sys_fork)
{
   if (res == 0) {
      do_atfork_child(tid);

      /* I am the child.  Nuke all other threads which I might
	 have inherited from my parent.  POSIX mandates this. */
      VG_(nuke_all_threads_except)( tid );

      /* XXX TODO: tid 1 is special, and is presumed to be present.
	 We should move this TID to 1 in the child. */

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } else {
      PRINT("   fork: process %d created child %d\n", VG_(main_pid), res);

      do_atfork_parent(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }
}

PRE(sys_ftruncate, MayBlock)
{
   PRINT("sys_ftruncate ( %d, %lld )", arg1,(ULong)arg2);
   PRE_REG_READ2(long, "ftruncate", unsigned int, fd, unsigned long, length);
}

PRE(sys_truncate, MayBlock)
{
   PRINT("sys_truncate ( %p(%s), %d )", arg1,arg1,arg2);
   PRE_REG_READ2(long, "truncate", 
                 const char *, path, unsigned long, length);
   PRE_MEM_RASCIIZ( "truncate(path)", arg1 );
}

// XXX: this wrapper is only suitable for 32-bit platforms
PRE(sys_ftruncate64, MayBlock)
{
   PRINT("sys_ftruncate64 ( %d, %lld )", arg1, LOHI64(arg2,arg3));
   PRE_REG_READ3(long, "ftruncate64",
                 unsigned int, fd,
                 vki_u32, length_low32, vki_u32, length_high32);
}

// XXX: this wrapper is only suitable for 32-bit platforms
PRE(sys_truncate64, MayBlock)
{
   PRINT("sys_truncate64 ( %p, %lld )", arg1, LOHI64(arg2, arg3));
   PRE_REG_READ3(long, "truncate64",
                 const char *, path,
                 vki_u32, length_low32, vki_u32, length_high32);
   PRE_MEM_RASCIIZ( "truncate64(path)", arg1 );
}


PRE(sys_getdents, MayBlock)
{
   PRINT("sys_getdents ( %d, %p, %d )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "getdents",
                 unsigned int, fd, struct linux_dirent *, dirp,
                 unsigned int, count);
   PRE_MEM_WRITE( "getdents(dirp)", arg2, arg3 );
}

POST(sys_getdents)
{
   if (res > 0)
      POST_MEM_WRITE( arg2, res );
}

PRE(sys_getdents64, MayBlock)
{
   PRINT("sys_getdents64 ( %d, %p, %d )",arg1,arg2,arg3);
   PRE_REG_READ3(long, "getdents64",
                 unsigned int, fd, struct linux_dirent64 *, dirp,
                 unsigned int, count);
   PRE_MEM_WRITE( "getdents64(dirp)", arg2, arg3 );
}

POST(sys_getdents64)
{
   if (res > 0)
      POST_MEM_WRITE( arg2, res );
}

PRE(sys_getgroups16, 0)
{
   PRINT("sys_getgroups16 ( %d, %p )", arg1, arg2);
   PRE_REG_READ2(long, "getgroups16", int, size, vki_old_gid_t *, list);
   if (arg1 > 0)
      PRE_MEM_WRITE( "getgroups16(list)", arg2, arg1 * sizeof(vki_old_gid_t) );
}

POST(sys_getgroups16)
{
   if (arg1 > 0 && res > 0)
      POST_MEM_WRITE( arg2, res * sizeof(vki_old_gid_t) );
}

PRE(sys_getgroups, 0)
{
   PRINT("sys_getgroups ( %d, %p )", arg1, arg2);
   PRE_REG_READ2(long, "getgroups", int, size, vki_gid_t *, list);
   if (arg1 > 0)
      PRE_MEM_WRITE( "getgroups(list)", arg2, arg1 * sizeof(vki_gid_t) );
}

POST(sys_getgroups)
{
   if (arg1 > 0 && res > 0)
      POST_MEM_WRITE( arg2, res * sizeof(vki_gid_t) );
}

PRE(sys_getcwd, 0)
{
   // Note that the kernel version of getcwd() behaves quite differently to
   // the glibc one.
   PRINT("sys_getcwd ( %p, %llu )", arg1,(ULong)arg2);
   PRE_REG_READ2(long, "getcwd", char *, buf, unsigned long, size);
   PRE_MEM_WRITE( "getcwd(buf)", arg1, arg2 );
}

POST(sys_getcwd)
{
   if (res != (Addr)NULL)
      POST_MEM_WRITE( arg1, res );
}

PRE(sys_geteuid16, 0)
{
   PRINT("sys_geteuid16 ( )");
   PRE_REG_READ0(long, "geteuid16");
}

PRE(sys_geteuid, 0)
{
   PRINT("sys_geteuid ( )");
   PRE_REG_READ0(long, "geteuid");
}

PRE(sys_getegid16, 0)
{
   PRINT("sys_getegid16 ( )");
   PRE_REG_READ0(long, "getegid16");
}

PRE(sys_getegid, 0)
{
   PRINT("sys_getegid ( )");
   PRE_REG_READ0(long, "getegid");
}

PRE(sys_getgid16, 0)
{
   PRINT("sys_getgid16 ( )");
   PRE_REG_READ0(long, "getgid16");
}

PRE(sys_getgid, 0)
{
   PRINT("sys_getgid ( )");
   PRE_REG_READ0(long, "getgid");
}

PRE(sys_getpid, 0)
{
   PRINT("sys_getpid ()");
   PRE_REG_READ0(long, "getpid");
}

PRE(sys_getpgid, 0)
{
   PRINT("sys_getpgid ( %d )", arg1);
   PRE_REG_READ1(long, "getpgid", vki_pid_t, pid);
}

PRE(sys_getpgrp, 0)
{
   PRINT("sys_getpgrp ()");
   PRE_REG_READ0(long, "getpgrp");
}

PRE(sys_getppid, 0)
{
   PRINT("sys_getppid ()");
   PRE_REG_READ0(long, "getppid");
}

static void common_post_getrlimit(UWord a1, UWord a2)
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

PRE(sys_old_getrlimit, 0)
{
   PRINT("sys_old_getrlimit ( %d, %p )", arg1,arg2);
   PRE_REG_READ2(long, "old_getrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_WRITE( "old_getrlimit(rlim)", arg2, sizeof(struct vki_rlimit) );
}

POST(sys_old_getrlimit)
{
   common_post_getrlimit(arg1, arg2);
}

PRE(sys_getrlimit, 0)
{
   PRINT("sys_getrlimit ( %d, %p )", arg1,arg2);
   PRE_REG_READ2(long, "getrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_WRITE( "getrlimit(rlim)", arg2, sizeof(struct vki_rlimit) );
}

POST(sys_getrlimit)
{
   common_post_getrlimit(arg1, arg2);
}

PRE(sys_getrusage, 0)
{
   /* int getrusage (int who, struct rusage *usage); */
   PRINT("sys_getrusage ( %d, %p )", arg1,arg2);
   PRE_REG_READ2(long, "getrusage", int, who, struct rusage *, usage);
   PRE_MEM_WRITE( "getrusage(usage)", arg2, sizeof(struct vki_rusage) );
}

POST(sys_getrusage)
{
   if (res == 0)
      VG_TRACK( post_mem_write, arg2, sizeof(struct vki_rusage) );
}

PRE(sys_gettimeofday, 0)
{
   PRINT("sys_gettimeofday ( %p, %p )", arg1,arg2);
   PRE_REG_READ2(long, "gettimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   PRE_MEM_WRITE( "gettimeofday(tv)", arg1, sizeof(struct vki_timeval) );
   if (arg2 != 0)
      PRE_MEM_WRITE( "gettimeofday(tz)", arg2, sizeof(struct vki_timezone) );
}

POST(sys_gettimeofday)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(struct vki_timeval) );
      if (arg2 != 0)
	 POST_MEM_WRITE( arg2, sizeof(struct vki_timezone) );
   }
}

PRE(sys_settimeofday, 0)
{
   PRINT("sys_settimeofday ( %p, %p )", arg1,arg2);
   PRE_REG_READ2(long, "settimeofday",
                 struct timeval *, tv, struct timezone *, tz);
   PRE_MEM_READ( "settimeofday(tv)", arg1, sizeof(struct vki_timeval) );
   if (arg2 != 0) {
      PRE_MEM_READ( "settimeofday(tz)", arg2, sizeof(struct vki_timezone) );
      /* maybe should warn if tz->tz_dsttime is non-zero? */
   }
}

PRE(sys_getuid16, 0)
{
   PRINT("sys_getuid16 ( )");
   PRE_REG_READ0(long, "getuid16");
}

PRE(sys_getuid, 0)
{
   PRINT("sys_getuid ( )");
   PRE_REG_READ0(long, "getuid");
}

// XXX: x86-specific
// XXX: should use the constants here (eg. SHMAT), not the numbers directly!
PRE(sys_ipc, 0)
{
   PRINT("sys_ipc ( %d, %d, %d, %d, %p, %d )", arg1,arg2,arg3,arg4,arg5,arg6);
   // XXX: this is simplistic -- some args are not used in all circumstances.
   PRE_REG_READ6(int, "ipc",
                 vki_uint, call, int, first, int, second, int, third,
                 void *, ptr, long, fifth)

   switch (arg1 /* call */) {
   case 1: /* IPCOP_semop */
      PRE_MEM_READ( "semop(sops)", arg5, arg3 * sizeof(struct vki_sembuf) );
      tst->sys_flags |= MayBlock;
      break;
   case 2: /* IPCOP_semget */
      break;
   case 3: /* IPCOP_semctl */
   {
      union vki_semun *arg = (union vki_semun *)arg5;
      switch (arg4 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_SEM_INFO:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(IPC_INFO, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_INFO, arg->buf)", buf, 
			sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT:
      case VKI_SEM_STAT:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_STAT, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_STAT, arg->buf)", buf, 
			sizeof(struct vki_semid_ds) );
	 break;
      }
      case VKI_IPC_SET:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_SET, arg)" );
	 PRE_MEM_READ( "semctl(IPC_SET, arg->buf)", buf, 
			sizeof(struct vki_semid_ds) );
	 break;
      }
      case VKI_GETALL:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_GETALL, arg)" );
         UInt nsems = get_sem_count( arg2 );
	 PRE_MEM_WRITE( "semctl(IPC_GETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETALL:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_SETALL, arg)" );
         UInt nsems = get_sem_count( arg2 );
	 PRE_MEM_READ( "semctl(IPC_SETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETVAL:
      {
	 PRE_MEM_READ( "semctl(IPC_SETVAL, arg->array)",
                        (Addr)&arg->val, sizeof(arg->val) );
	 break;
      }
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_SEM_INFO|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(IPC_INFO, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_INFO, arg->buf)", buf, 
			sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SEM_STAT|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_STAT, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_STAT, arg->buf)", buf, 
			sizeof(struct vki_semid64_ds) );
	 break;
      }
      case VKI_IPC_SET|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_SET, arg)" );
	 PRE_MEM_READ( "semctl(IPC_SET, arg->buf)", buf, 
			sizeof(struct vki_semid64_ds) );
	 break;
      }
      case VKI_GETALL|VKI_IPC_64:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_GETALL, arg)" );
         UInt nsems = get_sem_count( arg2 );
	 PRE_MEM_WRITE( "semctl(IPC_GETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETALL|VKI_IPC_64:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_SETALL, arg)" );
         UInt nsems = get_sem_count( arg2 );
	 PRE_MEM_READ( "semctl(IPC_SETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETVAL|VKI_IPC_64:
      {
	 PRE_MEM_READ( "semctl(IPC_SETVAL, arg->array)",
                        (Addr)&arg->val, sizeof(arg->val) );
	 break;
      }
      default:
	 break;
      }
      break;
   }
   case 4: /* IPCOP_semtimedop */
      PRE_MEM_READ( "semtimedop(sops)", arg5, 
		     arg3 * sizeof(struct vki_sembuf) );
      if (arg6 != 0)
         PRE_MEM_READ( "semtimedop(timeout)", arg6, 
                        sizeof(struct vki_timespec) );
      tst->sys_flags |= MayBlock;
      break;
   case 11: /* IPCOP_msgsnd */
   {
      struct vki_msgbuf *msgp = (struct vki_msgbuf *)arg5;
      Int msgsz = arg3;

      PRE_MEM_READ( "msgsnd(msgp->mtype)", 
		     (Addr)&msgp->mtype, sizeof(msgp->mtype) );
      PRE_MEM_READ( "msgsnd(msgp->mtext)", 
		     (Addr)msgp->mtext, msgsz );

      if ((arg4 & VKI_IPC_NOWAIT) == 0)
         tst->sys_flags |= MayBlock;
      break;
   }
   case 12: /* IPCOP_msgrcv */
   {
      struct vki_msgbuf *msgp;
      Int msgsz = arg3;
 
      msgp = (struct vki_msgbuf *)deref_Addr( tid,
					  (Addr) (&((struct vki_ipc_kludge *)arg5)->msgp),
					  "msgrcv(msgp)" );

      PRE_MEM_WRITE( "msgrcv(msgp->mtype)", 
		     (Addr)&msgp->mtype, sizeof(msgp->mtype) );
      PRE_MEM_WRITE( "msgrcv(msgp->mtext)", 
		     (Addr)msgp->mtext, msgsz );

      if ((arg4 & VKI_IPC_NOWAIT) == 0)
         tst->sys_flags |= MayBlock;
      break;
   }
   case 13: /* IPCOP_msgget */
      break;
   case 14: /* IPCOP_msgctl */
   {
      switch (arg3 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_MSG_INFO:
	 PRE_MEM_WRITE( "msgctl(IPC_INFO, buf)", arg5, 
			sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT:
      case VKI_MSG_STAT:
	 PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)", arg5, 
			sizeof(struct vki_msqid_ds) );
	 break;
      case VKI_IPC_SET:
	 PRE_MEM_READ( "msgctl(IPC_SET, buf)", arg5, 
			sizeof(struct vki_msqid_ds) );
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_MSG_INFO|VKI_IPC_64:
	 PRE_MEM_WRITE( "msgctl(IPC_INFO, buf)", arg5, 
			sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_MSG_STAT|VKI_IPC_64:
	 PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)", arg5, 
			sizeof(struct vki_msqid64_ds) );
	 break;
      case VKI_IPC_SET|VKI_IPC_64:
	 PRE_MEM_READ( "msgctl(IPC_SET, buf)", arg5, 
			sizeof(struct vki_msqid64_ds) );
	 break;
      default:
	 break;
      }
      break;
   }
   case 21: /* IPCOP_shmat */
   {
      UInt shmid = arg2;
      UInt segmentSize = get_shm_size ( shmid );
      
      /* If they didn't ask for a particular address, then place it
	 like an mmap. */
      if (arg5 == 0)
	 arg5 = VG_(find_map_space)(0, segmentSize, True);
      else if (!VG_(valid_client_addr)(arg5, segmentSize, tid, "shmat"))
	 set_result( -VKI_EINVAL );
      break;
   }
   case 22: /* IPCOP_shmdt */
      if (!VG_(valid_client_addr)(arg5, 1, tid, "shmdt"))
	 set_result( -VKI_EINVAL );
      break;
   case 23: /* IPCOP_shmget */
      break;
   case 24: /* IPCOP_shmctl */
   {
      switch (arg3 /* cmd */) {
      case VKI_IPC_INFO:
	 PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)", arg5, 
			sizeof(struct vki_shminfo) );
	 break;
      case VKI_SHM_INFO:
	 PRE_MEM_WRITE( "shmctl(SHM_INFO, buf)", arg5, 
			sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT:
      case VKI_SHM_STAT:
	 PRE_MEM_WRITE( "shmctl(IPC_STAT, buf)", arg5, 
			sizeof(struct vki_shmid_ds) );
	 break;
      case VKI_IPC_SET:
	 PRE_MEM_READ( "shmctl(IPC_SET, buf)", arg5, 
			sizeof(struct vki_shmid_ds) );
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
	 PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)", arg5, 
			sizeof(struct vki_shminfo64) );
	 break;
      case VKI_SHM_INFO|VKI_IPC_64:
	 PRE_MEM_WRITE( "shmctl(SHM_INFO, buf)", arg5, 
			sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SHM_STAT|VKI_IPC_64:
	 PRE_MEM_WRITE( "shmctl(IPC_STAT, buf)", arg5, 
			sizeof(struct vki_shmid64_ds) );
	 break;
      case VKI_IPC_SET|VKI_IPC_64:
	 PRE_MEM_READ( "shmctl(IPC_SET, buf)", arg5, 
			sizeof(struct vki_shmid_ds) );
	 break;
      default:
	 break;
      }
      break;
   }
   default:
      VG_(message)(Vg_DebugMsg,
		   "FATAL: unhandled syscall(ipc) %d",
		   arg1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }   
}

POST(sys_ipc)
{
   switch (arg1 /* call */) {
   case 1: /* IPCOP_semop */
   case 2: /* IPCOP_semget */
      break;
   case 3: /* IPCOP_semctl */
   {
      union vki_semun *arg = (union vki_semun *)arg5;
      switch (arg4 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_SEM_INFO:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT:
      case VKI_SEM_STAT:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_semid_ds) );
	 break;
      }
      case VKI_GETALL:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(arg)" );
         UInt nsems = get_sem_count( arg2 );
	 POST_MEM_WRITE( array, sizeof(short) * nsems );
	 break;
      }
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_SEM_INFO|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SEM_STAT|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_semid64_ds) );
	 break;
      }
      case VKI_GETALL|VKI_IPC_64:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(arg)" );
         UInt nsems = get_sem_count( arg2 );
	 POST_MEM_WRITE( array, sizeof(short) * nsems );
	 break;
      }
      default:
	 break;
      }
      break;
   }
   case 4: /* IPCOP_semtimedop */
      break;
   case 11: /* IPCOP_msgsnd */
      break;
   case 12: /* IPCOP_msgrcv */
   {
      struct vki_msgbuf *msgp;
 
      msgp = (struct vki_msgbuf *)deref_Addr( tid,
					  (Addr) (&((struct vki_ipc_kludge *)arg5)->msgp),
					  "msgrcv(msgp)" );
      if ( res > 0 ) {
	 POST_MEM_WRITE( (Addr)&msgp->mtype, sizeof(msgp->mtype) );
	 POST_MEM_WRITE( (Addr)msgp->mtext, res );
      }
      break;
   }
   case 13: /* IPCOP_msgget */
      break;
   case 14: /* IPCOP_msgctl */
   {
      switch (arg3 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_MSG_INFO:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT:
      case VKI_MSG_STAT:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_msqid_ds) );
	 break;
      case VKI_IPC_SET:
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_MSG_INFO|VKI_IPC_64:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_MSG_STAT|VKI_IPC_64:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_msqid64_ds) );
	 break;
      case VKI_IPC_SET|VKI_IPC_64:
	 break;
      default:
	 break;
      }
      break;
   }
   case 21: /* IPCOP_shmat */
   {
      Int shmid = arg2;
      Int shmflag = arg3;
      Addr addr;

      /* force readability. before the syscall it is
       * indeed uninitialized, as can be seen in
       * glibc/sysdeps/unix/sysv/linux/shmat.c */
      POST_MEM_WRITE( arg4, sizeof( ULong ) );

      addr = deref_Addr ( tid, arg4, "shmat(addr)" );
      if ( addr > 0 ) { 
	 UInt segmentSize = get_shm_size ( shmid );
	 if ( segmentSize > 0 ) {
	    UInt prot = VKI_PROT_READ|VKI_PROT_WRITE;
	    /* we don't distinguish whether it's read-only or
	     * read-write -- it doesn't matter really. */
	    VG_TRACK( new_mem_mmap, addr, segmentSize, True, True, False );

	    if (!(shmflag & 010000)) /* = SHM_RDONLY */
	       prot &= ~VKI_PROT_WRITE;
	    VG_(map_segment)(addr, segmentSize, prot, SF_SHARED|SF_SHM);
	 }
      }
      break;
   }
   case 22: /* IPCOP_shmdt */
   {
      Segment *s = VG_(find_segment)(arg5);

      if (s != NULL && (s->flags & SF_SHM) && VG_(seg_contains)(s, arg5, 1)) {
	 VG_TRACK( die_mem_munmap, s->addr, s->len );
	 VG_(unmap_range)(s->addr, s->len);
      }
      break;
   }
   case 23: /* IPCOP_shmget */
      break;
   case 24: /* IPCOP_shmctl */
   {
      switch (arg3 /* cmd */) {
      case VKI_IPC_INFO:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_shminfo) );
	 break;
      case VKI_SHM_INFO:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT:
      case VKI_SHM_STAT:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_shmid_ds) );
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_shminfo64) );
	 break;
      case VKI_SHM_INFO|VKI_IPC_64:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SHM_STAT|VKI_IPC_64:
	 POST_MEM_WRITE( arg5, sizeof(struct vki_shmid64_ds) );
	 break;
      default:
	 break;
      }
      break;
   }
   default:
      VG_(message)(Vg_DebugMsg,
		   "FATAL: unhandled syscall(ipc) %d",
		   arg1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
}

// XXX: I reckon some of these cases must be x86-specific
PRE(sys_ioctl, MayBlock)
{
   PRINT("sys_ioctl ( %d, 0x%x, %p )",arg1,arg2,arg3);
   PRE_REG_READ3(long, "ioctl",
                 unsigned int, fd, unsigned int, request, unsigned long, arg);

   switch (arg2 /* request */) {
   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
      PRE_MEM_READ( "ioctl(TCSET{S,SW,SF})", arg3, sizeof(struct vki_termios) );
      break; 
   case VKI_TCGETS:
      PRE_MEM_WRITE( "ioctl(TCGETS)", arg3, sizeof(struct vki_termios) );
      break;
   case VKI_TCSETA:
   case VKI_TCSETAW:
   case VKI_TCSETAF:
      PRE_MEM_READ( "ioctl(TCSET{A,AW,AF})", arg3, sizeof(struct vki_termio) );
      break;
   case VKI_TCGETA:
      PRE_MEM_WRITE( "ioctl(TCGETA)", arg3, sizeof(struct vki_termio) );
      break;
   case VKI_TCSBRK:
   case VKI_TCXONC:
   case VKI_TCSBRKP:
   case VKI_TCFLSH:
      /* These just take an int by value */
      break;
   case VKI_TIOCGWINSZ:
      PRE_MEM_WRITE( "ioctl(TIOCGWINSZ)", arg3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
      PRE_MEM_READ( "ioctl(TIOCSWINSZ)",  arg3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCMBIS:
      PRE_MEM_READ( "ioctl(TIOCMBIS)",    arg3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMBIC:
      PRE_MEM_READ( "ioctl(TIOCMBIC)",    arg3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMSET:
      PRE_MEM_READ( "ioctl(TIOCMSET)",    arg3, sizeof(unsigned int) );
      break;
   case VKI_TIOCLINUX:
      PRE_MEM_READ( "ioctl(TIOCLINUX)",   arg3, sizeof(char *) );
      if (*(char *)arg3 == 11) {
	 PRE_MEM_READ( "ioctl(TIOCLINUX, 11)", arg3, 2 * sizeof(char *) );
      }
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", arg3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", arg3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      PRE_MEM_WRITE( "ioctl(TIOCGPTN)", arg3, sizeof(int) );
      break;
   case VKI_TIOCSCTTY:
      /* Just takes an int value.  */
      break;
   case VKI_TIOCSPTLCK: /* Lock/unlock Pty */
      PRE_MEM_READ( "ioctl(TIOCSPTLCK)", arg3, sizeof(int) );
      break;
   case VKI_FIONBIO:
      PRE_MEM_READ( "ioctl(FIONBIO)",    arg3, sizeof(int) );
      break;
   case VKI_FIOASYNC:
      PRE_MEM_READ( "ioctl(FIOASYNC)",   arg3, sizeof(int) );
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      PRE_MEM_WRITE( "ioctl(FIONREAD)",  arg3, sizeof(int) );
      break;

   case VKI_SG_SET_COMMAND_Q:
      PRE_MEM_READ( "ioctl(SG_SET_COMMAND_Q)", arg3, sizeof(int) );
      break;
   case VKI_SG_IO:
      PRE_MEM_WRITE( "ioctl(SG_IO)", arg3, sizeof(vki_sg_io_hdr_t) );
      break;
   case VKI_SG_GET_SCSI_ID:
      PRE_MEM_WRITE( "ioctl(SG_GET_SCSI_ID)", arg3, sizeof(vki_sg_scsi_id_t) );
      break;
   case VKI_SG_SET_RESERVED_SIZE:
      PRE_MEM_READ( "ioctl(SG_SET_RESERVED_SIZE)", arg3, sizeof(int) );
      break;
   case VKI_SG_SET_TIMEOUT:
      PRE_MEM_READ( "ioctl(SG_SET_TIMEOUT)", arg3, sizeof(int) );
      break;
   case VKI_SG_GET_RESERVED_SIZE:
      PRE_MEM_WRITE( "ioctl(SG_GET_RESERVED_SIZE)", arg3, sizeof(int) );
      break;
   case VKI_SG_GET_TIMEOUT:
      PRE_MEM_WRITE( "ioctl(SG_GET_TIMEOUT)", arg3, sizeof(int) );
      break;
   case VKI_SG_GET_VERSION_NUM:
      PRE_MEM_READ(  "ioctl(SG_GET_VERSION_NUM)",  arg3, sizeof(int) );
      break;
   case VKI_SG_EMULATED_HOST: /* 0x2203 */
      PRE_MEM_WRITE( "ioctl(SG_EMULATED_HOST)",    arg3, sizeof(int) );
      break;
   case VKI_SG_GET_SG_TABLESIZE: /* 0x227f */
      PRE_MEM_WRITE( "ioctl(SG_GET_SG_TABLESIZE)", arg3, sizeof(int) );
      break;

   case VKI_IIOCGETCPS:
      PRE_MEM_WRITE( "ioctl(IIOCGETCPS)", arg3,
		     VKI_ISDN_MAX_CHANNELS * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      PRE_MEM_READ( "ioctl(IIOCNETGPN)",
		     (Addr)&((vki_isdn_net_ioctl_phone *)arg3)->name,
		     sizeof(((vki_isdn_net_ioctl_phone *)arg3)->name) );
      PRE_MEM_WRITE( "ioctl(IIOCNETGPN)", arg3,
		     sizeof(vki_isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case VKI_SIOCGIFINDEX:        /* get iface index              */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFINDEX)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFINDEX)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFFLAGS)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFFLAGS)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFHWADDR:       /* Get hardware address         */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFHWADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFHWADDR)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMTU)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMTU)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFADDR)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFNETMASK)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNETMASK)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMETRIC)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMETRIC)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMAP:          /* Get device parameters        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMAP)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMAP)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFTXQLEN:       /* Get the tx queue length      */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFTXQLEN)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFTXQLEN)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFDSTADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFDSTADDR)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFBRDADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFBRDADDR)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNAME:         /* get iface name               */
      PRE_MEM_READ( "ioctl(SIOCGIFNAME)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_ifindex,
                     sizeof(((struct vki_ifreq *)arg3)->vki_ifr_ifindex) );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNAME)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGMIIPHY:         /* get hardware entry           */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMIIPHY)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMIIPHY)", arg3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGMIIREG:         /* get hardware entry registers */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->phy_id,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->phy_id) );
      PRE_MEM_READ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->reg_num,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->reg_num) );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMIIREG)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 PRE_MEM_WRITE( "ioctl(SIOCGIFCONF)", arg3, sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,res);
	 if (!VG_(is_kerror)(res) && res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct ifconf));
      */
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)", arg3, sizeof(struct vki_ifconf));
      if ( arg3 ) {
	 // TODO len must be readable and writable
	 // buf pointer only needs to be readable
	 struct vki_ifconf *ifc = (struct vki_ifconf *) arg3;
	 PRE_MEM_WRITE( "ioctl(SIOCGIFCONF).ifc_buf",
			(Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
   case VKI_SIOCGSTAMP:
      PRE_MEM_WRITE( "ioctl(SIOCGSTAMP)", arg3, sizeof(struct vki_timeval));
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case VKI_SIOCOUTQ:
      PRE_MEM_WRITE( "ioctl(SIOCOUTQ)", arg3, sizeof(int));
      break;
   case VKI_SIOCGRARP:           /* get RARP table entry         */
   case VKI_SIOCGARP:            /* get ARP table entry          */
      PRE_MEM_WRITE( "ioctl(SIOCGARP)", arg3, sizeof(struct vki_arpreq));
      break;
                    
   case VKI_SIOCSIFFLAGS:        /* set flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_flags,
                     sizeof(((struct vki_ifreq *)arg3)->vki_ifr_flags) );
      break;
   case VKI_SIOCSIFMAP:          /* Set device parameters        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMAP)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMAP)",
                     (Addr)&((struct vki_ifreq *)arg3)->ifr_map,
                     sizeof(((struct vki_ifreq *)arg3)->ifr_map) );
      break;
   case VKI_SIOCSIFTXQLEN:       /* Set the tx queue length      */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFTXQLEN)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFTXQLEN)",
                     (Addr)&((struct vki_ifreq *)arg3)->ifr_qlen,
                     sizeof(((struct vki_ifreq *)arg3)->ifr_qlen) );
      break;
   case VKI_SIOCSIFADDR:         /* set PA address               */
   case VKI_SIOCSIFDSTADDR:      /* set remote PA address        */
   case VKI_SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case VKI_SIOCSIFNETMASK:      /* set network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)&((struct vki_ifreq *)arg3)->ifr_addr,
                     sizeof(((struct vki_ifreq *)arg3)->ifr_addr) );
      break;
   case VKI_SIOCSIFMETRIC:       /* set metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_metric,
                     sizeof(((struct vki_ifreq *)arg3)->vki_ifr_metric) );
      break;
   case VKI_SIOCSIFMTU:          /* set MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMTU)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMTU)",
                     (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_mtu,
                     sizeof(((struct vki_ifreq *)arg3)->vki_ifr_mtu) );
      break;
   case VKI_SIOCSIFHWADDR:       /* set hardware address         */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFHWADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFHWADDR)",
                     (Addr)&((struct vki_ifreq *)arg3)->ifr_hwaddr,
                     sizeof(((struct vki_ifreq *)arg3)->ifr_hwaddr) );
      break;
   case VKI_SIOCSMIIREG:         /* set hardware entry registers */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSMIIREG)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->phy_id,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->phy_id) );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->reg_num,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->reg_num) );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->val_in,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->val_in) );
      break;
      /* Routing table calls.  */
   case VKI_SIOCADDRT:           /* add routing table entry      */
   case VKI_SIOCDELRT:           /* delete routing table entry   */
      PRE_MEM_READ( "ioctl(SIOCADDRT/DELRT)", arg3, 
		    sizeof(struct vki_rtentry));
      break;

      /* RARP cache control calls. */
   case VKI_SIOCDRARP:           /* delete RARP table entry      */
   case VKI_SIOCSRARP:           /* set RARP table entry         */
      /* ARP cache control calls. */
   case VKI_SIOCSARP:            /* set ARP table entry          */
   case VKI_SIOCDARP:            /* delete ARP table entry       */
      PRE_MEM_READ( "ioctl(SIOCSIFFLAGS)", arg3, sizeof(struct vki_ifreq));
      break;

   case VKI_SIOCGPGRP:
      PRE_MEM_WRITE( "ioctl(SIOCGPGRP)", arg3, sizeof(int) );
      break;
   case VKI_SIOCSPGRP:
      PRE_MEM_READ( "ioctl(SIOCSPGRP)", arg3, sizeof(int) );
      tst->sys_flags &= ~MayBlock;
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
		     arg3, sizeof(int));
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
		     arg3, sizeof(int));
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOWR, int))", 
		     arg3, sizeof(int));
      break;
   case VKI_SNDCTL_DSP_GETOSPACE:
   case VKI_SNDCTL_DSP_GETISPACE:
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOR, audio_buf_info))",
                     arg3, sizeof(vki_audio_buf_info));
      break;
   case VKI_SNDCTL_DSP_SETTRIGGER:
      PRE_MEM_READ( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOW, int))", 
		     arg3, sizeof(int));
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
		     arg3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_ALM_SET:
      PRE_MEM_READ( "ioctl(RTC_ALM_SET)", arg3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_IRQP_READ:
      PRE_MEM_WRITE( "ioctl(RTC_IRQP_READ)", arg3, sizeof(unsigned long));
      break;

   case VKI_BLKGETSIZE:
      PRE_MEM_WRITE( "ioctl(BLKGETSIZE)", arg3, sizeof(unsigned long));
      break;

      /* Hard disks */
   case VKI_HDIO_GET_IDENTITY: /* 0x030d */
      PRE_MEM_WRITE( "ioctl(HDIO_GET_IDENTITY)", arg3,
                     VKI_SIZEOF_STRUCT_HD_DRIVEID );
      break;

      /* CD ROM stuff (??)  */
   case VKI_CDROM_GET_MCN:
      PRE_MEM_READ( "ioctl(CDROM_GET_MCN)", arg3,
                    sizeof(struct vki_cdrom_mcn) );
      break;
   case VKI_CDROM_SEND_PACKET:
      PRE_MEM_READ( "ioctl(CDROM_SEND_PACKET)", arg3,
                    sizeof(struct vki_cdrom_generic_command));
      break;
   case VKI_CDROMSUBCHNL:
      PRE_MEM_READ( "ioctl(CDROMSUBCHNL (cdsc_format, char))",
		    (Addr) &(((struct vki_cdrom_subchnl*) arg3)->cdsc_format),
		    sizeof(((struct vki_cdrom_subchnl*) arg3)->cdsc_format));
      PRE_MEM_WRITE( "ioctl(CDROMSUBCHNL)", arg3, 
		     sizeof(struct vki_cdrom_subchnl));
      break;
   case VKI_CDROMREADMODE2:
      PRE_MEM_READ( "ioctl(CDROMREADMODE2)", arg3, VKI_CD_FRAMESIZE_RAW0 );
      break;
   case VKI_CDROMREADTOCHDR:
      PRE_MEM_WRITE( "ioctl(CDROMREADTOCHDR)", arg3, 
		     sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMREADTOCENTRY:
      PRE_MEM_READ( "ioctl(CDROMREADTOCENTRY (cdte_format, char))",
		    (Addr) &(((struct vki_cdrom_tocentry*) arg3)->cdte_format),
		    sizeof(((struct vki_cdrom_tocentry*) arg3)->cdte_format));
      PRE_MEM_READ( "ioctl(CDROMREADTOCENTRY (cdte_track, char))",
		    (Addr) &(((struct vki_cdrom_tocentry*) arg3)->cdte_track), 
		    sizeof(((struct vki_cdrom_tocentry*) arg3)->cdte_track));
      PRE_MEM_WRITE( "ioctl(CDROMREADTOCENTRY)", arg3, 
		     sizeof(struct vki_cdrom_tocentry));
      break;
   case VKI_CDROMMULTISESSION: /* 0x5310 */
      PRE_MEM_WRITE( "ioctl(CDROMMULTISESSION)", arg3,
		     sizeof(struct vki_cdrom_multisession));
      break;
   case VKI_CDROMVOLREAD: /* 0x5313 */
      PRE_MEM_WRITE( "ioctl(CDROMVOLREAD)", arg3,
		     sizeof(struct vki_cdrom_volctrl));
      break;
   case VKI_CDROMREADAUDIO: /* 0x530e */
      PRE_MEM_READ( "ioctl(CDROMREADAUDIO)", arg3,
		     sizeof (struct vki_cdrom_read_audio));
      if ( arg3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_cdrom_read_audio *cra = (struct vki_cdrom_read_audio *) arg3;
	 PRE_MEM_WRITE( "ioctl(CDROMREADAUDIO).buf",
	                (Addr)(cra->buf), cra->nframes * VKI_CD_FRAMESIZE_RAW);
      }
      break;      
   case VKI_CDROMPLAYMSF:
      PRE_MEM_READ( "ioctl(CDROMPLAYMSF)", arg3, sizeof(struct vki_cdrom_msf));
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case VKI_CDROM_DRIVE_STATUS: /* 0x5326 */
   case VKI_CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

   case VKI_FIGETBSZ:
      PRE_MEM_WRITE( "ioctl(FIGETBSZ)", arg3, sizeof(unsigned long));
      break;
   case VKI_FIBMAP:
      PRE_MEM_READ( "ioctl(FIBMAP)", arg3, sizeof(unsigned long));
      break;

   case VKI_FBIOGET_VSCREENINFO: /* 0x4600 */
      PRE_MEM_WRITE( "ioctl(FBIOGET_VSCREENINFO)", arg3,
                     sizeof(struct vki_fb_var_screeninfo));
      break;
   case VKI_FBIOGET_FSCREENINFO: /* 0x4602 */
      PRE_MEM_WRITE( "ioctl(FBIOGET_FSCREENINFO)", arg3,
                     sizeof(struct vki_fb_fix_screeninfo));
      break;

   case VKI_PPCLAIM:
   case VKI_PPEXCL:
   case VKI_PPYIELD:
   case VKI_PPRELEASE:
      break;
   case VKI_PPSETMODE:
      PRE_MEM_READ( "ioctl(PPSETMODE)",   arg3, sizeof(int) );
      break;
   case VKI_PPGETMODE:
      PRE_MEM_WRITE( "ioctl(PPGETMODE)",  arg3, sizeof(int) );
      break;
   case VKI_PPSETPHASE:
      PRE_MEM_READ(  "ioctl(PPSETPHASE)", arg3, sizeof(int) );
      break;
   case VKI_PPGETPHASE:
      PRE_MEM_WRITE( "ioctl(PPGETPHASE)", arg3, sizeof(int) );
      break;
   case VKI_PPGETMODES:
      PRE_MEM_WRITE( "ioctl(PPGETMODES)", arg3, sizeof(unsigned int) );
      break;
   case VKI_PPSETFLAGS:
      PRE_MEM_READ(  "ioctl(PPSETFLAGS)", arg3, sizeof(int) );
      break;
   case VKI_PPGETFLAGS:
      PRE_MEM_WRITE( "ioctl(PPGETFLAGS)", arg3, sizeof(int) );
      break;
   case VKI_PPRSTATUS:
      PRE_MEM_WRITE( "ioctl(PPRSTATUS)",  arg3, sizeof(unsigned char) );
      break;
   case VKI_PPRDATA:
      PRE_MEM_WRITE( "ioctl(PPRDATA)",    arg3, sizeof(unsigned char) );
      break;
   case VKI_PPRCONTROL:
      PRE_MEM_WRITE( "ioctl(PPRCONTROL)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPWDATA:
      PRE_MEM_READ(  "ioctl(PPWDATA)",    arg3, sizeof(unsigned char) );
      break;
   case VKI_PPWCONTROL:
      PRE_MEM_READ(  "ioctl(PPWCONTROL)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPFCONTROL:
      PRE_MEM_READ(  "ioctl(PPFCONTROL)", arg3, 2 * sizeof(unsigned char) );
      break;
   case VKI_PPDATADIR:
      PRE_MEM_READ(  "ioctl(PPDATADIR)",  arg3, sizeof(int) );
      break;
   case VKI_PPNEGOT:
      PRE_MEM_READ(  "ioctl(PPNEGOT)",    arg3, sizeof(int) );
      break;
   case VKI_PPWCTLONIRQ:
      PRE_MEM_READ(  "ioctl(PPWCTLONIRQ)",arg3, sizeof(unsigned char) );
      break;
   case VKI_PPCLRIRQ:
      PRE_MEM_WRITE( "ioctl(PPCLRIRQ)",   arg3, sizeof(int) );
      break;
   case VKI_PPSETTIME:
      PRE_MEM_READ(  "ioctl(PPSETTIME)",  arg3, sizeof(struct vki_timeval) );
      break;
   case VKI_PPGETTIME:
      PRE_MEM_WRITE( "ioctl(PPGETTIME)",  arg3, sizeof(struct vki_timeval) );
      break;

   case VKI_GIO_FONT:
      PRE_MEM_WRITE( "ioctl(GIO_FONT)", arg3, 32 * 256 );
      break;
   case VKI_PIO_FONT:
      PRE_MEM_READ( "ioctl(PIO_FONT)", arg3, 32 * 256 );
      break;

   case VKI_GIO_FONTX:
      PRE_MEM_READ( "ioctl(GIO_FONTX)", arg3, sizeof(struct vki_consolefontdesc) );
      if ( arg3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_consolefontdesc *cfd = (struct vki_consolefontdesc *)arg3;
         PRE_MEM_WRITE( "ioctl(GIO_FONTX).chardata", (Addr)cfd->chardata,
                        32 * cfd->charcount );
      }
      break;
   case VKI_PIO_FONTX:
      PRE_MEM_READ( "ioctl(PIO_FONTX)", arg3, sizeof(struct vki_consolefontdesc) );
      if ( arg3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_consolefontdesc *cfd = (struct vki_consolefontdesc *)arg3;
         PRE_MEM_READ( "ioctl(PIO_FONTX).chardata", (Addr)cfd->chardata,
                       32 * cfd->charcount );
      }
      break;

   case VKI_PIO_FONTRESET:
      break;

   case VKI_GIO_CMAP:
      PRE_MEM_WRITE( "ioctl(GIO_CMAP)", arg3, 16 * 3 );
      break;
   case VKI_PIO_CMAP:
      PRE_MEM_READ( "ioctl(PIO_CMAP)", arg3, 16 * 3 );
      break;

   case VKI_KIOCSOUND:
   case VKI_KDMKTONE:
      break;

   case VKI_KDGETLED:
      PRE_MEM_WRITE( "ioctl(KDGETLED)", arg3, sizeof(char) );
      break;
   case VKI_KDSETLED:
      break;

   case VKI_KDGKBTYPE:
      PRE_MEM_WRITE( "ioctl(KDGKBTYPE)", arg3, sizeof(char) );
      break;

   case VKI_KDADDIO:
   case VKI_KDDELIO:
   case VKI_KDENABIO:
   case VKI_KDDISABIO:
      break;

   case VKI_KDSETMODE:
      break;
   case VKI_KDGETMODE:
      PRE_MEM_WRITE( "ioctl(KDGETMODE)", arg3, sizeof(int) );
      break;

   case VKI_KDMAPDISP:
   case VKI_KDUNMAPDISP:
      break;

   case VKI_GIO_SCRNMAP:
      PRE_MEM_WRITE( "ioctl(GIO_SCRNMAP)", arg3, VKI_E_TABSZ );
      break;
   case VKI_PIO_SCRNMAP:
      PRE_MEM_READ( "ioctl(PIO_SCRNMAP)", arg3, VKI_E_TABSZ  );
      break;
   case VKI_GIO_UNISCRNMAP:
      PRE_MEM_WRITE( "ioctl(GIO_UNISCRNMAP)", arg3,
                     VKI_E_TABSZ * sizeof(unsigned short) );
      break;
   case VKI_PIO_UNISCRNMAP:
      PRE_MEM_READ( "ioctl(PIO_UNISCRNMAP)", arg3,
                    VKI_E_TABSZ * sizeof(unsigned short) );
      break;

   case VKI_KDGKBMODE:
      PRE_MEM_WRITE( "ioctl(KDGKBMODE)", arg3, sizeof(int) );
      break;
   case VKI_KDSKBMODE:
      break;
      
   case VKI_KDGKBMETA:
      PRE_MEM_WRITE( "ioctl(KDGKBMETA)", arg3, sizeof(int) );
      break;
   case VKI_KDSKBMETA:
      break;
      
   case VKI_KDGKBLED:
      PRE_MEM_WRITE( "ioctl(KDGKBLED)", arg3, sizeof(char) );
      break;
   case VKI_KDSKBLED:
      break;
      
   case VKI_KDGKBENT:
      PRE_MEM_READ( "ioctl(KDGKBENT).kb_table",
                    (Addr)&((struct vki_kbentry *)arg3)->kb_table,
                    sizeof(((struct vki_kbentry *)arg3)->kb_table) );
      PRE_MEM_READ( "ioctl(KDGKBENT).kb_index",
                    (Addr)&((struct vki_kbentry *)arg3)->kb_index,
                    sizeof(((struct vki_kbentry *)arg3)->kb_index) );
      PRE_MEM_WRITE( "ioctl(KDGKBENT).kb_value",
		     (Addr)&((struct vki_kbentry *)arg3)->kb_value,
		     sizeof(((struct vki_kbentry *)arg3)->kb_value) );
      break;
   case VKI_KDSKBENT:
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_table",
                    (Addr)&((struct vki_kbentry *)arg3)->kb_table,
                    sizeof(((struct vki_kbentry *)arg3)->kb_table) );
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_index",
                    (Addr)&((struct vki_kbentry *)arg3)->kb_index,
                    sizeof(((struct vki_kbentry *)arg3)->kb_index) );
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_value",
                    (Addr)&((struct vki_kbentry *)arg3)->kb_value,
                    sizeof(((struct vki_kbentry *)arg3)->kb_value) );
      break;
      
   case VKI_KDGKBSENT:
      PRE_MEM_READ( "ioctl(KDGKBSENT).kb_func",
                    (Addr)&((struct vki_kbsentry *)arg3)->kb_func,
                    sizeof(((struct vki_kbsentry *)arg3)->kb_func) );
      PRE_MEM_WRITE( "ioctl(KDGKSENT).kb_string",
		     (Addr)((struct vki_kbsentry *)arg3)->kb_string,
		     sizeof(((struct vki_kbsentry *)arg3)->kb_string) );
      break;
   case VKI_KDSKBSENT:
      PRE_MEM_READ( "ioctl(KDSKBSENT).kb_func",
                    (Addr)&((struct vki_kbsentry *)arg3)->kb_func,
                    sizeof(((struct vki_kbsentry *)arg3)->kb_func) );
      PRE_MEM_RASCIIZ( "ioctl(KDSKBSENT).kb_string",
                       (Addr)((struct vki_kbsentry *)arg3)->kb_string );
      break;
      
   case VKI_KDGKBDIACR:
      PRE_MEM_WRITE( "ioctl(KDGKBDIACR)", arg3, sizeof(struct vki_kbdiacrs) );
      break;
   case VKI_KDSKBDIACR:
      PRE_MEM_READ( "ioctl(KDSKBDIACR)", arg3, sizeof(struct vki_kbdiacrs) );
      break;
      
   case VKI_KDGETKEYCODE:
      PRE_MEM_READ( "ioctl(KDGETKEYCODE).scancode",
                    (Addr)&((struct vki_kbkeycode *)arg3)->scancode,
                    sizeof(((struct vki_kbkeycode *)arg3)->scancode) );
      PRE_MEM_WRITE( "ioctl(KDGETKEYCODE).keycode",
		     (Addr)((struct vki_kbkeycode *)arg3)->keycode,
		     sizeof(((struct vki_kbkeycode *)arg3)->keycode) );
      break;
   case VKI_KDSETKEYCODE:
      PRE_MEM_READ( "ioctl(KDSETKEYCODE).scancode",
                    (Addr)&((struct vki_kbkeycode *)arg3)->scancode,
                    sizeof(((struct vki_kbkeycode *)arg3)->scancode) );
      PRE_MEM_READ( "ioctl(KDSETKEYCODE).keycode",
                    (Addr)((struct vki_kbkeycode *)arg3)->keycode,
                    sizeof(((struct vki_kbkeycode *)arg3)->keycode) );
      break;
      
   case VKI_KDSIGACCEPT:
      break;

   case VKI_KDKBDREP:
      PRE_MEM_READ( "ioctl(KBKBDREP)", arg3, sizeof(struct vki_kbd_repeat) );
      break;
      
      /* We don't have any specific information on it, so
	 try to do something reasonable based on direction and
	 size bits.  The encoding scheme is described in
	 /usr/include/asm/ioctl.h.  

	 According to Simon Hausmann, _IOC_READ means the kernel
	 writes a value to the ioctl value passed from the user
	 space and the other way around with _IOC_WRITE. */
   default: {
      UInt dir  = _VKI_IOC_DIR(arg2);
      UInt size = _VKI_IOC_SIZE(arg2);
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
	 if (moans > 0) {
	    moans--;
	    VG_(message)(Vg_UserMsg, 
			 "Warning: noted but unhandled ioctl 0x%x"
			 " with no size/direction hints",
			 arg2); 
	    VG_(message)(Vg_UserMsg, 
			 "   This could cause spurious value errors"
			 " to appear.");
	    VG_(message)(Vg_UserMsg, 
			 "   See README_MISSING_SYSCALL_OR_IOCTL for "
			 "guidance on writing a proper wrapper." );
	 }
      } else {
	 if ((dir & _VKI_IOC_WRITE) && size > 0)
	    PRE_MEM_READ( "ioctl(generic)", arg3, size);
	 if ((dir & _VKI_IOC_READ) && size > 0)
	    PRE_MEM_WRITE( "ioctl(generic)", arg3, size);
      }
      break;
   }
   }   
}

POST(sys_ioctl)
{
   switch (arg2 /* request */) {
   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
      break; 
   case VKI_TCGETS:
      POST_MEM_WRITE( arg3, sizeof(struct vki_termios) );
      break;
   case VKI_TCSETA:
   case VKI_TCSETAW:
   case VKI_TCSETAF:
      break;
   case VKI_TCGETA:
      POST_MEM_WRITE( arg3, sizeof(struct vki_termio) );
      break;
   case VKI_TCSBRK:
   case VKI_TCXONC:
   case VKI_TCSBRKP:
   case VKI_TCFLSH:
      break;
   case VKI_TIOCGWINSZ:
      POST_MEM_WRITE( arg3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
   case VKI_TIOCMBIS:
   case VKI_TIOCMBIC:
   case VKI_TIOCMSET:
      break;
   case VKI_TIOCLINUX:
      POST_MEM_WRITE( arg3, sizeof(char *) );
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      POST_MEM_WRITE( arg3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      POST_MEM_WRITE( arg3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      POST_MEM_WRITE( arg3, sizeof(int));
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
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;

   case VKI_SG_SET_COMMAND_Q:
      break;
   case VKI_SG_IO:
      VG_TRACK( post_mem_write,arg3, sizeof(vki_sg_io_hdr_t));
      break;
   case VKI_SG_GET_SCSI_ID:
      VG_TRACK( post_mem_write,arg3, sizeof(vki_sg_scsi_id_t));
      break;
   case VKI_SG_SET_RESERVED_SIZE:
      break;
   case VKI_SG_SET_TIMEOUT:
      break;
   case VKI_SG_GET_RESERVED_SIZE:
      VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case VKI_SG_GET_TIMEOUT:
      VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case VKI_SG_GET_VERSION_NUM:
      break;
   case VKI_SG_EMULATED_HOST:
      VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case VKI_SG_GET_SG_TABLESIZE:
      VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;      

   case VKI_IIOCGETCPS:
      POST_MEM_WRITE( arg3, VKI_ISDN_MAX_CHANNELS * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      POST_MEM_WRITE( arg3, sizeof(vki_isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case VKI_SIOCGIFINDEX:        /* get iface index              */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_ifindex,
                sizeof(((struct vki_ifreq *)arg3)->vki_ifr_ifindex) );
      break;
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_flags,
                sizeof(((struct vki_ifreq *)arg3)->vki_ifr_flags) );
      break;
   case VKI_SIOCGIFHWADDR:       /* Get hardware address         */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->ifr_hwaddr,
                sizeof(((struct vki_ifreq *)arg3)->ifr_hwaddr) );
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_mtu,
                sizeof(((struct vki_ifreq *)arg3)->vki_ifr_mtu) );
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->ifr_addr,
                sizeof(((struct vki_ifreq *)arg3)->ifr_addr) );
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_metric,
                sizeof(((struct vki_ifreq *)arg3)->vki_ifr_metric) );
      break;
   case VKI_SIOCGIFMAP:          /* Get device parameters        */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->ifr_map,
                sizeof(((struct vki_ifreq *)arg3)->ifr_map) );
      break;
     break;
   case VKI_SIOCGIFTXQLEN:       /* Get the tx queue length      */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->ifr_qlen,
                sizeof(((struct vki_ifreq *)arg3)->ifr_qlen) );
      break;
   case VKI_SIOCGIFNAME:         /* get iface name               */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_ifreq *)arg3)->vki_ifr_name,
                sizeof(((struct vki_ifreq *)arg3)->vki_ifr_name) );
      break;
   case VKI_SIOCGMIIPHY:         /* get hardware entry           */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->phy_id,
                sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->phy_id) );
      break;
   case VKI_SIOCGMIIREG:         /* get hardware entry registers */
      VG_TRACK( post_mem_write,
                (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->val_out,
                sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)arg3)->vki_ifr_data)->val_out) );
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 PRE_MEM_WRITE("ioctl(SIOCGIFCONF)", arg3, sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,res);
	 if (!VG_(is_kerror)(res) && res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct ifconf));
      */
      if (res == 0 && arg3 ) {
	 struct vki_ifconf *ifc = (struct vki_ifconf *) arg3;
	 if (ifc->vki_ifc_buf != NULL)
	    POST_MEM_WRITE( (Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
   case VKI_SIOCGSTAMP:
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_timeval));
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case VKI_SIOCOUTQ:
      VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case VKI_SIOCGRARP:           /* get RARP table entry         */
   case VKI_SIOCGARP:            /* get ARP table entry          */
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_arpreq));
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
      VG_TRACK( post_mem_write,arg3, sizeof(int));
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
      VG_TRACK( post_mem_write,arg3, sizeof(int));
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
      VG_TRACK( post_mem_write,arg3, sizeof(vki_audio_buf_info));
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
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_ALM_SET:
      break;
   case VKI_RTC_IRQP_READ:
      VG_TRACK( post_mem_write,arg3, sizeof(unsigned long));
      break;

   case VKI_BLKGETSIZE:
      VG_TRACK( post_mem_write,arg3, sizeof(unsigned long));
      break;

      /* Hard disks */
   case VKI_HDIO_GET_IDENTITY: /* 0x030d */
      VG_TRACK( post_mem_write,arg3, VKI_SIZEOF_STRUCT_HD_DRIVEID );
      break;

      /* CD ROM stuff (??)  */
   case VKI_CDROMSUBCHNL:
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_cdrom_subchnl));
      break;
   case VKI_CDROMREADTOCHDR:
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMREADTOCENTRY:
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMMULTISESSION:
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_cdrom_multisession));
      break;
   case VKI_CDROMVOLREAD:
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_cdrom_volctrl));
      break;
   case VKI_CDROMREADAUDIO:
   {
      struct vki_cdrom_read_audio *cra = (struct vki_cdrom_read_audio *) arg3;
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
      VG_TRACK( post_mem_write,arg3, sizeof(unsigned long));
      break;
   case VKI_FIBMAP:
      VG_TRACK( post_mem_write,arg3, sizeof(unsigned long));
      break;

   case VKI_FBIOGET_VSCREENINFO: //0x4600
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_fb_var_screeninfo));
      break;
   case VKI_FBIOGET_FSCREENINFO: //0x4602
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_fb_fix_screeninfo));
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
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;
   case VKI_PPGETPHASE:
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;
   case VKI_PPGETMODES:
      POST_MEM_WRITE( arg3, sizeof(unsigned int) );
      break;
   case VKI_PPGETFLAGS:
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;
   case VKI_PPRSTATUS:
      POST_MEM_WRITE( arg3, sizeof(unsigned char) );
      break;
   case VKI_PPRDATA:
      POST_MEM_WRITE( arg3, sizeof(unsigned char) );
      break;
   case VKI_PPRCONTROL:
      POST_MEM_WRITE( arg3, sizeof(unsigned char) );
      break;
   case VKI_PPCLRIRQ:
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;
   case VKI_PPGETTIME:
      POST_MEM_WRITE( arg3, sizeof(struct vki_timeval) );
      break;

   case VKI_GIO_FONT:
      POST_MEM_WRITE( arg3, 32 * 256 );
      break;
   case VKI_PIO_FONT:
      break;

   case VKI_GIO_FONTX:
      POST_MEM_WRITE( (Addr)((struct vki_consolefontdesc *)arg3)->chardata,
                      32 * ((struct vki_consolefontdesc *)arg3)->charcount );
      break;
   case VKI_PIO_FONTX:
      break;

   case VKI_PIO_FONTRESET:
      break;

   case VKI_GIO_CMAP:
      POST_MEM_WRITE( arg3, 16 * 3 );
      break;
   case VKI_PIO_CMAP:
      break;

   case VKI_KIOCSOUND:
   case VKI_KDMKTONE:
      break;

   case VKI_KDGETLED:
      POST_MEM_WRITE( arg3, sizeof(char) );
      break;
   case VKI_KDSETLED:
      break;

   case VKI_KDGKBTYPE:
      POST_MEM_WRITE( arg3, sizeof(char) );
      break;

   case VKI_KDADDIO:
   case VKI_KDDELIO:
   case VKI_KDENABIO:
   case VKI_KDDISABIO:
      break;

   case VKI_KDSETMODE:
      break;
   case VKI_KDGETMODE:
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;

   case VKI_KDMAPDISP:
   case VKI_KDUNMAPDISP:
      break;

   case VKI_GIO_SCRNMAP:
      POST_MEM_WRITE( arg3, VKI_E_TABSZ );
      break;
   case VKI_PIO_SCRNMAP:
      break;
   case VKI_GIO_UNISCRNMAP:
      POST_MEM_WRITE( arg3, VKI_E_TABSZ * sizeof(unsigned short) );
      break;
   case VKI_PIO_UNISCRNMAP:
      break;

   case VKI_KDGKBMODE:
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;
   case VKI_KDSKBMODE:
      break;
      
   case VKI_KDGKBMETA:
      POST_MEM_WRITE( arg3, sizeof(int) );
      break;
   case VKI_KDSKBMETA:
      break;
      
   case VKI_KDGKBLED:
      POST_MEM_WRITE( arg3, sizeof(char) );
      break;
   case VKI_KDSKBLED:
      break;
      
   case VKI_KDGKBENT:
      POST_MEM_WRITE( (Addr)&((struct vki_kbentry *)arg3)->kb_value,
                      sizeof(((struct vki_kbentry *)arg3)->kb_value) );
      break;
   case VKI_KDSKBENT:
      break;
      
   case VKI_KDGKBSENT:
      POST_MEM_WRITE( (Addr)((struct vki_kbsentry *)arg3)->kb_string,
                      sizeof(((struct vki_kbsentry *)arg3)->kb_string) );
      break;
   case VKI_KDSKBSENT:
      break;
      
   case VKI_KDGKBDIACR:
      POST_MEM_WRITE( arg3, sizeof(struct vki_kbdiacrs) );
      break;
   case VKI_KDSKBDIACR:
      break;
      
   case VKI_KDGETKEYCODE:
      POST_MEM_WRITE( (Addr)((struct vki_kbkeycode *)arg3)->keycode,
                      sizeof(((struct vki_kbkeycode *)arg3)->keycode) );
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
      UInt dir  = _VKI_IOC_DIR(arg2);
      UInt size = _VKI_IOC_SIZE(arg2);
      if (size > 0 && (dir & _VKI_IOC_READ)
	  && res == 0
	  && arg3 != (Addr)NULL)
	 VG_TRACK( post_mem_write,arg3, size);
      break;
   }
   }
}

PRE(sys_kill, 0)
{
   /* int kill(pid_t pid, int sig); */
   PRINT("sys_kill ( %d, %d )", arg1,arg2);
   PRE_REG_READ2(long, "kill", int, pid, int, sig);
   if (arg2 == VKI_SIGVGINT || arg2 == VKI_SIGVGKILL)
      set_result( -VKI_EINVAL );
}

POST(sys_kill)
{
   /* If this was a self-kill then wait for a signal to be
      delivered to any thread before claiming the kill is done. */
   if (res >= 0 &&                           // if it was successful and
       arg2 != 0 &&                          // if a real signal and
       !VG_(is_sig_ign)(arg2) &&             // that isn't ignored and
       !VG_(sigismember)(&tst->eff_sig_mask, arg2) && // we're not blocking it
       (arg1 == VG_(getpid)() ||             // directed at us or
	arg1 == -1	      ||             // directed at everyone or
	arg1 == 0	      ||             // directed at whole group or
	-arg1 == VG_(getpgrp)())) {          // directed at our group...
      /* ...then wait for that signal to be delivered to someone
	 (might be us, might be someone else who doesn't have it blocked) */
      VG_(proxy_waitsig)();
   }
}

PRE(sys_link, MayBlock)
{
   PRINT("sys_link ( %p, %p)", arg1, arg2);
   PRE_REG_READ2(long, "link", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "link(oldpath)", arg1);
   PRE_MEM_RASCIIZ( "link(newpath)", arg2);
}

PRE(sys_lseek, 0)
{
   PRINT("sys_lseek ( %d, %d, %d )", arg1,arg2,arg3);
   PRE_REG_READ3(vki_off_t, "lseek",
                 unsigned int, fd, vki_off_t, offset, unsigned int, whence);
}

PRE(sys_newlstat, 0)
{
   PRINT("sys_newlstat ( %p(%s), %p )", arg1,arg1,arg2);
   PRE_REG_READ2(long, "lstat", char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "lstat(file_name)", arg1 );
   PRE_MEM_WRITE( "lstat(buf)", arg2, sizeof(struct vki_stat) );
}

POST(sys_newlstat)
{
   if (res == 0) {
      POST_MEM_WRITE( arg2, sizeof(struct vki_stat) );
   }
}

PRE(sys_lstat64, 0)
{
   PRINT("sys_lstat64 ( %p(%s), %p )",arg1,arg1,arg2);
   PRE_REG_READ2(long, "lstat64", char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "lstat64(file_name)", arg1 );
   PRE_MEM_WRITE( "lstat64(buf)", arg2, sizeof(struct vki_stat64) );
}

POST(sys_lstat64)
{
   if (res == 0) {
      POST_MEM_WRITE( arg2, sizeof(struct vki_stat64) );
   }
}

PRE(sys_mkdir, MayBlock)
{
   PRINT("sys_mkdir ( %p, %d )", arg1,arg2);
   PRE_REG_READ2(long, "mkdir", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "mkdir(pathname)", arg1 );
}

PRE(old_mmap, Special)
{
   /* struct mmap_arg_struct {           
         unsigned long addr;
         unsigned long len;
         unsigned long prot;
         unsigned long flags;
         unsigned long fd;
         unsigned long offset;
   }; */
   UInt a1, a2, a3, a4, a5, a6;

   PRE_REG_READ1(long, "old_mmap", struct mmap_arg_struct *, args);
   PLATFORM_GET_MMAP_ARGS(tst, a1, a2, a3, a4, a5, a6);

   PRINT("old_mmap ( %p, %llu, %d, %d, %d, %d )",
         a1, (ULong)a2, a3, a4, a5, a6 );

   if (a4 & VKI_MAP_FIXED) {
      if (!VG_(valid_client_addr)(a1, a2, tid, "old_mmap")) {
         PRINT("old_mmap failing: %p-%p\n", a1, a1+a2);
         set_result( -VKI_ENOMEM );
      }
   } else {
      a1 = VG_(find_map_space)(a1, a2, True);
      if (a1 == 0)
         set_result( -VKI_ENOMEM );
      else
         a4 |= VKI_MAP_FIXED;
   }

   if (res != -VKI_ENOMEM) {
      PLATFORM_DO_MMAP(res, a1, a2, a3, a4, a5, a6);

      if (!VG_(is_kerror)(res)) {
         vg_assert(VG_(valid_client_addr)(res, a2, tid, "old_mmap"));
         mmap_segment( (Addr)res, a2, a3, a4, a5, a6 );
      }
   }
}

PRE(sys_mmap2, 0)
{
   // Exactly like old_mmap() except:
   //  - all 6 args are passed in regs, rather than in a memory-block.
   //  - the file offset is specified in pagesize units rather than bytes,
   //    so that it can be used for files bigger than 2^32 bytes.
   PRINT("sys_mmap2 ( %p, %llu, %d, %d, %d, %d )",
         arg1, (ULong)arg2, arg3, arg4, arg5, arg6 );
   PRE_REG_READ6(long, "mmap2",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   if (arg4 & VKI_MAP_FIXED) {
      if (!VG_(valid_client_addr)(arg1, arg2, tid, "mmap2"))
	 set_result( -VKI_ENOMEM );
   } else {
      arg1 = VG_(find_map_space)(arg1, arg2, True);
      if (arg1 == 0)
	 set_result( -VKI_ENOMEM );
      else 
         arg4 |= VKI_MAP_FIXED;
   }
}

POST(sys_mmap2)
{
   vg_assert(VG_(valid_client_addr)(res, arg2, tid, "mmap2"));
   mmap_segment( (Addr)res, arg2, arg3, arg4, arg5,
                 arg6 * (ULong)VKI_PAGE_SIZE );
}

PRE(sys_mprotect, 0)
{
   PRINT("sys_mprotect ( %p, %llu, %d )", arg1,(ULong)arg2,arg3);
   PRE_REG_READ3(long, "mprotect",
                 unsigned long, addr, vki_size_t, len, unsigned long, prot);

   if (!VG_(valid_client_addr)(arg1, arg2, tid, "mprotect"))
      set_result( -VKI_ENOMEM );
}

POST(sys_mprotect)
{
   Addr a    = arg1;
   SizeT len = arg2;
   Int  prot = arg3;
   Bool rr = prot & VKI_PROT_READ;
   Bool ww = prot & VKI_PROT_WRITE;
   Bool xx = prot & VKI_PROT_EXEC;

   mash_addr_and_len(&a, &len);
   VG_(mprotect_range)(a, len, prot);
   VG_TRACK( change_mem_mprotect, a, len, rr, ww, xx );
}

PRE(sys_munmap, 0)
{
   PRINT("sys_munmap ( %p, %llu )", arg1,(ULong)arg2);
   PRE_REG_READ2(long, "munmap", unsigned long, start, vki_size_t, length);

   if (!VG_(valid_client_addr)(arg1, arg2, tid, "munmap"))
      set_result( -VKI_EINVAL );
}

POST(sys_munmap)
{
   Addr  a   = arg1;
   SizeT len = arg2;

   mash_addr_and_len(&a, &len);
   VG_(unmap_range)(a, len);
   VG_TRACK( die_mem_munmap, a, len );
}

PRE(sys_mincore, 0)
{
   PRINT("sys_mincore ( %p, %llu, %p )", arg1,(ULong)arg2,arg3);
   PRE_REG_READ3(long, "mincore",
                 unsigned long, start, vki_size_t, length,
                 unsigned char *, vec);
   PRE_MEM_WRITE( "mincore(vec)", arg3, (arg2 + 4096 - 1) / 4096);
}

POST(sys_mincore)
{
   POST_MEM_WRITE( arg3, (arg2 + 4096 - 1) / 4096 );  
}

PRE(sys_nanosleep, MayBlock|PostOnFail)
{
   PRINT("sys_nanosleep ( %p, %p )", arg1,arg2);
   PRE_REG_READ2(long, "nanosleep", 
                 struct timespec *, req, struct timespec *, rem);
   PRE_MEM_READ( "nanosleep(req)", arg1, sizeof(struct vki_timespec) );
   if (arg2 != 0)
      PRE_MEM_WRITE( "nanosleep(rem)", arg2, sizeof(struct vki_timespec) );
}

POST(sys_nanosleep)
{
   if (arg2 != 0 && res == -VKI_EINTR)
      POST_MEM_WRITE( arg2, sizeof(struct vki_timespec) );
}

PRE(sys_open, MayBlock)
{
   if (arg2 & VKI_O_CREAT) {
      // 3-arg version
      PRINT("sys_open ( %p(%s), %d, %d )",arg1,arg1,arg2,arg3);
      PRE_REG_READ3(long, "open",
                    const char *, filename, int, flags, int, mode);
   } else {
      // 2-arg version
      PRINT("sys_open ( %p(%s), %d )",arg1,arg1,arg2);
      PRE_REG_READ2(long, "open",
                    const char *, filename, int, flags);
   }
   PRE_MEM_RASCIIZ( "open(filename)", arg1 );
}

POST(sys_open)
{
   if (!VG_(fd_allowed)(res, "open", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
}

PRE(sys_read, MayBlock)
{
   PRINT("sys_read ( %d, %p, %llu )", arg1, arg2, (ULong)arg3);
   PRE_REG_READ3(ssize_t, "read",
                 unsigned int, fd, char *, buf, size_t, count);

   if (!VG_(fd_allowed)(arg1, "read", tid, False))
      set_result( -VKI_EBADF );
   else
      PRE_MEM_WRITE( "read(buf)", arg2, arg3 );
}

POST(sys_read)
{
   POST_MEM_WRITE( arg2, res );
}

PRE(sys_write, MayBlock)
{
   PRINT("sys_write ( %d, %p, %llu )", arg1, arg2, (ULong)arg3);
   PRE_REG_READ3(ssize_t, "write",
                 unsigned int, fd, const char *, buf, size_t, count);
   if (!VG_(fd_allowed)(arg1, "write", tid, False))
      set_result( -VKI_EBADF );
   else
      PRE_MEM_READ( "write(buf)", arg2, arg3 );
}

PRE(sys_creat, MayBlock)
{
   PRINT("sys_creat ( %p(%s), %d )", arg1,arg1,arg2);
   PRE_REG_READ2(long, "creat", const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "creat(pathname)", arg1 );
}

POST(sys_creat)
{
   if (!VG_(fd_allowed)(res, "creat", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
}

// XXX: sort of x86-specific
PRE(sys_pipe, 0)
{
   PRINT("sys_pipe ( %p )", arg1);
   PRE_REG_READ1(int, "pipe", unsigned long *, filedes);
   PRE_MEM_WRITE( "pipe(filedes)", arg1, 2*sizeof(long) );
}

POST(sys_pipe)
{
   // XXX: use of Int here -- 32-bit-specific?
   Int *p = (Int *)arg1;

   if (!VG_(fd_allowed)(p[0], "pipe", tid, True) ||
       !VG_(fd_allowed)(p[1], "pipe", tid, True)) {
      VG_(close)(p[0]);
      VG_(close)(p[1]);
      set_result( -VKI_EMFILE );
   } else {
      POST_MEM_WRITE( arg1, 2*sizeof(int) );
      if (VG_(clo_track_fds)) {
         VG_(record_fd_open)(tid, p[0], NULL);
         VG_(record_fd_open)(tid, p[1], NULL);
      }
   }
}

// XXX: x86-specific, due to pollfd struct
PRE(sys_poll, MayBlock)
{
   /* struct pollfd {
        int fd;           -- file descriptor
        short events;     -- requested events
        short revents;    -- returned events
      };
      int poll(struct pollfd *ufds, unsigned int nfds, int timeout) 
   */
   UInt i;
   struct vki_pollfd* ufds = (struct vki_pollfd *)arg1;
   PRINT("sys_poll ( %p, %d, %d )\n", arg1,arg2,arg3);
   PRE_REG_READ3(long, "poll",
                 struct pollfd *, ufds, unsigned int, nfds, long, timeout);
                     
   for (i = 0; i < arg2; i++) {
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
   if (res > 0) {
      UInt i;
      struct vki_pollfd* ufds = (struct vki_pollfd *)arg1;
      // XXX: again, this is x86-specific
      for (i = 0; i < arg2; i++)
	 POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(Short) );
   }
}

PRE(sys_readlink, Special)
{
   int saved = SYSNO;
   PRINT("sys_readlink ( %p, %p, %llu )", arg1,arg2,(ULong)arg3);
   PRE_REG_READ3(long, "readlink",
                 const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlink(path)", arg1 );
   PRE_MEM_WRITE( "readlink(buf)", arg2,arg3 );

   /*
    * Handle the case where readlink is looking at /proc/self/exe or
    * /proc/<pid>/exe.
    */

   set_result( VG_(do_syscall)(saved, arg1, arg2, arg3));
   if ((Int)res == -2) {
      char name[25];

      VG_(sprintf)(name, "/proc/%d/exe", VG_(getpid)());
   
      if (VG_(strcmp)((Char *)arg1, name) == 0 ||
          VG_(strcmp)((Char *)arg1, "/proc/self/exe") == 0) {
         VG_(sprintf)(name, "/proc/self/fd/%d", VG_(clexecfd));
         set_result( VG_(do_syscall)(saved, name, arg2, arg3));
      }
   }

   if ((Int)res > 0)
      POST_MEM_WRITE( arg2, res );
}

PRE(sys_readv, MayBlock)
{
   Int i;
   struct vki_iovec * vec;
   PRINT("sys_readv ( %d, %p, %llu )",arg1,arg2,(ULong)arg3);
   PRE_REG_READ3(ssize_t, "readv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count);
   if (!VG_(fd_allowed)(arg1, "readv", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      PRE_MEM_READ( "readv(vector)", arg2, arg3 * sizeof(struct vki_iovec) );

      if (arg2 != 0) {
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct vki_iovec *)arg2;
         for (i = 0; i < (Int)arg3; i++)
            PRE_MEM_WRITE( "readv(vector[...])",
                           (Addr)vec[i].iov_base, vec[i].iov_len );
      }
   }
}

POST(sys_readv)
{
   if (res > 0) {
      Int i;
      struct vki_iovec * vec = (struct vki_iovec *)arg2;
      Int remains = res;

      /* res holds the number of bytes read. */
      for (i = 0; i < (Int)arg3; i++) {
	 Int nReadThisBuf = vec[i].iov_len;
	 if (nReadThisBuf > remains) nReadThisBuf = remains;
	 POST_MEM_WRITE( (Addr)vec[i].iov_base, nReadThisBuf );
	 remains -= nReadThisBuf;
	 if (remains < 0) VG_(core_panic)("readv: remains < 0");
      }
   }
}

PRE(sys_rename, 0)
{
   PRINT("sys_rename ( %p, %p )", arg1, arg2 );
   PRE_REG_READ2(long, "rename", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "rename(oldpath)", arg1 );
   PRE_MEM_RASCIIZ( "rename(newpath)", arg2 );
}

PRE(sys_rmdir, MayBlock)
{
   PRINT("sys_rmdir ( %p )", arg1);
   PRE_REG_READ1(long, "rmdir", const char *, pathname);
   PRE_MEM_RASCIIZ( "rmdir(pathname)", arg1 );
}

PRE(sys_sched_setparam, 0)
{
   PRINT("sched_setparam ( %d, %p )", arg1, arg2 );
   PRE_REG_READ2(long, "sched_setparam", 
                 vki_pid_t, pid, struct sched_param *, p);
   PRE_MEM_READ( "sched_setparam(p)", arg2, sizeof(struct vki_sched_param) );
}

POST(sys_sched_setparam)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_sched_param) );
}

PRE(sys_sched_getparam, 0)
{
   PRINT("sched_getparam ( %d, %p )", arg1, arg2 );
   PRE_REG_READ2(long, "sched_getparam", 
                 vki_pid_t, pid, struct sched_param *, p);
   PRE_MEM_WRITE( "sched_getparam(p)", arg2, sizeof(struct vki_sched_param) );
}

POST(sys_sched_getparam)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_sched_param) );
}

PRE(sys_select, MayBlock)
{
   PRINT("sys_select ( %d, %p, %p, %p, %p )", arg1,arg2,arg3,arg4,arg5);
   PRE_REG_READ5(long, "select",
                 int, n, vki_fd_set *, readfds, vki_fd_set *, writefds,
                 vki_fd_set *, exceptfds, struct timeval *, timeout);
   // XXX: this possibly understates how much memory is read.
   if (arg2 != 0)
      PRE_MEM_READ( "select(readfds)",   
		     arg2, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg3 != 0)
      PRE_MEM_READ( "select(writefds)",  
		     arg3, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg4 != 0)
      PRE_MEM_READ( "select(exceptfds)", 
		     arg4, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg5 != 0)
      PRE_MEM_READ( "select(timeout)", arg5, sizeof(struct vki_timeval) );
}

PRE(sys_setgid16, 0)
{
   PRINT("sys_setgid16 ( %d )", arg1);
   PRE_REG_READ1(long, "setgid16", vki_old_gid_t, gid);
}

PRE(sys_setgid, 0)
{
   PRINT("sys_setgid ( %d )", arg1);
   PRE_REG_READ1(long, "setgid", vki_gid_t, gid);
}

PRE(sys_setsid, 0)
{
   PRINT("sys_setsid ( )");
   PRE_REG_READ0(long, "setsid");
}

PRE(sys_setgroups16, 0)
{
   PRINT("sys_setgroups16 ( %llu, %p )", (ULong)arg1, arg2);
   PRE_REG_READ2(long, "setgroups16", int, size, vki_old_gid_t *, list);
   if (arg1 > 0)
      PRE_MEM_READ( "setgroups16(list)", arg2, arg1 * sizeof(vki_old_gid_t) );
}

PRE(sys_setgroups, 0)
{
   PRINT("setgroups ( %llu, %p )", (ULong)arg1, arg2);
   PRE_REG_READ2(long, "setgroups", int, size, vki_gid_t *, list);
   if (arg1 > 0)
      PRE_MEM_READ( "setgroups(list)", arg2, arg1 * sizeof(vki_gid_t) );
}

PRE(sys_setpgid, 0)
{
   PRINT("setpgid ( %d, %d )", arg1, arg2);
   PRE_REG_READ2(long, "setpgid", vki_pid_t, pid, vki_pid_t, pgid);
}

POST(sys_setpgid)
{
   VG_(main_pgrp) = VG_(getpgrp)();
}

PRE(sys_setregid, 0)
{
   PRINT("sys_setregid ( %d, %d )", arg1, arg2);
   PRE_REG_READ2(long, "setregid", vki_gid_t, rgid, vki_gid_t, egid);
}

PRE(sys_setreuid16, 0)
{
   PRINT("setreuid16 ( 0x%x, 0x%x )", arg1, arg2);
   PRE_REG_READ2(long, "setreuid16", vki_old_uid_t, ruid, vki_old_uid_t, euid);
}

PRE(sys_setreuid, 0)
{
   PRINT("sys_setreuid ( 0x%x, 0x%x )", arg1, arg2);
   PRE_REG_READ2(long, "setreuid", vki_uid_t, ruid, vki_uid_t, euid);
}

PRE(sys_setrlimit, 0)
{
   PRINT("sys_setrlimit ( %d, %p )", arg1,arg2);
   PRE_REG_READ2(long, "setrlimit",
                 unsigned int, resource, struct rlimit *, rlim);
   PRE_MEM_READ( "setrlimit(rlim)", arg2, sizeof(struct vki_rlimit) );

   if (arg1 == VKI_RLIMIT_NOFILE) {
      if (((struct vki_rlimit *)arg2)->rlim_cur > VG_(fd_hard_limit) ||
          ((struct vki_rlimit *)arg2)->rlim_max != VG_(fd_hard_limit)) {
         set_result( -VKI_EPERM );
      }
      else {
         VG_(fd_soft_limit) = ((struct vki_rlimit *)arg2)->rlim_cur;
         set_result( 0 );
      }
   }
   else if (arg1 == VKI_RLIMIT_DATA) {
      if (((struct vki_rlimit *)arg2)->rlim_cur > ((struct vki_rlimit *)arg2)->rlim_max ||
          ((struct vki_rlimit *)arg2)->rlim_max > ((struct vki_rlimit *)arg2)->rlim_max) {
         set_result( -VKI_EPERM );
      }
      else {
         VG_(client_rlimit_data) = *(struct vki_rlimit *)arg2;
         set_result( 0 );
      }
   }
   else if (arg1 == VKI_RLIMIT_STACK && tid == 1) {
      if (((struct vki_rlimit *)arg2)->rlim_cur > ((struct vki_rlimit *)arg2)->rlim_max ||
          ((struct vki_rlimit *)arg2)->rlim_max > ((struct vki_rlimit *)arg2)->rlim_max) {
         set_result( -VKI_EPERM );
      }
      else {
         VG_(threads)[tid].stack_size = ((struct vki_rlimit *)arg2)->rlim_cur;
         VG_(client_rlimit_stack) = *(struct vki_rlimit *)arg2;
         set_result( 0 );
      }
   }
}

PRE(sys_setuid16, 0)
{
   PRINT("sys_setuid16 ( %d )", arg1);
   PRE_REG_READ1(long, "setuid16", vki_old_uid_t, uid);
}

PRE(sys_setuid, 0)
{
   PRINT("sys_setuid ( %d )", arg1);
   PRE_REG_READ1(long, "setuid", vki_uid_t, uid);
}

PRE(sys_socketcall, MayBlock)
{
   PRINT("sys_socketcall ( %d, %p )",arg1,arg2);
   PRE_REG_READ2(long, "socketcall", int, call, unsigned long *, args);

   switch (arg1 /* request */) {

   case VKI_SYS_SOCKETPAIR:
      /* int socketpair(int d, int type, int protocol, int sv[2]); */
      PRE_MEM_READ( "socketcall.socketpair(args)", arg2, 4*sizeof(Addr) );
      PRE_MEM_WRITE( "socketcall.socketpair(sv)", 
		     ((UWord*)arg2)[3], 2*sizeof(int) );
      break;

   case VKI_SYS_SOCKET:
      /* int socket(int domain, int type, int protocol); */
      PRE_MEM_READ( "socketcall.socket(args)", arg2, 3*sizeof(Addr) );
      break;

   case VKI_SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
	 int addrlen); */
      PRE_MEM_READ( "socketcall.bind(args)", arg2, 3*sizeof(Addr) );
      pre_mem_read_sockaddr( tid, "socketcall.bind(my_addr.%s)",
			     (struct vki_sockaddr *) (((UWord*)arg2)[1]), ((UWord*)arg2)[2]);
      break;
               
   case VKI_SYS_LISTEN:
      /* int listen(int s, int backlog); */
      PRE_MEM_READ( "socketcall.listen(args)", arg2, 2*sizeof(Addr) );
      break;

   case VKI_SYS_ACCEPT: {
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      PRE_MEM_READ( "socketcall.accept(args)", arg2, 3*sizeof(Addr) );
      {
	 Addr addr_p     = ((UWord*)arg2)[1];
	 Addr addrlen_p  = ((UWord*)arg2)[2];
	 if (addr_p != (Addr)NULL) 
	    buf_and_len_pre_check ( tid, addr_p, addrlen_p,
				    "socketcall.accept(addr)",
				    "socketcall.accept(addrlen_in)" );
      }
      break;
   }

   case VKI_SYS_SENDTO:
      /* int sendto(int s, const void *msg, int len, 
	 unsigned int flags, 
	 const struct sockaddr *to, int tolen); */
      PRE_MEM_READ( "socketcall.sendto(args)", arg2, 6*sizeof(Addr) );
      PRE_MEM_READ( "socketcall.sendto(msg)",
		     ((UWord*)arg2)[1], /* msg */
		     ((UWord*)arg2)[2]  /* len */ );
      pre_mem_read_sockaddr( tid, "socketcall.sendto(to.%s)",
			     (struct vki_sockaddr *) (((UWord*)arg2)[4]), ((UWord*)arg2)[5]);
      break;

   case VKI_SYS_SEND:
      /* int send(int s, const void *msg, size_t len, int flags); */
      PRE_MEM_READ( "socketcall.send(args)", arg2, 4*sizeof(Addr) );
      PRE_MEM_READ( "socketcall.send(msg)",
		     ((UWord*)arg2)[1], /* msg */
		     ((UWord*)arg2)[2]  /* len */ );
      break;

   case VKI_SYS_RECVFROM:
      /* int recvfrom(int s, void *buf, int len, unsigned int flags,
	 struct sockaddr *from, int *fromlen); */
      PRE_MEM_READ( "socketcall.recvfrom(args)", arg2, 6*sizeof(Addr) );
      {
	 Addr buf_p      = ((UWord*)arg2)[1];
	 Int  len        = ((UWord*)arg2)[2];
	 Addr from_p     = ((UWord*)arg2)[4];
	 Addr fromlen_p  = ((UWord*)arg2)[5];

	 PRE_MEM_WRITE( "socketcall.recvfrom(buf)", buf_p, len );
	 if (from_p != (Addr)NULL) 
	    buf_and_len_pre_check ( tid, from_p, fromlen_p, 
				    "socketcall.recvfrom(from)",
				    "socketcall.recvfrom(fromlen_in)" );
      }
      break;
   
   case VKI_SYS_RECV:
      /* int recv(int s, void *buf, int len, unsigned int flags); */
      /* man 2 recv says:
	 The  recv call is normally used only on a connected socket
	 (see connect(2)) and is identical to recvfrom with a  NULL
	 from parameter.
      */
      PRE_MEM_READ( "socketcall.recv(args)", arg2, 4*sizeof(Addr) );
      PRE_MEM_WRITE( "socketcall.recv(buf)", 
		     ((UWord*)arg2)[1], /* buf */
		     ((UWord*)arg2)[2]  /* len */ );
      break;

   case VKI_SYS_CONNECT:
      /* int connect(int sockfd, 
	 struct sockaddr *serv_addr, int addrlen ); */
      PRE_MEM_READ( "socketcall.connect(args)", arg2, 3*sizeof(Addr) );
      PRE_MEM_READ( "socketcall.connect(serv_addr.sa_family)",
		     ((UWord*)arg2)[1], /* serv_addr */
		     sizeof(vki_sa_family_t));
      pre_mem_read_sockaddr( tid,
			     "socketcall.connect(serv_addr.%s)",
			     (struct vki_sockaddr *) (((UWord*)arg2)[1]), ((UWord*)arg2)[2]);
      break;

   case VKI_SYS_SETSOCKOPT:
      /* int setsockopt(int s, int level, int optname, 
	 const void *optval, int optlen); */
      PRE_MEM_READ( "socketcall.setsockopt(args)", arg2, 5*sizeof(Addr) );
      PRE_MEM_READ( "socketcall.setsockopt(optval)",
		     ((UWord*)arg2)[3], /* optval */
		     ((UWord*)arg2)[4]  /* optlen */ );
      break;

   case VKI_SYS_GETSOCKOPT:
      /* int setsockopt(int s, int level, int optname, 
	 void *optval, socklen_t *optlen); */
      PRE_MEM_READ( "socketcall.getsockopt(args)", arg2, 5*sizeof(Addr) );
      {
	 Addr optval_p  = ((UWord*)arg2)[3];
	 Addr optlen_p  = ((UWord*)arg2)[4];
	 /* vg_assert(sizeof(socklen_t) == sizeof(UInt)); */
	 if (optval_p != (Addr)NULL) 
	    buf_and_len_pre_check ( tid, optval_p, optlen_p,
				    "socketcall.getsockopt(optval)",
				    "socketcall.getsockopt(optlen)" );
      }
      break;

   case VKI_SYS_GETSOCKNAME:
      /* int getsockname(int s, struct sockaddr* name, int* namelen) */
      PRE_MEM_READ( "socketcall.getsockname(args)", arg2, 3*sizeof(Addr) );
      {
	 Addr name_p     = ((UWord*)arg2)[1];
	 Addr namelen_p  = ((UWord*)arg2)[2];

	 /* Nb: name_p cannot be NULL */
	 buf_and_len_pre_check ( tid, name_p, namelen_p,
				 "socketcall.getsockname(name)",
				 "socketcall.getsockname(namelen_in)" );
      }
      break;

   case VKI_SYS_GETPEERNAME:
      /* int getpeername(int s, struct sockaddr* name, int* namelen) */
      PRE_MEM_READ( "socketcall.getpeername(args)", arg2, 3*sizeof(Addr) );
      {
	 Addr name_p     = ((UWord*)arg2)[1];
	 Addr namelen_p  = ((UWord*)arg2)[2];

	 /* Nb: name_p cannot be NULL */
	 buf_and_len_pre_check ( tid, name_p, namelen_p,
				 "socketcall.getpeername(name)",
				 "socketcall.getpeername(namelen_in)" );
      }
      break;

   case VKI_SYS_SHUTDOWN:
      /* int shutdown(int s, int how); */
      PRE_MEM_READ( "socketcall.shutdown(args)", arg2, 2*sizeof(Addr) );
      break;

   case VKI_SYS_SENDMSG: {
      /* int sendmsg(int s, const struct msghdr *msg, int flags); */

      /* this causes warnings, and I don't get why. glibc bug?
       * (after all it's glibc providing the arguments array)
       PRE_MEM_READ( "socketcall.sendmsg(args)", arg2, 3*sizeof(Addr) );
      */

      struct vki_msghdr *msg = (struct vki_msghdr *)((UWord*)arg2)[ 1 ];
      msghdr_foreachfield ( tid, msg, pre_mem_read_sendmsg );

      break;
   }
      
   case VKI_SYS_RECVMSG: {
      /* int recvmsg(int s, struct msghdr *msg, int flags); */

      /* this causes warnings, and I don't get why. glibc bug?
       * (after all it's glibc providing the arguments array)
       PRE_MEM_READ("socketcall.recvmsg(args)", arg2, 3*sizeof(Addr) );
      */

      struct vki_msghdr *msg = (struct vki_msghdr *)((UWord*)arg2)[ 1 ];
      msghdr_foreachfield ( tid, msg, pre_mem_write_recvmsg );

      break;
   }

   default:
      VG_(message)(Vg_DebugMsg,"Warning: unhandled socketcall 0x%x",arg1);
      set_result( -VKI_EINVAL );
      break;
   }
}

POST(sys_socketcall)
{
   switch (arg1 /* request */) {

   case VKI_SYS_SOCKETPAIR: {
      Int fd1 = ((Int*)((UWord*)arg2)[3])[0];
      Int fd2 = ((Int*)((UWord*)arg2)[3])[1];
      POST_MEM_WRITE( ((UWord*)arg2)[3], 2*sizeof(int) );
      if (!VG_(fd_allowed)(fd1, "socketcall.socketpair", tid, True) ||
          !VG_(fd_allowed)(fd2, "socketcall.socketpair", tid, True)) {
         VG_(close)(fd1);
         VG_(close)(fd2);
         set_result( -VKI_EMFILE );
      } else {
         POST_MEM_WRITE( ((UWord*)arg2)[3], 2*sizeof(int) );
         if (VG_(clo_track_fds)) {
            VG_(record_fd_open)(tid, fd1, NULL);
            VG_(record_fd_open)(tid, fd2, NULL);
         }
      }
      break;
   }

   case VKI_SYS_SOCKET:
      if (!VG_(fd_allowed)(res, "socket", tid, True)) {
	 VG_(close)(res);
	 set_result( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, res, NULL);
      }
      break;

   case VKI_SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
			int addrlen); */
      break;
               
   case VKI_SYS_LISTEN:
      /* int listen(int s, int backlog); */
      break;

   case VKI_SYS_ACCEPT: {
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      if (!VG_(fd_allowed)(res, "accept", tid, True)) {
	 VG_(close)(res);
	 set_result( -VKI_EMFILE );
      } else {
	 Addr addr_p     = ((UWord*)arg2)[1];
	 Addr addrlen_p  = ((UWord*)arg2)[2];

	 if (addr_p != (Addr)NULL) 
	    buf_and_len_post_check ( tid, res, addr_p, addrlen_p,
				     "socketcall.accept(addrlen_out)" );
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, res, NULL);
      }
      break;
   }

   case VKI_SYS_SENDTO:
      break;

   case VKI_SYS_SEND:
      break;

   case VKI_SYS_RECVFROM:
      {
	 Addr buf_p      = ((UWord*)arg2)[1];
	 Int  len        = ((UWord*)arg2)[2];
	 Addr from_p     = ((UWord*)arg2)[4];
	 Addr fromlen_p  = ((UWord*)arg2)[5];

	 if (from_p != (Addr)NULL) 
	    buf_and_len_post_check ( tid, res, from_p, fromlen_p,
				     "socketcall.recvfrom(fromlen_out)" );
	 POST_MEM_WRITE( buf_p, len );
      }
      break;

   case VKI_SYS_RECV:
      if (res >= 0 && ((UWord*)arg2)[1] != 0) {
	 POST_MEM_WRITE( ((UWord*)arg2)[1], /* buf */
		         ((UWord*)arg2)[2]  /* len */ );
      }
      break;

   case VKI_SYS_CONNECT:
      break;

   case VKI_SYS_SETSOCKOPT:
      break;

   case VKI_SYS_GETSOCKOPT:
      {
	 Addr optval_p  = ((UWord*)arg2)[3];
	 Addr optlen_p  = ((UWord*)arg2)[4];

	 if (optval_p != (Addr)NULL) 
	    buf_and_len_post_check ( tid, res, optval_p, optlen_p,
				     "socketcall.getsockopt(optlen_out)" );
      }
      break;

   case VKI_SYS_GETSOCKNAME:
      {
	 Addr name_p     = ((UWord*)arg2)[1];
	 Addr namelen_p  = ((UWord*)arg2)[2];

	 buf_and_len_post_check ( tid, res, name_p, namelen_p,
				  "socketcall.getsockname(namelen_out)" );
      }
      break;

   case VKI_SYS_GETPEERNAME:
      {
	 Addr name_p     = ((UWord*)arg2)[1];
	 Addr namelen_p  = ((UWord*)arg2)[2];

	 buf_and_len_post_check ( tid, res, name_p, namelen_p,
				  "socketcall.getpeername(namelen_out)" );
      }
      break;

   case VKI_SYS_SHUTDOWN:
      break;

   case VKI_SYS_SENDMSG:
      break;

   case VKI_SYS_RECVMSG:
   {
      struct vki_msghdr *msg = (struct vki_msghdr *)((UWord*)arg2)[ 1 ];

      msghdr_foreachfield( tid, msg, post_mem_write_recvmsg );
      check_cmsg_for_fds( tid, msg );

      break;
   }

   default:
      VG_(message)(Vg_DebugMsg,"FATAL: unhandled socketcall 0x%x",arg1);
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
}

PRE(sys_newstat, 0)
{
   PRINT("sys_newstat ( %p(%s), %p )", arg1,arg1,arg2);
   PRE_REG_READ2(long, "stat", char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "stat(file_name)", arg1 );
   PRE_MEM_WRITE( "stat(buf)", arg2, sizeof(struct vki_stat) );
}

POST(sys_newstat)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat) );
}

PRE(sys_statfs, 0)
{
   PRINT("sys_statfs ( %p, %p )",arg1,arg2);
   PRE_REG_READ2(long, "statfs", const char *, path, struct statfs *, buf);
   PRE_MEM_RASCIIZ( "statfs(path)", arg1 );
   PRE_MEM_WRITE( "statfs(buf)", arg2, sizeof(struct vki_statfs) );
}

POST(sys_statfs)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_statfs) );
}

PRE(sys_statfs64, 0)
{
   PRINT("sys_statfs64 ( %p, %llu, %p )",arg1,(ULong)arg2,arg3);
   PRE_REG_READ3(long, "statfs64",
                 const char *, path, vki_size_t, size, struct statfs64 *, buf);
   PRE_MEM_RASCIIZ( "statfs64(path)", arg1 );
   PRE_MEM_WRITE( "statfs64(buf)", arg3, arg2 );
}

POST(sys_statfs64)
{
   POST_MEM_WRITE( arg3, arg2 );
}

PRE(sys_symlink, MayBlock)
{
   PRINT("sys_symlink ( %p, %p )",arg1,arg2);
   PRE_REG_READ2(long, "symlink", const char *, oldpath, const char *, newpath);
   PRE_MEM_RASCIIZ( "symlink(oldpath)", arg1 );
   PRE_MEM_RASCIIZ( "symlink(newpath)", arg2 );
}

PRE(sys_stat64, 0)
{
   PRINT("sys_stat64 ( %p, %p )",arg1,arg2);
   PRE_REG_READ2(long, "stat64", char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "stat64(file_name)", arg1 );
   PRE_MEM_WRITE( "stat64(buf)", arg2, sizeof(struct vki_stat64) );
}

POST(sys_stat64)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat64) );
}

PRE(sys_fstat64, 0)
{
   PRINT("sys_fstat64 ( %d, %p )",arg1,arg2);
   PRE_REG_READ2(long, "fstat64", unsigned long, fd, struct stat64 *, buf);
   PRE_MEM_WRITE( "fstat64(buf)", arg2, sizeof(struct vki_stat64) );
}

POST(sys_fstat64)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat64) );
}

PRE(sys_time, 0)
{
   /* time_t time(time_t *t); */
   PRINT("sys_time ( %p )",arg1);
   PRE_REG_READ1(long, "time", int *, t);
   if (arg1 != 0) {
      PRE_MEM_WRITE( "time(t)", arg1, sizeof(vki_time_t) );
   }
}

POST(sys_time)
{
   if (arg1 != 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_time_t) );
   }
}

PRE(sys_times, 0)
{
   PRINT("sys_times ( %p )", arg1);
   PRE_REG_READ1(long, "times", struct tms *, buf);
   PRE_MEM_WRITE( "times(buf)", arg1, sizeof(struct vki_tms) );
}

POST(sys_times)
{
   if (arg1 != 0) {
      POST_MEM_WRITE( arg1, sizeof(struct vki_tms) );
   }
}

PRE(sys_umask, 0)
{
   PRINT("sys_umask ( %d )", arg1);
   PRE_REG_READ1(long, "umask", int, mask);
}

PRE(sys_unlink, MayBlock)
{
   PRINT("sys_unlink ( %p(%s) )", arg1,arg1);
   PRE_REG_READ1(long, "unlink", const char *, pathname);
   PRE_MEM_RASCIIZ( "unlink(pathname)", arg1 );
}

PRE(sys_newuname, 0)
{
   PRINT("sys_newuname ( %p )", arg1);
   PRE_REG_READ1(long, "uname", struct new_utsname *, buf);
   PRE_MEM_WRITE( "uname(buf)", arg1, sizeof(struct vki_new_utsname) );
}

POST(sys_newuname)
{
   if (arg1 != 0) {
      POST_MEM_WRITE( arg1, sizeof(struct vki_new_utsname) );
   }
}

PRE(sys_utime, MayBlock)
{
   PRINT("sys_utime ( %p, %p )", arg1,arg2);
   PRE_REG_READ2(long, "utime", char *, filename, struct utimbuf *, buf);
   PRE_MEM_RASCIIZ( "utime(filename)", arg1 );
   if (arg2 != 0)
      PRE_MEM_READ( "utime(buf)", arg2, sizeof(struct vki_utimbuf) );
}

PRE(sys_waitpid, MayBlock)
{
   PRINT("sys_waitpid ( %d, %p, %d )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "waitpid", 
                 vki_pid_t, pid, unsigned int *, status, int, options);

   if (arg2 != (Addr)NULL)
      PRE_MEM_WRITE( "waitpid(status)", arg2, sizeof(int) );
}

POST(sys_waitpid)
{
   if (arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, sizeof(int) );
}

PRE(sys_wait4, MayBlock)
{
   PRINT("sys_wait4 ( %d, %p, %d, %p )", arg1,arg2,arg3,arg4);
   arg3 &= ~(__VKI_WCLONE | __VKI_WALL);

   PRE_REG_READ4(long, "wait4", 
                 vki_pid_t, pid, unsigned int *, status, int, options,
                 struct rusage *, rusage);
   if (arg2 != (Addr)NULL)
      PRE_MEM_WRITE( "wait4(status)", arg2, sizeof(int) );
   if (arg4 != (Addr)NULL)
      PRE_MEM_WRITE( "wait4(rusage)", arg4, sizeof(struct vki_rusage) );
}

POST(sys_wait4)
{
   if (arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, sizeof(int) );
   if (arg4 != (Addr)NULL)
      POST_MEM_WRITE( arg4, sizeof(struct vki_rusage) );
}

PRE(sys_writev, MayBlock)
{
   Int i;
   struct vki_iovec * vec;
   PRINT("sys_writev ( %d, %p, %llu )",arg1,arg2,(ULong)arg3);
   PRE_REG_READ3(ssize_t, "writev",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count);
   if (!VG_(fd_allowed)(arg1, "writev", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      PRE_MEM_READ( "writev(vector)", 
		     arg2, arg3 * sizeof(struct vki_iovec) );
      if (arg2 != 0) {
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct vki_iovec *)arg2;
         for (i = 0; i < (Int)arg3; i++)
            PRE_MEM_READ( "writev(vector[...])",
                           (Addr)vec[i].iov_base, vec[i].iov_len );
      }
   }
}

PRE(sys_utimes, 0)
{
   PRINT("sys_utimes ( %p, %p )", arg1,arg2);
   PRE_REG_READ2(long, "utimes", char *, filename, struct timeval *, tvp);
   PRE_MEM_RASCIIZ( "utimes(filename)", arg1 );
   if (arg2 != 0)
      PRE_MEM_READ( "utimes(tvp)", arg2, sizeof(struct vki_timeval) );
}

PRE(sys_sched_setaffinity, 0)
{
   PRINT("sched_setaffinity ( %d, %d, %p )", arg1, arg2, arg3);
   PRE_REG_READ3(long, "sched_setaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_READ( "sched_setaffinity(mask)", arg3, arg2);
}

PRE(sys_sched_getaffinity, 0)
{
   PRINT("sched_getaffinity ( %d, %d, %p )", arg1, arg2, arg3);
   PRE_REG_READ3(long, "sched_getaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_WRITE( "sched_getaffinity(mask)", arg3, arg2);
}

POST(sys_sched_getaffinity)
{
   VG_TRACK(post_mem_write, arg3, arg2);
}

PRE(sys_acct, 0)
{
   PRINT("sys_acct ( %p )", arg1);
   PRE_REG_READ1(long, "acct", const char *, filename);
   PRE_MEM_RASCIIZ( "acct(filename)", arg1 );
}

PRE(sys_pause, MayBlock)
{
   PRINT("sys_pause ( )");
   PRE_REG_READ0(long, "pause");
}

// XXX: x86-specific
PRE(sys_sigsuspend, MayBlock)
{
   /* The C library interface to sigsuspend just takes a pointer to
      a signal mask but this system call has three arguments - the first
      two don't appear to be used by the kernel and are always passed as
      zero by glibc and the third is the first word of the signal mask
      so only 32 signals are supported.
     
      In fact glibc normally uses rt_sigsuspend if it is available as
      that takes a pointer to the signal mask so supports more signals.
    */
   PRINT("sys_sigsuspend ( %d, %d, %d )", arg1,arg2,arg3 );
   PRE_REG_READ3(int, "sigsuspend",
                 int, history0, int, history1,
                 vki_old_sigset_t, mask);
}

// XXX: x86-specific
PRE(sys_rt_sigsuspend, MayBlock)
{
   /* The C library interface to sigsuspend just takes a pointer to
      a signal mask but this system call has two arguments - a pointer
      to the mask and the number of bytes used by it. The kernel insists
      on the size being equal to sizeof(sigset_t) however and will just
      return EINVAL if it isn't.
    */
   PRINT("sys_rt_sigsuspend ( %p, %d )", arg1,arg2 );
   PRE_REG_READ2(int, "rt_sigsuspend", vki_sigset_t *, mask, vki_size_t, size)
   if (arg1 != (Addr)NULL) {
      PRE_MEM_READ( "rt_sigsuspend(mask)", arg1, sizeof(vki_sigset_t) );
   }
}

PRE(sys_rt_sigtimedwait, MayBlock)
{
   PRINT("sys_rt_sigtimedwait ( %p, %p, %p, %lld )",
         arg1,arg2,arg3,(ULong)arg4);
   PRE_REG_READ4(long, "rt_sigtimedwait", 
                 const vki_sigset_t *, set, vki_siginfo_t *, info,
                 const struct timespec *, timeout, vki_size_t, sigsetsize);
   if (arg1 != 0) 
      PRE_MEM_READ(  "rt_sigtimedwait(set)",  arg1, sizeof(vki_sigset_t));
   if (arg2 != 0)
      PRE_MEM_WRITE( "rt_sigtimedwait(info)", arg2, sizeof(vki_siginfo_t) );
   PRE_MEM_READ( "rt_sigtimedwait(timeout)",
                 arg4, sizeof(struct vki_timespec) );
}

POST(sys_rt_sigtimedwait)
{
   if (arg2 != 0)
      POST_MEM_WRITE( arg2, sizeof(vki_siginfo_t) );
}

PRE(sys_rt_sigqueueinfo, 0)
{
   PRINT("sys_rt_sigqueueinfo(%d, %d, %p)", arg1, arg2, arg3);
   PRE_REG_READ3(long, "rt_sigqueueinfo", 
                 int, pid, int, sig, vki_siginfo_t *, uinfo);
   if (arg2 != 0)
      PRE_MEM_READ( "rt_sigqueueinfo(uinfo)", arg3, sizeof(vki_siginfo_t) );
}

POST(sys_rt_sigqueueinfo)
{
   if (res >= 0 && 
       arg2 != 0 &&
       !VG_(is_sig_ign)(arg2) &&
       !VG_(sigismember)(&tst->eff_sig_mask, arg2) &&
       arg1 == VG_(getpid)()) {
      VG_(proxy_waitsig)();
   }
}

#define SIGNAL_SIMULATION	1

#if SIGNAL_SIMULATION
#define SIG_SIM   Special
#else
#define SIG_SIM   0
#endif

// XXX: x86-specific
PRE(sys_sigaltstack, SIG_SIM)
{
   /* int sigaltstack(const stack_t *ss, stack_t *oss); */
   PRINT("sigaltstack ( %p, %p )",arg1,arg2);
   PRE_REG_READ2(int, "sigaltstack",
                 const vki_stack_t *, ss, vki_stack_t *, oss);
   if (arg1 != 0) {
      PRE_MEM_READ( "sigaltstack(ss)", arg1, sizeof(vki_stack_t) );
   }
   if (arg2 != 0) {
      PRE_MEM_WRITE( "sigaltstack(oss)", arg2, sizeof(vki_stack_t) );
   }

   if (SIGNAL_SIMULATION)
      VG_(do_sys_sigaltstack) (tid);
}

POST(sys_sigaltstack)
{
   if (res == 0 && arg2 != 0)
      POST_MEM_WRITE( arg2, sizeof(vki_stack_t));
}

// XXX: x86-specific
PRE(sys_sigaction, SIG_SIM)
{
   PRINT("sys_sigaction ( %d, %p, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(int, "sigaction",
                 int, signum, const struct old_sigaction *, act,
                 struct old_sigaction *, oldact)
   if (arg2 != 0)
      PRE_MEM_READ( "sigaction(act)", arg2, sizeof(struct vki_old_sigaction));
   if (arg3 != 0)
      PRE_MEM_WRITE( "sigaction(oldact)", arg3, sizeof(struct vki_old_sigaction));

   if (SIGNAL_SIMULATION)
      VG_(do_sys_sigaction)(tid);
}

POST(sys_sigaction)
{
   if (res == 0 && arg3 != 0)
      POST_MEM_WRITE( arg3, sizeof(struct vki_old_sigaction));
}

// XXX: x86-specific
PRE(sys_rt_sigaction, SIG_SIM)
{
   PRINT("sys_rt_sigaction ( %d, %p, %p, %d )", arg1,arg2,arg3,arg4);
   PRE_REG_READ4(long, "rt_sigaction",
                 int, signum, const struct sigaction *, act,
                 struct sigaction *, oldact, vki_size_t, sigsetsize);

   if (arg2 != 0)
      PRE_MEM_READ( "rt_sigaction(act)", arg2, sizeof(struct vki_sigaction));
   if (arg3 != 0)
      PRE_MEM_WRITE( "rt_sigaction(oldact)", arg3, sizeof(struct vki_sigaction));

   // XXX: hmm... doesn't seem quite right...
   if (SIGNAL_SIMULATION)
      VG_(do_sys_sigaction)(tid);
}

POST(sys_rt_sigaction)
{
   if (res == 0 && arg3 != 0)
      POST_MEM_WRITE( arg3, sizeof(struct vki_sigaction));
}

PRE(sys_sigprocmask, SIG_SIM)
{
   PRINT("sys_sigprocmask ( %d, %p, %p )",arg1,arg2,arg3);
   PRE_REG_READ3(long, "sigprocmask", 
                 int, how, vki_old_sigset_t *, set, vki_old_sigset_t *, oldset);
   if (arg2 != 0)
      PRE_MEM_READ( "sigprocmask(set)", arg2, sizeof(vki_old_sigset_t));
   if (arg3 != 0)
      PRE_MEM_WRITE( "sigprocmask(oldset)", arg3, sizeof(vki_old_sigset_t));

   if (SIGNAL_SIMULATION) {
      // Nb: We must convert the smaller vki_old_sigset_t params into bigger
      // vki_sigset_t params.
      vki_old_sigset_t* set    = (vki_old_sigset_t*)arg2;
      vki_old_sigset_t* oldset = (vki_old_sigset_t*)arg3;
      vki_sigset_t bigger_set;
      vki_sigset_t bigger_oldset;

      VG_(memset)(&bigger_set, 0, sizeof(vki_sigset_t));
      bigger_set.sig[0] = *(vki_old_sigset_t*)set;

      VG_(do_sys_sigprocmask) ( tid, 
				arg1 /*how*/, 
				&bigger_set,
				&bigger_oldset );

      if (oldset)
         *oldset = bigger_oldset.sig[0];
   }
}

POST(sys_sigprocmask)
{
   if (res == 0 && arg3 != 0)
      POST_MEM_WRITE( arg3, sizeof(vki_old_sigset_t));
}

PRE(sys_rt_sigprocmask, SIG_SIM)
{
   PRINT("sys_rt_sigprocmask ( %d, %p, %p, %llu )",arg1,arg2,arg3,(ULong)arg4);
   PRE_REG_READ4(long, "rt_sigprocmask", 
                 int, how, vki_sigset_t *, set, vki_sigset_t *, oldset,
                 vki_size_t, sigsetsize);
   if (arg2 != 0)
      PRE_MEM_READ( "rt_sigprocmask(set)", arg2, sizeof(vki_sigset_t));
   if (arg3 != 0)
      PRE_MEM_WRITE( "rt_sigprocmask(oldset)", arg3, sizeof(vki_sigset_t));

   // Like the kernel, we fail if the sigsetsize is not exactly what we expect.
   if (sizeof(vki_sigset_t) != arg4)
      set_result( -VKI_EMFILE );
   else if (SIGNAL_SIMULATION)
      VG_(do_sys_sigprocmask) ( tid, 
				arg1 /*how*/, 
				(vki_sigset_t*) arg2,
				(vki_sigset_t*) arg3 );
}

POST(sys_rt_sigprocmask)
{
   if (res == 0 && arg3 != 0)
      POST_MEM_WRITE( arg3, sizeof(vki_sigset_t));
}

PRE(sys_sigpending, NBRunInLWP)
{
   PRINT( "sys_sigpending ( %p )", arg1 );
   PRE_REG_READ1(long, "sigpending", vki_old_sigset_t *, set);
   PRE_MEM_WRITE( "sigpending(set)", arg1, sizeof(vki_old_sigset_t));
}

POST(sys_sigpending)
{
   POST_MEM_WRITE( arg1, sizeof(vki_old_sigset_t) ) ;
}

PRE(sys_rt_sigpending, NBRunInLWP)
{
   PRINT( "sys_rt_sigpending ( %p )", arg1 );
   PRE_REG_READ2(long, "rt_sigpending", 
                 vki_sigset_t *, set, vki_size_t, sigsetsize);
   PRE_MEM_WRITE( "rt_sigpending(set)", arg1, sizeof(vki_sigset_t));
}

POST(sys_rt_sigpending)
{
   POST_MEM_WRITE( arg1, sizeof(vki_sigset_t) ) ;
}

PRE(sys_mq_open, 0)
{
   PRINT("sys_mq_open( %p(%s), %d, %lld, %p )",
         arg1,arg1,arg2,(ULong)arg3,arg4);
   PRE_REG_READ4(long, "mq_open",
                 const char *, name, int, oflag, vki_mode_t, mode,
                 struct mq_attr *, attr);
   PRE_MEM_RASCIIZ( "mq_open(name)", arg1 );
   if ((arg2 & VKI_O_CREAT) != 0 && arg4 != 0) {
      const struct vki_mq_attr *attr = (struct vki_mq_attr *)arg4;
      PRE_MEM_READ( "mq_open(attr->mq_maxmsg)",
                     (Addr)&attr->mq_maxmsg, sizeof(attr->mq_maxmsg) );
      PRE_MEM_READ( "mq_open(attr->mq_msgsize)",
                     (Addr)&attr->mq_msgsize, sizeof(attr->mq_msgsize) );
   }
}

POST(sys_mq_open)
{
   if (!VG_(fd_allowed)(res, "mq_open", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open)(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
}

PRE(sys_mq_unlink, 0)
{
   PRINT("sys_mq_unlink ( %p(%s) )", arg1,arg1);
   PRE_REG_READ1(long, "mq_unlink", const char *, name);
   PRE_MEM_RASCIIZ( "mq_unlink(name)", arg1 );
}

PRE(sys_mq_timedsend, MayBlock)
{
   PRINT("sys_mq_timedsend ( %d, %p, %llu, %d, %p )",
         arg1,arg2,(ULong)arg3,arg4,arg5);
   PRE_REG_READ5(long, "mq_timedsend",
                 vki_mqd_t, mqdes, const char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int, msg_prio, const struct timespec *, abs_timeout);
   if (!VG_(fd_allowed)(arg1, "mq_timedsend", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      PRE_MEM_READ( "mq_timedsend(msg_ptr)", arg2, arg3 );
      if (arg5 != 0)
         PRE_MEM_READ( "mq_timedsend(abs_timeout)", arg5,
                        sizeof(struct vki_timespec) );
   }
}

PRE(sys_mq_timedreceive, MayBlock)
{
   PRINT("sys_mq_timedreceive( %d, %p, %llu, %p, %p )",
         arg1,arg2,(ULong)arg3,arg4,arg5);
   PRE_REG_READ5(ssize_t, "mq_timedreceive",
                 vki_mqd_t, mqdes, char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int *, msg_prio,
                 const struct timespec *, abs_timeout);
   if (!VG_(fd_allowed)(arg1, "mq_timedreceive", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      PRE_MEM_WRITE( "mq_timedreceive(msg_ptr)", arg2, arg3 );
      if (arg4 != 0)
         PRE_MEM_WRITE( "mq_timedreceive(msg_prio)",
                        arg4, sizeof(unsigned int) );
      if (arg5 != 0)
         PRE_MEM_READ( "mq_timedreceive(abs_timeout)",
                        arg5, sizeof(struct vki_timespec) );
   }
}

POST(sys_mq_timedreceive)
{
   POST_MEM_WRITE( arg2, arg3 );
   if (arg4 != 0)
      POST_MEM_WRITE( arg4, sizeof(unsigned int) );
}

PRE(sys_mq_notify, 0)
{
   PRINT("sys_mq_notify( %d, %p )", arg1,arg2 );
   PRE_REG_READ2(long, "mq_notify",
                 vki_mqd_t, mqdes, const struct sigevent *, notification);
   if (!VG_(fd_allowed)(arg1, "mq_notify", tid, False))
      set_result( -VKI_EBADF );
   else if (arg2 != 0)
      PRE_MEM_READ( "mq_notify(notification)",
                    arg2, sizeof(struct vki_sigevent) );
}

PRE(sys_mq_getsetattr, 0)
{
   PRINT("sys_mq_getsetattr( %d, %p, %p )", arg1,arg2,arg3 );
   PRE_REG_READ3(long, "mq_getsetattr",
                 vki_mqd_t, mqdes, const struct mq_attr *, mqstat,
                 struct mq_attr *, omqstat);
   if (!VG_(fd_allowed)(arg1, "mq_getsetattr", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      if (arg2 != 0) {
         const struct vki_mq_attr *attr = (struct vki_mq_attr *)arg2;
         PRE_MEM_READ( "mq_getsetattr(mqstat->mq_flags)",
                        (Addr)&attr->mq_flags, sizeof(attr->mq_flags) );
      }
      if (arg3 != 0)
         PRE_MEM_WRITE( "mq_getsetattr(omqstat)", arg3,
                        sizeof(struct vki_mq_attr) );
   }   
}

POST(sys_mq_getsetattr)
{
   if (arg3 != 0)
      POST_MEM_WRITE( arg3, sizeof(struct vki_mq_attr) );
}

PRE(sys_timer_create, 0)
{
   PRINT("sys_timer_create( %d, %p, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "timer_create",
                 vki_clockid_t, clockid, struct sigevent *, evp,
                 vki_timer_t *, timerid);
   if (arg2 != 0)
      PRE_MEM_READ( "timer_create(evp)", arg2, sizeof(struct vki_sigevent) );
   PRE_MEM_WRITE( "timer_create(timerid)", arg3, sizeof(vki_timer_t) );
}

POST(sys_timer_create)
{
   POST_MEM_WRITE( arg3, sizeof(vki_timer_t) );
}

PRE(sys_timer_settime, 0)
{
   PRINT("sys_timer_settime( %lld, %d, %p, %p )", (ULong)arg1,arg2,arg3,arg4);
   PRE_REG_READ4(long, "timer_settime", 
                 vki_timer_t, timerid, int, flags,
                 const struct itimerspec *, value,
                 struct itimerspec *, ovalue);
   PRE_MEM_READ( "timer_settime(value)", arg3,
                  sizeof(struct vki_itimerspec) );
   if (arg4 != 0)
       PRE_MEM_WRITE( "timer_settime(ovalue)", arg4,
                      sizeof(struct vki_itimerspec) );
}

POST(sys_timer_settime)
{
   if (arg4 != 0)
      POST_MEM_WRITE( arg4, sizeof(struct vki_itimerspec) );
}

PRE(sys_timer_gettime, 0)
{
   PRINT("sys_timer_gettime( %lld, %p )", (ULong)arg1,arg2);
   PRE_REG_READ2(long, "timer_gettime", 
                 vki_timer_t, timerid, struct itimerspec *, value);
   PRE_MEM_WRITE( "timer_gettime(value)", arg2,
                  sizeof(struct vki_itimerspec));
}

POST(sys_timer_gettime)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_itimerspec) );
}

PRE(sys_timer_getoverrun, 0)
{
   PRINT("sys_timer_getoverrun( %p )", arg1);
   PRE_REG_READ1(long, "timer_getoverrun", vki_timer_t, timerid);
}

PRE(sys_timer_delete, 0)
{
   PRINT("sys_timer_delete( %p )", arg1);
   PRE_REG_READ1(long, "timer_delete", vki_timer_t, timerid);
}

PRE(sys_clock_settime, 0)
{
   PRINT("sys_clock_settime( %d, %p )", arg1,arg2);
   PRE_REG_READ2(long, "clock_settime", 
                 vki_clockid_t, clk_id, const struct timespec *, tp);
   PRE_MEM_READ( "clock_settime(tp)", arg2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_gettime, 0)
{
   PRINT("sys_clock_gettime( %d, %p )" , arg1,arg2);
   PRE_REG_READ2(long, "clock_gettime", 
                 vki_clockid_t, clk_id, struct timespec *, tp);
   PRE_MEM_WRITE( "clock_gettime(tp)", arg2, sizeof(struct vki_timespec) );
}

POST(sys_clock_gettime)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_getres, 0)
{
   PRINT("sys_clock_getres( %d, %p )" , arg1,arg2);
   // Nb: we can't use "res" as the param name because that's a macro
   // defined above!
   PRE_REG_READ2(long, "clock_getres", 
                 vki_clockid_t, clk_id, struct timespec *, cres);
   PRE_MEM_WRITE( "clock_getres(cres)", arg2, sizeof(struct vki_timespec) );
}

POST(sys_clock_getres)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_timespec) );
}


/* ---------------------------------------------------------------------
   Executing the syscalls
   ------------------------------------------------------------------ */

static UInt bad_flags = Special;
static void bad_before(ThreadId tid, ThreadState *tst)
{
   VG_(message)
      (Vg_DebugMsg,"WARNING: unhandled syscall: %u", (UInt)SYSNO);
   if (VG_(clo_verbosity) > 1) {
      ExeContext *ec = VG_(get_ExeContext)(tid);
      VG_(pp_ExeContext)(ec);
   }
   VG_(message)
      (Vg_DebugMsg,"Do not panic.  You may be able to fix this easily.");
   VG_(message)
      (Vg_DebugMsg,"Read the file README_MISSING_SYSCALL_OR_IOCTL.");

   set_result( -VKI_ENOSYS );
}

static const struct SyscallTableEntry bad_sys =
   { &bad_flags, bad_before, NULL };


Bool VG_(pre_syscall) ( ThreadId tid )
{
   ThreadState* tst;
   UInt         syscallno, flags;
   const struct SyscallTableEntry *sys;
   Bool isSpecial    = False;
   Bool mayBlock     = False;
   Bool runInLWP     = False;
   Bool syscall_done = False;	/* we actually ran the syscall */

   VGP_PUSHCC(VgpCoreSysWrap);

   tst = VG_(get_ThreadState)(tid);

   /* Convert vfork to fork, since we can't handle it otherwise. */
   if (SYSNO == __NR_vfork)
      SYSNO = __NR_fork;

   syscallno = (UInt)SYSNO;

   if (tst->syscallno != -1)
      VG_(printf)("tid %d has syscall %d\n", tst->tid, tst->syscallno);

   vg_assert(tst->syscallno == -1);             // should be no current syscall
   vg_assert(tst->status == VgTs_Runnable);     // should be runnable */

   /* the syscall no is in %eax.  For syscalls with <= 6 args,
      args 1 .. 6 to the syscall are in %ebx %ecx %edx %esi %edi %ebp.
      For calls with > 6 args, %ebx points to a lump of memory
      containing the args.

      The result is returned in %eax.  If this value >= 0, the call
      succeeded, and this is the return value.  If < 0, it failed, and
      the negation of this value is errno.  To be more specific, 
      if res is in the range -EMEDIUMTYPE (-124) .. -EPERM (-1)
      (kernel 2.4.9 sources, include/asm-i386/errno.h)
      then it indicates an error.  Otherwise it doesn't.

      Dirk Mueller (mueller@kde.org) says that values -4095 .. -1
      (inclusive?) indicate error returns.  Not sure where the -4095
      comes from.
   */

   tst->syscallno = syscallno;
   vg_assert(tst->status == VgTs_Runnable);

   if (syscallno < VGA_(syscall_table_size) &&
       VGA_(syscall_table)[syscallno].before != NULL)
   {
      sys = &VGA_(syscall_table)[syscallno];
   } else {
      sys = &bad_sys;
   }
   flags = *(sys->flags_ptr);

   {
      Bool nbrunInLWP = ( flags & NBRunInLWP ? True : False );
      isSpecial       = ( flags & Special    ? True : False );
      mayBlock        = ( flags & MayBlock   ? True : False );
      runInLWP        = mayBlock || nbrunInLWP;
      // At most one of these should be true
      vg_assert( isSpecial + mayBlock + nbrunInLWP <= 1 );
   }

   tst->sys_flags = flags;

   /* Do any pre-syscall actions */
   if (VG_(needs).syscall_wrapper) {
      VGP_PUSHCC(VgpToolSysWrap);
      TL_(pre_syscall)(tid, syscallno);
      VGP_POPCC(VgpToolSysWrap);
   }

   PRINT("SYSCALL[%d,%d](%3d)%s%s:", 
         VG_(getpid)(), tid, syscallno, 
         isSpecial ? " special"  : "",
         runInLWP  ? " runInLWP" : "");

   if (isSpecial) {
      /* "Special" syscalls are implemented by Valgrind internally,
	 and do not generate real kernel calls.  The expectation,
	 therefore, is that the "before" function not only does the
	 appropriate tests, but also performs the syscall itself and
	 sets the result.  Special syscalls cannot block. */
      vg_assert(!mayBlock && !runInLWP);

      (sys->before)(tst->tid, tst);

      vg_assert(tst->sys_flags == flags);

      PRINT(" --> %lld (0x%llx)\n", (Long)(Word)res, (ULong)res);
      syscall_done = True;
   } else {
      (sys->before)(tst->tid, tst);

      if ((Word)res <= 0) {
	 /* "before" decided the syscall wasn't viable, so don't do
	    anything - just pretend the syscall happened. */
         PRINT(" ==> %lld (0x%llx)\n", (Long)(Word)res, (ULong)res);
	 syscall_done = True;
      } else if (runInLWP) {
	 /* Issue to worker.  If we're waiting on the syscall because
	    it's in the hands of the ProxyLWP, then set the thread
	    state to WaitSys. */
         PRINT(" --> ...\n");
	 tst->status = VgTs_WaitSys;
	 VG_(sys_issue)(tid);
      } else {
	 /* run the syscall directly */
	 res = VG_(do_syscall)(syscallno, arg1, arg2, arg3, arg4, arg5, arg6);
         PRINT(" --> %lld (0x%llx)\n", (Long)(Word)res, (ULong)res);
	 syscall_done = True;
      }
   }

   VGP_POPCC(VgpCoreSysWrap);

   vg_assert(( syscall_done && tst->status == VgTs_Runnable) ||
	     (!syscall_done && tst->status == VgTs_WaitSys ));

   return syscall_done;
}

static void restart_syscall(ThreadId tid)
{
   ThreadState* tst;
   tst = VG_(get_ThreadState)(tid);

   vg_assert(tst != NULL);
   vg_assert(tst->status == VgTs_WaitSys);
   vg_assert(tst->syscallno != -1);

   SYSNO = tst->syscallno;
   VGA_(restart_syscall)(&tst->arch);
}

void VG_(post_syscall) ( ThreadId tid, Bool restart )
{
   ThreadState* tst;
   UInt syscallno, flags;
   const struct SyscallTableEntry *sys;
   Bool isSpecial = False;
   Bool restarted = False;

   VGP_PUSHCC(VgpCoreSysWrap);

   tst = VG_(get_ThreadState)(tid);
   vg_assert(tst->tid == tid);

   /* Tell the tool about the syscall return value */
   SET_SYSCALL_RETVAL(tst->tid, res);

   syscallno = tst->syscallno;

   vg_assert(syscallno != -1);			/* must be a current syscall */

   if (syscallno < VGA_(syscall_table_size) &&
       VGA_(syscall_table)[syscallno].before != NULL)
   {
      sys = &VGA_(syscall_table)[syscallno];
   } else {
      sys = &bad_sys;
   }
   flags = *(sys->flags_ptr);

   isSpecial = flags & Special;

   if (res == -VKI_ERESTARTSYS) {
      /* Applications never expect to see this, so we should either
	 restart the syscall or fail it with EINTR, depending on what
	 our caller wants.  Generally they'll want to restart, but if
	 client set the signal state to not restart, then we fail with
	 EINTR.  Either way, ERESTARTSYS means the syscall made no
	 progress, and so can be failed or restarted without
	 consequence. */
      if (0)
	 VG_(printf)("syscall %d returned ERESTARTSYS; restart=%d\n",
		     syscallno, restart);

      if (restart) {
	 restarted = True;
	 restart_syscall(tid);
      } else
	 res = -VKI_EINTR;
   } 

   if (!restarted) {
      if (sys->after != NULL &&
          ((tst->sys_flags & PostOnFail) != 0 || !VG_(is_kerror)(res)))
	 (sys->after)(tst->tid, tst);

      /* Do any post-syscall actions

	 NOTE: this is only called if the syscall completed.  If the
	 syscall was restarted, then it will call the Tool's
	 pre_syscall again, without calling post_syscall (ie, more
	 pre's than post's)
       */
      if (VG_(needs).syscall_wrapper) {
	 VGP_PUSHCC(VgpToolSysWrap);
	 TL_(post_syscall)(tid, syscallno, res);
	 VGP_POPCC(VgpToolSysWrap);
      }
   }

   tst->status = VgTs_Runnable;	/* runnable again */
   tst->syscallno = -1;		/* no current syscall */

   VGP_POPCC(VgpCoreSysWrap);
}

#undef SYSNO
#undef res
#undef arg1
#undef arg2
#undef arg3
#undef arg4
#undef arg5
#undef arg6

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

