
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

   * notify the tool of the memory events (reads, writes) happening

   * perform the syscall, usually by passing it along to the kernel
     unmodified.

   A magical piece of assembly code, VG_(do_syscall)(), in vg_syscall.S
   does the tricky bit of passing a syscall to the kernel, whilst
   having the simulator retain control.
*/

#define PRE_MEM_READ(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_read, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define PRE_MEM_RASCIIZ(zzname, zzaddr) \
   VG_TRACK( pre_mem_read_asciiz, Vg_CoreSysCall, tid, zzname, zzaddr)

#define PRE_MEM_WRITE(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_write, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define POST_MEM_WRITE(zzaddr, zzlen) \
   VG_TRACK( post_mem_write, zzaddr, zzlen)

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
static Bool valid_client_addr(Addr start, SizeT size, ThreadId tid, const Char *syscallname)
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

   if (!valid_client_addr(old_addr, old_size, tid, "mremap(old_addr)"))
      return -VKI_EFAULT;

   /* fixed at the current address means we don't move it */
   if ((flags & VKI_MREMAP_FIXED) && (old_addr == new_addr))
      flags &= ~(VKI_MREMAP_FIXED|VKI_MREMAP_MAYMOVE);

   if (flags & VKI_MREMAP_FIXED) {
      if (PGROUNDDN(new_addr) != new_addr)
	 return -VKI_EINVAL;

      if (!valid_client_addr(new_addr, new_size, tid, "mremap(new_addr)"))
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

static
void record_fd_open(Int tid, Int fd, char *pathname)
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

   if(VG_(getrlimit) (VKI_RLIMIT_NOFILE, &lim) == -1) {
      /* Hmm.  getrlimit() failed.  Now we're screwed, so just choose
         an arbitrarily high number.  1024 happens to be the limit in
         the 2.4 kernels. */
      count = 1024;
   } else {
      count = lim.rlim_cur;
   }

   for (i = 0; i < count; i++)
      if(VG_(fcntl)(i, VKI_F_GETFL, 0) != -1)
         record_fd_open(-1, i, NULL);
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
               record_fd_open(-1, fno, VG_(resolve_filename)(fno));
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
               //      fd_allowed()?
               record_fd_open (tid, fds[i], VG_(resolve_filename)(fds[i]));
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
         SK_(pre_mem_write) ( Vg_CoreSysCall,
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
         SK_(post_mem_write) ( buf_p, buflen_out );
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
static Bool fd_allowed(Int fd, const Char *syscallname, ThreadId tid, Bool soft)
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
   The Main Entertainment ...
   ------------------------------------------------------------------ */

#define MayBlock   (1 << 0)
#define PostOnFail (1 << 1)

#define PRE(x)	static void before_##x(ThreadId tid, ThreadState *tst)
#define POST(x)	static void  after_##x(ThreadId tid, ThreadState *tst)

#define PREALIAS(new, old)	\
	PRE(new) __attribute__((alias(STR(before_##old))))
#define POSTALIAS(new, old)	\
	POST(new) __attribute__((alias(STR(after_##old))))

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

PRE(exit_group)
{
   VG_(core_panic)("syscall exit_group() not caught by the scheduler?!");
}

PRE(exit)
{
   VG_(core_panic)("syscall exit() not caught by the scheduler?!");
}

PRE(ptrace)
{
   /* long ptrace (enum __ptrace_request request, pid_t pid, 
                   void *addr, void *data); */
   PRINT("ptrace ( %d, %d, %p, %p )", arg1,arg2,arg3,arg4);
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

POST(ptrace)
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

PRE(mount)
{
   // int  mount(const char *source, const char *target,
   //            const char *filesystemtype, unsigned long mountflags,
   //            const void *data);
   PRINT( "mount( %p, %p, %p, %p, %p )" ,arg1,arg2,arg3);
   PRE_MEM_RASCIIZ( "mount(specialfile)", arg1);
   PRE_MEM_RASCIIZ( "mount(dir)", arg2);
   PRE_MEM_RASCIIZ( "mount(filesystemtype)", arg3);
}

PRE(umount)
{
   /* int umount(const char *path) */
   PRINT("umount( %p )", arg1);
   PRE_MEM_RASCIIZ( "umount(path)",arg1);
}

PRE(modify_ldt)
{
   PRINT("modify_ldt ( %d, %p, %d )", arg1,arg2,arg3);
   if (arg1 == 0) {
      /* read the LDT into ptr */
      PRE_MEM_WRITE( "modify_ldt(ptr)(func=0)", arg2, arg3 );
   }
   if (arg1 == 1 || arg1 == 0x11) {
      /* write the LDT with the entry pointed at by ptr */
      PRE_MEM_READ( "modify_ldt(ptr)(func=1 or 0x11)", arg2, 
		    sizeof(vki_modify_ldt_t) );
   }
   /* "do" the syscall ourselves; the kernel never sees it */
   res = VG_(sys_modify_ldt)( tid, arg1, (void*)arg2, arg3 );

   if (arg1 == 0 && !VG_(is_kerror)(res) && res > 0) {
      POST_MEM_WRITE( arg2, res );
   }
}

PRE(set_thread_area)
{
   PRINT("set_thread_area ( %p )", arg1);

   PRE_MEM_READ( "set_thread_area(ptr)", arg1, 
		 sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   set_result( VG_(sys_set_thread_area)( tid, (void *)arg1 ) );
}

PRE(get_thread_area)
{
   PRINT("get_thread_area ( %p )", arg1);
   PRE_MEM_WRITE( "get_thread_area(ptr)", arg1, 
		  sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   set_result( VG_(sys_get_thread_area)( tid, (void *)arg1 ) );

   if (!VG_(is_kerror)(res)) {
      POST_MEM_WRITE( arg1, sizeof(vki_modify_ldt_t) );
   }
}

PRE(set_tid_address)
{
   PRINT("set_tid_address ( %p )", arg1);
}

PRE(setresgid)
{
   /* int setresgid(gid_t rgid, gid_t egid, gid_t sgid); */
   PRINT("setresgid ( %d, %d, %d )", arg1, arg2, arg3);
}

PRE(vhangup)
{
   PRINT("vhangup()");
}

PRE(iopl)
{
   PRINT("iopl ( %d )", arg1);
}

PRE(setxattr)
{
   PRINT("setxattr ( %p, %p, %p, %llu, %d )",
         arg1, arg2, arg3, (ULong)arg4, arg5);
   PRE_MEM_RASCIIZ( "setxattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "setxattr(name)", arg2 );
   PRE_MEM_READ( "setxattr(value)", arg3, arg4 );
}

PREALIAS(lsetxattr, setxattr);

PRE(fsetxattr)
{
   /* int fsetxattr (int filedes, const char *name,
      const void *value, size_t size, int flags); */
   PRINT("fsetxattr ( %d, %p, %p, %llu, %d )",
         arg1, arg2, arg3, (ULong)arg4, arg5);
   PRE_MEM_RASCIIZ( "fsetxattr(name)", arg2 );
   PRE_MEM_READ( "fsetxattr(value)", arg3, arg4 );
}

PRE(getxattr)
{
   PRINT("getxattr ( %p, %p, %p, %llu )", arg1,arg2,arg3, (ULong)arg4);
   PRE_MEM_RASCIIZ( "getxattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "getxattr(name)", arg2 );
   PRE_MEM_WRITE( "getxattr(value)", arg3, arg4 );
}

POST(getxattr)
{
   if (res > 0 && arg3 != (Addr)NULL) {
      POST_MEM_WRITE( arg3, res );
   }
}

PREALIAS(lgetxattr, getxattr);
POSTALIAS(lgetxattr, getxattr);

PRE(fgetxattr)
{
   PRINT("fgetxattr ( %d, %p, %p, %llu )", arg1, arg2, arg3, (ULong)arg4);
   PRE_MEM_RASCIIZ( "fgetxattr(name)", arg2 );
   PRE_MEM_WRITE( "fgetxattr(value)", arg3, arg4 );
}

POST(fgetxattr)
{
   if (res > 0 && arg3 != (Addr)NULL)
      POST_MEM_WRITE( arg3, res );
}

PRE(listxattr)
{
   PRINT("listxattr ( %p, %p, %llu )", arg1, arg2, (ULong)arg3);
   PRE_MEM_RASCIIZ( "listxattr(path)", arg1 );
   PRE_MEM_WRITE( "listxattr(list)", arg2, arg3 );
}

POST(listxattr)
{
   if (res > 0 && arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, res );
}

PREALIAS(llistxattr, listxattr);
POSTALIAS(llistxattr, listxattr);

PRE(flistxattr)
{
   /* ssize_t flistxattr (int filedes, char *list, size_t size); */
   PRINT("flistxattr ( %d, %p, %llu )", arg1, arg2, (ULong)arg3);
   PRE_MEM_WRITE( "listxattr(list)", arg2, arg3 );
}

POST(flistxattr)
{
   if (res > 0 && arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, res );
}

PRE(removexattr)
{
   PRINT("removexattr ( %p, %p )", arg1, arg2);
   PRE_MEM_RASCIIZ( "listxattr(path)", arg1 );
   PRE_MEM_RASCIIZ( "listxattr(name)", arg2 );
}

PREALIAS(lremovexattr, removexattr);

PRE(fremovexattr)
{
   PRINT("removexattr ( %d, %p )", arg1, arg2);
   PRE_MEM_RASCIIZ( "listxattr(name)", arg2 );
}

PRE(quotactl)
{
   PRINT("quotactl (0x%x, %p, 0x%x, 0x%x )", arg1,arg2,arg3, arg4);
   PRE_MEM_RASCIIZ( "quotactl(special)", arg2 );
}

PRE(lookup_dcookie)
{
   PRINT("lookup_dcookie (0x%llx, %p, %d)", LOHI64(arg1,arg2), arg3, arg4);
   PRE_MEM_WRITE( "lookup_dcookie(buf)", arg3, arg4);
}

POST(lookup_dcookie)
{
   if (arg3 != (Addr)NULL)
      POST_MEM_WRITE( arg3, res);
}

PRE(truncate64)
{
   PRINT("truncate64 ( %p, %lld )", arg1, LOHI64(arg2, arg3));
   PRE_MEM_RASCIIZ( "truncate64(path)", arg1 );
}

PRE(fdatasync)
{
   /* int fdatasync(int fd); */
   PRINT("fdatasync ( %d )", arg1);
}

PRE(msync)
{
   /* int msync(const void *start, size_t length, int flags); */
   PRINT("msync ( %p, %llu, %d )", arg1,(ULong)arg2,arg3);
   PRE_MEM_READ( "msync(start)", arg1, arg2 );
}

// Nb: getpmsg() and putpmsg() are special additional syscalls used in early
// versions of LiS (Linux Streams).  They are not part of the kernel.
// Therefore, we have to provide this type ourself, rather than getting it
// from the kernel sources.
struct pmsg_strbuf {
   int     maxlen;         /* no. of bytes in buffer */
   int     len;            /* no. of bytes returned */
   vki_caddr_t buf;        /* pointer to data */
};

PRE(getpmsg)
{
   /* LiS getpmsg from http://www.gcom.com/home/linux/lis/ */
   /* int getpmsg(int fd, struct strbuf *ctrl, struct strbuf *data, 
      int *bandp, int *flagsp); */
   struct pmsg_strbuf *ctrl;
   struct pmsg_strbuf *data;
   PRINT("getpmsg ( %d, %p, %p, %p, %p )", arg1,arg2,arg3,arg4,arg5);
   ctrl = (struct pmsg_strbuf *)arg2;
   data = (struct pmsg_strbuf *)arg3;
   if (ctrl && ctrl->maxlen > 0)
      PRE_MEM_WRITE( "getpmsg(ctrl)", (Addr)ctrl->buf, ctrl->maxlen);
   if (data && data->maxlen > 0)
      PRE_MEM_WRITE( "getpmsg(data)", (Addr)data->buf, data->maxlen);
   if (arg4)
      PRE_MEM_WRITE( "getpmsg(bandp)", (Addr)arg4, sizeof(int));
   if (arg5)
      PRE_MEM_WRITE( "getpmsg(flagsp)", (Addr)arg5, sizeof(int));
}

POST(getpmsg)
{
   struct pmsg_strbuf *ctrl;
   struct pmsg_strbuf *data;

   ctrl = (struct pmsg_strbuf *)arg2;
   data = (struct pmsg_strbuf *)arg3;
   if (res == 0 && ctrl && ctrl->len > 0) {
      POST_MEM_WRITE( (Addr)ctrl->buf, ctrl->len);
   }
   if (res == 0 && data && data->len > 0) {
      POST_MEM_WRITE( (Addr)data->buf, data->len);
   }
}

PRE(putpmsg)
{
   /* LiS putpmsg from http://www.gcom.com/home/linux/lis/ */
   /* int putpmsg(int fd, struct strbuf *ctrl, struct strbuf *data, 
      int band, int flags); */
   struct pmsg_strbuf *ctrl;
   struct pmsg_strbuf *data;
   PRINT("putpmsg ( %d, %p, %p, %d, %d )", arg1,arg2,arg3,arg4,arg5);
   ctrl = (struct pmsg_strbuf *)arg2;
   data = (struct pmsg_strbuf *)arg3;
   if (ctrl && ctrl->len > 0)
      PRE_MEM_READ( "putpmsg(ctrl)", (Addr)ctrl->buf, ctrl->len);
   if (data && data->len > 0)
      PRE_MEM_READ( "putpmsg(data)", (Addr)data->buf, data->len);
}

PRE(getitimer)
{
   /* int getitimer(int which, struct itimerval *value); */
   PRINT("getitimer ( %d, %p )", arg1, arg2);
   PRE_MEM_WRITE( "getitimer(timer)", arg2, sizeof(struct vki_itimerval) );
}

POST(getitimer)
{
   if (arg2 != (Addr)NULL) {
      VG_TRACK( post_mem_write,arg2, sizeof(struct vki_itimerval));
   }
}

PRE(syslog)
{
   /* int syslog(int type, char *bufp, int len); */
   PRINT("syslog (%d, %p, %d)",arg1,arg2,arg3);
   switch(arg1) {
   case 2: case 3: case 4:
      PRE_MEM_WRITE( "syslog(buf)", arg2, arg3);
      break;
   default: 
      break;
   }
}

POST(syslog)
{
   switch (arg1) {
   case 2: case 3: case 4:
      POST_MEM_WRITE( arg2, arg3 );
      break;
   default:
      break;
   }
}

PRE(personality)
{
   /* int personality(unsigned long persona); */
   PRINT("personality ( %d )", arg1);
}

PRE(chroot)
{
   /* int chroot(const char *path); */
   PRINT("chroot ( %p )", arg1);
   PRE_MEM_RASCIIZ( "chroot(path)", arg1 );
}

PRE(madvise)
{
   /* int madvise(void *start, size_t length, int advice ); */
   PRINT("madvise ( %p, %llu, %d )", arg1,(ULong)arg2,arg3);
}

PRE(mremap)
{
   // Nb: this is different to the glibc version described in the man pages,
   // which lacks the fifth 'new_address' argument.
   /* void* mremap(void * old_address, size_t old_size, 
      size_t new_size, unsigned long flags, void * new_address); */
   PRINT("mremap ( %p, %llu, %d, 0x%x, %p )", 
		arg1, (ULong)arg2, arg3, arg4, arg5);

   set_result( mremap_segment((Addr)arg1, arg2, (Addr)arg5, arg3, arg4, tid) );
}

PRE(nice)
{
   /* int nice(int inc); */
   PRINT("nice ( %d )", arg1);
}

PRE(setresgid32)
{
   /* int setresgid(gid_t rgid, gid_t egid, gid_t sgid); */
   PRINT("setresgid32 ( %d, %d, %d )", arg1, arg2, arg3);
}

PRE(setfsuid32)
{
   /* int setfsuid(uid_t fsuid); */
   PRINT("setfsuid ( %d )", arg1);
}

PRE(_sysctl)
{
   /* int _sysctl(struct __sysctl_args *args); */
   PRINT("_sysctl ( %p )", arg1 );
   PRE_MEM_WRITE( "_sysctl(args)", arg1, sizeof(struct __vki_sysctl_args) );
}

POST(_sysctl)
{
   POST_MEM_WRITE( arg1, sizeof(struct __vki_sysctl_args) );

}

PRE(sched_getscheduler)
{
   /* int sched_getscheduler(pid_t pid); */
   PRINT("sched_getscheduler ( %d )", arg1);
}

PRE(sched_setscheduler)
{
   /* int sched_setscheduler(pid_t pid, int policy, 
      const struct sched_param *p); */
   PRINT("sched_setscheduler ( %d, %d, %p )",arg1,arg2,arg3);
   if (arg3 != (UWord)NULL)
      PRE_MEM_READ( "sched_setscheduler(struct sched_param *p)", 
		    arg3, sizeof(struct vki_sched_param));
}

PRE(mlock)
{
   /* int mlock(const void * addr, size_t len) */
   PRINT("mlock ( %p, %llu )", arg1, (ULong)arg2);
}

PRE(munlock)
{
   /* int munlock(const void * addr, size_t len) */
   PRINT("munlock ( %p, %llu )", arg1, (ULong)arg2);
}

PRE(mlockall)
{
   /* int mlockall(int flags); */
   PRINT("mlockall ( %x )", arg1);
}

PRE(munlockall)
{
   /* int munlockall(void) */
   PRINT("munlockall()");
}

PRE(sched_get_priority_max)
{
   /* int sched_get_priority_max(int policy); */
   PRINT("sched_get_priority_max ( %d )", arg1);
}

PRE(sched_get_priority_min)
{
   /* int sched_get_priority_min(int policy); */
   PRINT("sched_get_priority_min ( %d )", arg1);
}

PRE(setpriority)
{
   /* int setpriority(int which, int who, int prio); */
   PRINT("setpriority ( %d, %d, %d )", arg1, arg2, arg3);
}

PRE(getpriority)
{
   /* int getpriority(int which, int who); */
   PRINT("getpriority ( %d, %d )", arg1, arg2);
}

PRE(setfsgid)
{
   /* int setfsgid(gid_t gid); */
   PRINT("setfsgid ( %d )", arg1);
}

PRE(setregid)
{
   /* int setregid(gid_t rgid, gid_t egid); */
   PRINT("setregid ( %d, %d )", arg1, arg2);
}

PRE(setresuid)
{
   /* int setresuid(uid_t ruid, uid_t euid, uid_t suid); */
   PRINT("setresuid ( %d, %d, %d )", arg1, arg2, arg3);
}

PRE(setfsuid)
{
   /* int setfsuid(uid_t uid); */
   PRINT("setfsuid ( %d )", arg1);
}

PRE(sendfile)
{
   /* ssize_t sendfile(int out_fd, int in_fd, off_t *offset, 
      size_t count) */
   PRINT("sendfile ( %d, %d, %p, %llu )",arg1,arg2,arg3,(ULong)arg4);
   if (arg3 != (UWord)NULL)
      PRE_MEM_WRITE( "sendfile(offset)", arg3, sizeof(vki_off_t) );
}

POST(sendfile)
{
   POST_MEM_WRITE( arg3, sizeof( vki_off_t ) );
}

PRE(sendfile64)
{
   /* ssize_t sendfile64(int out_df, int in_fd, loff_t *offset,
      size_t count); */
   PRINT("sendfile64 ( %d, %d, %p, %llu )",arg1,arg2,arg3,(ULong)arg4);
   if (arg3 != (UWord)NULL)
      PRE_MEM_WRITE( "sendfile64(offset)", arg3, sizeof(vki_loff_t) );
}

POST(sendfile64)
{
   if (arg3 != (UWord)NULL ) {
      POST_MEM_WRITE( arg3, sizeof(vki_loff_t) );
   }
}

PRE(pwrite64)
{
   // ssize_t pwrite64(int fd, const void *buf, size_t nbytes, loff_t offset);
   PRINT("pwrite64 ( %d, %p, %llu, %lld )",
         arg1, arg2, (ULong)arg3, LOHI64(arg4,arg5));
   PRE_MEM_READ( "pwrite64(buf)", arg2, arg3 );
}

PRE(sync)
{
   /* int sync(); */
   PRINT("sync ( )");
}

PRE(fstatfs)
{
   /* int fstatfs(int fd, struct statfs *buf); */
   PRINT("fstatfs ( %d, %p )",arg1,arg2);
   PRE_MEM_WRITE( "stat(buf)", arg2, sizeof(struct vki_statfs) );
}

POST(fstatfs)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_statfs) );
}

PRE(fstatfs64)
{
   /* int fstatfs64(int fd, size_t sz, struct statfs *buf); */
   PRINT("fstatfs64 ( %d, %llu, %p )",arg1,(ULong)arg2,arg3);
   PRE_MEM_WRITE( "stat(buf)", arg3, arg2 );
}

POST(fstatfs64)
{
   POST_MEM_WRITE( arg3, arg2 );
}

PRE(getsid)
{
   /* pid_t getsid(pid_t pid); */
   PRINT("getsid ( %d )", arg1);
}

PRE(pread64)
{
   /* ssize_t pread64(int fd, void *buf, size_t count, loff_t offset); */
   PRINT("pread ( %d, %p, %llu, %lld )",
         arg1, arg2, (ULong)arg3, LOHI64(arg4,arg5));
   PRE_MEM_WRITE( "pread(buf)", arg2, arg3 );
}

POST(pread64)
{
   if (res > 0) {
      POST_MEM_WRITE( arg2, res );
   }
}

PRE(mknod)
{
   /* int mknod(const char *pathname, mode_t mode, dev_t dev); */
   PRINT("mknod ( %p, 0x%x, 0x%x )", arg1, arg2, arg3 );
   PRE_MEM_RASCIIZ( "mknod(pathname)", arg1 );
}

PRE(flock)
{
   /* int flock(int fd, int operation); */
   PRINT("flock ( %d, %d )", arg1, arg2 );
}

PRE(init_module)
{
   /* int init_module(const char *name, struct module *image); */
   PRINT("init_module ( %p, %p )", arg1, arg2 );
   PRE_MEM_RASCIIZ( "init_module(name)", arg1 );
   PRE_MEM_READ( "init_module(image)", arg2, VKI_SIZEOF_STRUCT_MODULE );
}

PRE(ioperm)
{
   /* int ioperm(unsigned long from, unsigned long num, int turn_on); */
   PRINT("ioperm ( %d, %d, %d )", arg1, arg2, arg3 );
}

PRE(capget)
{
   /* int capget(cap_user_header_t header, cap_user_data_t data); */
   PRINT("capget ( %p, %p )", arg1, arg2 );
   PRE_MEM_READ( "capget(header)", arg1, 
                  sizeof(struct __vki_user_cap_header_struct) );
   PRE_MEM_WRITE( "capget(data)", arg2, 
                  sizeof(struct __vki_user_cap_data_struct) );
}

POST(capget)
{
   if (arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, sizeof(struct __vki_user_cap_data_struct) );
}

PRE(capset)
{
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

PRE(execve)
{
   /* int execve (const char *filename, 
      char *const argv [], 
      char *const envp[]); */
   PRINT("execve ( %p(%s), %p, %p )", arg1, arg1, arg2, arg3);

   PRE_MEM_RASCIIZ( "execve(filename)", arg1 );
   if (arg2 != (UWord)NULL)
      pre_argv_envp( arg2, tid, "execve(argv)", "execve(argv[i])" );
   if (arg3 != (UWord)NULL)
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

PRE(access)
{
   /* int access(const char *pathname, int mode); */
   PRINT("access ( %p(%s), %d )", arg1,arg1,arg2);
   PRE_MEM_RASCIIZ( "access(pathname)", arg1 );
}

PRE(alarm)
{
   /* unsigned int alarm(unsigned int seconds); */
   PRINT("alarm ( %d )", arg1);
}

PRE(brk)
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
   PRINT("brk ( %p )",arg1);

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

PRE(chdir)
{
   /* int chdir(const char *path); */
   PRINT("chdir ( %p )", arg1);
   PRE_MEM_RASCIIZ( "chdir(path)", arg1 );
}

PRE(chmod)
{
   /* int chmod(const char *path, mode_t mode); */
   PRINT("chmod ( %p, %d )", arg1,arg2);
   PRE_MEM_RASCIIZ( "chmod(path)", arg1 );
}

PRE(chown)
{
   /* int chown(const char *path, uid_t owner, gid_t group); */
   PRINT("chown ( %p, 0x%x, 0x%x )", arg1,arg2,arg3);
   PRE_MEM_RASCIIZ( "chown(path)", arg1 );
}

PREALIAS(chown32, chown);
PREALIAS(lchown32, chown);

PRE(close)
{
   /* int close(int fd); */
   PRINT("close ( %d )",arg1);
   /* Detect and negate attempts by the client to close Valgrind's log fd */
   if (!fd_allowed(arg1, "close", tid, False))
      set_result( -VKI_EBADF );
}

POST(close)
{
   if(VG_(clo_track_fds)) record_fd_close(tid, arg1);
}

PRE(dup)
{
   /* int dup(int oldfd); */
   PRINT("dup ( %d ) --> ", arg1);
}

POST(dup)
{
   PRINT("%d\n", res);
   if (!fd_allowed(res, "dup", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if(VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(resolve_filename)(res));
   }
}

PRE(dup2)
{
   /* int dup2(int oldfd, int newfd); */
   PRINT("dup2 ( %d, %d )", arg1,arg2);
   if (!fd_allowed(arg2, "dup2", tid, True))
      set_result( -VKI_EBADF );
}

POST(dup2)
{
   if(VG_(clo_track_fds))
      record_fd_open(tid, res, VG_(resolve_filename)(res));
}

PRE(fcntl)
{
   /* int fcntl(int fd, int cmd, int arg); */
   PRINT("fcntl ( %d, %d, %d )",arg1,arg2,arg3);
   if (arg2 == VKI_F_SETLKW)
      tst->sys_flags |= MayBlock;
}

POST(fcntl)
{
   if (arg2 == VKI_F_DUPFD) {
      if (!fd_allowed(res, "fcntl(DUPFD)", tid, True)) {
         VG_(close)(res);
         set_result( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            record_fd_open(tid, res, VG_(resolve_filename)(res));
      }
   }
}

PRE(fchdir)
{
   /* int fchdir(int fd); */
   PRINT("fchdir ( %d )", arg1);
}

PRE(fchown)
{
   /* int fchown(int filedes, uid_t owner, gid_t group); */
   PRINT("fchown ( %d, %d, %d )", arg1,arg2,arg3);
}

PREALIAS(fchown32, fchown);

PRE(fchmod)
{
   /* int fchmod(int fildes, mode_t mode); */
   PRINT("fchmod ( %d, %d )", arg1,arg2);
}

PRE(fcntl64)
{
   /* int fcntl64(int fd, int cmd, int arg); */
   PRINT("fcntl64 ( %d, %d, %d )", arg1,arg2,arg3);
   if (arg2 == VKI_F_SETLKW || arg2 == VKI_F_SETLKW64)
      tst->sys_flags |= MayBlock;
}

POST(fcntl64)
{
   if (arg2 == VKI_F_DUPFD) {
      if (!fd_allowed(res, "fcntl64(DUPFD)", tid, True)) {
         VG_(close)(res);
         set_result( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            record_fd_open(tid, res, VG_(resolve_filename)(res));
      }
   }
}

PRE(fstat)
{
   /* int fstat(int filedes, struct stat *buf); */
   PRINT("fstat ( %d, %p )",arg1,arg2);
   PRE_MEM_WRITE( "fstat", arg2, sizeof(struct vki_stat) );
}

POST(fstat)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat) );
}

static vki_sigset_t fork_saved_mask;

PRE(fork)
{
   vki_sigset_t mask;

   vg_assert(VG_(gettid)() == VG_(main_pid));

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   /* pid_t fork(void); */
   PRINT("fork ()");

   do_atfork_pre(tid);
}

POST(fork)
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

PRE(clone)
{
   PRINT("clone ( %d, %p, %p, %p, %p )",arg1,arg2,arg3,arg4,arg5);

   if (arg2 == 0 &&
       (arg1 == (VKI_CLONE_CHILD_CLEARTID|VKI_CLONE_CHILD_SETTID|VKI_SIGCHLD)
     || arg1 == (VKI_CLONE_PARENT_SETTID|VKI_SIGCHLD))) 
   {
      before_fork(tid, tst);
      set_result( VG_(do_syscall)(SYSNO, arg1, arg2, arg3, arg4, arg5) );
      after_fork(tid, tst);
   } else {
      VG_(unimplemented)
         ("clone(): not supported by Valgrind.\n   "
          "We do now support programs linked against\n   "
          "libpthread.so, though.  Re-run with -v and ensure that\n   "
          "you are picking up Valgrind's implementation of libpthread.so.");
   }
}

PRE(fsync)
{
   /* int fsync(int fd); */
   PRINT("fsync ( %d )", arg1);
}

PRE(ftruncate)
{
   /* int ftruncate(int fd, size_t length); */
   PRINT("ftruncate ( %d, %lld )", arg1,(ULong)arg2);
}

PRE(ftruncate64)
{
   /* int ftruncate64(int fd, off64_t length); */
   PRINT("ftruncate64 ( %d, %lld )", arg1, LOHI64(arg2,arg3));
}

PRE(getdents)
{
   /* int getdents(unsigned int fd, struct dirent *dirp, 
      unsigned int count); */
   PRINT("getdents ( %d, %p, %d )",arg1,arg2,arg3);
   PRE_MEM_WRITE( "getdents(dirp)", arg2, arg3 );
}

POST(getdents)
{
   if (res > 0)
      POST_MEM_WRITE( arg2, res );
}

PRE(getdents64)
{
   /* int getdents(unsigned int fd, struct dirent64 *dirp, 
      unsigned int count); */
   PRINT("getdents64 ( %d, %p, %d )",arg1,arg2,arg3);
   PRE_MEM_WRITE( "getdents64(dirp)", arg2, arg3 );
}

POST(getdents64)
{
   if (res > 0)
      POST_MEM_WRITE( arg2, res );
}

PRE(getgroups)
{
   /* int getgroups(int size, gid_t list[]); */
   PRINT("getgroups ( %d, %p )", arg1, arg2);
   if (arg1 > 0)
      PRE_MEM_WRITE( "getgroups(list)", arg2, arg1 * sizeof(vki_gid_t) );
}

POST(getgroups)
{
   if (arg1 > 0 && res > 0)
      POST_MEM_WRITE( arg2, res * sizeof(vki_gid_t) );
}

PREALIAS(getgroups32, getgroups);
POSTALIAS(getgroups32, getgroups);

PRE(getcwd)
{
   // Note that this prototype is the kernel one, with a different return
   // type to the glibc one!
   /* long getcwd(char *buf, size_t size); */
   PRINT("getcwd ( %p, %llu )",arg1,(ULong)arg2);
   PRE_MEM_WRITE( "getcwd(buf)", arg1, arg2 );
}

POST(getcwd)
{
   if (res != (Addr)NULL)
      POST_MEM_WRITE( arg1, res );
}

PRE(geteuid)
{
   /* uid_t geteuid(void); */
   PRINT("geteuid ( )");
}

PRE(geteuid32)
{
   /* ?? uid_t geteuid32(void); */
   PRINT("geteuid32(?) ( )");
}

PRE(getegid)
{
   /* gid_t getegid(void); */
   PRINT("getegid ()");
}

PRE(getegid32)
{
   /* gid_t getegid32(void); */
   PRINT("getegid32 ()");
}

PRE(getgid)
{
   /* gid_t getgid(void); */
   PRINT("getgid ()");
}

PRE(getgid32)
{
   /* gid_t getgid32(void); */
   PRINT("getgid32 ()");
}

PRE(getpid)
{
   /* pid_t getpid(void); */
   PRINT("getpid ()");
}

PRE(getpgid)
{
   /* pid_t getpgid(pid_t pid); */
   PRINT("getpgid ( %d )", arg1);
}

PRE(getpgrp)
{
   /* pid_t getpgrp(void); */
   PRINT("getpgrp ()");
}

PRE(getppid)
{
   /* pid_t getppid(void); */
   PRINT("getppid ()");
}

PRE(getresgid)
{
   /* int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid); */
   PRINT("getresgid ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_MEM_WRITE( "getresgid(rgid)", arg1, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(egid)", arg2, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(sgid)", arg3, sizeof(vki_gid_t) );
}

POST(getresgid)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_gid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_gid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_gid_t) );
   }
}

PRE(getresgid32)
{
   /* int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid); */
   PRINT("getresgid32 ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_MEM_WRITE( "getresgid32(rgid)", arg1, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid32(egid)", arg2, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid32(sgid)", arg3, sizeof(vki_gid_t) );
}

POST(getresgid32)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_gid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_gid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_gid_t) );
   }
}

PRE(getresuid)
{
   /* int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid); */
   PRINT("getresuid ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_MEM_WRITE( "getresuid(ruid)", arg1, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(euid)", arg2, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(suid)", arg3, sizeof(vki_uid_t) );
}

POST(getresuid)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_uid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_uid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_uid_t) );
   }
}

PRE(getresuid32)
{
   /* int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid); */
   PRINT("getresuid32 ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_MEM_WRITE( "getresuid32(ruid)", arg1, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid32(euid)", arg2, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid32(suid)", arg3, sizeof(vki_uid_t) );
}

POST(getresuid32)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_uid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_uid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_uid_t) );
   }
}

PRE(getrlimit)
{
   /* int getrlimit (int resource, struct rlimit *rlim); */
   PRINT("getrlimit ( %d, %p )", arg1,arg2);
   PRE_MEM_WRITE( "getrlimit(rlim)", arg2, sizeof(struct vki_rlimit) );
}

POST(getrlimit)
{
    POST_MEM_WRITE( arg2, sizeof(struct vki_rlimit) );

    switch(arg1) {
    case VKI_RLIMIT_NOFILE:
	((struct vki_rlimit *)arg2)->rlim_cur = VG_(fd_soft_limit);
	((struct vki_rlimit *)arg2)->rlim_max = VG_(fd_hard_limit);
	break;

    case VKI_RLIMIT_DATA:
	*((struct vki_rlimit *)arg2) = VG_(client_rlimit_data);
	break;

    case VKI_RLIMIT_STACK:
	*((struct vki_rlimit *)arg2) = VG_(client_rlimit_stack);
	break;
    }
}

PREALIAS(ugetrlimit, getrlimit);
POSTALIAS(ugetrlimit, getrlimit);

PRE(getrusage)
{
   /* int getrusage (int who, struct rusage *usage); */
   PRINT("getrusage ( %d, %p )", arg1,arg2);
   PRE_MEM_WRITE( "getrusage(usage)", arg2, sizeof(struct vki_rusage) );
}

POST(getrusage)
{
   if (res == 0)
      VG_TRACK( post_mem_write,arg2, sizeof(struct vki_rusage) );
}

PRE(gettimeofday)
{
   /* int gettimeofday(struct timeval *tv, struct timezone *tz); */
   PRINT("gettimeofday ( %p, %p )",arg1,arg2);
   PRE_MEM_WRITE( "gettimeofday(tv)", arg1, sizeof(struct vki_timeval) );
   if (arg2 != 0)
      PRE_MEM_WRITE( "gettimeofday(tz)", arg2, sizeof(struct vki_timezone) );
}

POST(gettimeofday)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(struct vki_timeval) );
      if (arg2 != 0)
	 POST_MEM_WRITE( arg2, sizeof(struct vki_timezone) );
   }
}

PRE(getuid)
{
   /* uid_t getuid(void); */
   PRINT("getuid ( )");
}

PRE(getuid32)
{
   /* ???uid_t getuid32(void); */
   PRINT("getuid32 ( )");
}

PRE(ipc)
{
   PRINT("ipc ( %d, %d, %d, %d, %p, %d )", arg1,arg2,arg3,arg4,arg5,arg6);
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
      if (arg6 != (UWord)NULL)
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
      else if (!valid_client_addr(arg5, segmentSize, tid, "shmat"))
	 set_result( -VKI_EINVAL );
      break;
   }
   case 22: /* IPCOP_shmdt */
      if (!valid_client_addr(arg5, 1, tid, "shmdt"))
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

POST(ipc)
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

PRE(ioctl)
{
   /* int ioctl(int d, int request, ...)
      [The  "third"  argument  is traditionally char *argp, 
      and will be so named for this discussion.]
   */
   /*
     VG_(message)(
     Vg_DebugMsg, 
     "is an IOCTL,  request = 0x%x,   d = %d,   argp = 0x%x", 
     arg2,arg1,arg3);
   */
   PRINT("ioctl ( %d, 0x%x, %p )",arg1,arg2,arg3);
   switch (arg2 /* request */) {
   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
      PRE_MEM_READ( "ioctl(TCSET{S,SW,SF})", arg3, 
		     sizeof(struct vki_termios) );
      break; 
   case VKI_TCGETS:
      PRE_MEM_WRITE( "ioctl(TCGETS)", arg3, 
		     sizeof(struct vki_termios) );
      break;
   case VKI_TCSETA:
   case VKI_TCSETAW:
   case VKI_TCSETAF:
      PRE_MEM_READ( "ioctl(TCSET{A,AW,AF})", arg3,
		     sizeof(struct vki_termio) );
      break;
   case VKI_TCGETA:
      PRE_MEM_WRITE( "ioctl(TCGETA)", arg3,
		     sizeof(struct vki_termio) );
      break;
   case VKI_TCSBRK:
   case VKI_TCXONC:
   case VKI_TCSBRKP:
   case VKI_TCFLSH:
      /* These just take an int by value */
      break;
   case VKI_TIOCGWINSZ:
      PRE_MEM_WRITE( "ioctl(TIOCGWINSZ)", arg3, 
		     sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
      PRE_MEM_READ( "ioctl(TIOCSWINSZ)", arg3, 
		     sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCMBIS:
      PRE_MEM_READ( "ioctl(TIOCMBIS)", arg3,
                     sizeof(unsigned int) );
      break;
   case VKI_TIOCMBIC:
      PRE_MEM_READ( "ioctl(TIOCMBIC)", arg3,
                     sizeof(unsigned int) );
      break;
   case VKI_TIOCMSET:
      PRE_MEM_READ( "ioctl(TIOCMSET)", arg3,
                     sizeof(unsigned int) );
      break;
   case VKI_TIOCLINUX:
      PRE_MEM_READ( "ioctl(TIOCLINUX)", arg3, 
		     sizeof(char *) );
      if (*(char *)arg3 == 11) {
	 PRE_MEM_READ( "ioctl(TIOCLINUX, 11)", 
			arg3, 2 * sizeof(char *) );
      }
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", arg3,
		     sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", arg3,
		     sizeof(vki_pid_t) );
      break;
   case VKI_TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      PRE_MEM_WRITE( "ioctl(TIOCGPTN)", 
		     arg3, sizeof(int) );
      break;
   case VKI_TIOCSCTTY:
      /* Just takes an int value.  */
      break;
   case VKI_TIOCSPTLCK: /* Lock/unlock Pty */
      PRE_MEM_READ( "ioctl(TIOCSPTLCK)", 
		     arg3, sizeof(int) );
      break;
   case VKI_FIONBIO:
      PRE_MEM_READ( "ioctl(FIONBIO)", 
		     arg3, sizeof(int) );
      break;
   case VKI_FIOASYNC:
      PRE_MEM_READ( "ioctl(FIOASYNC)", 
		     arg3, sizeof(int) );
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      PRE_MEM_WRITE( "ioctl(FIONREAD)", 
		     arg3, sizeof(int) );
      break;

   case VKI_SG_SET_COMMAND_Q:
      PRE_MEM_READ( "ioctl(SG_SET_COMMAND_Q)", 
		     arg3, sizeof(int) );
      break;
   case VKI_SG_IO:
      PRE_MEM_WRITE( "ioctl(SG_IO)", arg3, 
		     sizeof(vki_sg_io_hdr_t) );
      break;
   case VKI_SG_GET_SCSI_ID:
      PRE_MEM_WRITE( "ioctl(SG_GET_SCSI_ID)", arg3, 
		     sizeof(vki_sg_scsi_id_t) );
      break;
   case VKI_SG_SET_RESERVED_SIZE:
      PRE_MEM_READ( "ioctl(SG_SET_RESERVED_SIZE)", 
		     arg3, sizeof(int) );
      break;
   case VKI_SG_SET_TIMEOUT:
      PRE_MEM_READ( "ioctl(SG_SET_TIMEOUT)", arg3, 
		     sizeof(int) );
      break;
   case VKI_SG_GET_RESERVED_SIZE:
      PRE_MEM_WRITE( 
		     "ioctl(SG_GET_RESERVED_SIZE)", arg3, 
		     sizeof(int) );
      break;
   case VKI_SG_GET_TIMEOUT:
      PRE_MEM_WRITE( "ioctl(SG_GET_TIMEOUT)", arg3, 
		     sizeof(int) );
      break;
   case VKI_SG_GET_VERSION_NUM:
      PRE_MEM_READ( "ioctl(SG_GET_VERSION_NUM)", 
		     arg3, sizeof(int) );
      break;
   case VKI_SG_EMULATED_HOST: /* 0x2203 */
      PRE_MEM_WRITE(  "ioctl(SG_EMULATED_HOST)",
		     arg3, sizeof(int) );
      break;
   case VKI_SG_GET_SG_TABLESIZE: /* 0x227f */
      PRE_MEM_WRITE( "ioctl(SG_GET_SG_TABLESIZE)",
		     arg3, sizeof(int) );
      break;

   case VKI_IIOCGETCPS:
      /* In early 2.4 kernels, ISDN_MAX_CHANNELS was only defined
       * when KERNEL was. I never saw a larger value than 64 though */
#              ifndef ISDN_MAX_CHANNELS
#              define ISDN_MAX_CHANNELS 64
#              endif
      PRE_MEM_WRITE( "ioctl(IIOCGETCPS)", arg3,
		     ISDN_MAX_CHANNELS 
		     * 2 * sizeof(unsigned long) );
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
      PRE_MEM_WRITE( "ioctl(SIOCGIFINDEX)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFFLAGS)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFFLAGS)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFHWADDR:       /* Get hardware address         */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFHWADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFHWADDR)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMTU)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMTU)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFADDR)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFNETMASK)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNETMASK)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMETRIC)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMETRIC)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMAP:          /* Get device parameters        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMAP)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMAP)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFTXQLEN:       /* Get the tx queue length      */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFTXQLEN)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFTXQLEN)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFDSTADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFDSTADDR)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFBRDADDR)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFBRDADDR)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNAME:         /* get iface name               */
      PRE_MEM_READ( "ioctl(SIOCGIFNAME)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_ifindex,
                     sizeof(((struct vki_ifreq *)arg3)->vki_ifr_ifindex) );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNAME)", arg3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGMIIPHY:         /* get hardware entry           */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMIIPHY)",
                     (Addr)((struct vki_ifreq *)arg3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMIIPHY)", arg3, 
		     sizeof(struct vki_ifreq));
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
      PRE_MEM_WRITE( "ioctl(SIOCGSTAMP)", arg3, 
		     sizeof(struct vki_timeval));
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case VKI_SIOCOUTQ:
      PRE_MEM_WRITE( "ioctl(SIOCOUTQ)", arg3, 
		     sizeof(int));
      break;
   case VKI_SIOCGRARP:           /* get RARP table entry         */
   case VKI_SIOCGARP:            /* get ARP table entry          */
      PRE_MEM_WRITE( "ioctl(SIOCGARP)", arg3, 
		     sizeof(struct vki_arpreq));
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
#           if defined(SNDCTL_DSP_GETSPDIF)
   case VKI_SNDCTL_DSP_GETSPDIF:
#           endif
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
#           if defined(SNDCTL_DSP_GETCHANNELMASK)
   case VKI_SNDCTL_DSP_GETCHANNELMASK:
#           endif
#           if defined(SNDCTL_DSP_BIND_CHANNEL)
   case VKI_SNDCTL_DSP_BIND_CHANNEL:
#           endif
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
      PRE_MEM_READ( "ioctl(RTC_ALM_SET)", arg3,
		     sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_IRQP_READ:
      PRE_MEM_WRITE( "ioctl(RTC_IRQP_READ)", arg3,
		     sizeof(unsigned long));
      break;

#           ifdef BLKGETSIZE
   case BLKGETSIZE:
      PRE_MEM_WRITE( "ioctl(BLKGETSIZE)", arg3,
		     sizeof(unsigned long));
      break;
#           endif /* BLKGETSIZE */

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
      PRE_MEM_READ( 
		     "ioctl(CDROMSUBCHNL (cdsc_format, char))",
		     (Addr) &(((struct vki_cdrom_subchnl*) arg3)->cdsc_format),
		     sizeof(((struct vki_cdrom_subchnl*) arg3)->cdsc_format));
      PRE_MEM_WRITE( 
		     "ioctl(CDROMSUBCHNL)", arg3, 
		     sizeof(struct vki_cdrom_subchnl));
      break;
   case VKI_CDROMREADMODE2:
      PRE_MEM_READ( "ioctl(CDROMREADMODE2)", arg3,
                     VKI_CD_FRAMESIZE_RAW0 );
      break;
   case VKI_CDROMREADTOCHDR:
      PRE_MEM_WRITE( 
		     "ioctl(CDROMREADTOCHDR)", arg3, 
		     sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMREADTOCENTRY:
      PRE_MEM_READ( 
		     "ioctl(CDROMREADTOCENTRY (cdte_format, char))",
		     (Addr) &(((struct vki_cdrom_tocentry*) arg3)->cdte_format),
		     sizeof(((struct vki_cdrom_tocentry*) arg3)->cdte_format));
      PRE_MEM_READ( 
		     "ioctl(CDROMREADTOCENTRY (cdte_track, char))",
		     (Addr) &(((struct vki_cdrom_tocentry*) arg3)->cdte_track), 
		     sizeof(((struct vki_cdrom_tocentry*) arg3)->cdte_track));
      PRE_MEM_WRITE( 
		     "ioctl(CDROMREADTOCENTRY)", arg3, 
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
      PRE_MEM_READ( "ioctl(CDROMPLAYMSF)", arg3, 
		     sizeof(struct vki_cdrom_msf));
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case VKI_CDROM_DRIVE_STATUS: /* 0x5326 */
   case VKI_CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

   case VKI_FIGETBSZ:
      PRE_MEM_WRITE( "ioctl(FIGETBSZ)", arg3,
                     sizeof(unsigned long));
      break;
   case VKI_FIBMAP:
      PRE_MEM_READ( "ioctl(FIBMAP)", arg3,
                     sizeof(unsigned long));
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
      PRE_MEM_READ( "ioctl(PPSETMODE)", arg3, sizeof(int) );
      break;
   case VKI_PPGETMODE:
      PRE_MEM_WRITE( "ioctl(PPGETMODE)", arg3, sizeof(int) );
      break;
   case VKI_PPSETPHASE:
      PRE_MEM_READ( "ioctl(PPSETPHASE)", arg3, sizeof(int) );
      break;
   case VKI_PPGETPHASE:
      PRE_MEM_WRITE( "ioctl(PPGETPHASE)", arg3, sizeof(int) );
      break;
   case VKI_PPGETMODES:
      PRE_MEM_WRITE( "ioctl(PPGETMODES)", arg3, sizeof(unsigned int) );
      break;
   case VKI_PPSETFLAGS:
      PRE_MEM_READ( "ioctl(PPSETFLAGS)", arg3, sizeof(int) );
      break;
   case VKI_PPGETFLAGS:
      PRE_MEM_WRITE( "ioctl(PPGETFLAGS)", arg3, sizeof(int) );
      break;
   case VKI_PPRSTATUS:
      PRE_MEM_WRITE( "ioctl(PPRSTATUS)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPRDATA:
      PRE_MEM_WRITE( "ioctl(PPRDATA)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPRCONTROL:
      PRE_MEM_WRITE( "ioctl(PPRCONTROL)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPWDATA:
      PRE_MEM_READ( "ioctl(PPWDATA)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPWCONTROL:
      PRE_MEM_READ( "ioctl(PPWCONTROL)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPFCONTROL:
      PRE_MEM_READ( "ioctl(PPFCONTROL)", arg3, 2 * sizeof(unsigned char) );
      break;
   case VKI_PPDATADIR:
      PRE_MEM_READ( "ioctl(PPDATADIR)", arg3, sizeof(int) );
      break;
   case VKI_PPNEGOT:
      PRE_MEM_READ( "ioctl(PPNEGOT)", arg3, sizeof(int) );
      break;
   case VKI_PPWCTLONIRQ:
      PRE_MEM_READ( "ioctl(PPWCTLONIRQ)", arg3, sizeof(unsigned char) );
      break;
   case VKI_PPCLRIRQ:
      PRE_MEM_WRITE( "ioctl(PPCLRIRQ)", arg3, sizeof(int) );
      break;
   case VKI_PPSETTIME:
      PRE_MEM_READ( "ioctl(PPSETTIME)", arg3, sizeof(struct vki_timeval) );
      break;
   case VKI_PPGETTIME:
      PRE_MEM_WRITE( "ioctl(PPGETTIME)", arg3, sizeof(struct vki_timeval) );
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

POST(ioctl)
{
   /* int ioctl(int d, int request, ...)
      [The  "third"  argument  is traditionally char *argp, 
      and will be so named for this discussion.]
   */
   /*
     VG_(message)(
     Vg_DebugMsg, 
     "is an IOCTL,  request = 0x%x,   d = %d,   argp = 0x%x", 
     arg2,arg1,arg3);
   */
   PRINT("ioctl ( %d, 0x%x, %p )",arg1,arg2,arg3);
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
      /* In early 2.4 kernels, ISDN_MAX_CHANNELS was only defined
       * when KERNEL was. I never saw a larger value than 64 though */
#              ifndef ISDN_MAX_CHANNELS
#              define ISDN_MAX_CHANNELS 64
#              endif
      POST_MEM_WRITE( arg3, ISDN_MAX_CHANNELS * 2 * sizeof(unsigned long) );
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
#           if defined(SNDCTL_DSP_GETCHANNELMASK)
   case VKI_SNDCTL_DSP_GETCHANNELMASK:
#           endif
#           if defined(SNDCTL_DSP_BIND_CHANNEL)
   case VKI_SNDCTL_DSP_BIND_CHANNEL:
#           endif
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

#           ifdef BLKGETSIZE
   case BLKGETSIZE:
      VG_TRACK( post_mem_write,arg3, sizeof(unsigned long));
      break;
#           endif /* BLKGETSIZE */

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

PRE(kill)
{
   /* int kill(pid_t pid, int sig); */
   PRINT("kill ( %d, %d )", arg1,arg2);
   if (arg2 == VKI_SIGVGINT || arg2 == VKI_SIGVGKILL)
      set_result( -VKI_EINVAL );
}

POST(kill)
{
   /* If this was a self-kill then wait for a signal to be
      delivered to any thread before claiming the kill is done. */
   if (res >= 0 &&					/* if it was successful */
       arg2 != 0 &&					/* if a real signal */
       !VG_(is_sig_ign)(arg2) &&			/* that isn't ignored and */
       !VG_(sigismember)(&tst->eff_sig_mask, arg2) &&	/*      we're not blocking it */
       (arg1 == VG_(getpid)() ||			/* directed at us or */
	arg1 == -1	      ||			/* directed at everyone or */
	arg1 == 0	      ||			/* directed at whole group or */
	-arg1 == VG_(getpgrp)())) {			/* directed at our group... */
      /* ...then wait for that signal to be delivered to someone
	 (might be us, might be someone else who doesn't have it
	 blocked) */
      VG_(proxy_waitsig)();
   }
}

PRE(link)
{
   /* int link(const char *oldpath, const char *newpath); */
   PRINT("link ( %p, %p)", arg1, arg2);
   PRE_MEM_RASCIIZ( "link(oldpath)", arg1);
   PRE_MEM_RASCIIZ( "link(newpath)", arg2);
}

PRE(lseek)
{
   /* off_t lseek(int fildes, off_t offset, int whence); */
   PRINT("lseek ( %d, %d, %d )",arg1,arg2,arg3);
}

PRE(_llseek)
{
   /* int _llseek(unsigned int fd, unsigned long offset_high,       
      unsigned long  offset_low, 
      loff_t * result, unsigned int whence); */
   PRINT("llseek ( %d, 0x%x, 0x%x, %p, %d )", arg1,arg2,arg3,arg4,arg5);
   PRE_MEM_WRITE( "llseek(result)", arg4, sizeof(vki_loff_t));
}

POST(_llseek)
{
   if (res == 0)
      POST_MEM_WRITE( arg4, sizeof(vki_loff_t) );
}

PRE(lstat)
{
   /* int lstat(const char *file_name, struct stat *buf); */
   PRINT("lstat ( %p(%s), %p )",arg1,arg1,arg2);
   PRE_MEM_RASCIIZ( "lstat(file_name)", arg1 );
   PRE_MEM_WRITE( "lstat(buf)", arg2, sizeof(struct vki_stat) );
}

POST(lstat)
{
   if (res == 0) {
      POST_MEM_WRITE( arg2, sizeof(struct vki_stat) );
   }
}

PRE(lstat64)
{
   /* int lstat64(const char *file_name, struct stat64 *buf); */
   PRINT("lstat64 ( %p(%s), %p )",arg1,arg1,arg2);
   PRE_MEM_RASCIIZ( "lstat64(file_name)", arg1 );
   PRE_MEM_WRITE( "lstat64(buf)", arg2, sizeof(struct vki_stat64) );
}

POST(lstat64)
{
   if (res == 0) {
      POST_MEM_WRITE( arg2, sizeof(struct vki_stat64) );
   }
}

PRE(mkdir)
{
   /* int mkdir(const char *pathname, mode_t mode); */
   PRINT("mkdir ( %p, %d )", arg1,arg2);
   PRE_MEM_RASCIIZ( "mkdir(pathname)", arg1 );
}

PRE(mmap2)
{
   // Exactly like __NR_mmap except:
   //  - all 6 args are passed in regs, rather than in a memory-block.
   //  - the file offset is specified in pagesize units rather than bytes,
   //    so that it can be used for files bigger than 2^32 bytes.
   /* void* mmap2(void *start, size_t length, int prot, 
                  int flags, int fd, off_t offset); 
   */
   PRINT("mmap2 ( %p, %llu, %d, %d, %d, %d )",
		arg1, (ULong)arg2, arg3, arg4, arg5, arg6 );

   if (arg4 & VKI_MAP_FIXED) {
      if (!valid_client_addr(arg1, arg2, tid, "mmap2"))
	 set_result( -VKI_ENOMEM );
   } else {
      arg1 = VG_(find_map_space)(arg1, arg2, True);
      if (arg1 == 0)
	 set_result( -VKI_ENOMEM );
      else 
         arg4 |= VKI_MAP_FIXED;
   }
}

POST(mmap2)
{
   vg_assert(valid_client_addr(res, arg2, tid, "mmap2"));
   mmap_segment( (Addr)res, arg2, arg3, arg4, arg5, arg6 * (ULong)VKI_PAGE_SIZE );
}

PRE(mmap)
{
   /* void* mmap(void *start, size_t length, int prot, 
      int flags, int fd, off_t offset); 
   */

   UInt a1, a2, a3, a4, a5, a6;

   vg_assert(tid = tst->tid);
   PLATFORM_GET_MMAP_ARGS(tst, a1, a2, a3, a4, a5, a6);

   PRINT("mmap ( %p, %llu, %d, %d, %d, %d )",
		a1, (ULong)a2, a3, a4, a5, a6 );

   if (a4 & VKI_MAP_FIXED) {
      if (!valid_client_addr(a1, a2, tid, "mmap")) {
	 PRINT("mmap failing: %p-%p\n", a1, a1+a2);
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
         vg_assert(valid_client_addr(res, a2, tid, "mmap"));
         mmap_segment( (Addr)res, a2, a3, a4, a5, a6 );
      }
   }
}

PRE(mprotect)
{
   /* int mprotect(const void *addr, size_t len, int prot); */
   /* should addr .. addr+len-1 be checked before the call? */
   PRINT("mprotect ( %p, %llu, %d )", arg1,(ULong)arg2,arg3);

   if (!valid_client_addr(arg1, arg2, tid, "mprotect"))
      set_result( -VKI_ENOMEM );
}

POST(mprotect)
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

PRE(munmap)
{
   /* int munmap(void *start, size_t length); */
   /* should start .. start+length-1 be checked before the call? */
   PRINT("munmap ( %p, %llu )", arg1,(ULong)arg2);

   if (!valid_client_addr(arg1, arg2, tid, "munmap"))
      set_result( -VKI_EINVAL );
}

POST(munmap)
{
   Addr  a   = arg1;
   SizeT len = arg2;

   mash_addr_and_len(&a, &len);
   VG_(unmap_range)(a, len);
   VG_TRACK( die_mem_munmap, a, len );
}

PRE(mincore)
{
   /* int mincore(void *start, size_t length, unsigned char *vec); */
   PRINT("mincore ( %p, %llu, %p )", arg1,(ULong)arg2,arg3);
   PRE_MEM_WRITE( "mincore(vec)", arg3, (arg2 + 4096 - 1) / 4096);
}

POST(mincore)
{
   POST_MEM_WRITE( arg3, (arg2 + 4096 - 1) / 4096 );  
}

PRE(nanosleep)
{
   /* int nanosleep(const struct timespec *req, struct timespec *rem); */
   PRINT("nanosleep ( %p, %p )", arg1,arg2);
   PRE_MEM_READ( "nanosleep(req)", arg1, sizeof(struct vki_timespec) );
   if (arg2 != (UWord)NULL)
      PRE_MEM_WRITE( "nanosleep(rem)", arg2, sizeof(struct vki_timespec) );
}

POST(nanosleep)
{
   if (arg2 != (UWord)NULL && res == -VKI_EINTR)
      POST_MEM_WRITE( arg2, sizeof(struct vki_timespec) );
}

PRE(_newselect)
{
   /* int select(int n,  
		 fd_set *readfds, fd_set *writefds, fd_set *exceptfds, 
		 struct timeval *timeout);
   */
   PRINT("newselect ( %d, %p, %p, %p, %p )", arg1,arg2,arg3,arg4,arg5);
   if (arg2 != 0)
      PRE_MEM_READ( "newselect(readfds)",   
		     arg2, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg3 != 0)
      PRE_MEM_READ( "newselect(writefds)",  
		     arg3, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg4 != 0)
      PRE_MEM_READ( "newselect(exceptfds)", 
		     arg4, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg5 != 0)
      PRE_MEM_READ( "newselect(timeout)", arg5, sizeof(struct vki_timeval) );
}

PRE(open)
{
   /* int open(const char *pathname, int flags, mode_t mode); */
   if (arg2 & VKI_O_CREAT) {
      /* int open(const char *pathname, int flags, mode_t mode); */
      PRINT("open ( %p(%s), %d, %d )",arg1,arg1,arg2,arg3);
   } else {
      /* int open(const char *pathname, int flags); */
      PRINT("open ( %p(%s), %d )",arg1,arg1,arg2);
   }
   PRE_MEM_RASCIIZ( "open(pathname)", arg1 );
}

POST(open)
{
   if (!fd_allowed(res, "open", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
}

PRE(read)
{
   /* size_t read(int fd, void *buf, size_t count); */
   PRINT("read ( %d, %p, %llu )", arg1, arg2, (ULong)arg3);

   if (!fd_allowed(arg1, "read", tid, False))
      set_result( -VKI_EBADF );
   else
      PRE_MEM_WRITE( "read(buf)", arg2, arg3 );
}

POST(read)
{
   POST_MEM_WRITE( arg2, res );
}

PRE(write)
{
   /* size_t write(int fd, const void *buf, size_t count); */
   PRINT("write ( %d, %p, %llu )", arg1, arg2, (ULong)arg3);
   if (!fd_allowed(arg1, "write", tid, False))
      set_result( -VKI_EBADF );
   else
      PRE_MEM_READ( "write(buf)", arg2, arg3 );
}

PRE(creat)
{
   /* int creat(const char *pathname, mode_t mode); */
   PRINT("creat ( %p(%s), %d ) --> ",arg1,arg1,arg2);
   PRE_MEM_RASCIIZ( "creat(pathname)", arg1 );
}

POST(creat)
{
   if (!fd_allowed(res, "creat", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
   PRINT("%d\n",res);
}

PRE(pipe)
{
   /* int pipe(int filedes[2]); */
   PRINT("pipe ( %p )", arg1);
   PRE_MEM_WRITE( "pipe(filedes)", arg1, 2*sizeof(int) );
}

POST(pipe)
{
   Int *p = (Int *)arg1;

   if (!fd_allowed(p[0], "pipe", tid, True) ||
       !fd_allowed(p[1], "pipe", tid, True)) {
      VG_(close)(p[0]);
      VG_(close)(p[1]);
      set_result( -VKI_EMFILE );
   } else {
      POST_MEM_WRITE( arg1, 2*sizeof(int) );
      if (VG_(clo_track_fds)) {
         record_fd_open(tid, p[0], NULL);
         record_fd_open(tid, p[1], NULL);
      }
   }
}

PRE(poll)
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
   PRINT("poll ( %p, %d, %d )\n",arg1,arg2,arg3);
                     
   for (i = 0; i < arg2; i++) {
      // 'fd' and 'events' field are inputs;  'revents' is output.
      // XXX: this is x86 specific -- the pollfd struct varies across
      // different architectures.
      PRE_MEM_READ( "poll(ufds)",
                    (Addr)(&ufds[i]), sizeof(int) + sizeof(short) );
      PRE_MEM_WRITE( "poll(ufds)", (Addr)(&ufds[i].revents), sizeof(short) );
   }  
}

POST(poll)
{
   if (res > 0) {
      UInt i;
      struct vki_pollfd* ufds = (struct vki_pollfd *)arg1;
      // XXX: again, this is x86-specific
      for (i = 0; i < arg2; i++)
	 POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(Short) );
   }
}

PRE(epoll_create)
{
   /* int epoll_create(int size) */
   PRINT("epoll_create ( %d )", arg1);
}

POST(epoll_create)
{
   if (!fd_allowed(res, "open", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         record_fd_open (tid, res, NULL);
   }
}

PRE(epoll_ctl)
{
   /* int epoll_ctl(int epfd, int op, int fd, struct epoll_event *event) */
   static const char* epoll_ctl_s[3] = {
      "EPOLL_CTL_ADD",
      "EPOLL_CTL_DEL",
      "EPOLL_CTL_MOD"
   };
   PRINT("epoll_ctl ( %d, %s, %d, %p )", 
                arg1, ( arg2<3 ? epoll_ctl_s[arg2] : "?" ), arg3, arg4);
   PRE_MEM_READ( "epoll_ctl(event)", arg4, sizeof(struct epoll_event) );
}

PRE(epoll_wait)
{
   /* int epoll_wait(int epfd, struct epoll_event * events, 
                     int maxevents, int timeout) */
   PRINT("epoll_wait ( %d, %p, %d, %d )", arg1, arg2, arg3, arg4);
   PRE_MEM_WRITE( "epoll_wait(events)", arg2, sizeof(struct epoll_event)*arg3);
}

POST(epoll_wait)
{
   if (res > 0)
      POST_MEM_WRITE( arg2, sizeof(struct epoll_event)*res ) ;
}

PRE(readlink)
{
   /* int readlink(const char *path, char *buf, size_t bufsiz); */
   PRINT("readlink ( %p, %p, %llu )", arg1,arg2,(ULong)arg3);
   PRE_MEM_RASCIIZ( "readlink(path)", arg1 );
   PRE_MEM_WRITE( "readlink(buf)", arg2,arg3 );
}

POST(readlink)
{
   POST_MEM_WRITE( arg2, res );
}

PRE(readv)
{
   /* int readv(int fd, const struct iovec * vector, size_t count); */
   Int i;
   struct vki_iovec * vec;
   PRINT("readv ( %d, %p, %llu )",arg1,arg2,(ULong)arg3);
   if (!fd_allowed(arg1, "readv", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      PRE_MEM_READ( "readv(vector)", arg2, arg3 * sizeof(struct vki_iovec) );
      /* ToDo: don't do any of the following if the vector is invalid */
      vec = (struct vki_iovec *)arg2;
      for (i = 0; i < (Int)arg3; i++)
	 PRE_MEM_WRITE( "readv(vector[...])",
			(Addr)vec[i].iov_base, vec[i].iov_len );
   }
}

POST(readv)
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

PRE(rename)
{
   /* int rename(const char *oldpath, const char *newpath); */
   PRINT("rename ( %p, %p )", arg1, arg2 );
   PRE_MEM_RASCIIZ( "rename(oldpath)", arg1 );
   PRE_MEM_RASCIIZ( "rename(newpath)", arg2 );
}

PRE(rmdir)
{
   /* int rmdir(const char *pathname); */
   PRINT("rmdir ( %p )", arg1);
   PRE_MEM_RASCIIZ( "rmdir(pathname)", arg1 );
}

PRE(sched_setparam)
{
   /* int sched_setparam(pid_t pid, const struct sched_param *p); */
   PRINT("sched_setparam ( %d, %p )", arg1, arg2 );
   PRE_MEM_READ( "sched_setparam(ptr)", arg2, sizeof(struct vki_sched_param) );
}

POST(sched_setparam)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_sched_param) );
}

PRE(sched_getparam)
{
   /* int sched_getparam(pid_t pid, struct sched_param *p); */
   PRINT("sched_getparam ( %d, %p )", arg1, arg2 );
   PRE_MEM_WRITE( "sched_getparam(ptr)",
		  arg2, sizeof(struct vki_sched_param) );
}

POST(sched_getparam)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_sched_param) );
}

PRE(sched_yield)
{
   /* int sched_yield(void); */
   PRINT("sched_yield ()" );
}

PRE(select)
{
   /* struct sel_arg_struct {
      unsigned long n;
      fd_set *inp, *outp, *exp;
      struct timeval *tvp;
      };
      int old_select(struct sel_arg_struct *arg);
   */
   PRE_MEM_READ( "select(args)", arg1, 5*sizeof(UInt) );

   {
      UInt* arg_struct = (UInt*)arg1;
      UInt a1, a2, a3, a4, a5;

      a1 = arg_struct[0];
      a2 = arg_struct[1];
      a3 = arg_struct[2];
      a4 = arg_struct[3];
      a5 = arg_struct[4];

      PRINT("select ( %d, %p, %p, %p, %p )", a1,a2,a3,a4,a5);
      if (a2 != (Addr)NULL)
	 PRE_MEM_READ( "select(readfds)",   a2, a1/8 /* __FD_SETSIZE/8 */ );
      if (a3 != (Addr)NULL)
	 PRE_MEM_READ( "select(writefds)",  a3, a1/8 /* __FD_SETSIZE/8 */ );
      if (a4 != (Addr)NULL)
	 PRE_MEM_READ( "select(exceptfds)", a4, a1/8 /* __FD_SETSIZE/8 */ );
      if (a5 != (Addr)NULL)
	 PRE_MEM_READ( "select(timeout)", a5, sizeof(struct vki_timeval) );
   }
}

PRE(setitimer)
{
   /* setitimer(int which, const struct itimerval *value,
                           struct itimerval *ovalue); */
   PRINT("setitimer ( %d, %p, %p )", arg1,arg2,arg3);
   if (arg2 != (Addr)NULL)
      PRE_MEM_READ( "setitimer(value)", arg2, sizeof(struct vki_itimerval) );
   if (arg3 != (Addr)NULL)
      PRE_MEM_WRITE( "setitimer(ovalue)", arg3, sizeof(struct vki_itimerval));
}

POST(setitimer)
{
   if (arg3 != (Addr)NULL) {
      VG_TRACK( post_mem_write,arg3, sizeof(struct vki_itimerval));
   }
}

PRE(setfsgid32)
{
   /* int setfsgid(uid_t fsgid); */
   PRINT("setfsgid ( %d )", arg1);
}

PRE(setgid)
{
   /* int setgid(gid_t gid); */
   PRINT("setgid ( %d )", arg1);
}

PREALIAS(setgid32, setgid);

PRE(setsid)
{
   /* pid_t setsid(void); */
   PRINT("setsid ()");
}

PRE(setgroups)
{
   /* int setgroups(size_t size, const gid_t *list); */
   PRINT("setgroups ( %llu, %p )", (ULong)arg1, arg2);
   if (arg1 > 0)
      PRE_MEM_READ( "setgroups(list)", arg2, arg1 * sizeof(vki_gid_t) );
}

PREALIAS(setgroups32, setgroups);

PRE(setpgid)
{
   /* int setpgid(pid_t pid, pid_t pgid); */
   PRINT("setpgid ( %d, %d )", arg1, arg2);
}

POST(setpgid)
{
   VG_(main_pgrp) = VG_(getpgrp)();
}

PRE(setregid32)
{
   /* int setregid(gid_t rgid, gid_t egid); */
   PRINT("setregid32(?) ( %d, %d )", arg1, arg2);
}

PRE(setresuid32)
{
   /* int setresuid(uid_t ruid, uid_t euid, uid_t suid); */
   PRINT("setresuid32(?) ( %d, %d, %d )", arg1, arg2, arg3);
}

PRE(setreuid)
{
   /* int setreuid(uid_t ruid, uid_t euid); */
   PRINT("setreuid ( 0x%x, 0x%x )", arg1, arg2);
}

PREALIAS(setreuid32, setreuid);

PRE(setrlimit)
{
   /* int setrlimit (int resource, const struct rlimit *rlim); */
   PRINT("setrlimit ( %d, %p )", arg1,arg2);
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

PRE(settimeofday)
{
   /* int settimeofday(const struct timeval *tv, const struct timezone *tz); */
   PRINT("settimeofday ( %p, %p )",arg1,arg2);
   PRE_MEM_READ( "settimeofday(tv)", arg1, sizeof(struct vki_timeval) );
   if (arg2 != 0) {
      PRE_MEM_READ( "settimeofday(tz)", arg2, sizeof(struct vki_timezone) );
      /* maybe should warn if tz->tz_dsttime is non-zero? */
   }
}

PRE(setuid)
{
   /* int setuid(uid_t uid); */
   PRINT("setuid ( %d )", arg1);
}

PREALIAS(setuid32, setuid);

PRE(socketcall)
{
   /* int socketcall(int call, unsigned long *args); */
   PRINT("socketcall ( %d, %p )",arg1,arg2);
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

POST(socketcall)
{
   /* int socketcall(int call, unsigned long *args); */
   PRINT("socketcall ( %d, %p )",arg1,arg2);

   switch (arg1 /* request */) {

   case VKI_SYS_SOCKETPAIR: {
      Int fd1 = ((Int*)((UWord*)arg2)[3])[0];
      Int fd2 = ((Int*)((UWord*)arg2)[3])[1];
      POST_MEM_WRITE( ((UWord*)arg2)[3], 2*sizeof(int) );
      if (!fd_allowed(fd1, "socketcall.socketpair", tid, True) ||
          !fd_allowed(fd2, "socketcall.socketpair", tid, True)) {
         VG_(close)(fd1);
         VG_(close)(fd2);
         set_result( -VKI_EMFILE );
      } else {
         POST_MEM_WRITE( ((UWord*)arg2)[3], 2*sizeof(int) );
         if (VG_(clo_track_fds)) {
            record_fd_open(tid, fd1, NULL);
            record_fd_open(tid, fd2, NULL);
         }
      }
      break;
   }

   case VKI_SYS_SOCKET:
      if (!fd_allowed(res, "socket", tid, True)) {
	 VG_(close)(res);
	 set_result( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            record_fd_open(tid, res, NULL);
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
      if (!fd_allowed(res, "accept", tid, True)) {
	 VG_(close)(res);
	 set_result( -VKI_EMFILE );
      } else {
	 Addr addr_p     = ((UWord*)arg2)[1];
	 Addr addrlen_p  = ((UWord*)arg2)[2];

	 if (addr_p != (Addr)NULL) 
	    buf_and_len_post_check ( tid, res, addr_p, addrlen_p,
				     "socketcall.accept(addrlen_out)" );
         if (VG_(clo_track_fds))
            record_fd_open(tid, res, NULL);
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
      if (res >= 0 
	  && ((UWord*)arg2)[1] != (UWord)NULL) {
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

PRE(stat)
{
   /* int stat(const char *file_name, struct stat *buf); */
   PRINT("stat ( %p, %p )",arg1,arg2);
   PRE_MEM_RASCIIZ( "stat(file_name)", arg1 );
   PRE_MEM_WRITE( "stat(buf)", arg2, sizeof(struct vki_stat) );
}

POST(stat)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat) );
}

PRE(statfs)
{
   /* int statfs(const char *path, struct statfs *buf); */
   PRINT("statfs ( %p, %p )",arg1,arg2);
   PRE_MEM_RASCIIZ( "statfs(path)", arg1 );
   PRE_MEM_WRITE( "statfs(buf)", arg2, sizeof(struct vki_statfs) );
}

POST(statfs)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_statfs) );
}

PRE(statfs64)
{
   /* int statfs64(const char *path, size_t sz, struct statfs64 *buf); */
   PRINT("statfs64 ( %p, %llu, %p )",arg1,(ULong)arg2,arg3);
   PRE_MEM_RASCIIZ( "statfs64(path)", arg1 );
   PRE_MEM_WRITE( "statfs64(buf)", arg3, arg2 );
}

POST(statfs64)
{
   POST_MEM_WRITE( arg3, arg2 );
}

PRE(symlink)
{
   /* int symlink(const char *oldpath, const char *newpath); */
   PRINT("symlink ( %p, %p )",arg1,arg2);
   PRE_MEM_RASCIIZ( "symlink(oldpath)", arg1 );
   PRE_MEM_RASCIIZ( "symlink(newpath)", arg2 );
}

PRE(stat64)
{
   /* int stat64(const char *file_name, struct stat64 *buf); */
   PRINT("stat64 ( %p, %p )",arg1,arg2);
   PRE_MEM_RASCIIZ( "stat64(file_name)", arg1 );
   PRE_MEM_WRITE( "stat64(buf)", arg2, sizeof(struct vki_stat64) );
}

POST(stat64)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat64) );
}

PRE(fstat64)
{
   /* int fstat64(int filedes, struct stat64 *buf); */
   PRINT("fstat64 ( %d, %p )",arg1,arg2);
   PRE_MEM_WRITE( "fstat64(buf)", arg2, sizeof(struct vki_stat64) );
}

POST(fstat64)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_stat64) );
}

PRE(sysinfo)
{
   /* int sysinfo(struct sysinfo *info); */
   PRINT("sysinfo ( %p )",arg1);
   PRE_MEM_WRITE( "sysinfo(info)", arg1, sizeof(struct vki_sysinfo) );
}

POST(sysinfo)
{
   POST_MEM_WRITE( arg1, sizeof(struct vki_sysinfo) );
}

PRE(time)
{
   /* time_t time(time_t *t); */
   PRINT("time ( %p )",arg1);
   if (arg1 != (UWord)NULL) {
      PRE_MEM_WRITE( "time", arg1, sizeof(vki_time_t) );
   }
}

POST(time)
{
   if (arg1 != (UWord)NULL) {
      POST_MEM_WRITE( arg1, sizeof(vki_time_t) );
   }
}

PRE(times)
{
   /* clock_t times(struct tms *buf); */
   PRINT("times ( %p )",arg1);
   PRE_MEM_WRITE( "times(buf)", arg1, sizeof(struct vki_tms) );
}

POST(times)
{
   if (arg1 != (UWord)NULL) {
      POST_MEM_WRITE( arg1, sizeof(struct vki_tms) );
   }
}

PRE(truncate)
{
   /* int truncate(const char *path, off_t length); */
   PRINT("truncate ( %p(%s), %d )", arg1,arg1,arg2);
   PRE_MEM_RASCIIZ( "truncate(path)", arg1 );
}

PRE(umask)
{
   /* mode_t umask(mode_t mask); */
   PRINT("umask ( %d )", arg1);
}

PRE(unlink)
{
   /* int unlink(const char *pathname) */
   PRINT("unlink ( %p(%s) )",arg1, arg1);
   PRE_MEM_RASCIIZ( "unlink(pathname)", arg1 );
}

PRE(uname)
{
   /* int uname(struct utsname *buf); */
   PRINT("uname ( %p )",arg1);
   PRE_MEM_WRITE( "uname(buf)", arg1, sizeof(struct vki_new_utsname) );
}

POST(uname)
{
   if (arg1 != (UWord)NULL) {
      POST_MEM_WRITE( arg1, sizeof(struct vki_new_utsname) );
   }
}

PRE(utime)
{
   /* int utime(const char *filename, struct utimbuf *buf); */
   PRINT("utime ( %p, %p )", arg1,arg2);
   PRE_MEM_RASCIIZ( "utime(filename)", arg1 );
   if (arg2 != (UWord)NULL)
      PRE_MEM_READ( "utime(buf)", arg2, sizeof(struct vki_utimbuf) );
}

PRE(waitpid)
{
   /* pid_t waitpid(pid_t pid, int *status, int options); */

   PRINT("waitpid ( %d, %p, %d )", arg1,arg2,arg3);
   if (arg2 != (Addr)NULL)
      PRE_MEM_WRITE( "waitpid(status)", arg2, sizeof(int) );
}

POST(waitpid)
{
   if (arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, sizeof(int) );
}

PRE(wait4)
{
   /* pid_t wait4(pid_t pid, int *status, int options,
      struct rusage *rusage) */
   PRINT("wait4 ( %d, %p, %d, %p )", arg1,arg2,arg3,arg4);
   arg3 &= ~(__VKI_WCLONE | __VKI_WALL);

   if (arg2 != (Addr)NULL)
      PRE_MEM_WRITE( "wait4(status)", arg2, sizeof(int) );
   if (arg4 != (Addr)NULL)
      PRE_MEM_WRITE( "wait4(rusage)", arg4, sizeof(struct vki_rusage) );
}

POST(wait4)
{
   if (arg2 != (Addr)NULL)
      POST_MEM_WRITE( arg2, sizeof(int) );
   if (arg4 != (Addr)NULL)
      POST_MEM_WRITE( arg4, sizeof(struct vki_rusage) );
}

PRE(writev)
{
   /* int writev(int fd, const struct iovec * vector, size_t count); */
   Int i;
   struct vki_iovec * vec;
   PRINT("writev ( %d, %p, %llu )",arg1,arg2,(ULong)arg3);
   if (!fd_allowed(arg1, "writev", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      PRE_MEM_READ( "writev(vector)", 
		     arg2, arg3 * sizeof(struct vki_iovec) );
      /* ToDo: don't do any of the following if the vector is invalid */
      vec = (struct vki_iovec *)arg2;
      for (i = 0; i < (Int)arg3; i++)
	 PRE_MEM_READ( "writev(vector[...])",
			(Addr)vec[i].iov_base, vec[i].iov_len );
   }
}

PRE(prctl)
{
   /* int prctl(int option, unsigned long arg2, unsigned long arg3,
      unsigned long arg4, unsigned long arg5); */
   PRINT( "prctl ( %d, %d, %d, %d, %d )", arg1, arg2, arg3, arg4, arg5 );
}

PRE(adjtimex)
{
   struct vki_timex *tx = (struct vki_timex *)arg1;
   PRINT("adjtimex ( %p )", arg1);

   PRE_MEM_READ( "adjtimex(timex->modes)", arg1, sizeof(tx->modes));

#define ADJX(bit,field) 				\
   if (tx->modes & bit)					\
      PRE_MEM_READ( "adjtimex(timex->"#field")",	\
		    (Addr)&tx->field, sizeof(tx->field))
   ADJX(ADJ_FREQUENCY, freq);
   ADJX(ADJ_MAXERROR, maxerror);
   ADJX(ADJ_ESTERROR, esterror);
   ADJX(ADJ_STATUS, status);
   ADJX(ADJ_TIMECONST, constant);
   ADJX(ADJ_TICK, tick);
#undef ADJX
   
   PRE_MEM_WRITE( "adjtimex(timex)", arg1, sizeof(struct vki_timex));
}

POST(adjtimex)
{
   VG_TRACK(post_mem_write, arg1, sizeof(struct vki_timex));
}

PRE(utimes)
{
    /* int utimes(const char *filename, struct timeval *tvp); */
    PRINT("utimes ( %p, %p )", arg1,arg2);
    PRE_MEM_RASCIIZ( "utimes(filename)", arg1 );
    if (arg2 != (UWord)NULL)
         PRE_MEM_READ( "utimes(tvp)", arg2, sizeof(struct vki_timeval) );
}

PRE(futex)
{
    /* int futex(void *futex, int op, int val, const struct timespec *timeout); */
    PRINT("futex ( %p, %d, %d, %p, %p )", arg1,arg2,arg3,arg4,arg5);
    PRE_MEM_READ( "futex(futex)", arg1, sizeof(int) );
    if (arg2 == VKI_FUTEX_WAIT && arg4 != (UWord)NULL)
       PRE_MEM_READ( "futex(timeout)", arg4, sizeof(struct vki_timespec) );
    if (arg2 == VKI_FUTEX_REQUEUE)
       PRE_MEM_READ( "futex(futex2)", arg4, sizeof(int) );
}

POST(futex)
{
   POST_MEM_WRITE( arg1, sizeof(int) );
   if (arg2 == VKI_FUTEX_FD) {
      if (!fd_allowed(res, "futex", tid, True)) {
         VG_(close)(res);
         set_result( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            record_fd_open(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
      }
   }
}

PRE(sched_setaffinity)
{
   /* int sched_setaffinity(pid_t pid, unsigned int len, unsigned long *mask) */
   PRINT("sched_setaffinity ( %d, %d, %p )", arg1, arg2, arg3);
   PRE_MEM_READ( "sched_setaffinity(mask)", arg3, arg2);
}

PRE(sched_getaffinity)
{
   /* int sched_setaffinity(pid_t pid, unsigned int len, unsigned long *mask) */
   PRINT("sched_getaffinity ( %d, %d, %p )", arg1, arg2, arg3);
   PRE_MEM_WRITE( "sched_getaffinity(mask)", arg3, arg2);
}

POST(sched_getaffinity)
{
   VG_TRACK(post_mem_write, arg3, arg2);
}

PRE(acct)
{
   /* int acct(const char *filename); */
   PRINT("acct ( %p )", arg1);
   PRE_MEM_RASCIIZ( "acct(filename)", arg1 );
}

#define SIGNAL_SIMULATION	1

PRE(pause)
{
   /* int pause(void); */
   PRINT("pause ( )");
}

PRE(rt_sigsuspend)
{
   /* int sigsuspend(const sigset_t *mask); */
   PRINT("sigsuspend ( %p )", arg1 );
   if (arg1 != (Addr)NULL) {
      /* above NULL test is paranoia */
      PRE_MEM_READ( "sigsuspend(mask)", arg1, sizeof(vki_sigset_t) );
   }
}

PREALIAS(sigsuspend, rt_sigsuspend);

PRE(rt_sigtimedwait)
{
   /* int sigtimedwait(const  sigset_t  *set,  siginfo_t  *info,
      const struct timespec timeout); */
   PRINT("sigtimedwait ( %p, %p, timeout )", arg1, arg2);
   if (arg1 != (UWord)NULL) 
      PRE_MEM_READ(  "sigtimedwait(set)",  arg1, sizeof(vki_sigset_t));
   if (arg2 != (UWord)NULL)
      PRE_MEM_WRITE( "sigtimedwait(info)", arg2, sizeof(vki_siginfo_t) );
}

POST(rt_sigtimedwait)
{
   if (arg2 != (UWord)NULL)
      POST_MEM_WRITE( arg2, sizeof(vki_siginfo_t) );
}

PRE(rt_sigqueueinfo)
{
   /*  long sys_rt_sigqueueinfo(int pid, int sig, siginfo_t *uinfo) */
   PRINT("rt_sigqueueinfo(%d, %d, %p)", arg1, arg2, arg3);
   if (arg2 != (UWord)NULL)
      PRE_MEM_READ( "sigqueueinfo(uinfo)", arg3, sizeof(vki_siginfo_t) );
}

POST(rt_sigqueueinfo)
{
   if (res >= 0 && 
       arg2 != 0 &&
       !VG_(is_sig_ign)(arg2) &&
       !VG_(sigismember)(&tst->eff_sig_mask, arg2) &&
       arg1 == VG_(getpid)()) {
      VG_(proxy_waitsig)();
   }
}

PRE(sigaltstack)
{
   /* int sigaltstack(const stack_t *ss, stack_t *oss); */
   PRINT("sigaltstack ( %p, %p )",arg1,arg2);
   if (arg1 != (UWord)NULL) {
      PRE_MEM_READ( "sigaltstack(ss)", arg1, sizeof(vki_stack_t) );
   }
   if (arg2 != (UWord)NULL) {
      PRE_MEM_WRITE( "sigaltstack(oss)", arg2, sizeof(vki_stack_t) );
   }

   if (SIGNAL_SIMULATION)
      VG_(do__NR_sigaltstack) (tid);
}

POST(sigaltstack)
{
   if (res == 0 && arg2 != (UWord)NULL)
      POST_MEM_WRITE( arg2, sizeof(vki_stack_t));
}

PRE(sigaction)
{
   /* int sigaction(int signum, struct k_sigaction *act, 
      struct k_sigaction *oldact); */
   PRINT("sigaction ( %d, %p, %p )",arg1,arg2,arg3);
   if (arg2 != (UWord)NULL)
      PRE_MEM_READ( "sigaction(act)", arg2, sizeof(struct vki_sigaction));
   if (arg3 != (UWord)NULL)
      PRE_MEM_WRITE( "sigaction(oldact)", arg3, sizeof(struct vki_sigaction));

   if (SIGNAL_SIMULATION)
      VG_(do__NR_sigaction)(tid);
}

POST(sigaction)
{
   if (res == 0 && arg3 != (UWord)NULL)
      POST_MEM_WRITE( arg3, sizeof(struct vki_sigaction));
}

PREALIAS(rt_sigaction, sigaction);
POSTALIAS(rt_sigaction, sigaction);

PRE(sigprocmask)
{
   /* int sigprocmask(int how, k_sigset_t *set, 
      k_sigset_t *oldset); */
   PRINT("sigprocmask ( %d, %p, %p )",arg1,arg2,arg3);
   if (arg2 != (UWord)NULL)
      PRE_MEM_READ( "sigprocmask(set)", arg2, sizeof(vki_sigset_t));
   if (arg3 != (UWord)NULL)
      PRE_MEM_WRITE( "sigprocmask(oldset)", arg3, sizeof(vki_sigset_t));

   if (SIGNAL_SIMULATION)
      VG_(do__NR_sigprocmask) ( tid, 
				arg1 /*how*/, 
				(vki_sigset_t*) arg2,
				(vki_sigset_t*) arg3 );
}

POST(sigprocmask)
{
   if (res == 0 && arg3 != (UWord)NULL)
      POST_MEM_WRITE( arg3, sizeof(vki_sigset_t));
}

PREALIAS(rt_sigprocmask, sigprocmask);
POSTALIAS(rt_sigprocmask, sigprocmask);

PRE(sigpending)
{
   /* int sigpending( sigset_t *set ) ; */
   PRINT( "sigpending ( %p )", arg1 );
   PRE_MEM_WRITE( "sigpending(set)", arg1, sizeof(vki_sigset_t));
}

POST(sigpending)
{
   POST_MEM_WRITE( arg1, sizeof( vki_sigset_t ) ) ;
}

PREALIAS(rt_sigpending, sigpending);
POSTALIAS(rt_sigpending, sigpending);

PRE(io_setup)
{
   SizeT size;
   Addr addr;

   /* long io_setup (unsigned nr_events, aio_context_t *ctxp); */
   PRINT("io_setup ( %ul, %p )",arg1,arg2);
   PRE_MEM_WRITE( "io_setup(ctxp)", arg2, sizeof(vki_aio_context_t) );
   
   size = PGROUNDUP(sizeof(struct vki_aio_ring) + arg1 * sizeof(struct vki_io_event));
   addr = VG_(find_map_space)(0, size, True);
   VG_(map_segment)(addr, size, VKI_PROT_READ|VKI_PROT_EXEC, SF_FIXED);
   
   VG_(pad_address_space)();
   set_result( VG_(do_syscall)(SYSNO, arg1, arg2) );
   VG_(unpad_address_space)();

   if (res == 0) {
      struct vki_aio_ring *r = *(struct vki_aio_ring **)arg2;
        
      vg_assert(addr == (Addr)r);
      vg_assert(valid_client_addr(addr, size, tid, "io_setup"));
                
      VG_TRACK( new_mem_mmap, addr, size, True, True, False );
      POST_MEM_WRITE( arg2, sizeof(vki_aio_context_t) );
   }
   else {
      VG_(unmap_range)(addr, size);
   }
}

PRE(io_destroy)
{
   Segment *s = VG_(find_segment)(arg1);
   struct vki_aio_ring *r = *(struct vki_aio_ring **)arg1;
   SizeT size = PGROUNDUP(sizeof(struct vki_aio_ring) + r->nr * sizeof(struct vki_io_event));

   /* long io_destroy (aio_context_t ctx); */
   PRINT("io_destroy ( %ul )",arg1);

   set_result( VG_(do_syscall)(SYSNO, arg1) );
   
   if (res == 0 && s != NULL && VG_(seg_contains)(s, arg1, size)) {
      VG_TRACK( die_mem_munmap, arg1, size );
      VG_(unmap_range)(arg1, size);
   }
}

PRE(io_getevents)
{
   /* long io_getevents (aio_context_t ctx_id, long min_nr, long nr,
                         struct io_event *events, struct timespec *timeout); */
   PRINT("io_getevents ( %ul, %l, %l, %p, %p )",arg1,arg2,arg3,arg4,arg5);
   if (arg3 > 0)
      PRE_MEM_WRITE( "io_getevents(events)",
                     arg4, sizeof(struct vki_io_event)*arg3 );
   if (arg5 != (UWord)NULL)
      PRE_MEM_READ( "io_getevents(timeout)",
                     arg5, sizeof(struct vki_timespec));
}

POST(io_getevents)
{
   int i;

   if (res > 0) {
      POST_MEM_WRITE( arg4, sizeof(struct vki_io_event)*res );
      for (i = 0; i < res; i++) {
         const struct vki_io_event *vev = ((struct vki_io_event *)arg4) + i;
         const struct vki_iocb *cb = (struct vki_iocb *)(Addr)vev->obj;

         switch (cb->aio_lio_opcode) {
         case VKI_IOCB_CMD_PREAD:
            if (vev->result > 0)
               POST_MEM_WRITE( cb->aio_buf, vev->result );
            break;
            
         case VKI_IOCB_CMD_PWRITE:
            break;
           
         default:
            VG_(message)(Vg_DebugMsg,"Warning: unhandled io_getevents opcode: %u\n",cb->aio_lio_opcode);
            break;
         }
      }
   }
}

PRE(io_submit)
{
   int i;

   /* long io_submit (aio_context_t ctx_id, long nr, struct iocb **iocbpp); */
   PRINT("io_submit( %ul, %l, %p )",arg1,arg2,arg3);
   PRE_MEM_READ( "io_submit(iocbpp)", arg3, sizeof(struct vki_iocb *)*arg2 );
   for (i = 0; i < arg2; i++) {
      struct vki_iocb *cb = ((struct vki_iocb **)arg3)[i];
      PRE_MEM_READ( "io_submit(iocb)",
                     (Addr)cb, sizeof(struct vki_iocb) );
      switch (cb->aio_lio_opcode) {
      case VKI_IOCB_CMD_PREAD:
         PRE_MEM_WRITE( "io_submit(PREAD)", cb->aio_buf, cb->aio_nbytes );
         break;

      case VKI_IOCB_CMD_PWRITE:
         PRE_MEM_READ( "io_submit(PWRITE)", cb->aio_buf, cb->aio_nbytes );
         break;
        
      default:
         VG_(message)(Vg_DebugMsg,"Warning: unhandled io_submit opcode: %u\n",cb->aio_lio_opcode);
         break;
      }
   }
}

PRE(io_cancel)
{
   /* long io_cancel (aio_context_t ctx_id, struct iocb *iocb,
                      struct io_event *result); */
   PRINT("io_cancel( %ul, %p, %p )",arg1,arg2,arg3);
   PRE_MEM_READ( "io_cancel(iocb)", arg2, sizeof(struct vki_iocb) );
   PRE_MEM_WRITE( "io_cancel(result)", arg3, sizeof(struct vki_io_event) );
}

POST(io_cancel)
{
   POST_MEM_WRITE( arg3, sizeof(struct vki_io_event) );
}

PRE(mq_open)
{
   /* mqd_t mq_open(const char *name, int oflag, ...); */
   PRINT("mq_open( %p(%s), %d )", arg1,arg1,arg2);
   PRE_MEM_RASCIIZ( "mq_open(name)", arg1 );
   if ((arg2 & VKI_O_CREAT) != 0 && arg4 != 0) {
      const struct vki_mq_attr *attr = (struct vki_mq_attr *)arg4;
      PRE_MEM_READ( "mq_open(attr->mq_maxmsg)",
                     (Addr)&attr->mq_maxmsg, sizeof(attr->mq_maxmsg) );
      PRE_MEM_READ( "mq_open(attr->mq_msgsize)",
                     (Addr)&attr->mq_msgsize, sizeof(attr->mq_msgsize) );
   }
}

POST(mq_open)
{
   if (!fd_allowed(res, "mq_open", tid, True)) {
      VG_(close)(res);
      set_result( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
   PRINT("%d\n",res);
}

PRE(mq_unlink)
{
   /* int mq_unlink(const char *name) */
   PRINT("mq_unlink ( %p(%s) )",arg1, arg1);
   PRE_MEM_RASCIIZ( "mq_unlink(name)", arg1 );
}

PRE(mq_timedsend)
{
   /* int mq_timedsend(mqd_t mqdes, const char *msg_ptr, size_t msg_len,
                       unsigned msg_prio, const struct timespec *abs_timeout); */
   PRINT("mq_timedsend ( %d, %p, %llu, %d, %p )",
                arg1,arg2,(ULong)arg3,arg4,arg5);
   if (!fd_allowed(arg1, "mq_timedsend", tid, False)) {
      set_result( -VKI_EBADF );
   } else {
      PRE_MEM_READ( "mq_timedsend(msg_ptr)", arg2, arg3 );
      if (arg5 != 0)
         PRE_MEM_READ( "mq_timedsend(abs_timeout)", arg5,
                        sizeof(struct vki_timespec) );
   }
}

PRE(mq_timedreceive)
{
   /* ssize_t mq_timedreceive(mqd_t mqdes, char *restrict msg_ptr,
                              size_t msg_len, unsigned *restrict msg_prio,
                              const struct timespec *restrict abs_timeout); */
   PRINT("mq_timedreceive( %d, %p, %llu, %p, %p )",
                arg1,arg2,(ULong)arg3,arg4,arg5);
   if (!fd_allowed(arg1, "mq_timedreceive", tid, False)) {
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

POST(mq_timedreceive)
{
   POST_MEM_WRITE( arg2, arg3 );
   if (arg4 != 0)
      POST_MEM_WRITE( arg4, sizeof(unsigned int) );
}

PRE(mq_notify)
{
   /* int mq_notify(mqd_t mqdes, const struct sigevent *notification); */
   PRINT("mq_notify( %d, %p )", arg1,arg2 );
   if (!fd_allowed(arg1, "mq_notify", tid, False))
      set_result( -VKI_EBADF );
   else if (arg2 != 0)
      PRE_MEM_READ( "mq_notify", arg2, sizeof(struct vki_sigevent) );
}

PRE(mq_getsetattr)
{
   /* int mq_getsetattr(mqd_t mqdes, const struct mq_attr *restrict mqstat,
                        struct mq_attr *restrict omqstat); */
   PRINT("mq_getsetattr( %d, %p, %p )", arg1,arg2,arg3 );
   if (!fd_allowed(arg1, "mq_getsetattr", tid, False)) {
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

POST(mq_getsetattr)
{
   if (arg3 != 0)
      POST_MEM_WRITE( arg3, sizeof(struct vki_mq_attr) );
}

PRE(timer_create)
{
   /* int timer_create(clockid_t clock_id, struct sigevent *restrict evp,
                       timer_t *restrict timerid); */
   PRINT("timer_create( %d, %p, %p )", arg1,arg2,arg3);
   if (arg2 != 0)
      PRE_MEM_READ( "timer_create(evp)", arg2, sizeof(struct vki_sigevent) );
   PRE_MEM_WRITE( "timer_create(timerid)", arg3, sizeof(vki_timer_t) );
}

POST(timer_create)
{
   POST_MEM_WRITE( arg3, sizeof(vki_timer_t) );
}

PRE(timer_settime)
{
   /* int timer_settime(timer_t timerid, int flags,
                        const struct itimerspec *restrict value,
                        struct itimerspec *restrict ovalue); */
   PRINT("timer_settime( %p, %d, %p, %p )", arg1,arg2,arg3,arg4);
   PRE_MEM_READ( "timer_settime(value)", arg3,
                  sizeof(struct vki_itimerspec) );
   if (arg4 != 0)
       PRE_MEM_WRITE( "timer_settime(ovalue)", arg4,
                      sizeof(struct vki_itimerspec) );
}

POST(timer_settime)
{
   if (arg4 != 0)
      POST_MEM_WRITE( arg4, sizeof(struct vki_itimerspec) );
}

PRE(timer_gettime)
{
   /* int timer_gettime(timer_t timerid, struct itimerspec *value); */
   PRINT("timer_gettime( %p, %p )", arg1,arg2);
   PRE_MEM_WRITE( "timer_gettime(value)", arg2,
                  sizeof(struct vki_itimerspec));
}

POST(timer_gettime)
{
   POST_MEM_WRITE( arg2, sizeof(struct vki_itimerspec) );
}

PRE(timer_getoverrun)
{
   /* int timer_getoverrun(timer_t timerid); */
   PRINT("timer_getoverrun( %p )", arg1);
}

PRE(timer_delete)
{
   /* int timer_delete(timer_t timerid); */
   PRINT("timer_delete( %p )", arg1);
}

PRE(clock_settime)
{
    /* int clock_settime(clockid_t clk_id, const struct timespec *tp); */
    PRINT("clock_settime( %d, %p )", arg1,arg2);
    PRE_MEM_READ( "clock_gettime(tp)", arg2, sizeof(struct vki_timespec) );
}

PRE(clock_gettime)
{
    /* int clock_gettime(clockid_t clk_id, struct timespec *tp); */
    PRINT("clock_gettime( %d, %p )" , arg1,arg2);
    PRE_MEM_WRITE( "clock_gettime(tp)", arg2, sizeof(struct vki_timespec) );
}

POST(clock_gettime)
{
    POST_MEM_WRITE( arg2, sizeof(struct vki_timespec) );
}

PRE(clock_getres)
{
    /* int clock_getres(clockid_t clk_id, struct timespec *res); */
    PRINT("clock_getres( %d, %p )" , arg1,arg2);
    PRE_MEM_WRITE( "clock_getres(res)", arg2, sizeof(struct vki_timespec) );
}

POST(clock_getres)
{
    POST_MEM_WRITE( arg2, sizeof(struct vki_timespec) );
}

struct sys_info {
   UInt	flags;
   void	(*before)(ThreadId tid, ThreadState *tst);
   void	(*after)(ThreadId tid, ThreadState *tst);
};
#define SYSB_(name, flags)	[__NR_##name] = { flags, before_##name, NULL }
#define SYSBA(name, flags)	[__NR_##name] = { flags, before_##name, after_##name }

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

static void bad_after(ThreadId tid, ThreadState *tst)
{
}

static const struct sys_info bad_sys = { False, bad_before, bad_after };

static const struct sys_info special_sys[] = {
   /* special */
   SYSB_(exit_group,		0),
   SYSB_(exit,			0),
   SYSB_(clone,			0),

   SYSB_(modify_ldt,		0),
   SYSB_(set_thread_area,	0),
   SYSB_(get_thread_area,	0),
   SYSB_(set_tid_address,	0),

   SYSB_(execve,		0),
   SYSB_(brk,			0),
   SYSB_(mmap,			0),
   SYSB_(mremap,		0),

   SYSB_(io_setup,              0),
   SYSB_(io_destroy,            0),

#if SIGNAL_SIMULATION
   SYSBA(sigaltstack,		0),
   SYSBA(rt_sigaction,		0),
   SYSBA(sigaction,		0),
   SYSBA(rt_sigprocmask,	0),
   SYSBA(sigprocmask,		0),
#endif /* SIGNAL_SIMULATION */
};
#define MAX_SPECIAL_SYS		(sizeof(special_sys)/sizeof(special_sys[0]))

static const struct sys_info sys_info[] = {
   SYSBA(ptrace,		0),
   SYSB_(mount,			MayBlock),
   SYSB_(umount,		0),

   SYSB_(setresgid,		0),
   SYSB_(vhangup,		0),
   SYSB_(iopl,			0),

   SYSB_(setxattr,		MayBlock),
   SYSB_(lsetxattr,		MayBlock),
   SYSB_(fsetxattr,		MayBlock),
   SYSBA(getxattr,		MayBlock),
   SYSBA(lgetxattr,		MayBlock),
   SYSBA(fgetxattr,		MayBlock),
   SYSBA(listxattr,		MayBlock),
   SYSBA(llistxattr,		MayBlock),
   SYSBA(flistxattr,		MayBlock),
   SYSB_(removexattr,		MayBlock),
   SYSB_(lremovexattr,		MayBlock),
   SYSB_(fremovexattr,		MayBlock),

   SYSB_(quotactl,		0),
   SYSBA(lookup_dcookie,	0),

   SYSB_(truncate64,		MayBlock),
   SYSB_(fdatasync,		MayBlock),
   SYSB_(msync,			MayBlock),

   SYSBA(getpmsg,		MayBlock),
   SYSB_(putpmsg,		MayBlock),

   SYSBA(syslog,		MayBlock),
   SYSB_(personality,		0),
   SYSB_(chroot,		0),
   SYSB_(madvise,		MayBlock),
   SYSB_(nice,			0),
   SYSB_(setresgid32,		0),
   SYSB_(setfsuid32,		0),
   SYSBA(_sysctl,		0),

   SYSB_(sched_getscheduler,	0),	/* ??? */
   SYSB_(sched_setscheduler,	0),	/* ??? */

   SYSB_(mlock,			MayBlock),
   SYSB_(munlock,		MayBlock),
   SYSB_(mlockall,		MayBlock),
   SYSB_(munlockall,		MayBlock),

   SYSB_(sched_get_priority_max,	0),	/* ??? */
   SYSB_(sched_get_priority_min,	0),	/* ??? */

   SYSB_(setpriority,		0),
   SYSB_(getpriority,		0),

   SYSB_(setfsgid,		0),
   SYSB_(setregid,		0),
   SYSB_(setresuid,		0),
   SYSB_(setfsuid,		0),

   SYSBA(sendfile,		MayBlock),
   SYSBA(sendfile64,		MayBlock),
   SYSB_(pwrite64,		MayBlock),
   SYSB_(sync,			MayBlock),
   SYSBA(fstatfs,		0),
   SYSBA(fstatfs64,		0),
   SYSB_(getsid,		0),
   SYSBA(pread64,		MayBlock),
   SYSB_(mknod,			0),
   SYSB_(flock,			MayBlock),
   SYSB_(init_module,		MayBlock),
   SYSB_(ioperm,		0),
   SYSBA(capget,		0),
   SYSB_(capset,		0),
   SYSB_(access,		0),
   SYSB_(chdir,			0),
   SYSB_(chmod,			0),
   SYSB_(chown32,		0),
   SYSB_(lchown32,		0),
   SYSB_(chown,			0),
   SYSBA(close,			0),
   SYSBA(dup,			0),
   SYSBA(dup2,			0),
   SYSBA(fcntl,			0),
   SYSB_(fchdir,		0),
   SYSB_(fchown32,		0),
   SYSB_(fchown,		0),
   SYSB_(fchmod,		0),
   SYSBA(fcntl64,		0),
   SYSBA(fstat,			0),
   SYSBA(fork,			0),
   SYSB_(fsync,			MayBlock),
   SYSB_(ftruncate,		MayBlock),
   SYSB_(ftruncate64,		MayBlock),
   SYSBA(getdents,		MayBlock),
   SYSBA(getdents64,		MayBlock),
   SYSBA(getgroups32,		0),
   SYSBA(getgroups,		0),
   SYSBA(getcwd,		0),
   SYSB_(geteuid,		0),
   SYSB_(geteuid32,		0),
   SYSB_(getegid,		0),
   SYSB_(getegid32,		0),
   SYSB_(getgid,		0),
   SYSB_(getgid32,		0),
   SYSB_(getpid,		0),
   SYSB_(getpgid,		0),
   SYSB_(getpgrp,		0),
   SYSB_(getppid,		0),
   SYSBA(getresgid,		0),
   SYSBA(getresgid32,		0),
   SYSBA(getresuid,		0),
   SYSBA(getresuid32,		0),
   SYSBA(ugetrlimit,		0),
   SYSBA(getrlimit,		0),
   SYSBA(getrusage,		0),
   SYSBA(gettimeofday,		0),
   SYSB_(getuid,		0),
   SYSB_(getuid32,		0),
   SYSBA(ipc,			0),
   SYSBA(ioctl,			MayBlock),
   SYSBA(kill,			0),
   SYSB_(link,			MayBlock),
   SYSB_(lseek,			0),
   SYSBA(_llseek,		0),
   SYSBA(lstat,			0),
   SYSBA(lstat64,		0),
   SYSB_(mkdir,			MayBlock),
   SYSBA(mprotect,		0),
   SYSBA(munmap,		0),
   SYSBA(mincore,		0),
   SYSBA(nanosleep,		MayBlock|PostOnFail),
   SYSB_(_newselect,		MayBlock),
   SYSBA(open,			MayBlock),
   SYSBA(read,			MayBlock),
   SYSB_(write,			MayBlock),
   SYSBA(creat,			MayBlock),
   SYSBA(pipe,			0),
   SYSBA(poll,			MayBlock),
   SYSBA(epoll_create,          0),
   SYSB_(epoll_ctl,             0),
   SYSBA(epoll_wait,		MayBlock),
   SYSBA(readlink,		0),
   SYSBA(readv,			MayBlock),
   SYSB_(rename,		0),
   SYSB_(rmdir,			MayBlock),
   SYSBA(sched_setparam,	0),	/* ??? */
   SYSBA(sched_getparam,	0),	/* ??? */
   SYSB_(sched_yield,		0),	/* ??? */
   SYSB_(select,		MayBlock),
   SYSB_(setfsgid32,		0),
   SYSB_(setgid32,		0),
   SYSB_(setgid,		0),
   SYSB_(setsid,		0),
   SYSB_(setgroups32,		0),
   SYSB_(setgroups,		0),
   SYSBA(setpgid,		0),
   SYSB_(setregid32,		0),
   SYSB_(setresuid32,		0),
   SYSB_(setreuid32,		0),
   SYSB_(setreuid,		0),
   SYSB_(setrlimit,		0),
   SYSB_(settimeofday,		0),
   SYSB_(setuid32,		0),
   SYSB_(setuid,		0),
   SYSBA(socketcall,		MayBlock),
   SYSBA(stat,			0),
   SYSBA(statfs,		0),
   SYSBA(statfs64,		0),
   SYSB_(symlink,		MayBlock),
   SYSBA(stat64,		0),
   SYSBA(fstat64,		0),
   SYSBA(sysinfo,		0),
   SYSBA(time,			0),
   SYSBA(times,			0),
   SYSB_(truncate,		MayBlock),
   SYSB_(umask,			0),
   SYSB_(unlink,		MayBlock),
   SYSBA(uname,			0),
   SYSB_(utime,			MayBlock),
   SYSB_(utimes,                0),
   SYSBA(waitpid,		MayBlock),
   SYSBA(wait4,			MayBlock),
   SYSB_(writev,		MayBlock),
   SYSB_(prctl,			MayBlock),
   SYSBA(adjtimex,		0),
   SYSBA(mmap2,			0),
   SYSBA(futex,                 MayBlock),
   SYSB_(sched_setaffinity,     0),
   SYSBA(sched_getaffinity,     0),
   SYSB_(acct,                  0),

   /* new signal handling makes these normal blocking syscalls */
   SYSB_(pause,			MayBlock),
   SYSB_(sigsuspend,		MayBlock),
   SYSB_(rt_sigsuspend,		MayBlock),
   SYSBA(rt_sigtimedwait,	MayBlock),
   SYSBA(rt_sigqueueinfo,	0),

   SYSBA(sigpending,		MayBlock), /* not blocking, but must run in LWP context */
   SYSBA(rt_sigpending,		MayBlock), /* not blocking, but must run in LWP context */
   SYSB_(alarm,			MayBlock), /* not blocking, but must run in LWP context */
   SYSBA(setitimer,		MayBlock), /* not blocking, but must run in LWP context */
   SYSBA(getitimer,		MayBlock), /* not blocking, but must run in LWP context */

   SYSBA(io_getevents,          MayBlock),
   SYSB_(io_submit,             0),
   SYSBA(io_cancel,             0),

   SYSBA(mq_open,               0),
   SYSB_(mq_unlink,             0),
   SYSB_(mq_timedsend,          MayBlock),
   SYSBA(mq_timedreceive,       MayBlock),
   SYSB_(mq_notify,             0),
   SYSBA(mq_getsetattr,         0),

   SYSBA(timer_create,		0),
   SYSBA(timer_settime,		0),
   SYSBA(timer_gettime,		0),
   SYSB_(timer_getoverrun,	0),
   SYSB_(timer_delete,		0),

   SYSB_(clock_settime,         0),
   SYSBA(clock_gettime,         0),
   SYSBA(clock_getres,          0),

#if !SIGNAL_SIMULATION
   SYSBA(sigaltstack,		0),
   SYSBA(rt_sigaction,		0),
   SYSBA(sigaction,		0),
   SYSBA(rt_sigprocmask,	0),
   SYSBA(sigprocmask,		0),
#endif /* !SIGNAL_SIMULATION */
};
#define MAX_SYS_INFO		(sizeof(sys_info)/sizeof(sys_info[0]))

#undef SYSB_
#undef SYSBA

Bool VG_(pre_syscall) ( ThreadId tid )
{
   ThreadState* tst;
   UInt         syscallno;
   const struct sys_info *sys;
   Bool special = False;
   Bool syscall_done = False;	/* we actually ran the syscall */

   VGP_PUSHCC(VgpCoreSysWrap);

   tst = VG_(get_ThreadState)(tid);

   /* Convert vfork to fork, since we can't handle it otherwise. */
   if (SYSNO == __NR_vfork)
      SYSNO = __NR_fork;

   syscallno = (UInt)SYSNO;

   if (tst->syscallno != -1)
      VG_(printf)("tid %d has syscall %d\n", tst->tid, tst->syscallno);

   vg_assert(tst->syscallno == -1);		/* should be no current syscall */
   vg_assert(tst->status == VgTs_Runnable);	/* should be runnable */

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

   if (syscallno < MAX_SPECIAL_SYS && special_sys[syscallno].before != NULL) {
      sys = &special_sys[syscallno];
      special = True;
   } else if (syscallno < MAX_SYS_INFO && sys_info[syscallno].before != NULL) {
      sys = &sys_info[syscallno];
   } else {
      sys = &bad_sys;
      special = True;
   }

   tst->sys_flags = sys->flags;

   /* Do any pre-syscall actions */
   if (VG_(needs).syscall_wrapper) {
      VGP_PUSHCC(VgpSkinSysWrap);
      tst->sys_pre_res = SK_(pre_syscall)(tid, syscallno, /*isBlocking*/(sys->flags & MayBlock) != 0);
      VGP_POPCC(VgpSkinSysWrap);
   }

   PRINT("SYSCALL[%d,%d](%3d)%s%s:", 
         VG_(getpid)(), tid, syscallno, 
         special ? " special" : "",
         (sys->flags & MayBlock) != 0 ? " blocking" : "");

   if (special) {
      /* "Special" syscalls are implemented by Valgrind internally,
	 and do not generate real kernel calls.  The expectation,
	 therefore, is that the "before" function not only does the
	 appropriate tests, but also performs the syscall itself and
	 sets the result.  Special syscalls cannot block. */
      vg_assert((tst->sys_flags & MayBlock) == 0);

      (sys->before)(tst->tid, tst);

      vg_assert(tst->sys_flags == sys->flags);

      PRINT(" --> %lld (0x%llx)\n", (Long)(Word)res, (ULong)res);
      syscall_done = True;
   } else {
      (sys->before)(tst->tid, tst);

      if ((Word)res <= 0) {
	 /* "before" decided the syscall wasn't viable, so don't do
	    anything - just pretend the syscall happened. */
         PRINT(" ==> %lld (0x%llx)\n", (Long)(Word)res, (ULong)res);
	 syscall_done = True;
      } else if ((tst->sys_flags & MayBlock) != 0) {
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
   UInt syscallno;
   const struct sys_info *sys;
   Bool special = False;
   Bool restarted = False;
   void *pre_res;

   VGP_PUSHCC(VgpCoreSysWrap);

   tst = VG_(get_ThreadState)(tid);
   vg_assert(tst->tid == tid);

   /* Tell the tool about the syscall return value */
   SET_SYSCALL_RETVAL(tst->tid, res);

   syscallno = tst->syscallno;
   pre_res = tst->sys_pre_res;

   vg_assert(syscallno != -1);			/* must be a current syscall */

   if (syscallno < MAX_SPECIAL_SYS && special_sys[syscallno].before != NULL) {
      sys = &special_sys[syscallno];
      special = True;
   } else if (syscallno < MAX_SYS_INFO && sys_info[syscallno].before != NULL) {
      sys = &sys_info[syscallno];
   } else {
      sys = &bad_sys;
      special = True;
   }

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
	 VGP_PUSHCC(VgpSkinSysWrap);
	 SK_(post_syscall)(tid, syscallno, pre_res, res, /*isBlocking*/True); // did block
	 VGP_POPCC(VgpSkinSysWrap);
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
/*--- end                                            vg_syscalls.c ---*/
/*--------------------------------------------------------------------*/

