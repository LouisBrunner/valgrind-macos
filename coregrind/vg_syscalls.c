
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

#include "vg_include.h"

/* vg_unsafe.h should NOT be included into any file except this
   one. */
#include "vg_unsafe.h"


/* All system calls are channelled through here, doing two things:

   * notify the skin of the memory events (reads, writes) happening

   * perform the syscall, usually by passing it along to the kernel
     unmodified.

   A magical piece of assembly code, VG_(do_syscall)(), in vg_syscall.S
   does the tricky bit of passing a syscall to the kernel, whilst
   having the simulator retain control.
*/

#define SYSCALL_TRACK(fn, args...)  VG_TRACK(fn, Vg_CoreSysCall, ## args)

#define MAYBE_PRINTF(format, args...)  \
   if (VG_(clo_trace_syscalls))        \
      VG_(printf)(format, ## args)


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
static Bool valid_client_addr(Addr start, UInt size, ThreadId tid, const Char *syscall)
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
		  syscall, start, end, cl_base, VG_(client_end), ret);

   if (!ret && syscall != NULL) {
      VG_(message)(Vg_UserMsg, "Warning: client syscall %s tried to modify addresses %p-%p",
		   syscall, start, end);

      if (VG_(clo_verbosity) > 1) {
	 ExeContext *ec = VG_(get_ExeContext)(tid);
	 VG_(pp_ExeContext)(ec);
      }
   }

   return ret;
}

/* Walk through a colon-separated environment variable, and remove the
   entries which matches file_pattern.  It slides everything down over
   the removed entries, and pads the remaining space with '\0'.  It
   modifies the entries in place (in the client address space), but it
   shouldn't matter too much, since we only do this just before an
   execve().

   This is also careful to mop up any excess ':'s, since empty strings
   delimited by ':' are considered to be '.' in a path.
*/
void VG_(mash_colon_env)(Char *varp, const Char *remove_pattern)
{
   Char *const start = varp;
   Char *entry_start = varp;
   Char *output = varp;

   if (varp == NULL)
      return;

   while(*varp) {
      if (*varp == ':') {
	 Char prev;
	 Bool match;

	 /* This is a bit subtle: we want to match against the entry
	    we just copied, because it may have overlapped with
	    itself, junking the original. */

	 prev = *output;
	 *output = '\0';

	 match = VG_(string_match)(remove_pattern, entry_start);

	 *output = prev;
	 
	 if (match) {
	    output = entry_start;
	    varp++;			/* skip ':' after removed entry */
	 } else
	    entry_start = output+1;	/* entry starts after ':' */
      }

      *output++ = *varp++;
   }

   /* match against the last entry */
   if (VG_(string_match)(remove_pattern, entry_start)) {
      output = entry_start;
      if (output > start) {
	 /* remove trailing ':' */
	 output--;
	 vg_assert(*output == ':');
      }
   }	 

   /* pad out the left-overs with '\0' */
   while(output < varp)
      *output++ = '\0';
}


/* ---------------------------------------------------------------------
   Doing mmap, munmap, mremap, mprotect
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
void mash_addr_and_len( Addr* a, UInt* len)
{
   Addr ra;
   
   ra = PGROUNDDN(*a);
   *len = PGROUNDUP(*a + *len) - ra;
   *a = ra;
}

static
void mmap_segment ( Addr a, UInt len, UInt prot, UInt mm_flags, Int fd, ULong offset )
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
void munmap_segment ( Addr a, UInt len )
{
   /* Addr orig_a   = a;
      Addr orig_len = len; */

   mash_addr_and_len(&a, &len);

   VG_(unmap_range)(a, len);

   /*
   VG_(printf)("MUNMAP: correct (%p for %d) to (%p for %d) %s\n", 
      orig_a, orig_len, a, len, (orig_a!=start || orig_len!=length) 
                                    ? "CHANGE" : "");
   */

   VG_TRACK( die_mem_munmap, a, len );
}

static 
void mprotect_segment ( Addr a, UInt len, Int prot )
{
   Bool rr, ww, xx;

   VG_(mprotect_range)(a, len, prot);

   rr = prot & VKI_PROT_READ;
   ww = prot & VKI_PROT_WRITE;
   xx = prot & VKI_PROT_EXEC;

   // if removing exe permission, should check and remove from exe_seg list
   // if adding, should check and add to exe_seg list
   // easier to ignore both cases -- both v. unlikely?
   mash_addr_and_len(&a, &len);
   VG_TRACK( change_mem_mprotect, a, len, rr, ww, xx );
}

static 
Addr mremap_segment ( Addr old_addr, UInt old_size,
		      Addr new_addr, UInt new_size,
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
Bool VG_(is_kerror) ( Int res )
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

/* Given a file descriptor, attempt to deduce it's filename.  To do this,
   we use /proc/self/fd/<FD>.  If this doesn't point to a file, or if it
   doesn't exist, we just return NULL.  Otherwise, we return a pointer
   to the file name, which the caller is responsible for freeing. */

Char *VG_(resolve_filename)(Int fd)
{
   char tmp[28], buf[PATH_MAX];

   VG_(sprintf)(tmp, "/proc/self/fd/%d", fd);
   VG_(memset)(buf, 0, PATH_MAX);

   if(VG_(readlink)(tmp, buf, PATH_MAX) == -1)
      return NULL;

   return ((buf[0] == '/') ? VG_(arena_strdup)(VG_AR_CORE, buf) : NULL);
}


/* Note the fact that a file descriptor was just closed. */

static
void record_fd_close(Int tid, Int fd)
{
   OpenFd *i = allocated_fds;

   if (fd > VG_(max_fd))
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

   if (fd > VG_(max_fd))
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
Char *unix2name(struct sockaddr_un *sa, UInt len, Char *name)
{
   if(sa == NULL || len == 0 || sa->sun_path[0] == '\0') {
      VG_(sprintf)(name, "<unknown>");
   } else {
      VG_(sprintf)(name, "%s", sa->sun_path);
   }

   return name;
}

static
Char *inet2name(struct sockaddr_in *sa, UInt len, Char *name)
{
   if(sa == NULL || len == 0) {
      VG_(sprintf)(name, "<unknown>");
   } else {
      UInt addr = sa->sin_addr.s_addr;

      if (addr == 0) {
         VG_(sprintf)(name, "<unbound>");
      } else {
         VG_(sprintf)(name, "%u.%u.%u.%u:%u",
                      addr & 0xFF, (addr>>8) & 0xFF,
                      (addr>>16) & 0xFF, (addr>>24) & 0xFF,
                      ntohs(sa->sin_port));
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
      struct sockaddr a;
      struct sockaddr_in in;
      struct sockaddr_un un;
   } laddr;
   socklen_t llen;

   llen = sizeof(laddr);
   VG_(memset)(&laddr, 0, llen);

   if(VG_(getsockname)(fd, (struct vki_sockaddr *)&(laddr.a), &llen) != -1) {
      switch(laddr.a.sa_family) {
      case AF_INET: {
         static char lname[32];
         static char pname[32];
         struct sockaddr_in paddr;
         socklen_t plen = sizeof(struct sockaddr_in);

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
      case AF_UNIX: {
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


/* Dump out a summary, and optionally a more detailed list, of open file
   descriptors. */

void VG_(fd_stats) ()
{
   OpenFd *i = allocated_fds;

   VG_(message)(Vg_UserMsg,
                "FILE DESCRIPTORS: %d open at exit.", fd_count);

   while(i) {
      if(i->pathname) {
         VG_(message)(Vg_UserMsg, "Open file descriptor %d: %s", i->fd,
                      i->pathname);
      } else {
         int val;
         socklen_t len = sizeof(val);

         if(VG_(getsockopt)(i->fd, SOL_SOCKET, SO_TYPE, &val, &len) == -1) {
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
   struct shmid_ds buf;
   long __res = VG_(do_syscall)(__NR_ipc, 24 /* IPCOP_shmctl */, shmid, IPC_STAT, 0, &buf);
    if ( VG_(is_kerror) ( __res ) )
       return 0;
 
   return buf.shm_segsz;
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
                            Char *msg, UInt base, UInt size )
{
   Char *outmsg = strdupcat ( "socketcall.sendmsg", msg, VG_AR_TRANSIENT );
   SYSCALL_TRACK( pre_mem_read, tid, outmsg, base, size );

   VG_(arena_free) ( VG_AR_TRANSIENT, outmsg );
}

static 
void pre_mem_write_recvmsg ( ThreadId tid,
                             Char *msg, UInt base, UInt size )
{
   Char *outmsg = strdupcat ( "socketcall.recvmsg", msg, VG_AR_TRANSIENT );
   SYSCALL_TRACK( pre_mem_write, tid, outmsg, base, size );
   VG_(arena_free) ( VG_AR_TRANSIENT, outmsg );
}

static
void post_mem_write_recvmsg ( ThreadId tid,
                              Char *fieldName, UInt base, UInt size )
{
   VG_TRACK( post_mem_write, base, size );
}
 
static
void msghdr_foreachfield ( 
        ThreadId tid, 
        struct msghdr *msg, 
        void (*foreach_func)( ThreadId, Char *, UInt, UInt ) 
     )
{
   if ( !msg )
      return;

   foreach_func ( tid, "(msg)", (Addr)msg, sizeof( struct msghdr ) );

   if ( msg->msg_name )
      foreach_func ( tid, 
                     "(msg.msg_name)", 
                     (Addr)msg->msg_name, msg->msg_namelen );

   if ( msg->msg_iov ) {
      struct iovec *iov = msg->msg_iov;
      UInt i;

      foreach_func ( tid, 
                     "(msg.msg_iov)", 
                     (Addr)iov, msg->msg_iovlen * sizeof( struct iovec ) );

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

void check_cmsg_for_fds(Int tid, struct msghdr *msg)
{
   struct cmsghdr *cm = CMSG_FIRSTHDR(msg);

   while (cm) {
      if (cm->cmsg_level == SOL_SOCKET &&
          cm->cmsg_type == SCM_RIGHTS ) {
         int *fds = (int *) CMSG_DATA(cm);
         int fdc = (cm->cmsg_len - CMSG_ALIGN(sizeof(struct cmsghdr)))
                         / sizeof(int);
         int i;

         for (i = 0; i < fdc; i++)
            if(VG_(clo_track_fds))
               record_fd_open (tid, fds[i], VG_(resolve_filename)(fds[i]));
      }

      cm = CMSG_NXTHDR(msg, cm);
   }
}

static
void pre_mem_read_sockaddr ( ThreadId tid,
			     Char *description,
			     struct sockaddr *sa, UInt salen )
{
   Char *outmsg;

   /* NULL/zero-length sockaddrs are legal */
   if ( sa == NULL || salen == 0 ) return;

   outmsg = VG_(arena_malloc) ( VG_AR_TRANSIENT,
                                strlen( description ) + 30 );

   VG_(sprintf) ( outmsg, description, ".sa_family" );
   SYSCALL_TRACK( pre_mem_read, tid, outmsg, 
                  (UInt) &sa->sa_family, sizeof (sa_family_t));

   switch (sa->sa_family) {
                  
      case AF_UNIX:
         VG_(sprintf) ( outmsg, description, ".sun_path" );
         SYSCALL_TRACK( pre_mem_read_asciiz, tid, outmsg,
            (UInt) ((struct sockaddr_un *) sa)->sun_path);
         break;
                     
      case AF_INET:
         VG_(sprintf) ( outmsg, description, ".sin_port" );
         SYSCALL_TRACK( pre_mem_read, tid, outmsg,
            (UInt) &((struct sockaddr_in *) sa)->sin_port,
            sizeof (((struct sockaddr_in *) sa)->sin_port));
         VG_(sprintf) ( outmsg, description, ".sin_addr" );
         SYSCALL_TRACK( pre_mem_read, tid, outmsg,
            (UInt) &((struct sockaddr_in *) sa)->sin_addr,
            sizeof (struct in_addr));
         break;
                           
      case AF_INET6:
         VG_(sprintf) ( outmsg, description, ".sin6_port" );
         SYSCALL_TRACK( pre_mem_read, tid, outmsg,
            (UInt) &((struct sockaddr_in6 *) sa)->sin6_port,
            sizeof (((struct sockaddr_in6 *) sa)->sin6_port));
         VG_(sprintf) ( outmsg, description, ".sin6_flowinfo" );
         SYSCALL_TRACK( pre_mem_read, tid, outmsg,
            (UInt) &((struct sockaddr_in6 *) sa)->sin6_flowinfo,
            sizeof (uint32_t));
         VG_(sprintf) ( outmsg, description, ".sin6_addr" );
         SYSCALL_TRACK( pre_mem_read, tid, outmsg,
            (UInt) &((struct sockaddr_in6 *) sa)->sin6_addr,
            sizeof (struct in6_addr));
#        ifndef GLIBC_2_1
         VG_(sprintf) ( outmsg, description, ".sin6_scope_id" );
         SYSCALL_TRACK( pre_mem_read, tid, outmsg,
            (UInt) &((struct sockaddr_in6 *) sa)->sin6_scope_id,
			sizeof (uint32_t));
#        endif
         break;
               
      default:
         VG_(sprintf) ( outmsg, description, "" );
         SYSCALL_TRACK( pre_mem_read, tid, outmsg, (UInt) sa, salen );
         break;
   }
   
   VG_(arena_free) ( VG_AR_TRANSIENT, outmsg );
}

/* Dereference a pointer to a UInt. */
static UInt deref_UInt ( ThreadId tid, Addr a, Char* s )
{
   UInt* a_p = (UInt*)a;
   SYSCALL_TRACK( pre_mem_read, tid, s, (Addr)a_p, sizeof(UInt) );
   if (a_p == NULL)
      return 0;
   else
      return *a_p;
}

/* Dereference a pointer to a pointer. */
static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
{
   Addr* a_p = (Addr*)a;
   SYSCALL_TRACK( pre_mem_read, tid, s, (Addr)a_p, sizeof(Addr) );
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

   if (PGROUNDDN(newbrk) != PGROUNDDN(VG_(brk_limit))) {
      Addr current = PGROUNDUP(VG_(brk_limit));
      Addr newaddr = PGROUNDUP(newbrk);

      /* new brk in a new page - fix the mappings */
      if (newbrk > VG_(brk_limit)) {
	 
	 if (debug)
	    VG_(printf)("  extending brk: current=%p newaddr=%p delta=%d\n",
			current, newaddr, newaddr-current);

	 if (newaddr == current) {
	    ret = newbrk;
	 } else if (VG_(mmap)((void *)current , newaddr-current,
			      VKI_PROT_READ | VKI_PROT_WRITE | VKI_PROT_EXEC,
			      VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS | VKI_MAP_FIXED | VKI_MAP_CLIENT,
			      -1, 0) >= 0) {
	    VG_(map_segment)(current, newaddr-current, VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC,
			     SF_FIXED|SF_BRK);
	    ret = newbrk;
	 }
      } else {
	 vg_assert(newbrk < VG_(brk_limit));

	 if (debug)
	    VG_(printf)("  shrinking brk: current=%p newaddr=%p delta=%d\n",
			current, newaddr, current-newaddr);

	 if (newaddr != current) {
	    VG_(munmap)((void *)newaddr, current - newaddr);
	    VG_(unmap_range)(newaddr, current-newaddr);
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
static Bool fd_allowed(Int fd, const Char *syscall, ThreadId tid)
{
   if (fd < 0 || fd > VG_(max_fd) || fd == VG_(clo_logfile_fd)) {
      VG_(message)(Vg_UserMsg, 
         "Warning: invalid file descriptor %d in syscall %s()",
         fd, syscall);
      if (fd == VG_(clo_logfile_fd))
	 VG_(message)(Vg_UserMsg, 
            "   Use --logfile-fd=<number> to select an alternative "
	    "logfile fd.");
      if (VG_(clo_verbosity) > 1) {
	 ExeContext *ec = VG_(get_ExeContext)(tid);
	 VG_(pp_ExeContext)(ec);
      }
      return False;
   }
   return True;
}


/* ---------------------------------------------------------------------
   The Main Entertainment ...
   ------------------------------------------------------------------ */


#define PRE(x)	\
	static void before_##x(ThreadId tid, ThreadState *tst)
#define POST(x)	\
	static void after_##x(ThreadId tid, ThreadState *tst)

#define PREALIAS(new, old)	\
	PRE(new) __attribute__((alias(STR(before_##old))))
#define POSTALIAS(new, old)	\
	POST(new) __attribute__((alias(STR(after_##old))))

#define SYSNO	(tst->m_eax)		/* in PRE(x)  */
#define res	(tst->m_eax)	/* in POST(x) */
#define arg1	(tst->m_ebx)
#define arg2	(tst->m_ecx)
#define arg3	(tst->m_edx)
#define arg4	(tst->m_esi)
#define arg5	(tst->m_edi)
#define arg6	(tst->m_ebp)

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
      void *addr, void *data); ... sort of. */
   /* Sigh ... the /usr/include/sys/user.h on R H 6.2 doesn't 
      define struct user_fpxregs_struct.  On the basis that it 
      is defined as follows on my R H 7.2 (glibc-2.2.4) box, 
      I kludge it.

      struct user_fpxregs_struct
      {
      unsigned short int cwd;
      unsigned short int swd;
      unsigned short int twd;
      unsigned short int fop;
      long int fip;
      long int fcs;
      long int foo;
      long int fos;
      long int mxcsr;
      long int reserved;
      long int st_space[32];  8*16 bytes for each FP-reg = 128 bytes
      long int xmm_space[32]; 8*16 bytes for each XMM-reg = 128 bytes
      long int padding[56];
      };
   */
   const Int sizeof_struct_user_fpxregs_struct
      = sizeof(unsigned short) * (1 + 1 + 1 + 1) 
      + sizeof(long int) * (1 + 1 + 1 + 1 + 1 + 1 + 32 + 32 + 56);

   MAYBE_PRINTF("ptrace ( %d, %d, %p, %p )\n", arg1,arg2,arg3,arg4);
   switch (arg1) {
   case 12:   /* PTRACE_GETREGS */
      SYSCALL_TRACK( pre_mem_write, tid, "ptrace(getregs)", arg4, 
		     sizeof (struct user_regs_struct));
      break;
   case 14:   /* PTRACE_GETFPREGS */
      SYSCALL_TRACK( pre_mem_write, tid, "ptrace(getfpregs)", arg4, 
		     sizeof (struct user_fpregs_struct));
      break;
   case 18:   /* PTRACE_GETFPXREGS */
      SYSCALL_TRACK( pre_mem_write, tid, "ptrace(getfpxregs)", arg4, 
		     sizeof_struct_user_fpxregs_struct);
      break;
   case 1: case 2: case 3:    /* PTRACE_PEEK{TEXT,DATA,USER} */
      SYSCALL_TRACK( pre_mem_write, tid, "ptrace(peek)", arg4, 
		     sizeof (long));
      break;
   case 13:   /* PTRACE_SETREGS */
      SYSCALL_TRACK( pre_mem_read, tid, "ptrace(setregs)", arg4, 
		     sizeof (struct user_regs_struct));
      break;
   case 15:   /* PTRACE_SETFPREGS */
      SYSCALL_TRACK( pre_mem_read, tid, "ptrace(setfpregs)", arg4, 
		     sizeof (struct user_fpregs_struct));
      break;
   case 19:   /* PTRACE_SETFPXREGS */
      SYSCALL_TRACK( pre_mem_read, tid, "ptrace(setfpxregs)", arg4, 
		     sizeof_struct_user_fpxregs_struct);
      break;
   default:
      break;
   }
}

POST(ptrace)
{
   const Int sizeof_struct_user_fpxregs_struct
      = sizeof(unsigned short) * (1 + 1 + 1 + 1) 
      + sizeof(long int) * (1 + 1 + 1 + 1 + 1 + 1 + 32 + 32 + 56);

   switch (arg1) {
   case 12:  /* PTRACE_GETREGS */
      VG_TRACK( post_mem_write, arg4, 
		sizeof (struct user_regs_struct));
      break;
   case 14:  /* PTRACE_GETFPREGS */
      VG_TRACK( post_mem_write, arg4, 
		sizeof (struct user_fpregs_struct));
      break;
   case 18:  /* PTRACE_GETFPXREGS */
      VG_TRACK( post_mem_write, arg4, 
		sizeof_struct_user_fpxregs_struct);
      break;
   case 1: case 2: case 3:    /* PTRACE_PEEK{TEXT,DATA,USER} */
      VG_TRACK( post_mem_write, arg4, sizeof (long));
      break;
   default:
      break;
   }
}

PRE(mount)
{
   MAYBE_PRINTF( "mount( %p, %p, %p )\n" ,arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid,"mount(specialfile)",arg1);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid,"mount(dir)",arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid,"mount(filesystemtype)",arg3);
}

PRE(umount)
{
   /* int umount(const char *path) */
   MAYBE_PRINTF("umount( %p )\n", arg1);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid,"umount(path)",arg1);
}

PRE(modify_ldt)
{
   MAYBE_PRINTF("modify_ldt ( %d, %p, %d )\n", arg1,arg2,arg3);
   if (arg1 == 0) {
      /* read the LDT into ptr */
      SYSCALL_TRACK( pre_mem_write, tid, 
		     "modify_ldt(ptr)(func=0)", arg2, arg3 );
   }
   if (arg1 == 1 || arg1 == 0x11) {
      /* write the LDT with the entry pointed at by ptr */
      SYSCALL_TRACK( pre_mem_read, tid, 
		     "modify_ldt(ptr)(func=1 or 0x11)", arg2, 
		     sizeof(struct vki_modify_ldt_ldt_s) );
   }
   /* "do" the syscall ourselves; the kernel never sees it */
   res = VG_(sys_modify_ldt)( tid, arg1, (void*)arg2, arg3 );

   if (arg1 == 0 && !VG_(is_kerror)(res) && res > 0) {
      VG_TRACK( post_mem_write, arg2, res );
   }
}

PRE(set_thread_area)
{
   MAYBE_PRINTF("set_thread_area ( %p )\n", arg1);

   SYSCALL_TRACK( pre_mem_read, tid, 
		  "set_thread_area(ptr)", arg1, 
		  sizeof(struct vki_modify_ldt_ldt_s) );

   /* "do" the syscall ourselves; the kernel never sees it */
   res = VG_(sys_set_thread_area)( tid, (void *)arg1 );
}

PRE(get_thread_area)
{
   MAYBE_PRINTF("get_thread_area ( %p )\n", arg1);
   SYSCALL_TRACK( pre_mem_write, tid, 
		  "get_thread_area(ptr)", arg1, 
		  sizeof(struct vki_modify_ldt_ldt_s) );

   /* "do" the syscall ourselves; the kernel never sees it */
   res = VG_(sys_get_thread_area)( tid, (void *)arg1 );

   if (!VG_(is_kerror)(res)) {
      VG_TRACK( post_mem_write, arg1, sizeof(struct vki_modify_ldt_ldt_s) );
   }
}

PRE(setresgid)
{
   /* int setresgid(gid_t rgid, gid_t egid, gid_t sgid); */
   MAYBE_PRINTF("setresgid ( %d, %d, %d )\n", arg1, arg2, arg3);
}

PRE(vhangup)
{
   MAYBE_PRINTF("vhangup()\n");
}

PRE(iopl)
{
   MAYBE_PRINTF("iopl ( %d )\n", arg1);
}

PRE(setxattr)
{
   MAYBE_PRINTF("setxattr ( %p, %p, %p, %d, %d )\n",
		arg1, arg2, arg3, arg4, arg5);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "setxattr(path)", arg1 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "setxattr(name)", arg2 );
   SYSCALL_TRACK( pre_mem_read, tid, "setxattr(value)", arg3, arg4 );
}

PREALIAS(lsetxattr, setxattr);

PRE(fsetxattr)
{
   /* int fsetxattr (int filedes, const char *name,
      const void *value, size_t size, int flags); */
   MAYBE_PRINTF("fsetxattr ( %d, %p, %p, %d, %d )\n",
		arg1, arg2, arg3, arg4, arg5);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "fsetxattr(name)", arg2 );
   SYSCALL_TRACK( pre_mem_read, tid, "fsetxattr(value)", arg3, arg4 );
}

PRE(getxattr)
{
   MAYBE_PRINTF("getxattr ( %p, %p, %p, %d )\n", 
		arg1,arg2,arg3, arg4);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "getxattr(path)", arg1 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "getxattr(name)", arg2 );
   SYSCALL_TRACK( pre_mem_write, tid, "getxattr(value)", arg3, arg4 );
}

POST(getxattr)
{
   if (res > 0 && arg3 != (Addr)NULL) {
      VG_TRACK( post_mem_write, arg3, res );
   }
}

PREALIAS(lgetxattr, getxattr);
POSTALIAS(lgetxattr, getxattr);

PRE(fgetxattr)
{
   MAYBE_PRINTF("fgetxattr ( %d, %p, %p, %d )\n",
		arg1, arg2, arg3, arg4);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "fgetxattr(name)", arg2 );
   SYSCALL_TRACK( pre_mem_write, tid, "fgetxattr(value)", arg3, arg4 );
}

POST(fgetxattr)
{
   if (res > 0 && arg3 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg3, res );
}

PRE(listxattr)
{
   MAYBE_PRINTF("listxattr ( %p, %p, %d )\n", arg1, arg2, arg3);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "listxattr(path)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "listxattr(list)", arg2, arg3 );
}

POST(listxattr)
{
   if (res > 0 && arg2 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg2, res );
}

PREALIAS(llistxattr, listxattr);
POSTALIAS(llistxattr, listxattr);

PRE(flistxattr)
{
   /* ssize_t flistxattr (int filedes, char *list, size_t size); */
   MAYBE_PRINTF("flistxattr ( %d, %p, %d )\n", arg1, arg2, arg3);
   SYSCALL_TRACK( pre_mem_write, tid, "listxattr(list)", arg2, arg3 );
}

POST(flistxattr)
{
   if (res > 0 && arg2 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg2, res );
}

PRE(removexattr)
{
   MAYBE_PRINTF("removexattr ( %p, %p )\n", arg1, arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "listxattr(path)", arg1 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "listxattr(name)", arg2 );
}

PREALIAS(lremovexattr, removexattr);

PRE(fremovexattr)
{
   MAYBE_PRINTF("removexattr ( %d, %p )\n", arg1, arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "listxattr(name)", arg2 );
}

PRE(quotactl)
{
   MAYBE_PRINTF("quotactl (0x%x, %p, 0x%x, 0x%x )\n", 
		arg1,arg2,arg3, arg4);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "quotactl(special)", arg2 );
}

PRE(lookup_dcookie)
{
   MAYBE_PRINTF("lookup_dcookie (0x%llx, %p, %d)\n",
		arg1 | ((long long) arg2 << 32), arg3, arg4);
   SYSCALL_TRACK( pre_mem_write, tid, "lookup_dcookie(buf)", arg3, arg4);
}

POST(lookup_dcookie)
{
   if (arg3 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg3, res);
}

PRE(truncate64)
{
   MAYBE_PRINTF("truncate64 ( %p, %lld )\n",
		arg1, ((ULong)arg2) | (((ULong) arg3) << 32));
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "truncate64(path)", arg1 );
}

PRE(fdatasync)
{
   /* int fdatasync(int fd); */
   MAYBE_PRINTF("fdatasync ( %d )\n", arg1);
}

PRE(msync)
{
   /* int msync(const void *start, size_t length, int flags); */
   MAYBE_PRINTF("msync ( %p, %d, %d )\n", arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_read, tid, "msync(start)", arg1, arg2 );
}

struct getpmsg_strbuf {
   int     maxlen;         /* no. of bytes in buffer */
   int     len;            /* no. of bytes returned */
   caddr_t buf;            /* pointer to data */
};

PRE(getpmsg)
{
   /* LiS getpmsg from http://www.gcom.com/home/linux/lis/ */
   /* int getpmsg(int fd, struct strbuf *ctrl, struct strbuf *data, 
      int *bandp, int *flagsp); */
   struct getpmsg_strbuf *ctrl;
   struct getpmsg_strbuf *data;
   MAYBE_PRINTF("getpmsg ( %d, %p, %p, %p, %p )\n",
		arg1,arg2,arg3,arg4,arg5);
   ctrl = (struct getpmsg_strbuf *)arg2;
   data = (struct getpmsg_strbuf *)arg3;
   if (ctrl && ctrl->maxlen > 0)
      SYSCALL_TRACK( pre_mem_write,tid, "getpmsg(ctrl)", 
		     (UInt)ctrl->buf, ctrl->maxlen);
   if (data && data->maxlen > 0)
      SYSCALL_TRACK( pre_mem_write,tid, "getpmsg(data)", 
		     (UInt)data->buf, data->maxlen);
   if (arg4)
      SYSCALL_TRACK( pre_mem_write,tid, "getpmsg(bandp)", 
		     (UInt)arg4, sizeof(int));
   if (arg5)
      SYSCALL_TRACK( pre_mem_write,tid, "getpmsg(flagsp)", 
		     (UInt)arg5, sizeof(int));
}

POST(getpmsg)
{
   struct getpmsg_strbuf *ctrl;
   struct getpmsg_strbuf *data;

   ctrl = (struct getpmsg_strbuf *)arg2;
   data = (struct getpmsg_strbuf *)arg3;
   if (res == 0 && ctrl && ctrl->len > 0) {
      VG_TRACK( post_mem_write, (UInt)ctrl->buf, ctrl->len);
   }
   if (res == 0 && data && data->len > 0) {
      VG_TRACK( post_mem_write, (UInt)data->buf, data->len);
   }
}

PRE(putpmsg)
{
   /* LiS putpmsg from http://www.gcom.com/home/linux/lis/ */
   /* int putpmsg(int fd, struct strbuf *ctrl, struct strbuf *data, 
      int band, int flags); */
   struct strbuf {
      int     maxlen;         /* no. of bytes in buffer */
      int     len;            /* no. of bytes returned */
      caddr_t buf;            /* pointer to data */
   };
   struct strbuf *ctrl;
   struct strbuf *data;
   MAYBE_PRINTF("putpmsg ( %d, %p, %p, %d, %d )\n",
		arg1,arg2,arg3,arg4,arg5);
   ctrl = (struct strbuf *)arg2;
   data = (struct strbuf *)arg3;
   if (ctrl && ctrl->len > 0)
      SYSCALL_TRACK( pre_mem_read,tid, "putpmsg(ctrl)",
		     (UInt)ctrl->buf, ctrl->len);
   if (data && data->len > 0)
      SYSCALL_TRACK( pre_mem_read,tid, "putpmsg(data)",
		     (UInt)data->buf, data->len);
}

PRE(getitimer)
{
   /* int getitimer(int which, struct itimerval *value); */
   MAYBE_PRINTF("getitimer ( %d, %p )\n", arg1, arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "getitimer(timer)", arg2, 
		  sizeof(struct itimerval) );
}

POST(getitimer)
{
   if (arg2 != (Addr)NULL) {
      VG_TRACK( post_mem_write,arg2, sizeof(struct itimerval));
   }
}

PRE(syslog)
{
   /* int syslog(int type, char *bufp, int len); */
   MAYBE_PRINTF("syslog (%d, %p, %d)\n",arg1,arg2,arg3);
   switch(arg1) {
   case 2: case 3: case 4:
      SYSCALL_TRACK( pre_mem_write, tid, "syslog(buf)", arg2, arg3);
      break;
   default: 
      break;
   }
}

POST(syslog)
{
   switch (arg1) {
   case 2: case 3: case 4:
      VG_TRACK( post_mem_write, arg2, arg3 );
      break;
   default:
      break;
   }
}

PRE(personality)
{
   /* int personality(unsigned long persona); */
   MAYBE_PRINTF("personality ( %d )\n", arg1);
}

PRE(chroot)
{
   /* int chroot(const char *path); */
   MAYBE_PRINTF("chroot ( %p )\n", arg1);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "chroot(path)", arg1 );
}

PRE(madvise)
{
   /* int madvise(void *start, size_t length, int advice ); */
   MAYBE_PRINTF("madvise ( %p, %d, %d )\n", arg1,arg2,arg3);
}

PRE(mremap)
{
   /* void* mremap(void * old_address, size_t old_size, 
      size_t new_size, unsigned long flags, void * new_address); */
   MAYBE_PRINTF("mremap ( %p, %d, %d, 0x%x, %p )\n", 
		arg1, arg2, arg3, arg4, arg5);
   
   res = mremap_segment((Addr)arg1, arg2, (Addr)arg5, arg3, arg4, tid);
}

PRE(nice)
{
   /* int nice(int inc); */
   MAYBE_PRINTF("nice ( %d )\n", arg1);
}

PRE(setresgid32)
{
   /* int setresgid(gid_t rgid, gid_t egid, gid_t sgid); */
   MAYBE_PRINTF("setresgid32 ( %d, %d, %d )\n", arg1, arg2, arg3);
}

PRE(setfsuid32)
{
   /* int setfsuid(uid_t fsuid); */
   MAYBE_PRINTF("setfsuid ( %d )\n", arg1);
}

PRE(_sysctl)
{
   /* int _sysctl(struct __sysctl_args *args); */
   MAYBE_PRINTF("_sysctl ( %p )\n", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "_sysctl(args)", arg1, 
		  sizeof(struct __sysctl_args) );
}

POST(_sysctl)
{
   VG_TRACK( post_mem_write, arg1, sizeof(struct __sysctl_args) );

}

PRE(sched_getscheduler)
{
   /* int sched_getscheduler(pid_t pid); */
   MAYBE_PRINTF("sched_getscheduler ( %d )\n", arg1);
}

PRE(sched_setscheduler)
{
   /* int sched_setscheduler(pid_t pid, int policy, 
      const struct sched_param *p); */
   MAYBE_PRINTF("sched_setscheduler ( %d, %d, %p )\n",arg1,arg2,arg3);
   if (arg3 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_read, tid,
		     "sched_setscheduler(struct sched_param *p)", 
		     arg3, sizeof(struct sched_param));
}

PRE(mlock)
{
   /* int mlock(const void * addr, size_t len) */
   MAYBE_PRINTF("mlock ( %p, %d )\n", arg1, arg2);
}

PRE(munlock)
{
   /* int munlock(const void * addr, size_t len) */
   MAYBE_PRINTF("munlock ( %p, %d )\n", arg1, arg2);
}

PRE(mlockall)
{
   /* int mlockall(int flags); */
   MAYBE_PRINTF("mlockall ( %x )\n", arg1);
}

PRE(munlockall)
{
   /* int munlock(const void * addr, size_t len) */
   MAYBE_PRINTF("munlock ( %p, %d )\n", arg1, arg2);
}

PRE(sched_get_priority_max)
{
   /* int sched_get_priority_max(int policy); */
   MAYBE_PRINTF("sched_get_priority_max ( %d )\n", arg1);
}

PRE(sched_get_priority_min)
{
   /* int sched_get_priority_min(int policy); */
   MAYBE_PRINTF("sched_get_priority_min ( %d )\n", arg1);
}

PRE(setpriority)
{
   /* int setpriority(int which, int who, int prio); */
   MAYBE_PRINTF("setpriority ( %d, %d, %d )\n", arg1, arg2, arg3);
}

PRE(getpriority)
{
   /* int getpriority(int which, int who); */
   MAYBE_PRINTF("getpriority ( %d, %d )\n", arg1, arg2);
}

PRE(setfsgid)
{
   /* int setfsgid(gid_t gid); */
   MAYBE_PRINTF("setfsgid ( %d )\n", arg1);
}

PRE(setregid)
{
   /* int setregid(gid_t rgid, gid_t egid); */
   MAYBE_PRINTF("setregid ( %d, %d )\n", arg1, arg2);
}

PRE(setresuid)
{
   /* int setresuid(uid_t ruid, uid_t euid, uid_t suid); */
   MAYBE_PRINTF("setresuid ( %d, %d, %d )\n", arg1, arg2, arg3);
}

PRE(setfsuid)
{
   /* int setfsuid(uid_t uid); */
   MAYBE_PRINTF("setfsuid ( %d )\n", arg1);
}

PRE(sendfile)
{
   /* ssize_t sendfile(int out_fd, int in_fd, off_t *offset, 
      size_t count) */
   MAYBE_PRINTF("sendfile ( %d, %d, %p, %d )\n",arg1,arg2,arg3,arg4);
   if (arg3 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "sendfile(offset)",
		     arg3, sizeof(off_t) );
}

POST(sendfile)
{
   VG_TRACK( post_mem_write, arg3, sizeof( off_t ) );
}

PRE(sendfile64)
{
   /* ssize_t sendfile64(int out_df, int in_fd, loff_t *offset,
      size_t count); */
   MAYBE_PRINTF("sendfile64 ( %d, %d, %p, %d )\n",arg1,arg2,arg3,arg4);
   if (arg3 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "sendfile64(offset)",
		     arg3, sizeof(loff_t) );
}

POST(sendfile64)
{
   if (arg3 != (UInt)NULL ) {
      VG_TRACK( post_mem_write, arg3, sizeof(loff_t) );
   }
}

PRE(pwrite64)
{
   /* ssize_t pwrite (int fd, const void *buf, size_t nbytes,
      off_t offset); */
   MAYBE_PRINTF("pwrite64 ( %d, %p, %d, %d )\n", arg1, arg2, arg3, arg4);
   SYSCALL_TRACK( pre_mem_read, tid, "pwrite(buf)", arg2, arg3 );
}

PRE(sync)
{
   /* int sync(); */
   MAYBE_PRINTF("sync ( )\n");
}

PRE(fstatfs)
{
   /* int fstatfs(int fd, struct statfs *buf); */
   MAYBE_PRINTF("fstatfs ( %d, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "stat(buf)", 
		  arg2, sizeof(struct statfs) );
}

POST(fstatfs)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct statfs) );
}

PRE(getsid)
{
   /* pid_t getsid(pid_t pid); */
   MAYBE_PRINTF("getsid ( %d )\n", arg1);
}

PRE(pread64)
{
   /* ssize_t pread64(int fd, void *buf, size_t count, off_t offset); */
   MAYBE_PRINTF("pread ( %d, %p, %d, %d ) ...\n",arg1,arg2,arg3,arg4);
   SYSCALL_TRACK( pre_mem_write, tid, "pread(buf)", arg2, arg3 );
}

POST(pread64)
{
   MAYBE_PRINTF("SYSCALL[%d]       pread ( %d, %p, %d, %d ) --> %d\n",
		VG_(getpid)(),
		arg1, arg2, arg3, arg4, res);
   if (res > 0) {
      VG_TRACK( post_mem_write, arg2, res );
   }
}

PRE(mknod)
{
   /* int mknod(const char *pathname, mode_t mode, dev_t dev); */
   MAYBE_PRINTF("mknod ( %p, 0x%x, 0x%x )\n", arg1, arg2, arg3 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "mknod(pathname)", arg1 );
}

PRE(flock)
{
   /* int flock(int fd, int operation); */
   MAYBE_PRINTF("flock ( %d, %d )\n", arg1, arg2 );
}

PRE(init_module)
{
   /* int init_module(const char *name, struct module *image); */
   MAYBE_PRINTF("init_module ( %p, %p )\n", arg1, arg2 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "init_module(name)", arg1 );
   SYSCALL_TRACK( pre_mem_read, tid, "init_module(image)", arg2, 
		  VKI_SIZEOF_STRUCT_MODULE );
}

PRE(ioperm)
{
   /* int ioperm(unsigned long from, unsigned long num, int turn_on); */
   MAYBE_PRINTF("ioperm ( %d, %d, %d )\n", arg1, arg2, arg3 );
}

PRE(capget)
{
   /* int capget(cap_user_header_t header, cap_user_data_t data); */
   MAYBE_PRINTF("capget ( %p, %p )\n", arg1, arg2 );
   SYSCALL_TRACK( pre_mem_read, tid, "capget(header)", arg1, 
		  sizeof(vki_cap_user_header_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "capget(data)", arg2, 
		  sizeof( vki_cap_user_data_t) );
}

POST(capget)
{
   if (arg2 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg2, sizeof( vki_cap_user_data_t) );
}

PRE(capset)
{
   SYSCALL_TRACK( pre_mem_read, tid, "capset(header)", 
		  arg1, sizeof(vki_cap_user_header_t) );
   SYSCALL_TRACK( pre_mem_read, tid, "capset(data)", 
		  arg2, sizeof( vki_cap_user_data_t) );
}

// Pre_read a char** argument.
void pre_argv_envp(Addr a, ThreadId tid, Char* s1, Char* s2)
{
   while (True) {
      Addr a_deref = deref_Addr( tid, a, s1 );
      if (0 == a_deref)
         break;
      SYSCALL_TRACK( pre_mem_read_asciiz, tid, s2, a_deref );
      a += sizeof(char*);
   }
}

PRE(execve)
{
   /* int execve (const char *filename, 
      char *const argv [], 
      char *const envp[]); */
   MAYBE_PRINTF("execve ( %p(%s), %p, %p )\n", arg1, arg1, arg2, arg3);

   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "execve(filename)", arg1 );
   pre_argv_envp( arg2, tid, "execve(argv)", "execve(argv[i])" );
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
	 res = ret;
	 return;
      }
      /* just look for any X bit set
	 XXX do proper permissions check?
       */
      if ((st.st_mode & 0111) == 0) {
	 res = -VKI_EACCES;
	 return;
      }
   }

   /* Resistance is futile.  Nuke all other threads.  POSIX mandates
      this. (Really, nuke them all, since the new process will make
      its own new thread.) */
   VG_(nuke_all_threads_except)( VG_INVALID_THREADID );

   {
      /* Make the LD_LIBRARY_PATH/LD_PRELOAD disappear so that the
	 child doesn't get our libpthread and other stuff.  This is
	 done unconditionally, since if we are tracing the child,
	 stage1/2 will set up the appropriate client environment. */
      Int i;
      Char** envp = (Char**)arg3;
      Char*  ld_preload_str = NULL;
      Char*  ld_library_path_str = NULL;

      if (envp != NULL) {
	 Char *buf;

	 for (i = 0; envp[i] != NULL; i++) {
	    if (VG_(strncmp)(envp[i], "LD_PRELOAD=", 11) == 0)
	       ld_preload_str = &envp[i][11];
	    if (VG_(strncmp)(envp[i], "LD_LIBRARY_PATH=", 16) == 0)
	       ld_library_path_str = &envp[i][16];
	 }

	 buf = VG_(arena_malloc)(VG_AR_CORE, VG_(strlen)(VG_(libdir)) + 20);

         VG_(sprintf)(buf, "%s*/vg_inject.so", VG_(libdir));
         VG_(mash_colon_env)(ld_preload_str, buf);

         VG_(sprintf)(buf, "%s*/vgpreload_*.so", VG_(libdir));
         VG_(mash_colon_env)(ld_preload_str, buf);

         VG_(sprintf)(buf, "%s*", VG_(libdir));
         VG_(mash_colon_env)(ld_library_path_str, buf);

         VG_(env_unsetenv)(envp, VALGRINDCLO);

	 /* XXX if variable becomes empty, remove it completely? */
      }
   }

   if (VG_(clo_trace_children)) {
      /* If we're tracing the children, then we need to start it
	 with our starter+arguments.
      */
      Int i;
      Char *exec = (Char *)arg1;
      Char **env = (Char **)arg3;
      Char *cp;
      Char *exename;
      Bool sawexec = False;
      Char *optvar;
      Int  optlen;

      optlen = 1;
      for(i = 0; i < VG_(vg_argc); i++)
	 optlen += VG_(strlen)(VG_(vg_argv)[i]) + 1;

      /* All these are leaked - we're either going to exec, or panic
	 when we fail. */
      exename  = VG_(arena_malloc)(VG_AR_CORE, 64);
      exec = VG_(arena_malloc)(VG_AR_CORE, VG_(strlen)(exec) + 7 /* --exec= */ + 1 /* \0 */);

      VG_(sprintf)(exec, "--exec=%s", (Char *)arg1);
      VG_(sprintf)(exename, "/proc/self/fd/%d", VG_(vgexecfd));

      optlen += VG_(strlen)(exec)+1;

      optvar = VG_(arena_malloc)(VG_AR_CORE, optlen);

      /* valgrind arguments */
      cp = optvar;
      
      for(i = 1; i < VG_(vg_argc); i++) {
	 Char *arg = VG_(vg_argv)[i];
	 Int len;
	 
	 if (VG_(memcmp)(arg, "--exec=", 7) == 0) {
	    /* replace existing --exec= arg */
	    sawexec = True;
	    arg = exec;
	 } else if (VG_(strcmp)(VG_(vg_argv)[i], "--") == 0)
	    break;

	 len = VG_(strlen)(arg);
	 VG_(memcpy)(cp, arg, len);
	 cp += len;
	 *cp++ = '\01';
      }

      if (!sawexec) {
	 Int execlen = VG_(strlen)(exec);
	 VG_(memcpy)(cp, exec, execlen);
	 cp += execlen;
	 *cp++ = '\01';
      }
      *cp = '\0';

      VG_(env_setenv)(&env, VALGRINDCLO, optvar);

      arg1 = (UInt)exename;
      arg3 = (UInt)env;
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
      vki_ksigset_t allsigs;
      vki_ksiginfo_t info;
      static const struct vki_timespec zero = { 0, 0 };
    
      VG_(ksigfillset)(&allsigs);
      while(VG_(ksigtimedwait)(&allsigs, &info, &zero) > 0)
	 ;

      VG_(ksigprocmask)(VKI_SIG_SETMASK, &tst->sig_mask, NULL);
   }

   res = VG_(do_syscall)(__NR_execve, arg1, arg2, arg3);

   /* If we got here, then the execve failed.  We've already made too much of a mess
      of ourselves to continue, so we have to abort. */
   VG_(message)(Vg_UserMsg, "execve(%p \"%s\", %p, %p) failed, errno %d",
		arg1, arg1, arg2, arg3, -res);
   VG_(core_panic)("EXEC FAILED: I can't recover from execve() failing, so I'm dying.\n"
		   "Add more stringent tests in PRE(execve), or work out how to recover.");   
}

PRE(access)
{
   /* int access(const char *pathname, int mode); */
   MAYBE_PRINTF("access ( %p, %d )\n", arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "access(pathname)", arg1 );
}

PRE(alarm)
{
   /* unsigned int alarm(unsigned int seconds); */
   MAYBE_PRINTF("alarm ( %d )\n", arg1);
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
   MAYBE_PRINTF("brk ( %p ) --> ",arg1);

   res = do_brk(arg1);

   MAYBE_PRINTF("0x%x\n", res);

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
   MAYBE_PRINTF("chdir ( %p )\n", arg1);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "chdir(path)", arg1 );
}

PRE(chmod)
{
   /* int chmod(const char *path, mode_t mode); */
   MAYBE_PRINTF("chmod ( %p, %d )\n", arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "chmod(path)", arg1 );
}

PRE(chown)
{
   /* int chown(const char *path, uid_t owner, gid_t group); */
   MAYBE_PRINTF("chown ( %p, 0x%x, 0x%x )\n", arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "chown(path)", arg1 );
}

PREALIAS(chown32, chown);
PREALIAS(lchown32, chown);

PRE(close)
{
   /* int close(int fd); */
   MAYBE_PRINTF("close ( %d )\n",arg1);
   /* Detect and negate attempts by the client to close Valgrind's
      logfile fd ... */
   if (!fd_allowed(arg1, "close", tid))
      res = -VKI_EBADF;
}

POST(close)
{
   if(VG_(clo_track_fds)) record_fd_close(tid, arg1);
}

PRE(dup)
{
   /* int dup(int oldfd); */
   MAYBE_PRINTF("dup ( %d ) --> ", arg1);
}

POST(dup)
{
   MAYBE_PRINTF("%d\n", res);
   if (!fd_allowed(res, "dup", tid)) {
      VG_(close)(res);
      res = -VKI_EMFILE;
   } else {
      if(VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(resolve_filename)(res));
   }
}

PRE(dup2)
{
   /* int dup2(int oldfd, int newfd); */
   MAYBE_PRINTF("dup2 ( %d, %d ) ...\n", arg1,arg2);
   if (!fd_allowed(arg2, "dup2", tid))
      res = -VKI_EBADF;
}

POST(dup2)
{
   MAYBE_PRINTF("SYSCALL[%d]       dup2 ( %d, %d ) = %d\n", 
		VG_(getpid)(), 
		arg1, arg2, res);
   if(VG_(clo_track_fds))
      record_fd_open(tid, res, VG_(resolve_filename)(res));
}

PRE(fcntl)
{
   /* int fcntl(int fd, int cmd, int arg); */
   MAYBE_PRINTF("fcntl ( %d, %d, %d )\n",arg1,arg2,arg3);
}

POST(fcntl)
{
   if (arg2 == VKI_F_DUPFD)
      if (VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(resolve_filename)(res));
}

PRE(fchdir)
{
   /* int fchdir(int fd); */
   MAYBE_PRINTF("fchdir ( %d )\n", arg1);
}

PRE(fchown)
{
   /* int fchown(int filedes, uid_t owner, gid_t group); */
   MAYBE_PRINTF("fchown ( %d, %d, %d )\n", arg1,arg2,arg3);
}

PREALIAS(fchown32, fchown);

PRE(fchmod)
{
   /* int fchmod(int fildes, mode_t mode); */
   MAYBE_PRINTF("fchmod ( %d, %d )\n", arg1,arg2);
}

PRE(fcntl64)
{
   /* int fcntl64(int fd, int cmd, int arg); */
   MAYBE_PRINTF("fcntl64 ( %d, %d, %d )\n", arg1,arg2,arg3);
}

POST(fcntl64)
{
   if (arg2 == VKI_F_DUPFD)
      if(VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(resolve_filename)(res));
}

PRE(fstat)
{
   /* int fstat(int filedes, struct stat *buf); */
   MAYBE_PRINTF("fstat ( %d, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "fstat", arg2, sizeof(struct stat) );
}

POST(fstat)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct stat) );
}

static vki_ksigset_t fork_saved_mask;

PRE(fork)
{
   vki_ksigset_t mask;

   vg_assert(VG_(gettid)() == VG_(main_pid));

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(ksigfillset)(&mask);
   VG_(ksigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   /* pid_t fork(void); */
   MAYBE_PRINTF("fork ()\n");

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
      VG_(ksigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } else {
      MAYBE_PRINTF("   fork: process %d created child %d\n", VG_(main_pid), res);

      do_atfork_parent(tid);

      /* restore signal mask */
      VG_(ksigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }
}

PRE(clone)
{
   MAYBE_PRINTF("clone ( %d, %p, %p, %p, %p )\n",arg1,arg2,arg3,arg4,arg5);

   if (arg2 == 0 &&
       (arg1 == (VKI_CLONE_CHILD_CLEARTID|VKI_CLONE_CHILD_SETTID|VKI_SIGCHLD)
     || arg1 == (VKI_CLONE_PARENT_SETTID|VKI_SIGCHLD))) 
   {
      before_fork(tid, tst);
      res = VG_(do_syscall)(SYSNO, arg1, arg2, arg3, arg4, arg5);
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
   MAYBE_PRINTF("fsync ( %d )\n", arg1);
}

PRE(ftruncate)
{
   /* int ftruncate(int fd, size_t length); */
   MAYBE_PRINTF("ftruncate ( %d, %d )\n", arg1,arg2);
}

PRE(ftruncate64)
{
   /* int ftruncate64(int fd, off64_t length); */
   MAYBE_PRINTF("ftruncate64 ( %d, %lld )\n", 
		arg1,arg2|((long long) arg3 << 32));
}

PRE(getdents)
{
   /* int getdents(unsigned int fd, struct dirent *dirp, 
      unsigned int count); */
   MAYBE_PRINTF("getdents ( %d, %p, %d )\n",arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_write, tid, "getdents(dirp)", arg2, arg3 );
}

POST(getdents)
{
   if (res > 0)
      VG_TRACK( post_mem_write, arg2, res );
}

PRE(getdents64)
{
   /* int getdents(unsigned int fd, struct dirent64 *dirp, 
      unsigned int count); */
   MAYBE_PRINTF("getdents64 ( %d, %p, %d )\n",arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_write, tid, "getdents64(dirp)", arg2, arg3 );
}

POST(getdents64)
{
   if (res > 0)
      VG_TRACK( post_mem_write, arg2, res );
}

PRE(getgroups)
{
   /* int getgroups(int size, gid_t list[]); */
   MAYBE_PRINTF("getgroups ( %d, %p )\n", arg1, arg2);
   if (arg1 > 0)
      SYSCALL_TRACK( pre_mem_write, tid, "getgroups(list)", arg2, 
		     arg1 * sizeof(gid_t) );
}

POST(getgroups)
{
   if (arg1 > 0 && res > 0)
      VG_TRACK( post_mem_write, arg2, res * sizeof(gid_t) );
}

PREALIAS(getgroups32, getgroups);
POSTALIAS(getgroups32, getgroups);

PRE(getcwd)
{
   /* char *getcwd(char *buf, size_t size);  (but see comment below) */
   MAYBE_PRINTF("getcwd ( %p, %d )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "getcwd(buf)", arg1, arg2 );
}

POST(getcwd)
{
   if (res != (Addr)NULL)
      VG_TRACK( post_mem_write, arg1, res );
}

PRE(geteuid)
{
   /* uid_t geteuid(void); */
   MAYBE_PRINTF("geteuid ( )\n");
}

PRE(geteuid32)
{
   /* ?? uid_t geteuid32(void); */
   MAYBE_PRINTF("geteuid32(?) ( )\n");
}

PRE(getegid)
{
   /* gid_t getegid(void); */
   MAYBE_PRINTF("getegid ()\n");
}

PRE(getegid32)
{
   /* gid_t getegid32(void); */
   MAYBE_PRINTF("getegid32 ()\n");
}

PRE(getgid)
{
   /* gid_t getgid(void); */
   MAYBE_PRINTF("getgid ()\n");
}

PRE(getgid32)
{
   /* gid_t getgid32(void); */
   MAYBE_PRINTF("getgid32 ()\n");
}

PRE(getpid)
{
   /* pid_t getpid(void); */
   MAYBE_PRINTF("getpid ()\n");
}

PRE(getpgid)
{
   /* pid_t getpgid(pid_t pid); */
   MAYBE_PRINTF("getpgid ( %d )\n", arg1);
}

PRE(getpgrp)
{
   /* pid_t getpgrp(void); */
   MAYBE_PRINTF("getpgrp ()\n");
}

PRE(getppid)
{
   /* pid_t getppid(void); */
   MAYBE_PRINTF("getppid ()\n");
}

PRE(getresgid)
{
   /* int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid); */
   MAYBE_PRINTF("getresgid ( %p, %p, %p )\n", arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_write, tid, "getresgid(rgid)", 
		  arg1, sizeof(gid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresgid(egid)", 
		  arg2, sizeof(gid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresgid(sgid)", 
		  arg3, sizeof(gid_t) );
}

POST(getresgid)
{
   if (res == 0) {
      VG_TRACK( post_mem_write, arg1, sizeof(gid_t) );
      VG_TRACK( post_mem_write, arg2, sizeof(gid_t) );
      VG_TRACK( post_mem_write, arg3, sizeof(gid_t) );
   }
}

PRE(getresgid32)
{
   /* int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid); */
   MAYBE_PRINTF("getresgid32 ( %p, %p, %p )\n", arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_write, tid, "getresgid32(rgid)", 
		  arg1, sizeof(gid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresgid32(egid)", 
		  arg2, sizeof(gid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresgid32(sgid)", 
		  arg3, sizeof(gid_t) );
}

POST(getresgid32)
{
   if (res == 0) {
      VG_TRACK( post_mem_write, arg1, sizeof(gid_t) );
      VG_TRACK( post_mem_write, arg2, sizeof(gid_t) );
      VG_TRACK( post_mem_write, arg3, sizeof(gid_t) );
   }
}

PRE(getresuid)
{
   /* int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid); */
   MAYBE_PRINTF("getresuid ( %p, %p, %p )\n", arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_write, tid, "getresuid(ruid)", 
		  arg1, sizeof(uid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresuid(euid)", 
		  arg2, sizeof(uid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresuid(suid)", 
		  arg3, sizeof(uid_t) );
}

POST(getresuid)
{
   if (res == 0) {
      VG_TRACK( post_mem_write, arg1, sizeof(uid_t) );
      VG_TRACK( post_mem_write, arg2, sizeof(uid_t) );
      VG_TRACK( post_mem_write, arg3, sizeof(uid_t) );
   }
}

PRE(getresuid32)
{
   /* int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid); */
   MAYBE_PRINTF("getresuid32 ( %p, %p, %p )\n", arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_write, tid, "getresuid32(ruid)", 
		  arg1, sizeof(uid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresuid32(euid)", 
		  arg2, sizeof(uid_t) );
   SYSCALL_TRACK( pre_mem_write, tid, "getresuid32(suid)", 
		  arg3, sizeof(uid_t) );
}

POST(getresuid32)
{
   if (res == 0) {
      VG_TRACK( post_mem_write, arg1, sizeof(uid_t) );
      VG_TRACK( post_mem_write, arg2, sizeof(uid_t) );
      VG_TRACK( post_mem_write, arg3, sizeof(uid_t) );
   }
}

PRE(getrlimit)
{
   /* int getrlimit (int resource, struct rlimit *rlim); */
   MAYBE_PRINTF("getrlimit ( %d, %p )\n", arg1,arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "getrlimit(rlim)", arg2, 
		  sizeof(struct rlimit) );
}

POST(getrlimit)
{
   if (res == 0)
      VG_TRACK( post_mem_write, arg2, sizeof(struct rlimit) );

   if (res == 0 && arg1 == VKI_RLIMIT_NOFILE)
      ((struct rlimit *)arg2)->rlim_cur = VG_(max_fd);
}

PREALIAS(ugetrlimit, getrlimit);
POSTALIAS(ugetrlimit, getrlimit);

PRE(getrusage)
{
   /* int getrusage (int who, struct rusage *usage); */
   MAYBE_PRINTF("getrusage ( %d, %p )\n", arg1,arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "getrusage(usage)", arg2, 
		  sizeof(struct rusage) );
}

POST(getrusage)
{
   if (res == 0)
      VG_TRACK( post_mem_write,arg2, sizeof(struct rusage) );
}

PRE(gettimeofday)
{
   /* int gettimeofday(struct timeval *tv, struct timezone *tz); */
   MAYBE_PRINTF("gettimeofday ( %p, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "gettimeofday(tv)", arg1, 
		  sizeof(struct timeval) );
   if (arg2 != 0)
      SYSCALL_TRACK( pre_mem_write, tid, "gettimeofday(tz)", arg2, 
		     sizeof(struct timezone) );
}

POST(gettimeofday)
{
   if (res == 0) {
      VG_TRACK( post_mem_write, arg1, sizeof(struct timeval) );
      if (arg2 != 0)
	 VG_TRACK( post_mem_write, arg2, sizeof(struct timezone) );
   }
}

PRE(getuid)
{
   /* uid_t getuid(void); */
   MAYBE_PRINTF("getuid ( )\n");
}

PRE(getuid32)
{
   /* ???uid_t getuid32(void); */
   MAYBE_PRINTF("getuid32 ( )\n");
}

PRE(ipc)
{
   MAYBE_PRINTF("ipc ( %d, %d, %d, %d, %p, %d )\n",
		arg1,arg2,arg3,arg4,arg5,arg6);
   switch (arg1 /* call */) {
   case 1: /* IPCOP_semop */
      SYSCALL_TRACK( pre_mem_read, tid, "semop(sops)", arg5, 
		     arg3 * sizeof(struct sembuf) );
      break;
   case 2: /* IPCOP_semget */
   case 3: /* IPCOP_semctl */
      break;
   case 11: /* IPCOP_msgsnd */
   {
      struct msgbuf *msgp = (struct msgbuf *)arg5;
      Int msgsz = arg3;

      SYSCALL_TRACK( pre_mem_read, tid, "msgsnd(msgp->mtype)", 
		     (UInt)&msgp->mtype, sizeof(msgp->mtype) );
      SYSCALL_TRACK( pre_mem_read, tid, "msgsnd(msgp->mtext)", 
		     (UInt)msgp->mtext, msgsz );
      break;
   }
   case 12: /* IPCOP_msgrcv */
   {
      struct msgbuf *msgp;
      Int msgsz = arg3;
 
      msgp = (struct msgbuf *)deref_Addr( tid,
					  (Addr) (&((struct ipc_kludge *)arg5)->msgp),
					  "msgrcv(msgp)" );

      SYSCALL_TRACK( pre_mem_write, tid, "msgrcv(msgp->mtype)", 
		     (UInt)&msgp->mtype, sizeof(msgp->mtype) );
      SYSCALL_TRACK( pre_mem_write, tid, "msgrcv(msgp->mtext)", 
		     (UInt)msgp->mtext, msgsz );
      break;
   }
   case 13: /* IPCOP_msgget */
      break;
   case 14: /* IPCOP_msgctl */
   {
      switch (arg3 /* cmd */) {
      case IPC_STAT:
	 SYSCALL_TRACK( pre_mem_write, tid, "msgctl(buf)", arg5, 
			sizeof(struct msqid_ds) );
	 break;
      case IPC_SET:
	 SYSCALL_TRACK( pre_mem_read, tid, "msgctl(buf)", arg5, 
			sizeof(struct msqid_ds) );
	 break;
#                    if defined(IPC_64)
      case IPC_STAT|IPC_64:
	 SYSCALL_TRACK( pre_mem_write, tid, "msgctl(buf)", arg5, 
			sizeof(struct msqid64_ds) );
	 break;
#                    endif
#                    if defined(IPC_64)
      case IPC_SET|IPC_64:
	 SYSCALL_TRACK( pre_mem_read, tid, "msgctl(buf)", arg5, 
			sizeof(struct msqid64_ds) );
	 break;
#                    endif
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
	 res = -VKI_EINVAL;
      break;
   }
   case 22: /* IPCOP_shmdt */
      if (!valid_client_addr(arg1, 1, tid, "shmdt"))
	 res = -VKI_EINVAL;
      break;
   case 23: /* IPCOP_shmget */
      break;
   case 24: /* IPCOP_shmctl */
      /* Subject: shmctl: The True Story
	 Date: Thu, 9 May 2002 18:07:23 +0100 (BST)
	 From: Reuben Thomas <rrt@mupsych.org>
	 To: Julian Seward <jseward@acm.org>

	 1. As you suggested, the syscall subop is in arg1.

	 2. There are a couple more twists, so the arg order
	 is actually:

	 arg1 syscall subop
	 arg2 file desc
	 arg3 shm operation code (can have IPC_64 set)
	 arg4 0 ??? is arg3-arg4 a 64-bit quantity when IPC_64
	 is defined?
	 arg5 pointer to buffer

	 3. With this in mind, I've amended the case as below:
      */
   {
      UInt cmd = arg3;
      Bool out_arg = False;
      if ( arg5 ) {
#                    if defined(IPC_64)
	 cmd = cmd & (~IPC_64);
#                    endif
	 out_arg = cmd == SHM_STAT || cmd == IPC_STAT;
	 if ( out_arg )
	    SYSCALL_TRACK( pre_mem_write, tid, 
                           "shmctl(SHM_STAT or IPC_STAT,buf)", 
                           arg5, sizeof(struct shmid_ds) );
	 else
	    SYSCALL_TRACK( pre_mem_read, tid, 
                           "shmctl(SHM_XXXX,buf)", 
                           arg5, sizeof(struct shmid_ds) );
      }
   }
   break;
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
      break;
   case 2: /* IPCOP_semget */
   case 3: /* IPCOP_semctl */
      break;
   case 11: /* IPCOP_msgsnd */
      break;
   case 12: /* IPCOP_msgrcv */
   {
      struct msgbuf *msgp;
 
      msgp = (struct msgbuf *)deref_Addr( tid,
					  (Addr) (&((struct ipc_kludge *)arg5)->msgp),
					  "msgrcv(msgp)" );
      if ( res > 0 ) {
	 VG_TRACK( post_mem_write, (UInt)&msgp->mtype, 
		   sizeof(msgp->mtype) );
	 VG_TRACK( post_mem_write, (UInt)msgp->mtext, res );
      }
      break;
   }
   case 13: /* IPCOP_msgget */
      break;
   case 14: /* IPCOP_msgctl */
   {
      switch (arg3 /* cmd */) {
      case IPC_STAT:
	 if ( res > 0 ) {
	    VG_TRACK( post_mem_write, arg5, 
		      sizeof(struct msqid_ds) );
	 }
	 break;
      case IPC_SET:
	 break;
#                    if defined(IPC_64)
      case IPC_STAT|IPC_64:
	 if ( res > 0 ) {
	    VG_TRACK( post_mem_write, arg5, 
		      sizeof(struct msqid64_ds) );
	 }
	 break;
#                    endif
#                    if defined(IPC_64)
      case IPC_SET|IPC_64:
	 break;
#                    endif
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
      VG_TRACK( post_mem_write, arg4, sizeof( ULong ) );

      addr = deref_Addr ( tid, arg4, "shmat(addr)" );
      if ( addr > 0 ) { 
	 UInt segmentSize = get_shm_size ( shmid );
	 if ( segmentSize > 0 ) {
	    UInt prot;
	    /* we don't distinguish whether it's read-only or
	     * read-write -- it doesn't matter really. */
	    VG_TRACK( new_mem_mmap, addr, segmentSize, 
		      True, True, False );

	    prot = VKI_PROT_READ|VKI_PROT_WRITE;
	    if (!(shmflag & 010000)) /* = SHM_RDONLY */
	       prot &= ~VKI_PROT_WRITE;
	    VG_(map_segment)(addr, segmentSize, prot,
			     SF_SHARED | SF_SHM);
	 }
      }
      break;
   }
   case 22: /* IPCOP_shmdt */
   {
      Segment *s = VG_(find_segment)(arg1);

      if (s != NULL && (s->flags & SF_SHM) && VG_(seg_contains)(s, arg1, 1)) {
	 VG_TRACK( die_mem_munmap, s->addr, s->len );
	 VG_(unmap_range)(s->addr, s->len);
      }
      break;
   }
   case 23: /* IPCOP_shmget */
      break;
   case 24: /* IPCOP_shmctl */
      /* Subject: shmctl: The True Story
	 Date: Thu, 9 May 2002 18:07:23 +0100 (BST)
	 From: Reuben Thomas <rrt@mupsych.org>
	 To: Julian Seward <jseward@acm.org>

	 1. As you suggested, the syscall subop is in arg1.

	 2. There are a couple more twists, so the arg order
	 is actually:

	 arg1 syscall subop
	 arg2 file desc
	 arg3 shm operation code (can have IPC_64 set)
	 arg4 0 ??? is arg3-arg4 a 64-bit quantity when IPC_64
	 is defined?
	 arg5 pointer to buffer

	 3. With this in mind, I've amended the case as below:
      */
   {
      UInt cmd = arg3;
      Bool out_arg = False;
      if ( arg5 ) {
#                    if defined(IPC_64)
	 cmd = cmd & (~IPC_64);
#                    endif
	 out_arg = cmd == SHM_STAT || cmd == IPC_STAT;
      }
      if ( arg5 && res == 0 && out_arg )
	 VG_TRACK( post_mem_write, arg5, 
		   sizeof(struct shmid_ds) );
   }
   break;
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
   MAYBE_PRINTF("ioctl ( %d, 0x%x, %p )\n",arg1,arg2,arg3);
   switch (arg2 /* request */) {
   case TCSETS:
   case TCSETSW:
   case TCSETSF:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(TCSET{S,SW,SF})", arg3, 
		     VKI_SIZEOF_STRUCT_TERMIOS );
      break; 
   case TCGETS:
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(TCGETS)", arg3, 
		     VKI_SIZEOF_STRUCT_TERMIOS );
      break;
   case TCSETA:
   case TCSETAW:
   case TCSETAF:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(TCSET{A,AW,AF})", arg3,
		     VKI_SIZEOF_STRUCT_TERMIO );
      break;
   case TCGETA:
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(TCGETA)", arg3,
		     VKI_SIZEOF_STRUCT_TERMIO );
      break;
   case TCSBRK:
   case TCXONC:
   case TCSBRKP:
   case TCFLSH:
      /* These just take an int by value */
      break;
   case TIOCGWINSZ:
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(TIOCGWINSZ)", arg3, 
		     sizeof(struct winsize) );
      break;
   case TIOCSWINSZ:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(TIOCSWINSZ)", arg3, 
		     sizeof(struct winsize) );
      break;
   case TIOCLINUX:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(TIOCLINUX)", arg3, 
		     sizeof(char *) );
      if (*(char *)arg3 == 11) {
	 SYSCALL_TRACK( pre_mem_read, tid, "ioctl(TIOCLINUX, 11)", 
			arg3, 2 * sizeof(char *) );
      }
      break;
   case TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(TIOCGPGRP)", arg3,
		     sizeof(pid_t) );
      break;
   case TIOCSPGRP:
      /* Set a process group ID? */
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(TIOCGPGRP)", arg3,
		     sizeof(pid_t) );
      break;
   case TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(TIOCGPTN)", 
		     arg3, sizeof(int) );
      break;
   case TIOCSCTTY:
      /* Just takes an int value.  */
      break;
   case TIOCSPTLCK: /* Lock/unlock Pty */
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(TIOCSPTLCK)", 
		     arg3, sizeof(int) );
      break;
   case FIONBIO:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(FIONBIO)", 
		     arg3, sizeof(int) );
      break;
   case FIOASYNC:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(FIOASYNC)", 
		     arg3, sizeof(int) );
      break;
   case FIONREAD:                /* identical to SIOCINQ */
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(FIONREAD)", 
		     arg3, sizeof(int) );
      break;

      /* If you get compilation problems here, change the #if
	 1 to #if 0 and get rid of <scsi/sg.h> in
	 vg_unsafe.h. */
#       if 1
   case SG_SET_COMMAND_Q:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(SG_SET_COMMAND_Q)", 
		     arg3, sizeof(int) );
      break;
#           if defined(SG_IO)
   case SG_IO:
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(SG_IO)", arg3, 
		     sizeof(struct sg_io_hdr) );
      break;
#           endif /* SG_IO */
   case SG_GET_SCSI_ID:
      /* Note: sometimes sg_scsi_id is called sg_scsi_id_t */
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(SG_GET_SCSI_ID)", arg3, 
		     sizeof(struct sg_scsi_id) );
      break;
   case SG_SET_RESERVED_SIZE:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(SG_SET_RESERVED_SIZE)", 
		     arg3, sizeof(int) );
      break;
   case SG_SET_TIMEOUT:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(SG_SET_TIMEOUT)", arg3, 
		     sizeof(int) );
      break;
   case SG_GET_RESERVED_SIZE:
      SYSCALL_TRACK( pre_mem_write, tid, 
		     "ioctl(SG_GET_RESERVED_SIZE)", arg3, 
		     sizeof(int) );
      break;
   case SG_GET_TIMEOUT:
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(SG_GET_TIMEOUT)", arg3, 
		     sizeof(int) );
      break;
   case SG_GET_VERSION_NUM:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(SG_GET_VERSION_NUM)", 
		     arg3, sizeof(int) );
      break;
#       endif

   case VKI_IIOCGETCPS:
      /* In early 2.4 kernels, ISDN_MAX_CHANNELS was only defined
       * when KERNEL was. I never saw a larger value than 64 though */
#              ifndef ISDN_MAX_CHANNELS
#              define ISDN_MAX_CHANNELS 64
#              endif
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(IIOCGETCPS)", arg3,
		     ISDN_MAX_CHANNELS 
		     * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(IIOCNETGPN)",
		     (UInt)&((isdn_net_ioctl_phone *)arg3)->name,
		     sizeof(((isdn_net_ioctl_phone *)arg3)->name) );
      SYSCALL_TRACK( pre_mem_write, tid, "ioctl(IIOCNETGPN)", arg3,
		     sizeof(isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case SIOCGIFINDEX:
   case SIOCGIFFLAGS:        /* get flags                    */
   case SIOCGIFHWADDR:       /* Get hardware address         */
   case SIOCGIFMTU:          /* get MTU size                 */
   case SIOCGIFADDR:         /* get PA address               */
   case SIOCGIFNETMASK:      /* get network PA mask          */
   case SIOCGIFMETRIC:       /* get metric                   */
   case SIOCGIFMAP:          /* Get device parameters        */
   case SIOCGIFTXQLEN:       /* Get the tx queue length      */
   case SIOCGIFDSTADDR:      /* get remote PA address        */
   case SIOCGIFBRDADDR:      /* get broadcast PA address     */
   case SIOCGIFNAME:         /* get iface name               */
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(SIOCGIFINDEX)", arg3, 
		     sizeof(struct ifreq));
      break;
   case SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 SYSCALL_TRACK( pre_mem_write,"ioctl(SIOCGIFCONF)", arg3, 
	 sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,res);
	 if (!VG_(is_kerror)(res) && res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct ifconf));
      */
      SYSCALL_TRACK( pre_mem_read,tid, "ioctl(SIOCGIFCONF)", arg3, 
		     sizeof(struct ifconf));
      if ( arg3 ) {
	 // TODO len must be readable and writable
	 // buf pointer only needs to be readable
	 struct ifconf *ifc = (struct ifconf *) arg3;
	 SYSCALL_TRACK( pre_mem_write,tid, "ioctl(SIOCGIFCONF).ifc_buf",
			(Addr)(ifc->ifc_buf), (UInt)(ifc->ifc_len) );
      }
      break;
   case SIOCGSTAMP:
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(SIOCGSTAMP)", arg3, 
		     sizeof(struct timeval));
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case SIOCOUTQ:
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(SIOCOUTQ)", arg3, 
		     sizeof(int));
      break;
   case SIOCGRARP:           /* get RARP table entry         */
   case SIOCGARP:            /* get ARP table entry          */
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(SIOCGARP)", arg3, 
		     sizeof(struct arpreq));
      break;
                    
   case SIOCSIFFLAGS:        /* set flags                    */
   case SIOCSIFMAP:          /* Set device parameters        */
   case SIOCSIFTXQLEN:       /* Set the tx queue length      */
   case SIOCSIFDSTADDR:      /* set remote PA address        */
   case SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case SIOCSIFNETMASK:      /* set network PA mask          */
   case SIOCSIFMETRIC:       /* set metric                   */
   case SIOCSIFADDR:         /* set PA address               */
   case SIOCSIFMTU:          /* set MTU size                 */
   case SIOCSIFHWADDR:       /* set hardware address         */
      SYSCALL_TRACK( pre_mem_read,tid,"ioctl(SIOCSIFFLAGS)", arg3, 
		     sizeof(struct ifreq));
      break;
      /* Routing table calls.  */
   case SIOCADDRT:           /* add routing table entry      */
   case SIOCDELRT:           /* delete routing table entry   */
      SYSCALL_TRACK( pre_mem_read,tid,"ioctl(SIOCADDRT/DELRT)", arg3, 
		     sizeof(struct rtentry));
      break;

      /* RARP cache control calls. */
   case SIOCDRARP:           /* delete RARP table entry      */
   case SIOCSRARP:           /* set RARP table entry         */
      /* ARP cache control calls. */
   case SIOCSARP:            /* set ARP table entry          */
   case SIOCDARP:            /* delete ARP table entry       */
      SYSCALL_TRACK( pre_mem_read,tid, "ioctl(SIOCSIFFLAGS)", arg3, 
		     sizeof(struct ifreq));
      break;

   case SIOCSPGRP:
      SYSCALL_TRACK( pre_mem_read, tid, "ioctl(SIOCSPGRP)", arg3, 
		     sizeof(int) );
      break;

      /* linux/soundcard interface (OSS) */
   case SNDCTL_SEQ_GETOUTCOUNT:
   case SNDCTL_SEQ_GETINCOUNT:
   case SNDCTL_SEQ_PERCMODE:
   case SNDCTL_SEQ_TESTMIDI:
   case SNDCTL_SEQ_RESETSAMPLES:
   case SNDCTL_SEQ_NRSYNTHS:
   case SNDCTL_SEQ_NRMIDIS:
   case SNDCTL_SEQ_GETTIME:
   case SNDCTL_DSP_GETFMTS:
   case SNDCTL_DSP_GETTRIGGER:
   case SNDCTL_DSP_GETODELAY:
#           if defined(SNDCTL_DSP_GETSPDIF)
   case SNDCTL_DSP_GETSPDIF:
#           endif
   case SNDCTL_DSP_GETCAPS:
   case SOUND_PCM_READ_RATE:
   case SOUND_PCM_READ_CHANNELS:
   case SOUND_PCM_READ_BITS:
   case (SOUND_PCM_READ_BITS|0x40000000): /* what the fuck ? */
   case SOUND_PCM_READ_FILTER:
      SYSCALL_TRACK( pre_mem_write,tid,
		     "ioctl(SNDCTL_XXX|SOUND_XXX (SIOR, int))", 
		     arg3,
		     sizeof(int));
      break;
   case SNDCTL_SEQ_CTRLRATE:
   case SNDCTL_DSP_SPEED:
   case SNDCTL_DSP_STEREO:
   case SNDCTL_DSP_GETBLKSIZE: 
   case SNDCTL_DSP_CHANNELS:
   case SOUND_PCM_WRITE_FILTER:
   case SNDCTL_DSP_SUBDIVIDE:
   case SNDCTL_DSP_SETFRAGMENT:
#           if defined(SNDCTL_DSP_GETCHANNELMASK)
   case SNDCTL_DSP_GETCHANNELMASK:
#           endif
#           if defined(SNDCTL_DSP_BIND_CHANNEL)
   case SNDCTL_DSP_BIND_CHANNEL:
#           endif
   case SNDCTL_TMR_TIMEBASE:
   case SNDCTL_TMR_TEMPO:
   case SNDCTL_TMR_SOURCE:
   case SNDCTL_MIDI_PRETIME:
   case SNDCTL_MIDI_MPUMODE:
      SYSCALL_TRACK( pre_mem_read,tid, "ioctl(SNDCTL_XXX|SOUND_XXX "
		     "(SIOWR, int))", 
		     arg3, sizeof(int));
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(SNDCTL_XXX|SOUND_XXX "
		     "(SIOWR, int))", 
		     arg3, sizeof(int));
      break;
   case SNDCTL_DSP_GETOSPACE:
   case SNDCTL_DSP_GETISPACE:
      SYSCALL_TRACK( pre_mem_write,tid, 
		     "ioctl(SNDCTL_XXX|SOUND_XXX "
		     "(SIOR, audio_buf_info))", arg3,
		     sizeof(audio_buf_info));
      break;
   case SNDCTL_DSP_SETTRIGGER:
      SYSCALL_TRACK( pre_mem_read,tid, 
		     "ioctl(SNDCTL_XXX|SOUND_XXX (SIOW, int))", 
		     arg3, sizeof(int));
      break;

   case SNDCTL_DSP_POST:
   case SNDCTL_DSP_RESET:
   case SNDCTL_DSP_SYNC:
   case SNDCTL_DSP_SETSYNCRO:
   case SNDCTL_DSP_SETDUPLEX:
      break;

      /* Real Time Clock (/dev/rtc) ioctls */
#           ifndef GLIBC_2_1
   case RTC_UIE_ON:
   case RTC_UIE_OFF:
   case RTC_AIE_ON:
   case RTC_AIE_OFF:
   case RTC_PIE_ON:
   case RTC_PIE_OFF:
   case RTC_IRQP_SET:
      break;
   case RTC_RD_TIME:
   case RTC_ALM_READ:
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(RTC_RD_TIME/ALM_READ)", 
		     arg3, sizeof(struct rtc_time));
      break;
   case RTC_ALM_SET:
      SYSCALL_TRACK( pre_mem_read,tid, "ioctl(RTC_ALM_SET)", arg3,
		     sizeof(struct rtc_time));
      break;
   case RTC_IRQP_READ:
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(RTC_IRQP_READ)", arg3,
		     sizeof(unsigned long));
      break;
#           endif /* GLIBC_2_1 */

#           ifdef BLKGETSIZE
   case BLKGETSIZE:
      SYSCALL_TRACK( pre_mem_write,tid, "ioctl(BLKGETSIZE)", arg3,
		     sizeof(unsigned long));
      break;
#           endif /* BLKGETSIZE */

      /* CD ROM stuff (??)  */
   case CDROMSUBCHNL:
      SYSCALL_TRACK( pre_mem_read,tid, 
		     "ioctl(CDROMSUBCHNL (cdsc_format, char))",
		     (int) &(((struct cdrom_subchnl *) arg3)->cdsc_format), 
		     sizeof(((struct cdrom_subchnl *) arg3)->cdsc_format));
      SYSCALL_TRACK( pre_mem_write,tid, 
		     "ioctl(CDROMSUBCHNL)", arg3, 
		     sizeof(struct cdrom_subchnl));
      break;
   case CDROMREADTOCHDR:
      SYSCALL_TRACK( pre_mem_write,tid, 
		     "ioctl(CDROMREADTOCHDR)", arg3, 
		     sizeof(struct cdrom_tochdr));
      break;
   case CDROMREADTOCENTRY:
      SYSCALL_TRACK( pre_mem_read,tid, 
		     "ioctl(CDROMREADTOCENTRY (cdte_format, char))",
		     (int) &(((struct cdrom_tocentry *) arg3)->cdte_format), 
		     sizeof(((struct cdrom_tocentry *) arg3)->cdte_format));
      SYSCALL_TRACK( pre_mem_read,tid, 
		     "ioctl(CDROMREADTOCENTRY (cdte_track, char))",
		     (int) &(((struct cdrom_tocentry *) arg3)->cdte_track), 
		     sizeof(((struct cdrom_tocentry *) arg3)->cdte_track));
      SYSCALL_TRACK( pre_mem_write,tid, 
		     "ioctl(CDROMREADTOCENTRY)", arg3, 
		     sizeof(struct cdrom_tocentry));
      break;
   case CDROMPLAYMSF:
      SYSCALL_TRACK( pre_mem_read,tid, "ioctl(CDROMPLAYMSF)", arg3, 
		     sizeof(struct cdrom_msf));
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case CDROM_DRIVE_STATUS: /* 0x5326 */
   case CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

      /* We don't have any specific information on it, so
	 try to do something reasonable based on direction and
	 size bits.  The encoding scheme is described in
	 /usr/include/asm/ioctl.h.  

	 According to Simon Hausmann, _IOC_READ means the kernel
	 writes a value to the ioctl value passed from the user
	 space and the other way around with _IOC_WRITE. */
   default: {
      UInt dir  = _IOC_DIR(arg2);
      UInt size = _IOC_SIZE(arg2);
      if (VG_(strstr)(VG_(clo_weird_hacks), "lax-ioctls") != NULL) {
	 /* 
	  * Be very lax about ioctl handling; the only
	  * assumption is that the size is correct. Doesn't
	  * require the full buffer to be initialized when
	  * writing.  Without this, using some device
	  * drivers with a large number of strange ioctl
	  * commands becomes very tiresome.
	  */
      } else if (/* size == 0 || */ dir == _IOC_NONE) {
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
	 if ((dir & _IOC_WRITE) && size > 0)
	    SYSCALL_TRACK( pre_mem_read,tid, "ioctl(generic)", 
			   arg3, size);
	 if ((dir & _IOC_READ) && size > 0)
	    SYSCALL_TRACK( pre_mem_write,tid, "ioctl(generic)", 
			   arg3, size);
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
   MAYBE_PRINTF("ioctl ( %d, 0x%x, %p )\n",arg1,arg2,arg3);
   switch (arg2 /* request */) {
   case TCSETS:
   case TCSETSW:
   case TCSETSF:
      break; 
   case TCGETS:
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, VKI_SIZEOF_STRUCT_TERMIOS );
      break;
   case TCSETA:
   case TCSETAW:
   case TCSETAF:
      break;
   case TCGETA:
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, VKI_SIZEOF_STRUCT_TERMIO );
      break;
   case TCSBRK:
   case TCXONC:
   case TCSBRKP:
   case TCFLSH:
      break;
   case TIOCGWINSZ:
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, sizeof(struct winsize) );
      break;
   case TIOCSWINSZ:
      break;
   case TIOCLINUX:
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, sizeof(char *) );
      break;
   case TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, sizeof(pid_t) );
      break;
   case TIOCSPGRP:
      /* Set a process group ID? */
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, sizeof(pid_t) );
      break;
   case TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, sizeof(int));
      break;
   case TIOCSCTTY:
      break;
   case TIOCSPTLCK: /* Lock/unlock Pty */
      break;
   case FIONBIO:
      break;
   case FIOASYNC:
      break;
   case FIONREAD:                /* identical to SIOCINQ */
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, sizeof(int) );
      break;

      /* If you get compilation problems here, change the #if
	 1 to #if 0 and get rid of <scsi/sg.h> in
	 vg_unsafe.h. */
#       if 1
   case SG_SET_COMMAND_Q:
      break;
#           if defined(SG_IO)
   case SG_IO:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct sg_io_hdr));
      break;
#           endif /* SG_IO */
   case SG_GET_SCSI_ID:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct sg_scsi_id));
      break;
   case SG_SET_RESERVED_SIZE:
      break;
   case SG_SET_TIMEOUT:
      break;
   case SG_GET_RESERVED_SIZE:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case SG_GET_TIMEOUT:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case SG_GET_VERSION_NUM:
      break;
#       endif

   case VKI_IIOCGETCPS:
      /* In early 2.4 kernels, ISDN_MAX_CHANNELS was only defined
       * when KERNEL was. I never saw a larger value than 64 though */
#              ifndef ISDN_MAX_CHANNELS
#              define ISDN_MAX_CHANNELS 64
#              endif
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, ISDN_MAX_CHANNELS 
		   * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      if (res == 0)
	 VG_TRACK( post_mem_write, arg3, sizeof(isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case SIOCGIFINDEX:
   case SIOCGIFFLAGS:        /* get flags                    */
   case SIOCGIFHWADDR:       /* Get hardware address         */
   case SIOCGIFMTU:          /* get MTU size                 */
   case SIOCGIFADDR:         /* get PA address               */
   case SIOCGIFNETMASK:      /* get network PA mask          */
   case SIOCGIFMETRIC:       /* get metric                   */
   case SIOCGIFMAP:          /* Get device parameters        */
   case SIOCGIFTXQLEN:       /* Get the tx queue length      */
   case SIOCGIFDSTADDR:      /* get remote PA address        */
   case SIOCGIFBRDADDR:      /* get broadcast PA address     */
   case SIOCGIFNAME:         /* get iface name               */
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct ifreq));
      break;
   case SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 SYSCALL_TRACK( pre_mem_write,"ioctl(SIOCGIFCONF)", arg3, 
	 sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,res);
	 if (!VG_(is_kerror)(res) && res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct ifconf));
      */
      if (res == 0 && arg3 ) {
	 struct ifconf *ifc = (struct ifconf *) arg3;
	 if (ifc->ifc_buf != NULL)
	    VG_TRACK( post_mem_write, (Addr)(ifc->ifc_buf), 
		      (UInt)(ifc->ifc_len) );
      }
      break;
   case SIOCGSTAMP:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct timeval));
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case SIOCOUTQ:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case SIOCGRARP:           /* get RARP table entry         */
   case SIOCGARP:            /* get ARP table entry          */
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct arpreq));
      break;
                    
   case SIOCSIFFLAGS:        /* set flags                    */
   case SIOCSIFMAP:          /* Set device parameters        */
   case SIOCSIFTXQLEN:       /* Set the tx queue length      */
   case SIOCSIFDSTADDR:      /* set remote PA address        */
   case SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case SIOCSIFNETMASK:      /* set network PA mask          */
   case SIOCSIFMETRIC:       /* set metric                   */
   case SIOCSIFADDR:         /* set PA address               */
   case SIOCSIFMTU:          /* set MTU size                 */
   case SIOCSIFHWADDR:       /* set hardware address         */
      break;
      /* Routing table calls.  */
   case SIOCADDRT:           /* add routing table entry      */
   case SIOCDELRT:           /* delete routing table entry   */
      break;

      /* RARP cache control calls. */
   case SIOCDRARP:           /* delete RARP table entry      */
   case SIOCSRARP:           /* set RARP table entry         */
      /* ARP cache control calls. */
   case SIOCSARP:            /* set ARP table entry          */
   case SIOCDARP:            /* delete ARP table entry       */
      break;

   case SIOCSPGRP:
      break;

      /* linux/soundcard interface (OSS) */
   case SNDCTL_SEQ_GETOUTCOUNT:
   case SNDCTL_SEQ_GETINCOUNT:
   case SNDCTL_SEQ_PERCMODE:
   case SNDCTL_SEQ_TESTMIDI:
   case SNDCTL_SEQ_RESETSAMPLES:
   case SNDCTL_SEQ_NRSYNTHS:
   case SNDCTL_SEQ_NRMIDIS:
   case SNDCTL_SEQ_GETTIME:
   case SNDCTL_DSP_GETFMTS:
   case SNDCTL_DSP_GETTRIGGER:
   case SNDCTL_DSP_GETODELAY:
#           if defined(SNDCTL_DSP_GETSPDIF)
   case SNDCTL_DSP_GETSPDIF:
#           endif
   case SNDCTL_DSP_GETCAPS:
   case SOUND_PCM_READ_RATE:
   case SOUND_PCM_READ_CHANNELS:
   case SOUND_PCM_READ_BITS:
   case (SOUND_PCM_READ_BITS|0x40000000): /* what the fuck ? */
   case SOUND_PCM_READ_FILTER:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(int));
      break;
   case SNDCTL_SEQ_CTRLRATE:
   case SNDCTL_DSP_SPEED:
   case SNDCTL_DSP_STEREO:
   case SNDCTL_DSP_GETBLKSIZE: 
   case SNDCTL_DSP_CHANNELS:
   case SOUND_PCM_WRITE_FILTER:
   case SNDCTL_DSP_SUBDIVIDE:
   case SNDCTL_DSP_SETFRAGMENT:
#           if defined(SNDCTL_DSP_GETCHANNELMASK)
   case SNDCTL_DSP_GETCHANNELMASK:
#           endif
#           if defined(SNDCTL_DSP_BIND_CHANNEL)
   case SNDCTL_DSP_BIND_CHANNEL:
#           endif
   case SNDCTL_TMR_TIMEBASE:
   case SNDCTL_TMR_TEMPO:
   case SNDCTL_TMR_SOURCE:
   case SNDCTL_MIDI_PRETIME:
   case SNDCTL_MIDI_MPUMODE:
      break;
   case SNDCTL_DSP_GETOSPACE:
   case SNDCTL_DSP_GETISPACE:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(audio_buf_info));
      break;
   case SNDCTL_DSP_SETTRIGGER:
      break;

   case SNDCTL_DSP_POST:
   case SNDCTL_DSP_RESET:
   case SNDCTL_DSP_SYNC:
   case SNDCTL_DSP_SETSYNCRO:
   case SNDCTL_DSP_SETDUPLEX:
      break;

      /* Real Time Clock (/dev/rtc) ioctls */
#           ifndef GLIBC_2_1
   case RTC_UIE_ON:
   case RTC_UIE_OFF:
   case RTC_AIE_ON:
   case RTC_AIE_OFF:
   case RTC_PIE_ON:
   case RTC_PIE_OFF:
   case RTC_IRQP_SET:
      break;
   case RTC_RD_TIME:
   case RTC_ALM_READ:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct rtc_time));
      break;
   case RTC_ALM_SET:
      break;
   case RTC_IRQP_READ:
      if(res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(unsigned long));
      break;
#           endif /* GLIBC_2_1 */

#           ifdef BLKGETSIZE
   case BLKGETSIZE:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(unsigned long));
      break;
#           endif /* BLKGETSIZE */

      /* CD ROM stuff (??)  */
   case CDROMSUBCHNL:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct cdrom_subchnl));
      break;
   case CDROMREADTOCHDR:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct cdrom_tochdr));
      break;
   case CDROMREADTOCENTRY:
      if (res == 0)
	 VG_TRACK( post_mem_write,arg3, sizeof(struct cdrom_tochdr));
      break;
   case CDROMPLAYMSF:
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case CDROM_DRIVE_STATUS: /* 0x5326 */
   case CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

      /* We don't have any specific information on it, so
	 try to do something reasonable based on direction and
	 size bits.  The encoding scheme is described in
	 /usr/include/asm/ioctl.h.  

	 According to Simon Hausmann, _IOC_READ means the kernel
	 writes a value to the ioctl value passed from the user
	 space and the other way around with _IOC_WRITE. */
   default: {
      UInt dir  = _IOC_DIR(arg2);
      UInt size = _IOC_SIZE(arg2);
      if (size > 0 && (dir & _IOC_READ)
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
   MAYBE_PRINTF("kill ( %d, %d )\n", arg1,arg2);
   if (arg2 == VKI_SIGVGINT || arg2 == VKI_SIGVGKILL)
      res = -VKI_EINVAL;
}

POST(kill)
{
   /* If this was a self-kill then wait for a signal to be
      delivered to any thread before claiming the kill is done. */
   if (res >= 0 &&					/* if it was successful */
       arg2 != 0 &&					/* if a real signal */
       !VG_(is_sig_ign)(arg2) &&			/* that isn't ignored and */
       !VG_(ksigismember)(&tst->eff_sig_mask, arg2) &&	/*      we're not blocking it */
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
   MAYBE_PRINTF("link ( %p, %p)\n", arg1, arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "link(oldpath)", arg1);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "link(newpath)", arg2);
}

PRE(lseek)
{
   /* off_t lseek(int fildes, off_t offset, int whence); */
   MAYBE_PRINTF("lseek ( %d, %d, %d )\n",arg1,arg2,arg3);
}

PRE(_llseek)
{
   /* int _llseek(unsigned int fd, unsigned long offset_high,       
      unsigned long  offset_low, 
      loff_t * result, unsigned int whence); */
   MAYBE_PRINTF("llseek ( %d, 0x%x, 0x%x, %p, %d )\n",
		arg1,arg2,arg3,arg4,arg5);
   SYSCALL_TRACK( pre_mem_write, tid, "llseek(result)", arg4, 
		  sizeof(loff_t));
}

POST(_llseek)
{
   if (res == 0)
      VG_TRACK( post_mem_write, arg4, sizeof(loff_t) );
}

PRE(lstat)
{
   /* int lstat(const char *file_name, struct stat *buf); */
   MAYBE_PRINTF("lstat ( %p \"%s\", %p )\n",arg1,arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "lstat(file_name)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "lstat(buf)", arg2, 
		  sizeof(struct stat) );
}

POST(lstat)
{
   if (res == 0) {
      VG_TRACK( post_mem_write, arg2, sizeof(struct stat) );
   }
}

PRE(lstat64)
{
   /* int lstat64(const char *file_name, struct stat64 *buf); */
   MAYBE_PRINTF("lstat64 ( %p \"%s\", %p )\n",arg1,arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "lstat64(file_name)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "lstat64(buf)", arg2, 
		  sizeof(struct stat64) );
}

POST(lstat64)
{
   if (res == 0) {
      VG_TRACK( post_mem_write, arg2, sizeof(struct stat64) );
   }
}

PRE(mkdir)
{
   /* int mkdir(const char *pathname, mode_t mode); */
   MAYBE_PRINTF("mkdir ( %p, %d )\n", arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "mkdir(pathname)", arg1 );
}

PRE(mmap2)
{
   /* My impression is that this is exactly like __NR_mmap 
      except that all 6 args are passed in regs, rather than in 
      a memory-block.
      
      Almost.  The big difference is that the file offset is specified
      in pagesize units rather than bytes, so that it can be used for
      files bigger than 2^32 bytes. - JSGF
   */
   /* void* mmap(void *start, size_t length, int prot, 
      int flags, int fd, off_t offset); 
   */
   MAYBE_PRINTF("mmap2 ( %p, %d, %d, %d, %d, %d )\n",
		arg1, arg2, arg3, arg4, arg5, arg6 );

   if (arg4 & VKI_MAP_FIXED) {
      if (!valid_client_addr(arg1, arg2, tid, "mmap2"))
	 res = -VKI_ENOMEM;
   } else {
      arg1 = VG_(find_map_space)(arg1, arg2, True);
      arg4 |= VKI_MAP_FIXED;
      if (arg1 == 0)
	 res = -VKI_ENOMEM;
   }
}

POST(mmap2)
{
   if (!VG_(is_kerror)(res)) {
      if (!valid_client_addr(res, arg2, tid, "mmap2")) {
         VG_(munmap)((void *)res, arg2);
         res = -VKI_ENOMEM;
      } else
         mmap_segment( (Addr)res, arg2, arg3, arg4, arg5, arg6 * (ULong)VKI_BYTES_PER_PAGE );
   }
}

PRE(mmap)
{
   /* void* mmap(void *start, size_t length, int prot, 
      int flags, int fd, off_t offset); 
   */

   UInt* arg_block = (UInt*)arg1;
   UInt a1, a2, a3, a4, a5, a6;

   SYSCALL_TRACK( pre_mem_read, tid, "mmap(args)", arg1, 6*sizeof(UInt) );

   a1 = arg_block[0];
   a2 = arg_block[1];
   a3 = arg_block[2];
   a4 = arg_block[3];
   a5 = arg_block[4];
   a6 = arg_block[5];
   MAYBE_PRINTF("mmap ( %p, %d, %d, %d, %d, %d )\n",
		a1, a2, a3, a4, a5, a6 );

   if (a4 & VKI_MAP_FIXED) {
      if (!valid_client_addr(a1, a2, tid, "mmap")) {
	 MAYBE_PRINTF("mmap failing: %p-%p\n", a1, a1+a2);
	 res = -VKI_ENOMEM;
      }
   } else {
      a1 = VG_(find_map_space)(arg_block[0], arg_block[1], True);
      if (a1 == 0)
	 res = -VKI_ENOMEM;
      else
	 a4 |= VKI_MAP_FIXED;
   }

   if (res != -VKI_ENOMEM) {
      UInt new_arg_block[6];

      new_arg_block[0] = a1;
      new_arg_block[1] = a2;
      new_arg_block[2] = a3;
      new_arg_block[3] = a4;
      new_arg_block[4] = a5;
      new_arg_block[5] = a6;
      
      res = VG_(do_syscall)(__NR_mmap, new_arg_block);

      if (!VG_(is_kerror)(res)) {
         if (!valid_client_addr(res, a2, tid, "mmap")) {
	    VG_(munmap)((void *)res, a2);
	    res = -VKI_ENOMEM;
         } else
	    mmap_segment( (Addr)res, a2, a3, a4, a5, a6 );
      }
   }
}

PRE(mprotect)
{
   /* int mprotect(const void *addr, size_t len, int prot); */
   /* should addr .. addr+len-1 be checked before the call? */
   MAYBE_PRINTF("mprotect ( %p, %d, %d )\n", arg1,arg2,arg3);

   if (!valid_client_addr(arg1, arg2, tid, "mprotect"))
      res = -VKI_ENOMEM;
}

POST(mprotect)
{
   mprotect_segment( arg1, arg2, arg3 );
}

PRE(munmap)
{
   /* int munmap(void *start, size_t length); */
   /* should start .. start+length-1 be checked before the call? */
   MAYBE_PRINTF("munmap ( %p, %d )\n", arg1,arg2);

   if (!valid_client_addr(arg1, arg2, tid, "munmap"))
      res = -VKI_EINVAL;
}

POST(munmap)
{
   munmap_segment( arg1, arg2 );
}

PRE(mincore)
{
   /* int mincore(void *start, size_t length, unsigned char *vec); */
   MAYBE_PRINTF("mincore ( %p, %d, %p )\n", arg1,arg2,arg3);
   SYSCALL_TRACK(pre_mem_write, tid, "mincore(vec)",
                 arg3, (arg2 + 4096 - 1) / 4096);
}

POST(mincore)
{
   if (!VG_(is_kerror)(res))
      VG_TRACK( post_mem_write, arg3, (arg2 + 4096 - 1) / 4096 );  
}

PRE(nanosleep)
{
         /* int nanosleep(const struct timespec *req, struct timespec *rem); */
         MAYBE_PRINTF("nanosleep ( %p, %p )\n", arg1,arg2);
         SYSCALL_TRACK( pre_mem_read, tid, "nanosleep(req)", arg1, 
                                              sizeof(struct timespec) );
         if (arg2 != (UInt)NULL)
            SYSCALL_TRACK( pre_mem_write, tid, "nanosleep(rem)", arg2, 
			   sizeof(struct timespec) );
}

POST(nanosleep)
{
   /* Somewhat bogus ... is only written by the kernel if
      res == -1 && errno == EINTR. */
   if (arg2 != (UInt)NULL)
      VG_TRACK( post_mem_write, arg2, sizeof(struct timespec) );
}

PRE(_newselect)
{
   /* int select(int n,  
		 fd_set *readfds, fd_set *writefds, fd_set *exceptfds, 
		 struct timeval *timeout);
   */
   MAYBE_PRINTF("newselect ( %d, %p, %p, %p, %p )\n",
		arg1,arg2,arg3,arg4,arg5);
   if (arg2 != 0)
      SYSCALL_TRACK( pre_mem_read, tid, "newselect(readfds)",   
		     arg2, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg3 != 0)
      SYSCALL_TRACK( pre_mem_read, tid, "newselect(writefds)",  
		     arg3, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg4 != 0)
      SYSCALL_TRACK( pre_mem_read, tid, "newselect(exceptfds)", 
		     arg4, arg1/8 /* __FD_SETSIZE/8 */ );
   if (arg5 != 0)
      SYSCALL_TRACK( pre_mem_read, tid, "newselect(timeout)", arg5, 
		     sizeof(struct timeval) );
}

PRE(open)
{
   /* int open(const char *pathname, int flags); */
   MAYBE_PRINTF("open ( %p(%s), %d ) --> ",arg1,arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "open(pathname)", arg1 );
}

POST(open)
{
   if (!fd_allowed(res, "open", tid)) {
      VG_(close)(res);
      res = -VKI_EMFILE;
   } else {
      if(VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
   MAYBE_PRINTF("%d\n",res);
}

PRE(read)
{
   /* size_t read(int fd, void *buf, size_t count); */
   MAYBE_PRINTF("read ( %d, %p, %d )\n", arg1, arg2, arg3);

   if (!fd_allowed(arg1, "read", tid))
      res = -VKI_EBADF;   
}

POST(read)
{
   if (res > 0)
      VG_TRACK(post_mem_write, arg2, res);
}

PRE(write)
{
   /* size_t write(int fd, const void *buf, size_t count); */
   MAYBE_PRINTF("write ( %d, %p, %d )\n", arg1, arg2, arg3);
   if (!fd_allowed(arg1, "write", tid))
      res = -VKI_EBADF;
   else
      SYSCALL_TRACK( pre_mem_read, tid, "write(buf)", arg2, arg3 );
}

PRE(creat)
{
   /* int creat(const char *pathname, mode_t mode); */
   MAYBE_PRINTF("creat ( %p(%s), %d ) --> ",arg1,arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "creat(pathname)", arg1 );
}

POST(creat)
{
   if (!fd_allowed(res, "creat", tid)) {
      VG_(close)(res);
      res = -VKI_EMFILE;
   } else {
      if(VG_(clo_track_fds))
         record_fd_open(tid, res, VG_(arena_strdup)(VG_AR_CORE, (Char*)arg1));
   }
   MAYBE_PRINTF("%d\n",res);
}

PRE(pipe)
{
   /* int pipe(int filedes[2]); */
   MAYBE_PRINTF("pipe ( %p ) ...\n", arg1);
   SYSCALL_TRACK( pre_mem_write, tid, "pipe(filedes)", 
		  arg1, 2*sizeof(int) );
}

POST(pipe)
{
   Int *p = (Int *)arg1;

   if (!fd_allowed(p[0], "pipe", tid) ||
       !fd_allowed(p[1], "pipe", tid)) {
      VG_(close)(p[0]);
      VG_(close)(p[1]);
      res = -VKI_EMFILE;
   } else {
      VG_TRACK( post_mem_write, arg1, 2*sizeof(int) );
      if(VG_(clo_track_fds)) {
         record_fd_open(tid, p[0], NULL);
         record_fd_open(tid, p[1], NULL);
      }
   }

   MAYBE_PRINTF("SYSCALL[%d]       pipe --> %d (rd %d, wr %d)\n", 
		VG_(getpid)(), res,
		((UInt*)arg1)[0], ((UInt*)arg1)[1] );
}

PRE(poll)
{
   /* struct pollfd {
	int fd;           -- file descriptor
	short events;     -- requested events
	short revents;    -- returned events
      };
      int poll(struct pollfd *ufds, unsigned int nfds, 
      int timeout) 
   */
   MAYBE_PRINTF("poll ( %p, %d, %d )\n",arg1,arg2,arg3);
   /* In fact some parts of this struct should be readable too.
      This should be fixed properly. */
   SYSCALL_TRACK( pre_mem_write, tid, "poll(ufds)", 
		  arg1, arg2 * sizeof(struct pollfd) );
}

POST(poll)
{
   if (res > 0) {
      UInt i;
      struct pollfd * arr = (struct pollfd *)arg1;
      for (i = 0; i < arg2; i++)
	 VG_TRACK( post_mem_write, (Addr)(&arr[i].revents), 
		   sizeof(Short) );
   }
}

PRE(epoll_create)
{
   /* int epoll_create(int size) */
   MAYBE_PRINTF("epoll_create ( %d )\n", arg1);
}

POST(epoll_create)
{
   if (!fd_allowed(res, "open", tid)) {
      VG_(close)(res);
      res = -VKI_EMFILE;
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
   MAYBE_PRINTF("epoll_ctl ( %d, %s, %d, %p )\n", 
                arg1, ( arg2<3 ? epoll_ctl_s[arg2] : "?" ), arg3, arg4);
   SYSCALL_TRACK( pre_mem_read, tid, "epoll_ctl(event)",
                  arg4, sizeof(struct vki_epoll_event) );
}

PRE(epoll_wait)
{
   /* int epoll_wait(int epfd, struct epoll_event * events, 
                     int maxevents, int timeout) */
   MAYBE_PRINTF("epoll_wait ( %d, %p, %d, %d )\n", arg1, arg2, arg3, arg4);
   SYSCALL_TRACK( pre_mem_write, tid, "epoll_wait(events)",
                  arg2, sizeof(struct vki_epoll_event)*arg3);
}

POST(epoll_wait)
{
   if (res > 0)
      VG_TRACK( post_mem_write, arg2, sizeof(struct vki_epoll_event)*res ) ;
}

PRE(readlink)
{
   /* int readlink(const char *path, char *buf, size_t bufsiz); */
   MAYBE_PRINTF("readlink ( %p, %p, %d )\n", arg1,arg2,arg3);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "readlink(path)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "readlink(buf)", arg2,arg3 );
}

POST(readlink)
{
   VG_TRACK( post_mem_write, arg2, res );
}

PRE(readv)
{
   /* int readv(int fd, const struct iovec * vector, size_t count); */
   Int i;
   struct iovec * vec;
   MAYBE_PRINTF("readv ( %d, %p, %d )\n",arg1,arg2,arg3);
   if (!fd_allowed(arg1, "readv", tid)) {
      res = -VKI_EBADF;
   } else {
      SYSCALL_TRACK( pre_mem_read, tid, "readv(vector)", 
		     arg2, arg3 * sizeof(struct iovec) );
      /* ToDo: don't do any of the following if the vector is invalid */
      vec = (struct iovec *)arg2;
      for (i = 0; i < (Int)arg3; i++)
	 SYSCALL_TRACK( pre_mem_write, tid, "readv(vector[...])",
			(UInt)vec[i].iov_base,vec[i].iov_len );
   }
}

POST(readv)
{
   if (res > 0) {
      Int i;
      struct iovec * vec = (struct iovec *)arg2;
      Int remains = res;

      /* res holds the number of bytes read. */
      for (i = 0; i < (Int)arg3; i++) {
	 Int nReadThisBuf = vec[i].iov_len;
	 if (nReadThisBuf > remains) nReadThisBuf = remains;
	 VG_TRACK( post_mem_write, (UInt)vec[i].iov_base, nReadThisBuf );
	 remains -= nReadThisBuf;
	 if (remains < 0) VG_(core_panic)("readv: remains < 0");
      }
   }
}

PRE(rename)
{
   /* int rename(const char *oldpath, const char *newpath); */
   MAYBE_PRINTF("rename ( %p, %p )\n", arg1, arg2 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "rename(oldpath)", arg1 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "rename(newpath)", arg2 );
}

PRE(rmdir)
{
   /* int rmdir(const char *pathname); */
   MAYBE_PRINTF("rmdir ( %p )\n", arg1);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "rmdir(pathname)", arg1 );
}

PRE(sched_setparam)
{
   /* int sched_setparam(pid_t pid, const struct sched_param *p); */
   MAYBE_PRINTF("sched_setparam ( %d, %p )\n", arg1, arg2 );
   SYSCALL_TRACK( pre_mem_read, tid, "sched_setparam(ptr)",
		  arg2, sizeof(struct sched_param) );
}

POST(sched_setparam)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct sched_param) );
}

PRE(sched_getparam)
{
   /* int sched_getparam(pid_t pid, struct sched_param *p); */
   MAYBE_PRINTF("sched_getparam ( %d, %p )\n", arg1, arg2 );
   SYSCALL_TRACK( pre_mem_write, tid, "sched_getparam(ptr)",
		  arg2, sizeof(struct sched_param) );
}

POST(sched_getparam)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct sched_param) );
}

PRE(sched_yield)
{
   /* int sched_yield(void); */
   MAYBE_PRINTF("sched_yield ()\n" );
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
   SYSCALL_TRACK( pre_mem_read, tid, "select(args)", arg1, 5*sizeof(UInt) );

   {
      UInt* arg_struct = (UInt*)arg1;
      UInt a1, a2, a3, a4, a5;

      a1 = arg_struct[0];
      a2 = arg_struct[1];
      a3 = arg_struct[2];
      a4 = arg_struct[3];
      a5 = arg_struct[4];

      MAYBE_PRINTF("select ( %d, %p, %p, %p, %p )\n", 
		   a1,a2,a3,a4,a5);
      if (a2 != (Addr)NULL)
	 SYSCALL_TRACK( pre_mem_read, tid, "select(readfds)", a2, 
			a1/8 /* __FD_SETSIZE/8 */ );
      if (a3 != (Addr)NULL)
	 SYSCALL_TRACK( pre_mem_read, tid, "select(writefds)", a3, 
			a1/8 /* __FD_SETSIZE/8 */ );
      if (a4 != (Addr)NULL)
	 SYSCALL_TRACK( pre_mem_read, tid, "select(exceptfds)", a4, 
			a1/8 /* __FD_SETSIZE/8 */ );
      if (a5 != (Addr)NULL)
	 SYSCALL_TRACK( pre_mem_read, tid, "select(timeout)", a5, 
			sizeof(struct timeval) );
   }
}

PRE(setitimer)
{
         /* setitimer(int which, const struct itimerval *value,
                                 struct itimerval *ovalue); */
         MAYBE_PRINTF("setitimer ( %d, %p, %p )\n", arg1,arg2,arg3);
         if (arg2 != (Addr)NULL)
            SYSCALL_TRACK( pre_mem_read,tid, "setitimer(value)", 
                             arg2, sizeof(struct itimerval) );
         if (arg3 != (Addr)NULL)
            SYSCALL_TRACK( pre_mem_write,tid, "setitimer(ovalue)", 
                             arg3, sizeof(struct itimerval));
}

POST(setitimer)
{
   if (arg3 != (Addr)NULL) {
      VG_TRACK( post_mem_write,arg3, sizeof(struct itimerval));
   }
}

PRE(setfsgid32)
{
   /* int setfsgid(uid_t fsgid); */
   MAYBE_PRINTF("setfsgid ( %d )\n", arg1);
}

PRE(setgid)
{
   /* int setgid(gid_t gid); */
   MAYBE_PRINTF("setgid ( %d )\n", arg1);
}

PREALIAS(setgid32, setgid);

PRE(setsid)
{
   /* pid_t setsid(void); */
   MAYBE_PRINTF("setsid ()\n");
}

PRE(setgroups)
{
   /* int setgroups(size_t size, const gid_t *list); */
   MAYBE_PRINTF("setgroups ( %d, %p )\n", arg1, arg2);
   if (arg1 > 0)
      SYSCALL_TRACK( pre_mem_read, tid, "setgroups(list)", arg2, 
		     arg1 * sizeof(gid_t) );
}

PREALIAS(setgroups32, setgroups);

PRE(setpgid)
{
   /* int setpgid(pid_t pid, pid_t pgid); */
   MAYBE_PRINTF("setpgid ( %d, %d )\n", arg1, arg2);
}

POST(setpgid)
{
   VG_(main_pgrp) = VG_(getpgrp)();
}

PRE(setregid32)
{
   /* int setregid(gid_t rgid, gid_t egid); */
   MAYBE_PRINTF("setregid32(?) ( %d, %d )\n", arg1, arg2);
}

PRE(setresuid32)
{
   /* int setresuid(uid_t ruid, uid_t euid, uid_t suid); */
   MAYBE_PRINTF("setresuid32(?) ( %d, %d, %d )\n", arg1, arg2, arg3);
}

PRE(setreuid)
{
   /* int setreuid(uid_t ruid, uid_t euid); */
   MAYBE_PRINTF("setreuid ( 0x%x, 0x%x )\n", arg1, arg2);
}

PREALIAS(setreuid32, setreuid);

PRE(setrlimit)
{
   /* int setrlimit (int resource, const struct rlimit *rlim); */
   MAYBE_PRINTF("setrlimit ( %d, %p )\n", arg1,arg2);
   SYSCALL_TRACK( pre_mem_read, tid, "setrlimit(rlim)", 
		  arg2, sizeof(struct rlimit) );
}

PRE(setuid)
{
   /* int setuid(uid_t uid); */
   MAYBE_PRINTF("setuid ( %d )\n", arg1);
}

PREALIAS(setuid32, setuid);

PRE(socketcall)
{
   /* int socketcall(int call, unsigned long *args); */
   MAYBE_PRINTF("socketcall ( %d, %p )\n",arg1,arg2);
   switch (arg1 /* request */) {

   case SYS_SOCKETPAIR:
      /* int socketpair(int d, int type, int protocol, int sv[2]); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.socketpair(args)", 
		     arg2, 4*sizeof(Addr) );
      SYSCALL_TRACK( pre_mem_write, tid, "socketcall.socketpair(sv)", 
		     ((UInt*)arg2)[3], 2*sizeof(int) );
      break;

   case SYS_SOCKET:
      /* int socket(int domain, int type, int protocol); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.socket(args)", 
		     arg2, 3*sizeof(Addr) );
      break;

   case SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
	 int addrlen); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.bind(args)", 
		     arg2, 3*sizeof(Addr) );
      pre_mem_read_sockaddr( tid, "socketcall.bind(my_addr.%s)",
			     (struct sockaddr *) (((UInt*)arg2)[1]), ((UInt*)arg2)[2]);
      break;
               
   case SYS_LISTEN:
      /* int listen(int s, int backlog); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.listen(args)", 
		     arg2, 2*sizeof(Addr) );
      break;

   case SYS_ACCEPT: {
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.accept(args)", 
		     arg2, 3*sizeof(Addr) );
      {
	 Addr addr_p     = ((UInt*)arg2)[1];
	 Addr addrlen_p  = ((UInt*)arg2)[2];
	 if (addr_p != (Addr)NULL) 
	    buf_and_len_pre_check ( tid, addr_p, addrlen_p,
				    "socketcall.accept(addr)",
				    "socketcall.accept(addrlen_in)" );
      }
      break;
   }

   case SYS_SENDTO:
      /* int sendto(int s, const void *msg, int len, 
	 unsigned int flags, 
	 const struct sockaddr *to, int tolen); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.sendto(args)", arg2, 
		     6*sizeof(Addr) );
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.sendto(msg)",
		     ((UInt*)arg2)[1], /* msg */
		     ((UInt*)arg2)[2]  /* len */ );
      pre_mem_read_sockaddr( tid, "socketcall.sendto(to.%s)",
			     (struct sockaddr *) (((UInt*)arg2)[4]), ((UInt*)arg2)[5]);
      break;

   case SYS_SEND:
      /* int send(int s, const void *msg, size_t len, int flags); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.send(args)", arg2,
		     4*sizeof(Addr) );
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.send(msg)",
		     ((UInt*)arg2)[1], /* msg */
		     ((UInt*)arg2)[2]  /* len */ );
      break;

   case SYS_RECVFROM:
      /* int recvfrom(int s, void *buf, int len, unsigned int flags,
	 struct sockaddr *from, int *fromlen); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.recvfrom(args)", 
		     arg2, 6*sizeof(Addr) );
      {
	 Addr buf_p      = ((UInt*)arg2)[1];
	 Int  len        = ((UInt*)arg2)[2];
	 Addr from_p     = ((UInt*)arg2)[4];
	 Addr fromlen_p  = ((UInt*)arg2)[5];

	 SYSCALL_TRACK( pre_mem_write, tid, "socketcall.recvfrom(buf)", 
			buf_p, len );
	 if (from_p != (Addr)NULL) 
	    buf_and_len_pre_check ( tid, from_p, fromlen_p, 
				    "socketcall.recvfrom(from)",
				    "socketcall.recvfrom(fromlen_in)" );
      }
      break;
   
   case SYS_RECV:
      /* int recv(int s, void *buf, int len, unsigned int flags); */
      /* man 2 recv says:
	 The  recv call is normally used only on a connected socket
	 (see connect(2)) and is identical to recvfrom with a  NULL
	 from parameter.
      */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.recv(args)", 
		     arg2, 4*sizeof(Addr) );
      SYSCALL_TRACK( pre_mem_write, tid, "socketcall.recv(buf)", 
		     ((UInt*)arg2)[1], /* buf */
		     ((UInt*)arg2)[2]  /* len */ );
      break;

   case SYS_CONNECT:
      /* int connect(int sockfd, 
	 struct sockaddr *serv_addr, int addrlen ); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.connect(args)", 
		     arg2, 3*sizeof(Addr) );
      SYSCALL_TRACK( pre_mem_read, tid, 
		     "socketcall.connect(serv_addr.sa_family)",
		     ((UInt*)arg2)[1], /* serv_addr */
		     sizeof (sa_family_t));
      pre_mem_read_sockaddr( tid,
			     "socketcall.connect(serv_addr.%s)",
			     (struct sockaddr *) (((UInt*)arg2)[1]), ((UInt*)arg2)[2]);
      break;

   case SYS_SETSOCKOPT:
      /* int setsockopt(int s, int level, int optname, 
	 const void *optval, int optlen); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.setsockopt(args)", 
		     arg2, 5*sizeof(Addr) );
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.setsockopt(optval)",
		     ((UInt*)arg2)[3], /* optval */
		     ((UInt*)arg2)[4]  /* optlen */ );
      break;

   case SYS_GETSOCKOPT:
      /* int setsockopt(int s, int level, int optname, 
	 void *optval, socklen_t *optlen); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.getsockopt(args)", 
		     arg2, 5*sizeof(Addr) );
      {
	 Addr optval_p  = ((UInt*)arg2)[3];
	 Addr optlen_p  = ((UInt*)arg2)[4];
	 /* vg_assert(sizeof(socklen_t) == sizeof(UInt)); */
	 if (optval_p != (Addr)NULL) 
	    buf_and_len_pre_check ( tid, optval_p, optlen_p,
				    "socketcall.getsockopt(optval)",
				    "socketcall.getsockopt(optlen)" );
      }
      break;

   case SYS_GETSOCKNAME:
      /* int getsockname(int s, struct sockaddr* name, int* namelen) */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.getsockname(args)",
		     arg2, 3*sizeof(Addr) );
      {
	 Addr name_p     = ((UInt*)arg2)[1];
	 Addr namelen_p  = ((UInt*)arg2)[2];

	 /* Nb: name_p cannot be NULL */
	 buf_and_len_pre_check ( tid, name_p, namelen_p,
				 "socketcall.getsockname(name)",
				 "socketcall.getsockname(namelen_in)" );
      }
      break;

   case SYS_GETPEERNAME:
      /* int getpeername(int s, struct sockaddr* name, int* namelen) */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.getpeername(args)",
		     arg2, 3*sizeof(Addr) );
      {
	 Addr name_p     = ((UInt*)arg2)[1];
	 Addr namelen_p  = ((UInt*)arg2)[2];

	 /* Nb: name_p cannot be NULL */
	 buf_and_len_pre_check ( tid, name_p, namelen_p,
				 "socketcall.getpeername(name)",
				 "socketcall.getpeername(namelen_in)" );
      }
      break;

   case SYS_SHUTDOWN:
      /* int shutdown(int s, int how); */
      SYSCALL_TRACK( pre_mem_read, tid, "socketcall.shutdown(args)", 
		     arg2, 2*sizeof(Addr) );
      break;

   case SYS_SENDMSG: {
      /* int sendmsg(int s, const struct msghdr *msg, int flags); */

      /* this causes warnings, and I don't get why. glibc bug?
       * (after all it's glibc providing the arguments array)
       SYSCALL_TRACK( pre_mem_read, "socketcall.sendmsg(args)", 
       arg2, 3*sizeof(Addr) );
      */

      struct msghdr *msg = (struct msghdr *)((UInt *)arg2)[ 1 ];
      msghdr_foreachfield ( tid, msg, pre_mem_read_sendmsg );

      break;
   }
      
   case SYS_RECVMSG: {
      /* int recvmsg(int s, struct msghdr *msg, int flags); */

      /* this causes warnings, and I don't get why. glibc bug?
       * (after all it's glibc providing the arguments array)
       SYSCALL_TRACK( pre_mem_read, "socketcall.recvmsg(args)", 
       arg2, 3*sizeof(Addr) );
      */

      struct msghdr *msg = (struct msghdr *)((UInt *)arg2)[ 1 ];
      msghdr_foreachfield ( tid, msg, pre_mem_write_recvmsg );

      break;
   }

   default:
      VG_(message)(Vg_DebugMsg,"Warning: unhandled socketcall 0x%x",arg1);
      res = -VKI_EINVAL;
      break;
   }
}

POST(socketcall)
{
   /* int socketcall(int call, unsigned long *args); */
   MAYBE_PRINTF("socketcall ( %d, %p )\n",arg1,arg2);

   switch (arg1 /* request */) {

   case SYS_SOCKETPAIR:
      /* XXX TODO: check return fd against VG_(max_fd) */
      VG_TRACK( post_mem_write, ((UInt*)arg2)[3], 2*sizeof(int) );
      if(VG_(clo_track_fds)) {
         record_fd_open(tid, ((UInt*)((UInt*)arg2)[3])[0], NULL);
         record_fd_open(tid, ((UInt*)((UInt*)arg2)[3])[1], NULL);
      }
      break;

   case SYS_SOCKET:
      if (!fd_allowed(res, "socket", tid)) {
	 VG_(close)(res);
	 res = -VKI_EMFILE;
      } else {
         if(VG_(clo_track_fds))
            record_fd_open(tid, res, NULL);
      }
      break;

   case SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
			int addrlen); */
      break;
               
   case SYS_LISTEN:
      /* int listen(int s, int backlog); */
      break;

   case SYS_ACCEPT: {
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      if (!fd_allowed(res, "accept", tid)) {
	 VG_(close)(res);
	 res = -VKI_EMFILE;
      } else {
	 Addr addr_p     = ((UInt*)arg2)[1];
	 Addr addrlen_p  = ((UInt*)arg2)[2];

	 if (addr_p != (Addr)NULL) 
	    buf_and_len_post_check ( tid, res, addr_p, addrlen_p,
				     "socketcall.accept(addrlen_out)" );
         if(VG_(clo_track_fds))
            record_fd_open(tid, res, NULL);
      }
      break;
   }

   case SYS_SENDTO:
      break;

   case SYS_SEND:
      break;

   case SYS_RECVFROM:
      {
	 Addr buf_p      = ((UInt*)arg2)[1];
	 Int  len        = ((UInt*)arg2)[2];
	 Addr from_p     = ((UInt*)arg2)[4];
	 Addr fromlen_p  = ((UInt*)arg2)[5];

	 if (from_p != (Addr)NULL) 
	    buf_and_len_post_check ( tid, res, from_p, fromlen_p,
				     "socketcall.recvfrom(fromlen_out)" );
	 VG_TRACK( post_mem_write, buf_p, len );
      }
      break;

   case SYS_RECV:
      if (res >= 0 
	  && ((UInt*)arg2)[1] != (UInt)NULL) {
	 VG_TRACK( post_mem_write, ((UInt*)arg2)[1], /* buf */
		   ((UInt*)arg2)[2]  /* len */ );
      }
      break;

   case SYS_CONNECT:
      break;

   case SYS_SETSOCKOPT:
      break;

   case SYS_GETSOCKOPT:
      {
	 Addr optval_p  = ((UInt*)arg2)[3];
	 Addr optlen_p  = ((UInt*)arg2)[4];

	 if (optval_p != (Addr)NULL) 
	    buf_and_len_post_check ( tid, res, optval_p, optlen_p,
				     "socketcall.getsockopt(optlen_out)" );
      }
      break;

   case SYS_GETSOCKNAME:
      {
	 Addr name_p     = ((UInt*)arg2)[1];
	 Addr namelen_p  = ((UInt*)arg2)[2];

	 buf_and_len_post_check ( tid, res, name_p, namelen_p,
				  "socketcall.getsockname(namelen_out)" );
      }
      break;

   case SYS_GETPEERNAME:
      {
	 Addr name_p     = ((UInt*)arg2)[1];
	 Addr namelen_p  = ((UInt*)arg2)[2];

	 buf_and_len_post_check ( tid, res, name_p, namelen_p,
				  "socketcall.getpeername(namelen_out)" );
      }
      break;

   case SYS_SHUTDOWN:
      break;

   case SYS_SENDMSG:
      break;

   case SYS_RECVMSG:
   {
      struct msghdr *msg = (struct msghdr *)((UInt *)arg2)[ 1 ];

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
   MAYBE_PRINTF("stat ( %p, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "stat(file_name)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "stat(buf)", 
		  arg2, sizeof(struct stat) );
}

POST(stat)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct stat) );
}

PRE(statfs)
{
   /* int statfs(const char *path, struct statfs *buf); */
   MAYBE_PRINTF("statfs ( %p, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "statfs(path)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "statfs(buf)", 
		  arg2, sizeof(struct statfs) );
}

POST(statfs)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct statfs) );
}

PRE(statfs64)
{
   /* int statfs64(const char *path, struct statfs *buf); */
   MAYBE_PRINTF("statfs64 ( %p, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "statfs64(path)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "statfs64(buf)", 
		  arg2, sizeof(struct statfs64) );
}

POST(statfs64)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct statfs64) );
}

PRE(symlink)
{
   /* int symlink(const char *oldpath, const char *newpath); */
   MAYBE_PRINTF("symlink ( %p, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "symlink(oldpath)", arg1 );
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "symlink(newpath)", arg2 );
}

PRE(stat64)
{
   /* int stat64(const char *file_name, struct stat64 *buf); */
   MAYBE_PRINTF("stat64 ( %p, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "stat64(file_name)", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "stat64(buf)", 
		  arg2, sizeof(struct stat64) );
}

POST(stat64)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct stat64) );
}

PRE(fstat64)
{
   /* int fstat64(int filedes, struct stat64 *buf); */
   MAYBE_PRINTF("fstat64 ( %d, %p )\n",arg1,arg2);
   SYSCALL_TRACK( pre_mem_write, tid, "fstat64(buf)", 
		  arg2, sizeof(struct stat64) );
}

POST(fstat64)
{
   VG_TRACK( post_mem_write, arg2, sizeof(struct stat64) );
}

PRE(sysinfo)
{
   /* int sysinfo(struct sysinfo *info); */
   MAYBE_PRINTF("sysinfo ( %p )\n",arg1);
   SYSCALL_TRACK( pre_mem_write, tid, "sysinfo(info)", 
		  arg1, sizeof(struct sysinfo) );
}

POST(sysinfo)
{
   VG_TRACK( post_mem_write, arg1, sizeof(struct sysinfo) );
}

PRE(time)
{
   /* time_t time(time_t *t); */
   MAYBE_PRINTF("time ( %p )\n",arg1);
   if (arg1 != (UInt)NULL) {
      SYSCALL_TRACK( pre_mem_write, tid, "time", arg1, sizeof(time_t) );
   }
}

POST(time)
{
   if (arg1 != (UInt)NULL) {
      VG_TRACK( post_mem_write, arg1, sizeof(time_t) );
   }
}

PRE(times)
{
   /* clock_t times(struct tms *buf); */
   MAYBE_PRINTF("times ( %p )\n",arg1);
   SYSCALL_TRACK( pre_mem_write, tid, "times(buf)", 
		  arg1, sizeof(struct tms) );
}

POST(times)
{
   if (arg1 != (UInt)NULL) {
      VG_TRACK( post_mem_write, arg1, sizeof(struct tms) );
   }
}

PRE(truncate)
{
   /* int truncate(const char *path, size_t length); */
   MAYBE_PRINTF("truncate ( %p \"%s\", %d )\n", arg1,arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "truncate(path)", arg1 );
}

PRE(umask)
{
   /* mode_t umask(mode_t mask); */
   MAYBE_PRINTF("umask ( %d )\n", arg1);
}

PRE(unlink)
{
   /* int unlink(const char *pathname) */
   MAYBE_PRINTF("unlink ( %p \"%s\" )\n",arg1, arg1);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "unlink(pathname)", arg1 );
}

PRE(uname)
{
   /* int uname(struct utsname *buf); */
   MAYBE_PRINTF("uname ( %p )\n",arg1);
   SYSCALL_TRACK( pre_mem_write, tid, "uname(buf)", 
		  arg1, sizeof(struct utsname) );
}

POST(uname)
{
   if (arg1 != (UInt)NULL) {
      VG_TRACK( post_mem_write, arg1, sizeof(struct utsname) );
   }
}

PRE(utime)
{
   /* int utime(const char *filename, struct utimbuf *buf); */
   MAYBE_PRINTF("utime ( %p, %p )\n", arg1,arg2);
   SYSCALL_TRACK( pre_mem_read_asciiz, tid, "utime(filename)", arg1 );
   if (arg2 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_read, tid, "utime(buf)", arg2, 
		     sizeof(struct utimbuf) );
}

PRE(waitpid)
{
   /* pid_t waitpid(pid_t pid, int *status, int options); */

   MAYBE_PRINTF("waitpid ( %d, %p, %d )\n",
                arg1,arg2,arg3);
   if (arg2 != (Addr)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "waitpid(status)",
                     arg2, sizeof(int) );
}

POST(waitpid)
{
   if (arg2 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg2, sizeof(int) );
}

PRE(wait4)
{
   /* pid_t wait4(pid_t pid, int *status, int options,
      struct rusage *rusage) */
   MAYBE_PRINTF("wait4 ( %d, %p, %d, %p )\n",
		arg1,arg2,arg3,arg4);
   arg3 &= ~(VKI__WCLONE | VKI__WALL);

   if (arg2 != (Addr)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "wait4(status)", 
		     arg2, sizeof(int) );
   if (arg4 != (Addr)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "wait4(rusage)", arg4, 
		     sizeof(struct rusage) );
}

POST(wait4)
{
   if (arg2 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg2, sizeof(int) );
   if (arg4 != (Addr)NULL)
      VG_TRACK( post_mem_write, arg4, sizeof(struct rusage) );
}

PRE(writev)
{
   /* int writev(int fd, const struct iovec * vector, size_t count); */
   Int i;
   struct iovec * vec;
   MAYBE_PRINTF("writev ( %d, %p, %d )\n",arg1,arg2,arg3);
   if (!fd_allowed(arg1, "writev", tid)) {
      res = -VKI_EBADF;
   } else {
      SYSCALL_TRACK( pre_mem_read, tid, "writev(vector)", 
		     arg2, arg3 * sizeof(struct iovec) );
      /* ToDo: don't do any of the following if the vector is invalid */
      vec = (struct iovec *)arg2;
      for (i = 0; i < (Int)arg3; i++)
	 SYSCALL_TRACK( pre_mem_read, tid, "writev(vector[...])",
			(UInt)vec[i].iov_base,vec[i].iov_len );
   }
}

PRE(prctl)
{
   /* int prctl(int option, unsigned long arg2, unsigned long arg3,
      unsigned long arg4, unsigned long arg5); */
   MAYBE_PRINTF( "prctl ( %d, %d, %d, %d, %d )\n", arg1, arg2, arg3,
		 arg4, arg5 );
}

PRE(adjtimex)
{
   struct timex *tx = (struct timex *)arg1;
   MAYBE_PRINTF("adjtimex ( %p )\n", arg1);

   SYSCALL_TRACK(pre_mem_read, tid, "adjtimex(timex->modes)", arg1, sizeof(tx->modes));

#define ADJX(bit,field) 				\
   if (tx->modes & bit)					\
      SYSCALL_TRACK(pre_mem_read, tid,			\
		    "adjtimex(timex->"#field")",	\
		    (UInt)&tx->field, sizeof(tx->field))
   ADJX(ADJ_FREQUENCY, freq);
   ADJX(ADJ_MAXERROR, maxerror);
   ADJX(ADJ_ESTERROR, esterror);
   ADJX(ADJ_STATUS, status);
   ADJX(ADJ_TIMECONST, constant);
   ADJX(ADJ_TICK, tick);
#undef ADJX
   
   SYSCALL_TRACK(pre_mem_write, tid, "adjtimex(timex)", arg1, sizeof(struct timex));
}

POST(adjtimex)
{
   VG_TRACK(post_mem_write, arg1, sizeof(struct timex));
}

PRE(clock_gettime)
{
    /* int clock_gettime(clockid_t clk_id, struct timespec *tp); */
    MAYBE_PRINTF("clock_gettime(%d, %p)\n" ,arg1,arg2);
    SYSCALL_TRACK(pre_mem_write, tid, "clock_gettime(tp)",
                   arg2, sizeof(struct timespec));
}

POST(clock_gettime)
{
    if (!VG_(is_kerror)(res) && res == 0)
       VG_TRACK( post_mem_write, arg2, sizeof(struct timespec) );
}

PRE(utimes)
{
    /* int utimes(const char *filename, struct timeval *tvp); */
    MAYBE_PRINTF("utimes ( %p, %p )\n", arg1,arg2);
    SYSCALL_TRACK( pre_mem_read_asciiz, tid, "utimes(filename)", arg1 );
    if (arg2 != (UInt)NULL)
         SYSCALL_TRACK( pre_mem_read, tid, "utimes(tvp)", arg2,
                       sizeof(struct timeval) );
}

PRE(futex)
{
    /* int futex(void *futex, int op, int val, const struct timespec *timeout); */
    MAYBE_PRINTF("futex ( %p, %d, %d, %p, %p )\n", arg1,arg2,arg3,arg4,arg5);
    SYSCALL_TRACK( pre_mem_read, tid, "futex(futex)", arg1, sizeof(int) );
    if (arg2 == VKI_FUTEX_WAIT && arg4 != (UInt)NULL)
       SYSCALL_TRACK( pre_mem_read, tid, "futex(timeout)", arg4,
                      sizeof(struct timespec) );
    if (arg2 == VKI_FUTEX_REQUEUE)
       SYSCALL_TRACK( pre_mem_read, tid, "futex(futex2)", arg4, sizeof(int) );
}

POST(futex)
{
   if (!VG_(is_kerror)(res)) {
      VG_TRACK( post_mem_write, arg1, sizeof(int) );
      if (arg2 == VKI_FUTEX_FD && VG_(clo_track_fds))
         record_fd_open(tid, res, NULL);
   }
}

#define SIGNAL_SIMULATION	1

PRE(pause)
{
   /* int pause(void); */
   MAYBE_PRINTF("pause ( )\n");
}

PRE(rt_sigsuspend)
{
   /* int sigsuspend(const sigset_t *mask); */
   MAYBE_PRINTF("sigsuspend ( %p )\n", arg1 );
   if (arg1 != (Addr)NULL) {
      /* above NULL test is paranoia */
      SYSCALL_TRACK( pre_mem_read, tid, "sigsuspend(mask)", arg1, 
		     sizeof(vki_ksigset_t) );
   }
}

PREALIAS(sigsuspend, rt_sigsuspend);

PRE(rt_sigtimedwait)
{
   /* int sigtimedwait(const  sigset_t  *set,  siginfo_t  *info,
      const struct timespec timeout); */
   MAYBE_PRINTF("sigtimedwait ( %p, %p, timeout )\n", arg1, arg2);
   if (arg1 != (UInt)NULL) 
      SYSCALL_TRACK( pre_mem_read,  tid, "sigtimedwait(set)",  arg1,
                     sizeof(vki_ksigset_t));
   if (arg2 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "sigtimedwait(info)", arg2,
		     sizeof(siginfo_t) );
}

POST(rt_sigtimedwait)
{
   if (arg2 != (UInt)NULL)
      VG_TRACK( post_mem_write, arg2, sizeof(siginfo_t) );
}

PRE(rt_sigqueueinfo)
{
   /*  long sys_rt_sigqueueinfo(int pid, int sig, siginfo_t *uinfo) */
   MAYBE_PRINTF("rt_sigqueueinfo(%d, %d, %p)\n", arg1, arg2, arg3);
   if (arg2 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_read, tid, "sigqueueinfo(uinfo)", arg3, 
		     sizeof(siginfo_t) );
}

POST(rt_sigqueueinfo)
{
   if (res >= 0 && 
       arg2 != 0 &&
       !VG_(is_sig_ign)(arg2) &&
       !VG_(ksigismember)(&tst->eff_sig_mask, arg2) &&
       arg1 == VG_(getpid)()) {
      VG_(proxy_waitsig)();
   }
}

PRE(sigaltstack)
{
   /* int sigaltstack(const stack_t *ss, stack_t *oss); */
   MAYBE_PRINTF("sigaltstack ( %p, %p )\n",arg1,arg2);
   if (arg1 != (UInt)NULL) {
      SYSCALL_TRACK( pre_mem_read, tid, "sigaltstack(ss)", 
		     arg1, sizeof(vki_kstack_t) );
   }
   if (arg2 != (UInt)NULL) {
      SYSCALL_TRACK( pre_mem_write, tid, "sigaltstack(oss)", 
		     arg2, sizeof(vki_kstack_t) );
   }

   if (SIGNAL_SIMULATION)
      VG_(do__NR_sigaltstack) (tid);
}

POST(sigaltstack)
{
   if (res == 0 && arg2 != (UInt)NULL)
      VG_TRACK( post_mem_write, arg2, sizeof(vki_kstack_t));
}

PRE(sigaction)
{
   /* int sigaction(int signum, struct k_sigaction *act, 
      struct k_sigaction *oldact); */
   MAYBE_PRINTF("sigaction ( %d, %p, %p )\n",arg1,arg2,arg3);
   if (arg2 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_read, tid, "sigaction(act)", 
		     arg2, sizeof(vki_ksigaction));
   if (arg3 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "sigaction(oldact)", 
		     arg3, sizeof(vki_ksigaction));

   if (SIGNAL_SIMULATION)
      VG_(do__NR_sigaction)(tid);
}

POST(sigaction)
{
   if (res == 0 && arg3 != (UInt)NULL)
      VG_TRACK( post_mem_write, arg3, sizeof(vki_ksigaction));
}

PREALIAS(rt_sigaction, sigaction);
POSTALIAS(rt_sigaction, sigaction);

PRE(sigprocmask)
{
   /* int sigprocmask(int how, k_sigset_t *set, 
      k_sigset_t *oldset); */
   MAYBE_PRINTF("sigprocmask ( %d, %p, %p )\n",arg1,arg2,arg3);
   if (arg2 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_read, tid, "sigprocmask(set)", 
		     arg2, sizeof(vki_ksigset_t));
   if (arg3 != (UInt)NULL)
      SYSCALL_TRACK( pre_mem_write, tid, "sigprocmask(oldset)", 
		     arg3, sizeof(vki_ksigset_t));

   if (SIGNAL_SIMULATION)
      VG_(do__NR_sigprocmask) ( tid, 
				arg1 /*how*/, 
				(vki_ksigset_t*) arg2,
				(vki_ksigset_t*) arg3 );
}

POST(sigprocmask)
{
   if (res == 0 && arg3 != (UInt)NULL)
      VG_TRACK( post_mem_write, arg3, sizeof(vki_ksigset_t));
}

PREALIAS(rt_sigprocmask, sigprocmask);
POSTALIAS(rt_sigprocmask, sigprocmask);

PRE(sigpending)
{
   /* int sigpending( sigset_t *set ) ; */
   MAYBE_PRINTF( "sigpending ( %p )\n", arg1 );
   SYSCALL_TRACK( pre_mem_write, tid, "sigpending(set)", 
		  arg1, sizeof(vki_ksigset_t));
}

POST(sigpending)
{
   if ( !VG_( is_kerror )( res ) && res == 0 )
      VG_TRACK( post_mem_write, arg1, sizeof( vki_ksigset_t ) ) ;
}

PREALIAS(rt_sigpending, sigpending);
POSTALIAS(rt_sigpending, sigpending);


#undef SYSNO
#undef res
#undef arg1
#undef arg2
#undef arg3
#undef arg4
#undef arg5
#undef arg6

struct sys_info {
   Bool	may_block;		/* is a potentially blocking syscall */
   void	(*before)(ThreadId tid, ThreadState *tst);
   void	(*after)(ThreadId tid, ThreadState *tst);
};
#define SYSB_(name, blk)	[__NR_##name] = { blk, before_##name, NULL }
#define SYSBA(name, blk)	[__NR_##name] = { blk, before_##name, after_##name }

static void bad_before(ThreadId tid, ThreadState *tst)
{
   VG_(message)
      (Vg_DebugMsg,"WARNING: unhandled syscall: %d", tst->m_eax);
   if (VG_(clo_verbosity) > 1) {
      ExeContext *ec = VG_(get_ExeContext)(tid);
      VG_(pp_ExeContext)(ec);
   }
   VG_(message)
      (Vg_DebugMsg,"Do not panic.  You may be able to fix this easily.");
   VG_(message)
      (Vg_DebugMsg,"Read the file README_MISSING_SYSCALL_OR_IOCTL.");

   tst->m_eax = -VKI_ENOSYS;
}

static void bad_after(ThreadId tid, ThreadState *tst)
{
}

static const struct sys_info bad_sys = { False, bad_before, bad_after };

static const struct sys_info special_sys[] = {
   /* special */
   SYSB_(exit_group,		False),
   SYSB_(exit,			False),
   SYSB_(clone,			False),

   SYSB_(modify_ldt,		False),
   SYSB_(set_thread_area,	False),
   SYSB_(get_thread_area,	False),

   SYSB_(execve,		False),
   SYSB_(brk,			False),
   SYSB_(mmap,			False),
   SYSB_(mremap,		False),

#if SIGNAL_SIMULATION
   SYSBA(sigaltstack,		False),
   SYSBA(rt_sigaction,		False),
   SYSBA(sigaction,		False),
   SYSBA(rt_sigprocmask,	False),
   SYSBA(sigprocmask,		False),
#endif /* SIGNAL_SIMULATION */
};
#define MAX_SPECIAL_SYS		(sizeof(special_sys)/sizeof(special_sys[0]))

static const struct sys_info sys_info[] = {
   SYSBA(ptrace,		False),
   SYSB_(mount,			True),
   SYSB_(umount,		False),

   SYSB_(setresgid,		False),
   SYSB_(vhangup,		False),
   SYSB_(iopl,			False),

   SYSB_(setxattr,		True),
   SYSB_(lsetxattr,		True),
   SYSB_(fsetxattr,		True),
   SYSBA(getxattr,		True),
   SYSBA(lgetxattr,		True),
   SYSBA(fgetxattr,		True),
   SYSBA(listxattr,		True),
   SYSBA(llistxattr,		True),
   SYSBA(flistxattr,		True),
   SYSB_(removexattr,		True),
   SYSB_(lremovexattr,		True),
   SYSB_(fremovexattr,		True),

   SYSB_(quotactl,		False),
   SYSBA(lookup_dcookie,	False),

   SYSB_(truncate64,		True),
   SYSB_(fdatasync,		True),
   SYSB_(msync,			True),

   SYSBA(getpmsg,		True),
   SYSB_(putpmsg,		True),

   SYSBA(syslog,		True),
   SYSB_(personality,		False),
   SYSB_(chroot,		False),
   SYSB_(madvise,		True),
   SYSB_(nice,			False),
   SYSB_(setresgid32,		False),
   SYSB_(setfsuid32,		False),
   SYSBA(_sysctl,		False),

   SYSB_(sched_getscheduler,	False),	/* ??? */
   SYSB_(sched_setscheduler,	False),	/* ??? */

   SYSB_(mlock,			True),
   SYSB_(munlock,		True),
   SYSB_(mlockall,		True),
   SYSB_(munlockall,		True),

   SYSB_(sched_get_priority_max,	False),	/* ??? */
   SYSB_(sched_get_priority_min,	False),	/* ??? */

   SYSB_(setpriority,		False),
   SYSB_(getpriority,		False),

   SYSB_(setfsgid,		False),
   SYSB_(setregid,		False),
   SYSB_(setresuid,		False),
   SYSB_(setfsuid,		False),

   SYSBA(sendfile,		True),
   SYSBA(sendfile64,		True),
   SYSB_(pwrite64,		True),
   SYSB_(sync,			True),
   SYSBA(fstatfs,		False),
   SYSB_(getsid,		False),
   SYSBA(pread64,		True),
   SYSB_(mknod,			False),
   SYSB_(flock,			True),
   SYSB_(init_module,		True),
   SYSB_(ioperm,		False),
   SYSBA(capget,		False),
   SYSB_(capset,		False),
   SYSB_(access,		False),
   SYSB_(chdir,			False),
   SYSB_(chmod,			False),
   SYSB_(chown32,		False),
   SYSB_(lchown32,		False),
   SYSB_(chown,			False),
   SYSBA(close,			False),
   SYSBA(dup,			False),
   SYSBA(dup2,			False),
   SYSBA(fcntl,			True),
   SYSB_(fchdir,		False),
   SYSB_(fchown32,		False),
   SYSB_(fchown,		False),
   SYSB_(fchmod,		False),
   SYSBA(fcntl64,		True),
   SYSBA(fstat,			False),
   SYSBA(fork,			False),
   SYSB_(fsync,			True),
   SYSB_(ftruncate,		True),
   SYSB_(ftruncate64,		True),
   SYSBA(getdents,		True),
   SYSBA(getdents64,		True),
   SYSBA(getgroups32,		True),
   SYSBA(getgroups,		False),
   SYSBA(getcwd,		False),
   SYSB_(geteuid,		False),
   SYSB_(geteuid32,		False),
   SYSB_(getegid,		False),
   SYSB_(getegid32,		False),
   SYSB_(getgid,		False),
   SYSB_(getgid32,		False),
   SYSB_(getpid,		False),
   SYSB_(getpgid,		False),
   SYSB_(getpgrp,		False),
   SYSB_(getppid,		False),
   SYSBA(getresgid,		False),
   SYSBA(getresgid32,		False),
   SYSBA(getresuid,		False),
   SYSBA(getresuid32,		False),
   SYSBA(ugetrlimit,		False),
   SYSBA(getrlimit,		False),
   SYSBA(getrusage,		False),
   SYSBA(gettimeofday,		False),
   SYSB_(getuid,		False),
   SYSB_(getuid32,		False),
   SYSBA(ipc,			True),
   SYSBA(ioctl,			True),
   SYSBA(kill,			False),
   SYSB_(link,			True),
   SYSB_(lseek,			False),
   SYSBA(_llseek,		False),
   SYSBA(lstat,			False),
   SYSBA(lstat64,		False),
   SYSB_(mkdir,			True),
   SYSBA(mprotect,		False),
   SYSBA(munmap,		False),
   SYSBA(mincore,		False),
   SYSBA(nanosleep,		True),
   SYSB_(_newselect,		True),
   SYSBA(open,			True),
   SYSBA(read,			True),
   SYSB_(write,			True),
   SYSBA(creat,			True),
   SYSBA(pipe,			False),
   SYSBA(poll,			True),
   SYSBA(epoll_create,          False),
   SYSB_(epoll_ctl,             False),
   SYSBA(epoll_wait,		True),
   SYSBA(readlink,		False),
   SYSBA(readv,			True),
   SYSB_(rename,		False),
   SYSB_(rmdir,			True),
   SYSBA(sched_setparam,	False),	/* ??? */
   SYSBA(sched_getparam,	False),	/* ??? */
   SYSB_(sched_yield,		False),	/* ??? */
   SYSB_(select,		True),
   SYSB_(setfsgid32,		False),
   SYSB_(setgid32,		False),
   SYSB_(setgid,		False),
   SYSB_(setsid,		False),
   SYSB_(setgroups32,		False),
   SYSB_(setgroups,		False),
   SYSBA(setpgid,		False),
   SYSB_(setregid32,		False),
   SYSB_(setresuid32,		False),
   SYSB_(setreuid32,		False),
   SYSB_(setreuid,		False),
   SYSB_(setrlimit,		False),
   SYSB_(setuid32,		False),
   SYSB_(setuid,		False),
   SYSBA(socketcall,		True),
   SYSBA(stat,			False),
   SYSBA(statfs,		False),
   SYSBA(statfs64,		False),
   SYSB_(symlink,		True),
   SYSBA(stat64,		False),
   SYSBA(fstat64,		False),
   SYSBA(sysinfo,		False),
   SYSBA(time,			False),
   SYSBA(times,			False),
   SYSB_(truncate,		True),
   SYSB_(umask,			False),
   SYSB_(unlink,		True),
   SYSBA(uname,			False),
   SYSB_(utime,			True),
   SYSB_(utimes,                False),
   SYSBA(waitpid,		True),
   SYSBA(wait4,			True),
   SYSB_(writev,		True),
   SYSB_(prctl,			True),
   SYSBA(adjtimex,		False),
   SYSBA(mmap2,			False),
   SYSBA(clock_gettime,         False),
   SYSBA(futex,                 True),

   /* new signal handling makes these normal blocking syscalls */
   SYSB_(pause,			True),
   SYSB_(sigsuspend,		True),
   SYSB_(rt_sigsuspend,		True),
   SYSBA(rt_sigtimedwait,	True),
   SYSBA(rt_sigqueueinfo,	False),

   SYSBA(sigpending,		True), /* not blocking, but must run in LWP context */
   SYSBA(rt_sigpending,		True), /* not blocking, but must run in LWP context */
   SYSB_(alarm,			True), /* not blocking, but must run in LWP context */
   SYSBA(setitimer,		True), /* not blocking, but must run in LWP context */
   SYSBA(getitimer,		True), /* not blocking, but must run in LWP context */

#if !SIGNAL_SIMULATION
   SYSBA(sigaltstack,		False),
   SYSBA(rt_sigaction,		False),
   SYSBA(sigaction,		False),
   SYSBA(rt_sigprocmask,	False),
   SYSBA(sigprocmask,		False),
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
   if (tst->m_eax == __NR_vfork)
      tst->m_eax = __NR_fork;

   syscallno = tst->m_eax;

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

   /* Do any pre-syscall actions */
   if (VG_(needs).syscall_wrapper) {
      VGP_PUSHCC(VgpSkinSysWrap);
      tst->sys_pre_res = SK_(pre_syscall)(tid, syscallno, /*isBlocking*/sys->may_block);
      VGP_POPCC(VgpSkinSysWrap);
   }

   MAYBE_PRINTF("SYSCALL[%d,%d](%3d)%s%s:", 
		VG_(getpid)(), tid, syscallno, 
		special ? " special" : "",
		sys->may_block ? " blocking" : "");

   if (special) {
      /* "Special" syscalls are implemented by Valgrind internally,
	 and do not generate real kernel calls.  The expectation,
	 therefore, is that the "before" function not only does the
	 appropriate tests, but also performs the syscall itself and
	 sets the result.  Special syscalls cannot block. */
      vg_assert(sys->may_block == False);

      (sys->before)(tst->tid, tst);

      syscall_done = True;
   } else {
      (sys->before)(tst->tid, tst);

      if ((Int)tst->m_eax <= 0) {
	 /* "before" decided the syscall wasn't viable, so don't do
	    anything - just pretend the syscall happened. */
	 syscall_done = True;
      } else if (sys->may_block) {
	 /* Issue to worker.  If we're waiting on the syscall because
	    it's in the hands of the ProxyLWP, then set the thread
	    state to WaitSys. */
	 tst->status = VgTs_WaitSys;
	 VG_(sys_issue)(tid);
      } else {
	 /* run the syscall directly */
	 tst->m_eax = VG_(do_syscall)(syscallno, 
				      tst->m_ebx,
				      tst->m_ecx, 
				      tst->m_edx,
				      tst->m_esi,
				      tst->m_edi,
				      tst->m_ebp);
	 syscall_done = True;
      }
   }

   VGP_POPCC(VgpCoreSysWrap);

   vg_assert(( syscall_done && tst->status == VgTs_Runnable) ||
	     (!syscall_done && tst->status == VgTs_WaitSys ));

   return syscall_done;
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

   /* Tell the skin about the syscall return value */
   SET_SYSCALL_RETVAL(tst->tid, tst->m_eax);

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

   if (tst->m_eax == -VKI_ERESTARTSYS) {
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
	 VG_(restart_syscall)(tid);
      } else
	 tst->m_eax = -VKI_EINTR;
   } 

   if (!restarted) {
      if (!VG_(is_kerror)(tst->m_eax) && sys->after != NULL)
	 (sys->after)(tst->tid, tst);

      /* Do any post-syscall actions

	 NOTE: this is only called if the syscall completed.  If the
	 syscall was restarted, then it will call the Tool's
	 pre_syscall again, without calling post_syscall (ie, more
	 pre's than post's)
       */
      if (VG_(needs).syscall_wrapper) {
	 VGP_PUSHCC(VgpSkinSysWrap);
	 SK_(post_syscall)(tid, syscallno, pre_res, tst->m_eax, /*isBlocking*/True); // did block
	 VGP_POPCC(VgpSkinSysWrap);
      }
   }

   tst->status = VgTs_Runnable;	/* runnable again */
   tst->syscallno = -1;		/* no current syscall */

   VGP_POPCC(VgpCoreSysWrap);
}

void VG_(restart_syscall)(ThreadId tid)
{
   ThreadState* tst;
   tst = VG_(get_ThreadState)(tid);

   vg_assert(tst != NULL);
   vg_assert(tst->status == VgTs_WaitSys);
   vg_assert(tst->syscallno != -1);

   tst->m_eax = tst->syscallno;
   tst->m_eip -= 2;		/* sizeof(int $0x80) */

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80 
   */
   {
      UChar *p = (UChar *)tst->m_eip;
      
      if (p[0] != 0xcd || p[1] != 0x80)
	 VG_(message)(Vg_DebugMsg, 
		      "?! restarting over syscall at %p %02x %02x\n",
		      tst->m_eip, p[0], p[1]);

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                            vg_syscalls.c ---*/
/*--------------------------------------------------------------------*/

