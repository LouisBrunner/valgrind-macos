/*
 * Intercepts for various libc functions we want to capture (mostly for threading purposes)
 *
 * This has some nasty duplication of stuff from vg_libpthread.c
 */

#include <errno.h>
#include <sys/types.h>

#include "valgrind.h"
#include "vg_include.h"

# define strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

# define weak_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias (#name)));

#define WEAK	__attribute__((weak))

#include "vg_kerneliface.h"

static
__inline__
int is_kerror ( int res )
{
   if (res >= -4095 && res <= -1)
      return 1;
   else
      return 0;
}


static inline
int my_do_syscall5 ( int syscallno, 
                     int arg1, int arg2, int arg3, int arg4, int arg5 )
{ 
   int __res;
   __asm__ volatile ("int $0x80"
                     : "=a" (__res)
                     : "0" (syscallno),
                       "b" (arg1),
                       "c" (arg2),
                       "d" (arg3),
                       "S" (arg4),
                       "D" (arg5));
   return __res;
}

/* -------------------------------- msgsnd -------------------------------- */
#include <asm/ipc.h>		/* for ipc_kludge */

static inline int sys_ipc(unsigned call, int first, int second, int third, void *ptr)
{
   return my_do_syscall5(__NR_ipc, call, first, second, third, (int)ptr);
}

WEAK int VGL_(msgsnd)(int msgid, const void *msgp, size_t msgsz, int msgflg)
{
   int err = sys_ipc(11, msgid, msgsz, msgflg, (void *)msgp);
   if (is_kerror(err)) {
      *(__errno_location()) = -err;
      return -1;
   }
   return 0;
}

int msgsnd(int msgid, const void *msgp, size_t msgsz, int msgflg)
{
   return VGL_(msgsnd)(msgid, msgp, msgsz, msgflg);
}

/* -------------------------------- msgrcv -------------------------------- */

WEAK int VGL_(msgrcv)( int msqid, void  *msgp,  size_t msgsz, long msgtyp, int msgflg )
{
   struct ipc_kludge tmp;
   int err;

   tmp.msgp = msgp;
   tmp.msgtyp = msgtyp;

   err = sys_ipc(12, msqid, msgsz, msgflg, &tmp );

   if (is_kerror(err)) {
      *(__errno_location()) = -err;
      return -1;
   }
   return 0;
}

int msgrcv( int msqid, void  *msgp,  size_t msgsz, long msgtyp, int msgflg )
{
   return VGL_(msgrcv)( msqid, msgp,  msgsz, msgtyp, msgflg );
}

/* -------------------------------- accept -------------------------------- */

#include <sys/socket.h>

extern
int __libc_accept(int s, struct sockaddr *addr, socklen_t *addrlen);
WEAK int VGL_(accept)(int s, struct sockaddr *addr, socklen_t *addrlen)
{
   return __libc_accept(s, addr, addrlen);
}

int accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
   return VGL_(accept)(s, addr, addrlen);
}

/* -------------------------------- recv -------------------------------- */

extern
int __libc_recv(int s, void *buf, size_t len, int flags);

WEAK int VGL_(recv)(int s, void *buf, size_t len, int flags)
{
     return __libc_recv(s, buf, len, flags);
}

int recv(int s, void *buf, size_t len, int flags)
{
     return VGL_(recv)(s, buf, len, flags);
}

strong_alias(recv, __recv)
