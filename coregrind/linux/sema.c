#include "core.h"

#if FUTEX_SEMA
/* 
   Futex-based semaphore operations.

   Taken from futex-2.2/usersem.c
   Based on work by Matthew Kirkwood <matthew@hairy.beasts.org>. 
 */


#define FUTEX_PASSED (-(1024 * 1024 * 1024))

static inline Int sys_futex(Int *futex, Int op, Int val, struct vki_timespec *rel)
{
   return VG_(do_syscall)(__NR_futex, futex, op, val, rel);
}

/* Returns -1 on fail, 0 on wakeup, 1 on pass, 2 on didn't sleep */
int __futex_down_slow(vg_sema_t *futx, int val, struct vki_timespec *rel)
{
   Int ret;

   ret = sys_futex(&futx->count, VKI_FUTEX_WAIT, val, rel);
   if (ret == 0) {
      /* <= in case someone else decremented it */
      if (futx->count <= FUTEX_PASSED) {
	 futx->count = -1;
	 return 1;
      }
      return 0;
   }
   /* EWOULDBLOCK just means value changed before we slept: loop */
   if (ret == -VKI_EWOULDBLOCK)
      return 2;
   return -1;
}

int __futex_up_slow(vg_sema_t *futx)
{
   futx->count = 1;
   __futex_commit();
   return sys_futex(&futx->count, VKI_FUTEX_WAKE, 1, NULL);
}

void VGO_(sema_init)(vg_sema_t *sema)
{
   sema->count = 1;
   __futex_commit();
}

#else  /* !FUTEX_SEMA */

/* 
   Slower but more portable pipe-based token passing scheme.
 */

void VGO_(sema_init)(vg_sema_t *sema)
{
   VG_(pipe)(sema->pipe);
   sema->pipe[0] = VG_(safe_fd)(sema->pipe[0]);
   sema->pipe[1] = VG_(safe_fd)(sema->pipe[1]);

   sema->owner_thread = -1;

   /* create initial token */
   VG_(write)(sema->pipe[1], "T", 1);
}

void VGO_(sema_deinit)(vg_sema_t *sema)
{
   VG_(close)(sema->pipe[0]);
   VG_(close)(sema->pipe[1]);
   sema->pipe[0] = sema->pipe[1] = -1;
}

/* get a token */
void VGO_(sema_down)(vg_sema_t *sema)
{
   Char buf[2] = { 'x' };
   Int ret;
   Int lwpid = VG_(gettid)();

   vg_assert(sema->owner_thread != lwpid); /* can't have it already */

  again:
   ret = VG_(read)(sema->pipe[0], buf, 2);

   if (ret == -VKI_EINTR)
      goto again;

   vg_assert(ret == 1);		/* should get exactly 1 token */
   vg_assert(buf[0] == 'T');

   sema->owner_thread = lwpid;
}

/* put token back */
void VGO_(sema_up)(vg_sema_t *sema)
{
   Int ret;

   vg_assert(sema->owner_thread == VG_(gettid)()); /* must have it */

   sema->owner_thread = 0;

   ret = VG_(write)(sema->pipe[1], "T", 1);

   vg_assert(ret == 1);
}

#endif	/* FUTEX_SEMA */
