/* Scalar test for new labelsys syscall subcodes TSOL_GETCLEARANCE
   and TSOL_SETCLEARANCE available on Solaris 11. */

#include "scalar.h"

#include <sys/syscall.h>
#include <sys/tsol/tsyscall.h>
#include <tsol/label.h>

__attribute__((noinline))
static void sys_labelsys(void)
{
   GO(SYS_labelsys, "(TSOL_GETCLEARANCE) 2s 1m");
   SY(SYS_labelsys, x0 + TSOL_GETCLEARANCE, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_labelsys2(void)
{
   m_label_t *label = m_label_alloc(USER_CLEAR);
   if (label == NULL) {
      perror("m_label_alloc");
      return;
   }

   GO(SYS_labelsys, "(TSOL_GETCLEARANCE) 1s 0m");
   SY(SYS_labelsys, x0 + TSOL_GETCLEARANCE, label); SUCC;

   m_label_free(label);
}

__attribute__((noinline))
static void sys_labelsys3(void)
{
   GO(SYS_labelsys, "(TSOL_SETCLEARANCE) 2s 1m");
   SY(SYS_labelsys, x0 + TSOL_SETCLEARANCE, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_labelsys4(void)
{
   m_label_t *label = m_label_alloc(USER_CLEAR);
   if (label == NULL) {
      perror("m_label_alloc");
      return;
   }

   int ret = getclearance(label);
   if (ret != 0) {
      perror("getclearance");
      m_label_free(label);
      return;
   }

   GO(SYS_labelsys, "(TSOL_SETCLEARANCE) 1s 0m");
   SY(SYS_labelsys, x0 + TSOL_SETCLEARANCE, label); SUCC;

   m_label_free(label);
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_labelsys                52 */
   sys_labelsys();
   sys_labelsys2();
   sys_labelsys3();
   sys_labelsys4();

   return 0;
}

