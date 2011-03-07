
/* Program which uses a happens-before edge to coordinate an access to
   variable 'shared_var' between two threads.  The h-b edge is created
   by a custom (kludgesome!) mechanism and hence we need to use
   ANNOTATES_HAPPEN_{BEFORE,AFTER} to explain to Helgrind what's going
   on (else it reports a race). */

#include <pthread.h>
#include <stdio.h>
#include <assert.h>

#include "../../helgrind/helgrind.h"

/* Todo: move all this do_acasW guff into a support library.  It's
   useful for multiple tests, not just this one. 

   XXX: all the do_acasW routines assume the supplied address
   is UWord (naturally) aligned. */


typedef  unsigned long int  UWord;

#if defined(VGA_ppc64)

// ppc64
/* return 1 if success, 0 if failure */
UWord do_acasW ( UWord* addr, UWord expected, UWord nyu )
{
  UWord old, success;

  /* Fetch the old value, and set the reservation */
  __asm__ __volatile__ (
     "ldarx  %0, 0,%1"     "\n"  // rD,rA,rB
      : /*out*/   "=b"(old)
      : /*in*/    "b"(addr)
      : /*trash*/ "memory","cc"
   );

   /* If the old value isn't as expected, we've had it */
   if (old != expected) return 0;

   /* otherwise try to stuff the new value in */
   __asm__ __volatile__(
      "stdcx. %2, 0,%1"   "\n"      // rS,rA,rB
      "mfcr   %0"         "\n\t"
      "srdi   %0,%0,29"   "\n\t"
      "andi.  %0,%0,1"    "\n"
      : /*out*/ "=b"(success)
      : /*in*/ "b"(addr), "b"(nyu)
   );

   assert(success == 0 || success == 1);
   return success;
}

#elif defined(VGA_ppc32)

// ppc32
/* return 1 if success, 0 if failure */
UWord do_acasW ( UWord* addr, UWord expected, UWord nyu )
{
  UWord old, success;

  /* Fetch the old value, and set the reservation */
  __asm__ __volatile__ (
     "lwarx  %0, 0,%1"     "\n"  // rD,rA,rB
      : /*out*/   "=b"(old)
      : /*in*/    "b"(addr)
      : /*trash*/ "memory","cc"
   );

   /* If the old value isn't as expected, we've had it */
   if (old != expected) return 0;

   /* otherwise try to stuff the new value in */
   __asm__ __volatile__(
      "stwcx. %2, 0,%1"   "\n"      // rS,rA,rB
      "mfcr   %0"         "\n\t"
      "srwi   %0,%0,29"   "\n\t"
      "andi.  %0,%0,1"    "\n"
      : /*out*/ "=b"(success)
      : /*in*/ "b"(addr), "b"(nyu)
   );

   assert(success == 0 || success == 1);
   return success;
}

#elif defined(VGA_amd64)

// amd64
/* return 1 if success, 0 if failure */
UWord do_acasW ( UWord* addr, UWord expected, UWord nyu )
{
   UWord block[4] = { (UWord)addr, expected, nyu, 2 };
   __asm__ __volatile__(
      "movq 0(%%rsi),  %%rdi"         "\n\t" // addr
      "movq 8(%%rsi),  %%rax"         "\n\t" // expected
      "movq 16(%%rsi), %%rbx"         "\n\t" // nyu
      "xorq %%rcx,%%rcx"              "\n\t"
      "lock; cmpxchgq %%rbx,(%%rdi)"  "\n\t"
      "setz %%cl"                     "\n\t"
      "movq %%rcx, 24(%%rsi)"         "\n"
      : /*out*/ 
      : /*in*/ "S"(&block[0])
      : /*trash*/"memory","cc","rdi","rax","rbx","rcx"
   );
   assert(block[3] == 0 || block[3] == 1);
   return block[3] & 1;
}

#elif defined(VGA_x86)

// x86
/* return 1 if success, 0 if failure */
UWord do_acasW ( UWord* addr, UWord expected, UWord nyu )
{
   UWord block[4] = { (UWord)addr, expected, nyu, 2 };
   __asm__ __volatile__(
      "pushl %%ebx"                   "\n\t"
      "movl 0(%%esi),  %%edi"         "\n\t" // addr
      "movl 4(%%esi),  %%eax"         "\n\t" // expected
      "movl 8(%%esi),  %%ebx"         "\n\t" // nyu
      "xorl %%ecx,%%ecx"              "\n\t"
      "lock; cmpxchgl %%ebx,(%%edi)"  "\n\t"
      "setz %%cl"                     "\n\t"
      "movl %%ecx, 12(%%esi)"         "\n\t"
      "popl %%ebx"                    "\n"
      : /*out*/ 
      : /*in*/ "S"(&block[0])
      : /*trash*/"memory","cc","edi","eax","ecx"
   );
   assert(block[3] == 0 || block[3] == 1);
   return block[3] & 1;
}

#elif defined(VGA_arm)

// arm
/* return 1 if success, 0 if failure */
UWord do_acasW ( UWord* addr, UWord expected, UWord nyu )
{
  UWord old, success;
  UWord block[2] = { (UWord)addr, nyu };

  /* Fetch the old value, and set the reservation */
  __asm__ __volatile__ (
     "ldrex  %0, [%1]"    "\n"
      : /*out*/   "=r"(old)
      : /*in*/    "r"(addr)
   );

   /* If the old value isn't as expected, we've had it */
   if (old != expected) return 0;

   /* otherwise try to stuff the new value in */
   __asm__ __volatile__(
      "ldr    r4, [%1, #0]"      "\n\t"
      "ldr    r5, [%1, #4]"      "\n\t"
      "strex  r6, r5, [r4, #0]"  "\n\t"
      "eor    %0, r6, #1"        "\n\t"
      : /*out*/ "=r"(success)
      : /*in*/ "r"(&block[0])
      : /*trash*/ "r4","r5","r6","memory"
   );
   assert(success == 0 || success == 1);
   return success;
}

#elif defined(VGA_s390x)

// s390x
/* return 1 if success, 0 if failure */
UWord do_acasW(UWord* addr, UWord expected, UWord nyu )
{
   int cc;

   __asm__ __volatile__ (
     "csg %2,%3,%1\n\t"
     "ipm %0\n\t"
     "srl %0,28\n\t"
     : /* out */  "=r" (cc)
     : /* in */ "Q" (*addr), "d" (expected), "d" (nyu)
     : "memory", "cc"
   );
   return cc == 0;
}

#endif

void atomic_incW ( UWord* w )
{
   while (1) {
      UWord old = *w;
      UWord nyu = old + 1;
      UWord ok = do_acasW( w, old, nyu );
      if (ok) break;
   };
}

#if 0

#define NNN 1000000

void* thread_fn ( void* arg )
{
  UWord* w = (UWord*)arg;
  int i;
  for (i = 0; i < NNN; i++)
     atomic_incW( w );
  return NULL;
}


int main ( void )
{
   int r;
  //ANNOTATE_HAPPENS_BEFORE(0);
  //return 0;
   UWord w = 0;
  pthread_t t1, t2;

  r= pthread_create( &t1, NULL, &thread_fn, (void*)&w );   assert(!r);
  r= pthread_create( &t2, NULL, &thread_fn, (void*)&w );   assert(!r);

  r= pthread_join( t1, NULL );   assert(!r);
  r= pthread_join( t2, NULL );   assert(!r);

  printf("result = %lu\n", w );
  return 0;
}

#endif

int shared_var = 0;  // is not raced upon


void delay100ms ( void )
{
   struct timespec ts = { 0, 100 * 1000 * 1000 };
   nanosleep(&ts, NULL);
}

void do_wait ( UWord* w )
{
  UWord w0 = *w;
  UWord volatile * wV = w;
  while (*wV == w0)
    ;
  ANNOTATE_HAPPENS_AFTER(w);
}

void do_signal ( UWord* w )
{
  ANNOTATE_HAPPENS_BEFORE(w);
  atomic_incW(w);
}



void* thread_fn1 ( void* arg )
{
  UWord* w = (UWord*)arg;
  delay100ms();    // ensure t2 gets to its wait first
  shared_var = 1;  // first access
  do_signal(w);    // cause h-b edge to second thread

  delay100ms();
  return NULL;
}

void* thread_fn2 ( void* arg )
{
  UWord* w = (UWord*)arg;
  do_wait(w);      // wait for h-b edge from first thread
  shared_var = 2;  // second access

  delay100ms();
  return NULL;
}






int main ( void )
{
   int r;
   UWord w = 0;
   pthread_t t1, t2;

   r= pthread_create( &t1, NULL, &thread_fn1, (void*)&w );   assert(!r);
   r= pthread_create( &t2, NULL, &thread_fn2, (void*)&w );   assert(!r);

   r= pthread_join( t1, NULL );   assert(!r);
   r= pthread_join( t2, NULL );   assert(!r);
   return 0;
}
