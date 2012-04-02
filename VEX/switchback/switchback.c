
/* HOW TO USE

13 Dec '05 - Linker no longer used (apart from mymalloc)
Simply compile and link switchback.c with test_xxx.c,
e.g. for ppc64:
$ (cd .. && make EXTRA_CFLAGS="-m64" libvex_ppc64_linux.a) && gcc -m64 -mregnames -Wall -Wshadow -Wno-long-long -Winline -O -g -o switchback switchback.c linker.c ../libvex_ppc64_linux.a test_xxx.c

Test file test_xxx.c must have an entry point called "entry",
which expects to take a single argument which is a function pointer
(to "serviceFn").

Test file may not reference any other symbols.

NOTE: POWERPC: it is critical, when using this on ppc, to set
CacheLineSize to the right value.  Values we currently know of:

   imac (G3):   32
   G5 (ppc970): 128
*/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_guest_x86.h"
#include "../pub/libvex_guest_amd64.h"
#include "../pub/libvex_guest_ppc32.h"
#include "../pub/libvex_guest_ppc64.h"
#include "../pub/libvex.h"
#include "../pub/libvex_trc_values.h"
#include "linker.h"

static ULong n_bbs_done = 0;
static Int   n_translations_made = 0;


#if defined(__i386__)
#  define VexGuestState             VexGuestX86State
#  define LibVEX_Guest_initialise   LibVEX_GuestX86_initialise
#  define VexArch                   VexArchX86
#  define VexSubArch                VexSubArchX86_sse1
#  define GuestPC                   guest_EIP
#  define CacheLineSize             0/*irrelevant*/
#elif defined(__x86_64__)
#  define VexGuestState             VexGuestAMD64State
#  define LibVEX_Guest_initialise   LibVEX_GuestAMD64_initialise
#  define VexArch                   VexArchAMD64
#  define VexSubArch                VexSubArch_NONE
#  define GuestPC                   guest_RIP
#  define CacheLineSize             0/*irrelevant*/
#elif defined(__powerpc__)

#if !defined(__powerpc64__) // ppc32
#  define VexGuestState             VexGuestPPC32State
#  define LibVEX_Guest_initialise   LibVEX_GuestPPC32_initialise
#  define VexArch                   VexArchPPC32
#  define VexSubArch                VexSubArchPPC32_FI
#  define GuestPC                   guest_CIA
#  define CacheLineSize             128
#else
#  define VexGuestState             VexGuestPPC64State
#  define LibVEX_Guest_initialise   LibVEX_GuestPPC64_initialise
#  define VexArch                   VexArchPPC64
#  define VexSubArch                VexSubArchPPC64_FI
#  define GuestPC                   guest_CIA
#  define CacheLineSize             128
#endif

#else
#   error "Unknown arch"
#endif

/* 7: show conversion into IR */
/* 6: show after initial opt */
/* 5: show after instrumentation */
/* 4: show after second opt */
/* 3: show after tree building */
/* 2: show selected insns */
/* 1: show after reg-alloc */
/* 0: show final assembly */
#define TEST_FLAGS (1<<7)|(1<<3)|(1<<2)|(1<<1)|(1<<0)
#define DEBUG_TRACE_FLAGS 0//(1<<7)|(0<<6)|(0<<5)|(0<<4)|(1<<3)|(1<<2)|(1<<1)|(1<<0)


/* guest state */
UInt gstack[50000];
VexGuestState gst;
VexControl vcon;

/* only used for the switchback transition */
/* i386:  helper1 = &gst, helper2 = %EFLAGS */
/* amd64: helper1 = &gst, helper2 = %EFLAGS */
/* ppc32: helper1 = &gst, helper2 = %CR, helper3 = %XER */
HWord sb_helper1 = 0;
HWord sb_helper2 = 0;
HWord sb_helper3 = 0;

/* translation cache */
#define N_TRANS_CACHE 1000000
#define N_TRANS_TABLE 10000

ULong trans_cache[N_TRANS_CACHE];
VexGuestExtents trans_table [N_TRANS_TABLE];
ULong*          trans_tableP[N_TRANS_TABLE];

Int trans_cache_used = 0;
Int trans_table_used = 0;

static Bool chase_into_ok ( Addr64 dst ) { return False; }

#if 0
// local_sys_write_stderr(&c,1);
static void local_sys_write_stderr ( HChar* buf, Int n )
{
   UInt __res;
   __asm__ volatile (
      "li %%r0,4\n\t"      /* set %r0 = __NR_write */
      "li %%r3,1\n\t"      /* set %r3 = stdout */
      "mr %%r4,%1\n\t"     /* set %r4 = buf */
      "mr %%r5,%2\n\t"     /* set %r5 = n */
      "sc\n\t"             /* write(stderr, buf, n) */
      "mr %0,%%r3\n"       /* set __res = r3 */
      : "=mr" (__res)
      : "g" (buf), "g" (n)
      : "r0", "r3", "r4", "r5" );
}
#endif

/* For providing services. */
static HWord serviceFn ( HWord arg1, HWord arg2 )
{
   switch (arg1) {
      case 0: /* EXIT */
         printf("---STOP---\n");
         printf("serviceFn:EXIT\n");
	 printf("%llu bbs simulated\n", n_bbs_done);
	 printf("%d translations made, %d tt bytes\n", 
                n_translations_made, 8*trans_cache_used);
         exit(0);
      case 1: /* PUTC */
         putchar(arg2);
         return 0;
      case 2: /* MALLOC */
         return (HWord)malloc(arg2);
      case 3: /* FREE */
         free((void*)arg2);
         return 0;
      default:
         assert(0);
   }
}


/* -------------------- */
/* continue execution on the real CPU (never returns) */
extern void switchback_asm(void);

#if defined(__i386__)

asm(
"switchback_asm:\n"
"   movl sb_helper1, %eax\n"  // eax = guest state ptr
"   movl  16(%eax), %esp\n"   // switch stacks
"   pushl 56(%eax)\n"         // push continuation addr
"   movl sb_helper2, %ebx\n"  // get eflags
"   pushl %ebx\n"             // eflags:CA
"   pushl 0(%eax)\n"          //  EAX:eflags:CA
"   movl 4(%eax), %ecx\n" 
"   movl 8(%eax), %edx\n" 
"   movl 12(%eax), %ebx\n" 
"   movl 20(%eax), %ebp\n"
"   movl 24(%eax), %esi\n"
"   movl 28(%eax), %edi\n"
"   popl %eax\n"
"   popfl\n"
"   ret\n"
);
void switchback ( void )
{
   sb_helper1 = (HWord)&gst;
   sb_helper2 = LibVEX_GuestX86_get_eflags(&gst);
   switchback_asm(); // never returns
}

#elif defined(__x86_64__)

asm(
"switchback_asm:\n"
"   movq sb_helper1, %rax\n"  // rax = guest state ptr
"   movq  32(%rax), %rsp\n"   // switch stacks
"   pushq 168(%rax)\n"        // push continuation addr
"   movq sb_helper2, %rbx\n"  // get eflags
"   pushq %rbx\n"             // eflags:CA
"   pushq 0(%rax)\n"          // RAX:eflags:CA
"   movq 8(%rax), %rcx\n" 
"   movq 16(%rax), %rdx\n" 
"   movq 24(%rax), %rbx\n" 
"   movq 40(%rax), %rbp\n"
"   movq 48(%rax), %rsi\n"
"   movq 56(%rax), %rdi\n"

"   movq 64(%rax), %r8\n"
"   movq 72(%rax), %r9\n"
"   movq 80(%rax), %r10\n"
"   movq 88(%rax), %r11\n"
"   movq 96(%rax), %r12\n"
"   movq 104(%rax), %r13\n"
"   movq 112(%rax), %r14\n"
"   movq 120(%rax), %r15\n"

"   popq %rax\n"
"   popfq\n"
"   ret\n"
);
void switchback ( void )
{
   sb_helper1 = (HWord)&gst;
   sb_helper2 = LibVEX_GuestAMD64_get_rflags(&gst);
   switchback_asm(); // never returns
}

#elif defined(__powerpc__)

static void invalidate_icache(void *ptr, int nbytes)
{
   unsigned long startaddr = (unsigned long) ptr;
   unsigned long endaddr = startaddr + nbytes;
   unsigned long addr;
   unsigned long cls = CacheLineSize;

   startaddr &= ~(cls - 1);
   for (addr = startaddr; addr < endaddr; addr += cls)
      asm volatile("dcbst 0,%0" : : "r" (addr));
   asm volatile("sync");
   for (addr = startaddr; addr < endaddr; addr += cls)
      asm volatile("icbi 0,%0" : : "r" (addr));
   asm volatile("sync; isync");
}


#if !defined(__powerpc64__) // ppc32
asm(
"switchback_asm:\n"
// gst
"   lis  %r31,sb_helper1@ha\n"      // get hi-wd of guest_state_ptr addr
"   lwz  %r31,sb_helper1@l(%r31)\n" // load word of guest_state_ptr to r31

// LR
"   lwz  %r3,900(%r31)\n"           // guest_LR
"   mtlr %r3\n"                     // move to LR

// CR
"   lis  %r3,sb_helper2@ha\n"       // get hi-wd of flags addr
"   lwz  %r3,sb_helper2@l(%r3)\n"   // load flags word to r3
"   mtcr %r3\n"                     // move r3 to CR

// CTR
"   lwz %r3,904(%r31)\n"       // guest_CTR
"   mtctr %r3\n"               // move r3 to CTR

// XER
"   lis  %r3,sb_helper3@ha\n"       // get hi-wd of xer addr
"   lwz  %r3,sb_helper3@l(%r3)\n"   // load xer word to r3
"   mtxer %r3\n"                     // move r3 to XER


// GPR's
"   lwz %r0,    0(%r31)\n"
"   lwz %r1,    4(%r31)\n"     // switch stacks (r1 = SP)
"   lwz %r2,    8(%r31)\n"
"   lwz %r3,   12(%r31)\n"
"   lwz %r4,   16(%r31)\n"
"   lwz %r5,   20(%r31)\n"
"   lwz %r6,   24(%r31)\n"
"   lwz %r7,   28(%r31)\n"
"   lwz %r8,   32(%r31)\n"
"   lwz %r9,   36(%r31)\n"
"   lwz %r10,  40(%r31)\n"
"   lwz %r11,  44(%r31)\n"
"   lwz %r12,  48(%r31)\n"
"   lwz %r13,  52(%r31)\n"
"   lwz %r14,  56(%r31)\n"
"   lwz %r15,  60(%r31)\n"
"   lwz %r16,  64(%r31)\n"
"   lwz %r17,  68(%r31)\n"
"   lwz %r18,  72(%r31)\n"
"   lwz %r19,  76(%r31)\n"
"   lwz %r20,  80(%r31)\n"
"   lwz %r21,  84(%r31)\n"
"   lwz %r22,  88(%r31)\n"
"   lwz %r23,  92(%r31)\n"
"   lwz %r24,  96(%r31)\n"
"   lwz %r25, 100(%r31)\n"
"   lwz %r26, 104(%r31)\n"
"   lwz %r27, 108(%r31)\n"
"   lwz %r28, 112(%r31)\n"
"   lwz %r29, 116(%r31)\n"
"   lwz %r30, 120(%r31)\n"
"   lwz %r31, 124(%r31)\n"
"nop_start_point:\n"
"   nop\n"
"   nop\n"
"   nop\n"
"   nop\n"
"   nop\n"
"nop_end_point:\n"
);

#else // ppc64

asm(
".text\n"
"   .global switchback_asm\n"
"   .section \".opd\",\"aw\"\n"
"   .align 3\n"
"switchback_asm:\n"
"   .quad .switchback_asm,.TOC.@tocbase,0\n"
"   .previous\n"
"   .type .switchback_asm,@function\n"
"   .global  .switchback_asm\n"
".switchback_asm:\n"
"switchback_asm_undotted:\n"

// gst: load word of guest_state_ptr to r31
"   lis    %r31,sb_helper1@highest\n"
"   ori    %r31,%r31,sb_helper1@higher\n"
"   rldicr %r31,%r31,32,31\n"
"   oris   %r31,%r31,sb_helper1@h\n"
"   ori    %r31,%r31,sb_helper1@l\n"
"   ld     %r31,0(%r31)\n"


// LR
"   ld   %r3,1032(%r31)\n"          // guest_LR
"   mtlr %r3\n"                     // move to LR

// CR
"   lis    %r3,sb_helper2@highest\n"
"   ori    %r3,%r3,sb_helper2@higher\n"
"   rldicr %r3,%r3,32,31\n"
"   oris   %r3,%r3,sb_helper2@h\n"
"   ori    %r3,%r3,sb_helper2@l\n"
"   ld     %r3,0(%r3)\n"            // load flags word to r3
"   mtcr   %r3\n"                   // move r3 to CR

// CTR
"   ld     %r3,1040(%r31)\n"        // guest_CTR
"   mtctr  %r3\n"                   // move r3 to CTR

// XER
"   lis    %r3,sb_helper3@highest\n"
"   ori    %r3,%r3,sb_helper3@higher\n"
"   rldicr %r3,%r3,32,31\n"
"   oris   %r3,%r3,sb_helper3@h\n"
"   ori    %r3,%r3,sb_helper3@l\n"
"   ld     %r3,0(%r3)\n"            // load xer word to r3
"   mtxer  %r3\n"                   // move r3 to XER

// GPR's
"   ld %r0,    0(%r31)\n"
"   ld %r1,    8(%r31)\n"     // switch stacks (r1 = SP)
"   ld %r2,   16(%r31)\n"
"   ld %r3,   24(%r31)\n"
"   ld %r4,   32(%r31)\n"
"   ld %r5,   40(%r31)\n"
"   ld %r6,   48(%r31)\n"
"   ld %r7,   56(%r31)\n"
"   ld %r8,   64(%r31)\n"
"   ld %r9,   72(%r31)\n"
"   ld %r10,  80(%r31)\n"
"   ld %r11,  88(%r31)\n"
"   ld %r12,  96(%r31)\n"
"   ld %r13, 104(%r31)\n"
"   ld %r14, 112(%r31)\n"
"   ld %r15, 120(%r31)\n"
"   ld %r16, 128(%r31)\n"
"   ld %r17, 136(%r31)\n"
"   ld %r18, 144(%r31)\n"
"   ld %r19, 152(%r31)\n"
"   ld %r20, 160(%r31)\n"
"   ld %r21, 168(%r31)\n"
"   ld %r22, 176(%r31)\n"
"   ld %r23, 184(%r31)\n"
"   ld %r24, 192(%r31)\n"
"   ld %r25, 200(%r31)\n"
"   ld %r26, 208(%r31)\n"
"   ld %r27, 216(%r31)\n"
"   ld %r28, 224(%r31)\n"
"   ld %r29, 232(%r31)\n"
"   ld %r30, 240(%r31)\n"
"   ld %r31, 248(%r31)\n"
"nop_start_point:\n"
"   nop\n"
"   nop\n"
"   nop\n"
"   nop\n"
"   nop\n"
"nop_end_point:\n"
);
#endif

extern void switchback_asm_undotted;
extern void nop_start_point;
extern void nop_end_point;
void switchback ( void )
{
   Int i;
   /* blargh.  Copy the entire switchback_asm procedure into new
      memory on which can can set both write and execute permissions,
      so we can poke around with it and then run the results. */

#if defined(__powerpc64__) // ppc32
   UChar* sa_start     = (UChar*)&switchback_asm_undotted;
#else
   UChar* sa_start     = (UChar*)&switchback_asm;
#endif
   UChar* sa_nop_start = (UChar*)&nop_start_point;
   UChar* sa_end       = (UChar*)&nop_end_point;

#if 0
   printf("sa_start     %p\n", sa_start );
   printf("sa_nop_start %p\n", sa_nop_start);
   printf("sa_end       %p\n", sa_end);
#endif
   Int nbytes       = sa_end - sa_start;
   Int off_nopstart = sa_nop_start - sa_start;
   if (0)
      printf("nbytes = %d, nopstart = %d\n", nbytes, off_nopstart);

   /* copy it into mallocville */
   UChar* copy = mymalloc(nbytes);
   assert(copy);
   for (i = 0; i < nbytes; i++)
      copy[i] = sa_start[i];

   UInt* p = (UInt*)(&copy[off_nopstart]);

#if !defined(__powerpc64__) // ppc32
   Addr32 addr_of_nop = (Addr32)p;
   Addr32 where_to_go = gst.guest_CIA;
   Int    diff = ((Int)where_to_go) - ((Int)addr_of_nop);

#if 0
   printf("addr of first nop = 0x%x\n", addr_of_nop);
   printf("where to go       = 0x%x\n", where_to_go);
   printf("diff = 0x%x\n", diff);
#endif

#else // ppc64
   Addr64 addr_of_nop = (Addr64)p;
   Addr64 where_to_go = gst.guest_CIA;
   Long   diff = ((Long)where_to_go) - ((Long)addr_of_nop);

#if 0
   printf("addr of first nop = 0x%llx\n", addr_of_nop);
   printf("where to go       = 0x%llx\n", where_to_go);
   printf("diff = 0x%llx\n", diff);
#endif
#endif

   if (diff < -0x2000000 || diff >= 0x2000000) {
     // we're hosed.  Give up
     printf("hosed -- offset too large\n");
     assert(0);
   }

   sb_helper1 = (HWord)&gst;
#if !defined(__powerpc64__) // ppc32
   sb_helper2 = LibVEX_GuestPPC32_get_CR(&gst);
   sb_helper3 = LibVEX_GuestPPC32_get_XER(&gst);
#else // ppc64
   sb_helper2 = LibVEX_GuestPPC64_get_CR(&gst);
   sb_helper3 = LibVEX_GuestPPC64_get_XER(&gst);
#endif

   /* stay sane ... */
   assert(p[0] == 24<<26); /* nop */

   /* branch to diff */
   p[0] = ((18<<26) | (((diff >> 2) & 0xFFFFFF) << 2) | (0<<1) | (0<<0));

   invalidate_icache( copy, nbytes );

#if defined(__powerpc64__)
   //printf("jumping to %p\n", copy);
   { ULong faketoc[3];
     void* v;
     faketoc[0] = (ULong)copy;
     v = &faketoc[0];
     ( (void(*)(void)) v )();
   }
#else
   ( (void(*)(void))copy )();
#endif
}

#else
#   error "Unknown arch (switchback)"
#endif

/* -------------------- */
static HWord f, gp, res;
extern void run_translation_asm(void);

#if defined(__i386__)
asm(
"run_translation_asm:\n"
"   pushal\n"
"   movl gp, %ebp\n"
"   movl f, %eax\n"
"   call *%eax\n"
"   movl %eax, res\n"
"   popal\n"
"   ret\n"
);

#elif defined(__x86_64__)
asm(
"run_translation_asm:\n"

"   pushq %rax\n"
"   pushq %rbx\n"
"   pushq %rcx\n"
"   pushq %rdx\n"
"   pushq %rbp\n"
"   pushq %rsi\n"
"   pushq %rdi\n"
"   pushq %r8\n"
"   pushq %r9\n"
"   pushq %r10\n"
"   pushq %r11\n"
"   pushq %r12\n"
"   pushq %r13\n"
"   pushq %r14\n"
"   pushq %r15\n"

"   movq gp, %rbp\n"
"   movq f, %rax\n"
"   call *%rax\n"
"   movq %rax, res\n"

"   popq  %r15\n"
"   popq  %r14\n"
"   popq  %r13\n"
"   popq  %r12\n"
"   popq  %r11\n"
"   popq  %r10\n"
"   popq  %r9\n"
"   popq  %r8\n"
"   popq  %rdi\n"
"   popq  %rsi\n"
"   popq  %rbp\n"
"   popq  %rdx\n"
"   popq  %rcx\n"
"   popq  %rbx\n"
"   popq  %rax\n"

"   ret\n"
);

#elif defined(__powerpc__)

#if !defined(__powerpc64__) // ppc32
asm(
"run_translation_asm:\n"

// create new stack:
// save old sp at first word & update sp
"   stwu 1,-256(1)\n"

// save LR
"   mflr %r0\n"
"   stw  %r0,260(%r1)\n"

// leave hole @ 4(%r1) for a callee to save it's LR
// no params
// no need to save non-volatile CR fields

// store registers to stack: just the callee-saved regs
"   stw %r13,  8(%r1)\n"
"   stw %r14, 12(%r1)\n"
"   stw %r15, 16(%r1)\n"
"   stw %r16, 20(%r1)\n"
"   stw %r17, 24(%r1)\n"
"   stw %r18, 28(%r1)\n"
"   stw %r19, 32(%r1)\n"
"   stw %r20, 36(%r1)\n"
"   stw %r21, 40(%r1)\n"
"   stw %r22, 44(%r1)\n"
"   stw %r23, 48(%r1)\n"
"   stw %r24, 52(%r1)\n"
"   stw %r25, 56(%r1)\n"
"   stw %r26, 60(%r1)\n"
"   stw %r27, 64(%r1)\n"
"   stw %r28, 68(%r1)\n"
"   stw %r29, 72(%r1)\n"
"   stw %r30, 76(%r1)\n"
"   stw %r31, 80(%r1)\n"

// r31 (guest state ptr) := global var "gp"
"   lis %r31,gp@ha\n"
"   lwz %r31,gp@l(%r31)\n"

// call translation address in global var "f"
"   lis %r4,f@ha\n"
"   lwz %r4,f@l(%r4)\n"
"   mtctr %r4\n"
"   bctrl\n"

// save return value (in r3) into global var "res"
"   lis %r5,res@ha\n"
"   stw %r3,res@l(%r5)\n"

// save possibly modified guest state ptr (r31) in "gp"
"   lis %r5,gp@ha\n"
"   stw %r31,gp@l(%r5)\n"

// reload registers from stack
"   lwz %r13,  8(%r1)\n"
"   lwz %r14, 12(%r1)\n"
"   lwz %r15, 16(%r1)\n"
"   lwz %r16, 20(%r1)\n"
"   lwz %r17, 24(%r1)\n"
"   lwz %r18, 28(%r1)\n"
"   lwz %r19, 32(%r1)\n"
"   lwz %r20, 36(%r1)\n"
"   lwz %r21, 40(%r1)\n"
"   lwz %r22, 44(%r1)\n"
"   lwz %r23, 48(%r1)\n"
"   lwz %r24, 52(%r1)\n"
"   lwz %r25, 56(%r1)\n"
"   lwz %r26, 60(%r1)\n"
"   lwz %r27, 64(%r1)\n"
"   lwz %r28, 68(%r1)\n"
"   lwz %r29, 72(%r1)\n"
"   lwz %r30, 76(%r1)\n"
"   lwz %r31, 80(%r1)\n"

// restore LR
"   lwz  %r0,260(%r1)\n"
"   mtlr %r0\n"

// restore previous stack pointer
"   addi %r1,%r1,256\n"

// return
"   blr"
);

#else // ppc64

asm(
".text\n"
"   .global run_translation_asm\n"
"   .section \".opd\",\"aw\"\n"
"   .align 3\n"
"run_translation_asm:\n"
"   .quad .run_translation_asm,.TOC.@tocbase,0\n"
"   .previous\n"
"   .type .run_translation_asm,@function\n"
"   .global  .run_translation_asm\n"
".run_translation_asm:\n"

// save LR,CTR
"   mflr  %r0\n"
"   std   %r0,16(%r1)\n"
"   mfctr %r0\n"
"   std   %r0,8(%r1)\n"

// create new stack:
// save old sp at first word & update sp
"   stdu 1,-256(1)\n"

// leave hole @ 4(%r1) for a callee to save it's LR
// no params
// no need to save non-volatile CR fields

// store registers to stack: just the callee-saved regs
"   std %r13,  48(%r1)\n"
"   std %r14,  56(%r1)\n"
"   std %r15,  64(%r1)\n"
"   std %r16,  72(%r1)\n"
"   std %r17,  80(%r1)\n"
"   std %r18,  88(%r1)\n"
"   std %r19,  96(%r1)\n"
"   std %r20, 104(%r1)\n"
"   std %r21, 112(%r1)\n"
"   std %r22, 120(%r1)\n"
"   std %r23, 128(%r1)\n"
"   std %r24, 136(%r1)\n"
"   std %r25, 144(%r1)\n"
"   std %r26, 152(%r1)\n"
"   std %r27, 160(%r1)\n"
"   std %r28, 168(%r1)\n"
"   std %r29, 176(%r1)\n"
"   std %r30, 184(%r1)\n"
"   std %r31, 192(%r1)\n"

// r31 (guest state ptr) := global var "gp"
"   lis    %r31,gp@highest\n"
"   ori    %r31,%r31,gp@higher\n"
"   rldicr %r31,%r31,32,31\n"
"   oris   %r31,%r31,gp@h\n"
"   ori    %r31,%r31,gp@l\n"
"   ld     %r31,0(%r31)\n"

// call translation address in global var "f"
"   lis    %r4,f@highest\n"
"   ori    %r4,%r4,f@higher\n"
"   rldicr %r4,%r4,32,31\n"
"   oris   %r4,%r4,f@h\n"
"   ori    %r4,%r4,f@l\n"
"   ld     %r4,0(%r4)\n"
"   mtctr  %r4\n"
"   bctrl\n"

// save return value (in r3) into global var "res"
"   lis    %r5,res@highest\n"
"   ori    %r5,%r5,res@higher\n"
"   rldicr %r5,%r5,32,31\n"
"   oris   %r5,%r5,res@h\n"
"   ori    %r5,%r5,res@l\n"
"   std    %r3,0(%r5)\n"

// save possibly modified guest state ptr (r31) in "gp"
"   lis    %r5,gp@highest\n"
"   ori    %r5,%r5,gp@higher\n"
"   rldicr %r5,%r5,32,31\n"
"   oris   %r5,%r5,gp@h\n"
"   ori    %r5,%r5,gp@l\n"
"   std    %r31,0(%r5)\n"

// reload registers from stack
"   ld %r13,  48(%r1)\n"
"   ld %r14,  56(%r1)\n"
"   ld %r15,  64(%r1)\n"
"   ld %r16,  72(%r1)\n"
"   ld %r17,  80(%r1)\n"
"   ld %r18,  88(%r1)\n"
"   ld %r19,  96(%r1)\n"
"   ld %r20, 104(%r1)\n"
"   ld %r21, 112(%r1)\n"
"   ld %r22, 120(%r1)\n"
"   ld %r23, 128(%r1)\n"
"   ld %r24, 136(%r1)\n"
"   ld %r25, 144(%r1)\n"
"   ld %r26, 152(%r1)\n"
"   ld %r27, 160(%r1)\n"
"   ld %r28, 168(%r1)\n"
"   ld %r29, 176(%r1)\n"
"   ld %r30, 184(%r1)\n"
"   ld %r31, 192(%r1)\n"

// restore previous stack pointer
"   addi %r1,%r1,256\n"

// restore LR,CTR
"   ld    %r0,16(%r1)\n"
"   mtlr  %r0\n"
"   ld    %r0,8(%r1)\n"
"   mtctr %r0\n"

// return
"   blr"
);
#endif

#else

#   error "Unknown arch"
#endif

/* Run a translation at host address 'translation'.  Return
   True if Vex asked for an translation cache flush as a result.
*/
Bool run_translation ( HWord translation )
{
   if (0 && DEBUG_TRACE_FLAGS) {
      printf(" run translation %p\n", (void*)translation );
      printf(" simulated bb: %llu\n", n_bbs_done);
   }
   f = translation;
   gp = (HWord)&gst;
   run_translation_asm();
   gst.GuestPC = res;
   n_bbs_done ++;
   return gp==VEX_TRC_JMP_TINVAL;
}

HWord find_translation ( Addr64 guest_addr )
{
   Int i;
   HWord __res;
   if (0)
      printf("find translation %p ... ", ULong_to_Ptr(guest_addr));
   for (i = 0; i < trans_table_used; i++)
     if (trans_table[i].base[0] == guest_addr)
        break;
   if (i == trans_table_used) {
      if (0) printf("none\n");
      return 0; /* not found */
   }

   /* Move this translation one step towards the front, so finding it
      next time round is just that little bit cheaper. */
   if (i > 2) {
      VexGuestExtents tmpE = trans_table[i-1];
      ULong*          tmpP = trans_tableP[i-1];
      trans_table[i-1]  = trans_table[i];
      trans_tableP[i-1] = trans_tableP[i];
      trans_table[i] = tmpE;
      trans_tableP[i] = tmpP;
      i--;
   }

   __res = (HWord)trans_tableP[i];
   if (0) printf("%p\n", (void*)__res);
   return __res;
}

#define N_TRANSBUF 5000
static UChar transbuf[N_TRANSBUF];
void make_translation ( Addr64 guest_addr, Bool verbose )
{
   VexTranslateArgs   vta;
   VexTranslateResult tres;
   VexArchInfo vex_archinfo;
   Int trans_used, i, ws_needed;

   if (trans_table_used >= N_TRANS_TABLE
       || trans_cache_used >= N_TRANS_CACHE-1000) {
      /* If things are looking to full, just dump
         all the translations. */
      trans_cache_used = 0;
      trans_table_used = 0;
   }

   assert(trans_table_used < N_TRANS_TABLE);
   if (0)
      printf("make translation %p\n", ULong_to_Ptr(guest_addr));

   LibVEX_default_VexArchInfo(&vex_archinfo);
   vex_archinfo.subarch = VexSubArch;
   vex_archinfo.ppc_cache_line_szB = CacheLineSize;

   /* */
   vta.arch_guest       = VexArch;
   vta.archinfo_guest   = vex_archinfo;
   vta.arch_host        = VexArch;
   vta.archinfo_host    = vex_archinfo;
   vta.guest_bytes      = (UChar*)ULong_to_Ptr(guest_addr);
   vta.guest_bytes_addr = (Addr64)guest_addr;
   vta.guest_bytes_addr_noredir = (Addr64)guest_addr;
   vta.chase_into_ok    = chase_into_ok;
//   vta.guest_extents    = &vge;
   vta.guest_extents    = &trans_table[trans_table_used];
   vta.host_bytes       = transbuf;
   vta.host_bytes_size  = N_TRANSBUF;
   vta.host_bytes_used  = &trans_used;
   vta.instrument1      = NULL;
   vta.instrument2      = NULL;
   vta.do_self_check    = False;
   vta.traceflags       = verbose ? TEST_FLAGS : DEBUG_TRACE_FLAGS;
   vta.dispatch         = NULL;
   vta.addProfInc       = False;

   tres = LibVEX_Translate ( &vta );

   assert(tres == VexTransOK);
   ws_needed = (trans_used+7) / 8;
   assert(ws_needed > 0);
   assert(trans_cache_used + ws_needed < N_TRANS_CACHE);
   n_translations_made++;

   for (i = 0; i < trans_used; i++) {
      HChar* dst = ((HChar*)(&trans_cache[trans_cache_used])) + i;
      HChar* src = (HChar*)(&transbuf[i]);
      *dst = *src;
   }

#if defined(__powerpc__)
   invalidate_icache( &trans_cache[trans_cache_used], trans_used );
#endif

   trans_tableP[trans_table_used] = &trans_cache[trans_cache_used];
   trans_table_used++;
   trans_cache_used += ws_needed;
}


static Bool overlap ( Addr64 start, UInt len, VexGuestExtents* vge )
{
   Int i;
   for (i = 0; i < vge->n_used; i++) {
     if (vge->base[i]+vge->len[i] <= start
         || vge->base[i] >= start+len) {
       /* ok */
     } else {
        return True;
     }
   }
   return False; /* no overlap */
}

static void dump_translations ( Addr64 start, UInt len )
{
   Int i, j;
   j = 0;
   for (i = 0; i < trans_table_used; i++) {
      if (overlap(start, len, &trans_table[i])) {
         /* do nothing */
      } else {
         assert(j <= i);
         trans_table[j] = trans_table[i];
         trans_tableP[j] = trans_tableP[i];
	 j++;
      }
   }
   assert(j >= 0 && j <= trans_table_used);
   if (0) printf("dumped %d translations\n", trans_table_used - j);
   trans_table_used = j;
}


static ULong  stopAfter = 0;
static UChar* entryP    = NULL;


__attribute__ ((noreturn))
static
void failure_exit ( void )
{
   fprintf(stdout, "VEX did failure_exit.  Bye.\n");
   fprintf(stdout, "bb counter = %llu\n\n", n_bbs_done);
   exit(1);
}

static
void log_bytes ( HChar* bytes, Int nbytes )
{
   fwrite ( bytes, 1, nbytes, stdout );
   fflush ( stdout );
}


/* run simulated code forever (it will exit by calling
   serviceFn(0)). */
static void run_simulator ( void )
{
   static Addr64 last_guest = 0;
   Addr64 next_guest;
   HWord next_host;
   Bool need_inval;
   while (1) {
      next_guest = gst.GuestPC;

      if (0)
         printf("\nnext_guest: 0x%x\n", (UInt)next_guest);

#if defined(__powerpc64__)
      if (next_guest == Ptr_to_ULong( (void*)(*(ULong*)(&serviceFn)) )) {
#else
      if (next_guest == Ptr_to_ULong(&serviceFn)) {
#endif
         /* "do" the function call to serviceFn */
#        if defined(__i386__)
         {
            HWord esp = gst.guest_ESP;
            gst.guest_EIP = *(UInt*)(esp+0);
            gst.guest_EAX = serviceFn( *(UInt*)(esp+4), *(UInt*)(esp+8) );
            gst.guest_ESP = esp+4;
            next_guest = gst.guest_EIP;
         }
#        elif defined(__x86_64__)
         {
            HWord esp = gst.guest_RSP;
            gst.guest_RIP = *(UInt*)(esp+0);
            gst.guest_RAX = serviceFn( gst.guest_RDI, gst.guest_RSI );
            gst.guest_RSP = esp+8;
            next_guest = gst.guest_RIP;
         }
#        elif defined(__powerpc__)
         {
            gst.guest_GPR3 = serviceFn( gst.guest_GPR3, gst.guest_GPR4 );
            gst.guest_CIA  = gst.guest_LR;
            next_guest     = gst.guest_CIA;
         }
#        else
#        error "Unknown arch"
#        endif
      }

      next_host = find_translation(next_guest);
      if (next_host == 0) {
         make_translation(next_guest,False);
         next_host = find_translation(next_guest);
         assert(next_host != 0);
      }

      // Switchback
      if (n_bbs_done == stopAfter) {
         printf("---begin SWITCHBACK at bb:%llu---\n", n_bbs_done);
#if 1
         if (last_guest) {
            printf("\n*** Last run translation (bb:%llu):\n", n_bbs_done-1);
            make_translation(last_guest,True);
         }
#endif
#if 0
         if (next_guest) {
            printf("\n*** Current translation (bb:%llu):\n", n_bbs_done);
            make_translation(next_guest,True);
         }
#endif
         printf("---  end SWITCHBACK at bb:%llu ---\n", n_bbs_done);
         switchback();
         assert(0); /*NOTREACHED*/
      }

      last_guest = next_guest;
      need_inval = run_translation(next_host);
      if (need_inval) {
#if defined(__powerpc__)
         dump_translations( (Addr64)gst.guest_TISTART, gst.guest_TILEN );
	 if (0) printf("dump translations done\n");
#endif
      }
   }
}


static void usage ( void )
{
   printf("usage: switchback #bbs\n");
   printf("   - begins switchback for basic block #bbs\n");
   printf("   - use -1 for largest possible run without switchback\n\n");
   exit(1);
}

#if defined(__powerpc__)

#if !defined(__powerpc64__) // ppc32
UInt saved_R2;
asm(
"get_R2:\n"
"   lis  %r10,saved_R2@ha\n"
"   stw  %r2,saved_R2@l(%r10)\n"
"   blr\n"
);
#else // ppc64
ULong saved_R2;
ULong saved_R13;
asm(
".text\n"
"   .global get_R2\n"
"   .section \".opd\",\"aw\"\n"
"   .align 3\n"
"get_R2:\n"
"   .quad .get_R2,.TOC.@tocbase,0\n"
"   .previous\n"
"   .type .get_R2,@function\n"
"   .global  .get_R2\n"
".get_R2:\n"
"   lis    %r10,saved_R2@highest\n"
"   ori    %r10,%r10,saved_R2@higher\n"
"   rldicr %r10,%r10,32,31\n"
"   oris   %r10,%r10,saved_R2@h\n"
"   ori    %r10,%r10,saved_R2@l\n"
"   std    %r2,0(%r10)\n"
"   blr\n"
);
asm(
".text\n"
"   .global get_R13\n"
"   .section \".opd\",\"aw\"\n"
"   .align 3\n"
"get_R13:\n"
"   .quad .get_R13,.TOC.@tocbase,0\n"
"   .previous\n"
"   .type .get_R13,@function\n"
"   .global  .get_R13\n"
".get_R13:\n"
"   lis    %r10,saved_R13@highest\n"
"   ori    %r10,%r10,saved_R13@higher\n"
"   rldicr %r10,%r10,32,31\n"
"   oris   %r10,%r10,saved_R13@h\n"
"   ori    %r10,%r10,saved_R13@l\n"
"   std    %r13,0(%r10)\n"
"   blr\n"
);
#endif
extern void get_R2 ( void );
extern void get_R13 ( void );
#endif

int main ( Int argc, HChar** argv )
{
   if (argc != 2)
      usage();

   stopAfter = (ULong)atoll(argv[1]);

   extern void entry ( void*(*service)(int,int) );
   entryP = (UChar*)&entry;

   if (!entryP) {
      printf("switchback: can't find entry point\n");
      exit(1);
   }

   LibVEX_default_VexControl(&vcon);
   vcon.guest_max_insns=50;
   vcon.guest_chase_thresh=0;
   vcon.iropt_level=2;

   LibVEX_Init( failure_exit, log_bytes, 1, False, &vcon );
   LibVEX_Guest_initialise(&gst);

   /* set up as if a call to the entry point passing serviceFn as 
      the one and only parameter */
#  if defined(__i386__)
   gst.guest_EIP = (UInt)entryP;
   gst.guest_ESP = (UInt)&gstack[25000];
   *(UInt*)(gst.guest_ESP+4) = (UInt)serviceFn;
   *(UInt*)(gst.guest_ESP+0) = 0x12345678;
#  elif defined(__x86_64__)
   gst.guest_RIP = (ULong)entryP;
   gst.guest_RSP = (ULong)&gstack[25000];
   gst.guest_RDI = (ULong)serviceFn;
   *(ULong*)(gst.guest_RSP+0) = 0x12345678AABBCCDDULL;
#  elif defined(__powerpc__)
   get_R2();

#if !defined(__powerpc64__) // ppc32
   gst.guest_CIA   = (UInt)entryP;
   gst.guest_GPR1  = (UInt)&gstack[25000]; /* stack pointer */
   gst.guest_GPR3  = (UInt)serviceFn; /* param to entry */
   gst.guest_GPR2  = saved_R2;
   gst.guest_LR    = 0x12345678; /* bogus return address */
#else // ppc64
   get_R13();
   gst.guest_CIA   = * (ULong*)entryP;
   gst.guest_GPR1  = (ULong)&gstack[25000]; /* stack pointer */
   gst.guest_GPR3  = (ULong)serviceFn;      /* param to entry */
   gst.guest_GPR2  = saved_R2;
   gst.guest_GPR13 = saved_R13;
   gst.guest_LR    = 0x1234567812345678ULL; /* bogus return address */
//   printf("setting CIA to %p\n", (void*)gst.guest_CIA);
#endif

#  else
#  error "Unknown arch"
#  endif

   printf("\n---START---\n");

#if 1
   run_simulator();
#else
   ( (void(*)(HWord(*)(HWord,HWord))) entryP ) (serviceFn);
#endif


   return 0;
}
