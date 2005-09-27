
// This file is a unit self-test for ume.c, jmp_with_stack.c

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <elf.h>
#include "../../include/pub_tool_basics.h"
#include "../../coregrind/pub_core_ume.h"

#define STKSZ   (64*1024)

static void* init_sp;

//-------------------------------------------------------------------
// Test VG_(foreach_map)()
//-------------------------------------------------------------------

static int x[8];

static int f(char *start, char *end, const char *perm, off_t off,
             int maj, int min, int ino, void* dummy) {
   // Just do some nonsense action with each of the values so that Memcheck
   // checks that they are valid.
   x[0] = ( start == 0 ? 0 : 1 );
   x[1] = ( end   == 0 ? 0 : 1 );
   x[2] = ( perm  == 0 ? 0 : 1 );
   x[3] = ( off   == 0 ? 0 : 1 );
   x[4] = ( maj   == 0 ? 0 : 1 );
   x[5] = ( min   == 0 ? 0 : 1 );
   x[6] = ( ino   == 0 ? 0 : 1 );
   x[7] = ( dummy == 0 ? 0 : 1 );

   return /*True*/1 + x[0] + x[1] + x[2] + x[3] + x[4] + x[5] + x[6] + x[7];
}  

static void test__foreach_map(void)
{
   fprintf(stderr, "Calling VG_(foreach_map)()\n");
   VG_(foreach_map)(f, /*dummy*/NULL);
}

//-------------------------------------------------------------------
// Test VG_(find_auxv)()
//-------------------------------------------------------------------

static void test__find_auxv(void)
{
   struct ume_auxv *auxv;

   assert(init_sp != NULL);
   
   fprintf(stderr, "Calling VG_(find_auxv)()\n");
   auxv = VG_(find_auxv)((UWord*)init_sp);

   // Check the auxv value looks sane
   assert((void*)auxv > (void*)init_sp);
   assert((unsigned int)auxv - (unsigned int)init_sp < 0x10000);

   // Scan the auxv, check it looks sane
   for (; auxv->a_type != AT_NULL; auxv++) {
      switch(auxv->a_type) {
      // Check a_type value looks like a plausible small constant
      case 1 ... 64:
         break;
   
      default:
         fprintf(stderr, "auxv->a_type = %lld\n", (Long)auxv->a_type);
         assert(0);
      }
   }
}

//-------------------------------------------------------------------
// Test VG_(do_exec)()
//-------------------------------------------------------------------

static void push_auxv(unsigned char **espp, int type, void *val)
{
        struct ume_auxv *auxp = (struct ume_auxv *)*espp;
        auxp--;
        auxp->a_type = type;
        auxp->u.a_ptr = val;
        *espp = (unsigned char *)auxp;
}

static void push(unsigned char **espp, void *v)
{
        void **vp = *(void ***)espp;
        *--vp = v;
        *espp = (unsigned char *)vp;
}

static void test__do_exec(void)
{
   struct exeinfo info;
   int err;
   unsigned char* newstack;
   unsigned char *esp;

   info.argv     = NULL;
   info.exe_base = 0x50000000;
   info.exe_end  = 0x50ffffff;
   
   fprintf(stderr, "Calling VG_(do_exec)(\"hello\")\n");
   err = VG_(do_exec)("hello", &info);
   assert(0 == err);

//   printf("info.exe_base=%p exe_end=%p\n", 
//          (void*)info.exe_base, (void*)info.exe_end);

   newstack = malloc(STKSZ);
   assert(0 != newstack);

   esp = newstack+STKSZ;

   /* 
      Set the new executable's stack up like the kernel would after
      exec.

      These are being pushed onto the stack, towards decreasing
      addresses.
    */
   push_auxv(&esp, AT_NULL, 0);                         // auxv terminator
   push_auxv(&esp, AT_ENTRY, (void *)info.entry);       // entrypoint of the main executable */
   push_auxv(&esp, AT_BASE, (void *)info.interp_base);  // base address of ld-linux.so
   push_auxv(&esp, AT_PHDR, (void *)info.phdr);         // where the ELF PHDRs are mapped
   push_auxv(&esp, AT_PHNUM, (void*)info.phnum);        // and how many of them

   push(&esp, 0);               /* no env */
   push(&esp, 0);               /* no argv */
   push(&esp, 0);               /* argc=0 */

//   fprintf(stderr, "ume_go: %p %p\n", (void*)info.init_eip, (void*)esp);

   VG_(jump_and_switch_stacks)((Addr)esp, info.init_eip);

   assert(0);  // UNREACHABLE
}

int main(int argc, char** argv)
{
   init_sp = argv - 1;
   
   test__foreach_map();
   test__find_auxv();
   test__do_exec();
   
   return 0;
}
