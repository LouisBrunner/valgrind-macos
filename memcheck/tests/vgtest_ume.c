#define ELFSZ  32

// This file is a unit self-test for ume.c, ume_entry.c, jmp_with_stack.c

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <elf.h>
#include "../../coregrind/ume.h"

#define STKSZ   (64*1024)

//-------------------------------------------------------------------
// Test foreach_map()
//-------------------------------------------------------------------

static int x;

static int f(char *start, char *end, const char *perm, off_t off,
             int maj, int min, int ino, void* dummy) {
   // Just do some nonsense action with each of the values so that Memcheck
   // checks that they are valid.
   x = ( start == 0 ? 0 : 1 );
   x = ( end   == 0 ? 0 : 1 );
   x = ( perm  == 0 ? 0 : 1 );
   x = ( off   == 0 ? 0 : 1 );
   x = ( maj   == 0 ? 0 : 1 );
   x = ( min   == 0 ? 0 : 1 );
   x = ( ino   == 0 ? 0 : 1 );
   x = ( dummy == 0 ? 0 : 1 );

   return /*True*/1;
}  

static void test__foreach_map(void)
{
   fprintf(stderr, "Calling foreach_map()\n");
   foreach_map(f, /*dummy*/NULL);
}

//-------------------------------------------------------------------
// Test find_auxv()
//-------------------------------------------------------------------

static void test__find_auxv(void)
{
   struct ume_auxv *auxv;

   assert(ume_exec_esp != NULL);
   
   fprintf(stderr, "Calling find_auxv()\n");
   auxv = find_auxv((int*)ume_exec_esp);

   // Check the auxv value looks sane
   assert((void*)auxv > (void*)ume_exec_esp);
   assert((unsigned int)auxv - (unsigned int)ume_exec_esp < 0x10000);

   // Scan the auxv, check it looks sane
   for (; auxv->a_type != AT_NULL; auxv++) {
      switch(auxv->a_type) {
      // Check a_type value looks like a plausible small constant
      case 1 ... 64:
         break;
   
      default:
         assert(0);
      }
   }
}

//-------------------------------------------------------------------
// Test do_exec()
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
   info.map_base = 0x51000000;
   
   fprintf(stderr, "Calling do_exec(\"hello\")\n");
   err = do_exec("hello", &info);
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

   jmp_with_stack(info.init_eip, (addr_t)esp);

   assert(0);  // UNREACHABLE
}

int main(void)
{
   test__foreach_map();
   test__find_auxv();
   test__do_exec();
   
   return 0;
}
