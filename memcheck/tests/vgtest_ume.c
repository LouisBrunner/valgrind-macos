#define ELFSZ  32

// This file is a unit self-test for ume.c, ume_entry.c, jmp_with_stack.c

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <elf.h>
#include "../../coregrind/ume.h"

#define STKSZ   (64*1024)

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


int main(void)
{
   struct exeinfo info;
   int err;
   unsigned char* newstack;
   unsigned char *esp;

   info.argv     = NULL;
   info.exe_base = 0x50000000;
   info.exe_end  = 0x50ffffff;
   info.map_base = 0x51000000;
   
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

   return 0;
}
