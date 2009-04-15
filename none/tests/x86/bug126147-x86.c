#include "tests/asm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* These fns taken from dietlibc-0.30 (is GPL v2'd) */

/*
  Copyright (C) 2002 Thomas M. Ogrisegg

  This is free software. You can redistribute and
  modify it under the terms of the GNU General Public
  Public License.

  strncpy.S
    i386 assembler implementation of strncpy(3)
*/
extern char *mystrncpy(char *dest, const char *src, size_t n);
asm(
".text\n"
"\n"
VG_SYM(mystrncpy) ":\n"
"	pushl %esi\n"
"	pushl %edi\n"
"	movl %esp, %ecx\n"
"	movl  0x0c(%ecx), %edi\n"
"	movl  0x10(%ecx), %esi\n"
"	movl  0x14(%ecx), %ecx\n"
"\n"
"	movl %edi, %edx\n"
"	cld\n"
"0:\n"
"	dec %ecx\n"
"	js 1f\n"
"	lodsb\n"
"	stosb\n"
"	or %al, %al\n"
"	jnz 0b\n"
"	repnz stosb\n"
"1:\n"
"	movl %edx, %eax\n"
"	popl %edi\n"
"	popl %esi\n"
"	ret\n"
"\n"
);


/*
   Copyright (C) 2002 Thomas M. Ogrisegg

   __ltostr.S -- convert an integer into a string

 %eax = dividend
 %ebx = divisor
 %ecx = size of output-buffer
 %edi = output-buffer
 %ebp = if uppercase is set, then %ebp is 'A'-10 else %ebp is 'a'-10

*/
extern int __ltostr(char *s, unsigned int size, unsigned long i, 
                             unsigned int base, int UpCase);
asm(
".text\n"
VG_SYM(__ltostr) ":\n"
"        pushl %esi\n"
"        pushl %edi              # destination\n"
"        pushl %ebp\n"
"        pushl %ebx\n"
"        movl %esp, %eax\n"
"        movl 0x14(%eax), %edi\n"
"        movl 0x18(%eax), %ecx   # size\n"
"        movl 0x20(%eax), %ebx   # divisor\n"
"        movl 0x1c(%eax), %eax   # dividend\n"
"        decl %ecx\n"
"        movl %ecx, %esi\n"
"        movl $55, %ebp          # 55 == char(A)-10\n"
"        xorl %edx, %edx         # must be 0 -- used by idiv\n"
"        cmpl $0x0, 36(%esp)     # check for uppercase\n"
"        jnz 0f\n"
"        addl $0x20, %ebp        # set lowercase\n"
"0:\n"
"        idiv %ebx, %eax\n"
"        cmpb $0x9, %dl\n"
"        jg 1f\n"
"        addb $48, %dl           # 48 == '0'\n"
"        jmp 2f\n"
"1:\n"
"        addl %ebp, %edx\n"
"2:\n"
"        movb %dl, (%edi, %ecx)\n"
"        xorl %edx, %edx\n"
"        decl %ecx\n"
"        jz 3f\n"
"        orl %eax, %eax\n"
"        jnz 0b\n"
"3:\n"
"        cld\n"
"        movl %esi, %ebx\n"
"        leal 1(%edi, %ecx), %esi\n"
"        subl %ebx, %ecx\n"
"        negl %ecx\n"
"        movl %ecx, %eax\n"
"        repnz movsb\n"
"        movb $0x0, (%edi)\n"
"        popl %ebx\n"
"        popl %ebp\n"
"        popl %edi\n"
"        popl %esi\n"
"        ret\n"
);

#define STREQ(a, b)     (strcmp((a), (b)) == 0)

const char *it = "<UNSET>";     /* Routine name for message routines. */
size_t errors = 0;

/* Complain if condition is not true.  */
static void
check (int thing, int number)
{
  if (!thing)
    {
      printf("%s flunked test %d\n", it, number);
      ++errors;
    }
}

/* Complain if first two args don't strcmp as equal.  */
static void
equal (const char *a, const char *b, int number)
{
  check(a != NULL && b != NULL && STREQ (a, b), number);
}

char one[50];
char two[50];
char *cp;

static void
test_strncpy (void)
{
  /* Testing is a bit different because of odd semantics.  */
  it = "strncpy";
  check (mystrncpy (one, "abc", 4) == one, 1);    /* Returned value. */
  equal (one, "abc", 2);                        /* Did the copy go right? */

  (void) strcpy (one, "abcdefgh");
  (void) mystrncpy (one, "xyz", 2);
  equal (one, "xycdefgh", 3);                   /* Copy cut by count. */

  (void) strcpy (one, "abcdefgh");
  (void) mystrncpy (one, "xyz", 3);               /* Copy cut just before NUL. */
  equal (one, "xyzdefgh", 4);

  (void) strcpy (one, "abcdefgh");
  (void) mystrncpy (one, "xyz", 4);               /* Copy just includes NUL. */
  equal (one, "xyz", 5);
  equal (one+4, "efgh", 6);                     /* Wrote too much? */

  (void) strcpy (one, "abcdefgh");
  (void) mystrncpy (one, "xyz", 5);               /* Copy includes padding. */
  equal (one, "xyz", 7);
  equal (one+4, "", 8);
  equal (one+5, "fgh", 9);

  (void) strcpy (one, "abc");
  (void) mystrncpy (one, "xyz", 0);               /* Zero-length copy. */
  equal (one, "abc", 10);

  (void) mystrncpy (one, "", 2);          /* Zero-length source. */
  equal (one, "", 11);
  equal (one+1, "", 12);
  equal (one+2, "c", 13);

  (void) strcpy (one, "hi there");
  (void) mystrncpy (two, one, 9);
  equal (two, "hi there", 14);          /* Just paranoia. */
  equal (one, "hi there", 15);          /* Stomped on source? */
}


int main ( void )
{
  char buf[1024];

  /* test strncpy, hence repnz stosb */
  test_strncpy();
    if (errors == 0)
    {
      printf("No errors.\n");
    }
  else
    {
      printf("%d errors.\n", (int)errors);
    }

    /* test __ltostr, hence repnz stosb */ 
  assert(__ltostr(buf,10,1723,10,0)==4); assert(!strcmp(buf,"1723"));
  assert(__ltostr(buf,3,1723,10,0)==2); assert(!strcmp(buf,"23"));
  assert(__ltostr(buf,2,0x1234,16,0)==1); assert(!strcmp(buf,"4"));
  assert(__ltostr(buf,3,0xFEFE,16,1)==2); assert(!strcmp(buf,"FE"));

 return 0;
}
