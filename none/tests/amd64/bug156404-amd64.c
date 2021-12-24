
/* Check that the main thread's stack, on Linux, is automatically
   extended down to the lowest valid address when a syscall happens.
   Failure to do so was causing this test to fail on Linux amd64. */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <sys/syscall.h>
#include <unistd.h>

#define VG_STRINGIFZ(__str)  #__str
#define VG_STRINGIFY(__str)  VG_STRINGIFZ(__str)

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
#if !defined(__NR_readlink)
# define __NR_readlink 58
#endif

#define __NR_READLINK        VG_STRINGIFY(__NR_readlink)

extern long my_readlink ( const char* path );
asm(
".text\n"
".globl my_readlink\n"
"my_readlink:\n"
"\tsubq    $0x1008,%rsp\n"
"\tmovq    %rdi,%rdi\n"              // path is in rdi
"\tmovq    %rsp,%rsi\n"              // &buf[0] -> rsi
"\tmovl    $0x1000,%edx\n"           // sizeof(buf) in rdx
"\tmovl    $"__NR_READLINK",%eax\n"  // syscall number
"\tsyscall\n"
"\taddq    $0x1008,%rsp\n"
"\tret\n"
".previous\n"
);

#elif defined(VGO_solaris)
#define __NR_READLINKAT      VG_STRINGIFY(SYS_readlinkat)

extern long my_readlink ( const char* path );
asm(
".text\n"
".globl my_readlink\n"
"my_readlink:\n"
"\tsubq    $0x1008,%rsp\n"
"\tmovq    %rdi,%rsi\n"
"\txorq    %rdi,%rdi\n"
"\tmovq    %rsp,%rdx\n"
"\tmovq    $0x1000,%r10\n"
"\tmovl    $"__NR_READLINKAT",%eax\n"
"\tsyscall\n"
"\taddq    $0x1008,%rsp\n"
"\tret\n"
".previous\n"
);

#else
#error "Unknown OS"
#endif

long recurse ( const char* path, long count )
{
   if (count <= 0) {
      return my_readlink(path);
   } else { 
      long r = recurse(path, count-1);
      return r;
   }
}

int main ( void )
{
   long i, r;
   for (i = 0; i < 2000; i++) {
      printf("depth %ld: ", i );
      r = recurse( "/proc/self", i );
      if (r > 1) r = 1; /* to make the output repeatable */
      assert(r >= 1);
      printf("r = %ld\n", r);
   }
   return 0;
}
