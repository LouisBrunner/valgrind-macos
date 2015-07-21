/* Tests that Valgrind coredump support for XMM registers works correctly
   by producing a core dump analyzable by mdb.
   Basic register set is tested in coredump_single_thread. */

#include <stdio.h>
#include <sys/types.h>

__attribute__((noinline))
static void inner(void) {
   const char *input =
   "\x12\x34\x56\x78\x9a\xbc\xde\xf0\xfe\xdb\xca\x98\x76\x54\x32\x10"
   "\x23\x45\x67\x89\x09\x87\x65\x43\x21\xfe\xdc\xba\x94\x67\xfe\xca"
   "\xab\xcd\xab\xcd\xce\xde\xce\xde\xfa\xba\xfa\xba\x50\x65\x67\x54"
   "\x03\x05\x06\x08\x1d\x1b\x4b\x15\x25\x27\x21\x20\x37\x3a\x3d\x35"
   "\x9a\xbc\xde\xf0\x76\x54\x32\x10\x12\x34\x56\x78\xfe\xdb\xca\x98"
   "\x94\x67\xfe\xca\x23\x45\x67\x89\x21\xfe\xdc\xba\x09\x87\x65\x43"
   "\x50\x65\x67\x54\xce\xde\xce\xde\xab\xcd\xab\xcd\xfa\xba\xfa\xba"
   "\x37\x3a\x3d\x35\x1d\x1b\x4b\x15\x03\x05\x06\x08\x25\x27\x21\x20"
   "\x24\x15\xb1\x5e\x00\x96\x83\xdd\xdc\x92\x66\x29\xbc\x35\xb1\x8a"
   "\xc6\x72\x50\x4b\xbd\x8e\x9a\x95\xc6\xf7\xd3\x30\xd5\x34\x68\x22"
   "\xc8\xd1\xca\xb6\xf4\x5c\xd1\xc7\x03\xdb\xc8\xb5\x8a\x1a\xf3\xbd"
   "\x10\x60\x6d\x52\xa2\xd7\x75\x21\x35\x08\xfa\xe5\xa3\x4b\x5c\x9d"
   "\xab\x87\x21\xbe\xb0\xbc\x32\x72\x2c\x22\x00\x6f\xf5\x63\x80\x6e"
   "\x3d\x33\x4a\xab\xef\x9f\x3b\xf0\x25\xc3\x20\xa6\xe9\x55\x07\x0a"
   "\x78\x29\xa1\xb8\xa5\xfd\xd2\xdf\x25\x6a\x53\xba\x6a\x9c\x06\x04"
   "\x36\x39\x9e\x9b\x04\xdd\x2d\x24\xe1\xa7\x34\x95\x93\xef\x67\x2a";

   /* Set 128-bit wide XMM registers to apriori known values. */
   __asm__ __volatile__("\n"
      "movupd   0(%[input]), %%xmm0\n"
      "movupd  16(%[input]), %%xmm1\n"
      "movupd  32(%[input]), %%xmm2\n"
      "movupd  48(%[input]), %%xmm3\n"
      "movupd  64(%[input]), %%xmm4\n"
      "movupd  80(%[input]), %%xmm5\n"
      "movupd  96(%[input]), %%xmm6\n"
      "movupd 112(%[input]), %%xmm7\n"
      "movupd 128(%[input]), %%xmm8\n"
      "movupd 144(%[input]), %%xmm9\n"
      "movupd 160(%[input]), %%xmm10\n"
      "movupd 176(%[input]), %%xmm11\n"
      "movupd 192(%[input]), %%xmm12\n"
      "movupd 208(%[input]), %%xmm13\n"
      "movupd 224(%[input]), %%xmm14\n"
      "movupd 240(%[input]), %%xmm15\n"
      "movq $0x1, %%rax\n"
      "movq $0x1234, (%%rax)\n"  // should cause SEGV here
      : // no output registers
      : [input]  "r" (input)
      : "memory", "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6",
        "%xmm7", "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13",
        "%xmm14", "%xmm15");
}

__attribute__((noinline))
static void outer(void)
{
   inner();
}

int main(int argc, const char *argv[])
{
   outer();
   return 0;
}
