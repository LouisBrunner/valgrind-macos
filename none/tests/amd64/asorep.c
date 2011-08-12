#include <stdint.h>
#include <string.h>
#include <stdio.h>

char buf1[64], buf2[64];

int
main (void)
{
  unsigned long rdi, rsi, rcx, rax;
  uintptr_t b1 = (uintptr_t) buf1, b2 = (uintptr_t) buf2;

  if (b1 > 0xffffffffULL || b2 > 0xffffffffULL)
    return 0;

  b1 += 0x100000000ULL;
  b2 += 0xfff00000000ULL;
  memcpy (buf1, "abcde", 4);
  asm volatile ("addr32 rep movsb"
		: "=D" (rdi), "=S" (rsi), "=c" (rcx)
		: "D" (b2), "S" (b1), "c" (0x100000004ULL)
		: "memory");
  if (memcmp (buf2, "abcd", 5) != 0
      || rdi != (uintptr_t) buf2 + 4
      || rsi != (uintptr_t) buf1 + 4
      || rcx)
    fprintf (stderr, "addr32 rep movsb wrong\n");

  rax = 0x751234560000ULL + (' ' << 8) + '0';
  asm volatile ("addr32 rep stosw"
		: "=D" (rdi), "=c" (rcx), "+a" (rax)
		: "D" (b2), "c" (0x100000003ULL)
		: "memory");
  if (memcmp (buf2, "0 0 0 ", 7) != 0
      || rdi != (uintptr_t) buf2 + 6
      || rcx
      || rax != 0x751234560000ULL + (' ' << 8) + '0')
    fprintf (stderr, "addr32 rep stosw wrong\n");

  asm volatile ("addr32 lodsl"
                : "=S" (rsi), "=a" (rax)
		: "S" (b2), "a" (2ULL));
  if (rsi != (uintptr_t) buf2 + 4
      || rax != 0x20302030ULL)
    fprintf (stderr, "addr32 lodsl wrong\n");

  memcpy (buf1, "abcdefghijklmno", 16);
  memcpy (buf2, "abcdefghijklmnO", 16);
  asm volatile ("addr32 repe cmpsb"
		: "=D" (rdi), "=S" (rsi), "=c" (rcx)
		: "D" (b2), "S" (b1), "c" (0x100000020ULL));
  if (rdi != (uintptr_t) buf2 + 15
      || rsi != (uintptr_t) buf1 + 15
      || rcx != 17ULL)
    fprintf (stderr, "addr32 repe cmpsb wrong\n");

  memcpy (buf2, "ababababababababcdab", 20);
  rax = 0x123450000ULL + ('d' << 8) + 'c';
  asm volatile ("addr32 repne scasw"
		: "=D" (rdi), "=c" (rcx), "+a" (rax)
		: "D" (b2), "c" (0x100000020ULL));
  if (rdi != (uintptr_t) buf2 + 18
      || rcx != 23ULL
      || rax != 0x123450000ULL + ('d' << 8) + 'c')
    fprintf (stderr, "addr32 repne scasw wrong\n");

  rax = 0x543210000ULL + ('b' << 8) + 'a';
  asm volatile ("addr32 repe scasw"
		: "=D" (rdi), "=c" (rcx), "+a" (rax)
		: "D" (b2), "c" (0x100000020ULL));
  if (rdi != (uintptr_t) buf2 + 18
      || rcx != 23ULL
      || rax != 0x543210000ULL + ('b' << 8) + 'a')
    fprintf (stderr, "addr32 repe scasw wrong\n");

  return 0;
}
