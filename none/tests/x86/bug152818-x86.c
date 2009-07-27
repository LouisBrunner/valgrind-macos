#include <stdio.h>
#include <assert.h>
#include <string.h>

/* Test case supplied by Sergei Trofimovich */

/*
 * Real life example (MSDOS file INFO.EXE) has code like this
 *
 * I don't know why author/compiler done code like this. Only guess:
 *    guess 1 (strong :]):
 *      This archaic code was used by dynamic memory regeneration
 *      handler (according to code around it's called from
 *      interrupt handler).
 *
 *    guess 2: cache flush (whether processors had caches at that time?)
 *
 * a disasmed snippet:
 *
 *   mov     byte ptr [bx], 0FFh
 *   sti
 *   mov     cx, 0FFFFh         ; 65535
 *   rep lods byte ptr es:[si]
 *   jcxz    short somewhere_1  ; it seems code could be
 *                              ; interrupted here
 *
 *   call    something_2
 *   cmp     dx, 4
 *   mov     byte ptr [bx], 0
 *   jmp     somewhere_3
 */

#define GET_BIT(var, bit_no) ((var >> bit_no) & 1)

static char sz_eflags[] = "        "; // 8 spaces    
static void pp_eflags (unsigned int _8bits_eflags)
{
  assert (_8bits_eflags >= 0);
  assert (_8bits_eflags <= 0xFF);
  sz_eflags[0] = GET_BIT(_8bits_eflags, 7) ? 'S' : ' ';
  sz_eflags[1] = GET_BIT(_8bits_eflags, 6) ? 'Z' : ' ';
  sz_eflags[3] = GET_BIT(_8bits_eflags, 4) ? 'A' : ' ';
  sz_eflags[5] = GET_BIT(_8bits_eflags, 2) ? 'P' : ' ';
  sz_eflags[7] = GET_BIT(_8bits_eflags, 0) ? 'C' : ' ';
}

#define EMIT_CALL(dir_insn, insn, in_eax, in_esi, in_eflags, out_eax, out_esi, out_eflags, count) \
  asm volatile(                             \
    "movl %3, %%eax \t\n"                       \
    "sahf       \t\n" /* loading our eflags */          \
    "movl %4, %%eax \t\n"                       \
    "movl %5, %%esi \t\n"                       \
    "movl %6, %%ecx \t\n"                       \
                                    \
    dir_insn "\t\n"                         \
    insn "\t\n"                           \
                                    \
    /* return result */                       \
    "movl %%eax, %0 \t\n"                       \
    "lahf       \t\n"                       \
    "movl %%eax, %1 \t\n"                       \
    "movl %%esi, %2 \t\n"                       \
    "cld \t\n" \
    : "=d"(out_eax),                        \
      "=b"(out_eflags),                       \
      "=r"(out_esi)                         \
                                    \
    : "m"(in_eflags),                         \
      "m"(in_eax),                          \
      "m"(in_esi),                          \
      "q"(count)                          \
                                    \
    : "%eax", "%esi", "%ecx", "cc" /* we mess up EFLAGS */);

const signed char  b_mem_buff[] = {-4, -3, -2, -1, 0xaa, 1, 2, 3, 4};
const signed long  l_mem_buff[] = {-4, -3, -2, -1, 0xaa, 1, 2, 3, 4};
const signed short w_mem_buff[] = {-4, -3, -2, -1, 0xaa, 1, 2, 3, 4};

const int lens[] = { 4, 3, 2, 1, 0, 0, 1, 2, 3, 4};

int main ()
{
  const signed char * b_center = (signed char *) memchr(b_mem_buff, 0xaa, sizeof (b_mem_buff));
  const signed char * w_center = (signed char *) memchr(w_mem_buff, 0xaa, sizeof (w_mem_buff));
  const signed char * l_center = (signed char *) memchr(l_mem_buff, 0xaa, sizeof (l_mem_buff));
  
  int insn;
  for (insn = 0; insn < 4; ++insn) //b,w[rep/addr],d,w[addr/rep]
  {
    int idx;
    for (idx = 0; idx < sizeof (lens)/sizeof(lens[0]); ++idx)
    {
      unsigned int eflags;
      unsigned int eax = 0x12348765;
      unsigned int esi;
      const char * i_name = NULL;
      unsigned int resulting_eflags;
      unsigned int resulting_eax;
      unsigned int resulting_esi;
      int len;
      int df;

      switch (insn)
      {
        case 0: //b
          esi = (unsigned int) b_center;
          i_name = "lodsb";
          break;
        case 1: //w 
          esi = (unsigned int) w_center;
          i_name = "lodsw[rep/addr]";
          break;
        case 2: //d
          esi = (unsigned int) l_center;
          i_name = "lodsl";
          break;
        case 3: //w
          esi = (unsigned int) w_center;
          i_name = "lodsw[addr/rep]";
          break;
      }
      
      eflags = 0;
      pp_eflags ((eflags >> 8) & 0xFF); // scratching off AH
      printf ("REP %s (EAX = %08X, EFLAGS = %s) => ", i_name, eax, sz_eflags);
      
      resulting_eflags = 0;
      resulting_eax = 0;

      len = lens[idx];
      df  = (idx >= (sizeof(lens)/sizeof(lens[0]))/2);

      switch (insn)
      {
        case 0: //b
          if (df)
          {
            EMIT_CALL("cld",
                  "rep lodsb",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          else
          {
            EMIT_CALL("std",
                  "rep lodsb",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          break;
        case 1: //w[rep/addr]
          if (df)
          {
            EMIT_CALL("cld",
                  // "rep lodsw",
                  // explicit: rep-pref addr-pref op
                  ".byte 0x66,0xf3,0xad",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          else
          {
            EMIT_CALL("std",
                  // "rep lodsw",
                  // explicit: rep-pref addr-pref op
                  ".byte 0x66,0xf3,0xad",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          break;
        case 2: //d
          if (df)
          {
            EMIT_CALL("cld",
                  "rep lodsl",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          else
          {
            EMIT_CALL("std",
                  "rep lodsl",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          break;
        case 3: //w[addr/rep]
          if (df)
          {
            EMIT_CALL("cld",
                  // "rep lodsw",
                  // explicit: rep-pref addr-pref op
                  ".byte 0xf3,0x66,0xad",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          else
          {
            EMIT_CALL("std",
                  // "rep lodsw",
                  // explicit: rep-pref addr-pref op
                  ".byte 0xf3,0x66,0xad",
                  eax, esi, eflags, resulting_eax, resulting_esi, resulting_eflags,
                  len);
          }
          break;
      }
      printf ("DF = %d, count = %2d ", df, len);
      pp_eflags ((resulting_eflags >> 8) & 0xFF); // scratching off AH
      printf ("(EAX = %08X, EFLAGS = %s)\n", resulting_eax, sz_eflags);
    }
  }
  return 0;
}
