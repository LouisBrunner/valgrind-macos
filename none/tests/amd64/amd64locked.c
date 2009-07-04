
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define VERBOSE 0

typedef  unsigned int            UInt;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;
typedef  signed long long int    Long;
typedef  signed int              Int;
typedef  unsigned short          UShort;
typedef  unsigned long           UWord;
typedef  char                    HChar;

/////////////////////////////////////////////////////////////////
// BEGIN crc32 stuff                                           //
/////////////////////////////////////////////////////////////////

static const UInt crc32Table[256] = {

   /*-- Ugly, innit? --*/

   0x00000000L, 0x04c11db7L, 0x09823b6eL, 0x0d4326d9L,
   0x130476dcL, 0x17c56b6bL, 0x1a864db2L, 0x1e475005L,
   0x2608edb8L, 0x22c9f00fL, 0x2f8ad6d6L, 0x2b4bcb61L,
   0x350c9b64L, 0x31cd86d3L, 0x3c8ea00aL, 0x384fbdbdL,
   0x4c11db70L, 0x48d0c6c7L, 0x4593e01eL, 0x4152fda9L,
   0x5f15adacL, 0x5bd4b01bL, 0x569796c2L, 0x52568b75L,
   0x6a1936c8L, 0x6ed82b7fL, 0x639b0da6L, 0x675a1011L,
   0x791d4014L, 0x7ddc5da3L, 0x709f7b7aL, 0x745e66cdL,
   0x9823b6e0L, 0x9ce2ab57L, 0x91a18d8eL, 0x95609039L,
   0x8b27c03cL, 0x8fe6dd8bL, 0x82a5fb52L, 0x8664e6e5L,
   0xbe2b5b58L, 0xbaea46efL, 0xb7a96036L, 0xb3687d81L,
   0xad2f2d84L, 0xa9ee3033L, 0xa4ad16eaL, 0xa06c0b5dL,
   0xd4326d90L, 0xd0f37027L, 0xddb056feL, 0xd9714b49L,
   0xc7361b4cL, 0xc3f706fbL, 0xceb42022L, 0xca753d95L,
   0xf23a8028L, 0xf6fb9d9fL, 0xfbb8bb46L, 0xff79a6f1L,
   0xe13ef6f4L, 0xe5ffeb43L, 0xe8bccd9aL, 0xec7dd02dL,
   0x34867077L, 0x30476dc0L, 0x3d044b19L, 0x39c556aeL,
   0x278206abL, 0x23431b1cL, 0x2e003dc5L, 0x2ac12072L,
   0x128e9dcfL, 0x164f8078L, 0x1b0ca6a1L, 0x1fcdbb16L,
   0x018aeb13L, 0x054bf6a4L, 0x0808d07dL, 0x0cc9cdcaL,
   0x7897ab07L, 0x7c56b6b0L, 0x71159069L, 0x75d48ddeL,
   0x6b93dddbL, 0x6f52c06cL, 0x6211e6b5L, 0x66d0fb02L,
   0x5e9f46bfL, 0x5a5e5b08L, 0x571d7dd1L, 0x53dc6066L,
   0x4d9b3063L, 0x495a2dd4L, 0x44190b0dL, 0x40d816baL,
   0xaca5c697L, 0xa864db20L, 0xa527fdf9L, 0xa1e6e04eL,
   0xbfa1b04bL, 0xbb60adfcL, 0xb6238b25L, 0xb2e29692L,
   0x8aad2b2fL, 0x8e6c3698L, 0x832f1041L, 0x87ee0df6L,
   0x99a95df3L, 0x9d684044L, 0x902b669dL, 0x94ea7b2aL,
   0xe0b41de7L, 0xe4750050L, 0xe9362689L, 0xedf73b3eL,
   0xf3b06b3bL, 0xf771768cL, 0xfa325055L, 0xfef34de2L,
   0xc6bcf05fL, 0xc27dede8L, 0xcf3ecb31L, 0xcbffd686L,
   0xd5b88683L, 0xd1799b34L, 0xdc3abdedL, 0xd8fba05aL,
   0x690ce0eeL, 0x6dcdfd59L, 0x608edb80L, 0x644fc637L,
   0x7a089632L, 0x7ec98b85L, 0x738aad5cL, 0x774bb0ebL,
   0x4f040d56L, 0x4bc510e1L, 0x46863638L, 0x42472b8fL,
   0x5c007b8aL, 0x58c1663dL, 0x558240e4L, 0x51435d53L,
   0x251d3b9eL, 0x21dc2629L, 0x2c9f00f0L, 0x285e1d47L,
   0x36194d42L, 0x32d850f5L, 0x3f9b762cL, 0x3b5a6b9bL,
   0x0315d626L, 0x07d4cb91L, 0x0a97ed48L, 0x0e56f0ffL,
   0x1011a0faL, 0x14d0bd4dL, 0x19939b94L, 0x1d528623L,
   0xf12f560eL, 0xf5ee4bb9L, 0xf8ad6d60L, 0xfc6c70d7L,
   0xe22b20d2L, 0xe6ea3d65L, 0xeba91bbcL, 0xef68060bL,
   0xd727bbb6L, 0xd3e6a601L, 0xdea580d8L, 0xda649d6fL,
   0xc423cd6aL, 0xc0e2d0ddL, 0xcda1f604L, 0xc960ebb3L,
   0xbd3e8d7eL, 0xb9ff90c9L, 0xb4bcb610L, 0xb07daba7L,
   0xae3afba2L, 0xaafbe615L, 0xa7b8c0ccL, 0xa379dd7bL,
   0x9b3660c6L, 0x9ff77d71L, 0x92b45ba8L, 0x9675461fL,
   0x8832161aL, 0x8cf30badL, 0x81b02d74L, 0x857130c3L,
   0x5d8a9099L, 0x594b8d2eL, 0x5408abf7L, 0x50c9b640L,
   0x4e8ee645L, 0x4a4ffbf2L, 0x470cdd2bL, 0x43cdc09cL,
   0x7b827d21L, 0x7f436096L, 0x7200464fL, 0x76c15bf8L,
   0x68860bfdL, 0x6c47164aL, 0x61043093L, 0x65c52d24L,
   0x119b4be9L, 0x155a565eL, 0x18197087L, 0x1cd86d30L,
   0x029f3d35L, 0x065e2082L, 0x0b1d065bL, 0x0fdc1becL,
   0x3793a651L, 0x3352bbe6L, 0x3e119d3fL, 0x3ad08088L,
   0x2497d08dL, 0x2056cd3aL, 0x2d15ebe3L, 0x29d4f654L,
   0xc5a92679L, 0xc1683bceL, 0xcc2b1d17L, 0xc8ea00a0L,
   0xd6ad50a5L, 0xd26c4d12L, 0xdf2f6bcbL, 0xdbee767cL,
   0xe3a1cbc1L, 0xe760d676L, 0xea23f0afL, 0xeee2ed18L,
   0xf0a5bd1dL, 0xf464a0aaL, 0xf9278673L, 0xfde69bc4L,
   0x89b8fd09L, 0x8d79e0beL, 0x803ac667L, 0x84fbdbd0L,
   0x9abc8bd5L, 0x9e7d9662L, 0x933eb0bbL, 0x97ffad0cL,
   0xafb010b1L, 0xab710d06L, 0xa6322bdfL, 0xa2f33668L,
   0xbcb4666dL, 0xb8757bdaL, 0xb5365d03L, 0xb1f740b4L
};

#define UPDATE_CRC(crcVar,cha)                 \
{                                              \
   crcVar = (crcVar << 8) ^                    \
            crc32Table[(crcVar >> 24) ^        \
                       ((UChar)cha)];          \
}

static UInt crcBytes ( UChar* bytes, UWord nBytes, UInt crcIn )
{
   UInt crc = crcIn;
   while (nBytes >= 4) {
      UPDATE_CRC(crc, bytes[0]);
      UPDATE_CRC(crc, bytes[1]);
      UPDATE_CRC(crc, bytes[2]);
      UPDATE_CRC(crc, bytes[3]);
      bytes += 4;
      nBytes -= 4;
   }
   while (nBytes >= 1) {
      UPDATE_CRC(crc, bytes[0]);
      bytes += 1;
      nBytes -= 1;
   }
   return crc;
}

static UInt crcFinalise ( UInt crc ) {
   return ~crc;
}

////////

static UInt theCRC = 0xFFFFFFFF;

static HChar outBuf[1024];
// take output that's in outBuf, length as specified, and
// update the running crc.
static void send ( int nbytes )
{
   assert( ((unsigned int)nbytes) < sizeof(outBuf)-1);
   assert(outBuf[nbytes] == 0);
   theCRC = crcBytes( (UChar*)&outBuf[0], nbytes, theCRC );
   if (VERBOSE) printf("SEND %08x %s", theCRC, outBuf);
}


/////////////////////////////////////////////////////////////////
// END crc32 stuff                                             //
/////////////////////////////////////////////////////////////////

#if 0

// full version
#define NVALS 76

static ULong val[NVALS]
    = { 0x00ULL, 0x01ULL, 0x02ULL, 0x03ULL,
        0x3FULL, 0x40ULL, 0x41ULL,
        0x7EULL, 0x7FULL, 0x80ULL, 0x81ULL, 0x82ULL,
        0xBFULL, 0xC0ULL, 0xC1ULL,
        0xFCULL, 0xFDULL, 0xFEULL, 0xFFULL,

        0xFF00ULL, 0xFF01ULL, 0xFF02ULL, 0xFF03ULL,
        0xFF3FULL, 0xFF40ULL, 0xFF41ULL,
        0xFF7EULL, 0xFF7FULL, 0xFF80ULL, 0xFF81ULL, 0xFF82ULL,
        0xFFBFULL, 0xFFC0ULL, 0xFFC1ULL,
        0xFFFCULL, 0xFFFDULL, 0xFFFEULL, 0xFFFFULL,

        0xFFFFFF00ULL, 0xFFFFFF01ULL, 0xFFFFFF02ULL, 0xFFFFFF03ULL,
        0xFFFFFF3FULL, 0xFFFFFF40ULL, 0xFFFFFF41ULL,
        0xFFFFFF7EULL, 0xFFFFFF7FULL, 0xFFFFFF80ULL, 0xFFFFFF81ULL, 0xFFFFFF82ULL,
        0xFFFFFFBFULL, 0xFFFFFFC0ULL, 0xFFFFFFC1ULL,
        0xFFFFFFFCULL, 0xFFFFFFFDULL, 0xFFFFFFFEULL, 0xFFFFFFFFULL,

        0xFFFFFFFFFFFFFF00ULL, 0xFFFFFFFFFFFFFF01ULL, 0xFFFFFFFFFFFFFF02ULL,
                               0xFFFFFFFFFFFFFF03ULL,
        0xFFFFFFFFFFFFFF3FULL, 0xFFFFFFFFFFFFFF40ULL, 0xFFFFFFFFFFFFFF41ULL,
        0xFFFFFFFFFFFFFF7EULL, 0xFFFFFFFFFFFFFF7FULL, 0xFFFFFFFFFFFFFF80ULL,
                               0xFFFFFFFFFFFFFF81ULL, 0xFFFFFFFFFFFFFF82ULL,
        0xFFFFFFFFFFFFFFBFULL, 0xFFFFFFFFFFFFFFC0ULL, 0xFFFFFFFFFFFFFFC1ULL,
        0xFFFFFFFFFFFFFFFCULL, 0xFFFFFFFFFFFFFFFDULL, 0xFFFFFFFFFFFFFFFEULL,
                               0xFFFFFFFFFFFFFFFFULL
      };

#else

// shortened version, for use as valgrind regtest
#define NVALS 36

static ULong val[NVALS]
    = { 0x00ULL, 0x01ULL,
        0x3FULL, 0x40ULL,
        0x7FULL, 0x80ULL,
        0xBFULL, 0xC0ULL,
        0xFFULL,

        0xFF00ULL, 0xFF01ULL,
        0xFF3FULL, 0xFF40ULL,
        0xFF7FULL, 0xFF80ULL,
        0xFFBFULL, 0xFFC0ULL,
        0xFFFFULL,

        0xFFFFFF00ULL, 0xFFFFFF01ULL,
        0xFFFFFF3FULL, 0xFFFFFF40ULL,
        0xFFFFFF7EULL, 0xFFFFFF7FULL,
        0xFFFFFFBFULL, 0xFFFFFFC0ULL,
        0xFFFFFFFFULL,

        0xFFFFFFFFFFFFFF00ULL, 0xFFFFFFFFFFFFFF01ULL,
        0xFFFFFFFFFFFFFF3FULL, 0xFFFFFFFFFFFFFF40ULL,
        0xFFFFFFFFFFFFFF7FULL, 0xFFFFFFFFFFFFFF80ULL,
        0xFFFFFFFFFFFFFFBFULL, 0xFFFFFFFFFFFFFFC0ULL,
        0xFFFFFFFFFFFFFFFFULL
      };

#endif

/////////////////////////////////////

#define CC_C    0x0001
#define CC_P    0x0004
#define CC_A    0x0010
#define CC_Z    0x0040
#define CC_S    0x0080
#define CC_O    0x0800

#define CC_MASK (CC_C | CC_P | CC_A | CC_Z | CC_S | CC_O)

#define GEN_do_locked_G_E(_name,_eax)   \
  \
  __attribute__((noinline)) void do_locked_G_E_##_name ( void )  \
  {   \
    volatile Long e_val, g_val, e_val_before;   \
    Long o, s, z, a, c, p, v1, v2, flags_in;       \
    Long block[4];   \
    \
    for (v1 = 0; v1 < NVALS; v1++) {   \
    for (v2 = 0; v2 < NVALS; v2++) {   \
    \
    for (o = 0; o < 2; o++) {   \
    for (s = 0; s < 2; s++) {   \
    for (z = 0; z < 2; z++) {   \
    for (a = 0; a < 2; a++) {   \
    for (c = 0; c < 2; c++) {   \
    for (p = 0; p < 2; p++) {   \
      \
      flags_in = (o ? CC_O : 0)   \
               | (s ? CC_S : 0)   \
               | (z ? CC_Z : 0)   \
               | (a ? CC_A : 0)   \
               | (c ? CC_C : 0)   \
               | (p ? CC_P : 0);   \
      \
      g_val = val[v1];   \
      e_val = val[v2];   \
      e_val_before = e_val;   \
      \
      block[0] = flags_in;   \
      block[1] = g_val;   \
      block[2] = (long)&e_val;   \
      block[3] = 0;   \
      __asm__ __volatile__(   \
          "movq 0(%0), %%rax\n\t"   \
          "pushq %%rax\n\t"   \
          "popfq\n\t"   \
          "movq 8(%0), %%rax\n\t"   \
          "movq 16(%0), %%rbx\n\t"   \
          "lock; " #_name " %%" #_eax ",(%%rbx)\n\t"   \
          "pushfq\n\t"   \
          "popq %%rax\n\t"   \
          "movq %%rax, 24(%0)\n\t"   \
          : : "r"(&block[0]) : "rax","rbx","cc","memory"   \
      );   \
      \
      send( \
      sprintf(outBuf, \
             "%s G=%016llx E=%016llx CCIN=%08llx -> E=%016llx CCOUT=%08llx\n",       \
             #_name, g_val, e_val_before, flags_in,   \
              e_val, block[3] & CC_MASK));            \
      \
    }}}}}}   \
    \
    }}   \
  }

GEN_do_locked_G_E(addb,al)
GEN_do_locked_G_E(addw,ax)
GEN_do_locked_G_E(addl,eax)
GEN_do_locked_G_E(addq,rax)

GEN_do_locked_G_E(orb, al)
GEN_do_locked_G_E(orw, ax)
GEN_do_locked_G_E(orl, eax)
GEN_do_locked_G_E(orq, rax)

GEN_do_locked_G_E(adcb,al)
GEN_do_locked_G_E(adcw,ax)
GEN_do_locked_G_E(adcl,eax)
GEN_do_locked_G_E(adcq,rax)

GEN_do_locked_G_E(sbbb,al)
GEN_do_locked_G_E(sbbw,ax)
GEN_do_locked_G_E(sbbl,eax)
GEN_do_locked_G_E(sbbq,rax)

GEN_do_locked_G_E(andb,al)
GEN_do_locked_G_E(andw,ax)
GEN_do_locked_G_E(andl,eax)
GEN_do_locked_G_E(andq,rax)

GEN_do_locked_G_E(subb,al)
GEN_do_locked_G_E(subw,ax)
GEN_do_locked_G_E(subl,eax)
GEN_do_locked_G_E(subq,rax)

GEN_do_locked_G_E(xorb,al)
GEN_do_locked_G_E(xorw,ax)
GEN_do_locked_G_E(xorl,eax)
GEN_do_locked_G_E(xorq,rax)




#define GEN_do_locked_imm_E(_name,_eax,_imm)        \
  \
  __attribute__((noinline)) void do_locked_imm_E_##_name##_##_imm ( void )  \
  {   \
    volatile Long e_val, e_val_before;   \
    Long o, s, z, a, c, p, v2, flags_in;   \
    Long block[3];   \
    \
    for (v2 = 0; v2 < NVALS; v2++) {   \
    \
    for (o = 0; o < 2; o++) {   \
    for (s = 0; s < 2; s++) {   \
    for (z = 0; z < 2; z++) {   \
    for (a = 0; a < 2; a++) {   \
    for (c = 0; c < 2; c++) {   \
    for (p = 0; p < 2; p++) {   \
      \
      flags_in = (o ? CC_O : 0)   \
               | (s ? CC_S : 0)   \
               | (z ? CC_Z : 0)   \
               | (a ? CC_A : 0)   \
               | (c ? CC_C : 0)   \
               | (p ? CC_P : 0);   \
      \
      e_val = val[v2];   \
      e_val_before = e_val;   \
      \
      block[0] = flags_in;   \
      block[1] = (long)&e_val;   \
      block[2] = 0;   \
      __asm__ __volatile__(   \
          "movq 0(%0), %%rax\n\t"   \
          "pushq %%rax\n\t"   \
          "popfq\n\t"   \
          "movq 8(%0), %%rbx\n\t"   \
          "lock; " #_name " $" #_imm ",(%%rbx)\n\t"   \
          "pushfq\n\t"   \
          "popq %%rax\n\t"   \
          "movq %%rax, 16(%0)\n\t"   \
          : : "r"(&block[0]) : "rax","rbx","cc","memory"   \
      );   \
      \
      send( \
           sprintf(outBuf, \
           "%s I=%s E=%016llx CCIN=%08llx -> E=%016llx CCOUT=%08llx\n",    \
             #_name, #_imm, e_val_before, flags_in,         \
                   e_val, block[2] & CC_MASK));             \
      \
    }}}}}}   \
    \
    }   \
  }

GEN_do_locked_imm_E(addb,al,0x7F)
GEN_do_locked_imm_E(addb,al,0xF1)
GEN_do_locked_imm_E(addw,ax,0x7E)
GEN_do_locked_imm_E(addw,ax,0x9325)
GEN_do_locked_imm_E(addl,eax,0x7D)
GEN_do_locked_imm_E(addl,eax,0x31415927)
GEN_do_locked_imm_E(addq,rax,0x7D)
GEN_do_locked_imm_E(addq,rax,0x31415927)

GEN_do_locked_imm_E(orb,al,0x7F)
GEN_do_locked_imm_E(orb,al,0xF1)
GEN_do_locked_imm_E(orw,ax,0x7E)
GEN_do_locked_imm_E(orw,ax,0x9325)
GEN_do_locked_imm_E(orl,eax,0x7D)
GEN_do_locked_imm_E(orl,eax,0x31415927)
GEN_do_locked_imm_E(orq,rax,0x7D)
GEN_do_locked_imm_E(orq,rax,0x31415927)

GEN_do_locked_imm_E(adcb,al,0x7F)
GEN_do_locked_imm_E(adcb,al,0xF1)
GEN_do_locked_imm_E(adcw,ax,0x7E)
GEN_do_locked_imm_E(adcw,ax,0x9325)
GEN_do_locked_imm_E(adcl,eax,0x7D)
GEN_do_locked_imm_E(adcl,eax,0x31415927)
GEN_do_locked_imm_E(adcq,rax,0x7D)
GEN_do_locked_imm_E(adcq,rax,0x31415927)

GEN_do_locked_imm_E(sbbb,al,0x7F)
GEN_do_locked_imm_E(sbbb,al,0xF1)
GEN_do_locked_imm_E(sbbw,ax,0x7E)
GEN_do_locked_imm_E(sbbw,ax,0x9325)
GEN_do_locked_imm_E(sbbl,eax,0x7D)
GEN_do_locked_imm_E(sbbl,eax,0x31415927)
GEN_do_locked_imm_E(sbbq,rax,0x7D)
GEN_do_locked_imm_E(sbbq,rax,0x31415927)

GEN_do_locked_imm_E(andb,al,0x7F)
GEN_do_locked_imm_E(andb,al,0xF1)
GEN_do_locked_imm_E(andw,ax,0x7E)
GEN_do_locked_imm_E(andw,ax,0x9325)
GEN_do_locked_imm_E(andl,eax,0x7D)
GEN_do_locked_imm_E(andl,eax,0x31415927)
GEN_do_locked_imm_E(andq,rax,0x7D)
GEN_do_locked_imm_E(andq,rax,0x31415927)

GEN_do_locked_imm_E(subb,al,0x7F)
GEN_do_locked_imm_E(subb,al,0xF1)
GEN_do_locked_imm_E(subw,ax,0x7E)
GEN_do_locked_imm_E(subw,ax,0x9325)
GEN_do_locked_imm_E(subl,eax,0x7D)
GEN_do_locked_imm_E(subl,eax,0x31415927)
GEN_do_locked_imm_E(subq,rax,0x7D)
GEN_do_locked_imm_E(subq,rax,0x31415927)

GEN_do_locked_imm_E(xorb,al,0x7F)
GEN_do_locked_imm_E(xorb,al,0xF1)
GEN_do_locked_imm_E(xorw,ax,0x7E)
GEN_do_locked_imm_E(xorw,ax,0x9325)
GEN_do_locked_imm_E(xorl,eax,0x7D)
GEN_do_locked_imm_E(xorl,eax,0x31415927)
GEN_do_locked_imm_E(xorq,rax,0x7D)
GEN_do_locked_imm_E(xorq,rax,0x31415927)

#define GEN_do_locked_unary_E(_name,_eax)        \
  \
  __attribute__((noinline)) void do_locked_unary_E_##_name ( void )  \
  {   \
    volatile Long e_val, e_val_before;   \
    Long o, s, z, a, c, p, v2, flags_in;     \
    Long block[3];   \
    \
    for (v2 = 0; v2 < NVALS; v2++) {   \
    \
    for (o = 0; o < 2; o++) {   \
    for (s = 0; s < 2; s++) {   \
    for (z = 0; z < 2; z++) {   \
    for (a = 0; a < 2; a++) {   \
    for (c = 0; c < 2; c++) {   \
    for (p = 0; p < 2; p++) {   \
      \
      flags_in = (o ? CC_O : 0)   \
               | (s ? CC_S : 0)   \
               | (z ? CC_Z : 0)   \
               | (a ? CC_A : 0)   \
               | (c ? CC_C : 0)   \
               | (p ? CC_P : 0);   \
      \
      e_val = val[v2];   \
      e_val_before = e_val;   \
      \
      block[0] = flags_in;   \
      block[1] = (long)&e_val;   \
      block[2] = 0;   \
      __asm__ __volatile__(   \
          "movq 0(%0), %%rax\n\t"   \
          "pushq %%rax\n\t"   \
          "popfq\n\t"   \
          "movq 8(%0), %%rbx\n\t"   \
          "lock; " #_name " (%%rbx)\n\t"   \
          "pushfq\n\t"   \
          "popq %%rax\n\t"   \
          "movq %%rax, 16(%0)\n\t"   \
          : : "r"(&block[0]) : "rax","rbx","cc","memory"   \
      );   \
      \
      send( \
           sprintf(outBuf, \
            "%s E=%016llx CCIN=%08llx -> E=%016llx CCOUT=%08llx\n", \
             #_name, e_val_before, flags_in,         \
            e_val, block[2] & CC_MASK));                       \
      \
    }}}}}}   \
    \
    }   \
  }

GEN_do_locked_unary_E(decb,al)
GEN_do_locked_unary_E(decw,ax)
GEN_do_locked_unary_E(decl,eax)
GEN_do_locked_unary_E(decq,rax)

GEN_do_locked_unary_E(incb,al)
GEN_do_locked_unary_E(incw,ax)
GEN_do_locked_unary_E(incl,eax)
GEN_do_locked_unary_E(incq,rax)

GEN_do_locked_unary_E(negb,al)
GEN_do_locked_unary_E(negw,ax)
GEN_do_locked_unary_E(negl,eax)
GEN_do_locked_unary_E(negq,rax)

GEN_do_locked_unary_E(notb,al)
GEN_do_locked_unary_E(notw,ax)
GEN_do_locked_unary_E(notl,eax)
GEN_do_locked_unary_E(notq,rax)


/////////////////////////////////////////////////////////////////

ULong btsq_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btsq\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno) : "rdx","cc","memory" );
   /* Pretty meaningless to dereference base here, but that's what you
      have to do to get a btsl insn which refers to memory starting at
      base. */
   return res;
}
ULong btsl_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btsl\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((UInt)bitno));
   return res;
}
ULong btsw_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btsw\t%w2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno));
   return res;
}

ULong btrq_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btrq\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno));
   return res;
}
ULong btrl_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btrl\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((UInt)bitno));
   return res;
}
ULong btrw_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btrw\t%w2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno));
   return res;
}

ULong btcq_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btcq\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno));
   return res;
}
ULong btcl_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btcl\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((UInt)bitno));
   return res;
}
ULong btcw_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("lock; btcw\t%w2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno));
   return res;
}

ULong btq_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("btq\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno)
                : "cc", "memory");
   return res;
}
ULong btl_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("btl\t%2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((UInt)bitno)
                : "cc", "memory");
   return res;
}
ULong btw_mem ( UChar* base, int bitno )
{
   ULong res;
   __asm__ 
   __volatile__("btw\t%w2, %0\n\t"
                "setc   %%dl\n\t" 
                "movzbq %%dl,%1\n"
                : "=m" (*base), "=r" (res)
                : "r" ((ULong)bitno));
   return res;
}

ULong rol1 ( ULong x )
{
  return (x << 1) | (x >> 63);
}

void do_bt_G_E_tests ( void )
{
   ULong  n, bitoff, op;
   ULong  c;
   UChar* block;
   ULong  carrydep, res;;

   /*------------------------ MEM-Q -----------------------*/

   carrydep = 0;
   block = calloc(200,1);
   block += 100;
   /* Valid bit offsets are -800 .. 799 inclusive. */

   for (n = 0; n < 10000; n++) {
      bitoff = (random() % 1600) - 800;
      op = random() % 4;
      c = 2;
      switch (op) {
         case 0: c = btsq_mem(block, bitoff); break;
         case 1: c = btrq_mem(block, bitoff); break;
         case 2: c = btcq_mem(block, bitoff); break;
         case 3: c = btq_mem(block, bitoff); break;
      }
      c &= 255;
      assert(c == 0 || c == 1);
      carrydep = c ? (rol1(carrydep) ^ (Long)bitoff) : carrydep;
   }

   /* Compute final result */
   block -= 100;   
   res = 0;
   for (n = 0; n < 200; n++) {
      UChar ch = block[n];
      /* printf("%d ", (int)block[n]); */
      res = rol1(res) ^ (ULong)ch;
   }

   send( sprintf(outBuf,
                 "bt{s,r,c}q: final res 0x%llx, carrydep 0x%llx\n",
                 res, carrydep));
   free(block);

   /*------------------------ MEM-L -----------------------*/

   carrydep = 0;
   block = calloc(200,1);
   block += 100;
   /* Valid bit offsets are -800 .. 799 inclusive. */

   for (n = 0; n < 10000; n++) {
      bitoff = (random() % 1600) - 800;
      op = random() % 4;
      c = 2;
      switch (op) {
         case 0: c = btsl_mem(block, bitoff); break;
         case 1: c = btrl_mem(block, bitoff); break;
         case 2: c = btcl_mem(block, bitoff); break;
         case 3: c = btl_mem(block, bitoff); break;
      }
      c &= 255;
      assert(c == 0 || c == 1);
      carrydep = c ? (rol1(carrydep) ^ (Long)(Int)bitoff) : carrydep;
   }

   /* Compute final result */
   block -= 100;   
   res = 0;
   for (n = 0; n < 200; n++) {
      UChar ch = block[n];
      /* printf("%d ", (int)block[n]); */
      res = rol1(res) ^ (ULong)ch;
   }

   send( sprintf(outBuf,
                 "bt{s,r,c}l: final res 0x%llx, carrydep 0x%llx\n",
                 res, carrydep));
   free(block);

   /*------------------------ MEM-W -----------------------*/

   carrydep = 0;
   block = calloc(200,1);
   block += 100;
   /* Valid bit offsets are -800 .. 799 inclusive. */

   for (n = 0; n < 10000; n++) {
      bitoff = (random() % 1600) - 800;
      op = random() % 4;
      c = 2;
      switch (op) {
         case 0: c = btsw_mem(block, bitoff); break;
         case 1: c = btrw_mem(block, bitoff); break;
         case 2: c = btcw_mem(block, bitoff); break;
         case 3: c = btw_mem(block, bitoff); break;
      }
      c &= 255;
      assert(c == 0 || c == 1);
      carrydep = c ? (rol1(carrydep) ^ (Long)(Int)bitoff) : carrydep;
   }

   /* Compute final result */
   block -= 100;   
   res = 0;
   for (n = 0; n < 200; n++) {
      UChar ch = block[n];
      /* printf("%d ", (int)block[n]); */
      res = rol1(res) ^ (ULong)ch;
   }

   send(sprintf(outBuf,
                "bt{s,r,c}w: final res 0x%llx, carrydep 0x%llx\n",
                res, carrydep));
   free(block);
}


/////////////////////////////////////////////////////////////////

/* Given a word, do bt/bts/btr/btc on bits 0, 1, 2 and 3 of it, and
   also reconstruct the original bits 0, 1, 2, 3 by looking at the
   carry flag.  Returned result has mashed bits 0-3 at the bottom and
   the reconstructed original bits 0-3 as 4-7. */

ULong mash_mem_Q ( ULong* origp )
{
  ULong reconstructed, mashed;
  __asm__ __volatile__ (
     "movq %2, %%rdx\n\t"
     ""
     "movq $0, %%rax\n\t"
     "\n\t"
     "btq  $0, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btsq $1, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $1, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btrq $2, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $2, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btcq $3, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $3, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "movq %%rax, %0\n\t"
     "movq (%%rdx), %1"
     : "=r" (reconstructed), "=r" (mashed)
     : "r" (origp)
     : "rax", "rcx", "rdx", "cc");
  return (mashed & 0xF) | ((reconstructed & 0xF) << 4);
}

ULong mash_mem_L ( UInt* origp )
{
  ULong reconstructed; UInt mashed;
  __asm__ __volatile__ (
     "movq %2, %%rdx\n\t"
     ""
     "movq $0, %%rax\n\t"
     "\n\t"
     "btl  $0, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btsl $1, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $1, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btrl $2, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $2, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btcl $3, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $3, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "movq %%rax, %0\n\t"
     "movl (%%rdx), %1"
     : "=r" (reconstructed), "=r" (mashed)
     : "r" (origp)
     : "rax", "rcx", "rdx", "cc");
  return (mashed & 0xF) | ((reconstructed & 0xF) << 4);
}

ULong mash_mem_W ( UShort* origp )
{
  ULong reconstructed, mashed;
  __asm__ __volatile__ (
     "movq %2, %%rdx\n\t"
     ""
     "movq $0, %%rax\n\t"
     "\n\t"
     "btw  $0, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btsw $1, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $1, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btrw $2, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $2, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "lock; btcw $3, (%%rdx)\n\t"
     "setb %%cl\n\t"
     "movzbq %%cl, %%rcx\n\t"
     "shlq $3, %%rcx\n\t"
     "orq %%rcx, %%rax\n\t"
     "\n\t"
     "movq %%rax, %0\n\t"
     "movzwq (%%rdx), %1"
     : "=r" (reconstructed), "=r" (mashed)
     : "r" (origp)
     : "rax", "rcx", "rdx", "cc");
  return (mashed & 0xF) | ((reconstructed & 0xF) << 4);
}


void do_bt_imm_E_tests( void )
{
  ULong i;
  ULong*  iiq = malloc(sizeof(ULong));
  UInt*   iil = malloc(sizeof(UInt));
  UShort* iiw = malloc(sizeof(UShort));
  for (i = 0; i < 0x10; i++) {
    *iiq = i;
    *iil = i;
    *iiw = i;
    send(sprintf(outBuf,"0x%llx -> 0x%02llx 0x%02llx 0x%02llx\n", i, 
                 mash_mem_Q(iiq), mash_mem_L(iil), mash_mem_W(iiw)));
  }
  free(iiq);
  free(iil);
  free(iiw);
}


/////////////////////////////////////////////////////////////////

int main ( void )
{
  do_locked_G_E_addb();
  do_locked_G_E_addw();
  do_locked_G_E_addl();
  do_locked_G_E_addq();

  do_locked_G_E_orb();
  do_locked_G_E_orw();
  do_locked_G_E_orl();
  do_locked_G_E_orq();

  do_locked_G_E_adcb();
  do_locked_G_E_adcw();
  do_locked_G_E_adcl();
  do_locked_G_E_adcq();

  do_locked_G_E_sbbb();
  do_locked_G_E_sbbw();
  do_locked_G_E_sbbl();
  do_locked_G_E_sbbq();

  do_locked_G_E_andb();
  do_locked_G_E_andw();
  do_locked_G_E_andl();
  do_locked_G_E_andq();

  do_locked_G_E_subb();
  do_locked_G_E_subw();
  do_locked_G_E_subl();
  do_locked_G_E_subq();

  do_locked_G_E_xorb();
  do_locked_G_E_xorw();
  do_locked_G_E_xorl();
  do_locked_G_E_xorq();
  // 4 * 7

  do_locked_imm_E_addb_0x7F();
  do_locked_imm_E_addb_0xF1();
  do_locked_imm_E_addw_0x7E();
  do_locked_imm_E_addw_0x9325();
  do_locked_imm_E_addl_0x7D();
  do_locked_imm_E_addl_0x31415927();
  do_locked_imm_E_addq_0x7D();
  do_locked_imm_E_addq_0x31415927();

  do_locked_imm_E_orb_0x7F();
  do_locked_imm_E_orb_0xF1();
  do_locked_imm_E_orw_0x7E();
  do_locked_imm_E_orw_0x9325();
  do_locked_imm_E_orl_0x7D();
  do_locked_imm_E_orl_0x31415927();
  do_locked_imm_E_orq_0x7D();
  do_locked_imm_E_orq_0x31415927();

  do_locked_imm_E_adcb_0x7F();
  do_locked_imm_E_adcb_0xF1();
  do_locked_imm_E_adcw_0x7E();
  do_locked_imm_E_adcw_0x9325();
  do_locked_imm_E_adcl_0x7D();
  do_locked_imm_E_adcl_0x31415927();
  do_locked_imm_E_adcq_0x7D();
  do_locked_imm_E_adcq_0x31415927();

  do_locked_imm_E_sbbb_0x7F();
  do_locked_imm_E_sbbb_0xF1();
  do_locked_imm_E_sbbw_0x7E();
  do_locked_imm_E_sbbw_0x9325();
  do_locked_imm_E_sbbl_0x7D();
  do_locked_imm_E_sbbl_0x31415927();
  do_locked_imm_E_sbbq_0x7D();
  do_locked_imm_E_sbbq_0x31415927();

  do_locked_imm_E_andb_0x7F();
  do_locked_imm_E_andb_0xF1();
  do_locked_imm_E_andw_0x7E();
  do_locked_imm_E_andw_0x9325();
  do_locked_imm_E_andl_0x7D();
  do_locked_imm_E_andl_0x31415927();
  do_locked_imm_E_andq_0x7D();
  do_locked_imm_E_andq_0x31415927();

  do_locked_imm_E_subb_0x7F();
  do_locked_imm_E_subb_0xF1();
  do_locked_imm_E_subw_0x7E();
  do_locked_imm_E_subw_0x9325();
  do_locked_imm_E_subl_0x7D();
  do_locked_imm_E_subl_0x31415927();
  do_locked_imm_E_subq_0x7D();
  do_locked_imm_E_subq_0x31415927();

  do_locked_imm_E_xorb_0x7F();
  do_locked_imm_E_xorb_0xF1();
  do_locked_imm_E_xorw_0x7E();
  do_locked_imm_E_xorw_0x9325();
  do_locked_imm_E_xorl_0x7D();
  do_locked_imm_E_xorl_0x31415927();
  do_locked_imm_E_xorq_0x7D();
  do_locked_imm_E_xorq_0x31415927();
  // 4 * 7 + 8 * 7 == 84

  do_locked_unary_E_decb();
  do_locked_unary_E_decw();
  do_locked_unary_E_decl();
  do_locked_unary_E_decq();

  do_locked_unary_E_incb();
  do_locked_unary_E_incw();
  do_locked_unary_E_incl();
  do_locked_unary_E_incq();

  do_locked_unary_E_negb();
  do_locked_unary_E_negw();
  do_locked_unary_E_negl();
  do_locked_unary_E_negq();

  do_locked_unary_E_notb();
  do_locked_unary_E_notw();
  do_locked_unary_E_notl();
  do_locked_unary_E_notq();
  // 100

  do_bt_G_E_tests();
  // 109
  do_bt_imm_E_tests();
  // 118

  // So there should be 118 lock-prefixed instructions in the
  // disassembly of this compilation unit.
  // confirm with
  // objdump -d ./amd64locked | grep lock | grep -v do_lock | grep -v elf64 | wc


  { UInt crcExpd = 0x1F677629;
    theCRC = crcFinalise( theCRC );
    if (theCRC == crcExpd) {
       printf("amd64locked: PASS: CRCs actual 0x%08X expected 0x%08X\n",
              theCRC, crcExpd);
    } else {
       printf("amd64locked: FAIL: CRCs actual 0x%08X expected 0x%08X\n",
              theCRC, crcExpd);
       printf("amd64locked: set #define VERBOSE 1 to diagnose\n");
    }
  }

  return 0;
}
