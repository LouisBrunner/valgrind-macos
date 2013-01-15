/* -*- mode: C; c-basic-offset: 3; -*- */

#include <stdio.h>   // fprintf
#include <assert.h>  // assert
#if defined(__APPLE__)
#include <machine/endian.h>
#define __BYTE_ORDER    BYTE_ORDER
#define __LITTLE_ENDIAN LITTLE_ENDIAN
#else
#include <endian.h>
#endif
#include <inttypes.h>
#include "vbits.h"
#include "vtest.h"


/* Return the bits of V if they fit into 64-bit. If V has fewer than
   64 bits, the bit pattern is zero-extended to the left. */
static uint64_t
get_bits64(vbits_t v)
{
   switch (v.num_bits) {
   case 1:  return v.bits.u32;
   case 8:  return v.bits.u8;
   case 16: return v.bits.u16;
   case 32: return v.bits.u32;
   case 64: return v.bits.u64;
   case 128:
   case 256:
      /* fall through */
   default:
      panic(__func__);
   }
}

void
print_vbits(FILE *fp, vbits_t v)
{
   switch (v.num_bits) {
   case 1:   fprintf(fp, "%08x",   v.bits.u32); break;
   case 8:   fprintf(fp, "%02x",   v.bits.u8);  break;
   case 16:  fprintf(fp, "%04x",   v.bits.u16); break;
   case 32:  fprintf(fp, "%08x",   v.bits.u32); break;
   case 64:  fprintf(fp, "%016"PRIx64, v.bits.u64); break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, v.bits.u128[1]);
         fprintf(fp, "%016"PRIx64, v.bits.u128[0]);
      } else {
         fprintf(fp, "%016"PRIx64, v.bits.u128[0]);
         fprintf(fp, "%016"PRIx64, v.bits.u128[1]);
      }
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, v.bits.u256[3]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[2]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[1]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[0]);
      } else {
         fprintf(fp, "%016"PRIx64, v.bits.u256[0]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[1]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[2]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[3]);
      }
      break;
   default:
      panic(__func__);
   }
}


/* Return a value where all bits are set to undefined. */
vbits_t
undefined_vbits(unsigned num_bits)
{
   vbits_t new = { .num_bits = num_bits };

   switch (num_bits) {
   case   1: new.bits.u32 = 0x01;   break;
   case   8: new.bits.u8  = 0xff;   break;
   case  16: new.bits.u16 = 0xffff; break;
   case  32: new.bits.u32 = ~0;     break;
   case  64: new.bits.u64 = ~0ull;  break;
   case 128: new.bits.u128[0] = ~0ull;
             new.bits.u128[1] = ~0ull;
             break;
   case 256: new.bits.u256[0] = ~0ull;
             new.bits.u256[1] = ~0ull;
             new.bits.u256[2] = ~0ull;
             new.bits.u256[3] = ~0ull;
             break;
   default:
      panic(__func__);
   }
   return new;
}


/* Return a value where all bits are set to defined. */
vbits_t
defined_vbits(unsigned num_bits)
{
   vbits_t new = { .num_bits = num_bits };

   switch (num_bits) {
   case   1: new.bits.u32 = 0x0; break;
   case   8: new.bits.u8  = 0x0; break;
   case  16: new.bits.u16 = 0x0; break;
   case  32: new.bits.u32 = 0x0; break;
   case  64: new.bits.u64 = 0x0; break;
   case 128: new.bits.u128[0] = 0x0;
             new.bits.u128[1] = 0x0;
             break;
   case 256: new.bits.u256[0] = 0x0;
             new.bits.u256[1] = 0x0;
             new.bits.u256[2] = 0x0;
             new.bits.u256[3] = 0x0;
             break;
   default:
      panic(__func__);
   }
   return new;
}


/* Return 1, if equal. */
int
equal_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   switch (v1.num_bits) {
   case 1:   return v1.bits.u32 == v2.bits.u32;
   case 8:   return v1.bits.u8  == v2.bits.u8;
   case 16:  return v1.bits.u16 == v2.bits.u16;
   case 32:  return v1.bits.u32 == v2.bits.u32;
   case 64:  return v1.bits.u64 == v2.bits.u64;
   case 128: return v1.bits.u128[0] == v2.bits.u128[0] &&
                    v1.bits.u128[1] == v2.bits.u128[1];
   case 256: return v1.bits.u256[0] == v2.bits.u256[0] &&
                    v1.bits.u256[1] == v2.bits.u256[1] &&
                    v1.bits.u256[2] == v2.bits.u256[2] &&
                    v1.bits.u256[3] == v2.bits.u256[3];
   default:
      panic(__func__);
   }
}


/* Truncate the bit pattern in V1 to NUM_BITS bits */
vbits_t
truncate_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits <= v.num_bits);

   if (num_bits == v.num_bits) return v;

   vbits_t new = { .num_bits = num_bits };

   if (num_bits <= 64) {
      uint64_t bits;

      if (v.num_bits <= 64)
         bits = get_bits64(v);
      else if (v.num_bits == 128)
         if (__BYTE_ORDER == __LITTLE_ENDIAN)
            bits = v.bits.u128[0];
         else
            bits = v.bits.u128[1];
      else if (v.num_bits == 256)
         if (__BYTE_ORDER == __LITTLE_ENDIAN)
            bits = v.bits.u256[0];
         else
            bits = v.bits.u256[3];
      else
         panic(__func__);

      switch (num_bits) {
      case 1:   new.bits.u32 = bits & 0x01;   break;
      case 8:   new.bits.u8  = bits & 0xff;   break;
      case 16:  new.bits.u16 = bits & 0xffff; break;
      case 32:  new.bits.u32 = bits & ~0u;    break;
      case 64:  new.bits.u64 = bits & ~0ll;   break;
      default:
         panic(__func__);
      }
      return new;
   }

   if (num_bits == 128) {
      assert(v.num_bits == 256);
      /* From 256 bits to 128 */
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u128[0] = v.bits.u256[0];
         new.bits.u128[1] = v.bits.u256[1];
      } else {
         new.bits.u128[0] = v.bits.u256[2];
         new.bits.u128[1] = v.bits.u256[3];
      }
      return new;
   }

   /* Cannot truncate to 256 bits from something larger */
   panic(__func__);
}


/* Helper function to compute left_vbits */
static uint64_t
left64(uint64_t x)
{
   // left(x) = x | -x
   return x | (~x + 1);
}


vbits_t
left_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits >= v.num_bits);

   vbits_t new = { .num_bits = num_bits };

   if (v.num_bits <= 64) {
      uint64_t bits = left64(get_bits64(v));

      switch (num_bits) {
      case 8:   new.bits.u8  = bits & 0xff;   break;
      case 16:  new.bits.u16 = bits & 0xffff; break;
      case 32:  new.bits.u32 = bits & ~0u;    break;
      case 64:  new.bits.u64 = bits & ~0ll;   break;
      case 128:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u128[0] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u128[1] = ~0ull;
            } else {
               new.bits.u128[1] = 0;
            }
         } else {
            new.bits.u128[1] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u128[0] = ~0ull;
            } else {
               new.bits.u128[0] = 0;
            }
         }
         break;
      case 256:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u256[0] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u256[1] = ~0ull;
               new.bits.u256[2] = ~0ull;
               new.bits.u256[3] = ~0ull;
            } else {
               new.bits.u256[1] = 0;
               new.bits.u256[2] = 0;
               new.bits.u256[3] = 0;
            }
         } else {
            new.bits.u256[3] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u256[0] = ~0ull;
               new.bits.u256[1] = ~0ull;
               new.bits.u256[2] = ~0ull;
            } else {
               new.bits.u256[0] = 0;
               new.bits.u256[1] = 0;
               new.bits.u256[2] = 0;
            }
         }
         break;
      default:
         panic(__func__);
      }
      return new;
   }

   if (v.num_bits == 128) {
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         if (v.bits.u128[1] != 0) {
            new.bits.u128[0] = v.bits.u128[0];
            new.bits.u128[1] = left64(v.bits.u128[1]);
         } else {
            new.bits.u128[0] = left64(v.bits.u128[0]);
            if (new.bits.u128[0] & (1ull << 63)) {  // MSB is set
               new.bits.u128[1] = ~0ull;
            } else {
               new.bits.u128[1] = 0;
            }
         }
      } else {
         if (v.bits.u128[0] != 0) {
            new.bits.u128[0] = left64(v.bits.u128[0]);
            new.bits.u128[1] = v.bits.u128[1];
         } else {
            new.bits.u128[1] = left64(v.bits.u128[1]);
            if (new.bits.u128[1] & (1ull << 63)) {  // MSB is set
               new.bits.u128[0] = ~0ull;
            } else {
               new.bits.u128[0] = 0;
            }
         }
      }
      if (num_bits == 128) return new;

      assert(num_bits == 256);

      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         uint64_t b1 = new.bits.u128[1];
         uint64_t b0 = new.bits.u128[0];

         new.bits.u256[0] = b0;
         new.bits.u256[1] = b1;

         if (new.bits.u256[1] & (1ull << 63)) {  // MSB is set
            new.bits.u256[2] = ~0ull;
            new.bits.u256[3] = ~0ull;
         } else {
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         }
      } else {
         uint64_t b1 = new.bits.u128[0];
         uint64_t b0 = new.bits.u128[1];

         new.bits.u256[2] = b0;
         new.bits.u256[3] = b1;

         if (new.bits.u256[2] & (1ull << 63)) {  // MSB is set
            new.bits.u256[0] = ~0ull;
            new.bits.u256[1] = ~0ull;
         } else {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
         }
      }
      return new;
   }

   panic(__func__);
}


vbits_t
or_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   vbits_t new = { .num_bits = v1.num_bits };

   switch (v1.num_bits) {
   case 8:   new.bits.u8  = v1.bits.u8  | v2.bits.u8;  break;
   case 16:  new.bits.u16 = v1.bits.u16 | v2.bits.u16; break;
   case 32:  new.bits.u32 = v1.bits.u32 | v2.bits.u32; break;
   case 64:  new.bits.u64 = v1.bits.u64 | v2.bits.u64; break;
   case 128: new.bits.u128[0] = v1.bits.u128[0] | v2.bits.u128[0];
             new.bits.u128[1] = v1.bits.u128[1] | v2.bits.u128[1];
             break;
   case 256: new.bits.u256[0] = v1.bits.u256[0] | v2.bits.u256[0];
             new.bits.u256[1] = v1.bits.u256[1] | v2.bits.u256[1];
             new.bits.u256[2] = v1.bits.u256[2] | v2.bits.u256[2];
             new.bits.u256[3] = v1.bits.u256[3] | v2.bits.u256[3];
             break;
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
and_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   vbits_t new = { .num_bits = v1.num_bits };

   switch (v1.num_bits) {
   case 8:   new.bits.u8  = v1.bits.u8  & v2.bits.u8;  break;
   case 16:  new.bits.u16 = v1.bits.u16 & v2.bits.u16; break;
   case 32:  new.bits.u32 = v1.bits.u32 & v2.bits.u32; break;
   case 64:  new.bits.u64 = v1.bits.u64 & v2.bits.u64; break;
   case 128: new.bits.u128[0] = v1.bits.u128[0] & v2.bits.u128[0];
             new.bits.u128[1] = v1.bits.u128[1] & v2.bits.u128[1];
             break;
   case 256: new.bits.u256[0] = v1.bits.u256[0] & v2.bits.u256[0];
             new.bits.u256[1] = v1.bits.u256[1] & v2.bits.u256[1];
             new.bits.u256[2] = v1.bits.u256[2] & v2.bits.u256[2];
             new.bits.u256[3] = v1.bits.u256[3] & v2.bits.u256[3];
             break;
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
concat_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   vbits_t new = { .num_bits = v1.num_bits * 2 };

   switch (v1.num_bits) {
   case 8:   new.bits.u16 = (v1.bits.u8 << 8)    | v2.bits.u8;  break;
   case 16:  new.bits.u32 = (v1.bits.u16 << 16)  | v2.bits.u16; break;
   case 32:  new.bits.u64 =  v1.bits.u32;
             new.bits.u64 = (new.bits.u64 << 32) | v2.bits.u32; break;
   case 64:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u128[0] = v2.bits.u64;
         new.bits.u128[1] = v1.bits.u64;
      } else {
         new.bits.u128[0] = v1.bits.u64;
         new.bits.u128[1] = v2.bits.u64;
      }
      break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u256[0] = v2.bits.u128[0];
         new.bits.u256[1] = v2.bits.u128[1];
         new.bits.u256[2] = v1.bits.u128[0];
         new.bits.u256[3] = v1.bits.u128[1];
      } else {
         new.bits.u256[0] = v1.bits.u128[0];
         new.bits.u256[1] = v1.bits.u128[1];
         new.bits.u256[2] = v2.bits.u128[0];
         new.bits.u256[3] = v2.bits.u128[1];
      }
      break;
   case 256: /* Fall through */
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
upper_vbits(vbits_t v)
{
   vbits_t new = { .num_bits = v.num_bits / 2 };

   switch (v.num_bits) {
   case 16:  new.bits.u8  = v.bits.u16 >> 8;  break;
   case 32:  new.bits.u16 = v.bits.u32 >> 16; break;
   case 64:  new.bits.u32 = v.bits.u64 >> 32; break;
   case 128: 
      if (__BYTE_ORDER == __LITTLE_ENDIAN)
         new.bits.u64 = v.bits.u128[1];
      else
         new.bits.u64 = v.bits.u128[0];
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u128[0] = v.bits.u256[2];
         new.bits.u128[1] = v.bits.u256[3];
      } else {
         new.bits.u128[0] = v.bits.u256[0];
         new.bits.u128[1] = v.bits.u256[1];
      }
      break;
   case 8:
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
zextend_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits >= v.num_bits);

   if (num_bits == v.num_bits) return v;

   vbits_t new = { .num_bits = num_bits };

   if (v.num_bits <= 64) {
      uint64_t bits = get_bits64(v);

      switch (num_bits) {
      case 8:   new.bits.u8  = bits; break;
      case 16:  new.bits.u16 = bits; break;
      case 32:  new.bits.u32 = bits; break;
      case 64:  new.bits.u64 = bits; break;
      case 128:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u128[0] = bits;
            new.bits.u128[1] = 0;
         } else {
            new.bits.u128[0] = 0;
            new.bits.u128[1] = bits;
         }
         break;
      case 256:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u256[0] = bits;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = bits;
         }
         break;
      default:
         panic(__func__);
      }
      return new;
   }

   if (v.num_bits == 128) {
      assert(num_bits == 256);

      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u256[0] = v.bits.u128[0];
         new.bits.u256[1] = v.bits.u128[1];
         new.bits.u256[2] = 0;
         new.bits.u256[3] = 0;
      } else {
         new.bits.u256[0] = 0;
         new.bits.u256[1] = 0;
         new.bits.u256[2] = v.bits.u128[1];
         new.bits.u256[3] = v.bits.u128[0];
      }
      return new;
   }

   /* Cannot zero-extend a 256-bit value to something larger */
   panic(__func__);
}


vbits_t
sextend_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits >= v.num_bits);

   int sextend = 0;

   switch (v.num_bits) {
   case 8:   if (v.bits.u8  == 0x80)             sextend = 1; break;
   case 16:  if (v.bits.u16 == 0x8000)           sextend = 1; break;
   case 32:  if (v.bits.u32 == 0x80000000)       sextend = 1; break;
   case 64:  if (v.bits.u64 == (1ull << 63))     sextend = 1; break;
   case 128: if (v.bits.u128[1] == (1ull << 63)) sextend = 1; break;
   case 256: if (v.bits.u256[3] == (1ull << 63)) sextend = 1; break;

   default:
      panic(__func__);
   }

   return sextend ? left_vbits(v, num_bits) : zextend_vbits(v, num_bits);
}


vbits_t
onehot_vbits(unsigned bitno, unsigned num_bits)
{
   assert(bitno < num_bits);

   vbits_t new = { .num_bits = num_bits };

   switch (num_bits) {
   case 1:   new.bits.u32 = 1    << bitno; break;
   case 8:   new.bits.u8  = 1    << bitno; break;
   case 16:  new.bits.u16 = 1    << bitno; break;
   case 32:  new.bits.u32 = 1u   << bitno; break;
   case 64:  new.bits.u64 = 1ull << bitno; break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         if (bitno < 64) {
            new.bits.u128[0] = 1ull << bitno;
            new.bits.u128[1] = 0;
         } else {
            new.bits.u128[0] = 0;
            new.bits.u128[1] = 1ull << (bitno - 64);
         }
      } else {
         if (bitno < 64) {
            new.bits.u128[0] = 0;
            new.bits.u128[1] = 1ull << bitno;
         } else {
            new.bits.u128[0] = 1ull << (bitno - 64);
            new.bits.u128[1] = 0;
         }
      }
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         if (bitno < 64) {
            new.bits.u256[0] = 1ull << bitno;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else if (bitno < 128) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 1ull << (bitno - 64);
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else if (bitno < 192) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 1ull << (bitno - 128);
            new.bits.u256[3] = 0;
         } else {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 1ull << (bitno - 192);
         }
      } else {
         if (bitno < 64) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 1ull << bitno;
         } else if (bitno < 128) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 1ull << (bitno - 64);
            new.bits.u256[3] = 0;
         } else if (bitno < 192) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 1ull << (bitno - 128);
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else {
            new.bits.u256[0] = 1ull << (bitno - 192);
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         }
      }
      break;
   default:
      panic(__func__);
   }
   return new;
}


int
completely_defined_vbits(vbits_t v)
{
   return equal_vbits(v, defined_vbits(v.num_bits));
}


vbits_t
shl_vbits(vbits_t v, unsigned shift_amount)
{
   assert(shift_amount < v.num_bits);

   vbits_t new = v;

   switch (v.num_bits) {
   case 8:  new.bits.u8  <<= shift_amount; break;
   case 16: new.bits.u16 <<= shift_amount; break;
   case 32: new.bits.u32 <<= shift_amount; break;
   case 64: new.bits.u64 <<= shift_amount; break;
   case 128: /* fall through */
   case 256: /* fall through */
   default:
      panic(__func__);
   }
   
   return new;
}


vbits_t
shr_vbits(vbits_t v, unsigned shift_amount)
{
   assert(shift_amount < v.num_bits);

   vbits_t new = v;

   switch (v.num_bits) {
   case 8:  new.bits.u8  >>= shift_amount; break;
   case 16: new.bits.u16 >>= shift_amount; break;
   case 32: new.bits.u32 >>= shift_amount; break;
   case 64: new.bits.u64 >>= shift_amount; break;
   case 128: /* fall through */
   case 256: /* fall through */
   default:
      panic(__func__);
   }
   
   return new;
}


vbits_t
sar_vbits(vbits_t v, unsigned shift_amount)
{
   assert(shift_amount < v.num_bits);

   vbits_t new = v;
   int msb;

   switch (v.num_bits) {
   case 8: 
      new.bits.u8  >>= shift_amount;
      msb = (v.bits.u8 & 0x80) != 0;
      break;
   case 16:
      new.bits.u16 >>= shift_amount;
      msb = (v.bits.u16 & 0x8000) != 0;
      break;
   case 32:
      new.bits.u32 >>= shift_amount;
      msb = (v.bits.u32 & (1u << 31)) != 0;
      break;
   case 64:
      new.bits.u64 >>= shift_amount;
      msb = (v.bits.u64 & (1ull << 63)) != 0;
      break;
   case 128: /* fall through */
   case 256: /* fall through */
   default:
      panic(__func__);
   }

   if (msb)
      new = left_vbits(new, new.num_bits);
   return new;
}

/* Return a value for the POWER Iop_CmpORD class iops */
vbits_t
cmpord_vbits(unsigned v1_num_bits, unsigned v2_num_bits)
{
   vbits_t new = { .num_bits = v1_num_bits };

   /* Size of values being compared must be the same */
   assert( v1_num_bits == v2_num_bits);

   /* Comparison only produces 32-bit or 64-bit value where
    * the lower 3 bits are set to indicate, less than, equal and greater then.
    */
   switch (v1_num_bits) {
   case 32:
      new.bits.u32 = 0xE;
      break;

   case 64:
      new.bits.u64 = 0xE;
      break;

   default:
      panic(__func__);
   }

   return new;
}
