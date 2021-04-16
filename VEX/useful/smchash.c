
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef  signed int              Int;
typedef  unsigned int            UInt;
typedef  unsigned long long int  Addr64;
typedef  unsigned char           UChar;
typedef  unsigned long int       UWord;

static inline UInt ROL32 ( UInt x, UInt n ) {
  assert(n != 0);
  n &= 31;
  x = (x << n) | (x >> (32-n));
  return x;
}

//////////////////////////////////////////////////////////

typedef
   struct {
      Addr64 ga;
      Int    nbytes;
      UChar* bytes;
      UChar* actual; 
   }
   GuestBytes;

GuestBytes* read_one ( FILE* f )
{
  Int r;
  UInt i;
  UInt esum, csum;

  if (feof(f)) return NULL;
  assert(!ferror(f));

  GuestBytes* gb = malloc(sizeof(GuestBytes));
  assert(gb);

  r= fscanf(f, "GuestBytes %llx %d  ", &gb->ga, &gb->nbytes);
  if (0) printf("r = %d\n", r);
  assert(r == 2);

  assert(gb->ga != 0);
  assert(gb->nbytes > 0);
  assert(gb->nbytes < 5000); // let's say

  Int nToAlloc = gb->nbytes + (gb->ga & 3);

  gb->bytes  = malloc( gb->nbytes + nToAlloc);
  gb->actual = gb->bytes + (gb->ga & 3);
  assert(gb->bytes);

  csum = 0;
  for (i = 0; i < gb->nbytes; i++) {
    UInt b;
    r= fscanf(f, "%02x ", &b);
    assert(r == 1);
    gb->actual[i] = b;
    csum = (csum << 1) ^ b;
  }

  r= fscanf(f, " %08x\n", &esum);
  assert(r == 1);

  assert(esum == csum);

  return gb;
}

//////////////////////////////////////////////////////////

void apply_to_all ( FILE* f, 
                    void(*fn)( GuestBytes*, void* ),
                    void* opaque )
{
  while (!feof(f)) {
    GuestBytes* gb = read_one(f);
    if (0) printf("got %llu %d\n", gb->ga, gb->nbytes);
    fn( gb, opaque );
    free(gb->bytes);
    free(gb);
  }
}

//////////////////////////////////////////////////////////

UInt hash_const_zero ( GuestBytes* gb ) {
  return 0;
}

UInt hash_sum ( GuestBytes* gb ) {
  UInt i, sum = 0;
  for (i = 0; i < gb->nbytes; i++)
    sum += (UInt)gb->actual[i];
  return sum;
}

UInt hash_rol ( GuestBytes* gb ) {
  UInt i, sum = 0;
  for (i = 0; i < gb->nbytes; i++) {
    sum ^= (UInt)gb->actual[i];
    sum = ROL32(sum,7);
  }
  return sum;
}

static UInt cand0 ( GuestBytes* gb )
{
   UWord addr = (UWord)gb->actual;
   UWord len = gb->nbytes;
   UInt sum = 0;
   /* pull up to 4-alignment */
   while ((addr & 3) != 0 && len >= 1) {
      UChar* p = (UChar*)addr;
      sum = (sum << 8) | (UInt)p[0];
      addr++;
      len--;
   }
   /* vectorised + unrolled */
   while (len >= 16) {
      UInt* p = (UInt*)addr;
      sum = ROL32(sum ^ p[0], 13);
      sum = ROL32(sum ^ p[1], 13);
      sum = ROL32(sum ^ p[2], 13);
      sum = ROL32(sum ^ p[3], 13);
      addr += 16;
      len -= 16;
   }
   /* vectorised fixup */
   while (len >= 4) {
      UInt* p = (UInt*)addr;
      sum = ROL32(sum ^ p[0], 13);
      addr += 4;
      len -= 4;
   }
   /* scalar fixup */
   while (len >= 1) {
      UChar* p = (UChar*)addr;
      sum = ROL32(sum ^ (UInt)p[0], 19);
      addr++;
      len--;
   }
   return sum;
}

static UInt cand1 ( GuestBytes* gb )
{
   UWord addr = (UWord)gb->actual;
   UWord len = gb->nbytes;
   UInt sum1 = 0, sum2 = 0;
   /* pull up to 4-alignment */
   while ((addr & 3) != 0 && len >= 1) {
      UChar* p = (UChar*)addr;
      sum1 = (sum1 << 8) | (UInt)p[0];
      addr++;
      len--;
   }
   /* vectorised + unrolled */
   while (len >= 16) {
      UInt* p = (UInt*)addr;
      UInt  w;
      w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr += 16;
      len  -= 16;
      sum1 ^= sum2;
   }
   /* vectorised fixup */
   while (len >= 4) {
      UInt* p = (UInt*)addr;
      UInt  w = p[0];
      sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr += 4;
      len  -= 4;
      sum1 ^= sum2;
   }
   /* scalar fixup */
   while (len >= 1) {
      UChar* p = (UChar*)addr;
      UInt   w = (UInt)p[0];
      sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr++;
      len--;
   }
   return sum1 + sum2;
}
 
static UInt adler32 ( GuestBytes* gb )
{
   UWord addr = (UWord)gb->actual;
   UWord len = gb->nbytes;
   UInt   s1 = 1;
   UInt   s2 = 0;
   UChar* buf = (UChar*)addr;
   while (len >= 4) {
      s1 += buf[0];
      s2 += s1;
      s1 += buf[1];
      s2 += s1;
      s1 += buf[2];
      s2 += s1;
      s1 += buf[3];
      s2 += s1;
      buf += 4;
      len -= 4;
   }
   while (len > 0) {
      s1 += buf[0];
      s2 += s1;
      len--;
      buf++;
   }
   return (s2 << 16) + s1;
}




//////////////////////////////////////////////////////////

UInt (*theFn)(GuestBytes*) =
  //hash_const_zero;
  //hash_sum;
//hash_rol;
//cand0;
  cand1;
  //adler32;

Int cmp_UInt_ps ( UInt* p1, UInt* p2 ) {
  if (*p1 < *p2) return -1;
  if (*p1 > *p2) return 1;
  return 0;
}

Int nSetBits ( UInt w )
{
  Int i, j;
  j = 0;
  for (i = 0; i < 32; i++)
    if (w & (1<<i))
      j++;
  return j;
}

Int    toc_nblocks           = 0;
Int    toc_nblocks_with_zero = 0;
double toc_sum_of_avgs       = 0.0;

void invertBit ( UChar* b, UInt ix, UInt bix ) {
   b[ix] ^= (1 << bix);
}

void try_onebit_changes( GuestBytes* gb, void* opaque )
{
   toc_nblocks++;
   /* collect up the hash values for all one bit changes of the key,
      and also that for the unmodified key.  Then compute the number
      of changed bits for all of them. */
   UInt  hashIx  = 0;
   UInt  nHashes = 8 * gb->nbytes;
   UInt* hashes  = malloc( nHashes * sizeof(UInt) );

   UInt byteIx, bitIx;
   UInt hInit, hFinal, hRunning;
   Int dist, totDist = 0, nNoDist = 0;
   assert(hashes);
   hInit = theFn( gb );
    for (byteIx = 0; byteIx < gb->nbytes; byteIx++) {
      for (bitIx = 0; bitIx < 8; bitIx++) {

         invertBit(gb->actual, byteIx, bitIx);
         //invertBit(gb->actual, byteIx, bitIx ^ 4);

         hRunning = theFn( gb );

         dist = nSetBits(hRunning ^ hInit);
         totDist += dist;
         if (dist == 0) nNoDist++;

         hashes[hashIx++] = hRunning;

         invertBit(gb->actual, byteIx, bitIx);
         //invertBit(gb->actual, byteIx, bitIx ^ 4);

         if (0) printf("  %02d.%d  %08x  %d\n", 
                       byteIx, bitIx, hRunning ^ hInit, dist);
      }
   }
   hFinal = theFn( gb );
   assert(hFinal == hInit);
   assert(hashIx == nHashes);

   if (nNoDist > 0) 
      printf("%4d  measurements,  %5.2f avg dist,  %2d zeroes\n",
             (Int)nHashes, (double)totDist / (double)nHashes,  nNoDist);
   else
      printf("%4d  measurements,  %5.2f avg dist\n",
             (Int)nHashes, (double)totDist / (double)nHashes);

   if (nNoDist > 0)
      toc_nblocks_with_zero++;

   toc_sum_of_avgs += (double)totDist / (double)nHashes;

   free(hashes);
}

//////////////////////////////////////////////////////////

int main ( void )
{
  FILE* f = stdin;
  apply_to_all(f, try_onebit_changes, NULL);
  printf("\n%d blocks,  %d with a zero,  %5.2f avg avg\n\n",
         toc_nblocks, toc_nblocks_with_zero, toc_sum_of_avgs / (double)toc_nblocks );
  return 0;
}

//////////////////////////////////////////////////////////
