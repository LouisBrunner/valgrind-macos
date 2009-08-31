
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

  GuestBytes* gb = malloc(sizeof(GuestBytes));
  assert(gb);

  if (feof(f)) return NULL;
  assert(!ferror(f));

  r= fscanf(f, "GuestBytes %llx %d  ", &gb->ga, &gb->nbytes);
  printf("r = %d\n", r);
  assert(r == 2);

  assert(gb->ga != 0);
  assert(gb->nbytes > 0);
  assert(gb->nbytes < 500); // let's say

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
    printf("got %llu %d\n", gb->ga, gb->nbytes);
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
 


//////////////////////////////////////////////////////////

UInt (*theFn)(GuestBytes*) =
  //hash_const_zero;
  //hash_sum;
  //hash_rol;
  cand0;

Int cmp_UInt_ps ( UInt* p1, UInt* p2 ) {
  if (*p1 < *p2) return -1;
  if (*p1 > *p2) return 1;
  return 0;
}

void try_onebit_changes( GuestBytes* gb, void* opaque )
{
   /* collect up the hash values for all one bit changes of the key,
      and also that for the unmodified key.  Then sort and see if we
      have any dups. */
   UInt  hashIx  = 0;
   UInt  nHashes = 1 + 8 * gb->nbytes;
   UInt* hashes  = malloc( nHashes * sizeof(UInt) );

   UInt byteIx, bitIx;
   UInt hInit, hFinal, hRunning;
   assert(hashes);
   hInit = theFn( gb );
   hashes[hashIx++] = hInit;
    for (byteIx = 0; byteIx < gb->nbytes; byteIx++) {
      for (bitIx = 0; bitIx < 8; bitIx++) {
         gb->actual[byteIx] ^= (1 << bitIx);
         hRunning = theFn( gb );
         hashes[hashIx++] = hRunning;
         gb->actual[byteIx] ^= (1 << bitIx);
         printf("  %02d.%d  %08x\n", byteIx, bitIx, hRunning);
      }
   }
   hFinal = theFn( gb );
   assert(hFinal == hInit);
   assert(hashIx == nHashes);

   qsort( hashes, nHashes, sizeof(UInt), cmp_UInt_ps );



   free(hashes);
}

//////////////////////////////////////////////////////////

int main ( void )
{
  FILE* f = stdin;
  apply_to_all(f, try_onebit_changes, NULL);
  return 0;
}

//////////////////////////////////////////////////////////
