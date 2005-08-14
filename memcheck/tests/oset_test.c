
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"

// I need this to avoid some signedness warnings, not sure why
#define Char char

// Crudely redirect various VG_(foo)() functions to their libc equivalents.
#undef vg_assert
#define vg_assert(e)                   assert(e)
#undef vg_assert2
#define vg_assert2(e, fmt, args...)    assert(e)

#define vgPlain_printf                 printf
#define vgPlain_memset                 memset
#define vgPlain_memcpy                 memcpy
#define vgPlain_random                 random

#include "coregrind/m_oset.c"

#define NN  1000       // Size of OSets being created

//---------------------------------------------------------------------------
// Int example
//---------------------------------------------------------------------------

// This example shows that an element can be a single value (in this case an
// Int), in which case the element is also the key.

__attribute__((unused))
static Char *intToStr(void *p)
{
   static char buf[16];
   sprintf(buf, "%d", *(Int*)p);
   return buf;
}

__attribute__((unused))
static Int intCmp(void* vkey, void* velem)
{
   return *(Int*)vkey - *(Int*)velem;
}

void example1(void)
{
   Int i, v, n, prev;
   Int* vs[NN];
   Int *pv;

   // Create a static OSet of Ints.  This one uses fast (built-in)
   // comparisons.
   OSet* oset1 = VG_(OSet_Create)(0,
                                  NULL,
                                  (void*)malloc, free);

   // Try some operations on an empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSet_Contains)(oset1, &v) );
   vg_assert( ! VG_(OSet_Lookup)(oset1, &v) );
   vg_assert( ! VG_(OSet_Remove)(oset1, &v) );
   vg_assert( ! VG_(OSet_Next)(oset1) );
   vg_assert( 0 == VG_(OSet_Size)(oset1) );

   // Create some elements, with gaps (they're all even) but no dups,
   // and shuffle them randomly.
   for (i = 0; i < NN; i++) {
      vs[i] = VG_(OSet_AllocNode)(oset1, sizeof(Int));
      *(vs[i]) = 2*i;
   }
   for (i = 0; i < NN; i++) {
      Int r1  = random() % NN;
      Int r2  = random() % NN;
      Int* tmp= vs[r1];
      vs[r1]  = vs[r2];
      vs[r2]  = tmp;
   }

   // Insert the elements
   for (i = 0; i < NN; i++) {
      VG_(OSet_Insert)(oset1, vs[i]);
   }

   // Check the size
   vg_assert( NN == VG_(OSet_Size)(oset1) );

   // Check we can find all the elements we inserted
   for (i = 0; i < NN; i++) {
      assert( VG_(OSet_Contains)(oset1, vs[i]) );
   }

   // Check we cannot find elements we did not insert, below, within (odd
   // numbers), and above the inserted elements.
   v = -1;
   assert( ! VG_(OSet_Contains)(oset1, &v) );
   for (i = 0; i < NN; i++) {
      v = *(vs[i]) + 1;
      assert( ! VG_(OSet_Contains)(oset1, &v) );
   }
   v = NN*2;
   assert( ! VG_(OSet_Contains)(oset1, &v) );

   // Check we can find all the elements we inserted, and the right values
   // are returned.
   for (i = 0; i < NN; i++) {
      assert( vs[i] == VG_(OSet_Lookup)(oset1, vs[i]) );
   }

   // Check that we can iterate over the OSet elements in sorted order, and
   // there is the right number of them.
   n = 0;
   pv = NULL;
   prev = -1;
   VG_(OSet_ResetIter)(oset1);
   while ( (pv = VG_(OSet_Next)(oset1)) ) {
      Int curr = *pv;
      assert(prev < curr); 
      prev = curr;
      n++;
   }
   assert(NN == n);
   vg_assert( ! VG_(OSet_Next)(oset1) );
   vg_assert( ! VG_(OSet_Next)(oset1) );

   // Check that we can remove half of the elements, and that their values
   // are as expected.
   for (i = 0; i < NN; i += 2) {
      assert( pv = VG_(OSet_Remove)(oset1, vs[i]) );
      assert( pv == vs[i] );
   }

   // Check the size
   vg_assert( NN/2 == VG_(OSet_Size)(oset1) );

   // Check we can find the remaining elements (with the right values).
   for (i = 1; i < NN; i += 2) {
      assert( pv = VG_(OSet_Lookup)(oset1, vs[i]) );
      assert( pv == vs[i] );
   }

   // Check we cannot find any of the elements we removed.
   for (i = 0; i < NN; i += 2) {
      assert( ! VG_(OSet_Contains)(oset1, vs[i]) );
   }

   // Check that we can remove the remaining half of the elements, and that
   // their values are as expected.
   for (i = 1; i < NN; i += 2) {
      assert( pv = VG_(OSet_Remove)(oset1, vs[i]) );
      assert( pv == vs[i] );
   }

   // Try some more operations on the empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSet_Contains)(oset1, &v) );
   vg_assert( ! VG_(OSet_Lookup)(oset1, &v) );
   vg_assert( ! VG_(OSet_Remove)(oset1, &v) );
   vg_assert( ! VG_(OSet_Next)(oset1) );
   vg_assert( 0 == VG_(OSet_Size)(oset1) );

   // Free a few elements
   VG_(OSet_FreeNode)(oset1, vs[0]);
   VG_(OSet_FreeNode)(oset1, vs[1]);
   VG_(OSet_FreeNode)(oset1, vs[2]);

   // Re-insert remaining elements, to give OSet_Destroy something to work with.
   for (i = 3; i < NN; i++) {
      VG_(OSet_Insert)(oset1, vs[i]);
   }

   // Print the list
   OSet_Print(oset1, "foo", intToStr);

   // Destroy the OSet
   VG_(OSet_Destroy)(oset1);
}


//---------------------------------------------------------------------------
// Struct example
//---------------------------------------------------------------------------

// This element shows that a key can be in the middle of the element, and
// be of arbitrary size and even span multiple (contiguous) fields.  It
// also demonstrates how an OSet can be used to implement a list of
// non-overlapping intervals.

typedef struct {
   Int   b1;
   Addr  first;
   Addr  last;
   Int   b2;
}
Block;

__attribute__((unused))
static Char *blockToStr(void *p)
{
   static char buf[32];
   Block* b = (Block*)p;
   sprintf(buf, "<(%d) %lu..%lu (%d)>", b->b1, b->first, b->last, b->b2);
   return buf;
}

static Int blockCmp(void* vkey, void* velem)
{
   Addr   key  = *(Addr*)vkey;
   Block* elem = (Block*)velem;

   assert(elem->first <= elem->last);
   if (key < elem->first) return -1;
   if (key > elem->last)  return  1;
   return 0;
}

void example2(void)
{
   Int i, n;
   Addr a;
   Block* vs[NN];
   Block v, prev;
   Block *pv;

   // Create a dynamic OSet of Blocks.  This one uses slow (custom)
   // comparisons.
   OSet* oset2 = VG_(OSet_Create)(offsetof(Block, first),
                                  blockCmp,
                                  (void*)malloc, free);

   // Try some operations on an empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSet_Contains)(oset2, &v) );
   vg_assert( ! VG_(OSet_Lookup)(oset2, &v) );
   vg_assert( ! VG_(OSet_Remove)(oset2, &v) );
   vg_assert( ! VG_(OSet_Next)(oset2) );
   vg_assert( 0 == VG_(OSet_Size)(oset2) );

   // Create some inputs, with gaps -- intervals are 1..3, 11..13, ... -- but
   // no dups, and shuffle them randomly.
   for (i = 0; i < NN; i++) {
      vs[i] = VG_(OSet_AllocNode)(oset2, sizeof(Block));
      vs[i]->b1    = i;
      vs[i]->first = i*10 + 1;
      vs[i]->last  = vs[i]->first + 2;
      vs[i]->b2    = i+1;
   }
   for (i = 0; i < NN; i++) {
      Int r1  = random() % NN;
      Int r2  = random() % NN;
      Block* tmp = vs[r1];
      vs[r1]  = vs[r2];
      vs[r2]  = tmp;
   }

   // Insert the elements
   for (i = 0; i < NN; i++) {
      VG_(OSet_Insert)(oset2, vs[i]);
   }

   // Check the size
   vg_assert( NN == VG_(OSet_Size)(oset2) );

   // Check we can find all the elements we inserted, within the full range
   // of each Block.
   for (i = 0; i < NN; i++) {
      a = vs[i]->first + 0;    assert( VG_(OSet_Contains)(oset2, &a) );
      a = vs[i]->first + 1;    assert( VG_(OSet_Contains)(oset2, &a) );
      a = vs[i]->first + 2;    assert( VG_(OSet_Contains)(oset2, &a) );
   }

   // Check we cannot find elements we did not insert, below and above the
   // ranges of the inserted elements.
   a = 0;
   assert( ! VG_(OSet_Contains)(oset2, &a) );
   for (i = 0; i < NN; i++) {
      a = vs[i]->first - 1;    assert( ! VG_(OSet_Contains)(oset2, &a) );
      a = vs[i]->first + 3;    assert( ! VG_(OSet_Contains)(oset2, &a) );
   }

   // Check we can find all the elements we inserted, and the right values
   // are returned.
   for (i = 0; i < NN; i++) {
      a = vs[i]->first + 0;    assert( vs[i] == VG_(OSet_Lookup)(oset2, &a) );
      a = vs[i]->first + 1;    assert( vs[i] == VG_(OSet_Lookup)(oset2, &a) );
      a = vs[i]->first + 2;    assert( vs[i] == VG_(OSet_Lookup)(oset2, &a) );
   }

   // Check that we can iterate over the OSet elements in sorted order, and
   // there is the right number of them.
   n = 0;
   pv = NULL;
   prev.last = 0;
   VG_(OSet_ResetIter)(oset2);
   while ( (pv = VG_(OSet_Next)(oset2)) ) {
      Block curr = *pv;
      assert(prev.last < curr.first); 
      prev = curr;
      n++;
   }
   assert(NN == n);
   vg_assert( ! VG_(OSet_Next)(oset2) );
   vg_assert( ! VG_(OSet_Next)(oset2) );

   // Check that we can remove half of the elements, and that their values
   // are as expected.
   for (i = 0; i < NN; i += 2) {
      a = vs[i]->first;    assert( vs[i] == VG_(OSet_Remove)(oset2, &a) );
   }

   // Check the size
   vg_assert( NN/2 == VG_(OSet_Size)(oset2) );

   // Check we can find the remaining elements (with the right values).
   for (i = 1; i < NN; i += 2) {
      a = vs[i]->first + 0;    assert( vs[i] == VG_(OSet_Lookup)(oset2, &a) );
      a = vs[i]->first + 1;    assert( vs[i] == VG_(OSet_Lookup)(oset2, &a) );
      a = vs[i]->first + 2;    assert( vs[i] == VG_(OSet_Lookup)(oset2, &a) );
   }

   // Check we cannot find any of the elements we removed.
   for (i = 0; i < NN; i += 2) {
      a = vs[i]->first + 0;    assert( ! VG_(OSet_Contains)(oset2, &a) );
      a = vs[i]->first + 1;    assert( ! VG_(OSet_Contains)(oset2, &a) );
      a = vs[i]->first + 2;    assert( ! VG_(OSet_Contains)(oset2, &a) );
   }

   // Check that we can remove the remaining half of the elements, and that
   // their values are as expected.
   for (i = 1; i < NN; i += 2) {
      a = vs[i]->first;    assert( vs[i] == VG_(OSet_Remove)(oset2, &a) );
   }

   // Try some more operations on the empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSet_Contains)(oset2, &v) );
   vg_assert( ! VG_(OSet_Lookup)(oset2, &v) );
   vg_assert( ! VG_(OSet_Remove)(oset2, &v) );
   vg_assert( ! VG_(OSet_Next)(oset2) );
   vg_assert( 0 == VG_(OSet_Size)(oset2) );

   // Re-insert all elements, to give OSet_Destroy something to work with.
   for (i = 0; i < NN; i++) {
      VG_(OSet_Insert)(oset2, vs[i]);
   }

   // Destroy the OSet
   VG_(OSet_Destroy)(oset2);
}

//-----------------------------------------------------------------------
// main()
//-----------------------------------------------------------------------

int main(void)
{
   example1();
   example2();
   return 0;
}
