
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"

// I need this to avoid some signedness warnings, not sure why
// #define Char char
// jrs 19 Aug 2005: m_oset.c relies on Char being a signed char.
// It appears that plain 'char' on ppc32 is unsigned and so the
// above #define screws up the AVL tree balancing logic and
// leads to segfaults.  Commenting it out and using the standard
// definition of Char from pub_core_basics.h seems a good solution
// as that has the same signedness on all platforms.

// Crudely redirect various VG_(foo)() functions to their libc equivalents.
#undef vg_assert
#define vg_assert(e)                   assert(e)
#undef vg_assert2
#define vg_assert2(e, fmt, args...)    assert(e)

#define vgPlain_printf                 printf
#define vgPlain_memset                 memset
#define vgPlain_memcpy                 memcpy
#define vgPlain_memmove                memmove

// Crudely replace some functions (in m_xarray.c, but not needed for
// this unit test) by (hopefully) failing asserts.
#define vgPlain_ssort(a,b,c,d) assert(a)
#define vgPlain_vcbprintf(a,b,...) assert(a == b)
#include "coregrind/m_xarray.c"
#undef vgPlain_ssort
#undef vgPlain_vcbprintf
#include "coregrind/m_poolalloc.c"
#include "coregrind/m_oset.c"

#define NN  1000       // Size of OSets being created


/* Consistent random number generator, so it produces the
   same results on all platforms. */

#define random error_do_not_use_libc_random

static UInt seed = 0;
static UInt myrandom( void )
{
  seed = (1103515245 * seed + 12345);
  return seed;
}

static void* allocate_node(const HChar* cc, SizeT szB)
{ return malloc(szB); }

static void free_node(void* p)
{ return free(p); }


//---------------------------------------------------------------------------
// Word example
//---------------------------------------------------------------------------

// This example shows that an element can be a single value (in this
// case a Word), in which case the element is also the key.

__attribute__((unused))
static HChar *wordToStr(void *p)
{
   static HChar buf[32];
   sprintf(buf, "%ld", *(Word*)p);
   return buf;
}

__attribute__((unused))
static Word wordCmp(void* vkey, void* velem)
{
   return *(Word*)vkey - *(Word*)velem;
}

void example1singleset(OSet* oset, char *descr)
{
   Int  i, n;
   UWord v, prev;
   UWord* vs[NN];
   UWord *pv;
   UWord  sorted_elts[NN]; // Used to test VG_(OSetGen_ResetIterAt)

   // Try some operations on an empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSetGen_Contains)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Lookup)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Remove)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Next)(oset) );
   vg_assert( 0 == VG_(OSetGen_Size)(oset) );

   // Create some elements, with gaps (they're all even) but no dups,
   // and shuffle them randomly.
   for (i = 0; i < NN; i++) {
      vs[i] = VG_(OSetGen_AllocNode)(oset, sizeof(Word));
      *(vs[i]) = 2*(i+1);
      sorted_elts[i] = *(vs[i]);
   }
   seed = 0;
   for (i = 0; i < NN; i++) {
      UWord r1  = myrandom() % NN;
      UWord r2  = myrandom() % NN;
      UWord* tmp= vs[r1];
      vs[r1]   = vs[r2];
      vs[r2]   = tmp;
   }

   // Insert the elements
   for (i = 0; i < NN; i++) {
      VG_(OSetGen_Insert)(oset, vs[i]);
   }

   // Check the size
   vg_assert( NN == VG_(OSetGen_Size)(oset) );

   // Check we can find all the elements we inserted
   for (i = 0; i < NN; i++) {
      assert( VG_(OSetGen_Contains)(oset, vs[i]) );
   }

#define FULLCHECKEVERY 20
   // Check VG_(OSetGen_ResetIterAt) works before, at, and after each element.
   // For some elements, we check all the successive elements.
   for (i = 0; i < NN; i++) {
      UWord k;
      UWord *pval;
      Int j;

      // check ResetIterAt just before an elt gives this elt.
      k = sorted_elts[i] - 1;
      VG_(OSetGen_ResetIterAt) (oset, &k);
      // Check all elts till the end
      for (j = i; j < NN; j++) {
         pval = VG_(OSetGen_Next)(oset);
         assert (*pval == sorted_elts[j]);
         if (i % FULLCHECKEVERY != 0) break;
      }

      // check ResetIterAt at an elt gives this elt.
      k = sorted_elts[i];
      VG_(OSetGen_ResetIterAt) (oset, &k);
      // Check all elts till the end
      for (j = i; j < NN; j++) {
         pval = VG_(OSetGen_Next)(oset);
         assert (*pval == sorted_elts[j]);
         if (i % FULLCHECKEVERY != 0) break;
      }

      // check ResetIterAt after an elt gives the next elt or nothing
      // when we reset after the last elt.
      k = sorted_elts[i] + 1;
      VG_(OSetGen_ResetIterAt) (oset, &k);
      if (i < NN - 1) {
         for (j = i+1; j < NN; j++) {
            pval = VG_(OSetGen_Next)(oset);
            assert (*pval == sorted_elts[j]);
            if (i % FULLCHECKEVERY != 0) break;
         }
      } else {
         pval = VG_(OSetGen_Next)(oset);
         assert (pval == NULL);
      }
      
   }

   // Check we cannot find elements we did not insert, below, within (odd
   // numbers), and above the inserted elements.
   v = 0;
   assert( ! VG_(OSetGen_Contains)(oset, &v) );
   for (i = 0; i < NN; i++) {
      v = *(vs[i]) + 1;
      assert( ! VG_(OSetGen_Contains)(oset, &v) );
   }
   v = 2*(NN+1);
   assert( ! VG_(OSetGen_Contains)(oset, &v) );

   // Check we can find all the elements we inserted, and the right values
   // are returned.
   for (i = 0; i < NN; i++) {
      assert( vs[i] == VG_(OSetGen_Lookup)(oset, vs[i]) );
   }

   // Check that we can iterate over the OSet elements in sorted order, and
   // there is the right number of them.
   n = 0;
   pv = NULL;
   prev = 0;
   VG_(OSetGen_ResetIter)(oset);
   while ( (pv = VG_(OSetGen_Next)(oset)) ) {
      UWord curr = *pv;
      assert(prev < curr); 
      prev = curr;
      n++;
   }
   assert(NN == n);
   vg_assert( ! VG_(OSetGen_Next)(oset) );
   vg_assert( ! VG_(OSetGen_Next)(oset) );

   // Check that we can remove half of the elements, and that their values
   // are as expected.
   for (i = 0; i < NN; i += 2) {
      assert( pv = VG_(OSetGen_Remove)(oset, vs[i]) );
      assert( pv == vs[i] );
   }

   // Check the size
   vg_assert( NN/2 == VG_(OSetGen_Size)(oset) );

   // Check we can find the remaining elements (with the right values).
   for (i = 1; i < NN; i += 2) {
      assert( pv = VG_(OSetGen_LookupWithCmp)(oset, vs[i], NULL) );
      assert( pv == vs[i] );
   }

   // Check we cannot find any of the elements we removed.
   for (i = 0; i < NN; i += 2) {
      assert( ! VG_(OSetGen_Contains)(oset, vs[i]) );
   }

   // Check that we can remove the remaining half of the elements, and that
   // their values are as expected.
   for (i = 1; i < NN; i += 2) {
      assert( pv = VG_(OSetGen_Remove)(oset, vs[i]) );
      assert( pv == vs[i] );
   }

   // Try some more operations on the empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSetGen_Contains)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Lookup)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Remove)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Next)(oset) );
   vg_assert( 0 == VG_(OSetGen_Size)(oset) );

   // Free a few elements
   VG_(OSetGen_FreeNode)(oset, vs[0]);
   VG_(OSetGen_FreeNode)(oset, vs[1]);
   VG_(OSetGen_FreeNode)(oset, vs[2]);

   // Re-insert remaining elements, to give OSetGen_Destroy something to
   // work with.
   for (i = 3; i < NN; i++) {
      VG_(OSetGen_Insert)(oset, vs[i]);
   }

   // Print the list
   OSet_Print(oset, descr, wordToStr);

}

void example1(void)
{
   OSet *oset, *oset_empty_clone;

   // Create a static OSet of UWords.  This one uses fast (built-in)
   // comparisons.

   // First a single oset, no pool allocator.
   oset = VG_(OSetGen_Create)(0,
                              NULL,
                              allocate_node, "oset_test.1", free_node);
   example1singleset(oset, "single oset, no pool allocator");

   // Destroy the OSet
   VG_(OSetGen_Destroy)(oset);

   // Then same, but with a group allocator
   oset = VG_(OSetGen_Create_With_Pool)(0,
                                        NULL,
                                        allocate_node, "oset_test.1",
                                        free_node,
                                        101, sizeof(Word));
   example1singleset(oset, "single oset, pool allocator");

   // Destroy the OSet
   VG_(OSetGen_Destroy)(oset);


   // Then two sets, sharing a group allocator.
   oset = VG_(OSetGen_Create_With_Pool)
      (0,
       NULL,
       allocate_node, "oset_test.1", free_node,
       101, sizeof(Word));
   oset_empty_clone = VG_(OSetGen_EmptyClone) (oset);
   example1singleset(oset, "oset, shared pool");
   example1singleset(oset_empty_clone, "cloned oset, shared pool");
   // Destroy both OSet
   
   VG_(OSetGen_Destroy)(oset_empty_clone);
   VG_(OSetGen_Destroy)(oset);
   
}

void example1b(void)
{
   Int  i, n;
   UWord v = 0, prev;
   UWord vs[NN];

   // Create a static OSet of UWords.  This one uses fast (built-in)
   // comparisons.
   OSet* oset = VG_(OSetWord_Create)(allocate_node, "oset_test.2", free_node);

   // Try some operations on an empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSetWord_Contains)(oset, v) );
   vg_assert( ! VG_(OSetWord_Remove)(oset, v) );
   vg_assert( ! VG_(OSetWord_Next)(oset, (UWord *)&v) );
   vg_assert( 0 == VG_(OSetWord_Size)(oset) );

   // Create some elements, with gaps (they're all even) but no dups,
   // and shuffle them randomly.
   for (i = 0; i < NN; i++) {
      vs[i] = 2*i;
   }
   seed = 0;
   for (i = 0; i < NN; i++) {
      UWord r1  = myrandom() % NN;
      UWord r2  = myrandom() % NN;
      UWord tmp = vs[r1];
      vs[r1]   = vs[r2];
      vs[r2]   = tmp;
   }

   // Insert the elements
   for (i = 0; i < NN; i++) {
      VG_(OSetWord_Insert)(oset, vs[i]);
   }

   // Check the size
   vg_assert( NN == VG_(OSetWord_Size)(oset) );

   // Check we can find all the elements we inserted
   for (i = 0; i < NN; i++) {
      assert( VG_(OSetWord_Contains)(oset, vs[i]) );
   }

   // Check we cannot find elements we did not insert, below, within (odd
   // numbers), and above the inserted elements.
   v = 0xffffffff;
   assert( ! VG_(OSetWord_Contains)(oset, v) );
   for (i = 0; i < NN; i++) {
      v = vs[i] + 1;
      assert( ! VG_(OSetWord_Contains)(oset, v) );
   }
   v = NN*2;
   assert( ! VG_(OSetWord_Contains)(oset, v) );

   // Check we can find all the elements we inserted.
   for (i = 0; i < NN; i++) {
      assert( VG_(OSetWord_Contains)(oset, vs[i]) );
   }

   // Check that we can iterate over the OSet elements in sorted order, and
   // there is the right number of them.
   n = 0;
   prev = 0;
   VG_(OSetWord_ResetIter)(oset);
   while ( VG_(OSetWord_Next)(oset, (UWord *)&v) ) {
      UWord curr = v;
      if (n == 0)
         assert(prev == curr); 
      else
         assert(prev < curr); 
      prev = curr;
      n++;
   }
   assert(NN == n);
   vg_assert( ! VG_(OSetWord_Next)(oset, (UWord *)&v) );
   vg_assert( ! VG_(OSetWord_Next)(oset, (UWord *)&v) );

   // Check that we can remove half of the elements.
   for (i = 0; i < NN; i += 2) {
      assert( VG_(OSetWord_Remove)(oset, vs[i]) );
   }

   // Check the size
   vg_assert( NN/2 == VG_(OSetWord_Size)(oset) );

   // Check we can find the remaining elements (with the right values).
   for (i = 1; i < NN; i += 2) {
      assert( VG_(OSetWord_Contains)(oset, vs[i]) );
   }

   // Check we cannot find any of the elements we removed.
   for (i = 0; i < NN; i += 2) {
      assert( ! VG_(OSetWord_Contains)(oset, vs[i]) );
   }

   // Check that we can remove the remaining half of the elements.
   for (i = 1; i < NN; i += 2) {
      assert( VG_(OSetWord_Remove)(oset, vs[i]) );
   }

   // Try some more operations on the empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSetWord_Contains)(oset, v) );
   vg_assert( ! VG_(OSetWord_Remove)(oset, v) );
   vg_assert( ! VG_(OSetWord_Next)(oset, (UWord *)&v) );
   vg_assert( 0 == VG_(OSetWord_Size)(oset) );

   // Re-insert remaining elements, to give OSetWord_Destroy something to
   // work with.
   for (i = 3; i < NN; i++) {
      VG_(OSetWord_Insert)(oset, vs[i]);
   }

   // Print the list
   OSet_Print(oset, "oset1b", wordToStr);

   // Destroy the OSet
   VG_(OSetWord_Destroy)(oset);
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
static HChar *blockToStr(void *p)
{
   static HChar buf[32];
   Block* b = (Block*)p;
   sprintf(buf, "<(%d) %lu..%lu (%d)>", b->b1, b->first, b->last, b->b2);
   return buf;
}

static Word blockCmp(const void* vkey, const void* velem)
{
   Addr   key  = *(const Addr*)vkey;
   const Block* elem = (const Block*)velem;

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
   OSet* oset = VG_(OSetGen_Create)(offsetof(Block, first),
                                    blockCmp,
                                    allocate_node, "oset_test.3", free_node);

   // Try some operations on an empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSetGen_Contains)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Lookup)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Remove)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Next)(oset) );
   vg_assert( 0 == VG_(OSetGen_Size)(oset) );

   // Create some inputs, with gaps -- intervals are 1..3, 11..13, ... -- but
   // no dups, and shuffle them randomly.
   for (i = 0; i < NN; i++) {
      vs[i] = VG_(OSetGen_AllocNode)(oset, sizeof(Block));
      vs[i]->b1    = i;
      vs[i]->first = i*10 + 1;
      vs[i]->last  = vs[i]->first + 2;
      vs[i]->b2    = i+1;
   }
   seed = 0;
   for (i = 0; i < NN; i++) {
      Int r1  = myrandom() % NN;
      Int r2  = myrandom() % NN;
      Block* tmp = vs[r1];
      vs[r1]  = vs[r2];
      vs[r2]  = tmp;
   }

   // Insert the elements
   for (i = 0; i < NN; i++) {
      VG_(OSetGen_Insert)(oset, vs[i]);
   }

   // Check the size
   vg_assert( NN == VG_(OSetGen_Size)(oset) );

   // Check we can find all the elements we inserted, within the full range
   // of each Block.
   for (i = 0; i < NN; i++) {
      a = vs[i]->first + 0;    assert( VG_(OSetGen_Contains)(oset, &a) );
      a = vs[i]->first + 1;    assert( VG_(OSetGen_Contains)(oset, &a) );
      a = vs[i]->first + 2;    assert( VG_(OSetGen_Contains)(oset, &a) );
   }

   // Check we cannot find elements we did not insert, below and above the
   // ranges of the inserted elements.
   a = 0;
   assert( ! VG_(OSetGen_Contains)(oset, &a) );
   for (i = 0; i < NN; i++) {
      a = vs[i]->first - 1;    assert( ! VG_(OSetGen_Contains)(oset, &a) );
      a = vs[i]->first + 3;    assert( ! VG_(OSetGen_Contains)(oset, &a) );
   }

   // Check we can find all the elements we inserted, and the right values
   // are returned.
   for (i = 0; i < NN; i++) {
      a = vs[i]->first + 0;    assert( vs[i] == VG_(OSetGen_Lookup)(oset, &a) );
      a = vs[i]->first + 1;    assert( vs[i] == VG_(OSetGen_Lookup)(oset, &a) );
      a = vs[i]->first + 2;    assert( vs[i] == VG_(OSetGen_Lookup)(oset, &a) );
      assert( vs[i] == VG_(OSetGen_LookupWithCmp)(oset, &a, blockCmp) );
   }

   // Check that we can iterate over the OSet elements in sorted order, and
   // there is the right number of them.
   n = 0;
   pv = NULL;
   prev.last = 0;
   VG_(OSetGen_ResetIter)(oset);
   while ( (pv = VG_(OSetGen_Next)(oset)) ) {
      Block curr = *pv;
      assert(prev.last < curr.first); 
      prev = curr;
      n++;
   }
   assert(NN == n);
   vg_assert( ! VG_(OSetGen_Next)(oset) );
   vg_assert( ! VG_(OSetGen_Next)(oset) );

   // Check that we can remove half of the elements, and that their values
   // are as expected.
   for (i = 0; i < NN; i += 2) {
      a = vs[i]->first;    assert( vs[i] == VG_(OSetGen_Remove)(oset, &a) );
   }

   // Check the size
   vg_assert( NN/2 == VG_(OSetGen_Size)(oset) );

   // Check we can find the remaining elements (with the right values).
   for (i = 1; i < NN; i += 2) {
      a = vs[i]->first + 0;    assert( vs[i] == VG_(OSetGen_Lookup)(oset, &a) );
      a = vs[i]->first + 1;    assert( vs[i] == VG_(OSetGen_Lookup)(oset, &a) );
      a = vs[i]->first + 2;    assert( vs[i] == VG_(OSetGen_Lookup)(oset, &a) );
   }

   // Check we cannot find any of the elements we removed.
   for (i = 0; i < NN; i += 2) {
      a = vs[i]->first + 0;    assert( ! VG_(OSetGen_Contains)(oset, &a) );
      a = vs[i]->first + 1;    assert( ! VG_(OSetGen_Contains)(oset, &a) );
      a = vs[i]->first + 2;    assert( ! VG_(OSetGen_Contains)(oset, &a) );
   }

   // Check that we can remove the remaining half of the elements, and that
   // their values are as expected.
   for (i = 1; i < NN; i += 2) {
      a = vs[i]->first;    assert( vs[i] == VG_(OSetGen_Remove)(oset, &a) );
   }

   // Try some more operations on the empty OSet to ensure they don't screw up.
   vg_assert( ! VG_(OSetGen_Contains)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Lookup)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Remove)(oset, &v) );
   vg_assert( ! VG_(OSetGen_Next)(oset) );
   vg_assert( 0 == VG_(OSetGen_Size)(oset) );

   // Re-insert all elements, to give OSetGen_Destroy something to work with.
   for (i = 0; i < NN; i++) {
      VG_(OSetGen_Insert)(oset, vs[i]);
   }

   // Destroy the OSet
   VG_(OSetGen_Destroy)(oset);
}

//-----------------------------------------------------------------------
// main()
//-----------------------------------------------------------------------

int main(void)
{
   example1();
   example1b();
   example2();
   return 0;
}
