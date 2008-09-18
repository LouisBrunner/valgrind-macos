
0. CONTENTS
~~~~~~~~~~~

This document introduces Ptrcheck, a new, experimental Valgrind tool.
It contains the following sections:

   1. INTRODUCING PTRCHECK
   2. HOW TO RUN IT
   3. HOW IT WORKS: HEAP CHECKING
   4. HOW IT WORKS: STACK & GLOBAL CHECKING
   5. COMPARISON WITH MEMCHECK
   6. LIMITATIONS
   7. STILL TO DO -- User visible things
   8. STILL TO DO -- Implementation tidying



1. INTRODUCING PTRCHECK
~~~~~~~~~~~~~~~~~~~~~~~

Ptrcheck is a Valgrind tool for finding overruns of heap, stack and
global arrays.  Its functionality overlaps somewhat with Memcheck's,
but it is able to catch invalid accesses in a number of cases that
Memcheck would miss.  A detailed comparison against Memcheck is
presented below.

Ptrcheck is composed of two almost completely independent tools that
have been glued together.  One part, in h_main.[ch], checks accesses
through heap-derived pointers.  The other part, in sg_main.[ch],
checks accesses to stack and global arrays.  The remaining files
pc_{common,main}.[ch], provide common error-management and
coordination functions, so as to make it appear as a single tool.

The heap-check part is an extensively-hacked (largely rewritten)
version of the experimental "Annelid" tool developed and described by
Nicholas Nethercote and Jeremy Fitzhardinge.  The stack- and global-
check part uses a heuristic approach derived from an observation about
the likely forms of stack and global array accesses, and, as far as is
known, is entirely novel.



2. HOW TO RUN IT
~~~~~~~~~~~~~~~~

valgrind --tool=exp-ptrcheck [myprog] [args for myprog]

There are no Ptrcheck specific flags at present.



3. HOW IT WORKS: HEAP CHECKING
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ptrcheck can check for invalid uses of heap pointers, including out of
range accesses and accesses to freed memory.  The mechanism is however
completely different from Memcheck's, and the checking is more
powerful.

For each pointer in the program, Ptrcheck keeps track of which heap
block (if any) it was derived from.  Then, when an access is made
through that pointer, Ptrcheck compares the access address with the
bounds of the associated block, and reports an error if the address is
out of bounds, or if the block has been freed.

Of course it is rarely the case that one wants to access a block only
at the exact address returned by malloc (et al).  Ptrcheck understands
that adding or subtracting offsets from a pointer to a block results
in a pointer to the same block.

At a fundamental level, this scheme works because a correct program
cannot make assumptions about the addresses returned by malloc.  In
particular it cannot make any assumptions about the differences in
addresses returned by subsequent calls to malloc.  Hence there are
very few ways to take an address returned by malloc, modify it, and
still have a valid address.  In short, the only allowable operations
are adding and subtracting other non-pointer values.  Almost all other
operations produce a value which cannot possibly be a valid pointer.



4. HOW IT WORKS: STACK & GLOBAL CHECKING
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a source file is compiled with "-g", the compiler attaches Dwarf3
debugging information which describes the location of all stack and
global arrays in the file.

Checking of accesses to such arrays would then be relatively simple,
if the compiler could also tell us which array (if any) each memory
referencing instruction was supposed to access.  Unfortunately the
Dwarf3 debugging format does not provide a way to represent such
information, so we have to resort to a heuristic technique to
approximate the same information.  The key observation is that

   if a memory referencing instruction accesses inside a stack or
   global array once, then it is highly likely to always access that
   same array

To see how this might be useful, consider the following buggy
fragment:

   { int i, a[10];  // both are auto vars
     for (i = 0; i <= 10; i++)
        a[i] = 42;
   }

At run time we will know the precise address of a[] on the stack, and
so we can observer that the first store resulting from "a[i] = 42"
writes a[], and we will (correctly) assume that that instruction is
intended always to access a[].  Then, on the 11th iteration, it
accesses somewhere else, possibly a different local, possibly an
un-accounted for area of the stack (eg, spill slot), so Ptrcheck
reports an error.

There is an important caveat.

Imagine a function such as memcpy, which is used to read and write
many different areas of memory over the lifetime of the program.  If
we insist that the read and write instructions in its memory copying
loop only ever access one particular stack or global variable, we will
be flooded with errors resulting from calls to memcpy.

To avoid this problem, Ptrcheck instantiates fresh likely-target
records for each entry to a function, and discards them on exit.  This
allows detection of cases where (eg) memcpy overflows its source or
destination buffers for any specific call, but does not carry any
restriction from one call to the next.  Indeed, multiple threads may
be multiple simultaneous calls to (eg) memcpy without mutual
interference.



5. COMPARISON WITH MEMCHECK
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Memcheck does not do any access checks for stack or global arrays, so
the presence of those in Ptrcheck is a straight win.  (But see
LIMITATIONS below).

Memcheck and Ptrcheck use different approaches for checking heap
accesses.  Memcheck maintains bitmaps telling it which areas of memory
are accessible and which are not.  If a memory access falls in an
unaccessible area, it reports an error.  By marking the 16 bytes
before and after an allocated block unaccessible, Memcheck is able to
detect small over- and underruns of the block.  Similarly, by marking
freed memory as unaccessible, Memcheck can detect all accesses to
freed memory.

Memcheck's approach is simple.  But it's also weak.  It can't catch
block overruns beyond 16 bytes.  And, more generally, because it
focusses only on the question "is the target address accessible", it
fails to detect invalid accesses which just happen to fall within some
other valid area.  This is not improbable, especially in crowded areas
of the process' address space.

Ptrcheck's approach is to keep track of pointers derived from heap
blocks.  It tracks pointers which are derived directly from calls to
malloc et al, but also ones derived indirectly, by adding or
subtracting offsets from the directly-derived pointers.  When a
pointer is finally used to access memory, Ptrcheck compares the access
address with that of the block it was originally derived from, and
reports an error if the access address is not within the block bounds.

Consequently Ptrcheck can detect any out of bounds access through a
heap-derived pointer, no matter how far from the original block it is.

A second advantage is that Ptrcheck is better at detecting accesses to
blocks freed very far in the past.  Memcheck can detect these too, but
only for blocks freed relatively recently.  To detect accesses to a
freed block, Memcheck must make it inaccessible, hence requiring a
space overhead proportional to the size of the block.  If the blocks
are large, Memcheck will have to make them available for re-allocation
relatively quickly, thereby losing the ability to detect invalid
accesses to them.

By contrast, Ptrcheck has a constant per-block space requirement of
four machine words, for detection of accesses to freed blocks.  A
freed block can be reallocated immediately, yet Ptrcheck can still
detect all invalid accesses through any pointers derived from the old
allocation, providing only that the four-word descriptor for the old
allocation is stored.  For example, on a 64-bit machine, to detect
accesses in any of the most recently freed 10 million blocks, Ptrcheck
will require only 320MB of extra storage.  Achieveing the same level
of detection with Memcheck is close to impossible and would likely
involve several gigabytes of extra storage.

In defense of Memcheck ...

Remember that Memcheck performs uninitialised value checking, which
Ptrcheck does not.  Memcheck has also benefitted from years of
refinement, tuning, and experience with production-level usage, and so
is much faster than Ptrcheck as it currently stands, as of September
2008.

Consequently it is recommended to first make your programs run
Memcheck clean.  Once that's done, try Ptrcheck to see if you can
shake out any further heap, global or stack errors.



6. LIMITATIONS
~~~~~~~~~~~~~~

This is an experimental tool, which relies rather too heavily on some
not-as-robust-as-I-would-like assumptions on the behaviour of correct
programs.  There are a number of limitations which you should be aware
of.

* Heap checks: Ptrcheck can occasionally lose track of, or become
  confused about, which heap block a given pointer has been derived
  from.  This can cause it to falsely report errors, or to miss some
  errors.  This is not believed to be a serious problem.

* Heap checks: Ptrcheck only tracks pointers that are stored properly
  aligned in memory.  If a pointer is stored at a misaligned address,
  and then later read again, Ptrcheck will lose track of what it
  points at.  Similar problem if a pointer is split into pieces and
  later reconsitituted.

* Heap checks: Ptrcheck needs to "understand" which system calls
  return pointers and which don't.  Many, but not all system calls are
  handled.  If an unhandled one is encountered, Ptrcheck will abort.

* Stack checks: It follows from the description above (HOW IT WORKS:
  STACK & GLOBAL CHECKING) that the first access by a memory
  referencing instruction to a stack or global array creates an
  association between that instruction and the array, which is checked
  on subsequent accesses by that instruction, until the containing
  function exits.  Hence, the first access by an instruction to an
  array (in any given function instantiation) is not checked for
  overrun, since Ptrcheck uses that as the "example" of how subsequent
  accesses should behave.

* Stack checks: Similarly, and more serious, it is clearly possible to
  write legitimate pieces of code which break the basic assumption
  upon which the stack/global checking rests.  For example:

  { int a[10], b[10], *p, i;
    for (i = 0; i < 10; i++) {
       p = /* arbitrary condition */  ? &a[i]  : &b[i];
       *p = 42;
    }
  }

  In this case the store sometimes accesses a[] and sometimes b[], but
  in no cases is the addressed array overrun.  Nevertheless the change
  in target will cause an error to be reported.

  It is hard to see how to get around this problem.  The only
  mitigating factor is that such constructions appear very rare, at
  least judging from the results using the tool so far.  Such a
  construction appears only once in the Valgrind sources (running
  Valgrind on Valgrind) and perhaps two or three times for a start and
  exit of Firefox.  The best that can be done is to suppress the
  errors.

* Performance: the stack/global checks require reading all of the
  Dwarf3 type and variable information on the executable and its
  shared objects.  This is computationally expensive and makes startup
  quite slow.  You can expect debuginfo reading time to be in the
  region of a minute for an OpenOffice sized application, on a 2.4 GHz
  Core 2 machine.  Reading this information also requires a lot of
  memory.  To make it viable, Ptrcheck goes to considerable trouble to
  compress the in-memory representation of the Dwarf3 data, which is
  why the process of reading it appears slow.

* Performance: Ptrcheck runs slower than Memcheck.  This is partly due
  to a lack of tuning, but partly due to algorithmic difficulties.
  The heap-check side is potentially quite fast.  The stack and global
  checks can sometimes require a number of range checks per memory
  access, and these are difficult to short-circuit (despite
  considerable efforts having been made).

* Coverage: the heap checking is relatively robust, requiring only
  that Ptrcheck can see calls to malloc/free et al.  In that sense it
  has debug-info requirements comparable with Memcheck, and is able to
  heap-check programs even with no debugging information attached.

  Stack/global checking is much more fragile.  If a shared object does
  not have debug information attached, then Ptrcheck will not be able
  to determine the bounds of any stack or global arrays defined within
  that shared object, and so will not be able to check accesses to
  them.  This is true even when those arrays are accessed from some
  other shared object which was compiled with debug info.

  At the moment Ptrcheck accepts objects lacking debuginfo without
  comment.  This is dangerous as it causes Ptrcheck to silently skip
  stack & global checking for such objects.  It would be better to
  print a warning in such circumstances.

* Coverage: Ptrcheck checks that the areas read or written by system
  calls do not overrun heap blocks.  But it doesn't currently check
  them for overruns stack and global arrays.  This would be easy to
  add.

* Performance: for implementation reasons, system call checking has a
  cost proportional to the number of live and freed heap blocks being
  tracked, and so can be very expensive.  This is stupid and could
  easily be fixed (see "STILL TO DO -- User visible things" below).

* Platforms: the stack/global checks won't work properly on any
  PowerPC platforms, only on x86 and amd64 targets.  That's because
  the stack and global checking requires tracking function calls and
  exits reliably, and there's no obvious way to do it with the PPC
  ABIs.  (cf with the x86 and amd64 ABIs this is relatively
  straightforward.)

* Robustness: related to the previous point.  Function call/exit
  tracking for x86/amd64 is believed to work properly even in the
  presence of longjmps within the same stack (although this has not
  been tested).  However, code which switches stacks is likely to
  cause breakage/chaos.


7. STILL TO DO -- User visible things
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Extend system call checking to work on stack and global arrays

* Fix big performance problem to do with heap-vs-syscall checking.
  How: in h_main.c: get rid of get_Seg_containing_addr_SLOW and
  implement the same by doing a search in addr_to_seg_map.  This would
  fix the heap-vs-syscall performance problem noted above.

* Print a warning if a shared object does not have debug info attached



8. STILL TO DO -- Implementation tidying
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Items marked CRITICAL are considered important for correctness:
non-fixage of them is liable to lead to crashes or assertion failures
in real use.

* h_main.c: make N_FREED_SEGS command-line configurable

* Maybe add command line options to enable only heap checking, or only
  stack/global checking

* sg_main.c: Improve the performance of the stack / global checks by
  doing some up-front filtering to ignore references in areas which
  "obviously" can't be stack or globals.  This will require
  using information that m_aspacemgr knows about the address space
  layout.

* h_main.c: get rid of the last_seg_added hack; add suitable plumbing
  to the core/tool interface to do this cleanly

* h_main.c: move vast amounts of arch-dependent uglyness
  (get_IntRegInfo et al) to its own source file, a la mc_machine.c.

* h_main.c: make the lossage-check stuff work again, as a way of doing
  quality assurance on the implementation

* h_main.c: schemeEw_Atom: don't generate a call to nonptr_or_unknown,
  this is really stupid, since it could be done at translation time
  instead

* CRITICAL: h_main.c: h_instrument (main instrumentation fn): generate
  shadows for word-sized temps defined in the block's preamble.  (Why
  does this work at all, as it stands?)

* CRITICAL: sg_main.c: make preen_Invar work properly again.  Why
  isn't it being called?

* sg_main.c: fix compute_II_hash to make it a bit more sensible
  for ppc32/64 targets
