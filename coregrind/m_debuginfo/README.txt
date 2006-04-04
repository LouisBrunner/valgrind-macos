
On 4 Apr 06, the debuginfo reader (m_debuginfo) was majorly cleaned up
and restructured.  It has been a bit of a tangle for a while.  The new
structure looks like this:

                  debuginfo.c 
  
                   readelf.c

        readdwarf.c        readstabs.c

                   storage.c

Each .c can only call those below it on the page.

storage.c contains the SegInfo structure and stuff for 
maintaining/searching arrays of symbols, line-numbers, and Dwarf CF 
info records.

readdwarf.c and readstabs.c parse the relevant kind of info and 
call storage.c to store the results.

readelf.c reads ELF format, hands syms directly to storage.c,
then delegates to readdwarf.c/readstabs.c for debug info.  All 
straightforward.

debuginfo.c is the top-level file, and is quite small.

There are 3 goals to this:

(1) Generally tidy up something which needs tidying up

(2) Introduce more modularity, so as to make it easier to add
    readers for other formats, if needed

(3) Simplify the stabs reader.

Rationale for (1) and (2) are obvious.

Re (3), the stabs reader has for a good year contained a sophisticated
and impressive parser for stabs strings, with the aim of recording in 
detail the types of variables (I think) (Jeremy's work).  Unfortunately 
that has caused various segfaults reading stabs info in the past few months
(#77869, #117936, #119914, #120345 and another to do with deeply nested
template types).

The worst thing is that it is the stabs type reader that is crashing,
not the stabs line-number reader, but the type info is only used by
Helgrind, which is looking pretty dead at the moment.  So I have lifed
out the type-reader code and put it in UNUSED_STABS.txt for safe
storage, just leaving the line-number reader in place.

If Helgrind ever does come back to life we will need to reinstate the
type storage/reader stuff but with DWARF as its primary target.
Placing the existing stabs type-reader in hibernation improves
stability whilst retaining the development effort/expertise that went
into it for possible future reinstatement.
