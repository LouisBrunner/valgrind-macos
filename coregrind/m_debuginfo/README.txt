
On 4 Apr 06, the debuginfo reader (m_debuginfo) was majorly cleaned up
and restructured.  It has been a bit of a tangle for a while.  On 18 Sep 14
the STABS support was completely removed. The new structure looks like this:

                  debuginfo.c 
  
                   readelf.c

                  readdwarf.c

                   storage.c

Each .c can only call those below it on the page.

storage.c contains the SegInfo structure and stuff for 
maintaining/searching arrays of symbols, line-numbers, and Dwarf CF 
info records.

readdwarf.c parses the relevant kind of info and call storage.c to
store the results.

readelf.c reads ELF format, hands syms directly to storage.c,
then delegates to readdwarf.c for debug info.  All straightforward.

debuginfo.c is the top-level file, and is quite small.

There are 2 goals to this:

(1) Generally tidy up something which needs tidying up

(2) Introduce more modularity, so as to make it easier to add
    readers for other formats, if needed

Rationale for (1) and (2) are obvious.

Originally there was also goal (3) Simplify the stabs reader.
But stabs support was broken since 3.9.0 and completely removed in 3.10.0.

The worst thing is that it is the stabs type reader that was crashing,
not the stabs line-number reader.
Old versions of the stabs type reader can be found in the subversion repository
as m_debuginfo/UNUSED_STABS.txt, the stabs line-number reader was in
m_debuginfo/readstabs.c. The old version of this file explained more
about the setup.
