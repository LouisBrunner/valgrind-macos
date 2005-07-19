The structure of this module is worth noting.

The main part is in vg_replace_malloc.c.  It gets compiled into the tool's
'preload' shared object, which goes into the client's area of memory, and
runs on the simulated CPU just like client code.  As a result, it cannot
use any functions in the core directly;  it can only communicate with the
core using client requests, just like any other client code.

And yet it must call the tool's malloc wrappers.  How does it know where
they are?  The init function uses a client request which asks for the list
of all the core functions (and variables) that it needs to access.  It then
uses a client request each time it needs to call one of these.

This means that the following sequence occurs each time a tool that uses
this module starts up:

 - Tool does initialisation, including calling VG_(malloc_funcs)() to tell
   the core the names of its malloc wrappers.  These are stored in
   VG_(tdict).

 - On the first allocation, vg_replace_malloc.c:init() calls the
   GET_MALLOCFUNCS client request to get the names of the malloc wrappers
   out of VG_(tdict), storing them in 'info'.

 - All calls to these functions are done using 'info'.

This is a bit complex, but it's hard to see how it can be done more simply. 


