
As of May 2005, Valgrind can produce its output in XML form.  The
intention is to provide an easily parsed, stable format which is
suitable for GUIs to read.


Design goals
~~~~~~~~~~~~

* Produce XML output which is easily parsed

* Have a stable output format which does not change much over time, so
  that investments in parser-writing by GUI developers is not lost as
  new versions of Valgrind appear.

* Have an extensive output format, so that future changes to the
  format do not break backwards compatibility with existing parsers of
  it.

* Produce output in a form which suitable for both offline GUIs (run
  all the way to the end, then examine output) and interactive GUIs
  (parse XML incrementally, update display as we go).

* Put as much information as possible into the XML and let the GUIs
  decide what to show the user (a.k.a provide mechanism, not policy).


How to use
~~~~~~~~~~

Run with flag --xml=yes.  That's all.  Note however several 
caveats.

* At the present time only Memcheck is supported.  The scheme extends
  easily enough to cover Addrcheck and Helgrind if needed.

* When XML output is selected, various other settings are made.
  This is in order that the output format is more controlled.
  The settings which are changed are:

  - Suppression generation is disabled, as that would require user
    input.

  - Attaching to GDB is disabled for the same reason.

  - The verbosity level is set to 1 (-v).

  - Error limits are disabled.  Usually if the program generates a lot
    of errors, Valgrind slows down and eventually stops collecting
    them.  When outputting XML this is not the case.

  - VEX emulation warnings are not shown.

  - File descriptor leak checking is disabled.  This could be
    re-enabled at some future point.

  - Maximum-detail leak checking is selected (--leak-check=full).


The output format
~~~~~~~~~~~~~~~~~
For the most part this should be self descriptive.  It is printed
in a sort-of human-readable way for easy understanding.

All tags are balanced: a <foo> tag is always closed by </foo>.  Hence
in the description that follows, mention of a tag <foo> implicitly
means there is a matching closing tag </foo>.

Symbols in CAPITALS are nonterminals in the grammar and are defined
somewhere below.  The root nonterminal is TOPLEVEL.

The following nonterminals are not described further:
   INT   is a 64-bit signed decimal integer.
   TEXT  is arbitrary text.
   HEX64 is a 64-bit hexadecimal number.


TOPLEVEL
--------
All output is contained within the tag-pair <valgrindoutput>.

Inside that, the first entity is an indication of the protocol
version.  This is provided so that existing parsers can identify XML
created by future versions of Valgrind merely by observing that the
protocol version is one they don't understand.  Hence TOPLEVEL is:

  <valgrindoutput>
    <protocolversion>INT<protocolversion>
    VERSION1STUFF
  </valgrindoutput>

The only currently defined protocol version number is 1.  This
document only defines protocol version 1.


VERSION1STUFF
-------------
This is the main top-level construction.  Roughly speaking, it
contains a load of preamble, the errors from the run of the
program, and the result of the final leak check.  Hence the
following in sequence:

* Various preamble lines which give version info for the various
  components.  The text in them can be anything; it is not intended
  for interpretation by the GUI:

     <preamble>Misc version/copyright text</preamble>

* The PID of this process and of its parent:

     <pid>INT</pid>
     <ppid>INT</ppid>

* The name of the tool being used:

     <tool>TEXT</tool>

* The program and args being run.  Note, the program name is not
  distinguished; it is merely the first presented TEXT:

     <argv>
       <arg>TEXT</arg>
       (one or more of)
     </argv>

* The following, indicating that the program has now started:

     <status>RUNNING</status>

* Zero or more of (either ERROR or ERRORCOUNTS).

* The following, indicating that the program has now finished, and
  that the wrapup (leak checking) is happening.

     <status>FINISHED</status>

* SUPPCOUNTS, indicating how many times each suppression was used.

* Zero or more ERRORs, each of which is a complaint from the
  leak checker.

That's it.


ERROR
-----
This shows an error, and is the most complex nonterminal.  The format
is as follows:

  <error>
     <unique>HEX64</unique>
     <tid>INT</tid>
     <kind>KIND</kind>
     <what>TEXT</what>

     optionally: <leakedbytes>INT</leakedbytes>
     optionally: <leakedblocks>INT</leakedblocks>

     STACK

     optionally: <auxwhat>TEXT</auxwhat>
     optionally: STACK

  </error>

* Each error contains a unique, arbitrary 64-bit hex number.  This is
  used to refer to the error in ERRORCOUNTS nonterminals (see below).

* The <tid> tag indicates the Valgrind thread number.  This value
  is arbitrary but may be used to determine which threads produced
  which errors (at least, the first instance of each error).

* The <kind> tag specifies one of a small number of fixed error
  types (enumerated below), so that GUIs may roughly categorise
  errors by type if they want.

* The <what> tag gives a human-understandable description of the
  error.

* For <kind> tags specifying a KIND of the form "Leak_*", the
  optional <leakedbytes> and <leakedblocks> indicate the number of
  bytes and blocks leaked by this error.

* The primary STACK for this error, indicating where it occurred.

* Some error types may have auxiliary information attached:

     <auxwhat>TEXT</auxwhat> gives an auxiliary human-readable
     description (usually of invalid addresses)

     STACK gives an auxiliary stack (usually the allocation/free
     point of a block).  If this STACK is present then 
     <auxwhat>TEXT</auxwhat> will precede it.


KIND
----
This is a small enumeration indicating roughly the nature of an error.
The possible values are:

   InvalidFree

      free/delete/delete[] on an invalid pointer

   MismatchedFree

      free/delete/delete[] does not match allocation function
      (eg doing new[] then free on the result)

   InvalidRead

      read of an invalid address

   InvalidWrite

      write of an invalid address

   InvalidJump

      jump to an invalid address

   Overlap

      args overlap other otherwise bogus in eg memcpy

   InvalidMemPool

      invalid mem pool specified in client request

   UninitCondition

      conditional jump/move depends on undefined value

   UninitValue

      other use of undefined value (primarily memory addresses)

   SyscallParam

      system call params are undefined or point to
      undefined/unaddressible memory

   ClientCheck

      "error" resulting from a client check request

   Leak_DefinitelyLost

      memory leak; the referenced blocks are definitely lost

   Leak_IndirectlyLost

      memory leak; the referenced blocks are lost because all pointers
      to them are also in leaked blocks

   Leak_PossiblyLost

      memory leak; only interior pointers to referenced blocks were
      found

   Leak_StillReachable

      memory leak; pointers to un-freed blocks are still available


STACK
-----
STACK indicates locations in the program being debugged.  A STACK
is one or more FRAMEs.  The first is the innermost frame, the
next its caller, etc.  

   <stack>
      one or more FRAME
   </stack>


FRAME
-----
FRAME records a single program location:

   <frame>
      <ip>HEX64</ip>
      optionally <obj>TEXT</obj>
      optionally <fn>TEXT</fn>
      optionally <file>TEXT</file>
      optionally <line>INT</line>
   </frame>

Only the <ip> field is guaranteed to be present.  It indicates a
code ("instruction pointer") address.

The optional fields, if present, appear in the order stated:

* obj: gives the name of the ELF object containing the code address

* fn: gives the name of the function containing the code address

* file: gives the name of the source file containing the code address

* line: gives the line number in the source file


ERRORCOUNTS
-----------
This specifies, for each error that has been so far presented,
the number of occurrences of that error.

  <errorcounts>
     zero or more of
        <pair> <count>INT</count> <unique>HEX64</unique> </pair>
  </errorcounts>

Each <pair> gives the current error count <count> for the error with
unique tag </unique>.  The counts do not have to give a count for each
error so far presented - partial information is allowable.

As at Valgrind rev 3792, error counts are only emitted at program
termination.  However, it is perfectly acceptable to periodically emit
error counts as the program is running.  Doing so would facilitate a
GUI to dynamically update its error-count display as the program runs.


SUPPCOUNTS
----------
A SUPPCOUNTS block appears exactly once, after the program terminates.
It specifies the number of times each error-suppression was used.
Suppressions not mentioned were used zero times.

  <suppcounts>
     zero or more of
        <supp> <count>INT</count> <name>TEXT</name> </supp>
  </suppcounts>

The <name> is as specified in the suppression name fields in .supp
files.
