
YARD, Yet Another Race Detector, built on the Helgrind framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Julian Seward, OpenWorks Ltd, 19 August 2008
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The YARD race detector lives in svn://svn.valgrind.org/branches/YARD.

It uses a new and relatively simple race detection engine, based on
the idea of shadowing each memory location with two vector timestamps,
indicating respectively the "earliest safe read point" and "earliest
safe write point".  As far as I know this is a novel approach.  Some
features of the implementation:

* Modularity.  The entire race detection engine is placed in a
  standalone library (libhb_core.c) with a simple interface (libhb.h).
  This makes it easier to debug and verify the engine; indeed it can
  be built as a standalone executable with test harness using "make -f
  Makefile_sa".

* Simplified and scalable storage management, so that large programs,
  with many synchronisation events, can be handled.

* Ability to report both call stacks involved in a race, without
  excessive time or space overhead.

* Pure happens before operation, so as not to give any false
  positives.

To use, build as usual and run as "--tool=helgrind".

You can disable lock order checking with --track-lockorders=no, as it
sometimes produces an annoying amount of output.
