DRD: a Data Race Detector
=========================

Last update: December 3, 2007 by Bart Van Assche.


The Difficulty of Multithreading Programming
--------------------------------------------
Multithreading is a concept to model multiple concurrent activities within a
single process. Since the invention of the multithreading concept, there is an
ongoing debate about which way to model concurrent activities is better --
multithreading or message passing. This debate exists because
multithreaded programming is error prone: multithreaded programs can exhibit
data races and/or deadlocks. Despite these risks multithreaded programming is
popular: for many applications multithreading is a more natural programming
style, and multithreaded code often runs faster than the same application
implemented via message passing.

In the context of DRD, a data race is defined as two concurrent memory
accesses, where at least one of these two memory accesses is a store operation,
and these accesses are not protected by proper locking constructs. Data
races are harmful because these may lead to unpredictable results in
multithreaded programs. There is a general consensus that data races
should be avoided in multithreaded programs.


About DRD
---------
The current version of DRD is able to perform data race detection on small
programs -- DRD quickly runs out of memory for realistically sized programs.
The current version runs well under Linux on x86 CPU's for multithreaded
programs that use the POSIX threading library. Regular POSIX threads, detached
threads, mutexes, condition variables and spinlocks are supported. POSIX
semaphores, barriers and reader-writer locks are not yet supported.

Extensive scientific research has been carried out on the area of data-race
detection. The two most important algorithms are known as the Eraser algorithm
and the algorithm based on the happens-before relationship, first documented by
Netzer. The Eraser algorithm can result in false positives, while the Netzer
algorithm guarantees not to report false positives. The Netzer algorithm
ignores a certain class of data races however. Both algorithms have been
implemented in Valgrind. The helgrind tool implements the Eraser algorithm,
and the DRD tool implements the Netzer algorithm. Although [Savage 1997]
claims that the Netzer algorithm is harder to implement efficiently, as of
version 3.3.0 drd runs significantly faster on several regression tests than
helgrind.


How to use DRD
--------------
To use this tool, specify --tool=drd on the Valgrind command line.


Future DRD Versions
-------------------
The following may be expected in future versions of DRD:
* More extensive documentation.
* Drastically reduced memory consumption, such that realistic applications can
  be analyzed with DRD.
* Faster operation.
* Support for semaphores, barriers and reader-writer locks.
* Support for PowerPC CPU's.
* A lock dependency analyzer, as a help in deadlock prevention.
* Support for more than 256 mutexes per process.


See also
--------
* Robert H. B. Netzer and Barton P. Miller. What are race
  conditions? Some issues and formalizations. ACM Letters 136
  on Programming Languages and Systems, 1(1):74–88, March 1992.
  
* John Ousterhout, Why Threads Are A Bad Idea (for most
  purposes), Invited Talk at the 1996 USENIX Technical Conference (January
  25, 1996). http://home.pacbell.net/ouster/threads.pdf
  
* Stefan Savage, Michael Burrows, Greg Nelson, Patrick
  Sobalvarro and Thomas Anderson, Eraser: A Dynamic Data Race Detector for
  Multithreaded Programs, ACM Transactions on Computer Systems,
  15(4):391-411, November 1997.



