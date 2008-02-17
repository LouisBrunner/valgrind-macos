DRD: a Data Race Detector
=========================

Last update: February 16, 2008 by Bart Van Assche.


Introduction
------------

Multithreading is a concept to model multiple concurrent activities
within a single process. Each such concurrent activity is called a
thread. All threads that are active within a process share the same
set of memory locations. Data is exchanged between threads by writing
to and reading from the shared memory. Since the invention of the
multithreading concept, there is an ongoing debate about which way to
model concurrent activities is better -- shared memory programming or
message passing [Ousterhout 1996]. This debate exists because each
model has significant advantages and disadvantages. While shared
memory programming relieves the programmer from writing code for the
exchange of data between concurrent activities and while shared memory
programming has a performance advantage over message passing, shared
memory programming is error prone. Shared memory programs can exhibit
data races and/or deadlocks. Data races are harmful because these may
lead to unpredictable results and nondeterministic behavior in
multithreaded programs. There are two ways to detect data races and
deadlocks: static analysis and runtime detection by a tool. Since
there do not yet exist any tools that can carry out static analysis of
data races or deadlocks, the only option to statically detect such
anomolies is source reading by a human. It takes a huge effort however
to detect all possible data races or deadlocks via source
reading. This is why tools for detecting data races and deadlocks at
runtime are essential.


Data Races
----------

Threads in a multithreaded process exchange information by writing to
and reading from memory locations shared by the threads. Two accesses
to the same memory location by different threads are called
conflicting accesses if at least one of these two accesses modifies
the contents of the memory location.

A deterministic exchange of data between threads is only possible if
conflicting accesses happen in a well-defined order. It is the role of
synchronization actions to enforce the runtime execution order of
conflicting accesses. Examples of such synchronization actions are
pthread_mutex_lock(), pthread_mutex_unlock(), sem_wait(), sem_post(),
...

An important concept with regard to the ordering of load and store
operations on shared memory is the happens-before-1 relation or hb1
[Adve 1991]. The hb1 relation is a partial order defined over all
shared memory operations. The hb1 relation includes both the
intrathread execution order and the interthread ordering imposed by
synchronization operations. All intrathread accesses of a single
thread are totally ordered by hb1. Since hb1 is a partial order for
interthread memory accesses, interthread memory accesses are either
ordered or not ordered by hb1. A data race is defined by Adve et
al. as two conflicting accesses that are not ordered by the
happens-before-1 relation. Or: which accesses are considered as data
races depends on the runtime behavior of a program.

There is an interesting relationship between runtime behavior and
multithreaded design patterns. The most straightforward way to ensure
that different threads access shared data in an orderly fashion is to
ensure that at most one thread can access the object at any given
time. This can be realized by a programmer to surround all shared data
accesses with calls to proper synchronization functions. Such a source
code strategy for avoiding data races is also called a locking
discipline. An important property of programs that follow this
strategy is that these programs are data-race free.

There exist two kinds of tools for verifying the runtime behavior of
multithreaded programs. One class of tools verifies a locking
strategy, and another class of tools verifies the absence of data
races. The difference is subtle but important.

The most well know algorithm for runtime verification of a locking
strategy is the so called Eraser algorithm [Savage 1997]. While this
algorithm allows to catch more programming errors than the conflicting
accesses classified as data races by the definition of Sarita Adve et
al., unfortunately the Eraser algorithm also reports a lot of false
positives. It is tedious to review the output of the Eraser tool
manually and to verify which reported pairs of accesses are false
positives and which pairs are real data races. There is still research
ongoing about how to reduce the number of false positives reported by
the Eraser algorithm -- see e.g. [Müehlenfeld 2007]. The Helgrind
tool is a refinement of the Eraser algorithm.

A second class of data race detection tools detects all conflicting
accesses that are data races according to the definition of Sarita
Adve et al.  While in theory there is no guarantee that these tools
detect all locking discipline violations, these tools do not report
false positives. These tools are the most practical tools to
use. Examples of this class of tools are DIOTA [Ronsse 2004], Intel(R)
Thread Checker [Banerjee 2006a, Banerjee 2006b, Sack 2006] and DRD.


About DRD
---------

DRD is still under development, that is why the tool is named exp-drd.
The current version of DRD is able to perform data race detection on
small programs -- DRD quickly runs out of memory for realistically
sized programs.  The current version runs well under Linux on x86
CPU's for multithreaded programs that use the POSIX threading
library. Regular POSIX threads, detached threads, mutexes, condition
variables, spinlocks, semaphores and barriers are supported. POSIX
reader-writer locks are not yet supported.

Although [Savage 1997] claims that a happens-before detector is harder
to implement efficiently than the Eraser algorithm, as of Valgrind
version 3.3.0 exp-drd runs significantly faster on several regression
tests than Helgrind.


How to use DRD
--------------
To use this tool, specify --tool=drd on the Valgrind command line.


Future DRD Versions
-------------------
The following may be expected in future versions of DRD:
* Drastically reduced memory consumption, such that realistic applications can
  be analyzed with DRD.
* Faster operation.
* More extensive documentation.
* Support for reader-writer locks.
* Support for PowerPC CPU's.
* A lock dependency analyzer, as a help in deadlock prevention.
* Elimination of several artificial limitations.


References
----------

[Lamport 1978]
  Leslie Lamport.
  Time, clocks, and the ordering of events in a distributed system.
  Communications of the ACM archive, Volume 21, Issue 7, 1978.
  http://research.microsoft.com/users/lamport/pubs/time-clocks.pdf
  http://portal.acm.org/citation.cfm?id=359563

[Netzer 1992]
  Robert H. B. Netzer and Barton P. Miller.
  What are race conditions? Some issues and formalizations.
  ACM Letters on Programming Languages and Systems, 1(1):74–88, March 1992.
  http://www.securitytechnet.com/resource/security/os/race-conditions.pdf
  http://portal.acm.org/citation.cfm?id=130623

[Adve 1991]
  Sarita V. Adve, Mark D. Hill, Barton P. Miller, Robert H. B. Netzer.
  Detecting data races on weak memory systems.
  Proceedings of the 18th annual international symposium on Computer
  architecture, Toronto, Ontario, Canada, pp 234-243, 1991.
  http://rsim.cs.uiuc.edu/~sadve/Publications/isca91.dataraces.ps
  http://portal.acm.org/citation.cfm?doid=115953.115976

[Ousterhout 1996]
  John Ousterhout.
  Why Threads Are A Bad Idea (for most purposes).
  Invited Talk at the 1996 USENIX Technical Conference (January 25, 1996).
  http://home.pacbell.net/ouster/threads.pdf

[Savage 1997]
  Stefan Savage, Michael Burrows, Greg Nelson, Patrick Sobalvarro and
  Thomas Anderson.
  Eraser: A Dynamic Data Race Detector for Multithreaded Programs.
  ACM Transactions on Computer Systems, 15(4):391-411, November 1997.
  http://www.cs.ucsd.edu/users/savage/papers/Tocs97.pdf
  http://portal.acm.org/citation.cfm?id=265927

[Ronsse 1999]
  Michiel Ronsse, Koen De Bosschere.
  RecPlay: a fully integrated practical record/replay system.
  ACM Transactions on Computer Systems (TOCS), Volume 17, Issue 2 (May 1999),
  pp. 133-152, 1999.
  http://portal.acm.org/citation.cfm?id=312214

[Ronsse 2004]
  Michiel Ronsse, Jonas Maebe, Koen De Bosschere.
  Detecting Data Races in Sequential Programs with DIOTA.
  Proceedings of the 10th International Euro-Par Conference, Springer-Verlag,
  Lecture Notes in Computer Science, pp. 82-89, 2004.
  http://escher.elis.ugent.be/publ/Edocs/DOC/P104_076.pdf

[Banerjee 2006a]
  Utpal Banerjee, Brian Bliss, Zhiqiang Ma, Paul Petersen.
  Unraveling Data Race Detection in the Intel® Thread Checker.
  First Workshop on Software Tools for Multi-core Systems (STMCS), in
  conjunction with IEEE/ACM International Symposium on Code Generation and
  Optimization (CGO), March 26, 2006, Manhattan, New York, NY.

[Banerjee 2006b]
  Utpal Banerjee, Brian Bliss, Zhiqiang Ma, Paul Petersen.
  A theory of data race detection
  Proceeding of the 2006 workshop on Parallel and distributed systems: testing
  and debugging, Portland, Maine, USA, pp. 69-78, 2006.
  http://www.cs.ucsb.edu/~tiwari/papers/threadchecker06
  http://portal.acm.org/citation.cfm?id=1147416

[Lu 2006]
  Shan Lu, Joseph Tucek, Feng Qin, Yuanyuan Zhou.
  AVIO: detecting atomicity violations via access interleaving invariants.
  Proceedings of the 12th international conference on Architectural support
  for programming languages and operating systems, San Jose, California, USA,
  pp. 37-48, 2006.
  http://www.cse.ohio-state.edu/~qin/pub-papers/2006andbefore/asplos062-lu.pdf
  http://portal.acm.org/citation.cfm?id=1168864

[Sack 2006]
  Paul Sack, Brian E. Bliss, Zhiqiang Ma, Paul Petersen, Josep Torrellas
  Accurate and efficient filtering for the Intel thread checker race detector.
  Proceedings of the 1st workshop on Architectural and system support for
  improving software dependability, San Jose, California, pp. 34-41, 2006.
  http://iacoma.cs.uiuc.edu/iacoma-papers/asid06.pdf
  http://portal.acm.org/citation.cfm?id=1181309.1181315

[Müehlenfeld 2007]
  Arndt Müehlenfeld, Franz Wotawa.
  Fault detection in multi-threaded c++ server applications.
  Proceedings of the 12th ACM SIGPLAN symposium on Principles and practice of
  parallel programming, San Jose, California, USA, poster session,
  pp. 142-143, 2007.
  http://valgrind.org/docs/muehlenfeld2006.pdf
  http://portal.acm.org/citation.cfm?id=1229457

[Zhou 2007]
  Pin Zhou, Radu Teodorescu, Yuanyuan Zhou.
  HARD: Hardware-Assisted Lockset-based Race Detection.
  Proceedings of the 2007 IEEE 13th International Symposium on High
  Performance Computer Architecture, pp. 121-132, 2007.
  http://opera.cs.uiuc.edu/paper/Hard-HPCA07.pdf
  http://portal.acm.org/citation.cfm?id=1317533.1318108
