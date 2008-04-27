DRD: a Data Race Detector
=========================

Last update: March 16, 2008 by Bart Van Assche.


Introduction
------------

Multithreading is a concept to model multiple concurrent activities
within a single process. Each such concurrent activity is called a
thread. All threads that are active within a process share the same
set of memory locations. Data is exchanged between threads by writing
to and reading from the shared memory. Since the invention of the
multithreading concept, there is an ongoing debate about which way to
model concurrent activities is better -- shared memory programming or
message passing. This debate exists because each model has significant
advantages and disadvantages. While shared memory programming relieves
the programmer from writing code for the exchange of data between
concurrent activities and while shared memory programming has a
performance advantage over message passing, shared memory programming
is error prone. Shared memory programs can exhibit data races and/or
deadlocks. Data races are harmful because these may lead to
unpredictable results and nondeterministic behavior in multithreaded
programs. There are two ways to detect data races and deadlocks:
static analysis and runtime detection by a tool. Since there do not
yet exist any tools that can carry out static analysis of data races
or deadlocks, the only option to statically detect such anomalies is
source reading by a human. It takes a huge effort however to detect
all possible data races or deadlocks via source reading. This is why
tools for detecting data races and deadlocks at runtime are essential.

Threads can be used to model simultaneous activities in software, to
allow software to use more than one core of a multiprocessor or both
at the same time.  Most multithreaded server and embedded software
falls in the first category, while most multithreaded high performance
computing (HPC) applications fall in the second category. The source
code syntax for using multithreading depends on the programming
language and the threading library you want to use. Two common options
on Unix systems are the POSIX threads library for modeling
simultaneous activities in C and C++ software and OpenMP for C, C++
and Fortran HPC applications.


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
The current version runs well under Linux on x86 CPU's for
multithreaded programs that use the POSIX threading library. Regular
POSIX threads, detached threads, mutexes, condition variables,
reader-writer locks, spinlocks, semaphores and barriers are supported.
Client programs run under exp-drd typically run somewhere between 50
and 100 times slower than when executed natively. A notable exception
is Firefox, which runs too slow to be usable. This is because of the
huge number of mutex lock and unlock calls performed by
Firefox. E.g. just starting and stopping Firefox 3 triggers 2.5
million pthread_mutex_lock() calls and the same number of
pthread_mutex_unlock() calls.


Programming with Threads
------------------------

The difficulties with shared memory programming are well known and
have been outlined in more than one paper [Ousterhout 1996, Lee
2006]. It is possible however to develop and to debug multithreaded
shared memory software with a reasonable effort, even for large
applications (more than one million lines of code). In what follows an
approach is explained that has proven to work in practice. Before you
decide to use another approach, make sure you understand very well the
consequences of doing so.

Note: the guidelines below apply to the explicit use of threads such
as with the POSIX threads library, and not to OpenMP programs.

1. Use of synchronization calls.

Do not call synchronization functions directly but use objects that
encapsulate the mutex, Mesa monitor and reader/writer locking policies.
Never use POSIX condition variables directly, since direct use of 
condition variables can easily introduce race conditions. And never
lock or unlock mutexes explicitly -- use scoped locking instead.

2. Data hiding.

It is very important in multithreaded software to hide data that is
shared over threads. Make sure that all shared data is declared as
private data members of a class (not public, not protected). Design
the classes that contain shared data such that the number of data
members and the number of member functions is relatively small. Define
accessor functions as needed for querying and modifying member
data. Declare the associated locking objects also as private data
members, and document which locking object protects which data
members. Make sure that the query functions return a copy of data
members instead of a reference -- returning a reference would violate
data hiding anyway. This approach has a big advantage, namely that
correct use of a locking policy can be verified by reviewing one class
at a time.
 
3. Modularity and hierarchy.

For multithreaded software it is even more important than for single
threaded software that the software has a modular structure and that
there exists a hierarchy between modules. This way every call of a
function to another function can be classified as either a regular
function call (a call from a higher level to a lower level), a
callback (a call from a lower level to a higher level) or a recursive
function call.

4. Avoiding deadlocks.

Deadlocks can be nasty to solve since some deadlocks are very hard to
reproduce.  Prevent deadlocks instead of waiting until one pops
up. Preventing deadlocks is possible by making sure that whenever two
or more mutexes are locked simultaneously, these mutexes are always
locked in the same order. One way to ensure this is by assigning each
mutex a locking order and by verifying the locking order at runtime.
This reduces the complexity of testing for absence of deadlocks from a
multithreaded to a single-threaded problem, which is a huge win. In
order to verify a locking order policy at run time, one can either use
a threading library with built-in support for verifying such a policy
or one can use a tool that verifies the locking order.

Make sure that no mutexes are locked while invoking a callback
(calling a function from a higher level module) -- invoking a
callback while a mutex is locked is a well known way to trigger
a deadlock.

5. Real-time software

Software with hard real-time constraints is a special case. There
exist real-time applications that must be able to generate a response
within e.g. 1 ms after a certain input has been received. The proper
way to implement time-critical paths is not to call any function in
that path for which it is not known how long the function call will
take. Exmples for Linux of actions with an unknown call time are:
- Locking a mutex.
- Dynamic memory allocation via e.g. malloc() since malloc() internally
uses mutexes and since malloc() may trigger TLB modifications via the
kernel.
- File I/O, since file I/O uses several resources that are shared over
threads and even over processes.

An approach that has proven to work for interthread communication
between real-time threads is the use of preallocated fixed size
message queueus, and to lock any data needed by any real-time thread
in memory (mlock()).  Avoid mutexes with priority inheritance -- see
also [Yodaiken 2004] for more information. Lock-free data structures
like circular buffers are well suited for real-time software.


Linux and POSIX Threads
-----------------------

There exist two implementations of the POSIX threads API for
Linux. These implementations are called LinuxThreads and
NPTL. LinuxThreads was historically the first POSIX threads
implementation for Linux. LinuxThreads was compliant to most but not
all POSIX threads specifications. That is why a new threading library
for Linux was developed, called the NPTL (Native POSIX Threads
Library). Most Linux distributions switched from LinuxThreads to NPTL
around 2004. DRD only supports the NPTL. See also [Shukla 2006] for
more information.


How to use DRD
--------------

To use this tool, specify --tool=exp-drd on the Valgrind command line.


Interpreting DRD's data race reports
------------------------------------

You should be aware of the following when interpreting DRD's output:
* Every thread is assigned two thread ID's: one thread ID is assigned
  by the Valgrind core and one thread ID is assigned by DRD. Thread
  ID's start at one. Valgrind thread ID's are reused when one thread
  finishes and another thread is created. DRD does not reuse thread
  ID's. Thread ID's are displayed e.g. as follows: 2/3, where the
  first number is Valgrind's thread ID and the second number is the
  thread ID assigned by DRD.
* The term segment refers to a consecutive sequence of load, store and
  synchronization operations, all issued by the same thread. A segment
  always starts and ends at a synchronization operation. Data race
  analysis is performed between segments instead of between individual
  load and store operations because of performance reasons.

Below you can find an example of a (harmless) data race report from Firefox:

==7689== Thread 1:
==7689== Conflicting store by thread 1/1 at 0x1226f978 size 8
==7689==    at 0x5E983D3: _PR_CreateThread (ptthread.c:517)
==7689==    by 0x5E98474: PR_CreateThread (ptthread.c:544)
==7689==    by 0x57E1EAE: nsThread::Init() (nsThread.cpp:322)
==7689==    by 0x57E359B: nsThreadManager::NewThread(unsigned, nsIThread**) (nsThreadManager.cpp:226)
==7689==    by 0x57915FC: NS_NewThread_P(nsIThread**, nsIRunnable*) (nsThreadUtils.cpp:70)
==7689==    by 0x4B6CF4: nsSocketTransportService::Init() (nsSocketTransportService2.cpp:406)
==7689==    by 0x48DDF5: nsSocketTransportServiceConstructor(nsISupports*, nsID const&, void**) (nsNetModule.cpp:88)
==7689==    by 0x579331C: nsGenericFactory::CreateInstance(nsISupports*, nsID const&, void**) (nsGenericFactory.cpp:80)
==7689==    by 0x57D79E6: nsComponentManagerImpl::CreateInstanceByContractID(char const*, nsISupports*, nsID const&, void**) (nsComponentManager.cpp:1756)
==7689==    by 0x57D9381: nsComponentManagerImpl::GetServiceByContractID(char const*, nsID const&, void**) (nsComponentManager.cpp:2189)
==7689==    by 0x578AEAF: CallGetService(char const*, nsID const&, void**) (nsComponentManagerUtils.cpp:94)
==7689==    by 0x578AED1: nsGetServiceByContractIDWithError::operator()(nsID const&, void**) const (nsComponentManagerUtils.cpp:288)
==7689== Address 0x1226f978 is at offset 96 from 0x1226f918. Allocation context:
==7689==    at 0x4C21CCE: calloc (vg_replace_malloc.c:403)
==7689==    by 0x5E83F03: PR_Calloc (prmem.c:474)
==7689==    by 0x5E9816B: _PR_CreateThread (ptthread.c:385)
==7689==    by 0x5E98474: PR_CreateThread (ptthread.c:544)
==7689==    by 0x57E1EAE: nsThread::Init() (nsThread.cpp:322)
==7689==    by 0x57E359B: nsThreadManager::NewThread(unsigned, nsIThread**) (nsThreadManager.cpp:226)
==7689==    by 0x57915FC: NS_NewThread_P(nsIThread**, nsIRunnable*) (nsThreadUtils.cpp:70)
==7689==    by 0x4B6CF4: nsSocketTransportService::Init() (nsSocketTransportService2.cpp:406)
==7689==    by 0x48DDF5: nsSocketTransportServiceConstructor(nsISupports*, nsID const&, void**) (nsNetModule.cpp:88)
==7689==    by 0x579331C: nsGenericFactory::CreateInstance(nsISupports*, nsID const&, void**) (nsGenericFactory.cpp:80)
==7689==    by 0x57D79E6: nsComponentManagerImpl::CreateInstanceByContractID(char const*, nsISupports*, nsID const&, void**) (nsComponentManager.cpp:1756)
==7689==    by 0x57D9381: nsComponentManagerImpl::GetServiceByContractID(char const*, nsID const&, void**) (nsComponentManager.cpp:2189)
==7689== Other segment start (thread 2/2)
==7689==    at 0xA948F51: clone (in /lib64/libc-2.6.1.so)
==7689==    by 0x4E2FF4F: (within /lib64/libpthread-2.6.1.so)
==7689==    by 0x12B2A94F: ???
==7689== Other segment end (thread 2/2)
==7689==    at 0x4C23EE9: pthread_mutex_lock (drd_pthread_intercepts.c:364)
==7689==    by 0x5E92112: PR_Lock (ptsynch.c:207)
==7689==    by 0x5E97E87: _pt_root (ptthread.c:206)
==7689==    by 0x4C26660: vg_thread_wrapper (drd_pthread_intercepts.c:163)
==7689==    by 0x4E3001F: start_thread (in /lib64/libpthread-2.6.1.so)
==7689==    by 0xA948F8C: clone (in /lib64/libc-2.6.1.so)

The meaning of all the data in such a report is as follows:
* The numbers in the column on the left contains the process ID of the
  process being analyzed by DRD.
* The first line ("Thread 1") tells you Valgrind's thread ID of the
  thread in which context the data race was detected.
* The next line tells which kind of operation was performed (load or
  store) and by which thread (both Valgrind's and DRD's thread ID are
  displayed). On the same line the start address and the number of
  bytes involved in the conflicting access are also displayed.
* Below the "Conflicting access" line the call stack of the conflicting
  access is displayed. If your program has been compiled with debug
  information (-g), this call stack will include file names and line
  numbers.
* Next, the allocation context of the address on which the conflict
  was displayed.
* A conflicting access involves at least two memory accesses. For one
  of these accesses an exact call stack is displayed, and for the other
  accesses an approximate call stack is displayed: the start and the
  end of the segments of the other accesses are displayed. Sometimes
  this contains useful information, but not always.

Usually the first call stack displayed in a conflicting access report
is sufficient for finding on which variable the conflicting access
happened. The challenge is to find out whether or not such a
conflicting access can cause undesired behavior. A first step is to
identify all accesses to the same variable. If you do not have
sufficient knowledge of the software being analyzed, you can also
trace all accesses to that variable. For the above example it is
sufficient to insert the macro DRD_TRACE_VAR(thred->id) in file
ptthread.c just after allocation of the PRThread structure. The next
step is to recompile the application and to rerun it under DRD.  For
the above example, this will learn you that thred->id is assigned a
value both in the creator and in the created thread. In Firefox'
source code can see that twice the same value is assigned to the
per-thread variable thred->id. Or: this is a harmless conflicting
access, and you can safely replace DRD_TRACE_VAR(thred->id) by
DRD_IGNORE_VAR(thred->id).


DRD and OpenMP
--------------

Just as regular POSIX threads software, OpenMP software can contain
data races. DRD is able to detect data races in OpenMP programs, but
only if the shared library that implements OpenMP functionality
(libgomp.so) has been compiled such that it uses the POSIX threads
library instead of futexes. A second requirement is that libgomp.so
must contain debug information. You have to recompile gcc in order to
obtain a version of libgomp.so that is suited for use with DRD. Once
gcc has been recompiled, set the CC and LD_LIBRARY_PATH environment
variables appropriately. It can be convenient to set up shortcuts for
switching between the gcc compiler provided by your Linux distribution
and the newly compiled gcc. The shell command below will add the commands
system-gcc and my-gcc to your shell upon the next login:

cat <<EOF >>~/.bashrc
function system-gcc { unset CC LD_LIBRARY_PATH; export CC LD_LIBRARY_PATH; }
function my-gcc { export CC=$HOME/gcc-4.3.0/bin/gcc LD_LIBRARY_PATH=$HOME/gcc-4.3.0/lib64:; }
EOF

Recompiling gcc is possible with e.g. the following shell script:

---------------------------------------------------------------------------
#!/bin/sh

# Make sure that libgmp and libmpfr are installed before you run this script.
# On Debian systems, e.g. Ubuntu, you can install these libraries as follows:
# sudo apt-get install libgmp3-dev libmpfr-dev

GCC_VERSION=4.3.0
FSF_MIRROR=ftp://ftp.easynet.be/gnu
SRCDIR=$HOME/software
DOWNLOADS=$SRCDIR/downloads
SRC=$HOME/software/gcc-${GCC_VERSION}
BUILD=${SRC}-build
TAR=gcc-${GCC_VERSION}.tar.bz2
PREFIX=$HOME/gcc-${GCC_VERSION}

rm -rf   ${BUILD}     || exit $?
rm -rf   ${PREFIX}    || exit $?
mkdir -p ${BUILD}     || exit $?
mkdir -p ${DOWNLOADS} || exit $?
cd       ${BUILD}     || exit $?

if [ ! -e $DOWNLOADS/$TAR ]; then
  ( cd $DOWNLOADS && wget -q $FSF_MIRROR/gcc/gcc-${GCC_VERSION}/$TAR )
fi

if [ ! -e $SRC ]; then
  ( cd $SRCDIR && tar -xjf $DOWNLOADS/$TAR )
fi

${SRC}/configure            \
  --disable-linux-futex     \
  --disable-mudflap         \
  --disable-nls             \
  --enable-languages=c,c++  \
  --enable-threads=posix    \
  --enable-tls              \
  --prefix=$PREFIX

make -s         || exit $?
make -s install || exit $?
---------------------------------------------------------------------------


Future DRD Versions
-------------------
The following may be expected in future versions of DRD:
* A lock dependency analyzer, as a help in deadlock prevention.
* More extensive documentation.
* Support for PowerPC CPU's.


Acknowledgements
----------------

The DRD tool is built on top of the Valgrind core and VEX, which
proved to be an excellent infrastructure for building such a tool.

During 2006, the early versions of drd were improved via helpful
feedback of Julian Seward and Nicholas Nethercote.  Any bugs are my
responsibility of course.

Some of the regression tests used to test DRD were developed by
Julian Seward as regression tests for the Helgrind tool.

I would also like to thank Michiel Ronsse for introducing me a long
time ago to vector clocks and the JiTI and DIOTA projects.


References
----------

[Hansen 1972]
  Per Brinch Hansen
  A Comparison of Two Synchronizing Concepts.
  Acta Informatica, 1 3(1972), pp. 190--199.

[Dijkstra 1974]
  Edsger W. Dijkstra.
  Over seinpalen (About Semaphores).
  Circulated privately (never published), 1974.
  http://www.cs.utexas.edu/users/EWD/transcriptions/EWD00xx/EWD74.html

[Hoare 1974]
  C. A. R. Hoare.
  Monitors: an operating system structuring concept
  Communications of the ACM, October 1974, Vol. 17 No. 10, 1974.
  http://www.cs.wisc.edu/~remzi/Classes/736/Fall2003/Papers/hoare-monitors.pdf

[Lamport 1978]
  Leslie Lamport.
  Time, clocks, and the ordering of events in a distributed system.
  Communications of the ACM archive, Volume 21, Issue 7, 1978.
  http://research.microsoft.com/users/lamport/pubs/time-clocks.pdf
  http://portal.acm.org/citation.cfm?id=359563

[Accetta 1986]
  Mike Accetta, Robert Baron, William Bolosky, David Golub, Richard Rashid,
  Avadis Tevanian and Michael Young.
  Mach: A New Kernel Foundation For UNIX Development.
  USENIX 1986 (Atlanta. Ga., June 9-13), pp. 93-112, 1986.
  http://www.fsl.cs.sunysb.edu/~gopalan/seminar/papers/mach.pdf

[Young 1987]
  Michael Young, Avadis Tevanian, Richard Rashid, David Golub,
  Jeffrey Eppinger, Jonathan Chew, William Bolosky, David Black, Robert Baron.
  The duality of memory and communication in the implementation of a
  multiprocessor operating system.
  ACM Symposium on Operating Systems Principles, pp. 63-76, 1987.
  http://csalpha.ist.unomaha.edu/~stanw/papers/csci8550/87-duality.pdf
  http://portal.acm.org/citation.cfm?id=41457.37507

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

[Cameron 1995]
  Steven Cameron Woo, Moriyoshi Ohara, Evan Torrie, Jaswinder Pal Singh
  and Anoop Gupta.
  The SPLASH-2 Programs: Characterization and Methodological Considerations.
  Proceedings of the 22nd International Symposium on Computer Architecture,
  pages 24-36, Santa Margherita Ligure, Italy, June 1995.
  http://portal.acm.org/citation.cfm?doid=225830.223990
  ftp://www-flash.stanford.edu/pub/splash2/splash2_isca95.ps.Z
  http://www-flash.stanford.edu/apps/SPLASH/splash2.tar.gz

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

[Christiaens 2002]
  Mark Christiaens, Michiel Ronsse, Koen De Bosschere.
  Bounding the number of segment histories during data race detection.
  Parallel Computing archive, Volume 28, Issue 9, pp 1221-1238,
  September 2002.
  http://portal.acm.org/citation.cfm?id=638124
 
[Ronsse 2004]
  Michiel Ronsse, Jonas Maebe, Koen De Bosschere.
  Detecting Data Races in Sequential Programs with DIOTA.
  Proceedings of the 10th International Euro-Par Conference, Springer-Verlag,
  Lecture Notes in Computer Science, pp. 82-89, 2004.
  http://escher.elis.ugent.be/publ/Edocs/DOC/P104_076.pdf

[Yodaiken 2004]
  Victor Yodaiken.
  Against Priority Inheritance.
  FSMLabs Technical Report, 2004.
  http://www.yodaiken.com/papers/inherit.pdf

[Banerjee 2006a]
  Utpal Banerjee, Brian Bliss, Zhiqiang Ma, Paul Petersen.
  Unraveling Data Race Detection in the Intel® Thread Checker.
  First Workshop on Software Tools for Multi-core Systems (STMCS), in
  conjunction with IEEE/ACM International Symposium on Code Generation and
  Optimization (CGO), March 26, 2006, Manhattan, New York, NY.
  http://www.isi.edu/~kintali/stmcs06/UnravelingDataRace.pdf

[Banerjee 2006b]
  Utpal Banerjee, Brian Bliss, Zhiqiang Ma, Paul Petersen.
  A theory of data race detection
  Proceeding of the 2006 workshop on Parallel and distributed systems: testing
  and debugging, Portland, Maine, USA, pp. 69-78, 2006.
  http://www.cs.ucsb.edu/~tiwari/papers/threadchecker06
  http://portal.acm.org/citation.cfm?id=1147416

[Lee 2006]
  Edward A. Lee.
  The Problem with Threads.
  IEEE Computer, Volume 39, Issue 5 (May 2006), pp. 33-42, 2006.
  http://www.eecs.berkeley.edu/Pubs/TechRpts/2006/EECS-2006-1.pdf
  http://portal.acm.org/citation.cfm?id=1137232.1137289

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

[Shukla 2006]
  Vikram Shukla
  NPTL -- A rundown of the key differences for developers who need to port
  July 31, 2006.
  http://www-128.ibm.com/developerworks/linux/library/l-threading.html?ca=dgr-lnxw07LinuxThreadsAndNPTL

[Müehlenfeld 2007]
  Arndt Müehlenfeld, Franz Wotawa.
  Fault Detection in Multi-threaded C++ Server Applications.
  Proceedings of the 12th ACM SIGPLAN symposium on Principles and practice of
  parallel programming, San Jose, California, USA, poster session,
  pp. 142-143, 2007.
  http://valgrind.org/docs/muehlenfeld2006.pdf
  http://portal.acm.org/citation.cfm?id=1229457

[Sun 2007]
  Sun Studio 12: Thread Analyzer User's Guide
  http://docs.sun.com/app/docs/doc/820-0619

[Venetis 2007]
  Ioannis E. Venetis
  The Modified SPLASH-2 Home Page
  http://www.capsl.udel.edu/splash/Download.html

[Zhou 2007]
  Pin Zhou, Radu Teodorescu, Yuanyuan Zhou.
  HARD: Hardware-Assisted Lockset-based Race Detection.
  Proceedings of the 2007 IEEE 13th International Symposium on High
  Performance Computer Architecture, pp. 121-132, 2007.
  http://opera.cs.uiuc.edu/paper/Hard-HPCA07.pdf
  http://portal.acm.org/citation.cfm?id=1317533.1318108
