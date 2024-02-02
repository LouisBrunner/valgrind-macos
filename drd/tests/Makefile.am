
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS =		    \
	compare_error_count_with    \
	filter_annotate_barrier_xml \
	filter_lambda               \
	filter_stderr_and_thread_no \
	filter_stderr_solaris       \
	filter_thread_name_xml      \
	run_openmp_test             \
	supported_libpthread	    \
	supported_sem_init

noinst_SCRIPTS = \
	filter_error_count \
	filter_error_summary \
	filter_stderr \
	filter_stderr_and_thread_no_and_offset \
	filter_thread_no \
	filter_xml_and_thread_no

noinst_HEADERS =                                    \
	tsan_thread_wrappers_pthread.h		    \
	unified_annotations.h			    \
	dlopen_lib.h

EXTRA_DIST =                                        \
	annotate_barrier.stderr.exp		    \
	annotate_barrier.vgtest			    \
	annotate_barrier_xml.stderr.exp		    \
	annotate_barrier_xml.vgtest		    \
	annotate_hb_err.stderr.exp		    \
	annotate_hb_err.vgtest			    \
	annotate_hb_race.stderr.exp		    \
	annotate_hb_race.vgtest			    \
	annotate_hbefore.stderr.exp		    \
	annotate_hbefore.vgtest			    \
	annotate_order_1.stderr.exp		    \
	annotate_order_1.vgtest			    \
	annotate_order_2.stderr.exp		    \
	annotate_order_2.vgtest			    \
	annotate_order_3.stderr.exp		    \
	annotate_order_3.vgtest			    \
	annotate_publish_hg.stderr.exp		    \
	annotate_publish_hg.vgtest		    \
	annotate_sem.stderr.exp                     \
	annotate_sem.vgtest                         \
	annotate_smart_pointer.stderr.exp	    \
	annotate_smart_pointer.vgtest		    \
	annotate_smart_pointer2.stderr.exp	    \
	annotate_smart_pointer2.stderr.exp-darwin   \
	annotate_smart_pointer2.vgtest		    \
	annotate_spinlock.stderr.exp		    \
	annotate_spinlock.vgtest		    \
	annotate_rwlock.stderr.exp		    \
	annotate_rwlock.vgtest			    \
	annotate_rwlock_hg.stderr.exp		    \
	annotate_rwlock_hg.vgtest		    \
	annotate_ignore_read.stderr.exp		    \
	annotate_ignore_read.vgtest		    \
	annotate_ignore_rw.stderr.exp		    \
	annotate_ignore_rw.vgtest		    \
	annotate_ignore_rw2.stderr.exp		    \
	annotate_ignore_rw2.stderr.exp-solaris	    \
	annotate_ignore_rw2.vgtest		    \
	annotate_ignore_write.stderr.exp	    \
	annotate_ignore_write.stderr.exp-solaris    \
	annotate_ignore_write.vgtest		    \
	annotate_ignore_write2.stderr.exp	    \
	annotate_ignore_write2.stderr.exp-solaris   \
	annotate_ignore_write2.vgtest		    \
	annotate_trace_memory.stderr.exp-arm        \
	annotate_trace_memory.stderr.exp-32bit	    \
	annotate_trace_memory.stderr.exp-32bit-clang \
	annotate_trace_memory.stderr.exp-64bit	    \
	annotate_trace_memory.stderr.exp-mips32	    \
	annotate_trace_memory.vgtest		    \
	annotate_trace_memory_xml.stderr.exp-arm    \
	annotate_trace_memory_xml.stderr.exp-32bit  \
	annotate_trace_memory_xml.stderr.exp-32bit-clang \
	annotate_trace_memory_xml.stderr.exp-64bit  \
	annotate_trace_memory_xml.stderr.exp-mips32 \
	annotate_trace_memory_xml.vgtest	    \
	annotate_sem.stderr.exp                     \
	annotate_sem.vgtest                         \
	annotate_static.stderr.exp	            \
	annotate_static.vgtest		            \
	atomic_var.stderr.exp			    \
	atomic_var.vgtest			    \
	bug322621.vgtest			    \
	bug322621.stderr.exp			    \
	bar_bad.stderr.exp			    \
	bar_bad.stderr.exp-nohang		    \
	bar_bad.stderr.exp-solaris		    \
	bar_bad.stderr.exp-freebsd		    \
	bar_bad.vgtest				    \
	bar_bad_xml.stderr.exp			    \
	bar_bad_xml.stderr.exp-nohang		    \
	bar_bad_xml.stderr.exp-solaris		    \
	bar_bad_xml.stderr.exp-freebsd		    \
	bar_bad_xml.vgtest			    \
	bar_trivial.stderr.exp			    \
	bar_trivial.stdout.exp			    \
	bar_trivial.vgtest			    \
	boost_thread.stderr.exp			    \
	boost_thread.vgtest			    \
	bug-235681.stderr.exp			    \
	bug-235681.vgtest			    \
	circular_buffer.stderr.exp		    \
	circular_buffer.vgtest			    \
	concurrent_close.stderr.exp		    \
	concurrent_close.vgtest			    \
	condvar.stderr.exp			    \
	condvar.vgtest				    \
	custom_alloc.stderr.exp			    \
	custom_alloc.vgtest			    \
	custom_alloc_fiw.stderr.exp		    \
	custom_alloc_fiw.vgtest			    \
	dlopen.stderr.exp			    \
	dlopen.stdout.exp			    \
	dlopen.vgtest				    \
	fork-serial.stderr.exp			    \
	fork-serial.vgtest			    \
	fork-parallel.stderr.exp		    \
	fork-parallel.vgtest			    \
	fp_race.stderr.exp                          \
	fp_race.stderr.exp-mips32-be                \
	fp_race.stderr.exp-mips32-le                \
	fp_race.vgtest                              \
	fp_race2.stderr.exp                         \
	fp_race2.vgtest                             \
	fp_race_xml.stderr.exp                      \
	fp_race_xml.stderr.exp-mips32-be            \
	fp_race_xml.stderr.exp-mips32-le            \
	fp_race_xml.stderr.exp-solaris              \
	fp_race_xml.vgtest                          \
	free_is_write.stderr.exp		    \
	free_is_write.vgtest			    \
	free_is_write2.stderr.exp		    \
	free_is_write2.vgtest			    \
	hg01_all_ok.stderr.exp                      \
	hg01_all_ok.vgtest                          \
	hg02_deadlock.stderr.exp                    \
	hg02_deadlock.vgtest                        \
	hg03_inherit.stderr.exp                     \
	hg03_inherit.vgtest                         \
	hg04_race.stderr.exp                        \
	hg04_race.vgtest                            \
	hg05_race2.stderr.exp                       \
	hg05_race2.stderr.exp-powerpc               \
	hg05_race2.vgtest                           \
	hg06_readshared.stderr.exp                  \
	hg06_readshared.vgtest                      \
	hold_lock_1.stderr.exp                      \
	hold_lock_1.vgtest                          \
	hold_lock_2.stderr.exp                      \
	hold_lock_2.vgtest                          \
	getaddrinfo.stderr.exp                      \
	getaddrinfo.vgtest                          \
	linuxthreads_det.stderr.exp                 \
	linuxthreads_det.stderr.exp-linuxthreads    \
	linuxthreads_det.stdout.exp                 \
	linuxthreads_det.stdout.exp-linuxthreads    \
	linuxthreads_det.vgtest                     \
	local_static.stderr.exp                     \
	local_static.vgtest                         \
	matinv.stderr.exp                           \
	matinv.stdout.exp                           \
	matinv.vgtest                               \
	memory_allocation.stderr.exp		    \
	memory_allocation.vgtest		    \
	monitor_example.stderr.exp		    \
	monitor_example.vgtest			    \
	new_delete.stderr.exp                       \
	new_delete.vgtest                           \
	omp_matinv.stderr.exp                       \
	omp_matinv.stdout.exp                       \
	omp_matinv.vgtest                           \
	omp_matinv_racy.stderr.exp                  \
	omp_matinv_racy.stdout.exp                  \
	omp_matinv_racy.vgtest                      \
	omp_prime_racy.stderr.exp                   \
	omp_prime_racy.vgtest                       \
	omp_printf.stderr.exp                       \
	omp_printf.vgtest                           \
	pth_barrier.stderr.exp                      \
	pth_barrier.vgtest                          \
	pth_barrier2.stderr.exp                     \
	pth_barrier2.vgtest                         \
	pth_barrier3.stderr.exp                     \
	pth_barrier3.vgtest                         \
	pth_barrier_race.stderr.exp                 \
	pth_barrier_race.vgtest                     \
	pth_barrier_reinit.stderr.exp               \
	pth_barrier_reinit.vgtest                   \
	pth_barrier_thr_cr.stderr.exp               \
	pth_barrier_thr_cr.vgtest                   \
	pth_barrier_thr_cr.supp                     \
	pth_broadcast.stderr.exp                    \
	pth_broadcast.vgtest                        \
	pth_cancel_locked.stderr.exp		    \
	pth_cancel_locked.stderr.exp-darwin	    \
	pth_cancel_locked.vgtest		    \
	pth_cleanup_handler.stderr.exp		    \
	pth_cleanup_handler.vgtest		    \
	pth_cond_destroy_busy.stderr.exp            \
	pth_cond_destroy_busy.stderr.exp-solaris    \
	pth_cond_destroy_busy.vgtest                \
	pth_cond_race.stderr.exp                    \
	pth_cond_race.vgtest                        \
	pth_cond_race2.stderr.exp                   \
	pth_cond_race2.vgtest                       \
	pth_cond_race3.stderr.exp                   \
	pth_cond_race3.vgtest                       \
	pth_create_chain.stderr.exp                 \
	pth_create_chain.vgtest                     \
	pth_create_glibc_2_0.stderr.exp             \
	pth_create_glibc_2_0.vgtest                 \
	pth_detached.stderr.exp                     \
	pth_detached.stdout.exp                     \
	pth_detached.vgtest                         \
	pth_detached2.stderr.exp                    \
	pth_detached2.stdout.exp                    \
	pth_detached2.vgtest                        \
	pth_detached3.stderr.exp1                   \
	pth_detached3.stderr.exp2                   \
	pth_detached3.stderr.exp-freebsd            \
	pth_detached3.vgtest                        \
	pth_detached_sem.stderr.exp                 \
	pth_detached_sem.stdout.exp                 \
	pth_detached_sem.vgtest                     \
	pth_inconsistent_cond_wait.stderr.exp1      \
	pth_inconsistent_cond_wait.stderr.exp2      \
	pth_inconsistent_cond_wait.vgtest           \
	pth_mutex_reinit.stderr.exp		    \
	pth_mutex_reinit.vgtest			    \
	pth_mutex_signal.stderr.exp		    \
	pth_mutex_signal.vgtest			    \
	pth_once.stderr.exp			    \
	pth_once.vgtest				    \
	pth_process_shared_mutex.stderr.exp         \
	pth_process_shared_mutex.vgtest             \
	pth_spinlock.stderr.exp                     \
	pth_spinlock.vgtest                         \
	pth_uninitialized_cond.stderr.exp           \
	pth_uninitialized_cond.vgtest               \
	read_and_free_race.stderr.exp		    \
	read_and_free_race.vgtest		    \
	recursive_mutex.stderr.exp-linux            \
	recursive_mutex.stderr.exp-darwin           \
	recursive_mutex.stderr.exp-solaris          \
	recursive_mutex.stderr.exp-freebsd          \
	recursive_mutex.vgtest                      \
	rwlock_race.stderr.exp	                    \
	rwlock_race.stderr.exp2	                    \
	rwlock_race.vgtest                          \
	rwlock_test.stderr.exp	                    \
	rwlock_test.vgtest                          \
	rwlock_type_checking.stderr.exp	            \
	rwlock_type_checking.vgtest                 \
	sem_as_mutex.stderr.exp                     \
	sem_as_mutex.stderr.exp-mips32-be           \
	sem_as_mutex.stderr.exp-mips32-le           \
	sem_as_mutex.vgtest                         \
	sem_as_mutex2.stderr.exp                    \
	sem_as_mutex2.vgtest                        \
	sem_as_mutex3.stderr.exp                    \
	sem_as_mutex3.stderr.exp-mips32-be          \
	sem_as_mutex3.stderr.exp-mips32-le          \
	sem_as_mutex3.vgtest                        \
	sem_open.stderr.exp                         \
	sem_open.stderr.exp-mips32-be               \
	sem_open.stderr.exp-mips32-le               \
	sem_open.vgtest                             \
	sem_open2.stderr.exp                        \
	sem_open2.vgtest                            \
	sem_open3.stderr.exp                        \
	sem_open3.stderr.exp-mips32-be              \
	sem_open3.stderr.exp-mips32-le              \
	sem_open3.vgtest                            \
	sem_open_traced.stderr.exp                  \
	sem_open_traced.vgtest                      \
	sem_wait.stderr.exp                         \
	sem_wait.vgtest                             \
	shared_timed_mutex.stderr.exp               \
	shared_timed_mutex.vgtest                   \
	swapcontext.stderr.exp                      \
	swapcontext.vgtest                          \
	sigalrm.stderr.exp                          \
	sigalrm.vgtest                              \
	sigaltstack.stderr.exp                      \
	sigaltstack.vgtest                          \
	std_atomic.stderr.exp		  	    \
	std_atomic.vgtest			    \
	std_list.stderr.exp		  	    \
	std_list.vgtest			            \
	std_mutex.stderr.exp		  	    \
	std_mutex.vgtest			    \
	std_string.stderr.exp		  	    \
	std_string.vgtest			    \
	std_thread.stderr.exp		  	    \
	std_thread.vgtest			    \
	std_thread2.stderr.exp		  	    \
	std_thread2.vgtest			    \
	std_thread2.supp			    \
	str_tester.stderr.exp		  	    \
	str_tester.vgtest			    \
	tc01_simple_race.stderr.exp                 \
	tc01_simple_race.vgtest                     \
	tc02_simple_tls.stderr.exp                  \
	tc02_simple_tls.vgtest                      \
	tc03_re_excl.stderr.exp                     \
	tc03_re_excl.vgtest                         \
	tc04_free_lock.stderr.exp-x86               \
	tc04_free_lock.stderr.exp-ppc               \
	tc04_free_lock.stderr.exp-s390              \
	tc04_free_lock.vgtest                       \
	tc05_simple_race.stderr.exp                 \
	tc05_simple_race.vgtest                     \
	tc06_two_races.stderr.exp                   \
	tc06_two_races.vgtest                       \
	tc07_hbl1.stderr.exp                        \
	tc07_hbl1.stdout.exp                        \
	tc07_hbl1.vgtest                            \
	tc08_hbl2.stderr.exp                        \
	tc08_hbl2.stdout.exp                        \
	tc08_hbl2.vgtest                            \
	tc09_bad_unlock.stderr.exp-x86              \
	tc09_bad_unlock.stderr.exp-ppc              \
	tc09_bad_unlock.stderr.exp-glibc2.8         \
	tc09_bad_unlock.stderr.exp-s390             \
	tc09_bad_unlock.stderr.exp-solaris          \
	tc09_bad_unlock.stderr.exp-freebsd          \
	tc09_bad_unlock.stderr.exp-freebsd-clang    \
	tc09_bad_unlock.vgtest                      \
	tc10_rec_lock.stderr.exp                    \
	tc10_rec_lock.vgtest                        \
	tc11_XCHG.stderr.exp tc11_XCHG.stdout.exp   \
	tc11_XCHG.vgtest                            \
	tc12_rwl_trivial.stderr.exp                 \
	tc12_rwl_trivial.vgtest                     \
	tc13_laog1.stderr.exp                       \
	tc13_laog1.vgtest                           \
	tc15_laog_lockdel.stderr.exp                \
	tc15_laog_lockdel.vgtest                    \
	tc16_byterace.stderr.exp                    \
	tc16_byterace.vgtest                        \
	tc17_sembar.stderr.exp                      \
	tc17_sembar.vgtest                          \
	tc18_semabuse.stderr.exp                    \
	tc18_semabuse.stderr.exp-solaris            \
	tc18_semabuse.vgtest                        \
	tc19_shadowmem.stderr.exp-32bit             \
	tc19_shadowmem.stderr.exp-32bit-BE          \
	tc19_shadowmem.stderr.exp-64bit             \
	tc19_shadowmem.vgtest                       \
	tc21_pthonce.stderr.exp                     \
	tc21_pthonce.stdout.exp                     \
	tc21_pthonce.vgtest                         \
	tc22_exit_w_lock.stderr.exp-32bit	    \
	tc22_exit_w_lock.stderr.exp-64bit           \
	tc22_exit_w_lock.vgtest                     \
	tc23_bogus_condwait.stderr.exp-linux-x86    \
	tc23_bogus_condwait.stderr.exp-linux-ppc    \
	tc23_bogus_condwait.stderr.exp-darwin-x86   \
	tc23_bogus_condwait.stderr.exp-darwin-amd64 \
	tc23_bogus_condwait.stderr.exp-freebsd      \
	tc23_bogus_condwait.vgtest                  \
	tc24_nonzero_sem.stderr.exp                 \
	tc24_nonzero_sem.vgtest                     \
	thrd_create.stderr.exp			    \
	thrd_create.vgtest			    \
	thread_name.stderr.exp			    \
	thread_name.vgtest			    \
	thread_name_xml.stderr.exp		    \
	thread_name_xml.vgtest			    \
	thread_name_freebsd.stderr.exp		    \
	thread_name_freebsd.vgtest		    \
	threaded-fork.stderr.exp		    \
	threaded-fork.vgtest			    \
	threaded-fork-vcs.stderr.exp		    \
	threaded-fork-vcs.vgtest		    \
	timed_mutex.stderr.exp                      \
	timed_mutex.vgtest                          \
	tls_threads.stderr.exp			    \
	tls_threads.vgtest			    \
	trylock.stderr.exp                          \
	trylock.vgtest				    \
	unit_bitmap.stderr.exp                      \
	unit_bitmap.vgtest                          \
	unit_vc.stderr.exp                          \
	unit_vc.vgtest


check_PROGRAMS =      \
  annotate_hb_err     \
  annotate_hb_race    \
  annotate_ignore_rw  \
  annotate_ignore_write \
  annotate_publish_hg \
  annotate_static     \
  annotate_trace_memory \
  bug-235681          \
  custom_alloc        \
  concurrent_close    \
  dlopen_main         \
  dlopen_lib.so       \
  fork                \
  fp_race             \
  free_is_write	      \
  hold_lock           \
  linuxthreads_det    \
  local_static        \
  memory_allocation   \
  monitor_example     \
  new_delete          \
  pth_broadcast       \
  pth_cancel_locked   \
  pth_cleanup_handler \
  pth_cond_destroy_busy \
  pth_cond_race       \
  pth_create_chain    \
  pth_detached        \
  pth_detached_sem    \
  pth_detached3       \
  pth_inconsistent_cond_wait \
  pth_mutex_reinit    \
  pth_mutex_signal    \
  pth_process_shared_mutex   \
  recursive_mutex     \
  rwlock_race         \
  rwlock_test         \
  rwlock_type_checking \
  sem_as_mutex        \
  sem_open            \
  sigalrm             \
  threaded-fork       \
  trylock             \
  unit_bitmap         \
  unit_vc             \
  std_atomic          \
  std_list            \
  std_mutex	      \
  std_string

if HAVE_BOOST_1_35
check_PROGRAMS += boost_thread
endif

if HAVE_BUILTIN_ATOMIC
check_PROGRAMS +=        \
  annotate_barrier       \
  annotate_rwlock        \
  annotate_sem           \
  atomic_var             \
  circular_buffer
endif

if HAVE_BUILTIN_ATOMIC_CXX
check_PROGRAMS +=        \
  annotate_smart_pointer
endif

if !VGCONF_OS_IS_FREEBSD
check_PROGRAMS += tsan_unittest \
  pth_uninitialized_cond
endif

# clang 3.3 will generate errors about libstdc++ headers from 
# gcc (Ubuntu/Linaro 4.6.3-1ubuntu5) 4.6.3
if CXX_CAN_INCLUDE_THREAD_HEADER
if HAVE_SHARED_POINTER_ANNOTATION
check_PROGRAMS += \
  std_thread      \
  std_thread2
endif
endif

if CXX_CAN_INCLUDE_CONDITION_VARIABLE_HEADER
check_PROGRAMS += \
    condvar
endif

if CXX_CAN_USE_SHARED_TIMED_MUTEX
check_PROGRAMS += \
    shared_timed_mutex
endif

if CXX_CAN_USE_TIMED_MUTEX
check_PROGRAMS += \
    timed_mutex
endif

if HAVE_OPENMP
check_PROGRAMS += omp_matinv omp_prime omp_printf
endif

if HAVE_PTHREAD_BARRIER
check_PROGRAMS +=		\
	bug322621		\
	matinv			\
	pth_barrier		\
	pth_barrier_race	\
	pth_barrier_reinit	\
	pth_barrier_thr_cr	\
	thread_name
endif

if HAVE_PTHREAD_CREATE_GLIBC_2_0
check_PROGRAMS += pth_create_glibc_2_0
endif

if HAVE_PTHREAD_SPINLOCK
check_PROGRAMS += pth_spinlock
endif

if HAVE_SWAPCONTEXT
check_PROGRAMS += swapcontext
endif

if HAVE_THRD_CREATE
check_PROGRAMS += thrd_create
endif

if !VGCONF_OS_IS_DARWIN
check_PROGRAMS += sem_wait
endif

if VGCONF_OS_IS_FREEBSD
check_PROGRAMS += thread_name_freebsd
endif


AM_CFLAGS   += $(AM_FLAG_M3264_PRI) @FLAG_W_EXTRA@ -Wno-inline -Wno-unused-parameter
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI) @FLAG_W_EXTRA@ @FLAG_FALIGNED_NEW@ \
	-Wno-inline -Wno-unused-parameter

LDADD = -lpthread

annotate_sem_CFLAGS         = $(AM_CFLAGS) @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@
annotate_rwlock_CFLAGS      = $(AM_CFLAGS) @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@
bug322621_SOURCES           = bug322621.cpp
condvar_SOURCES		    = condvar.cpp
condvar_CXXFLAGS            = $(AM_CXXFLAGS) -std=c++0x
concurrent_close_SOURCES    = concurrent_close.cpp
if !VGCONF_OS_IS_FREEBSD
dlopen_main_LDADD           = -ldl
endif
dlopen_lib_so_SOURCES       = dlopen_lib.c
dlopen_lib_so_CFLAGS        = -fPIC
dlopen_lib_so_LDFLAGS       = -shared -fPIC
monitor_example_SOURCES     = monitor_example.cpp
new_delete_SOURCES          = new_delete.cpp
new_delete_CXXFLAGS         = $(AM_CXXFLAGS) @FLAG_W_NO_MISMATCHED_NEW_DELETE@

free_is_write_CFLAGS        = $(AM_CFLAGS)
pth_barrier_CFLAGS          = $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
free_is_write_CFLAGS        += -D__EXTENSIONS__
pth_barrier_CFLAGS          += -D__EXTENSIONS__
endif

pth_create_chain_CFLAGS     = $(AM_CFLAGS)
pth_detached_CFLAGS         = $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
pth_create_chain_CFLAGS     += -D__EXTENSIONS__
pth_detached_CFLAGS         += -D__EXTENSIONS__
endif

tsan_unittest_SOURCES       = tsan_unittest.cpp
tsan_unittest_CXXFLAGS      = $(AM_CXXFLAGS) \
			-DTHREAD_WRAPPERS='"tsan_thread_wrappers_pthread.h"'

unit_bitmap_CFLAGS          = $(AM_CFLAGS) -O2 \
                              -DENABLE_DRD_CONSISTENCY_CHECKS \
                              @FLAG_UNLIMITED_INLINE_UNIT_GROWTH@
unit_bitmap_LDADD           = # nothing, i.e. not -lpthread

unit_vc_CFLAGS              = $(AM_CFLAGS) -DENABLE_DRD_CONSISTENCY_CHECKS
unit_vc_LDADD               = # nothing, i.e. not -lpthread

if HAVE_BOOST_1_35
boost_thread_SOURCES        = boost_thread.cpp
boost_thread_CXXFLAGS       = $(AM_CXXFLAGS) $(BOOST_CFLAGS)
boost_thread_LDADD          = $(BOOST_LIBS)
endif

if HAVE_BUILTIN_ATOMIC
annotate_smart_pointer_SOURCES = annotate_smart_pointer.cpp
endif

annotate_static_SOURCES     = annotate_static.cpp
annotate_static_CXXFLAGS    = $(AM_CXXFLAGS) @FLAG_W_NO_UNUSED_FUNCTION@

local_static_SOURCES        = local_static.cpp

if HAVE_OPENMP
omp_matinv_CFLAGS           = $(AM_CFLAGS) -fopenmp
omp_matinv_LDFLAGS          = -fopenmp
omp_matinv_LDADD            = $(LDADD) -lm

omp_prime_CFLAGS            = $(AM_CFLAGS) -fopenmp
omp_prime_LDFLAGS           = -fopenmp
omp_prime_LDADD             = $(LDADD) -lm

omp_printf_CFLAGS           = $(AM_CFLAGS) -fopenmp
omp_printf_LDFLAGS          = -fopenmp
omp_printf_LDADD            = $(LDADD) -lm
endif

matinv_CFLAGS               = $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
matinv_CFLAGS               += -D__EXTENSIONS__
endif
if HAVE_PTHREAD_BARRIER
matinv_LDADD                = $(LDADD) -lm
endif

rwlock_test_CFLAGS          = $(AM_CFLAGS) @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@
if VGCONF_OS_IS_SOLARIS
rwlock_test_CFLAGS          += -D__EXTENSIONS__
endif

shared_timed_mutex_SOURCES  = shared_timed_mutex.cpp
shared_timed_mutex_CXXFLAGS = $(AM_CXXFLAGS) -std=c++1y

std_atomic_SOURCES          = std_atomic.cpp
std_atomic_CXXFLAGS         = $(AM_CXXFLAGS) -std=c++0x @FLAG_W_NO_SIGN_COMPARE@

std_list_SOURCES            = std_list.cpp
std_list_CXXFLAGS           = $(AM_CXXFLAGS) -std=c++0x @FLAG_W_NO_SIGN_COMPARE@

std_mutex_SOURCES           = std_mutex.cpp
std_mutex_CXXFLAGS          = $(AM_CXXFLAGS) -std=c++0x @FLAG_W_NO_SIGN_COMPARE@
std_mutex_LDFLAGS           = -pthread

std_string_SOURCES          = std_string.cpp
std_string_CXXFLAGS         = $(AM_CXXFLAGS) -std=c++0x @FLAG_W_NO_SIGN_COMPARE@

# Note: -Wl,--no-as-needed is a workaround for
# https://bugs.launchpad.net/ubuntu/+source/gcc-defaults/+bug/1228201

std_thread_SOURCES          = std_thread.cpp
std_thread_CXXFLAGS         = $(AM_CXXFLAGS) -std=c++0x
if !VGCONF_OS_IS_SOLARIS
std_thread_LDFLAGS          = -Wl,--no-as-needed
endif

std_thread2_SOURCES          = std_thread2.cpp
std_thread2_CXXFLAGS         = $(AM_CXXFLAGS) -std=c++0x
if !VGCONF_OS_IS_SOLARIS
std_thread2_LDFLAGS          = -Wl,--no-as-needed
endif

sem_wait_SOURCES            = sem_wait.cpp

swapcontext_SOURCES         = swapcontext.c
swapcontext_CFLAGS          = $(AM_CFLAGS) -g
if VGCONF_OS_IS_SOLARIS
swapcontext_CFLAGS          += -D__EXTENSIONS__
endif

timed_mutex_SOURCES         = timed_mutex.cpp
timed_mutex_CXXFLAGS        = $(AM_CXXFLAGS) -std=c++0x

thrd_create_SOURCES	    = thrd_create.c
if VGCONF_OS_IS_FREEBSD
thrd_create_LDFLAGS          = -lstdthreads
endif
