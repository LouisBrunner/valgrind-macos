
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS =		    \
	compare_error_count_with    \
	filter_annotate_barrier_xml \
	filter_error_count	    \
	filter_error_summary	    \
	filter_stderr               \
	filter_stderr_and_thread_no \
	filter_stderr_and_thread_no_and_offset \
	filter_thread_no	    \
	filter_xml_and_thread_no    \
	run_openmp_test             \
	supported_libpthread	    \
	supported_sem_init

noinst_HEADERS =                                    \
	tsan_thread_wrappers_pthread.h		    \
	unified_annotations.h

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
	annotate_ignore_rw2.vgtest		    \
	annotate_ignore_write.stderr.exp	    \
	annotate_ignore_write.vgtest		    \
	annotate_ignore_write2.stderr.exp	    \
	annotate_ignore_write2.vgtest		    \
	annotate_trace_memory.stderr.exp-32bit	    \
	annotate_trace_memory.stderr.exp-64bit	    \
	annotate_trace_memory.stderr.exp-mips32	   \
	annotate_trace_memory.vgtest		    \
	annotate_trace_memory_xml.stderr.exp-32bit  \
	annotate_trace_memory_xml.stderr.exp-64bit  \
	annotate_trace_memory_xml.stderr.exp-mips32 \
	annotate_trace_memory_xml.vgtest	    \
	annotate_sem.stderr.exp                     \
	annotate_sem.vgtest                         \
	annotate_static.stderr.exp	            \
	annotate_static.vgtest		            \
	atomic_var.stderr.exp			    \
	atomic_var.vgtest			    \
	bar_bad.stderr.exp			    \
	bar_bad.vgtest				    \
	bar_bad_xml.stderr.exp			    \
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
	custom_alloc.stderr.exp			    \
	custom_alloc.vgtest			    \
	custom_alloc_fiw.stderr.exp		    \
	custom_alloc_fiw.vgtest			    \
	fp_race.stderr.exp                          \
	fp_race.stderr.exp-mips32-be                \
	fp_race.stderr.exp-mips32-le                \
	fp_race.vgtest                              \
	fp_race2.stderr.exp                         \
	fp_race2.vgtest                             \
	fp_race_xml.stderr.exp                      \
	fp_race_xml.stderr.exp-mips32-be            \
	fp_race_xml.stderr.exp-mips32-le            \
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
	linuxthreads_det.stderr.exp                 \
	linuxthreads_det.stderr.exp-linuxthreads    \
	linuxthreads_det.stdout.exp                 \
	linuxthreads_det.stdout.exp-linuxthreads    \
	linuxthreads_det.vgtest                     \
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
	pth_detached3.vgtest                        \
	pth_detached_sem.stderr.exp                 \
	pth_detached_sem.stdout.exp                 \
	pth_detached_sem.vgtest                     \
	pth_inconsistent_cond_wait.stderr.exp1      \
	pth_inconsistent_cond_wait.stderr.exp2      \
	pth_inconsistent_cond_wait.vgtest           \
	pth_mutex_reinit.stderr.exp		    \
	pth_mutex_reinit.vgtest			    \
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
	sigalrm.stderr.exp                          \
	sigalrm.vgtest                              \
	sigaltstack.stderr.exp                      \
	sigaltstack.vgtest                          \
	std_atomic.stderr.exp		  	    \
	std_atomic.vgtest			            \
	std_list.stderr.exp		  	    \
	std_list.vgtest			            \
	std_string.stderr.exp		  	    \
	std_string.vgtest			    \
	std_thread.stderr.exp		  	    \
	std_thread.vgtest			    \
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
	tc18_semabuse.vgtest                        \
	tc19_shadowmem.stderr.exp-32bit             \
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
	tc23_bogus_condwait.vgtest                  \
	tc24_nonzero_sem.stderr.exp                 \
	tc24_nonzero_sem.vgtest                     \
	thread_name.stderr.exp			    \
	thread_name.vgtest			    \
	thread_name_xml.stderr.exp		    \
	thread_name_xml.vgtest			    \
	threaded-fork.stderr.exp		    \
	threaded-fork.vgtest			    \
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
  fp_race             \
  free_is_write	      \
  hold_lock           \
  linuxthreads_det    \
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
  pth_process_shared_mutex   \
  pth_uninitialized_cond \
  recursive_mutex     \
  rwlock_race         \
  rwlock_test         \
  rwlock_type_checking \
  sem_as_mutex        \
  sem_open            \
  sigalrm             \
  thread_name         \
  threaded-fork       \
  trylock             \
  unit_bitmap         \
  unit_vc

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
  annotate_smart_pointer \
  tsan_unittest
endif

# clang 3.3 will generate errors about libstdc++ headers from 
# gcc (Ubuntu/Linaro 4.6.3-1ubuntu5) 4.6.3
if CXX_CAN_INCLUDE_THREAD_HEADER
if HAVE_SHARED_POINTER_ANNOTATION
check_PROGRAMS += \
  std_atomic      \
  std_list        \
  std_string      \
  std_thread
endif
endif

if HAVE_OPENMP
check_PROGRAMS += omp_matinv omp_prime omp_printf
endif

if HAVE_PTHREAD_BARRIER
check_PROGRAMS += matinv pth_barrier pth_barrier_race pth_barrier_reinit \
	pth_barrier_thr_cr
endif

if HAVE_PTHREAD_CREATE_GLIBC_2_0
check_PROGRAMS += pth_create_glibc_2_0
endif

if HAVE_PTHREAD_SPINLOCK
check_PROGRAMS += pth_spinlock
endif

if !VGCONF_OS_IS_DARWIN
check_PROGRAMS += sem_wait
endif


AM_CFLAGS   += $(AM_FLAG_M3264_PRI) @FLAG_W_EXTRA@ -Wno-inline -Wno-unused-parameter
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI) @FLAG_W_EXTRA@ -Wno-inline -Wno-unused-parameter

LDADD = -lpthread


concurrent_close_SOURCES    = concurrent_close.cpp
monitor_example_SOURCES     = monitor_example.cpp
new_delete_SOURCES          = new_delete.cpp

pth_cleanup_handler_CFLAGS  = $(AM_CFLAGS) @FLAG_W_NO_EMPTY_BODY@

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

if HAVE_PTHREAD_BARRIER
matinv_LDADD                = $(LDADD) -lm
endif

std_atomic_SOURCES          = std_atomic.cpp
std_atomic_CXXFLAGS         = $(AM_CXXFLAGS) -std=c++0x -Wno-sign-compare

std_list_SOURCES            = std_list.cpp
std_list_CXXFLAGS           = $(AM_CXXFLAGS) -std=c++0x -Wno-sign-compare

std_string_SOURCES          = std_string.cpp
std_string_CXXFLAGS         = $(AM_CXXFLAGS) -std=c++0x -Wno-sign-compare

std_thread_SOURCES          = std_thread.cpp
std_thread_CXXFLAGS         = $(AM_CXXFLAGS) -std=c++0x

sem_wait_SOURCES            = sem_wait.cpp
