
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .

DIST_SUBDIRS = .

noinst_SCRIPTS =		\
	filter_error_summary	\
	filter_stderr           \
	run_openmp_test         \
	supported_libpthread

EXTRA_DIST =                                        \
	$(noinst_SCRIPTS)                           \
	atomic_var.stderr.exp-with-atomic-builtins-1  \
	atomic_var.stderr.exp-with-atomic-builtins-2  \
	atomic_var.stderr.exp-without-atomic-builtins \
	atomic_var.vgtest			    \
	bar_bad.stderr.exp			    \
	bar_bad.vgtest				    \
	bar_trivial.stdout.exp			    \
	bar_trivial.stderr.exp			    \
	bar_trivial.vgtest			    \
	boost_thread.stderr.exp			    \
	boost_thread.vgtest			    \
	circular_buffer.stderr.exp-with-atomic-builtins		\
	circular_buffer.stderr.exp-without-atomic-builtins	\
	circular_buffer.vgtest			    \
	drd_bitmap_test.stderr.exp                  \
	drd_bitmap_test.stdout.exp                  \
	drd_bitmap_test.vgtest                      \
	fp_race.stderr.exp                          \
	fp_race.vgtest                              \
	fp_race2.stderr.exp                         \
	fp_race2.vgtest                             \
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
	pth_broadcast.stderr.exp                    \
	pth_broadcast.vgtest                        \
	pth_cancel_locked.stderr.exp		    \
	pth_cancel_locked.vgtest		    \
	pth_cond_race.stderr.exp                    \
	pth_cond_race.vgtest                        \
	pth_cond_race2.stderr.exp                   \
	pth_cond_race2.vgtest                       \
	pth_cond_race3.stderr.exp                   \
	pth_cond_race3.vgtest                       \
	pth_create_chain.stderr.exp                 \
	pth_create_chain.stderr.exp-ppc             \
	pth_create_chain.vgtest                     \
	pth_detached.stderr.exp                     \
	pth_detached.stdout.exp                     \
	pth_detached.vgtest                         \
	pth_detached2.stderr.exp                    \
	pth_detached2.stdout.exp                    \
	pth_detached2.vgtest                        \
	pth_detached_sem.stderr.exp                 \
	pth_detached_sem.stdout.exp                 \
	pth_detached_sem.vgtest                     \
	pth_inconsistent_cond_wait.stderr.exp1      \
	pth_inconsistent_cond_wait.stderr.exp2      \
	pth_inconsistent_cond_wait.vgtest           \
	pth_spinlock.stderr.exp                     \
	pth_spinlock.vgtest                         \
	qt4_mutex.stderr.exp                        \
	qt4_mutex.vgtest                            \
	qt4_rwlock.stderr.exp                       \
	qt4_rwlock.vgtest                           \
	qt4_semaphore.stderr.exp                    \
	qt4_semaphore.vgtest                        \
	recursive_mutex.stderr.exp	            \
	recursive_mutex.stdout.exp	            \
	recursive_mutex.vgtest                      \
	rwlock_race.stderr.exp	                    \
	rwlock_race.stderr.exp2	                    \
	rwlock_race.vgtest                          \
	rwlock_test.stderr.exp	                    \
	rwlock_test.vgtest                          \
	sem_as_mutex.stderr.exp                     \
	sem_as_mutex.vgtest                         \
	sem_as_mutex2.stderr.exp                    \
	sem_as_mutex2.vgtest                        \
	sigalrm.stderr.exp                          \
	sigalrm.vgtest                              \
	tc01_simple_race.stderr.exp                 \
	tc01_simple_race.vgtest                     \
	tc02_simple_tls.stderr.exp                  \
	tc02_simple_tls.vgtest                      \
	tc03_re_excl.stderr.exp                     \
	tc03_re_excl.vgtest                         \
	tc04_free_lock.stderr.exp                   \
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
	tc09_bad_unlock.stderr.exp                  \
	tc09_bad_unlock.stderr.exp-glibc2.8         \
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
	tc19_shadowmem.stderr.exp                   \
	tc19_shadowmem.vgtest                       \
	tc20_verifywrap.stderr.exp-glibc2.3         \
	tc20_verifywrap.stderr.exp-glibc2.5         \
	tc20_verifywrap.stderr.exp-glibc2.5-ppc     \
	tc20_verifywrap.stderr.exp-glibc2.8         \
	tc20_verifywrap.vgtest                      \
	tc20_verifywrap2.stderr.exp-glibc2.3        \
	tc20_verifywrap2.stderr.exp-glibc2.3-b      \
	tc20_verifywrap2.stderr.exp-glibc2.5        \
	tc20_verifywrap2.stderr.exp-glibc2.5-ppc    \
	tc20_verifywrap2.stderr.exp-glibc2.8        \
	tc20_verifywrap2.vgtest                     \
	tc21_pthonce.stderr.exp                     \
	tc21_pthonce.stdout.exp                     \
	tc21_pthonce.vgtest                         \
	tc22_exit_w_lock.stderr.exp-32bit	    \
	tc22_exit_w_lock.stderr.exp-64bit           \
	tc22_exit_w_lock.vgtest                     \
	tc23_bogus_condwait.stderr.exp              \
	tc23_bogus_condwait.vgtest                  \
	tc24_nonzero_sem.stderr.exp                 \
	tc24_nonzero_sem.vgtest                     \
	trylock.stderr.exp                          \
	trylock.vgtest

check_PROGRAMS =      \
  atomic_var          \
  circular_buffer     \
  drd_bitmap_test     \
  fp_race             \
  hold_lock           \
  linuxthreads_det    \
  memory_allocation   \
  monitor_example     \
  new_delete          \
  pth_broadcast       \
  pth_cancel_locked   \
  pth_cond_race       \
  pth_create_chain    \
  pth_detached        \
  pth_detached_sem    \
  pth_inconsistent_cond_wait \
  recursive_mutex     \
  rwlock_race         \
  rwlock_test         \
  sem_as_mutex        \
  sigalrm             \
  trylock

if HAVE_BOOST_1_35
check_PROGRAMS += boost_thread
endif

if HAVE_OPENMP
check_PROGRAMS += omp_matinv omp_prime omp_printf
endif

if HAVE_PTHREAD_BARRIER
check_PROGRAMS += matinv pth_barrier pth_barrier_race pth_barrier_reinit
endif

if HAVE_PTHREAD_SPINLOCK
check_PROGRAMS += pth_spinlock
endif

if HAVE_QTCORE
check_PROGRAMS += qt4_mutex qt4_rwlock qt4_semaphore
endif


AM_CFLAGS   += $(AM_FLAG_M3264_PRI) @FLAG_W_EXTRA@ -Wno-inline -Wno-unused-parameter
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI) @FLAG_W_EXTRA@ -Wno-inline -Wno-unused-parameter

LDADD = -lpthread


drd_bitmap_test_CFLAGS      = $(AM_CFLAGS) -O2 \
                              -DENABLE_DRD_CONSISTENCY_CHECKS\
                              @FLAG_UNLIMITED_INLINE_UNIT_GROWTH@
drd_bitmap_test_LDADD       = # nothing, ie. not -lpthread

monitor_example_SOURCES     = monitor_example.cpp
new_delete_SOURCES          = new_delete.cpp

if HAVE_BOOST_1_35
boost_thread_SOURCES        = boost_thread.cpp
boost_thread_CXXFLAGS       = $(AM_CXXFLAGS) $(BOOST_CFLAGS)
boost_thread_LDADD          = $(BOOST_LIBS)
endif

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

if HAVE_QTCORE
qt4_mutex_SOURCES           = qt4_mutex.cpp
qt4_mutex_CXXFLAGS          = $(AM_CXXFLAGS) $(QTCORE_CFLAGS)
qt4_mutex_LDADD             = $(LDADD) $(QTCORE_LIBS)

qt4_rwlock_SOURCES          = qt4_rwlock.cpp
qt4_rwlock_CXXFLAGS         = $(AM_CXXFLAGS) $(QTCORE_CFLAGS)
qt4_rwlock_LDADD            = $(LDADD) $(QTCORE_LIBS)

qt4_semaphore_SOURCES       = qt4_semaphore.cpp
qt4_semaphore_CXXFLAGS      = $(AM_CXXFLAGS) $(QTCORE_CFLAGS)
qt4_semaphore_LDADD         = $(LDADD) $(QTCORE_LIBS)
endif
