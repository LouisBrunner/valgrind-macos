# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .

DIST_SUBDIRS = .

noinst_SCRIPTS =		\
	filter_cmdline0		\
	filter_error_summary	\
	filter_fdleak		\
	filter_linenos		\
	filter_none_discards	\
	filter_stderr           \
	run_openmp_test         \
	supported_libpthread

EXTRA_DIST =                                        \
	$(noinst_SCRIPTS)                           \
	atomic_var.stderr.exp-with-atomic-builtins    \
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
	pth_inconsistent_cond_wait.stderr.exp       \
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

AM_CFLAGS   = $(WERROR) -Wall @FLAG_W_EXTRA@ -Wno-inline -Wno-unused-parameter\
              -g $(AM_FLAG_M3264_PRI) -DVGA_$(VG_ARCH_PRI)=1 -DVGO_$(VG_OS)=1 \
              -DVGP_$(VG_ARCH_PRI)_$(VG_OS)=1
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include \
              -I$(top_builddir)/include -I$(top_srcdir)/VEX/pub
AM_CXXFLAGS = $(AM_CFLAGS)

check_PROGRAMS =      \
  atomic_var          \
  bar_bad	      \
  bar_trivial         \
  circular_buffer     \
  drd_bitmap_test     \
  fp_race             \
  hg01_all_ok         \
  hg02_deadlock       \
  hg03_inherit        \
  hg04_race           \
  hg05_race2          \
  hg06_readshared     \
  hold_lock           \
  linuxthreads_det    \
  matinv              \
  memory_allocation   \
  monitor_example     \
  new_delete          \
  pth_barrier         \
  pth_barrier_reinit  \
  pth_broadcast       \
  pth_cancel_locked   \
  pth_cond_race       \
  pth_create_chain    \
  pth_detached        \
  pth_detached_sem    \
  pth_inconsistent_cond_wait \
  pth_spinlock        \
  recursive_mutex     \
  rwlock_race         \
  rwlock_test         \
  sem_as_mutex        \
  sigalrm             \
  tc01_simple_race    \
  tc02_simple_tls     \
  tc03_re_excl        \
  tc04_free_lock      \
  tc05_simple_race    \
  tc06_two_races      \
  tc07_hbl1           \
  tc08_hbl2           \
  tc09_bad_unlock     \
  tc10_rec_lock       \
  tc11_XCHG           \
  tc12_rwl_trivial    \
  tc13_laog1          \
  tc15_laog_lockdel   \
  tc16_byterace       \
  tc17_sembar         \
  tc18_semabuse       \
  tc19_shadowmem      \
  tc20_verifywrap     \
  tc21_pthonce        \
  tc22_exit_w_lock    \
  tc23_bogus_condwait \
  tc24_nonzero_sem    \
  trylock

if HAVE_BOOST_1_35
check_PROGRAMS += boost_thread
endif

if HAVE_OPENMP
check_PROGRAMS += omp_matinv omp_prime omp_printf
endif

if HAVE_QTCORE
check_PROGRAMS += qt4_mutex qt4_rwlock qt4_semaphore
endif


atomic_var_SOURCES          = atomic_var.c
atomic_var_LDADD            = -lpthread

bar_bad_SOURCES             = ../../helgrind/tests/bar_bad.c
bar_bad_LDADD               = -lpthread

bar_trivial_SOURCES         = ../../helgrind/tests/bar_trivial.c
bar_trivial_LDADD           = -lpthread

circular_buffer_SOURCES     = circular_buffer.c
circular_buffer_LDADD       = -lpthread

drd_bitmap_test_SOURCES     = drd_bitmap_test.c
drd_bitmap_test_CFLAGS      = $(AM_CFLAGS) -O2 \
                              -DENABLE_DRD_CONSISTENCY_CHECKS\
                              @FLAG_UNLIMITED_INLINE_UNIT_GROWTH@

fp_race_SOURCES             = fp_race.c
fp_race_LDADD               = -lpthread

hg01_all_ok_SOURCES         = ../../helgrind/tests/hg01_all_ok.c
hg01_all_ok_LDADD           = -lpthread

hg02_deadlock_SOURCES       = ../../helgrind/tests/hg02_deadlock.c
hg02_deadlock_LDADD         = -lpthread

hg03_inherit_SOURCES        = ../../helgrind/tests/hg03_inherit.c
hg03_inherit_LDADD          = -lpthread

hg04_race_SOURCES           = ../../helgrind/tests/hg04_race.c
hg04_race_LDADD             = -lpthread

hg05_race2_SOURCES          = ../../helgrind/tests/hg05_race2.c
hg05_race2_LDADD            = -lpthread

hg06_readshared_SOURCES     = ../../helgrind/tests/hg06_readshared.c
hg06_readshared_LDADD       = -lpthread

hold_lock_SOURCES           = hold_lock.c
hold_lock_LDADD             = -lpthread

linuxthreads_det_SOURCES    = linuxthreads_det.c
linuxthreads_det_LDADD      = -lpthread

matinv_SOURCES              = matinv.c
matinv_LDADD                = -lpthread -lm

memory_allocation_SOURCES   = memory_allocation.c

monitor_example_SOURCES     = monitor_example.cpp
monitor_example_LDADD       = -lpthread

new_delete_SOURCES          = new_delete.cpp

pth_barrier_SOURCES         = pth_barrier.c
pth_barrier_LDADD           = -lpthread

pth_barrier_reinit_SOURCES  = pth_barrier_reinit.c
pth_barrier_reinit_LDADD    = -lpthread

pth_broadcast_SOURCES       = pth_broadcast.c
pth_broadcast_LDADD         = -lpthread

pth_cancel_locked_SOURCES   = pth_cancel_locked.c
pth_cancel_locked_LDADD     = -lpthread

pth_cond_race_SOURCES       = pth_cond_race.c
pth_cond_race_LDADD         = -lpthread

pth_create_chain_SOURCES    = pth_create_chain.c
pth_create_chain_LDADD      = -lpthread

pth_detached_SOURCES        = pth_detached.c
pth_detached_LDADD          = -lpthread

pth_detached_sem_SOURCES    = pth_detached_sem.c
pth_detached_sem_LDADD      = -lpthread

pth_inconsistent_cond_wait_SOURCES = pth_inconsistent_cond_wait.c
pth_inconsistent_cond_wait_LDADD   = -lpthread

pth_spinlock_SOURCES        = pth_spinlock.c
pth_spinlock_LDADD          = -lpthread

recursive_mutex_SOURCES     = recursive_mutex.c
recursive_mutex_LDADD       = -lpthread

rwlock_race_SOURCES         = rwlock_race.c
rwlock_race_LDADD           = -lpthread

rwlock_test_SOURCES         = rwlock_test.c
rwlock_test_LDADD           = -lpthread

sem_as_mutex_SOURCES        = sem_as_mutex.c
sem_as_mutex_LDADD          = -lpthread

sigalrm_SOURCES             = sigalrm.c
sigalrm_LDADD               = -lpthread

tc01_simple_race_SOURCES    = ../../helgrind/tests/tc01_simple_race.c
tc01_simple_race_LDADD      = -lpthread

tc02_simple_tls_SOURCES     = ../../helgrind/tests/tc02_simple_tls.c
tc02_simple_tls_LDADD       = -lpthread

tc03_re_excl_SOURCES        = ../../helgrind/tests/tc03_re_excl.c
tc03_re_excl_LDADD          = -lpthread

tc04_free_lock_SOURCES      = ../../helgrind/tests/tc04_free_lock.c
tc04_free_lock_LDADD        = -lpthread

tc05_simple_race_SOURCES    = ../../helgrind/tests/tc05_simple_race.c
tc05_simple_race_LDADD      = -lpthread

tc06_two_races_SOURCES      = ../../helgrind/tests/tc06_two_races.c
tc06_two_races_LDADD        = -lpthread

tc07_hbl1_SOURCES           = ../../helgrind/tests/tc07_hbl1.c
tc07_hbl1_LDADD             = -lpthread

tc08_hbl2_SOURCES           = ../../helgrind/tests/tc08_hbl2.c
tc08_hbl2_LDADD             = -lpthread

tc09_bad_unlock_SOURCES     = ../../helgrind/tests/tc09_bad_unlock.c
tc09_bad_unlock_LDADD       = -lpthread

tc10_rec_lock_SOURCES       = ../../helgrind/tests/tc10_rec_lock.c
tc10_rec_lock_LDADD         = -lpthread

tc11_XCHG_SOURCES           = ../../helgrind/tests/tc11_XCHG.c
tc11_XCHG_LDADD             = -lpthread

tc12_rwl_trivial_SOURCES    = ../../helgrind/tests/tc12_rwl_trivial.c
tc12_rwl_trivial_LDADD      = -lpthread

tc13_laog1_SOURCES          = ../../helgrind/tests/tc13_laog1.c
tc13_laog1_LDADD            = -lpthread

tc15_laog_lockdel_SOURCES   = ../../helgrind/tests/tc15_laog_lockdel.c
tc15_laog_lockdel_LDADD     = -lpthread

tc16_byterace_SOURCES       = ../../helgrind/tests/tc16_byterace.c
tc16_byterace_LDADD         = -lpthread

tc17_sembar_SOURCES         = ../../helgrind/tests/tc17_sembar.c
tc17_sembar_LDADD           = -lpthread

tc18_semabuse_SOURCES       = ../../helgrind/tests/tc18_semabuse.c
tc18_semabuse_LDADD         = -lpthread

tc19_shadowmem_SOURCES      = ../../helgrind/tests/tc19_shadowmem.c
tc19_shadowmem_LDADD        = -lpthread

tc20_verifywrap_SOURCES     = ../../helgrind/tests/tc20_verifywrap.c
tc20_verifywrap_LDADD       = -lpthread

tc21_pthonce_SOURCES        = ../../helgrind/tests/tc21_pthonce.c
tc21_pthonce_LDADD          = -lpthread

tc22_exit_w_lock_SOURCES    = ../../helgrind/tests/tc22_exit_w_lock.c
tc22_exit_w_lock_LDADD      = -lpthread

tc23_bogus_condwait_SOURCES = ../../helgrind/tests/tc23_bogus_condwait.c
tc23_bogus_condwait_LDADD   = -lpthread

tc24_nonzero_sem_SOURCES    = ../../helgrind/tests/tc24_nonzero_sem.c
tc24_nonzero_sem_LDADD      = -lpthread

trylock_SOURCES             = trylock.c
trylock_LDADD               = -lpthread -lrt

if HAVE_BOOST_1_35
boost_thread_SOURCES        = boost_thread.cpp
boost_thread_CXXFLAGS       = $(AM_CXXFLAGS) $(BOOST_CFLAGS)
boost_thread_LDADD          = $(BOOST_LIBS)
endif

if HAVE_OPENMP
omp_matinv_SOURCES          = omp_matinv.c
omp_matinv_CFLAGS           = $(AM_CFLAGS) -fopenmp
omp_matinv_LDFLAGS          = -fopenmp
omp_matinv_LDADD            = -lm

omp_prime_SOURCES           = omp_prime.c
omp_prime_CFLAGS            = $(AM_CFLAGS) -fopenmp
omp_prime_LDFLAGS           = -fopenmp
omp_prime_LDADD             = -lm

omp_printf_SOURCES          = omp_printf.c
omp_printf_CFLAGS           = $(AM_CFLAGS) -fopenmp
omp_printf_LDFLAGS          = -fopenmp
omp_printf_LDADD            = -lm
endif

if HAVE_QTCORE
qt4_mutex_SOURCES           = qt4_mutex.cpp
qt4_mutex_CXXFLAGS          = $(AM_CXXFLAGS) $(QTCORE_CFLAGS)
qt4_mutex_LDADD             = $(QTCORE_LIBS)

qt4_rwlock_SOURCES          = qt4_rwlock.cpp
qt4_rwlock_CXXFLAGS         = $(AM_CXXFLAGS) $(QTCORE_CFLAGS)
qt4_rwlock_LDADD            = $(QTCORE_LIBS)

qt4_semaphore_SOURCES       = qt4_semaphore.cpp
qt4_semaphore_CXXFLAGS      = $(AM_CXXFLAGS) $(QTCORE_CFLAGS)
qt4_semaphore_LDADD         = $(QTCORE_LIBS)
endif
