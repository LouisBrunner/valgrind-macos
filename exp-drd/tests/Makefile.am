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
	filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS)                                  \
	fp_race.vgtest                                          \
	fp_race.stdout.exp fp_race.stderr.exp                   \
	fp_race.stderr.exp2                                     \
	fp_race2.vgtest                                         \
	fp_race2.stdout.exp fp_race2.stderr.exp                 \
	hg01_all_ok.vgtest                                      \
	hg01_all_ok.stderr.exp                                  \
	hg02_deadlock.vgtest                                    \
	hg02_deadlock.stderr.exp                                \
	hg03_inherit.vgtest                                     \
	hg03_inherit.stderr.exp                                 \
	hg03_inherit.stderr.exp2				\
	hg04_race.vgtest                                        \
	hg04_race.stderr.exp                                    \
	hg04_race.stderr.exp2					\
	hg05_race2.vgtest                                       \
	hg05_race2.stderr.exp                                   \
	hg06_readshared.vgtest                                  \
	hg06_readshared.stderr.exp                              \
	matinv.vgtest                                           \
	matinv.stdout.exp matinv.stderr.exp                     \
	pth_barrier.vgtest  pth_barrier.stderr.exp              \
	pth_barrier2.vgtest pth_barrier2.stderr.exp             \
	pth_barrier3.vgtest pth_barrier3.stderr.exp             \
	pth_broadcast.vgtest                                    \
	pth_broadcast.stdout.exp pth_broadcast.stderr.exp       \
	pth_cond_race.vgtest                                    \
	pth_cond_race.stdout.exp pth_cond_race.stderr.exp       \
	pth_cond_race2.vgtest                                   \
	pth_cond_race2.stdout.exp pth_cond_race2.stderr.exp     \
	pth_create_chain.vgtest                                 \
	pth_create_chain.stdout.exp pth_create_chain.stderr.exp \
	pth_detached.vgtest                                     \
	pth_detached.stdout.exp pth_detached.stderr.exp         \
	pth_detached2.vgtest                                    \
	pth_detached2.stdout.exp pth_detached2.stderr.exp       \
	recursive_mutex.vgtest recursive_mutex.stderr.exp	\
	sem_as_mutex.vgtest                                     \
	sem_as_mutex.stderr.exp sem_as_mutex.stderr.exp2        \
	sem_as_mutex2.vgtest                                    \
	sem_as_mutex2.stderr.exp                                \
	tc01_simple_race.vgtest                                 \
	tc01_simple_race.stderr.exp                             \
	tc01_simple_race.stderr.exp2				\
	tc02_simple_tls.vgtest                                  \
	tc02_simple_tls.stderr.exp                              \
	tc03_re_excl.vgtest                                     \
	tc03_re_excl.stderr.exp                                 \
	tc04_free_lock.vgtest                                   \
	tc04_free_lock.stderr.exp                               \
	tc05_simple_race.vgtest                                 \
	tc05_simple_race.stderr.exp                             \
	tc06_two_races.vgtest                                   \
	tc06_two_races.stderr.exp                               \
	tc07_hbl1.vgtest                                        \
	tc07_hbl1.stderr.exp tc07_hbl1.stdout.exp               \
	tc07_hbl1.stderr.exp2					\
	tc08_hbl2.vgtest					\
	tc08_hbl2.stderr.exp					\
	tc09_bad_unlock.vgtest                                  \
	tc09_bad_unlock.stderr.exp                              \
	tc10_rec_lock.vgtest                                    \
	tc10_rec_lock.stderr.exp                                \
	tc11_XCHG.vgtest                                        \
	tc11_XCHG.stderr.exp tc11_XCHG.stdout.exp               \
	tc11_XCHG.stderr.exp2					\
	tc12_rwl_trivial.vgtest                                 \
	tc12_rwl_trivial.stderr.exp                             \
	tc13_laog1.vgtest                                       \
	tc13_laog1.stderr.exp                                   \
	tc15_laog_lockdel.vgtest                                \
	tc15_laog_lockdel.stderr.exp                            \
	tc16_byterace.vgtest                                    \
	tc16_byterace.stderr.exp                                \
	tc16_byterace.stderr.exp2				\
	tc17_sembar.vgtest                                      \
	tc17_sembar.stderr.exp                                  \
	tc18_semabuse.vgtest                                    \
	tc18_semabuse.stderr.exp                                \
	tc19_shadowmem.vgtest                                   \
	tc19_shadowmem.stderr.exp                               \
	tc20_verifywrap.vgtest                                  \
	tc20_verifywrap.stderr.exp                              \
	tc20_verifywrap.stderr.exp2				\
	tc21_pthonce.vgtest                                     \
	tc21_pthonce.stderr.exp tc21_pthonce.stdout.exp         \
	tc22_exit_w_lock.vgtest                                 \
	tc22_exit_w_lock.stderr.exp                             \
	tc22_exit_w_lock.stderr.exp-32bit			\
	tc23_bogus_condwait.vgtest                              \
	tc23_bogus_condwait.stderr.exp                          \
	tc24_nonzero_sem.vgtest                                 \
	tc24_nonzero_sem.stderr.exp                             \
	sigalrm.vgtest                                          \
	sigalrm.stdout.exp sigalrm.stderr.exp

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

check_PROGRAMS =      \
  fp_race             \
  hg01_all_ok         \
  hg02_deadlock       \
  hg03_inherit        \
  hg04_race           \
  hg05_race2          \
  hg06_readshared     \
  matinv              \
  pth_barrier         \
  pth_broadcast       \
  pth_cond_race       \
  pth_create_chain    \
  pth_detached        \
  sem_as_mutex        \
  recursive_mutex     \
  sigalrm             \
  tc01_simple_race    \
  tc02_simple_tls     \
  tc03_re_excl        \
  tc04_free_lock      \
  tc05_simple_race    \
  tc06_two_races      \
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
  tc24_nonzero_sem

#  tc07_hbl1 -- requires bus locking support.
#  tc08_hbl2 -- requires bus locking support.
#  tc14_laog_dinphils  -- hangs.


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

matinv_SOURCES              = matinv.c
matinv_LDADD                = -lpthread -lm

pth_barrier_SOURCES         = pth_barrier.c
pth_barrier_LDADD           = -lpthread

pth_broadcast_SOURCES       = pth_broadcast.c
pth_broadcast_LDADD         = -lpthread

pth_cond_race_SOURCES       = pth_cond_race.c
pth_cond_race_LDADD         = -lpthread

pth_create_chain_SOURCES    = pth_create_chain.c
pth_create_chain_LDADD      = -lpthread

pth_detached_SOURCES        = pth_detached.c
pth_detached_LDADD          = -lpthread

recursive_mutex_SOURCES     = recursive_mutex.c
recursive_mutex_LDADD       = -lpthread

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

# tc07_hbl1_SOURCES           = ../../helgrind/tests/tc07_hbl1.c
# tc07_hbl1_LDADD             = -lpthread

# tc08_hbl2_SOURCES           = ../../helgrind/tests/tc08_hbl2.c
# tc08_hbl2_LDADD             = -lpthread

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
