# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .

DIST_SUBDIRS = .

noinst_SCRIPTS =                                          \
        filter_cmdline0 filter_linenos                    \
        filter_fdleak filter_none_discards filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS)                                  \
	fp_race.vgtest                                          \
	fp_race.stdout.exp fp_race.stderr.exp                   \
	fp_race.stderr.exp2                                     \
	fp_race2.vgtest                                         \
	fp_race2.stdout.exp fp_race2.stderr.exp                 \
	matinv.vgtest                                           \
	matinv.stdout.exp matinv.stderr.exp                     \
	pth_barrier.vgtest                                      \
	pth_barrier.stdout.exp pth_barrier.stderr.exp           \
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
	sem_as_mutex.vgtest                                     \
	sem_as_mutex.stdout.exp sem_as_mutex.stderr.exp         \
	sem_as_mutex.stderr.exp2                                \
	sem_as_mutex2.vgtest                                    \
	sem_as_mutex2.stdout.exp sem_as_mutex2.stderr.exp       \
	tc17_sembar.vgtest                                      \
	tc17_sembar.stdout.exp tc17_sembar.stderr.exp           \
	tc18_semabuse.vgtest                                    \
	tc18_semabuse.stdout.exp tc18_semabuse.stderr.exp       \
	sigalrm.vgtest                                          \
	sigalrm.stdout.exp sigalrm.stderr.exp

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

check_PROGRAMS =   \
  fp_race          \
  matinv           \
  pth_barrier      \
  pth_broadcast    \
  pth_cond_race    \
  pth_create_chain \
  pth_detached     \
  sem_as_mutex     \
  sigalrm          \
  tc17_sembar      \
  tc18_semabuse

fp_race_SOURCES          = fp_race.c
fp_race_LDADD            = -lpthread

matinv_SOURCES           = matinv.c
matinv_LDADD             = -lpthread -lm

pth_barrier_SOURCES      = pth_barrier.c
pth_barrier_LDADD        = -lpthread

pth_broadcast_SOURCES    = pth_broadcast.c
pth_broadcast_LDADD      = -lpthread

pth_cond_race_SOURCES    = pth_cond_race.c
pth_cond_race_LDADD      = -lpthread

pth_create_chain_SOURCES = pth_create_chain.c
pth_create_chain_LDADD   = -lpthread

pth_detached_SOURCES     = pth_detached.c
pth_detached_LDADD       = -lpthread

sem_as_mutex_SOURCES     = sem_as_mutex.c
sem_as_mutex_LDADD       = -lpthread

sigalrm_SOURCES          = sigalrm.c
sigalrm_LDADD            = -lpthread

tc17_sembar_SOURCES      = ../../helgrind/tests/tc17_sembar.c
tc17_sembar_LDADD        = -lpthread

tc18_semabuse_SOURCES    = ../../helgrind/tests/tc18_semabuse.c
tc18_semabuse_LDADD      = -lpthread
