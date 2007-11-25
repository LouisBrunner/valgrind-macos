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
	fp_race2.vgtest                                         \
	fp_race2.stdout.exp fp_race2.stderr.exp                 \
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
	sigalrm.vgtest                                          \
	sigalrm.stdout.exp sigalrm.stderr.exp

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

check_PROGRAMS =   \
  abort            \
  fp_race          \
  new_delete       \
  pth_broadcast    \
  pth_cond_race    \
  pth_create_chain \
  pth_detached     \
  sigalrm          \
  std-string

abort_SOURCES         = abort.cpp
abort_LDADD           = -lpthread

fp_race_SOURCES       = fp_race.cpp
fp_race_LDADD         = -lpthread

new_delete_SOURCES    = new_delete.cpp
new_delete_LDADD      = -lpthread

pth_broadcast_SOURCES = pth_broadcast.cpp
pth_broadcast_LDADD   = -lpthread

pth_cond_race_SOURCES = pth_cond_race.cpp
pth_cond_race_LDADD   = -lpthread

pth_create_chain_SOURCES = pth_create_chain.cpp
pth_create_chain_LDADD   = -lpthread

pth_detached_SOURCES  = pth_detached.c
pth_detached_LDADD    = -lpthread

sigalrm_SOURCES       = sigalrm.cpp
sigalrm_LDADD         = -lpthread -lrt

std_string_SOURCES    = std-string.cpp
std_string_LDADD      = -lpthread
