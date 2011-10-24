include $(top_srcdir)/Makefile.all.am

dist_noinst_SCRIPTS = \
	change-copyright-year \
	dump_insn_ppc.sh \
	gen-mdg \
	gsl16test \
	gsl19test \
	posixtestsuite-1.5.1-diff-results

EXTRA_DIST = \
	DotToScc.hs \
	Merge3Way.hs \
	gsl16-badfree.patch \
	gsl16-wavelet.patch \
	posixtestsuite-1.5.1-diff.txt \
	ppcfround.c \
	ppc64shifts.c \
	primes.c

#----------------------------------------------------------------------------
# valgrind_listener (built for the primary target only)
#----------------------------------------------------------------------------

bin_PROGRAMS = valgrind-listener

valgrind_listener_SOURCES = valgrind-listener.c
valgrind_listener_CPPFLAGS  = $(AM_CPPFLAGS_PRI) -I$(top_srcdir)/coregrind
valgrind_listener_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_listener_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_listener_LDFLAGS   = $(AM_CFLAGS_PRI)
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
valgrind_listener_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
