include $(top_srcdir)/Makefile.all.am

dist_noinst_SCRIPTS = \
	change-copyright-year \
	dump_insn_ppc.sh \
	gen-mdg \
	gsl16test \
	gsl19test \
	nightly-build-summary \
	update-demangler \
	posixtestsuite-1.5.1-diff-results

EXTRA_DIST = \
	docs/valgrind-listener-manpage.xml \
	DotToScc.hs \
	Merge3Way.hs \
	gsl16-badfree.patch \
	gsl16-wavelet.patch \
	posixtestsuite-1.5.1-diff.txt \
	ppcfround.c \
	ppc64shifts.c \
	primes.c

#----------------------------------------------------------------------------
# valgrind_listener  (built for the primary target only)
# valgrind-di-server (ditto)
#----------------------------------------------------------------------------

bin_PROGRAMS = valgrind-listener valgrind-di-server

valgrind_listener_SOURCES = valgrind-listener.c
valgrind_listener_CPPFLAGS  = $(AM_CPPFLAGS_PRI) -I$(top_srcdir)/coregrind
valgrind_listener_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_listener_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_listener_LDFLAGS   = $(AM_CFLAGS_PRI)
if VGCONF_PLATVARIANT_IS_ANDROID
valgrind_listener_CFLAGS    += -static
endif
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
valgrind_listener_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif

valgrind_di_server_SOURCES   = valgrind-di-server.c
valgrind_di_server_CPPFLAGS  = $(AM_CPPFLAGS_PRI) -I$(top_srcdir)/coregrind
valgrind_di_server_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_di_server_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_di_server_LDFLAGS   = $(AM_CFLAGS_PRI)
if VGCONF_PLATVARIANT_IS_ANDROID
valgrind_di_server_CFLAGS    += -static
endif
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
valgrind_di_server_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
