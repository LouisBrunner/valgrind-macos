include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core.am

bin_PROGRAMS = valgrind-listener

noinst_SCRIPTS = gen-mdg DotToScc.hs primes.c \
		gsl16test gsl16-badfree.patch gsl16-wavelet.patch

EXTRA_DIST = $(noinst_SCRIPTS)


#------------------------- listener -----------------------
# Build valgrind_listener for the primary target only.
#
valgrind_listener_SOURCES = valgrind-listener.c

valgrind_listener_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
valgrind_listener_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_listener_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_listener_LDFLAGS   = $(AM_CFLAGS_PRI)
#
#----------------------------------------------------------


