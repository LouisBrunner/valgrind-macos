include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

bin_PROGRAMS = valgrind-listener

noinst_SCRIPTS = gen-mdg DotToScc.hs \
		gsl16test gsl16-badfree.patch gsl16-wavelet.patch

EXTRA_DIST = $(noinst_SCRIPTS)

valgrind_listener_SOURCES = valgrind-listener.c

