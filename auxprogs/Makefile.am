include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core.am

bin_PROGRAMS = valgrind-listener

noinst_SCRIPTS = gen-mdg DotToScc.hs primes.c \
		gsl16test gsl16-badfree.patch gsl16-wavelet.patch \
		ppcfround.c ppc64shifts.c mpiwrap.c mpiwrap_type_test.c

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


#------------------------- mpi wrappers -----------------------
# Build libmpiwrap.so for the primary target only.
#
# This is really horrible.
#
# Don't let automake install this, since it puts it in the
# wrong place.  Instead install it ourselves in the right
# place using the install-exec-local target below.
#
# Also, automake isn't good at supporting non-$(CC) compilers.
# But we need to use $(MPI_CC) here.  Hence the nasty hack of
# directly saying how to build libmpiwrap.so, instead of
# using automake's standard gunk.
#
if BUILD_MPIWRAP
noinst_PROGRAMS       = libmpiwrap.so
#libmpiwrap_so_SOURCES = mpiwrap.c
#libmpiwrap_so_CFLAGS  = $(AM_FLAG_M3264_PRI) \
#				-g -O -fpic -fno-omit-frame-pointer \
#				-I../include -I@MPI_PREFIX@/include
#libmpiwrap_so_LDFLAGS = $(AM_FLAG_M3264_PRI) -g -shared
libmpiwrap.so: mpiwrap.c
	$(MPI_CC) -g -O -fno-omit-frame-pointer -Wall -fpic -shared \
		-I../include \
		$(AM_FLAG_M3264_PRI) \
		-o libmpiwrap.so mpiwrap.c

install-exec-local:
# convert (eg) X86_LINUX to x86-linux
# really should use sed here, rather than assume tr is available
	pD=`echo @VG_PLATFORM_PRI@ | tr A-Z_ a-z-` ; \
	$(mkinstalldirs) $(DESTDIR)$(valdir)/$$pD; \
	$(INSTALL_PROGRAM) ./libmpiwrap.so $(DESTDIR)$(valdir)/$$pD
endif
#
#----------------------------------------------------------

