include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core.am

bin_PROGRAMS = valgrind-listener

noinst_SCRIPTS = gen-mdg DotToScc.hs Merge3Way.hs primes.c \
		gsl16test gsl16-badfree.patch gsl16-wavelet.patch \
		gsl19test \
		ppcfround.c ppc64shifts.c libmpiwrap.c mpiwrap_type_test.c \
		aix5_VKI_info.c libmpiwrap_aix5.exp \
		aix5_proc_self_sysent.c \
		posixtestsuite-1.5.1-diff-results \
		posixtestsuite-1.5.1-diff.txt

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
# Build libmpiwrap.so for the primary target, and for the secondary
# target if relevant.
#
# This is really horrible.
#
# Don't let automake install this, since it puts the .so's in the
# wrong place.  Instead install them ourselves in the right
# place using the install-exec-local target below.
#
# Also, automake isn't good at supporting non-$(CC) compilers.
# But we need to use $(MPI_CC) here.  Hence the nasty hack of
# directly saying how to build libmpiwrap-*.so, instead of
# using automake's standard gunk.
#
if VGO_AIX5
 HACKY_FLAGS_PRI = -g -O -bE:libmpiwrap_aix5.exp -bM:SRE -bnoentry \
			-qflag=w:w \
			`echo $(AM_FLAG_M3264_PRI) | sed s/maix/q/g`
 HACKY_FLAGS_SEC = -g -O -bE:libmpiwrap_aix5.exp -bM:SRE -bnoentry \
			-qflag=w:w \
			`echo $(AM_FLAG_M3264_SEC) | sed s/maix/q/g`
else
 HACKY_FLAGS_PRI = -g -O -fno-omit-frame-pointer -Wall -fpic -shared \
			$(AM_FLAG_M3264_PRI)
 HACKY_FLAGS_SEC = -g -O -fno-omit-frame-pointer -Wall -fpic -shared \
			$(AM_FLAG_M3264_SEC)
endif


## First, we have to say how to build the .so's ..
##
noinst_PROGRAMS =
if BUILD_MPIWRAP_PRI
noinst_PROGRAMS       += libmpiwrap-@VG_PLATFORM_PRI@.so
libmpiwrap-@VG_PLATFORM_PRI@.so: libmpiwrap.c
	$(MPI_CC) $(HACKY_FLAGS_PRI) \
		-I../include \
		-o libmpiwrap-@VG_PLATFORM_PRI@.so libmpiwrap.c
endif
if BUILD_MPIWRAP_SEC
noinst_PROGRAMS       += libmpiwrap-@VG_PLATFORM_SEC@.so
libmpiwrap-@VG_PLATFORM_SEC@.so: libmpiwrap.c
	$(MPI_CC) $(HACKY_FLAGS_SEC) \
		-I../include \
		-o libmpiwrap-@VG_PLATFORM_SEC@.so libmpiwrap.c
endif


## And here we say how to install them.
##
# The following install hack is serialised by "libmpiwrap.so".
# Hence force -j 1.
.NOTPARALLEL:

install-exec-local:
if BUILD_MPIWRAP_PRI
# convert (eg) X86_LINUX to x86-linux
# really should use sed here, rather than assume tr is available
	pD=`echo @VG_PLATFORM_PRI@ | tr A-Z_ a-z-` ; \
		$(mkinstalldirs) $(DESTDIR)$(valdir)/$$pD; \
	rm -f ./libmpiwrap.so; \
	cp ./libmpiwrap-@VG_PLATFORM_PRI@.so ./libmpiwrap.so; \
	$(INSTALL_PROGRAM) ./libmpiwrap.so \
		$(DESTDIR)$(valdir)/$$pD; \
	rm -f ./libmpiwrap.so
endif
if BUILD_MPIWRAP_SEC
	pD=`echo @VG_PLATFORM_SEC@ | tr A-Z_ a-z-` ; \
	$(mkinstalldirs) $(DESTDIR)$(valdir)/$$pD; \
	rm -f ./libmpiwrap.so; \
	cp ./libmpiwrap-@VG_PLATFORM_SEC@.so ./libmpiwrap.so; \
	$(INSTALL_PROGRAM) ./libmpiwrap.so \
		$(DESTDIR)$(valdir)/$$pD; \
	rm -f ./libmpiwrap.so
endif


## And some hacks to keep 'make dist' happy.
## It would seem that using nodist_SOURCES is the right fix, but
## I can't figure out how to do it.
##
#nodist_SOURCES        = libmpiwrap-@VG_PLATFORM_PRI@.c
#nodist_SOURCES        = libmpiwrap-@VG_PLATFORM_SEC@.c

libmpiwrap-.c:
	rm -f libmpiwrap-.c
	touch libmpiwrap-.c

libmpiwrap-@VG_PLATFORM_PRI@.c:
	rm -f libmpiwrap-@VG_PLATFORM_PRI@.c
	touch libmpiwrap-@VG_PLATFORM_PRI@.c

if VGP_HAVE_SECONDARY
libmpiwrap-@VG_PLATFORM_SEC@.c:
	rm -f libmpiwrap-@VG_PLATFORM_SEC@.c
	touch libmpiwrap-@VG_PLATFORM_SEC@.c
endif

## cleanery
clean-local:
	rm -f libmpiwrap-.c \
	libmpiwrap-@VG_PLATFORM_PRI@.c libmpiwrap-@VG_PLATFORM_SEC@.c

#
#----------------------------------------------------------

