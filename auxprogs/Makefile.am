include $(top_srcdir)/Makefile.all.am

dist_noinst_SCRIPTS = \
	change-copyright-year \
	dump_insn_ppc.sh \
	gen-mdg \
	gsl19test \
	make_or_upd_vgversion_h \
	nightly-build-summary \
	update-demangler \
	posixtestsuite-1.5.1-diff-results

EXTRA_DIST = \
	docs/valgrind-listener-manpage.xml \
	docs/valgrind-di-server-manpage.xml \
	gsl-1.6.patch \
	gsl-1.6.supp \
	gsl-1.6.out.x86.exp \
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
# If there is no secondary platform, and the platforms include x86-darwin,
# then the primary platform must be x86-darwin.  Hence:
if ! VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
valgrind_listener_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
endif
if VGCONF_OS_IS_SOLARIS
valgrind_listener_LDADD      = -lsocket -lnsl
endif

valgrind_di_server_SOURCES   = valgrind-di-server.c
valgrind_di_server_CPPFLAGS  = $(AM_CPPFLAGS_PRI) -I$(top_srcdir)/coregrind
valgrind_di_server_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_di_server_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_di_server_LDFLAGS   = $(AM_CFLAGS_PRI)
if VGCONF_PLATVARIANT_IS_ANDROID
valgrind_di_server_CFLAGS    += -static
endif
# If there is no secondary platform, and the platforms include x86-darwin,
# then the primary platform must be x86-darwin.  Hence:
if ! VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
valgrind_di_server_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
endif
if VGCONF_OS_IS_SOLARIS
valgrind_di_server_LDADD     = -lsocket -lnsl
endif

#----------------------------------------------------------------------------
# getoff-<platform>
# Used to retrieve user space various offsets, using user space libraries.
#----------------------------------------------------------------------------

noinst_PROGRAMS  = getoff-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += getoff-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

# The link flags for this are tricky, because we want to build it for
# both the primary and secondary platforms, and add 
# "-Wl,-read_only_relocs -Wl,suppress" to whichever of those is x86-darwin,
# if any.  Hence there's a double-nested conditional that adds to the
# LDFLAGS in both cases.

getoff_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES   = getoff.c
getoff_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS  = $(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
getoff_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS    = $(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
getoff_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CCASFLAGS = $(AM_CCASFLAGS_PRI)
getoff_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS   = $(AM_CFLAGS_PRI) @LIB_UBSAN@
if HAVE_DLINFO_RTLD_DI_TLS_MODID
getoff_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD = $(LDADD) -ldl
endif
# If there is no secondary platform, and the platforms include x86-darwin,
# then the primary platform must be x86-darwin.  Hence:
if ! VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
getoff_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
endif

if VGCONF_HAVE_PLATFORM_SEC
getoff_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES   = getoff.c
getoff_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS  = $(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
getoff_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS    = $(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
getoff_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CCASFLAGS = $(AM_CCASFLAGS_SEC)
getoff_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS   = $(AM_CFLAGS_SEC)
if HAVE_DLINFO_RTLD_DI_TLS_MODID
getoff_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD = $(LDADD) -ldl
endif
endif
# If there is a secondary platform, and the platforms include x86-darwin,
# then the primary platform must be amd64-darwin and the secondary platform
# must be x86-darwin.  Hence:
if VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
getoff_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
endif

#----------------------------------------------------------------------------
# Auxiliary testsuits
#----------------------------------------------------------------------------

auxchecks: gsl-check
auxclean: gsl-clean

# You can override AUX_CHECK_DIR to use a previous download/build.
# Must be an absolute path.
# e.g. make auxchecks AUX_CHECK_DIR=$HOME/valgrind-auxtests
AUX_CHECK_DIR=$(abs_builddir)/auxchecks

# GNU Scientific Library 1.6
GSL_DIR_NAME=gsl-1.6
GSL_TAR_NAME=$(GSL_DIR_NAME).tar.gz
GSL_URL=https://ftpmirror.gnu.org/gsl/$(GSL_TAR_NAME)
GSL_SHA256_SUM=52e097b5228a617fef788d54eba6855c1addc62b8f68a1dfb5895cad25594f1f
GSL_TAR=$(AUX_CHECK_DIR)/$(GSL_TAR_NAME)
GSL_SRC_DIR=$(AUX_CHECK_DIR)/$(GSL_DIR_NAME)
# By default we like -O3 to hopefully get some loop vectorization
# You can also override GSL_CFLAGS if you want e.g. -march=core-avx2
# Different GSL_CFLAGS will result in different build dirs (under AUX_CHECK_DIR)
GSL_CFLAGS=-g -O3
# i386 needs sse to get rounding for floating point correct.
# But we only want this if the primary isn't AMD64
if VGCONF_ARCHS_INCLUDE_X86
if !VGCONF_ARCHS_INCLUDE_AMD64
GSL_CFLAGS+=-mfpmath=sse -msse2
endif
endif

# Trick to get a literal space to use in substitutions
sp := $(subst ,, )

# Filter out spaces from GSL_CFLAGS to get unique build dir
GSL_BUILD_DIR=$(AUX_CHECK_DIR)/gsl-build$(subst $(sp),,$(GSL_CFLAGS))

# These are all the tests, except siman and randist which can take minutes.
GSL_TESTS=block cblas cdf cheb combination complex const deriv dht diff \
	  eigen err fft fit histogram ieee-utils integration interpolation \
	  linalg matrix min monte multifit multimin multiroots ntuple \
	  ode-initval permutation poly qrng rng roots sort specfunc \
	  statistics sum sys vector wavelet

# Get the tar file if we don't have it yet.
$(GSL_TAR):
	mkdir -p $(AUX_CHECK_DIR)
	wget -q -O $(GSL_TAR) $(GSL_URL)

# We need to autoreconf to make sure to get config.guess, config.sub
# and libtool for newer architectures.
$(GSL_SRC_DIR)/gsl-patched: $(GSL_TAR)
	echo "$(GSL_SHA256_SUM)  $(GSL_TAR)" | @SHA256SUM@ --check -
	(cd $(AUX_CHECK_DIR) && \
	 tar zxf $(GSL_TAR_NAME) && \
	 cd $(GSL_DIR_NAME) && \
	 patch -p1 < $(abs_top_srcdir)/auxprogs/gsl-1.6.patch && \
	 autoreconf -f -i -Wnone)
	touch $@

# We need make check -k because
# some tests might fail even native (only on i386 though).
# make check doesn't work reliably with -j.
$(GSL_BUILD_DIR)/gsl-build: $(GSL_SRC_DIR)/gsl-patched
	mkdir -p $(GSL_BUILD_DIR)
	(cd $(GSL_BUILD_DIR) && \
	 $(GSL_SRC_DIR)/configure CC="${CC}" CXX="${CXX}" CFLAGS="$(GSL_CFLAGS)" && \
	 ${MAKE} -j $(nproc) && \
	 ${MAKE} check -k || true)
	touch $@

# We hope all tests PASS (so don't produce output except for the test names).
# But on x86 we get one FAIL, so that is "fine" too.
# We currently don't check stderr, but we probably should.
gsl-check: $(GSL_BUILD_DIR)/gsl-build
	(cd  $(GSL_BUILD_DIR); \
	 for gsl_test in $(GSL_TESTS); do \
	   echo $$gsl_test; \
	   ./libtool --mode=execute $(abs_top_builddir)/vg-in-place -q \
	             --suppressions=$(abs_top_srcdir)/auxprogs/gsl-1.6.supp \
		     $$gsl_test/test; \
	 done | grep --line-buffered -v ^PASS: | tee valgrind-gsl.out)
	for gsl_test in $(GSL_TESTS); do echo $$gsl_test; done \
	  | cmp - $(GSL_BUILD_DIR)/valgrind-gsl.out || \
	diff -u $(abs_top_srcdir)/auxprogs/gsl-1.6.out.x86.exp \
		$(GSL_BUILD_DIR)/valgrind-gsl.out

# We keep the tarball but remove the unpacked sources and build
gsl-clean:
	rm -rf $(GSL_SRC_NAME) $(GSL_BUILD_DIR)

#----------------------------------------------------------------------------
# General stuff
#----------------------------------------------------------------------------

all-local: inplace-noinst_PROGRAMS inplace-noinst_DSYMS

clean-local: clean-noinst_DSYMS auxclean

install-exec-local: install-noinst_PROGRAMS install-noinst_DSYMS

uninstall-local: uninstall-noinst_PROGRAMS uninstall-noinst_DSYMS
