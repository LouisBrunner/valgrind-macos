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
# General stuff
#----------------------------------------------------------------------------

all-local: inplace-noinst_PROGRAMS inplace-noinst_DSYMS

clean-local: clean-noinst_DSYMS

install-exec-local: install-noinst_PROGRAMS install-noinst_DSYMS

uninstall-local: uninstall-noinst_PROGRAMS uninstall-noinst_DSYMS
