include $(top_srcdir)/Makefile.all.am

EXTRA_DIST = iropt-test.vgtest \
             iropt-test-sec.vgtest \
             irops.tab

dist_noinst_SCRIPTS =

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

pkginclude_HEADERS =
noinst_HEADERS = vtest.h

#----------------------------------------------------------------------------
# iropt_test
#----------------------------------------------------------------------------

noinst_PROGRAMS = iropt-test

if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += iropt-test-sec
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

SOURCES = \
	main.c \
	unary.c \
	binary.c \
	util.c \
	valgrind.c

# The link flags for this are tricky, because we want to build it for
# both the primary and secondary platforms, and add
# "-Wl,-read_only_relocs -Wl,suppress" to whichever of those is x86-darwin,
# if any.  Hence there's a double-nested conditional that adds to the
# LDFLAGS in both cases.

iropt_test_SOURCES      = $(SOURCES)
iropt_test_CPPFLAGS     = $(AM_CPPFLAGS_PRI) \
                          -I$(top_srcdir)/include  \
                          -I$(top_srcdir)/memcheck \
                          -I$(top_srcdir)/VEX/pub
iropt_test_CFLAGS       = $(AM_CFLAGS_PRI) -fhosted
iropt_test_DEPENDENCIES = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
iropt_test_LDADD        = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
iropt_test_LDFLAGS      = $(AM_CFLAGS_PRI) @LIB_UBSAN@
# If there is no secondary platform, and the platforms include x86-darwin,
# then the primary platform must be x86-darwin.  Hence:
if ! VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
iropt_test_LDFLAGS      += -Wl,-read_only_relocs -Wl,suppress
endif
endif

if VGCONF_HAVE_PLATFORM_SEC
iropt_test_sec_SOURCES      = $(SOURCES)
iropt_test_sec_CPPFLAGS     = $(AM_CPPFLAGS_SEC) \
                              $(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) \
                              -I$(top_srcdir)/include  \
                              -I$(top_srcdir)/memcheck \
                              -I$(top_srcdir)/VEX/pub
iropt_test_sec_CFLAGS       = $(AM_CFLAGS_SEC) -fhosted \
                              $(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
iropt_test_sec_DEPENDENCIES = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a \
                              $(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
iropt_test_sec_LDADD        = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a \
                              $(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
iropt_test_sec_LDFLAGS      = $(AM_CFLAGS_SEC) @LIB_UBSAN@ \
                              $(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif
# If there is a secondary platform, and the platforms include x86-darwin,
# then the primary platform must be amd64-darwin and the secondary platform
# must be x86-darwin.  Hence:
if VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
iropt_test_sec_LDFLAGS      += -Wl,-read_only_relocs -Wl,suppress
endif
endif
