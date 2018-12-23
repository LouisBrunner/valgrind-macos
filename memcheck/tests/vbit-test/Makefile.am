include $(top_srcdir)/Makefile.all.am

EXTRA_DIST = vbit-test.vgtest vbit-test.stderr.exp

dist_noinst_SCRIPTS = filter_stderr

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

pkginclude_HEADERS = 
noinst_HEADERS = vtest.h vbits.h

#----------------------------------------------------------------------------
# vbit_test
#----------------------------------------------------------------------------

noinst_PROGRAMS = vbit-test

if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += vbit-test-sec
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

SOURCES = \
	main.c \
	unary.c \
	binary.c \
	ternary.c \
	qernary.c \
	util.c \
	vbits.c \
	irops.c \
	valgrind.c

vbit_test_SOURCES      = $(SOURCES)
vbit_test_CPPFLAGS     = $(AM_CPPFLAGS_PRI) \
                         -I$(top_srcdir)/include  \
                         -I$(top_srcdir)/memcheck \
                         -I$(top_srcdir)/VEX/pub
vbit_test_CFLAGS       = $(AM_CFLAGS_PRI)
vbit_test_DEPENDENCIES = 
vbit_test_LDADD        = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
vbit_test_LDFLAGS      = $(AM_CFLAGS_PRI) @LIB_UBSAN@

if VGCONF_HAVE_PLATFORM_SEC
vbit_test_sec_SOURCES      = $(SOURCES)
vbit_test_sec_CPPFLAGS     = $(AM_CPPFLAGS_SEC) \
                             $(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) \
                             -I$(top_srcdir)/include  \
                             -I$(top_srcdir)/memcheck \
                             -I$(top_srcdir)/VEX/pub
vbit_test_sec_CFLAGS       = $(AM_CFLAGS_SEC) \
                             $(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
vbit_test_sec_DEPENDENCIES = 
vbit_test_sec_LDADD        = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a \
                              $(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
vbit_test_sec_LDFLAGS      = $(AM_CFLAGS_SEC) @LIB_UBSAN@ \
                             $(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif
