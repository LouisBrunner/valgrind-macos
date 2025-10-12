include $(top_srcdir)/Makefile.tool-tests.am

EXTRA_DIST = disasm-test.vgtest disasm-test.stderr.exp \
             disasm-test.post.exp
dist_noinst_SCRIPTS = filter_stderr

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

pkginclude_HEADERS =
noinst_HEADERS = main.h objdump.h vex.h

#----------------------------------------------------------------------------
# disasm_test
#----------------------------------------------------------------------------

noinst_PROGRAMS = disasm-test

SOURCES = \
	main.c \
	generate.c \
	objdump.c \
	opcode.c \
	verify.c \
	vex.c

disasm_test_SOURCES      = $(SOURCES)
disasm_test_CPPFLAGS     = $(AM_CPPFLAGS_PRI) \
                           -I$(top_srcdir)/VEX/pub \
                           -I$(top_srcdir)/VEX/priv
disasm_test_CFLAGS       = $(AM_CFLAGS_PRI) -fhosted
disasm_test_DEPENDENCIES = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
disasm_test_LDADD        = $(top_builddir)/VEX/libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
disasm_test_LDFLAGS      = $(AM_CFLAGS_PRI) @LIB_UBSAN@
