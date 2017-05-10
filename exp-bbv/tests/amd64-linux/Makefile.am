include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

check_PROGRAMS = \
	million rep_prefix fldcw_check complex_rep clone_test

EXTRA_DIST = \
	   clone_test.stderr.exp \
	   clone_test.post.exp \
	   clone_test.vgtest \
	   complex_rep.stderr.exp \
	   complex_rep.vgtest \
	   fldcw_check.stderr.exp \
	   fldcw_check.vgtest \
	   ll.stderr.exp \
	   ll.stdout.exp \
	   ll.post.exp \
	   ll.vgtest \
	   million.stderr.exp \
	   million.post.exp \
	   million.vgtest \
	   rep_prefix.stderr.exp \
	   rep_prefix.vgtest 

AM_CCASFLAGS += -ffreestanding

AM_LDFLAGS = -nostartfiles -nodefaultlibs

clone_test_SOURCES = clone_test.S
complex_rep_SOURCES = complex_rep.S
fldcw_check_SOURCES = fldcw_check.S
million_SOURCES = million.S
rep_prefix_SOURCES = rep_prefix.S

# To compile the ll testcase, the compiler needs to support -Xassembler
# ICC does not do that
if !COMPILER_IS_ICC
AM_CCASFLAGS   += -Xassembler -I$(top_srcdir)/exp-bbv/tests
check_PROGRAMS += ll
ll_SOURCES      = ll.S
endif

clone_test_CFLAGS	= $(AM_CFLAGS) @FLAG_NO_PIE@
complex_rep_CFLAGS	= $(AM_CFLAGS) @FLAG_NO_PIE@
fldcw_check_CFLAGS	= $(AM_CFLAGS) @FLAG_NO_PIE@
ll_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@
million_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@
rep_prefix_CFLAGS	= $(AM_CFLAGS) @FLAG_NO_PIE@
