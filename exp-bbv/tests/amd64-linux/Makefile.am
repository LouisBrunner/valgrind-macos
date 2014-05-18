include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

check_PROGRAMS = \
	million rep_prefix ll fldcw_check complex_rep clone_test

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

AM_CCASFLAGS += -ffreestanding -Xassembler -I$(top_srcdir)/exp-bbv/tests

LDFLAGS += -nostartfiles -nodefaultlibs

clone_test_SOURCES = clone_test.S
complex_rep_SOURCES = complex_rep.S
fldcw_check_SOURCES = fldcw_check.S
ll_SOURCES = ll.S
million_SOURCES = million.S
rep_prefix_SOURCES = rep_prefix.S

