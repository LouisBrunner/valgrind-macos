include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

check_PROGRAMS = \
	ll clone_test

EXTRA_DIST = \
	   clone_test.stderr.exp \
	   clone_test.post.exp \
	   clone_test.vgtest \
	   ll.stderr.exp \
	   ll.stdout.exp \
	   ll.post.exp \
	   ll.vgtest

AM_CCASFLAGS += -ffreestanding -Xassembler -I$(top_srcdir)/exp-bbv/tests

AM_LDFLAGS = @FLAG_M32@ -static -nostartfiles -nodefaultlibs

clone_test_SOURCES = clone_test.S
ll_SOURCES = ll.S

AM_CCASFLAGS += @FLAG_M32@
