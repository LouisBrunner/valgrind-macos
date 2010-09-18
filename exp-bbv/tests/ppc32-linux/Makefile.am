include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

check_PROGRAMS = \
	million ll

EXTRA_DIST = \
	   ll.stderr.exp \
	   ll.stdout.exp \
	   ll.post.exp \
	   ll.vgtest \
	   million.stderr.exp \
	   million.post.exp \
	   million.vgtest

AM_CCASFLAGS += -ffreestanding -m32

LDFLAGS += -nostartfiles -nodefaultlibs -m32

ll_SOURCES = ll.S
million_SOURCES = million.S
