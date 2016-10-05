include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

check_PROGRAMS = \
	million rep_prefix fldcw_check complex_rep

EXTRA_DIST = \
	   complex_rep.stderr.exp \
	   complex_rep.vgtest \
	   fldcw_check.stderr.exp \
	   fldcw_check.vgtest \
	   million.stderr.exp \
	   million.post.exp \
	   million.vgtest \
	   rep_prefix.stderr.exp \
	   rep_prefix.vgtest 

AM_CCASFLAGS += -ffreestanding

AM_LDFLAGS = @FLAG_M32@ -static -nostartfiles -nodefaultlibs

complex_rep_SOURCES = complex_rep.S
fldcw_check_SOURCES = fldcw_check.S
million_SOURCES = million.S
rep_prefix_SOURCES = rep_prefix.S

AM_CCASFLAGS += @FLAG_M32@
