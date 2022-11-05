include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	445032.vgtest \
	445032.stderr.exp

check_PROGRAMS = \
	445032

# Linux also adds  $(FLAG_MMMX) $(FLAG_MSSE) to the first two
AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@
