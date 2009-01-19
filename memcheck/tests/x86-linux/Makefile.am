noinst_SCRIPTS = filter_stderr

noinst_HEADERS = scalar.h

EXTRA_DIST = $(noinst_SCRIPTS) \
	scalar.stderr.exp scalar.stderr.exp2 scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_exit_group.stderr.exp scalar_exit_group.stderr.exp2 \
	scalar_exit_group.vgtest \
	scalar_supp.stderr.exp scalar_supp.stderr.exp2 \
	scalar_supp.vgtest scalar_supp.supp \
	scalar_vfork.stderr.exp scalar_vfork.vgtest

check_PROGRAMS = \
	scalar scalar_exit_group scalar_fork scalar_supp scalar_vfork

AM_CPPFLAGS  = -I$(top_srcdir)/include
AM_CFLAGS    = $(WERROR) @FLAG_M32@ -Winline -Wall -Wshadow -g \
		$(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  = $(AM_CFLAGS)
AM_CCASFLAGS = @FLAG_M32@
