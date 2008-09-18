
noinst_SCRIPTS = filter_stderr

check_PROGRAMS = asm lea

EXTRA_DIST = $(noinst_SCRIPTS) \
	asm.vgtest-disabled asm.stderr.exp \
	lea.vgtest-disabled lea.stderr.exp

AM_CPPFLAGS  = -I$(top_srcdir)/include
AM_CFLAGS    = $(WERROR) @FLAG_M32@ -Winline -Wall -Wshadow -g \
		$(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  = $(AM_CFLAGS)
AM_CCASFLAGS = @FLAG_M32@

asm_SOURCES = asm.S
lea_SOURCES = lea.S
