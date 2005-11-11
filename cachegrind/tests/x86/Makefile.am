noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	fpu-28-108.vgtest fpu-28-108.stderr.exp

check_PROGRAMS = \
	fpu-28-108

AM_CFLAGS    = $(WERROR) @FLAG_M32@ -Winline -Wall -Wshadow -g
AM_CCASFLAGS = @FLAG_M32@

fpu_28_108_SOURCES	= fpu-28-108.S
