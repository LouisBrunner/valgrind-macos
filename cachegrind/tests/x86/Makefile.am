noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	fpu-28-108.vgtest fpu-28-108.stderr.exp

check_PROGRAMS = \
	fpu-28-108

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g

fpu_28_108_SOURCES	= fpu-28-108.S
