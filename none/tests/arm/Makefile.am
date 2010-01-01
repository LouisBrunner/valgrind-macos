
### jrs: re-check this against known-good equivalents, eg x86 version

EXTRA_DIST = $(noinst_SCRIPTS) \
	instructions.stderr.exp instructions.stdout.exp
# if any tests appear here, remember to include @FLAG_M32@ in the
# compilation flags
#

check_PROGRAMS = instructions

AM_CFLAGS    = $(WERROR) -Winline -Wall -Wshadow \
		@FLAG_M32@ -g -I$(top_srcdir)/include \
		$(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  = $(AM_CFLAGS)
AM_CCASFLAGS = @FLAG_M32@
