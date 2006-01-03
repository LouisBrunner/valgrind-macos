
noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	jm-int.stderr.exp jm-int.stdout.exp jm-int.vgtest \
	jm-fp.stderr.exp  jm-fp.stdout.exp  jm-fp.vgtest \
	jm-vmx.stderr.exp jm-vmx.stdout.exp jm-vmx.vgtest

check_PROGRAMS = \
	jm-insns

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

jm_insns_CFLAGS = -Winline -Wall -O -mregnames -DHAS_ALTIVEC -maltivec -m64 
jm_insns_LDFLAGS = -m64
