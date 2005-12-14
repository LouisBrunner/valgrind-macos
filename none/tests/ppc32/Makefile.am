
noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	lsw.stderr.exp lsw.stdout.exp lsw.vgtest \
	jm-int.stderr.exp jm-int.stdout.exp jm-int.vgtest \
	jm-vmx.stderr.exp   jm-vmx.stdout.exp   jm-vmx.vgtest \
	testVMX.stderr.exp  testVMX.stdout.exp  testVMX.vgtest

check_PROGRAMS = \
	lsw jm-insns testVMX

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

jm_insns_CFLAGS = -Winline -Wall -O -mregnames -DHAS_ALTIVEC -maltivec 
testVMX_CFLAGS  = -O -g -Wall -maltivec -mabi=altivec -DALTIVEC -DGCC_COMPILER
