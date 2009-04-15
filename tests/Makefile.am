
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = \
	filter_addresses \
	filter_discards \
	filter_libc \
	filter_numbers \
	filter_stderr_basic \
	filter_sink \
	platform_test \
	vg_regtest

noinst_HEADERS = \
	asm.h \
	malloc.h \
	sys_mman.h

EXTRA_DIST = $(noinst_SCRIPTS)

check_PROGRAMS = \
	arch_test \
	os_test \
	true \
	x86_amd64_features

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

