
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = \
	filter_addresses \
	filter_discards \
	filter_libc \
	filter_numbers \
	filter_stderr_basic \
	filter_sink \
	os_test \
	platform_test \
	vg_regtest

EXTRA_DIST = $(noinst_SCRIPTS)

check_PROGRAMS = \
	arch_test \
	true

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

