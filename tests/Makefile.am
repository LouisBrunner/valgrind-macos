
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

noinst_SCRIPTS = \
	vg_regtest \
	filter_addresses \
	filter_discards \
	filter_libc \
	filter_numbers \
	filter_stderr_basic \
	filter_sink \
	filter_test_paths

EXTRA_DIST = $(noinst_SCRIPTS)

check_PROGRAMS = \
	cputest \
	toobig-allocs \
	true

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
cputest_CFLAGS  	= $(AM_CFLAGS) \
				-DVGA_$(VGCONF_ARCH_PRI)=1 \
				-DVGO_$(VGCONF_OS)=1 \
				-DVGP_$(VGCONF_ARCH_PRI)_$(VGCONF_OS)=1
