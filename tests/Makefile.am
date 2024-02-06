
include $(top_srcdir)/Makefile.tool-tests.am

if HAS_ISA_2_05
ISA_2_05_FLAG = -DHAS_ISA_2_05
else
ISA_2_05_FLAG =
endif

if HAS_ISA_2_06
ISA_2_06_FLAG = -DHAS_ISA_2_06
else
ISA_2_06_FLAG =
endif

if HAS_ISA_2_07
ISA_2_07_FLAG = -DHAS_ISA_2_07
else
ISA_2_07_FLAG =
endif

min_power_isa_FLAGS = $(ISA_2_05_FLAG)  $(ISA_2_06_FLAG) $(ISA_2_07_FLAG)

dist_noinst_SCRIPTS = \
	check_headers_and_includes \
	check_makefile_consistency \
	check_ppc64_auxv_cap \
	filter_addresses \
	filter_libc \
	filter_numbers \
	filter_stderr_basic \
	filter_sink \
	filter_xml_frames \
	platform_test \
	post_regtest_checks \
	vg_regtest

noinst_SCRIPTS = \
	filter_discards

noinst_HEADERS = \
	asm.h \
	malloc.h \
	sys_mman.h

check_PROGRAMS = \
	arch_test \
	os_test \
	libc_test \
	true \
	x86_amd64_features \
	s390x_features \
	mips_features \
	power_insn_available \
	is_ppc64_BE \
	min_power_isa \
	arm64_features


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

if VGCONF_OS_IS_DARWIN
x86_amd64_features_CFLAGS = $(AM_CFLAGS) -mdynamic-no-pic
else
x86_amd64_features_CFLAGS = $(AM_CFLAGS)
endif

min_power_isa_CFLAGS = $(min_power_isa_FLAGS)
