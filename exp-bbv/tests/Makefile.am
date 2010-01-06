
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .

# Platform-specific tests
if VGCONF_ARCHS_INCLUDE_X86
SUBDIRS += x86
endif
if VGCONF_PLATFORMS_INCLUDE_X86_LINUX
SUBDIRS += x86-linux
endif
if VGCONF_PLATFORMS_INCLUDE_AMD64_LINUX
SUBDIRS += amd64-linux
endif
if VGCONF_PLATFORMS_INCLUDE_PPC32_LINUX
SUBDIRS += ppc32-linux
endif
if VGCONF_PLATFORMS_INCLUDE_ARM_LINUX
SUBDIRS += arm-linux
endif

DIST_SUBDIRS = x86 x86-linux amd64-linux ppc32-linux arm-linux .

dist_noinst_SCRIPTS = \
	filter_bb \
	filter_stderr

EXTRA_DIST = \
	   logo.include logo.lzss_new
	
check_PROGRAMS = 
	
AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

