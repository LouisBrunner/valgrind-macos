
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_add filter_suppgen

EXTRA_DIST = \
	is_arch_supported \
	bad_percentify.vgtest bad_percentify.c \
	bad_percentify.stdout.exp bad_percentify.stderr.exp-glibc28-amd64 \
	globalerr.vgtest globalerr.stdout.exp \
	globalerr.stderr.exp-glibc28-amd64 \
	hackedbz2.vgtest hackedbz2.stdout.exp \
	hackedbz2.stderr.exp-glibc28-amd64 \
	hsg.vgtest hsg.stdout.exp hsg.stderr.exp \
	preen_invars.vgtest preen_invars.stdout.exp \
	preen_invars.stderr.exp-glibc28-amd64 \
	stackerr.vgtest stackerr.stdout.exp \
	stackerr.stderr.exp-glibc28-amd64 stackerr.stderr.exp-glibc27-x86

check_PROGRAMS = \
	bad_percentify \
	globalerr hackedbz2 \
	hsg \
	preen_invars preen_invars_so.so \
	stackerr

# DDD: not sure if these ones should work on Darwin or not... if not, should
# be moved into x86-linux/.
#if ! VGCONF_OS_IS_DARWIN
#   check_PROGRAMS += \
#	ccc
#endif


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# To make it a bit more realistic, build hackedbz2.c with at 
# least some optimisation.
hackedbz2_CFLAGS	= $(AM_CFLAGS) -O -Wno-inline

# C ones
#pth_create_LDADD	= -lpthread

# C++ ones
#ccc_SOURCES		= ccc.cpp

# Build shared object for preen_invars
preen_invars_DEPENDENCIES      = preen_invars_so.so
if VGCONF_PLATFORMS_INCLUDE_PPC64_AIX5 
 preen_invars_LDADD            = -ldl
 preen_invars_LDFLAGS          = $(AM_FLAG_M3264_PRI)
else
if VGCONF_PLATFORMS_INCLUDE_PPC32_AIX5
 preen_invars_LDADD            = -ldl
 preen_invars_LDFLAGS          = $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
else
if VGCONF_OS_IS_DARWIN
 preen_invars_LDADD            = -ldl
 preen_invars_LDFLAGS          = $(AM_FLAG_M3264_PRI)
else
 preen_invars_LDADD            = -ldl
 preen_invars_LDFLAGS          = $(AM_FLAG_M3264_PRI) \
				-Wl,-rpath,$(top_builddir)/memcheck/tests
endif
endif
endif

preen_invars_so_so_SOURCES      = preen_invars_so.c	# For automake-1.7.
preen_invars_so_so_CFLAGS       = $(AM_CFLAGS) -fpic
if VGCONF_PLATFORMS_INCLUDE_PPC64_AIX5
 preen_invars_so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared
else
if VGCONF_PLATFORMS_INCLUDE_PPC32_AIX5
 preen_invars_so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared \
					-Wl,-G -Wl,-bnogc
else
if VGCONF_OS_IS_DARWIN
 preen_invars_so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -dynamic \
					-dynamiclib -all_load
else
 preen_invars_so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared \
					-Wl,-soname -Wl,preen_invars_so.so
endif
endif
endif

