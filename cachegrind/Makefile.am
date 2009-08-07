include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = \
	docs/cg-manual.xml \
	docs/cg_annotate-manpage.xml

#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

bin_SCRIPTS = cg_annotate

noinst_HEADERS = \
	cg_arch.h \
	cg_branchpred.c \
	cg_sim.c

#----------------------------------------------------------------------------
# cg_merge (built for the primary target only)
#----------------------------------------------------------------------------

bin_PROGRAMS = cg_merge

cg_merge_SOURCES = cg_merge.c
cg_merge_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
cg_merge_CFLAGS    = $(AM_CFLAGS_PRI)
cg_merge_CCASFLAGS = $(AM_CCASFLAGS_PRI)
cg_merge_LDFLAGS   = $(AM_CFLAGS_PRI)

#----------------------------------------------------------------------------
# cachegrind-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = cachegrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += cachegrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

CACHEGRIND_SOURCES_COMMON = \
	cg_main.c \
	cg-x86-amd64.c \
	cg-ppc32.c \
	cg-ppc64.c

cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(CACHEGRIND_SOURCES_COMMON)
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
if VGCONF_HAVE_PLATFORM_SEC
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(CACHEGRIND_SOURCES_COMMON)
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif


