include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = \
	docs/cg-manual.xml \
	docs/cg_annotate-manpage.xml

#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

bin_SCRIPTS = cg_annotate cg_diff

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
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
cg_merge_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif

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
	cg-ppc64.c \
 	cg-arm.c   \
	cg-s390x.c

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
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

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
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif


