include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = \
	docs/hg-manual.xml \
	README_MSMProp2.txt \
	README_YARD.txt

#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

pkginclude_HEADERS = helgrind.h

noinst_HEADERS = \
	hg_addrdescr.h \
	hg_basics.h \
	hg_errors.h \
	hg_lock_n_thread.h \
	hg_wordset.h \
	libhb.h

#----------------------------------------------------------------------------
# helgrind-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = helgrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += helgrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

HELGRIND_SOURCES_COMMON = \
	hg_addrdescr.c \
	hg_basics.c \
	hg_errors.c \
	hg_lock_n_thread.c \
	hg_main.c \
	hg_wordset.c \
	libhb_core.c

helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(HELGRIND_SOURCES_COMMON)
helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) -O2
helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(HELGRIND_SOURCES_COMMON)
helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) -O2
helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif

#----------------------------------------------------------------------------
# vgpreload_helgrind-<platform>.so
#----------------------------------------------------------------------------

noinst_PROGRAMS += vgpreload_helgrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += vgpreload_helgrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

VGPRELOAD_HELGRIND_SOURCES_COMMON = hg_intercepts.c

vgpreload_helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_SOURCES      = \
	$(VGPRELOAD_HELGRIND_SOURCES_COMMON)
vgpreload_helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_helgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)

if VGCONF_HAVE_PLATFORM_SEC
vgpreload_helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_SOURCES      = \
	$(VGPRELOAD_HELGRIND_SOURCES_COMMON)
vgpreload_helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_helgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif

