include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = docs/mc-manual.xml docs/mc-tech-docs.xml

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

pkginclude_HEADERS = \
	memcheck.h

noinst_HEADERS = \
	mc_include.h

#----------------------------------------------------------------------------
# memcheck-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = memcheck-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += memcheck-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

MEMCHECK_SOURCES_COMMON = \
	mc_leakcheck.c \
	mc_malloc_wrappers.c \
	mc_main.c mc_main_asm.c \
	mc_translate.c \
	mc_machine.c \
	mc_errors.c

memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(MEMCHECK_SOURCES_COMMON)
memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       =  $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) -O2
memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(MEMCHECK_SOURCES_COMMON)
memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) -O2
memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif

#----------------------------------------------------------------------------
# vgpreload_memcheck-<platform>.so
#----------------------------------------------------------------------------

noinst_PROGRAMS += vgpreload_memcheck-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += vgpreload_memcheck-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

# mc_replace_strmem.c runs on the simulated CPU, and is built with
# AM_CFLAGS_PSO_* (see $(top_srcdir)/Makefile.all.am).
VGPRELOAD_MEMCHECK_SOURCES_COMMON = mc_replace_strmem.c

vgpreload_memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_SOURCES      = \
	$(VGPRELOAD_MEMCHECK_SOURCES_COMMON)
vgpreload_memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_PRI_CAPS@) -O2
vgpreload_memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_memcheck_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)

if VGCONF_HAVE_PLATFORM_SEC
vgpreload_memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_SOURCES      = \
	$(VGPRELOAD_MEMCHECK_SOURCES_COMMON)
vgpreload_memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_SEC_CAPS@) -O2
vgpreload_memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_memcheck_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif

