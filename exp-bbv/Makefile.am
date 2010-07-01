include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = docs/bbv-manual.xml

#----------------------------------------------------------------------------
# exp-bbv-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = exp-bbv-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += exp-bbv-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

BBV_SOURCES_COMMON = bbv_main.c

exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(BBV_SOURCES_COMMON)
exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(exp_bbv_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(BBV_SOURCES_COMMON)
exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(exp_bbv_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif
