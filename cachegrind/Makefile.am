include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = \
	docs/cg-manual.xml \
	docs/cg_annotate-manpage.xml \
	docs/cg_diff-manpage.xml \
	docs/cg_merge-manpage.xml

#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

pkginclude_HEADERS = cachegrind.h

bin_SCRIPTS = cg_annotate cg_diff cg_merge

noinst_HEADERS = \
	cg_arch.h \
	cg_branchpred.c \
	cg_sim.c

#----------------------------------------------------------------------------
# cachegrind-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = cachegrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += cachegrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

CACHEGRIND_SOURCES_COMMON = \
	cg_main.c \
	cg_arch.c

cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(CACHEGRIND_SOURCES_COMMON)
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cachegrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
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
cachegrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS)\
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

#----------------------------------------------------------------------------
# Miscellaneous
#----------------------------------------------------------------------------

# "Build" `cg_annotate`. The `+` avoids warnings about the jobserver.
pyann:
	+../auxprogs/pybuild.sh ../auxprogs cg_annotate.in cg_annotate

# "Build" `cg_diff`. The `+` avoids warnings about the jobserver.
pydiff:
	+../auxprogs/pybuild.sh ../auxprogs cg_diff.in cg_diff

# "Build" `cg_merge`. The `+` avoids warnings about the jobserver.
pymerge:
	+../auxprogs/pybuild.sh ../auxprogs cg_merge.in cg_merge

.PHONY: pyann pydiff pymerge
