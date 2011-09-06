include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST =  \
	docs/callgrind_annotate-manpage.xml \
	docs/callgrind_control-manpage.xml \
	docs/cl-manual.xml \
	docs/cl-format.xml

#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

pkginclude_HEADERS = callgrind.h

bin_SCRIPTS = \
	callgrind_annotate \
	callgrind_control

noinst_HEADERS = \
	costs.h \
	events.h \
	global.h

#----------------------------------------------------------------------------
# callgrind-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = callgrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += callgrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

CALLGRIND_SOURCES_COMMON = \
	bb.c \
	bbcc.c \
	callstack.c \
	clo.c \
	command.c \
	context.c \
	costs.c \
	debug.c \
	dump.c \
	events.c \
	fn.c \
	jumps.c \
	main.c \
	sim.c \
	threads.c \
	../cachegrind/cg-arch.c \
	../cachegrind/cg-x86-amd64.c \
	../cachegrind/cg-ppc32.c \
	../cachegrind/cg-ppc64.c \
	../cachegrind/cg-arm.c   \
	../cachegrind/cg-s390x.c

CALLGRIND_CFLAGS_COMMON = -I$(top_srcdir)/cachegrind

callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(CALLGRIND_SOURCES_COMMON)
callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) $(CALLGRIND_CFLAGS_COMMON)
callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(callgrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(CALLGRIND_SOURCES_COMMON)
callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) $(CALLGRIND_CFLAGS_COMMON)
callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(callgrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif

