include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = docs/drd-manual.xml

#----------------------------------------------------------------------------
# Headers, flags
#----------------------------------------------------------------------------

pkginclude_HEADERS = drd.h

noinst_HEADERS =        \
  drd_barrier.h         \
  drd_basics.h          \
  drd_bitmap.c          \
  drd_bitmap.h          \
  drd_clientobj.h       \
  drd_clientreq.h       \
  drd_cond.h            \
  drd_error.h           \
  drd_hb.h              \
  drd_load_store.h      \
  drd_malloc_wrappers.h \
  drd_mutex.h           \
  drd_rwlock.h          \
  drd_segment.c         \
  drd_segment.h         \
  drd_semaphore.h       \
  drd_suppression.h     \
  drd_thread.c          \
  drd_thread.h          \
  drd_thread_bitmap.h   \
  drd_vc.c              \
  drd_vc.h              \
  pub_drd_bitmap.h

DRD_CFLAGS =				\
  @FLAG_UNLIMITED_INLINE_UNIT_GROWTH@	\
  -O2					\
  @FLAG_W_EXTRA@			\
  -Wformat-nonliteral			\
  -Wno-inline				\
  -Wno-unused-parameter

#DRD_CFLAGS += -DENABLE_DRD_CONSISTENCY_CHECKS

#----------------------------------------------------------------------------
# drd-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = drd-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += drd-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

DRD_SOURCES_COMMON =    \
  drd_barrier.c         \
  drd_bitmap2_node.c    \
  drd_clientobj.c       \
  drd_clientreq.c       \
  drd_cond.c            \
  drd_cond_initializer.c \
  drd_error.c           \
  drd_hb.c              \
  drd_load_store.c      \
  drd_main.c            \
  drd_malloc_wrappers.c \
  drd_mutex.c           \
  drd_rwlock.c          \
  drd_semaphore.c       \
  drd_suppression.c

drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(DRD_SOURCES_COMMON)
drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) $(DRD_CFLAGS)
drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(DRD_SOURCES_COMMON)
drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) $(DRD_CFLAGS)
drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif

#----------------------------------------------------------------------------
# vgpreload_drd-<platform>.so
#----------------------------------------------------------------------------

noinst_PROGRAMS += vgpreload_drd-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += vgpreload_drd-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

VGPRELOAD_DRD_SOURCES_COMMON = \
  drd_pthread_intercepts.c     \
  drd_qtcore_intercepts.c      \
  drd_strmem_intercepts.c

if VGCONF_OS_IS_DARWIN
VGPRELOAD_DRD_SOURCES_COMMON += drd_darwin_intercepts.c
endif

vgpreload_drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_SOURCES      = \
	$(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) $(AM_CFLAGS_PIC) $(DRD_CFLAGS)
vgpreload_drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_drd_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)

if VGCONF_HAVE_PLATFORM_SEC
vgpreload_drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_SOURCES      = \
	$(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) $(AM_CFLAGS_PIC) $(DRD_CFLAGS)
vgpreload_drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_drd_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif

