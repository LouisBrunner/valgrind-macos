include $(top_srcdir)/Makefile.tool.am

noinst_PROGRAMS =
if VGP_X86_LINUX
  noinst_PROGRAMS += drd-x86-linux   vgpreload_drd-x86-linux.so
endif
if VGP_AMD64_LINUX
  noinst_PROGRAMS += drd-amd64-linux vgpreload_drd-amd64-linux.so
endif
if VGP_PPC32_LINUX
  noinst_PROGRAMS += drd-ppc32-linux vgpreload_drd-ppc32-linux.so
endif
if VGP_PPC64_LINUX
  noinst_PROGRAMS += drd-ppc64-linux vgpreload_drd-ppc64-linux.so
endif
if VGP_PPC32_AIX5
  noinst_PROGRAMS += drd-ppc32-aix5  vgpreload_drd-ppc32-aix5.so
endif
if VGP_PPC64_AIX5
  noinst_PROGRAMS += drd-ppc64-aix5  vgpreload_drd-ppc64-aix5.so
endif


VGPRELOAD_DRD_SOURCES =    \
  drd_gomp_intercepts.c    \
  drd_pthread_intercepts.c \
  drd_qtcore_intercepts.c  \
  drd_strmem_intercepts.c


DRD_CFLAGS =				\
  @FLAG_UNLIMITED_INLINE_UNIT_GROWTH@	\
  -O2					\
  @FLAG_W_EXTRA@			\
  -Wformat-nonliteral			\
  -Wno-inline				\
  -Wno-unused-parameter

AM_CFLAGS_X86_LINUX   += $(DRD_CFLAGS)
AM_CFLAGS_AMD64_LINUX += $(DRD_CFLAGS)
AM_CFLAGS_PPC32_LINUX += $(DRD_CFLAGS)
AM_CFLAGS_PPC64_LINUX += $(DRD_CFLAGS)
AM_CFLAGS_PPC32_AIX5  += $(DRD_CFLAGS)
AM_CFLAGS_PPC64_AIX5  += $(DRD_CFLAGS)

vgpreload_drd_x86_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES)
vgpreload_drd_x86_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
vgpreload_drd_x86_linux_so_CFLAGS       = $(AM_CFLAGS_X86_LINUX) $(AM_CFLAGS_PIC)
vgpreload_drd_x86_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_X86_LINUX)
vgpreload_drd_x86_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_X86_LINUX)
vgpreload_drd_x86_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_X86_LINUX)\
                                        $(LIBREPLACEMALLOC_LDFLAGS_X86_LINUX)

vgpreload_drd_amd64_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES)
vgpreload_drd_amd64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
vgpreload_drd_amd64_linux_so_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX) $(AM_CFLAGS_PIC)
vgpreload_drd_amd64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_AMD64_LINUX)
vgpreload_drd_amd64_linux_so_DEPENDENCIES =
vgpreload_drd_amd64_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_AMD64_LINUX)\
                                        $(LIBREPLACEMALLOC_LDFLAGS_AMD64_LINUX)

vgpreload_drd_ppc32_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES)
vgpreload_drd_ppc32_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
vgpreload_drd_ppc32_linux_so_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX) $(AM_CFLAGS_PIC)
vgpreload_drd_ppc32_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_LINUX)
vgpreload_drd_ppc32_linux_so_DEPENDENCIES =
vgpreload_drd_ppc32_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC32_LINUX)\
                                        $(LIBREPLACEMALLOC_LDFLAGS_PPC32_LINUX)

vgpreload_drd_ppc64_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES)
vgpreload_drd_ppc64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
vgpreload_drd_ppc64_linux_so_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX) $(AM_CFLAGS_PIC)
vgpreload_drd_ppc64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_LINUX)
vgpreload_drd_ppc64_linux_so_DEPENDENCIES =
vgpreload_drd_ppc64_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC64_LINUX)\
                                        $(LIBREPLACEMALLOC_LDFLAGS_PPC64_LINUX)

vgpreload_drd_ppc32_aix5_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES)
vgpreload_drd_ppc32_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
vgpreload_drd_ppc32_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5) $(AM_CFLAGS_PIC)
vgpreload_drd_ppc32_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_AIX5)
vgpreload_drd_ppc32_aix5_so_DEPENDENCIES =
vgpreload_drd_ppc32_aix5_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC32_AIX5)\
                                        $(LIBREPLACEMALLOC_LDFLAGS_PPC32_AIX5)

vgpreload_drd_ppc64_aix5_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES)
vgpreload_drd_ppc64_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
vgpreload_drd_ppc64_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5) $(AM_CFLAGS_PIC)
vgpreload_drd_ppc64_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_AIX5)
vgpreload_drd_ppc64_aix5_so_DEPENDENCIES =
vgpreload_drd_ppc64_aix5_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC64_AIX5)\
                                        $(LIBREPLACEMALLOC_LDFLAGS_PPC64_AIX5)


DRD_SOURCES =    \
  drd_barrier.c         \
  drd_clientobj.c       \
  drd_clientreq.c       \
  drd_cond.c            \
  drd_error.c           \
  drd_main.c            \
  drd_malloc_wrappers.c \
  drd_mutex.c           \
  drd_rwlock.c          \
  drd_semaphore.c       \
  drd_suppression.c

drdincludedir = $(includedir)/valgrind

drdinclude_HEADERS = drd.h

noinst_HEADERS =        \
  drd_barrier.h         \
  drd_bitmap.c          \
  drd_bitmap.h          \
  drd_clientobj.h       \
  drd_clientreq.h       \
  drd_cond.h            \
  drd_error.h           \
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
  drd_track.h           \
  drd_vc.c              \
  drd_vc.h              \
  pub_drd_bitmap.h

drd_x86_linux_SOURCES        = $(DRD_SOURCES)
drd_x86_linux_CPPFLAGS       = $(AM_CPPFLAGS_X86_LINUX)
drd_x86_linux_CFLAGS         = $(AM_CFLAGS_X86_LINUX)
drd_x86_linux_DEPENDENCIES   = $(COREGRIND_LIBS_X86_LINUX)
drd_x86_linux_LDADD          = $(TOOL_LDADD_X86_LINUX)
drd_x86_linux_LDFLAGS        = $(TOOL_LDFLAGS_X86_LINUX)

drd_amd64_linux_SOURCES      = $(DRD_SOURCES)
drd_amd64_linux_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
drd_amd64_linux_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX)
drd_amd64_linux_DEPENDENCIES = $(COREGRIND_LIBS_AMD64_LINUX)
drd_amd64_linux_LDADD        = $(TOOL_LDADD_AMD64_LINUX)
drd_amd64_linux_LDFLAGS      = $(TOOL_LDFLAGS_AMD64_LINUX)

drd_ppc32_linux_SOURCES      = $(DRD_SOURCES)
drd_ppc32_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
drd_ppc32_linux_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX)
drd_ppc32_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_LINUX)
drd_ppc32_linux_LDADD        = $(TOOL_LDADD_PPC32_LINUX)
drd_ppc32_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_LINUX)

drd_ppc64_linux_SOURCES      = $(DRD_SOURCES)
drd_ppc64_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
drd_ppc64_linux_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX)
drd_ppc64_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_LINUX)
drd_ppc64_linux_LDADD        = $(TOOL_LDADD_PPC64_LINUX)
drd_ppc64_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_LINUX)

drd_ppc32_aix5_SOURCES      = $(DRD_SOURCES)
drd_ppc32_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
drd_ppc32_aix5_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5)
drd_ppc32_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_AIX5)
drd_ppc32_aix5_LDADD        = $(TOOL_LDADD_PPC32_AIX5)
drd_ppc32_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_AIX5)

drd_ppc64_aix5_SOURCES      = $(DRD_SOURCES)
drd_ppc64_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
drd_ppc64_aix5_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5)
drd_ppc64_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_AIX5)
drd_ppc64_aix5_LDADD        = $(TOOL_LDADD_PPC64_AIX5)
drd_ppc64_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_AIX5)
