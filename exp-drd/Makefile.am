include $(top_srcdir)/Makefile.tool.am

noinst_PROGRAMS =
if VGP_X86_LINUX
  noinst_PROGRAMS += exp-drd-x86-linux vgpreload_exp-drd-x86-linux.so
endif
if VGP_AMD64_LINUX
  noinst_PROGRAMS += exp-drd-amd64-linux vgpreload_exp-drd-amd64-linux.so
endif
if VGP_PPC32_LINUX
  noinst_PROGRAMS += exp-drd-ppc32-linux vgpreload_exp-drd-ppc32-linux.so
endif
if VGP_PPC64_LINUX
  noinst_PROGRAMS += exp-drd-ppc64-linux vgpreload_exp-drd-ppc64-linux.so
endif
if VGP_PPC32_AIX5
  noinst_PROGRAMS += exp-drd-ppc32-aix5 vgpreload_exp-drd-ppc32-aix5.so
endif
if VGP_PPC64_AIX5
  noinst_PROGRAMS += exp-drd-ppc64-aix5 vgpreload_exp-drd-ppc64-aix5.so
endif

VGPRELOAD_DRD_SOURCES_COMMON = drd_preloaded.c

vgpreload_exp_drd_x86_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_exp_drd_x86_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
vgpreload_exp_drd_x86_linux_so_CFLAGS       = $(AM_CFLAGS_X86_LINUX) $(AM_CFLAGS_PIC)
vgpreload_exp_drd_x86_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_X86_LINUX)
vgpreload_exp_drd_x86_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_X86_LINUX)
vgpreload_exp_drd_x86_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_X86_LINUX)\
                                          $(LIBREPLACEMALLOC_LDFLAGS_X86_LINUX)

vgpreload_exp_drd_amd64_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_exp_drd_amd64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
vgpreload_exp_drd_amd64_linux_so_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX) $(AM_CFLAGS_PIC)
vgpreload_exp_drd_amd64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_AMD64_LINUX)
vgpreload_exp_drd_amd64_linux_so_DEPENDENCIES =
vgpreload_exp_drd_amd64_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_AMD64_LINUX)

vgpreload_exp_drd_ppc32_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_exp_drd_ppc32_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
vgpreload_exp_drd_ppc32_linux_so_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX) $(AM_CFLAGS_PIC)
vgpreload_exp_drd_ppc32_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_LINUX)
vgpreload_exp_drd_ppc32_linux_so_DEPENDENCIES =
vgpreload_exp_drd_ppc32_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC32_LINUX)

vgpreload_exp_drd_ppc64_linux_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_exp_drd_ppc64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
vgpreload_exp_drd_ppc64_linux_so_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX) $(AM_CFLAGS_PIC)
vgpreload_exp_drd_ppc64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_LINUX)
vgpreload_exp_drd_ppc64_linux_so_DEPENDENCIES =
vgpreload_exp_drd_ppc64_linux_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC64_LINUX)

vgpreload_exp_drd_ppc32_aix5_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_exp_drd_ppc32_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
vgpreload_exp_drd_ppc32_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5) $(AM_CFLAGS_PIC)
vgpreload_exp_drd_ppc32_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_AIX5)
vgpreload_exp_drd_ppc32_aix5_so_DEPENDENCIES =
vgpreload_exp_drd_ppc32_aix5_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC32_AIX5)

vgpreload_exp_drd_ppc64_aix5_so_SOURCES      = $(VGPRELOAD_DRD_SOURCES_COMMON)
vgpreload_exp_drd_ppc64_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
vgpreload_exp_drd_ppc64_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5) $(AM_CFLAGS_PIC)
vgpreload_exp_drd_ppc64_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_AIX5)
vgpreload_exp_drd_ppc64_aix5_so_DEPENDENCIES =
vgpreload_exp_drd_ppc64_aix5_so_LDFLAGS      = $(PRELOAD_LDFLAGS_PPC64_AIX5)


DRD_SOURCES_COMMON =    \
  drd_bitmap.c          \
  drd_clientreq.c       \
  drd_cond.c            \
  drd_error.c           \
  drd_main.c            \
  drd_malloc_wrappers.c \
  drd_mutex.c           \
  drd_segment.c         \
  drd_suppression.c     \
  drd_thread.c          \
  drd_vc.c

noinst_HEADERS = drd_bitmap.h \
		 drd_mutex.h \
		 drd_vc.h \
		 drd_clientreq.h \
		 drd_segment.h \
		 priv_drd_clientreq.h \
		 drd_cond.h \
		 drd_suppression.h \
		 pub_drd_bitmap.h \
		 drd_error.h \
		 drd_thread.h \
		 drd_malloc_wrappers.h \
		 drd_track.h

AM_CFLAGS_X86_LINUX   += -I$(top_srcdir)/coregrind
AM_CFLAGS_AMD64_LINUX += -I$(top_srcdir)/coregrind
AM_CFLAGS_PPC32_LINUX += -I$(top_srcdir)/coregrind
AM_CFLAGS_PPC64_LINUX += -I$(top_srcdir)/coregrind
AM_CFLAGS_PPC32_AIX5  += -I$(top_srcdir)/coregrind
AM_CFLAGS_PPC64_AIX5  += -I$(top_srcdir)/coregrind

exp_drd_x86_linux_SOURCES        = $(DRD_SOURCES_COMMON)
exp_drd_x86_linux_CPPFLAGS       = $(AM_CPPFLAGS_X86_LINUX)
exp_drd_x86_linux_CFLAGS         = $(AM_CFLAGS_X86_LINUX)
exp_drd_x86_linux_DEPENDENCIES   = $(COREGRIND_LIBS_X86_LINUX)
exp_drd_x86_linux_LDADD          = $(TOOL_LDADD_X86_LINUX)
exp_drd_x86_linux_LDFLAGS        = $(TOOL_LDFLAGS_X86_LINUX)

exp_drd_amd64_linux_SOURCES      = $(DRD_SOURCES_COMMON)
exp_drd_amd64_linux_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
exp_drd_amd64_linux_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX)
exp_drd_amd64_linux_DEPENDENCIES = $(COREGRIND_LIBS_AMD64_LINUX)
exp_drd_amd64_linux_LDADD        = $(TOOL_LDADD_AMD64_LINUX)
exp_drd_amd64_linux_LDFLAGS      = $(TOOL_LDFLAGS_AMD64_LINUX)

exp_drd_ppc32_linux_SOURCES      = $(DRD_SOURCES_COMMON)
exp_drd_ppc32_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
exp_drd_ppc32_linux_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX)
exp_drd_ppc32_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_LINUX)
exp_drd_ppc32_linux_LDADD        = $(TOOL_LDADD_PPC32_LINUX)
exp_drd_ppc32_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_LINUX)

exp_drd_ppc64_linux_SOURCES      = $(DRD_SOURCES_COMMON)
exp_drd_ppc64_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
exp_drd_ppc64_linux_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX)
exp_drd_ppc64_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_LINUX)
exp_drd_ppc64_linux_LDADD        = $(TOOL_LDADD_PPC64_LINUX)
exp_drd_ppc64_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_LINUX)

exp_drd_ppc32_aix5_SOURCES      = $(DRD_SOURCES_COMMON)
exp_drd_ppc32_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
exp_drd_ppc32_aix5_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5)
exp_drd_ppc32_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_AIX5)
exp_drd_ppc32_aix5_LDADD        = $(TOOL_LDADD_PPC32_AIX5)
exp_drd_ppc32_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_AIX5)

exp_drd_ppc64_aix5_SOURCES      = $(DRD_SOURCES_COMMON)
exp_drd_ppc64_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
exp_drd_ppc64_aix5_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5)
exp_drd_ppc64_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_AIX5)
exp_drd_ppc64_aix5_LDADD        = $(TOOL_LDADD_PPC64_AIX5)
exp_drd_ppc64_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_AIX5)


#all-local:
#	for f in $(noinst_PROGRAMS); do \
#	  p=`echo $$f | sed -e 's/^[^-]*-[^-]*-//' -e 's/\..*$$//'`; \
#	  n=`echo $$f | sed -e 's/^\([^-]*-[^-]*\)-[^-]*-[^-]*/\1/'`; \
#	  mkdir -p $(inplacedir)/$$p; \
#	  rm -f $(inplacedir)/$$p/$$n; \
#	  ln -f -s ../../$(subdir)/$$f $(inplacedir)/$$p/$$n; \
#	done
