include $(top_srcdir)/Makefile.tool.am

# include memcheck/ for mac_shared.h
noinst_PROGRAMS = 
if VGP_X86_LINUX
noinst_PROGRAMS += exp-omega-x86-linux vgpreload_exp-omega-x86-linux.so
endif
if VGP_AMD64_LINUX
noinst_PROGRAMS += exp-omega-amd64-linux vgpreload_exp-omega-amd64-linux.so
endif
if VGP_PPC32_LINUX
noinst_PROGRAMS += exp-omega-ppc32-linux vgpreload_exp-omega-ppc32-linux.so
endif
if VGP_PPC64_LINUX
noinst_PROGRAMS += exp-omega-ppc64-linux vgpreload_exp-omega-ppc64-linux.so
endif
if VGP_PPC32_AIX5
noinst_PROGRAMS += exp-omega-ppc32-aix5 vgpreload_exp-omega-ppc32-aix5.so
endif
if VGP_PPC64_AIX5
noinst_PROGRAMS += exp-omega-ppc64-aix5 vgpreload_exp-omega-ppc64-aix5.so
endif

VGPRELOAD_OMEGA_SOURCES_COMMON = o_replace_memops.c

vgpreload_exp_omega_x86_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
vgpreload_exp_omega_x86_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
vgpreload_exp_omega_x86_linux_so_CFLAGS       = $(AM_CFLAGS_X86_LINUX) $(AM_CFLAGS_PIC) -O2
vgpreload_exp_omega_x86_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_X86_LINUX)
vgpreload_exp_omega_x86_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_X86_LINUX)
vgpreload_exp_omega_x86_linux_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_X86_LINUX) \
	$(LIBREPLACEMALLOC_LDFLAGS_X86_LINUX)

vgpreload_exp_omega_amd64_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
vgpreload_exp_omega_amd64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
vgpreload_exp_omega_amd64_linux_so_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX) $(AM_CFLAGS_PIC) -O2
vgpreload_exp_omega_amd64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_AMD64_LINUX)
vgpreload_exp_omega_amd64_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_AMD64_LINUX)
vgpreload_exp_omega_amd64_linux_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_AMD64_LINUX) \
	$(LIBREPLACEMALLOC_LDFLAGS_AMD64_LINUX)

vgpreload_exp_omega_ppc32_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
vgpreload_exp_omega_ppc32_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
vgpreload_exp_omega_ppc32_linux_so_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX) $(AM_CFLAGS_PIC) -O2
vgpreload_exp_omega_ppc32_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_LINUX)
vgpreload_exp_omega_ppc32_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC32_LINUX)
vgpreload_exp_omega_ppc32_linux_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_PPC32_LINUX) \
	$(LIBREPLACEMALLOC_LDFLAGS_PPC32_LINUX)

vgpreload_exp_omega_ppc64_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
vgpreload_exp_omega_ppc64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
vgpreload_exp_omega_ppc64_linux_so_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX) $(AM_CFLAGS_PIC) -O2
vgpreload_exp_omega_ppc64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_LINUX)
vgpreload_exp_omega_ppc64_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC64_LINUX)
vgpreload_exp_omega_ppc64_linux_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_PPC64_LINUX) \
	$(LIBREPLACEMALLOC_LDFLAGS_PPC64_LINUX)

vgpreload_exp_omega_ppc32_aix5_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
vgpreload_exp_omega_ppc32_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
vgpreload_exp_omega_ppc32_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5) $(AM_CFLAGS_PIC) -O2
vgpreload_exp_omega_ppc32_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_AIX5)
vgpreload_exp_omega_ppc32_aix5_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC32_AIX5)
vgpreload_exp_omega_ppc32_aix5_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_PPC32_AIX5) \
	$(LIBREPLACEMALLOC_LDFLAGS_PPC32_AIX5)

vgpreload_exp_omega_ppc64_aix5_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
vgpreload_exp_omega_ppc64_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
vgpreload_exp_omega_ppc64_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5) $(AM_CFLAGS_PIC) -O2
vgpreload_exp_omega_ppc64_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_AIX5)
vgpreload_exp_omega_ppc64_aix5_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC64_AIX5)
vgpreload_exp_omega_ppc64_aix5_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_PPC64_AIX5) \
	$(LIBREPLACEMALLOC_LDFLAGS_PPC64_AIX5)

OMEGA_SOURCES_COMMON = \
	o_main.c

exp_omega_x86_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
exp_omega_x86_linux_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
exp_omega_x86_linux_CFLAGS       = $(AM_CFLAGS_X86_LINUX) -O2
exp_omega_x86_linux_CCASFLAGS    = $(AM_CCASFLAGS_X86_LINUX)
exp_omega_x86_linux_DEPENDENCIES = $(COREGRIND_LIBS_X86_LINUX)
exp_omega_x86_linux_LDADD        = $(TOOL_LDADD_X86_LINUX)
exp_omega_x86_linux_LDFLAGS      = $(TOOL_LDFLAGS_X86_LINUX)

exp_omega_amd64_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
exp_omega_amd64_linux_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
exp_omega_amd64_linux_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX) -g -O0 #-O2
exp_omega_amd64_linux_CCASFLAGS    = $(AM_CCASFLAGS_AMD64_LINUX)
exp_omega_amd64_linux_DEPENDENCIES = $(COREGRIND_LIBS_AMD64_LINUX)
exp_omega_amd64_linux_LDADD        = $(TOOL_LDADD_AMD64_LINUX)
exp_omega_amd64_linux_LDFLAGS      = $(TOOL_LDFLAGS_AMD64_LINUX)

exp_omega_ppc32_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
exp_omega_ppc32_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
exp_omega_ppc32_linux_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX) -O2
exp_omega_ppc32_linux_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_LINUX)
exp_omega_ppc32_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_LINUX)
exp_omega_ppc32_linux_LDADD        = $(TOOL_LDADD_PPC32_LINUX)
exp_omega_ppc32_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_LINUX)

exp_omega_ppc64_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
exp_omega_ppc64_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
exp_omega_ppc64_linux_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX) -O2
exp_omega_ppc64_linux_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_LINUX)
exp_omega_ppc64_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_LINUX)
exp_omega_ppc64_linux_LDADD        = $(TOOL_LDADD_PPC64_LINUX)
exp_omega_ppc64_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_LINUX)

exp_omega_ppc32_aix5_SOURCES      = $(OMEGA_SOURCES_COMMON)
exp_omega_ppc32_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
exp_omega_ppc32_aix5_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5) -O2
exp_omega_ppc32_aix5_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_AIX5)
exp_omega_ppc32_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_AIX5)
exp_omega_ppc32_aix5_LDADD        = $(TOOL_LDADD_PPC32_AIX5)
exp_omega_ppc32_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_AIX5)

exp_omega_ppc64_aix5_SOURCES      = $(OMEGA_SOURCES_COMMON)
exp_omega_ppc64_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
exp_omega_ppc64_aix5_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5) -O2
exp_omega_ppc64_aix5_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_AIX5)
exp_omega_ppc64_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_AIX5)
exp_omega_ppc64_aix5_LDADD        = $(TOOL_LDADD_PPC64_AIX5)
exp_omega_ppc64_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_AIX5)

oincludedir = $(includedir)/valgrind

oinclude_HEADERS = exp-omega.h

noinst_HEADERS =

o_replace_memops.o: CFLAGS += -fno-omit-frame-pointer

o_main.o: CFLAGS += -fno-omit-frame-pointer #-fomit-frame-pointer
