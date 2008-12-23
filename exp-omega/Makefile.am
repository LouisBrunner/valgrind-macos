
# Don't build Omega.  Bundle all the relevant stuff up into
# the tarfile, though.

SUBDIRS = docs tests

EXTRA_DIST = exp-omega.h o_main.c o_replace_memops.c

###--- The original Makefile.am begins here ---###
##zz 
##zz include $(top_srcdir)/Makefile.tool.am
##zz 
##zz # include memcheck/ for mac_shared.h
##zz noinst_PROGRAMS = 
##zz if VGP_X86_LINUX
##zz noinst_PROGRAMS += exp-omega-x86-linux vgpreload_exp-omega-x86-linux.so
##zz endif
##zz if VGP_AMD64_LINUX
##zz noinst_PROGRAMS += exp-omega-amd64-linux vgpreload_exp-omega-amd64-linux.so
##zz endif
##zz if VGP_PPC32_LINUX
##zz noinst_PROGRAMS += exp-omega-ppc32-linux vgpreload_exp-omega-ppc32-linux.so
##zz endif
##zz if VGP_PPC64_LINUX
##zz noinst_PROGRAMS += exp-omega-ppc64-linux vgpreload_exp-omega-ppc64-linux.so
##zz endif
##zz if VGP_PPC32_AIX5
##zz noinst_PROGRAMS += exp-omega-ppc32-aix5 vgpreload_exp-omega-ppc32-aix5.so
##zz endif
##zz if VGP_PPC64_AIX5
##zz noinst_PROGRAMS += exp-omega-ppc64-aix5 vgpreload_exp-omega-ppc64-aix5.so
##zz endif
##zz 
##zz VGPRELOAD_OMEGA_SOURCES_COMMON = o_replace_memops.c
##zz 
##zz vgpreload_exp_omega_x86_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
##zz vgpreload_exp_omega_x86_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
##zz vgpreload_exp_omega_x86_linux_so_CFLAGS       = $(AM_CFLAGS_X86_LINUX) $(AM_CFLAGS_PIC) -O2
##zz vgpreload_exp_omega_x86_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_X86_LINUX)
##zz vgpreload_exp_omega_x86_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_X86_LINUX)
##zz vgpreload_exp_omega_x86_linux_so_LDFLAGS      = \
##zz 	$(PRELOAD_LDFLAGS_X86_LINUX) \
##zz 	$(LIBREPLACEMALLOC_LDFLAGS_X86_LINUX)
##zz 
##zz vgpreload_exp_omega_amd64_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
##zz vgpreload_exp_omega_amd64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
##zz vgpreload_exp_omega_amd64_linux_so_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX) $(AM_CFLAGS_PIC) -O2
##zz vgpreload_exp_omega_amd64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_AMD64_LINUX)
##zz vgpreload_exp_omega_amd64_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_AMD64_LINUX)
##zz vgpreload_exp_omega_amd64_linux_so_LDFLAGS      = \
##zz 	$(PRELOAD_LDFLAGS_AMD64_LINUX) \
##zz 	$(LIBREPLACEMALLOC_LDFLAGS_AMD64_LINUX)
##zz 
##zz vgpreload_exp_omega_ppc32_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
##zz vgpreload_exp_omega_ppc32_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
##zz vgpreload_exp_omega_ppc32_linux_so_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX) $(AM_CFLAGS_PIC) -O2
##zz vgpreload_exp_omega_ppc32_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_LINUX)
##zz vgpreload_exp_omega_ppc32_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC32_LINUX)
##zz vgpreload_exp_omega_ppc32_linux_so_LDFLAGS      = \
##zz 	$(PRELOAD_LDFLAGS_PPC32_LINUX) \
##zz 	$(LIBREPLACEMALLOC_LDFLAGS_PPC32_LINUX)
##zz 
##zz vgpreload_exp_omega_ppc64_linux_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
##zz vgpreload_exp_omega_ppc64_linux_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
##zz vgpreload_exp_omega_ppc64_linux_so_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX) $(AM_CFLAGS_PIC) -O2
##zz vgpreload_exp_omega_ppc64_linux_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_LINUX)
##zz vgpreload_exp_omega_ppc64_linux_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC64_LINUX)
##zz vgpreload_exp_omega_ppc64_linux_so_LDFLAGS      = \
##zz 	$(PRELOAD_LDFLAGS_PPC64_LINUX) \
##zz 	$(LIBREPLACEMALLOC_LDFLAGS_PPC64_LINUX)
##zz 
##zz vgpreload_exp_omega_ppc32_aix5_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
##zz vgpreload_exp_omega_ppc32_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
##zz vgpreload_exp_omega_ppc32_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5) $(AM_CFLAGS_PIC) -O2
##zz vgpreload_exp_omega_ppc32_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_AIX5)
##zz vgpreload_exp_omega_ppc32_aix5_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC32_AIX5)
##zz vgpreload_exp_omega_ppc32_aix5_so_LDFLAGS      = \
##zz 	$(PRELOAD_LDFLAGS_PPC32_AIX5) \
##zz 	$(LIBREPLACEMALLOC_LDFLAGS_PPC32_AIX5)
##zz 
##zz vgpreload_exp_omega_ppc64_aix5_so_SOURCES      = $(VGPRELOAD_OMEGA_SOURCES_COMMON)
##zz vgpreload_exp_omega_ppc64_aix5_so_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
##zz vgpreload_exp_omega_ppc64_aix5_so_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5) $(AM_CFLAGS_PIC) -O2
##zz vgpreload_exp_omega_ppc64_aix5_so_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_AIX5)
##zz vgpreload_exp_omega_ppc64_aix5_so_DEPENDENCIES = $(LIBREPLACEMALLOC_PPC64_AIX5)
##zz vgpreload_exp_omega_ppc64_aix5_so_LDFLAGS      = \
##zz 	$(PRELOAD_LDFLAGS_PPC64_AIX5) \
##zz 	$(LIBREPLACEMALLOC_LDFLAGS_PPC64_AIX5)
##zz 
##zz OMEGA_SOURCES_COMMON = \
##zz 	o_main.c
##zz 
##zz exp_omega_x86_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
##zz exp_omega_x86_linux_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
##zz exp_omega_x86_linux_CFLAGS       = $(AM_CFLAGS_X86_LINUX) -O2
##zz exp_omega_x86_linux_CCASFLAGS    = $(AM_CCASFLAGS_X86_LINUX)
##zz exp_omega_x86_linux_DEPENDENCIES = $(COREGRIND_LIBS_X86_LINUX)
##zz exp_omega_x86_linux_LDADD        = $(TOOL_LDADD_X86_LINUX)
##zz exp_omega_x86_linux_LDFLAGS      = $(TOOL_LDFLAGS_X86_LINUX)
##zz 
##zz exp_omega_amd64_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
##zz exp_omega_amd64_linux_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
##zz exp_omega_amd64_linux_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX) -g -O0 #-O2
##zz exp_omega_amd64_linux_CCASFLAGS    = $(AM_CCASFLAGS_AMD64_LINUX)
##zz exp_omega_amd64_linux_DEPENDENCIES = $(COREGRIND_LIBS_AMD64_LINUX)
##zz exp_omega_amd64_linux_LDADD        = $(TOOL_LDADD_AMD64_LINUX)
##zz exp_omega_amd64_linux_LDFLAGS      = $(TOOL_LDFLAGS_AMD64_LINUX)
##zz 
##zz exp_omega_ppc32_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
##zz exp_omega_ppc32_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
##zz exp_omega_ppc32_linux_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX) -O2
##zz exp_omega_ppc32_linux_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_LINUX)
##zz exp_omega_ppc32_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_LINUX)
##zz exp_omega_ppc32_linux_LDADD        = $(TOOL_LDADD_PPC32_LINUX)
##zz exp_omega_ppc32_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_LINUX)
##zz 
##zz exp_omega_ppc64_linux_SOURCES      = $(OMEGA_SOURCES_COMMON)
##zz exp_omega_ppc64_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
##zz exp_omega_ppc64_linux_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX) -O2
##zz exp_omega_ppc64_linux_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_LINUX)
##zz exp_omega_ppc64_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_LINUX)
##zz exp_omega_ppc64_linux_LDADD        = $(TOOL_LDADD_PPC64_LINUX)
##zz exp_omega_ppc64_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_LINUX)
##zz 
##zz exp_omega_ppc32_aix5_SOURCES      = $(OMEGA_SOURCES_COMMON)
##zz exp_omega_ppc32_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
##zz exp_omega_ppc32_aix5_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5) -O2
##zz exp_omega_ppc32_aix5_CCASFLAGS    = $(AM_CCASFLAGS_PPC32_AIX5)
##zz exp_omega_ppc32_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_AIX5)
##zz exp_omega_ppc32_aix5_LDADD        = $(TOOL_LDADD_PPC32_AIX5)
##zz exp_omega_ppc32_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_AIX5)
##zz 
##zz exp_omega_ppc64_aix5_SOURCES      = $(OMEGA_SOURCES_COMMON)
##zz exp_omega_ppc64_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
##zz exp_omega_ppc64_aix5_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5) -O2
##zz exp_omega_ppc64_aix5_CCASFLAGS    = $(AM_CCASFLAGS_PPC64_AIX5)
##zz exp_omega_ppc64_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_AIX5)
##zz exp_omega_ppc64_aix5_LDADD        = $(TOOL_LDADD_PPC64_AIX5)
##zz exp_omega_ppc64_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_AIX5)
##zz 
##zz oincludedir = $(includedir)/valgrind
##zz 
##zz oinclude_HEADERS = exp-omega.h
##zz 
##zz noinst_HEADERS =
##zz 
##zz o_replace_memops.o: CFLAGS += -fno-omit-frame-pointer
##zz 
##zz o_main.o: CFLAGS += -fno-omit-frame-pointer #-fomit-frame-pointer
##zz 
