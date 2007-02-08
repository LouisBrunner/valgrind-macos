include $(top_srcdir)/Makefile.tool.am

bin_SCRIPTS = cg_annotate

noinst_HEADERS = cg_arch.h cg_sim.c

noinst_PROGRAMS = 
if VGP_X86_LINUX
noinst_PROGRAMS += cachegrind-x86-linux
endif
if VGP_AMD64_LINUX
noinst_PROGRAMS += cachegrind-amd64-linux
endif
if VGP_PPC32_LINUX
noinst_PROGRAMS += cachegrind-ppc32-linux
endif
if VGP_PPC64_LINUX
noinst_PROGRAMS += cachegrind-ppc64-linux
endif
if VGP_PPC32_AIX5
noinst_PROGRAMS += cachegrind-ppc32-aix5
endif
if VGP_PPC64_AIX5
noinst_PROGRAMS += cachegrind-ppc64-aix5
endif

# Build cg_merge for the primary target only.
bin_PROGRAMS = cg_merge
cg_merge_SOURCES = cg_merge.c
cg_merge_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
cg_merge_CFLAGS    = $(AM_CFLAGS_PRI)
cg_merge_CCASFLAGS = $(AM_CCASFLAGS_PRI)
cg_merge_LDFLAGS   = $(AM_CFLAGS_PRI)


CACHEGRIND_SOURCES_COMMON = cg_main.c
CACHEGRIND_SOURCES_X86 = cg-x86.c
CACHEGRIND_SOURCES_AMD64 = cg-amd64.c
CACHEGRIND_SOURCES_PPC32 = cg-ppc32.c
CACHEGRIND_SOURCES_PPC64 = cg-ppc64.c

cachegrind_x86_linux_SOURCES      = $(CACHEGRIND_SOURCES_COMMON) $(CACHEGRIND_SOURCES_X86)
cachegrind_x86_linux_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
cachegrind_x86_linux_CFLAGS       = $(AM_CFLAGS_X86_LINUX)
cachegrind_x86_linux_DEPENDENCIES = $(COREGRIND_LIBS_X86_LINUX)
cachegrind_x86_linux_LDADD        = $(TOOL_LDADD_X86_LINUX)
cachegrind_x86_linux_LDFLAGS      = $(TOOL_LDFLAGS_X86_LINUX)

cachegrind_amd64_linux_SOURCES      = $(CACHEGRIND_SOURCES_COMMON) $(CACHEGRIND_SOURCES_AMD64)
cachegrind_amd64_linux_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
cachegrind_amd64_linux_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX)
cachegrind_amd64_linux_DEPENDENCIES = $(COREGRIND_LIBS_AMD64_LINUX)
cachegrind_amd64_linux_LDADD        = $(TOOL_LDADD_AMD64_LINUX)
cachegrind_amd64_linux_LDFLAGS      = $(TOOL_LDFLAGS_AMD64_LINUX)

cachegrind_ppc32_linux_SOURCES      = $(CACHEGRIND_SOURCES_COMMON) $(CACHEGRIND_SOURCES_PPC32)
cachegrind_ppc32_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
cachegrind_ppc32_linux_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX)
cachegrind_ppc32_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_LINUX)
cachegrind_ppc32_linux_LDADD        = $(TOOL_LDADD_PPC32_LINUX)
cachegrind_ppc32_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_LINUX)

cachegrind_ppc64_linux_SOURCES      = $(CACHEGRIND_SOURCES_COMMON) $(CACHEGRIND_SOURCES_PPC64)
cachegrind_ppc64_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
cachegrind_ppc64_linux_CFLAGS       = $(AM_CFLAGS_PPC64_LINUX)
cachegrind_ppc64_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_LINUX)
cachegrind_ppc64_linux_LDADD        = $(TOOL_LDADD_PPC64_LINUX)
cachegrind_ppc64_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_LINUX)

cachegrind_ppc32_aix5_SOURCES      = $(CACHEGRIND_SOURCES_COMMON) $(CACHEGRIND_SOURCES_PPC32)
cachegrind_ppc32_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
cachegrind_ppc32_aix5_CFLAGS       = $(AM_CFLAGS_PPC32_AIX5)
cachegrind_ppc32_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_AIX5)
cachegrind_ppc32_aix5_LDADD        = $(TOOL_LDADD_PPC32_AIX5)
cachegrind_ppc32_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_AIX5)

cachegrind_ppc64_aix5_SOURCES      = $(CACHEGRIND_SOURCES_COMMON) $(CACHEGRIND_SOURCES_PPC64)
cachegrind_ppc64_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
cachegrind_ppc64_aix5_CFLAGS       = $(AM_CFLAGS_PPC64_AIX5)
cachegrind_ppc64_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_AIX5)
cachegrind_ppc64_aix5_LDADD        = $(TOOL_LDADD_PPC64_AIX5)
cachegrind_ppc64_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_AIX5)
