include $(top_srcdir)/Makefile.tool.am

noinst_PROGRAMS = 
if VG_X86_LINUX
noinst_PROGRAMS += lackey-x86-linux
endif
if VG_AMD64_LINUX
noinst_PROGRAMS += lackey-amd64-linux
endif
if VG_PPC32_LINUX
noinst_PROGRAMS += lackey-ppc32-linux
endif

LACKEY_SOURCES_COMMON = lk_main.c

lackey_x86_linux_SOURCES      = $(LACKEY_SOURCES_COMMON)
lackey_x86_linux_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
lackey_x86_linux_CFLAGS       = $(AM_CFLAGS_X86_LINUX)
lackey_x86_linux_DEPENDENCIES = $(COREGRIND_LIBS_X86_LINUX)
lackey_x86_linux_LDADD        = $(TOOL_LDADD_X86_LINUX)
lackey_x86_linux_LDFLAGS      = $(TOOL_LDFLAGS_X86_LINUX)

lackey_amd64_linux_SOURCES      = $(LACKEY_SOURCES_COMMON)
lackey_amd64_linux_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
lackey_amd64_linux_CFLAGS       = $(AM_CFLAGS_AMD64_LINUX)
lackey_amd64_linux_DEPENDENCIES = $(COREGRIND_LIBS_AMD64_LINUX)
lackey_amd64_linux_LDADD        = $(TOOL_LDADD_AMD64_LINUX)
lackey_amd64_linux_LDFLAGS      = $(TOOL_LDFLAGS_AMD64_LINUX)

lackey_ppc32_linux_SOURCES      = $(LACKEY_SOURCES_COMMON)
lackey_ppc32_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
lackey_ppc32_linux_CFLAGS       = $(AM_CFLAGS_PPC32_LINUX)
lackey_ppc32_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_LINUX)
lackey_ppc32_linux_LDADD        = $(TOOL_LDADD_PPC32_LINUX)
lackey_ppc32_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_LINUX)
