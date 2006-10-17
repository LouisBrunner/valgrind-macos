include $(top_srcdir)/Makefile.tool.am

bin_SCRIPTS = callgrind_annotate callgrind_control

noinst_HEADERS = global.h costs.h events.h

noinst_PROGRAMS = 
if VGP_X86_LINUX
noinst_PROGRAMS += callgrind-x86-linux
endif
if VGP_AMD64_LINUX
noinst_PROGRAMS += callgrind-amd64-linux
endif
if VGP_PPC32_LINUX
noinst_PROGRAMS += callgrind-ppc32-linux
endif
if VGP_PPC64_LINUX
noinst_PROGRAMS += callgrind-ppc64-linux
endif
if VGP_PPC32_AIX5
noinst_PROGRAMS += callgrind-ppc32-aix5
endif
if VGP_PPC64_AIX5
noinst_PROGRAMS += callgrind-ppc64-aix5
endif

CALLGRIND_SOURCES_COMMON = main.c events.c bb.c clo.c \
                           costs.c bbcc.c command.c debug.c fn.c \
                           sim.c callstack.c context.c dump.c jumps.c \
                           threads.c

CALLGRIND_SOURCES_X86 = ../cachegrind/cg-x86.c
CALLGRIND_SOURCES_AMD64 = ../cachegrind/cg-amd64.c
CALLGRIND_SOURCES_PPC32 = ../cachegrind/cg-ppc32.c
CALLGRIND_SOURCES_PPC64 = ../cachegrind/cg-ppc64.c

CALLGRIND_CFLAGS_COMMON = -I../cachegrind

clincludedir = $(includedir)/valgrind
clinclude_HEADERS = \
	callgrind.h

callgrind_x86_linux_SOURCES      = $(CALLGRIND_SOURCES_COMMON) $(CALLGRIND_SOURCES_X86)
callgrind_x86_linux_CPPFLAGS     = $(AM_CPPFLAGS_X86_LINUX)
callgrind_x86_linux_CFLAGS       = $(CALLGRIND_CFLAGS_COMMON) $(AM_CFLAGS_X86_LINUX)
callgrind_x86_linux_DEPENDENCIES = $(COREGRIND_LIBS_X86_LINUX)
callgrind_x86_linux_LDADD        = $(TOOL_LDADD_X86_LINUX)
callgrind_x86_linux_LDFLAGS      = $(TOOL_LDFLAGS_X86_LINUX)

callgrind_amd64_linux_SOURCES      = $(CALLGRIND_SOURCES_COMMON) $(CALLGRIND_SOURCES_AMD64)
callgrind_amd64_linux_CPPFLAGS     = $(AM_CPPFLAGS_AMD64_LINUX)
callgrind_amd64_linux_CFLAGS       = $(CALLGRIND_CFLAGS_COMMON) $(AM_CFLAGS_AMD64_LINUX)
callgrind_amd64_linux_DEPENDENCIES = $(COREGRIND_LIBS_AMD64_LINUX)
callgrind_amd64_linux_LDADD        = $(TOOL_LDADD_AMD64_LINUX)
callgrind_amd64_linux_LDFLAGS      = $(TOOL_LDFLAGS_AMD64_LINUX)

callgrind_ppc32_linux_SOURCES      = $(CALLGRIND_SOURCES_COMMON) $(CALLGRIND_SOURCES_PPC32)
callgrind_ppc32_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_LINUX)
callgrind_ppc32_linux_CFLAGS       = $(CALLGRIND_CFLAGS_COMMON) $(AM_CFLAGS_PPC32_LINUX)
callgrind_ppc32_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_LINUX)
callgrind_ppc32_linux_LDADD        = $(TOOL_LDADD_PPC32_LINUX)
callgrind_ppc32_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_LINUX)

callgrind_ppc64_linux_SOURCES      = $(CALLGRIND_SOURCES_COMMON) $(CALLGRIND_SOURCES_PPC64)
callgrind_ppc64_linux_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_LINUX)
callgrind_ppc64_linux_CFLAGS       = $(CALLGRIND_CFLAGS_COMMON) $(AM_CFLAGS_PPC64_LINUX)
callgrind_ppc64_linux_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_LINUX)
callgrind_ppc64_linux_LDADD        = $(TOOL_LDADD_PPC64_LINUX)
callgrind_ppc64_linux_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_LINUX)

callgrind_ppc32_aix5_SOURCES      = $(CALLGRIND_SOURCES_COMMON) $(CALLGRIND_SOURCES_PPC32)
callgrind_ppc32_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC32_AIX5)
callgrind_ppc32_aix5_CFLAGS       = $(CALLGRIND_CFLAGS_COMMON) $(AM_CFLAGS_PPC32_AIX5)
callgrind_ppc32_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC32_AIX5)
callgrind_ppc32_aix5_LDADD        = $(TOOL_LDADD_PPC32_AIX5)
callgrind_ppc32_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC32_AIX5)

callgrind_ppc64_aix5_SOURCES      = $(CALLGRIND_SOURCES_COMMON) $(CALLGRIND_SOURCES_PPC64)
callgrind_ppc64_aix5_CPPFLAGS     = $(AM_CPPFLAGS_PPC64_AIX5)
callgrind_ppc64_aix5_CFLAGS       = $(CALLGRIND_CFLAGS_COMMON) $(AM_CFLAGS_PPC64_AIX5)
callgrind_ppc64_aix5_DEPENDENCIES = $(COREGRIND_LIBS_PPC64_AIX5)
callgrind_ppc64_aix5_LDADD        = $(TOOL_LDADD_PPC64_AIX5)
callgrind_ppc64_aix5_LDFLAGS      = $(TOOL_LDFLAGS_PPC64_AIX5)
