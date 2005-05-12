include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

MODULES = \
	m_aspacemgr \
	m_demangle \
	m_dispatch \
	m_replacemalloc \
	m_sigframe \
	m_syscalls

## When building, we are only interested in the current arch/OS/platform.
## But when doing 'make dist', we are interested in every arch/OS/platform.
## That's what DIST_SUBDIRS specifies.
SUBDIRS = \
	$(VG_ARCH)     $(VG_OS)     $(VG_PLATFORM) \
	$(MODULES) .

DIST_SUBDIRS = \
	$(VG_ARCH_ALL) $(VG_OS_ALL) $(VG_PLATFORM_ALL) \
	$(MODULES) .

AM_CPPFLAGS += -DVG_LIBDIR="\"$(valdir)"\" \
		-DKICKSTART_BASE=@KICKSTART_BASE@

AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g @ARCH_CORE_AM_CFLAGS@
AM_CFLAGS += -fno-omit-frame-pointer

default.supp: $(SUPP_FILES)

bin_PROGRAMS = \
	valgrind

val_PROGRAMS = \
	stage2 \
	vg_inject.so

noinst_HEADERS = \
	core.h			\
	core_asm.h		\
	pub_core_aspacemgr.h	\
	pub_core_debuglog.h	\
	pub_core_demangle.h	\
	pub_core_dispatch.h	\
	pub_core_dispatch_asm.h	\
	pub_core_errormgr.h	\
	pub_core_execontext.h	\
	pub_core_mallocfree.h	\
	pub_core_replacemalloc.h\
	pub_core_sigframe.h	\
	pub_core_skiplist.h	\
	pub_core_stacktrace.h	\
	pub_core_syscalls.h	\
	pub_core_tooliface.h	\
	ume.h			\
	vg_symtab2.h		\
	vg_symtypes.h

EXTRA_DIST = \
	valgrind.vs \
	README_MODULES.txt

valgrind_SOURCES = \
	ume.c \
	\
	stage1.c \
	m_debuglog.c \
	${VG_ARCH}/jmp_with_stack.c 
valgrind_DEPENDENCIES =
valgrind_LDFLAGS=-static -g
valgrind_LDADD=

stage2_SOURCES = \
	m_debuglog.c \
	m_errormgr.c \
	m_execontext.c \
	m_mallocfree.c \
	m_skiplist.c \
	m_stacktrace.c \
	m_tooliface.c \
	ume.c \
	\
	vg_scheduler.c \
	vg_hashtable.c \
	vg_main.c \
	vg_messages.c \
	vg_mylibc.c \
	vg_dummy_profile.c \
	vg_signals.c \
	vg_symtab2.c \
	vg_threadmodel.c \
	vg_pthreadmodel.c \
	vg_redir.c \
	vg_dwarf.c \
	vg_stabs.c \
	vg_symtypes.c \
	vg_translate.c \
	vg_transtab.c

## libplatform.a must be before libarch.a and libos.a, it seems.
stage2_extra= \
	m_dispatch/libdispatch.a \
	m_demangle/libdemangle.a \
	m_aspacemgr/libaspacemgr.a \
	m_sigframe/libsigframe.a \
	m_syscalls/libsyscalls.a \
	${VG_PLATFORM}/libplatform.a \
	${VG_ARCH}/libarch.a \
	${VG_OS}/libos.a \
	@VEX_DIR@/libvex.a

## These ones must be linked in with the --whole-archive flag, because they
## wouldn't get pulled into stage otherwise (because they contain symbols
## only referred to by tool shared objects).
stage2_extra2 = \
	m_replacemalloc/libreplacemalloc_core.a
	
## Nb: older versions of automake don't seem to like having += within an
## if-then-else, so we have to use these variables for the common parts.
st2_DEPS_common = \
	$(srcdir)/valgrind.vs \
	$(stage2_extra) \
	$(stage2_extra2)

st2_LDFLAGS_common = \
	-Wl,--export-dynamic -g \
	-Wl,-version-script $(srcdir)/valgrind.vs

if USE_PIE
stage2_CFLAGS = $(AM_CFLAGS) -fpie
stage2_DEPENDENCIES = $(st2_DEPS_common)
stage2_LDFLAGS = \
	$(st2_LDFLAGS_common) \
	-Wl,--whole-archive $(stage2_extra2) -Wl,--no-whole-archive \
	-pie
else
stage2_CFLAGS = $(AM_CFLAGS)
stage2_DEPENDENCIES = $(st2_DEPS_common) ${VG_ARCH}/stage2.lds
stage2_LDFLAGS = \
	$(st2_LDFLAGS_common) \
	-Wl,--whole-archive $(stage2_extra2) -Wl,--no-whole-archive \
	-Wl,-defsym,kickstart_base=@KICKSTART_BASE@ -Wl,-T,${VG_ARCH}/stage2.lds
endif

stage2_LDADD= $(stage2_extra) \
	-ldl

vg_inject_so_SOURCES = vg_intercept.c
vg_inject_so_CFLAGS = $(AM_CFLAGS) -fpic
vg_inject_so_LDADD = -ldl
vg_inject_so_LDFLAGS = \
	-shared \
	-Wl,--soname,vg_inject.so \
	-Wl,-z,initfirst

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS)

all-local:
	mkdir -p $(inplacedir)
	for i in $(val_PROGRAMS); do \
		to=$(inplacedir)/$$i; \
		rm -f $$$to; \
		ln -sf ../$(subdir)/$$i $$to; \
	done

