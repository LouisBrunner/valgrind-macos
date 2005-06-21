include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

MODULES = \
	m_aspacemgr \
	m_debuginfo \
	m_demangle \
	m_dispatch \
	m_replacemalloc \
	m_scheduler \
	m_sigframe \
	m_syswrap

## When building, we are only interested in the current arch/OS/platform.
## But when doing 'make dist', we are interested in every arch/OS/platform.
## That's what DIST_SUBDIRS specifies.
SUBDIRS = $(MODULES) .

DIST_SUBDIRS = $(MODULES) .

AM_CPPFLAGS += -DVG_LIBDIR="\"$(valdir)"\" \
		-DKICKSTART_BASE=@KICKSTART_BASE@

default.supp: $(SUPP_FILES)

bin_PROGRAMS = \
	valgrind

val_PROGRAMS = \
	stage2 \
	vg_preload_core.so

noinst_HEADERS = \
	coregrind.h		\
	pub_core_aspacemgr.h	\
	pub_core_basics.h	\
	pub_core_cpuid.h	\
	pub_core_debuginfo.h	\
	pub_core_debugger.h	\
	pub_core_debuglog.h	\
	pub_core_demangle.h	\
	pub_core_dispatch.h	\
	pub_core_dispatch_asm.h	\
	pub_core_errormgr.h	\
	pub_core_execontext.h	\
	pub_core_hashtable.h	\
	pub_core_libcbase.h	\
	pub_core_libcassert.h	\
	pub_core_libcfile.h	\
	pub_core_libcmman.h	\
	pub_core_libcprint.h	\
	pub_core_libcproc.h	\
	pub_core_libcsignal.h	\
	pub_core_machine.h	\
	pub_core_main.h		\
	pub_core_mallocfree.h	\
	pub_core_options.h	\
	pub_core_profile.h	\
	pub_core_pthreadmodel.h	\
	pub_core_redir.h	\
	pub_core_replacemalloc.h\
	pub_core_scheduler.h	\
	pub_core_sigframe.h	\
	pub_core_signals.h	\
	pub_core_skiplist.h	\
	pub_core_stacktrace.h	\
	pub_core_syswrap.h	\
	pub_core_threadmodel.h	\
	pub_core_threadstate.h	\
	pub_core_tooliface.h	\
	pub_core_trampoline.h	\
	pub_core_translate.h	\
	pub_core_transtab.h	\
	pub_core_transtab_asm.h	\
	ume.h			\
	vki_unistd.h		\
	vki_unistd-amd64-linux.h\
	vki_unistd-arm-linux.h	\
	vki_unistd-ppc32-linux.h\
	vki_unistd-x86-linux.h

EXTRA_DIST = \
	valgrind.vs \
	README_MODULES.txt

BUILT_SOURCES = stage2.lds
CLEANFILES = stage2.lds
	
valgrind_SOURCES = \
	ume.c \
	\
	stage1.c \
	m_debuglog.c
valgrind_DEPENDENCIES =
valgrind_LDFLAGS=-static -g
valgrind_LDADD=

stage2_SOURCES = \
	m_cpuid.S \
	m_debugger.c \
	m_debuglog.c \
	m_errormgr.c \
	m_execontext.c \
	m_hashtable.c \
	m_libcbase.c \
	m_libcassert.c \
	m_libcfile.c \
	m_libcmman.c \
	m_libcprint.c \
	m_libcproc.c \
	m_libcsignal.c \
	m_machine.c \
	m_main.c \
	m_mallocfree.c \
	m_options.c \
	m_profile.c \
	m_pthreadmodel.c \
	m_redir.c \
	m_signals.c \
	m_skiplist.c \
	m_stacktrace.c \
	m_syscall.c \
	m_threadmodel.c \
	m_threadstate.c \
	m_tooliface.c \
	m_trampoline.S \
	m_translate.c \
	m_transtab.c \
	\
	ume.c

## Nb: libscheduler.a must precede libdispatch.a in this list.
stage2_extra= \
	m_debuginfo/libdebuginfo.a \
	m_demangle/libdemangle.a \
	m_scheduler/libscheduler.a \
	m_dispatch/libdispatch.a \
	m_aspacemgr/libaspacemgr.a \
	m_sigframe/libsigframe.a \
	m_syswrap/libsyswrap.a \
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
stage2_DEPENDENCIES = $(st2_DEPS_common)
stage2_LDFLAGS = \
	$(st2_LDFLAGS_common) \
	-Wl,--whole-archive $(stage2_extra2) -Wl,--no-whole-archive \
	-pie
else
stage2_DEPENDENCIES = $(st2_DEPS_common) stage2.lds
stage2_LDFLAGS = \
	$(st2_LDFLAGS_common) \
	-Wl,--whole-archive $(stage2_extra2) -Wl,--no-whole-archive \
	-Wl,-defsym,kickstart_base=@KICKSTART_BASE@ -Wl,-T,stage2.lds
endif

stage2_LDADD= $(stage2_extra) \
	-ldl

vg_preload_core_so_SOURCES = vg_preloaded.c
vg_preload_core_so_CFLAGS = $(AM_CFLAGS) -fpic
vg_preload_core_so_LDADD = -ldl
vg_preload_core_so_LDFLAGS = \
	-shared \
	-Wl,--soname,vg_preload_core.so \
	-Wl,-z,initfirst

# Extract ld's default linker script and hack it to our needs.
# First we cut everything above and below the "=====..." lines at the top
# and bottom.
# Then we have to replace the load address with "kickstart_base".
# The line to replace in has one of the following two forms:
#
#   . = 0x08048000 + SIZEOF_HEADERS;
#
# or
#   PROVIDE (__executable_start = 0x08048000); . = 0x08048000 + SIZEOF_HEADERS;
#
# So we search for the line with a hex value "+ SIZEOF_HEADERS", and replace
# all the hex values in that line with "kickstart_base".
stage2.lds: Makefile
	$(CC) -Wl,--verbose -nostdlib 2>&1 | sed \
		-e '1,/^=====\+$$/d' \
		-e '/^=====\+$$/d' \
		-e '/\. = 0x[0-9A-Fa-f]\+ + SIZEOF_HEADERS/s/0x[0-9A-Fa-f]\+/kickstart_base/g' > $@ \
	|| rm -f $@

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS)

all-local:
	mkdir -p $(inplacedir)
	for i in $(val_PROGRAMS); do \
		to=$(inplacedir)/$$i; \
		rm -f $$$to; \
		ln -sf ../$(subdir)/$$i $$to; \
	done

