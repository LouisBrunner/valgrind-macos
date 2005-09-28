include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core.am

AM_CPPFLAGS += -DVG_LIBDIR="\"$(valdir)"\" \
		-DKICKSTART_BASE=@KICKSTART_BASE@

default.supp: $(SUPP_FILES)

noinst_LIBRARIES = \
    libcoregrind.a \
    libreplacemalloc_toolpreload.a

bin_PROGRAMS = \
	valgrind

val_PROGRAMS = \
	vg_preload_core.so

# Remember to include all the arch-specific files in the distribution.
EXTRA_DIST = \
        $(addsuffix .S,$(addprefix m_dispatch/dispatch-,$(VG_ARCH_ALL))) \
        $(addsuffix .c,$(addprefix m_sigframe/sigframe-,$(VG_PLATFORM_ALL))) \
        $(addsuffix .S,$(addprefix m_syswrap/syscall-,$(VG_PLATFORM_ALL))) \
        $(addsuffix .c,$(addprefix m_syswrap/syswrap-,$(VG_OS_ALL))) \
        $(addsuffix .c,$(addprefix m_syswrap/syswrap-,$(VG_PLATFORM_ALL)))

noinst_HEADERS = \
	coregrind.h		\
	pub_core_aspacemgr.h	\
	pub_core_basics.h	\
	pub_core_basics_asm.h	\
	pub_core_clientstate.h	\
	pub_core_commandline.h	\
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
	pub_core_libcprint.h	\
	pub_core_libcproc.h	\
	pub_core_libcsignal.h	\
	pub_core_machine.h	\
	pub_core_main.h		\
	pub_core_mallocfree.h	\
	pub_core_options.h	\
	pub_core_oset.h		\
	pub_core_profile.h	\
	pub_core_pthreadmodel.h	\
	pub_core_redir.h	\
	pub_core_replacemalloc.h\
	pub_core_scheduler.h	\
	pub_core_sigframe.h	\
	pub_core_signals.h	\
	pub_core_skiplist.h	\
	pub_core_stacks.h	\
	pub_core_stacktrace.h	\
	pub_core_syscall.h	\
	pub_core_syswrap.h	\
	pub_core_threadmodel.h	\
	pub_core_threadstate.h	\
	pub_core_tooliface.h	\
	pub_core_trampoline.h	\
	pub_core_translate.h	\
	pub_core_transtab.h	\
	pub_core_transtab_asm.h	\
	pub_core_ume.h		\
	vki_unistd.h		\
	vki_unistd-amd64-linux.h\
	vki_unistd-ppc32-linux.h\
	vki_unistd-x86-linux.h	\
	m_debuginfo/priv_symtab.h	\
	m_debuginfo/priv_symtypes.h	\
	m_demangle/ansidecl.h	\
	m_demangle/dyn-string.h	\
	m_demangle/demangle.h	\
	m_demangle/safe-ctype.h \
	m_scheduler/priv_sema.h \
	m_syswrap/priv_types_n_macros.h \
	m_syswrap/priv_syswrap-generic.h \
	m_syswrap/priv_syswrap-linux.h \
	m_syswrap/priv_syswrap-main.h

BUILT_SOURCES = 
CLEANFILES = 

valgrind_SOURCES = \
	launcher.c \
	m_debuglog.c

libcoregrind_a_SOURCES = \
	m_commandline.c \
	m_coredump.c \
	m_cpuid.S \
	m_clientstate.c \
	m_debugger.c \
	m_debuglog.c \
	m_errormgr.c \
	m_execontext.c \
	m_hashtable.c \
	m_libcbase.c \
	m_libcassert.c \
	m_libcfile.c \
	m_libcprint.c \
	m_libcproc.c \
	m_libcsignal.c \
	m_machine.c \
	m_main.c \
	m_mallocfree.c \
	m_options.c \
	m_oset.c \
	m_profile.c \
	m_pthreadmodel.c \
	m_redir.c \
	m_signals.c \
	m_skiplist.c \
	m_stacks.c \
	m_stacktrace.c \
	m_syscall.c \
	m_threadmodel.c \
	m_threadstate.c \
	m_tooliface.c \
	m_trampoline.S \
	m_translate.c \
	m_transtab.c \
	m_ume.c \
	m_aspacemgr/read_procselfmaps.c \
	m_aspacemgr/aspacemgr.c \
	m_debuginfo/dwarf.c \
	m_debuginfo/stabs.c \
	m_debuginfo/symtab.c \
	m_debuginfo/symtypes.c \
	m_demangle/cp-demangle.c \
	m_demangle/cplus-dem.c \
	m_demangle/demangle.c \
	m_demangle/dyn-string.c \
	m_demangle/safe-ctype.c \
	m_dispatch/dispatch-@VG_ARCH@.S \
	m_replacemalloc/replacemalloc_core.c \
	m_scheduler/scheduler.c \
	m_scheduler/sema.c \
	m_sigframe/sigframe-@VG_PLATFORM@.c \
	m_syswrap/syscall-@VG_PLATFORM@.S \
	m_syswrap/syswrap-generic.c \
	m_syswrap/syswrap-@VG_OS@.c \
	m_syswrap/syswrap-@VG_PLATFORM@.c \
	m_syswrap/syswrap-main.c

libreplacemalloc_toolpreload_a_SOURCES = m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_a_CFLAGS = $(PIC_AM_CFLAGS)

m_dispatch/dispatch-@VG_ARCH@.S:   libvex_guest_offsets.h
m_syswrap/syscall-@VG_PLATFORM@.S: libvex_guest_offsets.h
m_syswrap/syswrap-main.c: 	   libvex_guest_offsets.h

libvex_guest_offsets.h:
	$(MAKE) -C @VEX_DIR@ pub/libvex_guest_offsets.h

vg_preload_core_so_SOURCES = vg_preloaded.c
vg_preload_core_so_CFLAGS = $(AM_CFLAGS) -fpic
vg_preload_core_so_LDADD = -ldl
vg_preload_core_so_LDFLAGS = \
	-shared \
	-Wl,--soname,vg_preload_core.so \
	-Wl,-z,initfirst

clean-local:
	$(MAKE) -C @VEX_DIR@ CC="$(CC)" clean

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS)

all-local:
	mkdir -p $(inplacedir)
	for i in $(val_PROGRAMS); do \
		to=$(inplacedir)/$$i; \
		rm -f $$$to; \
		ln -sf ../$(subdir)/$$i $$to; \
	done

