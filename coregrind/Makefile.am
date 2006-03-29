
# Be very careful when renaming any files, targets, whatever, in this
# Makefile.  The install-exec-local target in ../Makefile.install.am
# does some fragile sed file-name-mangling which is liable to break if
# the names of noinst_LIBRARIES or noinst_PROGRAMS change 'shape'.

include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core.am
include $(top_srcdir)/Makefile.install.am

AM_CPPFLAGS_X86_LINUX   += -DVG_LIBDIR="\"$(valdir)"\"
AM_CPPFLAGS_AMD64_LINUX += -DVG_LIBDIR="\"$(valdir)"\"
AM_CPPFLAGS_PPC32_LINUX += -DVG_LIBDIR="\"$(valdir)"\"
AM_CPPFLAGS_PPC64_LINUX += -DVG_LIBDIR="\"$(valdir)"\"


default.supp: $(SUPP_FILES)

noinst_LIBRARIES =
if VG_X86_LINUX
noinst_LIBRARIES += libcoregrind_x86_linux.a libreplacemalloc_toolpreload_x86_linux.a
endif
if VG_AMD64_LINUX
noinst_LIBRARIES += libcoregrind_amd64_linux.a libreplacemalloc_toolpreload_amd64_linux.a
endif
if VG_PPC32_LINUX
noinst_LIBRARIES += libcoregrind_ppc32_linux.a libreplacemalloc_toolpreload_ppc32_linux.a
endif
if VG_PPC64_LINUX
noinst_LIBRARIES += libcoregrind_ppc64_linux.a libreplacemalloc_toolpreload_ppc64_linux.a
endif

#------------------------- launcher -----------------------
# Build the launcher (valgrind) for the primary target only.
#
bin_PROGRAMS = \
	valgrind

valgrind_SOURCES = \
	launcher.c \
	m_debuglog.c

valgrind_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
valgrind_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_LDFLAGS   = $(AM_CFLAGS_PRI)
#
#----------------------------------------------------------


noinst_PROGRAMS =
if VG_X86_LINUX
noinst_PROGRAMS += vgpreload_core-x86-linux.so
endif
if VG_AMD64_LINUX
noinst_PROGRAMS += vgpreload_core-amd64-linux.so
endif
if VG_PPC32_LINUX
noinst_PROGRAMS += vgpreload_core-ppc32-linux.so
endif
if VG_PPC64_LINUX
noinst_PROGRAMS += vgpreload_core-ppc64-linux.so
endif

noinst_HEADERS = \
	pub_core_aspacemgr.h	\
	pub_core_basics.h	\
	pub_core_basics_asm.h	\
	pub_core_clientstate.h	\
	pub_core_clreq.h	\
	pub_core_commandline.h	\
	pub_core_coredump.h	\
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
	pub_core_mallocfree.h	\
	pub_core_options.h	\
	pub_core_oset.h		\
	pub_core_pthreadmodel.h	\
	pub_core_redir.h	\
	pub_core_replacemalloc.h\
	pub_core_scheduler.h	\
	pub_core_sigframe.h	\
	pub_core_signals.h	\
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
	vki_unistd-ppc64-linux.h\
	vki_unistd-x86-linux.h	\
	m_coredump/priv_elf.h	\
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
	m_syswrap/priv_syswrap-linux-variants.h \
	m_syswrap/priv_syswrap-main.h

BUILT_SOURCES = 
CLEANFILES = 


COREGRIND_SOURCES_COMMON = \
	m_commandline.c \
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
	m_pthreadmodel.c \
	m_redir.c \
	m_signals.c \
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
	m_replacemalloc/replacemalloc_core.c \
	m_scheduler/scheduler.c \
	m_scheduler/sema.c \
	m_syswrap/syswrap-generic.c \
	m_syswrap/syswrap-main.c

COREGRIND_LINUX_SOURCE = \
	m_coredump/coredump-elf.c \
	m_syswrap/syswrap-linux.c \
	m_syswrap/syswrap-linux-variants.c

libcoregrind_x86_linux_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_LINUX_SOURCE) \
	m_coredump/coredump-x86-linux.c \
	m_dispatch/dispatch-x86-linux.S \
	m_sigframe/sigframe-x86-linux.c \
	m_syswrap/syscall-x86-linux.S \
	m_syswrap/syswrap-x86-linux.c

libcoregrind_x86_linux_a_CPPFLAGS = $(AM_CPPFLAGS_X86_LINUX)
libcoregrind_x86_linux_a_CFLAGS = $(AM_CFLAGS_X86_LINUX)
libcoregrind_x86_linux_a_CCASFLAGS = $(AM_CCASFLAGS_X86_LINUX)

libcoregrind_amd64_linux_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_LINUX_SOURCE) \
	m_coredump/coredump-amd64-linux.c \
	m_dispatch/dispatch-amd64-linux.S \
	m_sigframe/sigframe-amd64-linux.c \
	m_syswrap/syscall-amd64-linux.S \
	m_syswrap/syswrap-amd64-linux.c

libcoregrind_amd64_linux_a_CPPFLAGS = $(AM_CPPFLAGS_AMD64_LINUX)
libcoregrind_amd64_linux_a_CFLAGS = $(AM_CFLAGS_AMD64_LINUX)
libcoregrind_amd64_linux_a_CCASFLAGS = $(AM_CCASFLAGS_AMD64_LINUX)

libcoregrind_ppc32_linux_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_LINUX_SOURCE) \
	m_coredump/coredump-ppc32-linux.c \
	m_dispatch/dispatch-ppc32-linux.S \
	m_sigframe/sigframe-ppc32-linux.c \
	m_syswrap/syscall-ppc32-linux.S \
	m_syswrap/syswrap-ppc32-linux.c

libcoregrind_ppc32_linux_a_CPPFLAGS = $(AM_CPPFLAGS_PPC32_LINUX)
libcoregrind_ppc32_linux_a_CFLAGS = $(AM_CFLAGS_PPC32_LINUX)
libcoregrind_ppc32_linux_a_CCASFLAGS = $(AM_CCASFLAGS_PPC32_LINUX)

libcoregrind_ppc64_linux_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_LINUX_SOURCE) \
	m_coredump/coredump-ppc64-linux.c \
	m_dispatch/dispatch-ppc64-linux.S \
	m_sigframe/sigframe-ppc64-linux.c \
	m_syswrap/syscall-ppc64-linux.S \
	m_syswrap/syswrap-ppc64-linux.c

libcoregrind_ppc64_linux_a_CPPFLAGS = $(AM_CPPFLAGS_PPC64_LINUX)
libcoregrind_ppc64_linux_a_CFLAGS = $(AM_CFLAGS_PPC64_LINUX)
libcoregrind_ppc64_linux_a_CCASFLAGS = $(AM_CCASFLAGS_PPC64_LINUX)

libreplacemalloc_toolpreload_x86_linux_a_SOURCES = m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_x86_linux_a_CPPFLAGS = $(AM_CPPFLAGS_X86_LINUX)
libreplacemalloc_toolpreload_x86_linux_a_CFLAGS = $(AM_CFLAGS_X86_LINUX) -fpic -fno-omit-frame-pointer

libreplacemalloc_toolpreload_amd64_linux_a_SOURCES = m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_amd64_linux_a_CPPFLAGS = $(AM_CPPFLAGS_AMD64_LINUX)
libreplacemalloc_toolpreload_amd64_linux_a_CFLAGS = $(AM_CFLAGS_AMD64_LINUX) -fpic -fno-omit-frame-pointer

libreplacemalloc_toolpreload_ppc32_linux_a_SOURCES = m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_ppc32_linux_a_CPPFLAGS = $(AM_CPPFLAGS_PPC32_LINUX)
libreplacemalloc_toolpreload_ppc32_linux_a_CFLAGS = $(AM_CFLAGS_PPC32_LINUX) -fpic -fno-omit-frame-pointer

libreplacemalloc_toolpreload_ppc64_linux_a_SOURCES = m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_ppc64_linux_a_CPPFLAGS = $(AM_CPPFLAGS_PPC64_LINUX)
libreplacemalloc_toolpreload_ppc64_linux_a_CFLAGS = $(AM_CFLAGS_PPC64_LINUX) -fpic -fno-omit-frame-pointer

m_dispatch/dispatch-x86-linux.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-amd64-linux.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-ppc32-linux.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-ppc64-linux.S:	 libvex_guest_offsets.h
m_syswrap/syscall-x86-linux.S:		 libvex_guest_offsets.h
m_syswrap/syscall-amd64-linux.S:	 libvex_guest_offsets.h
m_syswrap/syscall-ppc32-linux.S:	 libvex_guest_offsets.h
m_syswrap/syscall-ppc64-linux.S:	 libvex_guest_offsets.h
m_syswrap/syswrap-main.c: 		 libvex_guest_offsets.h

libvex_guest_offsets.h:
	$(MAKE) -C @VEX_DIR@ pub/libvex_guest_offsets.h

VGPRELOAD_CORE_SOURCES_COMMON = vg_preloaded.c

vgpreload_core_x86_linux_so_SOURCES = $(VGPRELOAD_CORE_SOURCES_COMMON)
vgpreload_core_x86_linux_so_CPPFLAGS = $(AM_CPPFLAGS_X86_LINUX)
vgpreload_core_x86_linux_so_CFLAGS = $(AM_CFLAGS_X86_LINUX) $(AM_CFLAGS_PIC)
vgpreload_core_x86_linux_so_LDFLAGS = $(PRELOAD_LDFLAGS_X86_LINUX)

vgpreload_core_amd64_linux_so_SOURCES = $(VGPRELOAD_CORE_SOURCES_COMMON)
vgpreload_core_amd64_linux_so_CPPFLAGS = $(AM_CPPFLAGS_AMD64_LINUX)
vgpreload_core_amd64_linux_so_CFLAGS = $(AM_CFLAGS_AMD64_LINUX) $(AM_CFLAGS_PIC)
vgpreload_core_amd64_linux_so_LDFLAGS = $(PRELOAD_LDFLAGS_AMD64_LINUX)

vgpreload_core_ppc32_linux_so_SOURCES = $(VGPRELOAD_CORE_SOURCES_COMMON)
vgpreload_core_ppc32_linux_so_CPPFLAGS = $(AM_CPPFLAGS_PPC32_LINUX)
vgpreload_core_ppc32_linux_so_CFLAGS = $(AM_CFLAGS_PPC32_LINUX) $(AM_CFLAGS_PIC)
vgpreload_core_ppc32_linux_so_LDFLAGS = $(PRELOAD_LDFLAGS_PPC32_LINUX)

vgpreload_core_ppc64_linux_so_SOURCES = $(VGPRELOAD_CORE_SOURCES_COMMON)
vgpreload_core_ppc64_linux_so_CPPFLAGS = $(AM_CPPFLAGS_PPC64_LINUX)
vgpreload_core_ppc64_linux_so_CFLAGS = $(AM_CFLAGS_PPC64_LINUX) $(AM_CFLAGS_PIC)
vgpreload_core_ppc64_linux_so_LDFLAGS = $(PRELOAD_LDFLAGS_PPC64_LINUX)

clean-local:
	$(MAKE) -C @VEX_DIR@ CC="$(CC)" clean

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS)

all-local:
	for f in $(noinst_PROGRAMS); do \
	  p=`echo $$f | sed -e 's/^[^-]*-//' -e 's/\..*$$//'`; \
	  n=`echo $$f | sed -e 's/-[^-]\{1,\}-[^-.]\{1,\}//'`; \
	  mkdir -p $(inplacedir)/$$p; \
	  rm -f $(inplacedir)/$$p/$$n; \
	  ln -f -s ../../$(subdir)/$$f $(inplacedir)/$$p/$$n; \
	done
