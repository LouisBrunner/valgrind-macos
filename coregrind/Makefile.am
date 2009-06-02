
# Be very careful when renaming any files, targets, whatever, in this
# Makefile.  Various parts of the system rely on these names having
# particular forms.

include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.flags.am
include $(top_srcdir)/Makefile.core-tool.am

#----------------------------------------------------------------------------
# Basics, flags
#----------------------------------------------------------------------------

AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@ += \
 	-I$(top_srcdir)/coregrind \
	-DVG_LIBDIR="\"$(valdir)"\" \
	-DVG_PLATFORM="\"@VGCONF_ARCH_PRI@-@VGCONF_OS@\""
if VGCONF_HAVE_PLATFORM_SEC
AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@ += \
 	-I$(top_srcdir)/coregrind \
	-DVG_LIBDIR="\"$(valdir)"\" \
	-DVG_PLATFORM="\"@VGCONF_ARCH_SEC@-@VGCONF_OS@\""
endif


default.supp: $(SUPP_FILES)


pkglib_LIBRARIES =

if VGCONF_PLATFORMS_INCLUDE_X86_LINUX
pkglib_LIBRARIES += libcoregrind-x86-linux.a
endif

if VGCONF_PLATFORMS_INCLUDE_AMD64_LINUX
pkglib_LIBRARIES += libcoregrind-amd64-linux.a
endif

if VGCONF_PLATFORMS_INCLUDE_PPC32_LINUX
pkglib_LIBRARIES += libcoregrind-ppc32-linux.a
endif

if VGCONF_PLATFORMS_INCLUDE_PPC64_LINUX
pkglib_LIBRARIES += libcoregrind-ppc64-linux.a
endif

if VGCONF_PLATFORMS_INCLUDE_PPC32_AIX5
pkglib_LIBRARIES += libcoregrind-ppc32-aix5.a
endif

if VGCONF_PLATFORMS_INCLUDE_PPC64_AIX5
pkglib_LIBRARIES += libcoregrind-ppc64-aix5.a
endif

if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
pkglib_LIBRARIES += libcoregrind-x86-darwin.a
endif

if VGCONF_PLATFORMS_INCLUDE_AMD64_DARWIN
pkglib_LIBRARIES += libcoregrind-amd64-darwin.a
endif

#----------------------------------------------------------------------------
# The launcher
#----------------------------------------------------------------------------
# Build the launcher (valgrind) for the primary target only.
#
bin_PROGRAMS = \
	valgrind no_op_client_for_valgrind

if VGCONF_OS_IS_LINUX
valgrind_SOURCES = \
	launcher-linux.c \
	m_debuglog.c
endif
if VGCONF_OS_IS_AIX5
valgrind_SOURCES = \
	launcher-aix5.c \
	m_debuglog.c \
	m_vkiscnums.c
endif
if VGCONF_OS_IS_DARWIN
valgrind_SOURCES = \
	launcher-darwin.c \
	m_debuglog.c
endif

valgrind_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
valgrind_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_LDFLAGS   = $(AM_CFLAGS_PRI)

no_op_client_for_valgrind_SOURCES = no_op_client_for_valgrind.c
no_op_client_for_valgrind_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
no_op_client_for_valgrind_CFLAGS    = $(AM_CFLAGS_PRI)
no_op_client_for_valgrind_CCASFLAGS = $(AM_CCASFLAGS_PRI)
no_op_client_for_valgrind_LDFLAGS   = $(AM_CFLAGS_PRI)

#----------------------------------------------------------------------------
# Darwin Mach stuff
#----------------------------------------------------------------------------

# Mach RPC interface definitions
# Here are some more .defs files that are not used, but could be in the
# future:
#	clock.defs \
#	clock_priv.defs \
#	clock_reply.defs \
#	exc.defs \
#	host_priv.defs \
#	host_security.defs \
#	ledger.defs \
#	lock_set.defs \
#	mach_host.defs \
#	mach_port.defs \
#	notify.defs \
#	processor.defs \
#	processor_set.defs \
#
if VGCONF_OS_IS_DARWIN
mach_defs = \
	mach_vm.defs \
	task.defs \
	thread_act.defs \
	vm_map.defs
else
mach_defs =
endif

mach_srcs = $(addprefix m_mach/,$(mach_defs:.defs=User.c))
mach_server_srcs = $(addprefix m_mach/,$(mach_defs:.defs=Server.c))
mach_hdrs = $(addprefix m_mach/,$(mach_defs:.defs=.h))
mach_files = $(addprefix /usr/include/mach/,$(mach_defs))

$(mach_srcs) $(mach_hdrs): $(mach_files)
	(cd m_mach && mig $(mach_files))

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

noinst_HEADERS = \
	$(mach_hdrs) \
	launcher-aix5-bootblock.h \
	pub_core_aspacehl.h	\
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
	pub_core_initimg.h	\
	pub_core_libcbase.h	\
	pub_core_libcassert.h	\
	pub_core_libcfile.h	\
	pub_core_libcprint.h	\
	pub_core_libcproc.h	\
	pub_core_libcsignal.h	\
	pub_core_mach.h	\
	pub_core_machine.h	\
	pub_core_mallocfree.h	\
	pub_core_options.h	\
	pub_core_oset.h		\
	pub_core_redir.h	\
	pub_core_replacemalloc.h\
	pub_core_scheduler.h	\
	pub_core_seqmatch.h	\
	pub_core_sigframe.h	\
	pub_core_signals.h	\
	pub_core_sparsewa.h	\
	pub_core_stacks.h	\
	pub_core_stacktrace.h	\
	pub_core_syscall.h	\
	pub_core_syswrap.h	\
	pub_core_threadstate.h	\
	pub_core_tooliface.h	\
	pub_core_trampoline.h	\
	pub_core_translate.h	\
	pub_core_transtab.h	\
	pub_core_transtab_asm.h	\
	pub_core_ume.h		\
	pub_core_vki.h		\
	pub_core_vkiscnums.h	\
	pub_core_wordfm.h	\
	pub_core_xarray.h	\
	m_aspacemgr/priv_aspacemgr.h \
	m_coredump/priv_elf.h	\
	m_debuginfo/priv_misc.h	\
	m_debuginfo/priv_storage.h	\
	m_debuginfo/priv_tytypes.h      \
	m_debuginfo/priv_readstabs.h	\
	m_debuginfo/priv_readpdb.h	\
	m_debuginfo/priv_d3basics.h	\
	m_debuginfo/priv_readdwarf.h	\
	m_debuginfo/priv_readdwarf3.h	\
	m_debuginfo/priv_readelf.h	\
	m_debuginfo/priv_readmacho.h	\
	m_debuginfo/priv_readxcoff.h	\
	m_demangle/ansidecl.h	\
	m_demangle/cp-demangle.h \
	m_demangle/dyn-string.h	\
	m_demangle/demangle.h	\
	m_demangle/safe-ctype.h \
	m_demangle/vg_libciface.h \
	m_initimg/priv_initimg_pathscan.h \
	m_initimg/simple_huffman.c \
	m_scheduler/priv_sema.h \
	m_syswrap/priv_types_n_macros.h \
	m_syswrap/priv_syswrap-generic.h \
	m_syswrap/priv_syswrap-linux.h \
	m_syswrap/priv_syswrap-linux-variants.h \
	m_syswrap/priv_syswrap-aix5.h \
	m_syswrap/priv_syswrap-darwin.h \
	m_syswrap/priv_syswrap-main.h \
	m_ume/priv_ume.h

#----------------------------------------------------------------------------
# libcoregrind_<platform>.so
#----------------------------------------------------------------------------

BUILT_SOURCES = 
CLEANFILES = 
if VGCONF_OS_IS_DARWIN
BUILT_SOURCES += $(COREGRIND_DARWIN_BUILT_SOURCES)
CLEANFILES    += $(COREGRIND_DARWIN_BUILT_SOURCES)
endif

COREGRIND_SOURCES_COMMON = \
	m_commandline.c \
	m_clientstate.c \
	m_cpuid.S \
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
	m_redir.c \
	m_seqmatch.c \
	m_signals.c \
	m_sparsewa.c \
	m_stacks.c \
	m_stacktrace.c \
	m_syscall.c \
	m_threadstate.c \
	m_tooliface.c \
	m_trampoline.S \
	m_translate.c \
	m_transtab.c \
	m_vki.c \
	m_vkiscnums.c \
	m_wordfm.c \
	m_xarray.c \
	m_aspacehl.c \
	m_aspacemgr/aspacemgr-common.c \
	m_debuginfo/misc.c \
	m_debuginfo/d3basics.c \
	m_debuginfo/storage.c \
	m_debuginfo/debuginfo.c \
	m_debuginfo/tytypes.c \
	m_demangle/cp-demangle.c \
	m_demangle/cplus-dem.c \
	m_demangle/demangle.c \
	m_demangle/dyn-string.c \
	m_demangle/safe-ctype.c \
	m_replacemalloc/replacemalloc_core.c \
	m_scheduler/scheduler.c \
	m_scheduler/sema.c \
	m_syswrap/syswrap-main.c \
	m_ume/elf.c \
	m_ume/macho.c \
	m_ume/main.c \
	m_ume/script.c

COREGRIND_LINUX_SOURCE = \
	m_aspacemgr/aspacemgr-linux.c \
	m_coredump/coredump-elf.c \
	m_debuginfo/readelf.c \
	m_debuginfo/readdwarf.c \
	m_debuginfo/readdwarf3.c \
	m_debuginfo/readstabs.c \
	m_debuginfo/readpdb.c \
	m_initimg/initimg-linux.c \
	m_initimg/initimg-pathscan.c \
	m_syswrap/syswrap-linux.c \
	m_syswrap/syswrap-linux-variants.c \
	m_syswrap/syswrap-generic.c

COREGRIND_AIX5_SOURCE = \
	m_aspacemgr/aspacemgr-aix5.c \
	m_debuginfo/readxcoff.c \
	m_initimg/initimg-aix5.c \
	m_syswrap/syswrap-aix5.c

    # Note that the *User.c files are generated using 'mig' from $mach_defs
    # above.
COREGRIND_DARWIN_SOURCE = \
	m_aspacemgr/aspacemgr-linux.c \
	m_debuginfo/readdwarf.c \
	m_debuginfo/readdwarf3.c \
	m_debuginfo/readstabs.c \
	m_debuginfo/readmacho.c \
	m_debuginfo/readpdb.c \
	m_mach/mach_basics.c \
	m_mach/mach_msg.c \
	m_initimg/initimg-darwin.c \
	m_initimg/initimg-pathscan.c \
	m_syswrap/syswrap-darwin.c \
	m_syswrap/syswrap-generic.c

COREGRIND_DARWIN_BUILT_SOURCES = \
	m_mach/mach_vmUser.c \
	m_mach/taskUser.c \
	m_mach/thread_actUser.c \
	m_mach/vm_mapUser.c

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

libcoregrind_ppc32_aix5_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_AIX5_SOURCE) \
	m_coredump/coredump-ppc32-aix5.c \
	m_dispatch/dispatch-ppc32-aix5.S \
	m_sigframe/sigframe-ppc32-aix5.c \
	m_syswrap/syscall-ppc32-aix5.S \
	m_syswrap/syswrap-ppc32-aix5.c
libcoregrind_ppc32_aix5_a_CPPFLAGS = $(AM_CPPFLAGS_PPC32_AIX5)
libcoregrind_ppc32_aix5_a_CFLAGS = $(AM_CFLAGS_PPC32_AIX5)
libcoregrind_ppc32_aix5_a_CCASFLAGS = $(AM_CCASFLAGS_PPC32_AIX5)
libcoregrind_ppc32_aix5_a_AR = $(AR) -X32 cru

libcoregrind_ppc64_aix5_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_AIX5_SOURCE) \
	m_coredump/coredump-ppc64-aix5.c \
	m_dispatch/dispatch-ppc64-aix5.S \
	m_sigframe/sigframe-ppc64-aix5.c \
	m_syswrap/syscall-ppc64-aix5.S \
	m_syswrap/syswrap-ppc64-aix5.c
libcoregrind_ppc64_aix5_a_CPPFLAGS = $(AM_CPPFLAGS_PPC64_AIX5)
libcoregrind_ppc64_aix5_a_CFLAGS = $(AM_CFLAGS_PPC64_AIX5)
libcoregrind_ppc64_aix5_a_CCASFLAGS = $(AM_CCASFLAGS_PPC64_AIX5)
libcoregrind_ppc64_aix5_a_AR = $(AR) -X64 cru

libcoregrind_x86_darwin_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_DARWIN_SOURCE) \
	m_coredump/coredump-x86-darwin.c \
	m_dispatch/dispatch-x86-darwin.S \
	m_mach/mach_traps-x86-darwin.S \
	m_sigframe/sigframe-x86-darwin.c \
	m_start-x86-darwin.S \
	m_syswrap/syscall-x86-darwin.S \
	m_syswrap/syswrap-x86-darwin.c
nodist_libcoregrind_x86_darwin_a_SOURCES = $(COREGRIND_DARWIN_BUILT_SOURCES)
libcoregrind_x86_darwin_a_CPPFLAGS = $(AM_CPPFLAGS_X86_DARWIN)
libcoregrind_x86_darwin_a_CFLAGS = $(AM_CFLAGS_X86_DARWIN)
libcoregrind_x86_darwin_a_CCASFLAGS = $(AM_CCASFLAGS_X86_DARWIN)


libcoregrind_amd64_darwin_a_SOURCES = \
	$(COREGRIND_SOURCES_COMMON) \
	$(COREGRIND_DARWIN_SOURCE) \
	m_coredump/coredump-amd64-darwin.c \
	m_dispatch/dispatch-amd64-darwin.S \
	m_mach/mach_traps-amd64-darwin.S \
	m_sigframe/sigframe-amd64-darwin.c \
	m_start-amd64-darwin.S \
	m_syswrap/syscall-amd64-darwin.S \
	m_syswrap/syswrap-amd64-darwin.c
nodist_libcoregrind_amd64_darwin_a_SOURCES = $(COREGRIND_DARWIN_BUILT_SOURCES)
libcoregrind_amd64_darwin_a_CPPFLAGS = $(AM_CPPFLAGS_AMD64_DARWIN)
libcoregrind_amd64_darwin_a_CFLAGS = $(AM_CFLAGS_AMD64_DARWIN)
libcoregrind_amd64_darwin_a_CCASFLAGS = $(AM_CCASFLAGS_AMD64_DARWIN)


m_dispatch/dispatch-x86-linux.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-amd64-linux.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-ppc32-linux.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-ppc64-linux.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-ppc32-aix5.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-ppc64-aix5.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-x86-darwin.S:	 libvex_guest_offsets.h
m_dispatch/dispatch-amd64-darwin.S:	 libvex_guest_offsets.h
m_syswrap/syscall-x86-linux.S:		 libvex_guest_offsets.h
m_syswrap/syscall-amd64-linux.S:	 libvex_guest_offsets.h
m_syswrap/syscall-ppc32-linux.S:	 libvex_guest_offsets.h
m_syswrap/syscall-ppc64-linux.S:	 libvex_guest_offsets.h
m_syswrap/syscall-ppc32-aix5.S:		 libvex_guest_offsets.h
m_syswrap/syscall-ppc64-aix5.S:		 libvex_guest_offsets.h
m_syswrap/syscall-x86-darwin.S:		 libvex_guest_offsets.h
m_syswrap/syscall-amd64-darwin.S:	 libvex_guest_offsets.h
m_syswrap/syswrap-main.c: 		 libvex_guest_offsets.h

libvex_guest_offsets.h:
	$(MAKE) -C @VEX_DIR@ CC="$(CC)" AR="$(AR)" pub/libvex_guest_offsets.h

#----------------------------------------------------------------------------
# libreplacemalloc_toolpreload_<platform>.so
#----------------------------------------------------------------------------

pkglib_LIBRARIES += libreplacemalloc_toolpreload-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
if VGCONF_HAVE_PLATFORM_SEC
pkglib_LIBRARIES += libreplacemalloc_toolpreload-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a
endif

libreplacemalloc_toolpreload_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_SOURCES = \
	m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
libreplacemalloc_toolpreload_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CFLAGS = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) $(AM_CFLAGS_PIC)
if VGCONF_HAVE_PLATFORM_SEC
libreplacemalloc_toolpreload_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_SOURCES = \
	m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
libreplacemalloc_toolpreload_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CFLAGS = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) $(AM_CFLAGS_PIC)
endif

# Special AR for AIX.
libreplacemalloc_toolpreload_ppc32_aix5_a_AR = $(AR) -X32 cru
libreplacemalloc_toolpreload_ppc64_aix5_a_AR = $(AR) -X64 cru

#----------------------------------------------------------------------------
# vgpreload_core_<platform>.so
#----------------------------------------------------------------------------

noinst_PROGRAMS  = vgpreload_core-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += vgpreload_core-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_SOURCES = vg_preloaded.c
vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CFLAGS = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) $(AM_CFLAGS_PIC)
vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
if VGCONF_HAVE_PLATFORM_SEC
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_SOURCES = vg_preloaded.c
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CFLAGS = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) $(AM_CFLAGS_PIC)
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif

#----------------------------------------------------------------------------
# General stuff
#----------------------------------------------------------------------------

all-local: inplace-noinst_PROGRAMS inplace-noinst_DSYMS

clean-local: clean-noinst_DSYMS
	$(MAKE) -C @VEX_DIR@ CC="$(CC)" AR="$(AR)" clean
	rm -f $(mach_srcs) $(mach_server_srcs) $(mach_hdrs)

LIBVEX  = libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
if VGCONF_HAVE_PLATFORM_SEC
LIBVEX += libvex-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a
endif

# Nb: The loop installs the libvex library for possible use by standalone
# tools.
install-exec-local: install-noinst_PROGRAMS install-noinst_DSYMS
	for v in $(LIBVEX) ; do \
	  $(INSTALL_DATA) @VEX_DIR@/$$v $(DESTDIR)$(valdir) ; \
	done


MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS)

EXTRA_DIST = \
	m_debuginfo/UNUSED_STABS.txt \
	m_debuginfo/README.txt
