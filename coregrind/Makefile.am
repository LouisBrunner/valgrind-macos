include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

## When building, we are only interested in the current arch/OS/platform.
## But when doing 'make dist', we are interested in every arch/OS/platform.
## That's what DIST_SUBDIRS specifies.
SUBDIRS      = $(VG_ARCH)     $(VG_OS)     $(VG_PLATFORM)     demangle .
DIST_SUBDIRS = $(VG_ARCH_ALL) $(VG_OS_ALL) $(VG_PLATFORM_ALL) demangle .

AM_CPPFLAGS += -DVG_LIBDIR="\"$(valdir)"\" -I$(srcdir)/demangle \
		-DKICKSTART_BASE=@KICKSTART_BASE@ \
		-DVG_PLATFORM="\"$(VG_PLATFORM)"\"
AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g @ARCH_CORE_AM_CFLAGS@
AM_CFLAGS += -fno-omit-frame-pointer

default.supp: $(SUPP_FILES)

bin_PROGRAMS = \
	valgrind

val_PROGRAMS = \
	stage2 \
	vg_inject.so

noinst_LIBRARIES = lib_replace_malloc.a

noinst_HEADERS = \
	core.h			\
	core_asm.h		\
	pub_core_execontext.h	\
	pub_core_stacktrace.h	\
	ume.h			\
	vg_symtab2.h		\
	vg_symtypes.h		\
	vg_toolint.h

EXTRA_DIST = \
	valgrind.vs \
	gen_toolint.pl toolfuncs.def \
	gen_intercepts.pl vg_replace_malloc.c.base

BUILT_SOURCES = vg_toolint.c vg_toolint.h
CLEANFILES = vg_toolint.c vg_toolint.h vg_replace_malloc.c

valgrind_SOURCES = \
	ume.c \
	\
	stage1.c \
	${VG_ARCH}/jmp_with_stack.c 
valgrind_DEPENDENCIES =
valgrind_LDFLAGS=-static -g
valgrind_LDADD=

stage2_SOURCES = \
	execontext.c \
	stacktrace.c \
	ume.c \
	\
	vg_scheduler.c \
	vg_default.c \
	vg_demangle.c \
	vg_errcontext.c \
	vg_hashtable.c \
	vg_instrument.c \
	vg_main.c \
	vg_malloc2.c \
	vg_memory.c \
	vg_messages.c \
	vg_mylibc.c \
	vg_needs.c \
	vg_procselfmaps.c \
	vg_dummy_profile.c \
	vg_signals.c \
	vg_symtab2.c \
	vg_threadmodel.c \
	vg_pthreadmodel.c \
	vg_redir.c \
	vg_dwarf.c \
	vg_stabs.c \
	vg_skiplist.c \
	vg_symtypes.c \
	vg_syscalls.c \
	vg_toolint.c \
	vg_translate.c \
	vg_transtab.c

## libplatform.a must be before libarch.a and libos.a, it seems.
stage2_extra= \
	demangle/libdemangle.a \
	${VG_PLATFORM}/libplatform.a \
	${VG_ARCH}/libarch.a \
	${VG_OS}/libos.a \
	@VEX_DIR@/libvex.a

## Test repeated in both arms of the if-then-else because older versions of
## automake don't seem to like having += within an if-then-else.
if USE_PIE
stage2_CFLAGS = $(AM_CFLAGS) -fpie
stage2_DEPENDENCIES = \
	$(srcdir)/valgrind.vs \
	$(stage2_extra)
stage2_LDFLAGS = -Wl,--export-dynamic -g \
	-Wl,-version-script $(srcdir)/valgrind.vs \
	-pie
else
stage2_CFLAGS = $(AM_CFLAGS)
stage2_DEPENDENCIES = \
	$(srcdir)/valgrind.vs ${VG_ARCH}/stage2.lds \
	$(stage2_extra)
stage2_LDFLAGS = -Wl,--export-dynamic -g \
	-Wl,-version-script $(srcdir)/valgrind.vs \
	-Wl,-defsym,kickstart_base=@KICKSTART_BASE@ -Wl,-T,${VG_ARCH}/stage2.lds
endif

stage2_LDADD= $(stage2_extra) -ldl


vg_replace_malloc.c: $(srcdir)/gen_intercepts.pl $(srcdir)/vg_replace_malloc.c.base
	rm -f $@
	$(PERL) $(srcdir)/gen_intercepts.pl < $(srcdir)/vg_replace_malloc.c.base > $@

vg_toolint.c: $(srcdir)/gen_toolint.pl $(srcdir)/toolfuncs.def ./Makefile
	rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl callwrap     < $(srcdir)/toolfuncs.def >  $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl missingfuncs < $(srcdir)/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl initfunc     < $(srcdir)/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl structdef    < $(srcdir)/toolfuncs.def >> $@ || rm -f $@

vg_toolint.h:  $(srcdir)/gen_toolint.pl $(srcdir)/toolfuncs.def ./Makefile
	rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl proto  < $(srcdir)/toolfuncs.def >  $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl struct < $(srcdir)/toolfuncs.def >> $@ || rm -f $@

vg_inject_so_SOURCES = vg_intercept.c
vg_inject_so_CFLAGS = $(AM_CFLAGS) -fpic
vg_inject_so_LDADD = -ldl
vg_inject_so_LDFLAGS = \
	-shared \
	-Wl,--soname,vg_inject.so \
	-Wl,-z,initfirst

lib_replace_malloc_a_SOURCES = vg_replace_malloc.c
lib_replace_malloc_a_CFLAGS  = $(AM_CFLAGS) -fpic -fno-omit-frame-pointer

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS)

all-local:
	mkdir -p $(inplacedir)
	for i in $(val_PROGRAMS); do \
		to=$(inplacedir)/$$i; \
		rm -f $$$to; \
		ln -sf ../$(subdir)/$$i $$to; \
	done

