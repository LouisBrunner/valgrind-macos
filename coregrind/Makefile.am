include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

SUBDIRS = $(VG_ARCH) $(VG_OS) $(VG_PLATFORM) demangle . docs

AM_CPPFLAGS += -DVG_LIBDIR="\"$(valdir)"\" -I$(srcdir)/demangle \
		-DKICKSTART_BASE=@KICKSTART_BASE@ \
		-DVG_PLATFORM="\"$(VG_PLATFORM)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -g @ARCH_CORE_AM_CFLAGS@
AM_CFLAGS += -fno-omit-frame-pointer

default.supp: $(SUPP_FILES)

bin_PROGRAMS = \
	valgrind

val_PROGRAMS = \
	stage2 \
	libpthread.so \
	vg_inject.so

noinst_LIBRARIES = lib_replace_malloc.a

noinst_HEADERS = \
	core.h			\
	core_asm.h		\
	ume.h			\
	vg_symtab2.h		\
	vg_symtypes.h		\
	vg_toolint.h

EXTRA_DIST = \
	vg_libpthread.vs valgrind.vs \
	gen_toolint.pl toolfuncs.def \
	gen_intercepts.pl vg_replace_malloc.c.base vg_intercept.c.base

BUILT_SOURCES = vg_toolint.c vg_toolint.h
CLEANFILES = vg_toolint.c vg_toolint.h vg_replace_malloc.c vg_intercept.c

valgrind_SOURCES = \
	ume.c \
	\
	stage1.c \
	${VG_ARCH}/jmp_with_stack.c 
valgrind_DEPENDENCIES =
valgrind_LDFLAGS=-static -g
valgrind_LDADD=

stage2_SOURCES = \
	ume.c \
	\
	vg_scheduler.c \
	vg_default.c \
	vg_demangle.c \
	vg_errcontext.c \
	vg_execontext.c \
	vg_hashtable.c \
	vg_instrument.c \
	vg_main.c \
	vg_malloc2.c \
	vg_memory.c \
	vg_messages.c \
	vg_mylibc.c \
	vg_needs.c \
	vg_procselfmaps.c \
	vg_proxylwp.c \
	vg_dummy_profile.c \
	vg_signals.c \
	vg_symtab2.c \
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


vg_intercept.c: $(srcdir)/gen_intercepts.pl $(srcdir)/vg_intercept.c.base
	rm -f $@
	$(PERL) $(srcdir)/gen_intercepts.pl < $(srcdir)/vg_intercept.c.base > $@

vg_replace_malloc.c: $(srcdir)/gen_intercepts.pl $(srcdir)/vg_replace_malloc.c.base
	rm -f $@
	$(PERL) $(srcdir)/gen_intercepts.pl < $(srcdir)/vg_replace_malloc.c.base > $@

vg_toolint.c: $(srcdir)/gen_toolint.pl $(srcdir)/toolfuncs.def ./Makefile
	rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl callwrap     < $(srcdir)/toolfuncs.def >  $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl missingfuncs < $(srcdir)/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl initfunc     < $(srcdir)/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl initdlsym    < $(srcdir)/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl structdef    < $(srcdir)/toolfuncs.def >> $@ || rm -f $@

vg_toolint.h:  $(srcdir)/gen_toolint.pl $(srcdir)/toolfuncs.def ./Makefile
	rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl proto  < $(srcdir)/toolfuncs.def >  $@ || rm -f $@
	$(PERL) $(srcdir)/gen_toolint.pl struct < $(srcdir)/toolfuncs.def >> $@ || rm -f $@

libpthread_so_SOURCES = \
	vg_libpthread.c \
	vg_libpthread_unimp.c \
	${VG_ARCH}/libpthread.c \
	${VG_PLATFORM}/syscall.S
libpthread_so_DEPENDENCIES = $(srcdir)/vg_libpthread.vs
libpthread_so_CFLAGS = $(AM_CFLAGS) -fpic -fno-omit-frame-pointer
libpthread_so_LDFLAGS	   = -Werror -fno-omit-frame-pointer -UVG_LIBDIR \
	-shared -ldl \
	-Wl,-version-script $(srcdir)/vg_libpthread.vs \
	-Wl,-z,nodelete \
	-Wl,--soname=libpthread.so.0

vg_inject_so_SOURCES = \
	vg_intercept.c 
vg_inject_so_CFLAGS = $(AM_CFLAGS) -fpic
vg_inject_so_LDFLAGS = \
	-shared \
	-Wl,--soname,vg_inject.so \
	-Wl,-z,initfirst

lib_replace_malloc_a_SOURCES = vg_replace_malloc.c
lib_replace_malloc_a_CFLAGS  = $(AM_CFLAGS) -fpic -fno-omit-frame-pointer

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS) $(inplacedir)/libpthread.so.0

all-local:
	mkdir -p $(inplacedir)
	for i in $(val_PROGRAMS); do \
		to=$(inplacedir)/$$(echo $$i | sed 's,libpthread.so,libpthread.so.0,'); \
		rm -f $$$to; \
		ln -sf ../$(subdir)/$$i $$to; \
	done

