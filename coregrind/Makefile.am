include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

SUBDIRS = x86 demangle . docs

AM_CPPFLAGS += -DVG_LIBDIR="\"$(valdir)"\" -I$(srcdir)/demangle \
		-I$(srcdir)/x86 \
		-DKICKSTART_BASE=$(KICKSTART_BASE)
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fno-omit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g -DELFSZ=32
AM_CCASFLAGS = $(add_includes) -I..

default.supp: $(SUPP_FILES)

bin_PROGRAMS = \
	valgrind

val_PROGRAMS = \
	stage2 \
	libpthread.so \
	vg_inject.so

EXTRA_DIST = \
	vg_libpthread.vs valgrind.vs \
	gen_toolint.pl toolfuncs.def \
	gen_intercepts.pl vg_replace_malloc.c.base vg_intercept.c.base \
	vg_replace_malloc.c

BUILT_SOURCES = vg_toolint.c vg_toolint.h
CLEANFILES = vg_toolint.c vg_toolint.h vg_replace_malloc.c vg_intercept.c

valgrind_SOURCES = \
	stage1.c \
	ume.c \
	x86/ume_entry.S \
	x86/ume_go.c 
valgrind_DEPENDENCIES =
valgrind_LDFLAGS=-static -g -Wl,-e,_ume_entry
valgrind_LDADD=

# Where stage2 will be put.
# Nb: Hard-wiring this sucks.  A configure-time test would be better.  A
#     load-time test would be even better, but would require building stage2 in
#     a position-independent way...
KICKSTART_BASE=0xb0000000

stage2_SOURCES = \
	ume.c \
	x86/ume_entry.S \
	x86/ume_go.c \
	\
	vg_scheduler.c \
	vg_default.c \
	vg_demangle.c \
	vg_dispatch.S \
	vg_errcontext.c \
	vg_execontext.c \
	vg_from_ucode.c \
	vg_hashtable.c \
	vg_helpers.S \
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
	vg_syscall.S \
	vg_to_ucode.c \
	vg_toolint.c \
	vg_translate.c \
	vg_transtab.c \
	vg_ldt.c \
	vg_cpuid.S
stage2_DEPENDENCIES = $(srcdir)/valgrind.vs x86/stage2.lds
stage2_LDFLAGS=-Wl,--export-dynamic -Wl,-e,_ume_entry  -g \
	-Wl,-defsym,kickstart_base=$(KICKSTART_BASE) \
	-Wl,-T,x86/stage2.lds \
	-Wl,-version-script $(srcdir)/valgrind.vs 
stage2_LDADD= \
	demangle/cp-demangle.o \
	demangle/cplus-dem.o \
	demangle/dyn-string.o \
	demangle/safe-ctype.o \
	-ldl

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
	vg_syscall.S
libpthread_so_DEPENDENCIES = $(srcdir)/vg_libpthread.vs
libpthread_so_LDFLAGS	   = -Werror -fno-omit-frame-pointer -UVG_LIBDIR \
	-shared -fpic -ldl \
	-Wl,-version-script $(srcdir)/vg_libpthread.vs \
	-Wl,-z,nodelete \
	-Wl,--soname=libpthread.so.0

vg_inject_so_SOURCES = \
	vg_intercept.c 

# Not really true, but we need to build vg_replace_malloc.o somehow
vg_inject_so_DEPENDENCIES = \
	vg_replace_malloc.o

vg_inject_so_LDFLAGS = \
	-shared \
	-Wl,--soname,vg_inject.so \
	-Wl,-z,initfirst

noinst_HEADERS = \
	core.h			\
	ume.h			\
	ume_arch.h		\
        vg_constants.h          \
	vg_symtab2.h		\
        vg_unistd.h             \
	vg_symtypes.h		\
	vg_toolint.h		\
	vg_unsafe.h

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS) $(inplacedir)/libpthread.so.0

vg_replace_malloc.o vg_intercept.o vg_libpthread.o: CFLAGS += -fno-omit-frame-pointer -g -fpic

all-local:
	mkdir -p $(inplacedir)
	for i in $(val_PROGRAMS); do \
		to=$(inplacedir)/$$(echo $$i | sed 's,libpthread.so,libpthread.so.0,'); \
		rm -f $$$to; \
		ln -sf ../$(subdir)/$$i $$to; \
	done

