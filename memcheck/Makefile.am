
SUBDIRS = . tests docs

all_includes = -I$(top_builddir)/include -I$(top_srcdir)/include

AM_CPPFLAGS = $(all_includes)
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O2 -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g
AM_CCASFLAGS = $(all_includes)

valdir = $(libdir)/valgrind
inplacedir = $(top_builddir)/.in_place

val_PROGRAMS = vgskin_memcheck.so vgpreload_memcheck.so

vgpreload_memcheck_so_SOURCES = \
	mac_replace_strmem.c
vgpreload_memcheck_so_LDADD = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_memcheck_so_DEPENDENCIES = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_memcheck_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst

vgskin_memcheck_so_SOURCES = \
	mac_leakcheck.c \
	mac_malloc_wrappers.c \
	mac_needs.c \
	mc_main.c \
	mc_clientreqs.c \
	mc_errcontext.c \
	mc_from_ucode.c \
	mc_translate.c \
	mc_helpers.S
vgskin_memcheck_so_LDFLAGS = -shared

mcincludedir = $(includedir)/valgrind

mcinclude_HEADERS = \
	memcheck.h

noinst_HEADERS =	\
	mac_shared.h	\
	mc_constants.h	\
	mc_include.h

mac_replace_strmem.o: CFLAGS += -fno-omit-frame-pointer

all-local:
	mkdir -p $(inplacedir)
	-rm -f $(addprefix $(inplacedir)/,$(val_PROGRAMS))
	ln -f -s $(addprefix $(top_builddir)/$(subdir)/,$(val_PROGRAMS)) $(inplacedir)
