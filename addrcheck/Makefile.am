
SUBDIRS = . docs tests

# include memcheck/ for mac_shared.h
AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/memcheck \
		-DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind
inplacedir = $(top_srcdir)/.in_place

val_PROGRAMS = vgskin_addrcheck.so vgpreload_addrcheck.so

vgskin_addrcheck_so_SOURCES = ac_main.c
vgskin_addrcheck_so_LDFLAGS = -shared
vgskin_addrcheck_so_LDADD = \
	../memcheck/mac_leakcheck.o \
	../memcheck/mac_malloc_wrappers.o \
	../memcheck/mac_needs.o \
	../memcheck/mac_replace_strmem.o

vgpreload_addrcheck_so_SOURCES = 
vgpreload_addrcheck_so_LDADD = $(top_srcdir)/coregrind/vg_replace_malloc.o
vgpreload_addrcheck_so_DEPENDENCIES = $(top_srcdir)/coregrind/vg_replace_malloc.o
vgpreload_addrcheck_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst

all-local:
	mkdir -p $(inplacedir)
	-rm -f $(addprefix $(inplacedir)/,$(val_PROGRAMS))
	ln -f -s $(addprefix $(top_srcdir)/$(subdir)/,$(val_PROGRAMS)) $(inplacedir)
