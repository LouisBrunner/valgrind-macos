
SUBDIRS = . docs tests

# include memcheck/ for mac_shared.h
INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/memcheck

AM_CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_addrcheck.so

vgskin_addrcheck_so_SOURCES = ac_main.c
vgskin_addrcheck_so_LDFLAGS = -shared
vgskin_addrcheck_so_LDADD = \
	../memcheck/mac_leakcheck.o \
	../memcheck/mac_malloc_wrappers.o \
	../memcheck/mac_needs.o \
	../coregrind/vg_replace_malloc.o

