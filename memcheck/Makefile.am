
SUBDIRS = . tests docs

all_includes = -I$(top_srcdir)/include

AM_CPPFLAGS = $(all_includes) -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g
AM_CCASFLAGS = $(all_includes)

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_memcheck.so

vgskin_memcheck_so_SOURCES = \
	mac_leakcheck.c \
	mac_malloc_wrappers.c \
	mac_needs.c \
	mac_replace_strmem.c \
	mc_main.c \
	mc_clientreqs.c \
	mc_errcontext.c \
	mc_from_ucode.c \
	mc_translate.c \
	mc_helpers.S
vgskin_memcheck_so_LDFLAGS = -shared
vgskin_memcheck_so_LDADD = ../coregrind/vg_replace_malloc.o

mcincludedir = $(includedir)/valgrind

mcinclude_HEADERS = \
	memcheck.h

noinst_HEADERS =	\
	mac_shared.h	\
	mc_constants.h	\
	mc_include.h

mac_replace_strmem.o: CFLAGS += -fno-omit-frame-pointer

