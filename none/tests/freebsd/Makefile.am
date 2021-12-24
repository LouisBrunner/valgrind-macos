
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr test.sh
EXTRA_DIST = \
	auxv.vgtest \
	auxv.stderr.exp \
	auxv.stderr.exp-32on64 \
	auxv.stderr.exp-freebsd13 \
	osrel.vgtest \
	osrel.stderr.exp \
	osrel.stdout.exp \
	swapcontext.vgtest \
	swapcontext.stderr.exp \
	swapcontext.stdout.exp \
	fexecve_hw1.vgtest \
	fexecve_hw1.stdout.exp \
	fexecve_hw1.stderr.exp \
	fexecve_hw2.vgtest \
	fexecve_hw2.stdout.exp \
	fexecve_hw2.stderr.exp \
	fexecve_script1.vgtest \
	fexecve_script1.stderr.exp \
	fexecve_script2.vgtest \
	fexecve_script2.stdout.exp \
	fexecve_script2.stderr.exp \
	fexecve_txt.vgtest \
	fexecve_txt.stderr.exp

check_PROGRAMS = \
	auxv osrel swapcontext hello_world fexecve

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

auxv_CFLAGS = ${AM_CFLAGS}
osrel_CFLAGS = ${AM_CFLAGS}
swapcontext_CFLAGS = ${AM_CFLAGS}

hello_world_SOURCES = hello_world.cpp
