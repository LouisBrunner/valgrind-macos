
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr test.sh filter_452275 filter_auxv

EXTRA_DIST = \
	auxv.vgtest \
	auxv.stderr.exp \
	auxv.stderr.exp-32on64 \
	auxv.stderr.exp-freebsd13 \
	auxv.stderr.exp-freebsd131 \
	auxv.stderr.exp-freebsd14 \
	cp.vgtest \
	cp.stderr.exp \
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
	fexecve_txt.stderr.exp \
	452275.vgtest \
	452275.stderr.exp \
	usrstack.vgtest \
	usrstack.stderr.exp \
	usrstack.stdout.exp \
	proc_pid_file.vgtest \
	proc_pid_file.stderr.exp \
	bash_test.vgtest \
	bash_test.sh \
	bash_test.stderr.exp \
	bash_test.stdout.exp \
	csh_test.vgtest \
	csh_test.csh \
	csh_test.stderr.exp \
	csh_test.stdout.exp \
	ksh_test.vgtest \
	ksh_test.ksh \
	ksh_test.stderr.exp \
	ksh_test.stdout.exp \
	umtx_op_timeout.vgtest \
	umtx_op_timeout.stderr.exp

check_PROGRAMS = \
	auxv osrel swapcontext hello_world fexecve 452275 usrstack \
	proc_pid_file

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

auxv_CFLAGS = ${AM_CFLAGS}
osrel_CFLAGS = ${AM_CFLAGS}
swapcontext_CFLAGS = ${AM_CFLAGS}

hello_world_SOURCES = hello_world.cpp

proc_pid_file_SOURCES = proc_pid_file.cpp
