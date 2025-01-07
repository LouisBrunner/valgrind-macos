
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr test.sh filter_452275 filter_auxv filter_fdleak

EXTRA_DIST = \
	auxv.vgtest \
	auxv.stderr.exp \
	auxv.stderr.exp-32on64 \
	auxv.stderr.exp-freebsd13 \
	auxv.stderr.exp-freebsd131 \
	auxv.stderr.exp-freebsd14 \
	auxv.stderr.exp-arm64 \
	bug498317.vgtest bug498317.stderr.exp \
	bug498317.supp \
	cp.vgtest \
	cp.stderr.exp \
	osrel.vgtest \
	osrel.stderr.exp \
	osrel.stdout.exp \
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
	sanity_level_thread.vgtest \
	sanity_level_thread.stderr.exp \
	swapcontext.vgtest \
	swapcontext.stderr.exp \
	swapcontext.stdout.exp \
	umtx_op_timeout.vgtest \
	umtx_op_timeout.stderr.exp \
	umtx_shm_creat1.vgtest \
	umtx_shm_creat1.stderr.exp \
	umtx_shm_creat2.vgtest \
	umtx_shm_creat2.stderr.exp \
	usrstack.vgtest \
	usrstack.stderr.exp \
	usrstack.stdout.exp

check_PROGRAMS = \
	auxv bug498317 osrel swapcontext hello_world fexecve 452275 usrstack \
	proc_pid_file sanity_level_thread umtx_shm_creat

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

auxv_CFLAGS = ${AM_CFLAGS}
osrel_CFLAGS = ${AM_CFLAGS}
swapcontext_CFLAGS = ${AM_CFLAGS}

hello_world_SOURCES = hello_world.cpp

proc_pid_file_SOURCES = proc_pid_file.cpp

sanity_level_thread_SOURCES = sanity_level_thread.cpp
sanity_level_thread_LDFLAGS = ${AM_LDFLAGS} -pthread

umtx_shm_creat_SOURCES = umtx_shm_creat.cpp
