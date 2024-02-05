
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .

# OS-specific tests 
if VGCONF_OS_IS_SOLARIS
SUBDIRS += solaris
endif

DIST_SUBDIRS = solaris

dist_noinst_SCRIPTS = \
	invoker simulate_control_c \
	make_local_links \
	filter_make_empty \
	filter_helgrind_monitor \
	filter_helgrind_monitor_solaris \
	filter_passsigalrm \
	send_signal

noinst_SCRIPTS = \
	filter_gdb \
	filter_memcheck_monitor \
	filter_stderr \
	filter_vgdb

EXTRA_DIST = \
	README_DEVELOPERS \
	hginfo.stderrB.exp \
	hginfo.stderr.exp \
	hginfo.stdinB.gdb \
	hginfo.stdoutB.exp \
	hginfo.vgtest \
	hgtls.stderrB.exp \
	hgtls.stderr.exp \
	hgtls.stdinB.gdb \
	hgtls.stdoutB.exp \
	hgtls.vgtest \
	mcblocklistsearch.stderr.exp \
	mcblocklistsearch.stdinB.gdb \
	mcblocklistsearch.vgtest \
	mcblocklistsearch.stderrB.exp \
	mcbreak.stderrB.exp \
	mcbreak.stderr.exp \
	mcbreak.stdinB.gdb \
	mcbreak.stdoutB.exp \
	mcbreak.stdout.exp \
	mcbreak.vgtest \
	mcclean_after_fork.stderrB.exp \
	mcclean_after_fork.stderr.exp \
	mcclean_after_fork.stdinB.gdb \
	mcclean_after_fork.stdoutB.exp \
	mcclean_after_fork.vgtest \
	mchelp.stderrB.exp \
	mchelp.stderr.exp \
	mchelp.stdoutB.exp \
	mchelp.vgtest \
	mcinfcallRU.stderrB.exp \
	mcinfcallRU.stderr.exp \
	mcinfcallRU.stdinB.gdb \
	mcinfcallRU.vgtest \
	mcinfcallWSRU.stderrB.exp \
	mcinfcallWSRU.stderr.exp \
	mcinfcallWSRU.stdinB.gdb \
	mcinfcallWSRU.vgtest \
	mcinvokeRU.stderrB.exp \
	mcinvokeRU.stderr.exp \
	mcinvokeRU.stdoutB.exp \
	mcinvokeRU.vgtest \
	mcinvokeWS.stderrB.exp \
	mcinvokeWS.stderr.exp \
	mcinvokeWS.stdoutB.exp \
	mcinvokeWS.vgtest \
	mcleak.stderrB.exp \
	mcleak.stderr.exp \
	mcleak.stdinB.gdb \
	mcleak.stdoutB.exp \
	mcleak.vgtest \
	mcmain_pic.stderrB.exp \
	mcmain_pic.stderr.exp \
	mcmain_pic.stderr.exp-solaris \
	mcmain_pic.stdinB.gdb \
	mcmain_pic.stdoutB.exp \
	mcmain_pic.stdout.exp \
	mcmain_pic.vgtest \
	mcmain_pic.stderrB.exp \
	mcmain_pic.stderr.exp \
	mcmain_pic.stdinB.gdb \
	mcmain_pic.stdoutB.exp \
	mcmain_pic.stdout.exp \
	mcsignopass.stderrB.exp \
	mcsignopass.stderr.exp \
	mcsignopass.stdinB.gdb \
	mcsignopass.stdoutB.exp \
	mcsignopass.stdoutB.exp-freebsd \
	mcsignopass.vgtest \
	mcsigpass.stderrB.exp \
	mcsigpass.stderr.exp \
	mcsigpass.stdinB.gdb \
	mcsigpass.stdoutB.exp \
	mcsigpass.stdoutB.exp-freebsd \
	mcsigpass.vgtest \
	mcvabits.stderrB.exp \
	mcvabits.stderr.exp \
	mcvabits.stdinB.gdb \
	mcvabits.stdoutB.exp \
	mcvabits.vgtest \
	mcwatchpoints.stderrB.exp \
	mcwatchpoints.stderr.exp \
	mcwatchpoints.stdinB.gdb \
	mcwatchpoints.stdoutB.exp \
	mcwatchpoints.vgtest \
	mssnapshot.stderrB.exp \
	mssnapshot.stderr.exp \
	mssnapshot.stdinB.gdb \
	mssnapshot.stdoutB.exp \
	mssnapshot.vgtest \
	nlcontrolc.stderrB.exp \
	nlcontrolc.stderr.exp \
	nlcontrolc.stdinB.gdb \
	nlcontrolc.stdoutB.exp \
	nlcontrolc.stdoutB.exp-freebsd \
	nlcontrolc.vgtest \
	nlfork_chain.stderr.exp \
	nlfork_chain.stdout.exp \
	nlfork_chain.vgtest \
	nlgone_abrt.stderr.exp \
	nlgone_abrt.stderrB.exp \
	nlgone_abrt.stdinB.gdb \
	nlgone_abrt.stdoutB.exp \
	nlgone_abrt.vgtest \
	nlgone_exit.stderr.exp \
	nlgone_exit.stderrB.exp \
	nlgone_exit.stdinB.gdb \
	nlgone_exit.stdoutB.exp \
	nlgone_exit.vgtest \
	nlgone_return.stderr.exp \
	nlgone_return.stderrB.exp \
	nlgone_return.stdinB.gdb \
	nlgone_return.stdoutB.exp \
	nlgone_return.vgtest \
	nlpasssigalrm.vgtest \
	nlpasssigalrm.stderrB.exp \
	nlpasssigalrm.stderr.exp \
	nlpasssigalrm.stdinB.gdb \
	nlpasssigalrm.stdoutB.exp \
	nlself_invalidate.stderrB.exp \
	nlself_invalidate.stderr.exp \
	nlself_invalidate.stdinB.gdb \
	nlself_invalidate.vgtest \
	nlsigvgdb.vgtest \
	nlsigvgdb.stderr.exp \
	nlsigvgdb.stderrB.exp \
	nlsigvgdb.stdinB.gdb \
	nlvgdbsigqueue.vgtest \
	nlvgdbsigqueue.stderrB.exp \
	nlvgdbsigqueue.stderr.exp \
	nlvgdbsigqueue.stdinB.gdb \
	nlvgdbsigqueue.stdoutB.exp \
	nlvgdbsigqueue.stdoutB.exp-solaris1 \
	nlvgdbsigqueue.stdoutB.exp-solaris2

check_PROGRAMS = \
	clean_after_fork \
	fork_chain \
	gone \
	passsigalrm \
	self_invalidate \
	sleepers \
	t \
	watchpoints

if !VGCONF_OS_IS_FREEBSD
  check_PROGRAMS += main_pic

  main_pic_LDFLAGS = @FLAG_PIE@
  main_pic_CFLAGS = $(AM_CFLAGS) -fPIC
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

LDADD = -lpthread

sleepers_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@
