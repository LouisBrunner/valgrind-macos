
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr   \
		      filter_helgrind \
		      filter_xml

EXTRA_DIST = \
	annotate_hbefore.vgtest annotate_hbefore.stdout.exp \
		annotate_hbefore.stderr.exp \
	annotate_rwlock.vgtest annotate_rwlock.stdout.exp \
		annotate_rwlock.stderr.exp \
	annotate_smart_pointer.vgtest annotate_smart_pointer.stdout.exp \
		annotate_smart_pointer.stderr.exp \
	cond_timedwait_invalid.vgtest cond_timedwait_invalid.stdout.exp \
		cond_timedwait_invalid.stderr.exp \
	bar_bad.vgtest bar_bad.stdout.exp bar_bad.stderr.exp \
	bar_trivial.vgtest bar_trivial.stdout.exp bar_trivial.stderr.exp \
	free_is_write.vgtest free_is_write.stdout.exp \
		free_is_write.stderr.exp \
	hg01_all_ok.vgtest hg01_all_ok.stdout.exp hg01_all_ok.stderr.exp \
	hg02_deadlock.vgtest hg02_deadlock.stdout.exp hg02_deadlock.stderr.exp \
	hg03_inherit.vgtest hg03_inherit.stdout.exp hg03_inherit.stderr.exp \
	hg04_race.vgtest hg04_race.stdout.exp hg04_race.stderr.exp \
	hg05_race2.vgtest hg05_race2.stdout.exp hg05_race2.stderr.exp \
	hg06_readshared.vgtest hg06_readshared.stdout.exp \
		hg06_readshared.stderr.exp \
	locked_vs_unlocked1_fwd.vgtest \
		locked_vs_unlocked1_fwd.stderr.exp \
		locked_vs_unlocked1_fwd.stdout.exp \
	locked_vs_unlocked1_rev.vgtest \
		locked_vs_unlocked1_rev.stderr.exp \
		locked_vs_unlocked1_rev.stdout.exp \
	locked_vs_unlocked2.vgtest \
		locked_vs_unlocked2.stderr.exp \
		locked_vs_unlocked2.stdout.exp \
	locked_vs_unlocked3.vgtest \
		locked_vs_unlocked3.stderr.exp \
		locked_vs_unlocked3.stdout.exp \
	pth_barrier1.vgtest pth_barrier1.stdout.exp pth_barrier1.stderr.exp \
	pth_barrier2.vgtest pth_barrier2.stdout.exp pth_barrier2.stderr.exp \
	pth_barrier3.vgtest pth_barrier3.stdout.exp pth_barrier3.stderr.exp \
	pth_destroy_cond.vgtest \
		pth_destroy_cond.stdout.exp pth_destroy_cond.stderr.exp \
	pth_spinlock.vgtest pth_spinlock.stdout.exp pth_spinlock.stderr.exp \
	rwlock_race.vgtest rwlock_race.stdout.exp rwlock_race.stderr.exp \
	rwlock_test.vgtest rwlock_test.stdout.exp rwlock_test.stderr.exp \
	t2t_laog.vgtest t2t_laog.stdout.exp t2t_laog.stderr.exp \
	tc01_simple_race.vgtest tc01_simple_race.stdout.exp \
		tc01_simple_race.stderr.exp \
	tc02_simple_tls.vgtest tc02_simple_tls.stdout.exp \
		tc02_simple_tls.stderr.exp \
	tc03_re_excl.vgtest tc03_re_excl.stdout.exp \
		tc03_re_excl.stderr.exp \
	tc04_free_lock.vgtest tc04_free_lock.stdout.exp \
		tc04_free_lock.stderr.exp \
	tc05_simple_race.vgtest tc05_simple_race.stdout.exp \
		tc05_simple_race.stderr.exp \
	tc06_two_races.vgtest tc06_two_races.stdout.exp \
		tc06_two_races.stderr.exp \
	tc06_two_races_xml.vgtest tc06_two_races_xml.stdout.exp \
		tc06_two_races_xml.stderr.exp \
	tc07_hbl1.vgtest tc07_hbl1.stdout.exp tc07_hbl1.stderr.exp \
	tc08_hbl2.vgtest tc08_hbl2.stdout.exp tc08_hbl2.stderr.exp \
	tc09_bad_unlock.vgtest tc09_bad_unlock.stdout.exp \
		tc09_bad_unlock.stderr.exp \
	tc10_rec_lock.vgtest tc10_rec_lock.stdout.exp tc10_rec_lock.stderr.exp \
	tc11_XCHG.vgtest tc11_XCHG.stdout.exp tc11_XCHG.stderr.exp \
	tc12_rwl_trivial.vgtest tc12_rwl_trivial.stdout.exp \
		tc12_rwl_trivial.stderr.exp \
		tc12_rwl_trivial.stderr.exp-darwin970 \
	tc13_laog1.vgtest tc13_laog1.stdout.exp tc13_laog1.stderr.exp \
	tc14_laog_dinphils.vgtest tc14_laog_dinphils.stdout.exp \
		tc14_laog_dinphils.stderr.exp \
	tc15_laog_lockdel.vgtest tc15_laog_lockdel.stdout.exp \
		tc15_laog_lockdel.stderr.exp \
	tc16_byterace.vgtest tc16_byterace.stdout.exp \
		tc16_byterace.stderr.exp \
	tc17_sembar.vgtest tc17_sembar.stdout.exp \
		tc17_sembar.stderr.exp \
	tc18_semabuse.vgtest tc18_semabuse.stdout.exp \
		tc18_semabuse.stderr.exp \
	tc19_shadowmem.vgtest tc19_shadowmem.stdout.exp \
		tc19_shadowmem.stderr.exp \
	tc20_verifywrap.vgtest tc20_verifywrap.stdout.exp \
		tc20_verifywrap.stderr.exp \
	tc21_pthonce.vgtest tc21_pthonce.stdout.exp tc21_pthonce.stderr.exp \
	tc22_exit_w_lock.vgtest tc22_exit_w_lock.stdout.exp \
		tc22_exit_w_lock.stderr.exp \
		tc22_exit_w_lock.stderr.exp-kfail-x86 \
	tc23_bogus_condwait.vgtest tc23_bogus_condwait.stdout.exp \
		tc23_bogus_condwait.stderr.exp \
	tc24_nonzero_sem.vgtest tc24_nonzero_sem.stdout.exp \
		tc24_nonzero_sem.stderr.exp

# XXX: tc18_semabuse uses operations that are unsupported on Darwin.  It
# should be conditionally compiled like tc20_verifywrap is.
check_PROGRAMS = \
	annotate_hbefore \
	cond_timedwait_invalid \
	free_is_write \
	hg01_all_ok \
	hg02_deadlock \
	hg03_inherit \
	hg04_race \
	hg05_race2 \
	hg06_readshared \
	locked_vs_unlocked1 \
	locked_vs_unlocked2 \
	locked_vs_unlocked3 \
	pth_destroy_cond \
	t2t \
	tc01_simple_race \
	tc02_simple_tls \
	tc03_re_excl \
	tc04_free_lock \
	tc05_simple_race \
	tc06_two_races \
	tc07_hbl1 \
	tc08_hbl2 \
	tc09_bad_unlock \
	tc10_rec_lock \
	tc11_XCHG \
	tc12_rwl_trivial \
	tc13_laog1 \
	tc14_laog_dinphils \
	tc15_laog_lockdel \
	tc16_byterace \
	tc17_sembar \
	tc18_semabuse \
	tc19_shadowmem \
	tc21_pthonce \
	tc23_bogus_condwait \
	tc24_nonzero_sem

# DDD: it seg faults, and then the Valgrind exit path hangs
# JRS 29 July 09: it craps out in the stack unwinder, in
#==13480==    at 0xF00B81FF: ??? f00b8180 VG_(get_StackTrace_wrk)
#==13480==    by 0xF00B83F8: ??? f00b8340 VG_(get_StackTrace)
#==13480==    by 0xF009FE19: ??? f009fd70 record_ExeContext_wrk
#==13480==    by 0xF009D92E: ??? f009d8c0 construct_error
#==13480==    by 0xF009F001: ??? f009eef0 VG_(maybe_record_error)
#==13480==    by 0xF0081F80: ??? f0081f00 HG_(record_error_misc)
#==13480==    by 0xF0089C00: ??? f0089b80 evh__pre_thread_ll_exit
#==13480==    by 0xF01111D1: ??? f0111070 run_a_thread_NORETURN
#==13480==    by 0xF0111512: ??? f0111500 start_thread_NORETURN
# when the thread being unwound is at __bsdthread_terminate+0
#
# Like Tom says, the stack unwinder protection is bollocks.
# We should junk all previous schemes and simply get the 
# stack unwinder to consult aspacem at each frame (cache-accelerated,
# of course) to check each page it visits is accessible.
#
if ! VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
   check_PROGRAMS += \
	tc22_exit_w_lock
endif

if VGCONF_PLATFORMS_INCLUDE_ARM_LINUX
annotate_hbefore_CFLAGS = $(AM_CFLAGS) -mcpu=cortex-a8
tc07_hbl1_CFLAGS        = $(AM_CFLAGS) -mcpu=cortex-a8
tc08_hbl2_CFLAGS        = $(AM_CFLAGS) -mcpu=cortex-a8
else
annotate_hbefore_CFLAGS = $(AM_CFLAGS)
tc07_hbl1_CFLAGS        = $(AM_CFLAGS)
tc08_hbl2_CFLAGS        = $(AM_CFLAGS)
endif

if HAVE_PTHREAD_BARRIER
check_PROGRAMS += bar_bad bar_trivial
endif

if HAVE_PTHREAD_MUTEX_TIMEDLOCK
check_PROGRAMS += tc20_verifywrap
endif

if HAVE_BUILTIN_ATOMIC
check_PROGRAMS += annotate_rwlock
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

LDADD = -lpthread

if VGCONF_OS_IS_DARWIN
annotate_hbefore_CFLAGS = $(AM_CFLAGS) -mdynamic-no-pic
else
annotate_hbefore_CFLAGS = $(AM_CFLAGS)
endif
