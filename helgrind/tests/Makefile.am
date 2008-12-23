
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	bar_bad.vgtest bar_bad.stdout.exp \
		bar_bad.stderr.exp-glibc28-amd64 \
	bar_trivial.vgtest bar_trivial.stdout.exp \
		bar_trivial.stderr.exp-glibc28-amd64 \
	hg01_all_ok.vgtest hg01_all_ok.stdout.exp \
		hg01_all_ok.stderr.exp-glibc25-amd64 \
	hg02_deadlock.vgtest hg02_deadlock.stdout.exp \
		hg02_deadlock.stderr.exp-glibc25-amd64 \
		hg02_deadlock.stderr.exp-glibc25-x86 \
		hg02_deadlock.stderr.exp-glibc27-amd64 \
	hg03_inherit.vgtest hg03_inherit.stdout.exp \
		hg03_inherit.stderr.exp-glibc25-amd64 \
		hg03_inherit.stderr.exp-glibc25-x86 \
		hg03_inherit.stderr.exp-glibc27-amd64 \
	hg04_race.vgtest hg04_race.stdout.exp \
		hg04_race.stderr.exp-glibc25-amd64 \
		hg04_race.stderr.exp-glibc25-x86 \
		hg04_race.stderr.exp-glibc27-amd64 \
	hg05_race2.vgtest hg05_race2.stdout.exp \
		hg05_race2.stderr.exp-glibc25-amd64 \
		hg05_race2.stderr.exp-glibc25-x86 \
		hg05_race2.stderr.exp-glibc27-amd64 \
	hg06_readshared.vgtest hg06_readshared.stdout.exp \
		hg06_readshared.stderr.exp-glibc25-amd64 \
	pth_barrier1.vgtest pth_barrier1.stdout.exp \
		pth_barrier1.stderr.exp-glibc27-amd64 \
		pth_barrier1.stderr.exp-glibc28-amd64 \
	pth_barrier2.vgtest pth_barrier2.stdout.exp \
		pth_barrier2.stderr.exp-glibc27-amd64 \
		pth_barrier2.stderr.exp-glibc28-amd64 \
	pth_barrier3.vgtest pth_barrier3.stdout.exp \
		pth_barrier3.stderr.exp-glibc27-amd64 \
		pth_barrier3.stderr.exp-glibc28-amd64 \
	rwlock_race.vgtest rwlock_race.stdout.exp \
		rwlock_race.stderr.exp-glibc25-amd64 \
		rwlock_race.stderr.exp-glibc27-amd64 \
	rwlock_test.vgtest rwlock_test.stdout.exp \
		rwlock_test.stderr.exp-glibc25-amd64 \
	tc01_simple_race.vgtest tc01_simple_race.stdout.exp \
		tc01_simple_race.stderr.exp-glibc25-amd64 \
		tc01_simple_race.stderr.exp-glibc25-x86 \
		tc01_simple_race.stderr.exp-glibc27-amd64 \
	tc02_simple_tls.vgtest tc02_simple_tls.stdout.exp \
		tc02_simple_tls.stderr.exp-glibc25-amd64 \
	tc03_re_excl.vgtest tc03_re_excl.stdout.exp \
		tc03_re_excl.stderr.exp-glibc25-amd64 \
	tc04_free_lock.vgtest tc04_free_lock.stdout.exp \
		tc04_free_lock.stderr.exp-glibc25-amd64 \
	tc05_simple_race.vgtest tc05_simple_race.stdout.exp \
		tc05_simple_race.stderr.exp-glibc25-amd64 \
		tc05_simple_race.stderr.exp-glibc25-x86 \
		tc05_simple_race.stderr.exp-glibc27-amd64 \
	tc06_two_races.vgtest tc06_two_races.stdout.exp \
		tc06_two_races.stderr.exp-glibc25-amd64 \
		tc06_two_races.stderr.exp-glibc25-x86 \
		tc06_two_races.stderr.exp-glibc27-amd64 \
	tc07_hbl1.vgtest tc07_hbl1.stdout.exp \
		tc07_hbl1.stderr.exp-glibc25-amd64 \
	tc08_hbl2.vgtest tc08_hbl2.stdout.exp \
		tc08_hbl2.stderr.exp-glibc25-amd64 \
	tc09_bad_unlock.vgtest tc09_bad_unlock.stdout.exp \
		tc09_bad_unlock.stderr.exp-glibc23-amd64 \
		tc09_bad_unlock.stderr.exp-glibc25-amd64 \
		tc09_bad_unlock.stderr.exp-glibc25-x86 \
		tc09_bad_unlock.stderr.exp-glibc27-amd64 \
	tc10_rec_lock.vgtest tc10_rec_lock.stdout.exp \
		tc10_rec_lock.stderr.exp-glibc25-amd64 \
	tc11_XCHG.vgtest tc11_XCHG.stdout.exp \
		tc11_XCHG.stderr.exp-glibc25-amd64 \
	tc12_rwl_trivial.vgtest tc12_rwl_trivial.stdout.exp \
		tc12_rwl_trivial.stderr.exp-glibc25-amd64 \
	tc13_laog1.vgtest tc13_laog1.stdout.exp \
		tc13_laog1.stderr.exp-glibc25-amd64 \
	tc14_laog_dinphils.vgtest tc14_laog_dinphils.stdout.exp \
		tc14_laog_dinphils.stderr.exp-glibc25-amd64 \
		tc14_laog_dinphils.stderr.exp-glibc25-x86 \
		tc14_laog_dinphils.stderr.exp-glibc27-amd64 \
	tc15_laog_lockdel.vgtest tc15_laog_lockdel.stdout.exp \
		tc15_laog_lockdel.stderr.exp-glibc25-amd64 \
	tc16_byterace.vgtest tc16_byterace.stdout.exp \
		tc16_byterace.stderr.exp-glibc25-amd64 \
		tc16_byterace.stderr.exp-glibc25-x86 \
		tc16_byterace.stderr.exp-glibc27-amd64 \
	tc17_sembar.vgtest tc17_sembar.stdout.exp \
		tc17_sembar.stderr.exp-glibc25-amd64 \
		tc17_sembar.stderr.exp-glibc25-x86 \
	tc18_semabuse.vgtest tc18_semabuse.stdout.exp \
		tc18_semabuse.stderr.exp-glibc25-amd64 \
		tc18_semabuse.stderr.exp-glibc25-x86 \
		tc18_semabuse.stderr.exp-glibc28-amd64 \
	tc19_shadowmem.vgtest tc19_shadowmem.stdout.exp \
		tc19_shadowmem.stderr.exp-glibc25-amd64 \
		tc19_shadowmem.stderr.exp-glibc25-x86 \
		tc19_shadowmem.stderr.exp-glibc27-amd64 \
	tc20_verifywrap.vgtest tc20_verifywrap.stdout.exp \
		tc20_verifywrap.stderr.exp-glibc23-amd64 \
		tc20_verifywrap.stderr.exp-glibc25-amd64 \
		tc20_verifywrap.stderr.exp-glibc25-x86 \
		tc20_verifywrap.stderr.exp-glibc27-amd64 \
		tc20_verifywrap.stderr.exp-glibc28-amd64 \
	tc21_pthonce.vgtest tc21_pthonce.stdout.exp \
		tc21_pthonce.stderr.exp-glibc23-amd64 \
		tc21_pthonce.stderr.exp-glibc25-amd64 \
		tc21_pthonce.stderr.exp-glibc25-x86 \
		tc21_pthonce.stderr.exp-glibc27-amd64 \
	tc22_exit_w_lock.vgtest tc22_exit_w_lock.stdout.exp \
		tc22_exit_w_lock.stderr.exp-glibc25-amd64 \
		tc22_exit_w_lock.stderr.exp-glibc25-x86 \
		tc22_exit_w_lock.stderr.exp-glibc27-amd64 \
	tc23_bogus_condwait.vgtest tc23_bogus_condwait.stdout.exp \
		tc23_bogus_condwait.stderr.exp-glibc25-amd64 \
		tc23_bogus_condwait.stderr.exp-glibc25-x86 \
	tc24_nonzero_sem.vgtest tc24_nonzero_sem.stdout.exp \
		tc24_nonzero_sem.stderr.exp-glibc25-amd64

check_PROGRAMS = \
	bar_bad \
	bar_trivial \
	hg01_all_ok \
	hg02_deadlock \
	hg03_inherit \
	hg04_race \
	hg05_race2 \
	hg06_readshared \
	pth_barrier \
	rwlock_race \
	rwlock_test \
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
	tc20_verifywrap \
	tc21_pthonce \
	tc22_exit_w_lock \
	tc23_bogus_condwait \
	tc24_nonzero_sem

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include \
                -I$(top_srcdir)/coregrind -I$(top_builddir)/include \
                -I@VEX_DIR@/pub

# are these necessary?
AM_CFLAGS	= $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)
LDADD		= -lpthread

# only needed because of referencing sources in drd/tests
pth_barrier_SOURCES	= ../../drd/tests/pth_barrier.c
rwlock_race_SOURCES	= ../../drd/tests/rwlock_race.c
rwlock_test_SOURCES	= ../../drd/tests/rwlock_test.c
