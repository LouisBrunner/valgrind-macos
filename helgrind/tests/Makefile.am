noinst_SCRIPTS = filter_stderr

INSN_TESTS=insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = $(noinst_SCRIPTS) \
	allok.stderr.exp allok.vgtest		\
	deadlock.stderr.exp deadlock.vgtest	\
	inherit.stderr.exp inherit.vgtest	\
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	race.stderr.exp race.vgtest		\
	race2.stderr.exp race2.vgtest		\
	readshared.stderr.exp readshared.vgtest

check_PROGRAMS = \
	allok deadlock inherit race race2 readshared

allok_SOURCES		= allok.c
deadlock_SOURCES	= deadlock.c
inherit_SOURCES		= inherit.c
race_SOURCES		= race.c
race2_SOURCES		= race2.c
readshared_SOURCES	= readshared.c

# force -gstabs, because we don't print symaddr for DWARF yet
AM_CFLAGS	= $(WERROR) -Winline -Wall -Wshadow -gstabs
LDADD		= -lpthread
