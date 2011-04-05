include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

INSN_TESTS = clc clcle cvb cvd icm lpr tcxb lam_stam xc mvst add sub mul \
             and or xor insert div srst fold_And16
INSN_EI = flogr sub_EI add_EI and_EI or_EI xor_EI insert_EI
INSN_GE = mul_GE add_GE
INSN_Z196 = condloadstore

if S390_BUILDS_EI
   INSN_TESTS += $(INSN_EI)
endif

if S390_BUILDS_GE
   INSN_TESTS += $(INSN_GE)
endif

if S390_BUILDS_Z196
   INSN_TESTS += $(INSN_Z196)
endif


check_PROGRAMS = $(INSN_TESTS) \
		 ex_sig \
		 ex_clone

EXTRA_DIST = \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	$(addsuffix .stderr.exp,$(INSN_EI)) \
	$(addsuffix .stdout.exp,$(INSN_EI)) \
	$(addsuffix .vgtest,$(INSN_EI)) \
	$(addsuffix .stderr.exp,$(INSN_GE)) \
	$(addsuffix .stdout.exp,$(INSN_GE)) \
	$(addsuffix .vgtest,$(INSN_GE)) \
	$(addsuffix .stderr.exp,$(INSN_Z196)) \
	$(addsuffix .stdout.exp,$(INSN_Z196)) \
	$(addsuffix .vgtest,$(INSN_Z196)) \
	ex_sig.stdout.exp ex_sig.stderr.exp ex_sig.vgtest \
	ex_clone.stdout.exp ex_clone.stderr.exp ex_clone.vgtest \
	test.h add.h  and.h  div.h  insert.h \
	mul.h  or.h  sub.h  test.h  xor.h

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

ex_clone_LDFLAGS = -lpthread
tcxb_CFLAGS      = $(AM_CFLAGS) -std=gnu99
# some versions of binutils require the right march flag
add_CFLAGS       = $(AM_CFLAGS) -march=z990
and_CFLAGS       = $(AM_CFLAGS) -march=z990
mul_CFLAGS       = $(AM_CFLAGS) -march=z990
sub_CFLAGS       = $(AM_CFLAGS) -march=z990
or_CFLAGS        = $(AM_CFLAGS) -march=z990
xor_CFLAGS       = $(AM_CFLAGS) -march=z990
insert_CFLAGS    = $(AM_CFLAGS) -march=z990
mul_GE_CFLAGS    = $(AM_CFLAGS) -march=z10
sub_EI_CFLAGS    = $(AM_CFLAGS) -march=z9-109
add_EI_CFLAGS    = $(AM_CFLAGS) -march=z9-109
add_GE_CFLAGS    = $(AM_CFLAGS) -march=z10
and_EI_CFLAGS    = $(AM_CFLAGS) -march=z9-109
or_EI_CFLAGS    = $(AM_CFLAGS) -march=z9-109
xor_EI_CFLAGS    = $(AM_CFLAGS) -march=z9-109
insert_EI_CFLAGS    = $(AM_CFLAGS) -march=z9-109
condloadstore_CFLAGS    = $(AM_CFLAGS) -march=z196
