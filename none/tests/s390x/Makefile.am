include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

INSN_TESTS = clc clcle cvb cvd icm lpr tcxb lam_stam xc mvst add sub mul \
             and or xor insert div srst fold_And16 flogr sub_EI add_EI \
             and_EI or_EI xor_EI insert_EI mul_GE add_GE condloadstore \
             op_exception fgx stck stckf stcke stfle cksm mvcl clcl troo \
             trto trot trtt tr tre cij cgij clij clgij crj cgrj clrj clgrj \
             cs csg cds cdsg cu21 cu21_1 cu24 cu24_1 cu42 cu12 cu12_1 \
             ex_sig ex_clone cu14 cu14_1 cu41 fpconv ecag fpext fpext_warn \
             rounding-1 rounding-2 rounding-3 rounding-4 rounding-5 bfp-1 \
             bfp-2 bfp-3 bfp-4 srnm srnmb comp-1 comp-2 exrl tmll tm stmg \
	     ex clst mvc test_fork test_sig rounding-6 rxsbg\
	     spechelper-alr spechelper-algr \
	     spechelper-slr spechelper-slgr \
	     spechelper-cr  spechelper-clr  \
	     spechelper-ltr spechelper-or   \
	     spechelper-icm-1  spechelper-icm-2 spechelper-tmll \
	     spechelper-tm laa
if BUILD_DFP_TESTS
  INSN_TESTS += dfp-1 dfp-2 dfp-3 dfp-4 dfptest dfpext dfpconv srnmt pfpo
endif

check_PROGRAMS = $(INSN_TESTS) \
		 allexec \
		 op00

EXTRA_DIST = \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	ecag.stdout.exp-z10ec ecag.stdout.exp-z196 ecag.stdout.exp-zec12 \
	op00.stderr.exp1 op00.stderr.exp2 op00.vgtest \
	fpext_fail.vgtest fpext_fail.stderr.exp fpext_fail.stdout.exp \
	test.h opcodes.h add.h  and.h  div.h  insert.h dfp_utils.h \
	mul.h  or.h  sub.h  xor.h table.h svc.h rounding.h \
	dfp-1.stderr.exp dfp-1.stdout.exp dfp-1.vgtest \
	dfp-2.stderr.exp dfp-2.stdout.exp dfp-2.vgtest \
	dfp-3.stderr.exp dfp-3.stdout.exp dfp-3.vgtest \
	dfp-4.stderr.exp dfp-4.stdout.exp dfp-4.vgtest \
	dfptest.stderr.exp dfptest.stdout.exp dfptest.vgtest \
	dfpext.stderr.exp dfpext.stdout.exp dfpext.vgtest \
	dfpconv.stderr.exp dfpconv.stdout.exp dfpconv.vgtest \
	srnmt.stderr.exp srnmt.stdout.exp srnmt.vgtest \
	pfpo.stderr.exp pfpo.stdout.exp pfpo.vgtest

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS   = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
tcxb_CFLAGS      = $(AM_CFLAGS) -std=gnu99
cu12_1_CFLAGS    = $(AM_CFLAGS) -DM3=1
cu14_1_CFLAGS    = $(AM_CFLAGS) -DM3=1
cu21_1_CFLAGS    = $(AM_CFLAGS) -DM3=1
cu24_1_CFLAGS    = $(AM_CFLAGS) -DM3=1

ex_clone_LDFLAGS = -lpthread
