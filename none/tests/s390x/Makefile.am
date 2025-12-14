include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr bfp-emit.pl emwarn-gen.pl

INSN_TESTS = clc clcle cvb cvd icm lpr lam_stam xc mvst add sub mul \
             and or xor insert div srst fold_And16 sub_EI add_EI \
             and_EI or_EI xor_EI insert_EI mul_GE add_GE condloadstore \
             op_exception stck stckf stcke stfle mvcl clcl troo \
             trto trot trtt tr tre cij cgij clij clgij crj cgrj clrj clgrj \
             cs csg cds cdsg cu21 cu21_1 cu24 cu24_1 cu42 cu12 cu12_1 \
             ex_sig ex_clone cu14 cu14_1 cu41 ecag \
             bfp-fpc bfp-306054 \
             bfp-muldiv comp-1 comp-2 exrl tmll tm stmg \
	     ex clst mvc test_fork test_sig rxsbg popcnt \
	     high-word traps \
	     spechelper-alr spechelper-algr \
	     spechelper-slr spechelper-slgr \
	     spechelper-cr  spechelper-clr  \
	     spechelper-ltr spechelper-or   \
	     spechelper-icm-1  spechelper-icm-2 spechelper-tmll \
	     spechelper-tm laa vector lsc2 ppno vector_string vector_integer \
	     vector_float add-z14 sub-z14 mul-z14 bic \
	     misc3 vec2 vec2_float \
	     dfp-1 dfp-2 dfp-3 dfp-4 dfpconv dfpext dfptest pfpo srnmt \
	     hfp

if HAS_MLONG_DOUBLE_128
INSN_TESTS += bfp-arith bfp-compare bfp-convert bfp-load bfp-tdc
endif

check_PROGRAMS = $(INSN_TESTS) \
		 allexec \
		 cksm \
		 flogr \
		 op00

noinst_HEADERS = add.h and.h div.h dfp_utils.h insert.h mul.h opcodes.h \
	or.h rounding.h sub.h svc.h table.h test.h vector.h xor.h

EXTRA_DIST = \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	bfp-arith.vgtest bfp-arith.stdout.exp bfp-arith.stderr.exp \
	bfp-compare.vgtest bfp-compare.stdout.exp bfp-compare.stderr.exp \
	bfp-convert.vgtest bfp-convert.stdout.exp bfp-convert.stderr.exp \
	bfp-load.vgtest bfp-load.stdout.exp bfp-load.stderr.exp \
	bfp-tdc.vgtest bfp-tdc.stdout.exp bfp-tdc.stderr.exp \
	bfp-emit.vgtest bfp-emit.stderr.exp bfp-emit.post.exp \
	bfp-XxC.vgtest bfp-XxC.stderr.exp bfp-XxC.post.exp \
	cksm.vgtest cksm.stderr.exp \
	flogr.vgtest flogr.stderr.exp \
	ecag.stdout.exp-z196 ecag.stdout.exp-zec12 \
	ecag.stdout.exp-z13 ecag.stdout.exp-z14 ecag.stdout.exp-z15 \
	ecag.stdout.exp-z16 ecag.stdout.exp-z17 \
	op00.stderr.exp op00.vgtest \
	dfp-XxC.vgtest dfp-XxC.stderr.exp dfp-XxC.post.exp \
	dfp-XiC.vgtest dfp-XiC.stderr.exp dfp-XiC.post.exp \
	stfle.stdout.exp-z16

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS   = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
cu12_1_CFLAGS    = $(AM_CFLAGS) -DM3=1
cu14_1_CFLAGS    = $(AM_CFLAGS) -DM3=1
cu21_1_CFLAGS    = $(AM_CFLAGS) -DM3=1
cu24_1_CFLAGS    = $(AM_CFLAGS) -DM3=1
ex_clone_LDADD   = -lpthread
vector_CFLAGS    = $(AM_CFLAGS) -march=z13
lsc2_CFLAGS       = -march=z13 -DS390_TESTS_NOCOLOR
vector_string_CFLAGS = $(AM_CFLAGS) -march=z13 -DS390_TEST_COUNT=5
vector_integer_CFLAGS    = $(AM_CFLAGS) -march=z13 -DS390_TEST_COUNT=4
vector_float_CFLAGS    = $(AM_CFLAGS) -march=z13 -DS390_TEST_COUNT=4
vec2_CFLAGS      = $(AM_CFLAGS) -march=z13
vec2_float_CFLAGS = $(AM_CFLAGS) -march=z13
bfp_arith_CFLAGS   = $(AM_CFLAGS) -mlong-double-128
bfp_compare_CFLAGS = $(AM_CFLAGS) -mlong-double-128
bfp_convert_CFLAGS = $(AM_CFLAGS) -mlong-double-128
bfp_load_CFLAGS    = $(AM_CFLAGS) -mlong-double-128
bfp_tdc_CFLAGS     = $(AM_CFLAGS) -mlong-double-128
