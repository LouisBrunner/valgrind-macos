include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

INSN_TESTS = clc clcle cvb cvd icm lpr tcxb lam_stam xc mvst add sub mul \
             and or xor insert div srst fold_And16 flogr sub_EI add_EI \
             and_EI or_EI xor_EI insert_EI mul_GE add_GE condloadstore \
             op_exception fgx stck stckf stcke stfle cksm mvcl clcl troo \
             trto trot trtt tr tre cij cgij clij clgij crj cgrj clrj clgrj \
             cs csg cds cdsg cu21 cu21_1 cu24 cu24_1 cu42 cu12 cu12_1 \
             ex_sig ex_clone cu14 cu14_1 cu41 fpconv

check_PROGRAMS = $(INSN_TESTS) \
		 allexec \
		 op00

EXTRA_DIST = \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	op00.stderr.exp1 op00.stderr.exp2 op00.vgtest \
	test.h opcodes.h add.h  and.h  div.h  insert.h \
	mul.h  or.h  sub.h  test.h  xor.h table.h

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
