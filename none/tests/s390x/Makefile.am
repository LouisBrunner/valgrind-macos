include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

INSN_TESTS = clc clcle cvb cvd icm lpr tcxb lam_stam xc mvst add sub mul \
             and or xor insert div srst fold_And16 flogr sub_EI add_EI \
             and_EI or_EI xor_EI insert_EI mul_GE add_GE condloadstore \
             op_exception fgx stck stckf stcke stfle cksm mvcl clcl

check_PROGRAMS = $(INSN_TESTS) \
		 ex_sig \
		 ex_clone \
		 op00

EXTRA_DIST = \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	ex_sig.stdout.exp ex_sig.stderr.exp ex_sig.vgtest \
	ex_clone.stdout.exp ex_clone.stderr.exp ex_clone.vgtest \
	op00.stderr.exp1 op00.stderr.exp2 op00.vgtest \
	test.h opcodes.h add.h  and.h  div.h  insert.h \
	mul.h  or.h  sub.h  test.h  xor.h

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

ex_clone_LDFLAGS = -lpthread
tcxb_CFLAGS      = $(AM_CFLAGS) -std=gnu99
