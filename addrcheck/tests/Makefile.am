noinst_SCRIPTS = filter_stderr

INSN_TESTS=insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = $(noinst_SCRIPTS) \
	badrw.stderr.exp badrw.vgtest \
	fprw.stderr.exp fprw.vgtest \
	insn_basic.vgtest insn_cmov.vgtest insn_mmx.vgtest \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	overlap.stderr.exp overlap.stdout.exp overlap.vgtest \
	toobig-allocs.stderr.exp toobig-allocs.vgtest

