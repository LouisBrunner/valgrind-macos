
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	jm-int.stderr.exp jm-int.stdout.exp jm-int.vgtest \
	jm-fp.stderr.exp  jm-fp.stdout.exp  jm-fp.vgtest \
	jm-vmx.stderr.exp jm-vmx.stdout.exp jm-vmx.vgtest \
	lsw.stderr.exp lsw.stdout.exp lsw.vgtest \
	std_reg_imm.vgtest std_reg_imm.stderr.exp std_reg_imm.stdout.exp \
	round.stderr.exp round.stdout.exp round.vgtest \
	twi_tdi.stderr.exp twi_tdi.stdout.exp twi_tdi.vgtest \
	tw_td.stderr.exp tw_td.stdout.exp tw_td.vgtest \
	power6_bcmp.stderr.exp power6_bcmp.stdout.exp power6_bcmp.vgtest \
	power6_mf_gpr.stderr.exp power6_mf_gpr.stdout.exp power6_mf_gpr.vgtest

check_PROGRAMS = \
	jm-insns lsw round std_reg_imm twi_tdi tw_td power6_bcmp power6_mf_gpr

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

if HAS_ALTIVEC
ALTIVEC_FLAG = -DHAS_ALTIVEC
else
ALTIVEC_FLAG =
endif

jm_insns_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames -maltivec \
			@FLAG_M64@ $(ALTIVEC_FLAG)

