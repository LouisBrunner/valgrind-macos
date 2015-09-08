
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = gen_test.sh

if VGCONF_ARCHS_INCLUDE_TILEGX
EXTRA_DIST += \
	insn_test_move_X0.stdout.exp insn_test_move_X0.stderr.exp \
	 insn_test_move_X0.vgtest \
	insn_test_move_X1.stdout.exp insn_test_move_X1.stderr.exp \
	 insn_test_move_X1.vgtest \
	insn_test_move_Y0.stdout.exp insn_test_move_Y0.stderr.exp \
	 insn_test_move_Y0.vgtest \
	insn_test_move_Y1.stdout.exp insn_test_move_Y1.stderr.exp \
	 insn_test_move_Y1.vgtest \
	insn_test_movei_X0.stdout.exp insn_test_movei_X0.stderr.exp \
	 insn_test_movei_X0.vgtest \
	insn_test_movei_X1.stdout.exp insn_test_movei_X1.stderr.exp \
	 insn_test_movei_X1.vgtest \
	insn_test_movei_Y0.stdout.exp insn_test_movei_Y0.stderr.exp \
	 insn_test_movei_Y0.vgtest \
	insn_test_movei_Y1.stdout.exp insn_test_movei_Y1.stderr.exp \
	 insn_test_movei_Y1.vgtest \
	insn_test_moveli_X0.stdout.exp insn_test_moveli_X0.stderr.exp \
	 insn_test_moveli_X0.vgtest \
	insn_test_moveli_X1.stdout.exp insn_test_moveli_X1.stderr.exp \
	 insn_test_moveli_X1.vgtest \
	insn_test_prefetch_X1.stdout.exp insn_test_prefetch_X1.stderr.exp \
	 insn_test_prefetch_X1.vgtest \
	insn_test_prefetch_Y2.stdout.exp insn_test_prefetch_Y2.stderr.exp \
	 insn_test_prefetch_Y2.vgtest \
	insn_test_prefetch_l1_X1.stdout.exp \
	 insn_test_prefetch_l1_X1.stderr.exp \
	 insn_test_prefetch_l1_X1.vgtest \
	insn_test_prefetch_l1_Y2.stdout.exp \
	 insn_test_prefetch_l1_Y2.stderr.exp \
	 insn_test_prefetch_l1_Y2.vgtest \
	insn_test_prefetch_l2_X1.stdout.exp \
	 insn_test_prefetch_l2_X1.stderr.exp \
	 insn_test_prefetch_l2_X1.vgtest \
	insn_test_prefetch_l2_Y2.stdout.exp \
	 insn_test_prefetch_l2_Y2.stderr.exp \
	 insn_test_prefetch_l2_Y2.vgtest \
	insn_test_prefetch_l3_X1.stdout.exp \
	 insn_test_prefetch_l3_X1.stderr.exp \
	 insn_test_prefetch_l3_X1.vgtest \
	insn_test_prefetch_l3_Y2.stdout.exp \
	 insn_test_prefetch_l3_Y2.stderr.exp \
	 insn_test_prefetch_l3_Y2.vgtest \
	insn_test_add_X0.stdout.exp insn_test_add_X0.stderr.exp \
	 insn_test_add_X0.vgtest \
	insn_test_add_X1.stdout.exp insn_test_add_X1.stderr.exp \
	 insn_test_add_X1.vgtest \
	insn_test_add_Y0.stdout.exp insn_test_add_Y0.stderr.exp \
	 insn_test_add_Y0.vgtest \
	insn_test_add_Y1.stdout.exp insn_test_add_Y1.stderr.exp \
	 insn_test_add_Y1.vgtest \
	insn_test_addi_X0.stdout.exp insn_test_addi_X0.stderr.exp \
	 insn_test_addi_X0.vgtest \
	insn_test_addi_X1.stdout.exp insn_test_addi_X1.stderr.exp \
	 insn_test_addi_X1.vgtest \
	insn_test_addi_Y0.stdout.exp insn_test_addi_Y0.stderr.exp \
	 insn_test_addi_Y0.vgtest \
	insn_test_addi_Y1.stdout.exp insn_test_addi_Y1.stderr.exp \
	 insn_test_addi_Y1.vgtest \
	insn_test_addli_X0.stdout.exp insn_test_addli_X0.stderr.exp \
	 insn_test_addli_X0.vgtest \
	insn_test_addli_X1.stdout.exp insn_test_addli_X1.stderr.exp \
	 insn_test_addli_X1.vgtest \
	insn_test_addx_X0.stdout.exp insn_test_addx_X0.stderr.exp \
	 insn_test_addx_X0.vgtest \
	insn_test_addx_X1.stdout.exp insn_test_addx_X1.stderr.exp \
	 insn_test_addx_X1.vgtest \
	insn_test_addx_Y0.stdout.exp insn_test_addx_Y0.stderr.exp \
	 insn_test_addx_Y0.vgtest \
	insn_test_addx_Y1.stdout.exp insn_test_addx_Y1.stderr.exp \
	 insn_test_addx_Y1.vgtest \
	insn_test_addxi_X0.stdout.exp insn_test_addxi_X0.stderr.exp \
	 insn_test_addxi_X0.vgtest \
	insn_test_addxi_X1.stdout.exp insn_test_addxi_X1.stderr.exp \
	 insn_test_addxi_X1.vgtest \
	insn_test_addxi_Y0.stdout.exp insn_test_addxi_Y0.stderr.exp \
	 insn_test_addxi_Y0.vgtest \
	insn_test_addxi_Y1.stdout.exp insn_test_addxi_Y1.stderr.exp \
	 insn_test_addxi_Y1.vgtest \
	insn_test_addxli_X0.stdout.exp insn_test_addxli_X0.stderr.exp \
	 insn_test_addxli_X0.vgtest \
	insn_test_addxli_X1.stdout.exp insn_test_addxli_X1.stderr.exp \
	 insn_test_addxli_X1.vgtest \
	insn_test_addxsc_X0.stdout.exp insn_test_addxsc_X0.stderr.exp \
	 insn_test_addxsc_X0.vgtest \
	insn_test_addxsc_X1.stdout.exp insn_test_addxsc_X1.stderr.exp \
	 insn_test_addxsc_X1.vgtest \
	insn_test_and_X0.stdout.exp insn_test_and_X0.stderr.exp \
	 insn_test_and_X0.vgtest \
	insn_test_and_X1.stdout.exp insn_test_and_X1.stderr.exp \
	 insn_test_and_X1.vgtest \
	insn_test_and_Y0.stdout.exp insn_test_and_Y0.stderr.exp \
	 insn_test_and_Y0.vgtest \
	insn_test_and_Y1.stdout.exp insn_test_and_Y1.stderr.exp \
	 insn_test_and_Y1.vgtest \
	insn_test_andi_X0.stdout.exp insn_test_andi_X0.stderr.exp \
	 insn_test_andi_X0.vgtest \
	insn_test_andi_X1.stdout.exp insn_test_andi_X1.stderr.exp \
	 insn_test_andi_X1.vgtest \
	insn_test_andi_Y0.stdout.exp insn_test_andi_Y0.stderr.exp \
	 insn_test_andi_Y0.vgtest \
	insn_test_andi_Y1.stdout.exp insn_test_andi_Y1.stderr.exp \
	 insn_test_andi_Y1.vgtest \
	insn_test_beqz_X1.stdout.exp insn_test_beqz_X1.stderr.exp \
	 insn_test_beqz_X1.vgtest \
	insn_test_beqzt_X1.stdout.exp insn_test_beqzt_X1.stderr.exp \
	 insn_test_beqzt_X1.vgtest \
	insn_test_bfexts_X0.stdout.exp insn_test_bfexts_X0.stderr.exp \
	 insn_test_bfexts_X0.vgtest \
	insn_test_bfextu_X0.stdout.exp insn_test_bfextu_X0.stderr.exp \
	 insn_test_bfextu_X0.vgtest \
	insn_test_bfins_X0.stdout.exp insn_test_bfins_X0.stderr.exp \
	 insn_test_bfins_X0.vgtest \
	insn_test_bgez_X1.stdout.exp insn_test_bgez_X1.stderr.exp \
	 insn_test_bgez_X1.vgtest \
	insn_test_bgezt_X1.stdout.exp insn_test_bgezt_X1.stderr.exp \
	 insn_test_bgezt_X1.vgtest \
	insn_test_bgtz_X1.stdout.exp insn_test_bgtz_X1.stderr.exp \
	 insn_test_bgtz_X1.vgtest \
	insn_test_bgtzt_X1.stdout.exp insn_test_bgtzt_X1.stderr.exp \
	 insn_test_bgtzt_X1.vgtest \
	insn_test_blbc_X1.stdout.exp insn_test_blbc_X1.stderr.exp \
	 insn_test_blbc_X1.vgtest \
	insn_test_blbct_X1.stdout.exp insn_test_blbct_X1.stderr.exp \
	 insn_test_blbct_X1.vgtest \
	insn_test_blbs_X1.stdout.exp insn_test_blbs_X1.stderr.exp \
	 insn_test_blbs_X1.vgtest \
	insn_test_blbst_X1.stdout.exp insn_test_blbst_X1.stderr.exp \
	 insn_test_blbst_X1.vgtest \
	insn_test_blez_X1.stdout.exp insn_test_blez_X1.stderr.exp \
	 insn_test_blez_X1.vgtest \
	insn_test_blezt_X1.stdout.exp insn_test_blezt_X1.stderr.exp \
	 insn_test_blezt_X1.vgtest \
	insn_test_bltz_X1.stdout.exp insn_test_bltz_X1.stderr.exp \
	 insn_test_bltz_X1.vgtest \
	insn_test_bltzt_X1.stdout.exp insn_test_bltzt_X1.stderr.exp \
	 insn_test_bltzt_X1.vgtest \
	insn_test_bnez_X1.stdout.exp insn_test_bnez_X1.stderr.exp \
	 insn_test_bnez_X1.vgtest \
	insn_test_bnezt_X1.stdout.exp insn_test_bnezt_X1.stderr.exp \
	 insn_test_bnezt_X1.vgtest \
	insn_test_clz_X0.stdout.exp insn_test_clz_X0.stderr.exp \
	 insn_test_clz_X0.vgtest \
	insn_test_clz_Y0.stdout.exp insn_test_clz_Y0.stderr.exp \
	 insn_test_clz_Y0.vgtest \
	insn_test_cmoveqz_X0.stdout.exp insn_test_cmoveqz_X0.stderr.exp \
	 insn_test_cmoveqz_X0.vgtest \
	insn_test_cmoveqz_Y0.stdout.exp insn_test_cmoveqz_Y0.stderr.exp \
	 insn_test_cmoveqz_Y0.vgtest \
	insn_test_cmovnez_X0.stdout.exp insn_test_cmovnez_X0.stderr.exp \
	 insn_test_cmovnez_X0.vgtest \
	insn_test_cmovnez_Y0.stdout.exp insn_test_cmovnez_Y0.stderr.exp \
	 insn_test_cmovnez_Y0.vgtest \
	insn_test_cmpeq_X0.stdout.exp insn_test_cmpeq_X0.stderr.exp \
	 insn_test_cmpeq_X0.vgtest \
	insn_test_cmpeq_X1.stdout.exp insn_test_cmpeq_X1.stderr.exp \
	 insn_test_cmpeq_X1.vgtest \
	insn_test_cmpeq_Y0.stdout.exp insn_test_cmpeq_Y0.stderr.exp \
	 insn_test_cmpeq_Y0.vgtest \
	insn_test_cmpeq_Y1.stdout.exp insn_test_cmpeq_Y1.stderr.exp \
	 insn_test_cmpeq_Y1.vgtest \
	insn_test_cmpeqi_X0.stdout.exp insn_test_cmpeqi_X0.stderr.exp \
	 insn_test_cmpeqi_X0.vgtest \
	insn_test_cmpeqi_X1.stdout.exp insn_test_cmpeqi_X1.stderr.exp \
	 insn_test_cmpeqi_X1.vgtest \
	insn_test_cmpeqi_Y0.stdout.exp insn_test_cmpeqi_Y0.stderr.exp \
	 insn_test_cmpeqi_Y0.vgtest \
	insn_test_cmpeqi_Y1.stdout.exp insn_test_cmpeqi_Y1.stderr.exp \
	 insn_test_cmpeqi_Y1.vgtest \
	insn_test_cmples_X0.stdout.exp insn_test_cmples_X0.stderr.exp \
	 insn_test_cmples_X0.vgtest \
	insn_test_cmples_X1.stdout.exp insn_test_cmples_X1.stderr.exp \
	 insn_test_cmples_X1.vgtest \
	insn_test_cmples_Y0.stdout.exp insn_test_cmples_Y0.stderr.exp \
	 insn_test_cmples_Y0.vgtest \
	insn_test_cmples_Y1.stdout.exp insn_test_cmples_Y1.stderr.exp \
	 insn_test_cmples_Y1.vgtest \
	insn_test_cmpleu_X0.stdout.exp insn_test_cmpleu_X0.stderr.exp \
	 insn_test_cmpleu_X0.vgtest \
	insn_test_cmpleu_X1.stdout.exp insn_test_cmpleu_X1.stderr.exp \
	 insn_test_cmpleu_X1.vgtest \
	insn_test_cmpleu_Y0.stdout.exp insn_test_cmpleu_Y0.stderr.exp \
	 insn_test_cmpleu_Y0.vgtest \
	insn_test_cmpleu_Y1.stdout.exp insn_test_cmpleu_Y1.stderr.exp \
	 insn_test_cmpleu_Y1.vgtest \
	insn_test_cmplts_X0.stdout.exp insn_test_cmplts_X0.stderr.exp \
	 insn_test_cmplts_X0.vgtest \
	insn_test_cmplts_X1.stdout.exp insn_test_cmplts_X1.stderr.exp \
	 insn_test_cmplts_X1.vgtest \
	insn_test_cmplts_Y0.stdout.exp insn_test_cmplts_Y0.stderr.exp \
	 insn_test_cmplts_Y0.vgtest \
	insn_test_cmplts_Y1.stdout.exp insn_test_cmplts_Y1.stderr.exp \
	 insn_test_cmplts_Y1.vgtest \
	insn_test_cmpltsi_X0.stdout.exp insn_test_cmpltsi_X0.stderr.exp \
	 insn_test_cmpltsi_X0.vgtest \
	insn_test_cmpltsi_X1.stdout.exp insn_test_cmpltsi_X1.stderr.exp \
	 insn_test_cmpltsi_X1.vgtest \
	insn_test_cmpltsi_Y0.stdout.exp insn_test_cmpltsi_Y0.stderr.exp \
	 insn_test_cmpltsi_Y0.vgtest \
	insn_test_cmpltsi_Y1.stdout.exp insn_test_cmpltsi_Y1.stderr.exp \
	 insn_test_cmpltsi_Y1.vgtest \
	insn_test_cmpltu_X0.stdout.exp insn_test_cmpltu_X0.stderr.exp \
	 insn_test_cmpltu_X0.vgtest \
	insn_test_cmpltu_X1.stdout.exp insn_test_cmpltu_X1.stderr.exp \
	 insn_test_cmpltu_X1.vgtest \
	insn_test_cmpltu_Y0.stdout.exp insn_test_cmpltu_Y0.stderr.exp \
	 insn_test_cmpltu_Y0.vgtest \
	insn_test_cmpltu_Y1.stdout.exp insn_test_cmpltu_Y1.stderr.exp \
	 insn_test_cmpltu_Y1.vgtest \
	insn_test_cmpltui_X0.stdout.exp insn_test_cmpltui_X0.stderr.exp \
	 insn_test_cmpltui_X0.vgtest \
	insn_test_cmpltui_X1.stdout.exp insn_test_cmpltui_X1.stderr.exp \
	 insn_test_cmpltui_X1.vgtest \
	insn_test_cmpne_X0.stdout.exp insn_test_cmpne_X0.stderr.exp \
	 insn_test_cmpne_X0.vgtest \
	insn_test_cmpne_X1.stdout.exp insn_test_cmpne_X1.stderr.exp \
	 insn_test_cmpne_X1.vgtest \
	insn_test_cmpne_Y0.stdout.exp insn_test_cmpne_Y0.stderr.exp \
	 insn_test_cmpne_Y0.vgtest \
	insn_test_cmpne_Y1.stdout.exp insn_test_cmpne_Y1.stderr.exp \
	 insn_test_cmpne_Y1.vgtest \
	insn_test_cmul_X0.stdout.exp insn_test_cmul_X0.stderr.exp \
	 insn_test_cmul_X0.vgtest \
	insn_test_cmula_X0.stdout.exp insn_test_cmula_X0.stderr.exp \
	 insn_test_cmula_X0.vgtest \
	insn_test_cmulaf_X0.stdout.exp insn_test_cmulaf_X0.stderr.exp \
	 insn_test_cmulaf_X0.vgtest \
	insn_test_cmulf_X0.stdout.exp insn_test_cmulf_X0.stderr.exp \
	 insn_test_cmulf_X0.vgtest \
	insn_test_cmulfr_X0.stdout.exp insn_test_cmulfr_X0.stderr.exp \
	 insn_test_cmulfr_X0.vgtest \
	insn_test_cmulh_X0.stdout.exp insn_test_cmulh_X0.stderr.exp \
	 insn_test_cmulh_X0.vgtest \
	insn_test_cmulhr_X0.stdout.exp insn_test_cmulhr_X0.stderr.exp \
	 insn_test_cmulhr_X0.vgtest \
	insn_test_crc32_32_X0.stdout.exp insn_test_crc32_32_X0.stderr.exp \
	 insn_test_crc32_32_X0.vgtest \
	insn_test_crc32_8_X0.stdout.exp insn_test_crc32_8_X0.stderr.exp \
	 insn_test_crc32_8_X0.vgtest \
	insn_test_ctz_X0.stdout.exp insn_test_ctz_X0.stderr.exp \
	 insn_test_ctz_X0.vgtest \
	insn_test_ctz_Y0.stdout.exp insn_test_ctz_Y0.stderr.exp \
	 insn_test_ctz_Y0.vgtest \
	insn_test_dblalign_X0.stdout.exp insn_test_dblalign_X0.stderr.exp \
	 insn_test_dblalign_X0.vgtest \
	insn_test_dblalign2_X0.stdout.exp insn_test_dblalign2_X0.stderr.exp \
	 insn_test_dblalign2_X0.vgtest \
	insn_test_dblalign2_X1.stdout.exp insn_test_dblalign2_X1.stderr.exp \
	 insn_test_dblalign2_X1.vgtest \
	insn_test_dblalign4_X0.stdout.exp insn_test_dblalign4_X0.stderr.exp \
	 insn_test_dblalign4_X0.vgtest \
	insn_test_dblalign4_X1.stdout.exp insn_test_dblalign4_X1.stderr.exp \
	 insn_test_dblalign4_X1.vgtest \
	insn_test_dblalign6_X0.stdout.exp insn_test_dblalign6_X0.stderr.exp \
	 insn_test_dblalign6_X0.vgtest \
	insn_test_dblalign6_X1.stdout.exp insn_test_dblalign6_X1.stderr.exp \
	 insn_test_dblalign6_X1.vgtest \
	insn_test_dtlbpr_X1.stdout.exp insn_test_dtlbpr_X1.stderr.exp \
	 insn_test_dtlbpr_X1.vgtest \
	insn_test_fdouble_add_flags_X0.stdout.exp \
	 insn_test_fdouble_add_flags_X0.stderr.exp \
	 insn_test_fdouble_add_flags_X0.vgtest \
	insn_test_fdouble_addsub_X0.stdout.exp \
	 insn_test_fdouble_addsub_X0.stderr.exp \
	 insn_test_fdouble_addsub_X0.vgtest \
	insn_test_fdouble_mul_flags_X0.stdout.exp \
	 insn_test_fdouble_mul_flags_X0.stderr.exp \
	 insn_test_fdouble_mul_flags_X0.vgtest \
	insn_test_fdouble_pack1_X0.stdout.exp \
	 insn_test_fdouble_pack1_X0.stderr.exp \
	 insn_test_fdouble_pack1_X0.vgtest \
	insn_test_fdouble_pack2_X0.stdout.exp \
	 insn_test_fdouble_pack2_X0.stderr.exp \
	 insn_test_fdouble_pack2_X0.vgtest \
	insn_test_fdouble_sub_flags_X0.stdout.exp \
	 insn_test_fdouble_sub_flags_X0.stderr.exp \
	 insn_test_fdouble_sub_flags_X0.vgtest \
	insn_test_fdouble_unpack_max_X0.stdout.exp \
	 insn_test_fdouble_unpack_max_X0.stderr.exp \
	 insn_test_fdouble_unpack_max_X0.vgtest \
	insn_test_fdouble_unpack_min_X0.stdout.exp \
	 insn_test_fdouble_unpack_min_X0.stderr.exp \
	 insn_test_fdouble_unpack_min_X0.vgtest \
	insn_test_flushwb_X1.stdout.exp insn_test_flushwb_X1.stderr.exp \
	 insn_test_flushwb_X1.vgtest \
	insn_test_fnop_X0.stdout.exp insn_test_fnop_X0.stderr.exp \
	 insn_test_fnop_X0.vgtest \
	insn_test_fnop_X1.stdout.exp insn_test_fnop_X1.stderr.exp \
	 insn_test_fnop_X1.vgtest \
	insn_test_fnop_Y0.stdout.exp insn_test_fnop_Y0.stderr.exp \
	 insn_test_fnop_Y0.vgtest \
	insn_test_fnop_Y1.stdout.exp insn_test_fnop_Y1.stderr.exp \
	 insn_test_fnop_Y1.vgtest \
	insn_test_fsingle_add1_X0.stdout.exp \
	 insn_test_fsingle_add1_X0.stderr.exp \
	 insn_test_fsingle_add1_X0.vgtest \
	insn_test_fsingle_addsub2_X0.stdout.exp \
	 insn_test_fsingle_addsub2_X0.stderr.exp \
	 insn_test_fsingle_addsub2_X0.vgtest \
	insn_test_fsingle_mul1_X0.stdout.exp \
	 insn_test_fsingle_mul1_X0.stderr.exp \
	 insn_test_fsingle_mul1_X0.vgtest \
	insn_test_fsingle_mul2_X0.stdout.exp \
	 insn_test_fsingle_mul2_X0.stderr.exp \
	 insn_test_fsingle_mul2_X0.vgtest \
	insn_test_fsingle_pack1_X0.stdout.exp \
	 insn_test_fsingle_pack1_X0.stderr.exp \
	 insn_test_fsingle_pack1_X0.vgtest \
	insn_test_fsingle_pack1_Y0.stdout.exp \
	 insn_test_fsingle_pack1_Y0.stderr.exp \
	 insn_test_fsingle_pack1_Y0.vgtest \
	insn_test_fsingle_pack2_X0.stdout.exp \
	 insn_test_fsingle_pack2_X0.stderr.exp \
	 insn_test_fsingle_pack2_X0.vgtest \
	insn_test_fsingle_sub1_X0.stdout.exp \
	 insn_test_fsingle_sub1_X0.stderr.exp \
	 insn_test_fsingle_sub1_X0.vgtest \
	insn_test_icoh_X1.stdout.exp insn_test_icoh_X1.stderr.exp \
	 insn_test_icoh_X1.vgtest \
	insn_test_j_X1.stdout.exp insn_test_j_X1.stderr.exp \
	 insn_test_j_X1.vgtest \
	insn_test_jal_X1.stdout.exp insn_test_jal_X1.stderr.exp \
	 insn_test_jal_X1.vgtest \
	insn_test_jalr_X1.stdout.exp insn_test_jalr_X1.stderr.exp \
	 insn_test_jalr_X1.vgtest \
	insn_test_jalr_Y1.stdout.exp insn_test_jalr_Y1.stderr.exp \
	 insn_test_jalr_Y1.vgtest \
	insn_test_jalrp_X1.stdout.exp insn_test_jalrp_X1.stderr.exp \
	 insn_test_jalrp_X1.vgtest \
	insn_test_jalrp_Y1.stdout.exp insn_test_jalrp_Y1.stderr.exp \
	 insn_test_jalrp_Y1.vgtest \
	insn_test_jr_X1.stdout.exp insn_test_jr_X1.stderr.exp \
	 insn_test_jr_X1.vgtest \
	insn_test_jr_Y1.stdout.exp insn_test_jr_Y1.stderr.exp \
	 insn_test_jr_Y1.vgtest \
	insn_test_jrp_X1.stdout.exp insn_test_jrp_X1.stderr.exp \
	 insn_test_jrp_X1.vgtest \
	insn_test_jrp_Y1.stdout.exp insn_test_jrp_Y1.stderr.exp \
	 insn_test_jrp_Y1.vgtest \
	insn_test_ld_X1.stdout.exp insn_test_ld_X1.stderr.exp \
	 insn_test_ld_X1.vgtest \
	insn_test_ld_Y2.stdout.exp insn_test_ld_Y2.stderr.exp \
	 insn_test_ld_Y2.vgtest \
	insn_test_ld1s_X1.stdout.exp insn_test_ld1s_X1.stderr.exp \
	 insn_test_ld1s_X1.vgtest \
	insn_test_ld1s_Y2.stdout.exp insn_test_ld1s_Y2.stderr.exp \
	 insn_test_ld1s_Y2.vgtest \
	insn_test_ld1s_add_X1.stdout.exp insn_test_ld1s_add_X1.stderr.exp \
	 insn_test_ld1s_add_X1.vgtest \
	insn_test_ld1u_X1.stdout.exp insn_test_ld1u_X1.stderr.exp \
	 insn_test_ld1u_X1.vgtest \
	insn_test_ld1u_Y2.stdout.exp insn_test_ld1u_Y2.stderr.exp \
	 insn_test_ld1u_Y2.vgtest \
	insn_test_ld1u_add_X1.stdout.exp insn_test_ld1u_add_X1.stderr.exp \
	 insn_test_ld1u_add_X1.vgtest \
	insn_test_ld2s_X1.stdout.exp insn_test_ld2s_X1.stderr.exp \
	 insn_test_ld2s_X1.vgtest \
	insn_test_ld2s_Y2.stdout.exp insn_test_ld2s_Y2.stderr.exp \
	 insn_test_ld2s_Y2.vgtest \
	insn_test_ld2u_X1.stdout.exp insn_test_ld2u_X1.stderr.exp \
	 insn_test_ld2u_X1.vgtest \
	insn_test_ld2u_Y2.stdout.exp insn_test_ld2u_Y2.stderr.exp \
	 insn_test_ld2u_Y2.vgtest \
	insn_test_ld4s_X1.stdout.exp insn_test_ld4s_X1.stderr.exp \
	 insn_test_ld4s_X1.vgtest \
	insn_test_ld4s_add_X1.stdout.exp insn_test_ld4s_add_X1.stderr.exp \
	 insn_test_ld4s_add_X1.vgtest \
	insn_test_ld4u_X1.stdout.exp insn_test_ld4u_X1.stderr.exp \
	 insn_test_ld4u_X1.vgtest \
	insn_test_ld4u_Y2.stdout.exp insn_test_ld4u_Y2.stderr.exp \
	 insn_test_ld4u_Y2.vgtest \
	insn_test_ld4u_add_X1.stdout.exp insn_test_ld4u_add_X1.stderr.exp \
	 insn_test_ld4u_add_X1.vgtest \
	insn_test_ld_add_X1.stdout.exp insn_test_ld_add_X1.stderr.exp \
	 insn_test_ld_add_X1.vgtest \
	insn_test_ldna_X1.stdout.exp insn_test_ldna_X1.stderr.exp \
	 insn_test_ldna_X1.vgtest \
	insn_test_ldna_add_X1.stdout.exp insn_test_ldna_add_X1.stderr.exp \
	 insn_test_ldna_add_X1.vgtest \
	insn_test_ldnt_X1.stdout.exp insn_test_ldnt_X1.stderr.exp \
	 insn_test_ldnt_X1.vgtest \
	insn_test_ldnt1s_X1.stdout.exp insn_test_ldnt1s_X1.stderr.exp \
	 insn_test_ldnt1s_X1.vgtest \
	insn_test_ldnt1s_add_X1.stdout.exp insn_test_ldnt1s_add_X1.stderr.exp \
	 insn_test_ldnt1s_add_X1.vgtest \
	insn_test_ldnt1u_X1.stdout.exp insn_test_ldnt1u_X1.stderr.exp \
	 insn_test_ldnt1u_X1.vgtest \
	insn_test_ldnt1u_add_X1.stdout.exp insn_test_ldnt1u_add_X1.stderr.exp \
	 insn_test_ldnt1u_add_X1.vgtest \
	insn_test_ldnt2s_X1.stdout.exp insn_test_ldnt2s_X1.stderr.exp \
	 insn_test_ldnt2s_X1.vgtest \
	insn_test_ldnt2s_add_X1.stdout.exp insn_test_ldnt2s_add_X1.stderr.exp \
	 insn_test_ldnt2s_add_X1.vgtest \
	insn_test_ldnt2u_add_X1.stdout.exp insn_test_ldnt2u_add_X1.stderr.exp \
	 insn_test_ldnt2u_add_X1.vgtest \
	insn_test_ldnt4s_X1.stdout.exp insn_test_ldnt4s_X1.stderr.exp \
	 insn_test_ldnt4s_X1.vgtest \
	insn_test_ldnt4s_add_X1.stdout.exp insn_test_ldnt4s_add_X1.stderr.exp \
	 insn_test_ldnt4s_add_X1.vgtest \
	insn_test_ldnt4u_X1.stdout.exp insn_test_ldnt4u_X1.stderr.exp \
	 insn_test_ldnt4u_X1.vgtest \
	insn_test_ldnt4u_add_X1.stdout.exp insn_test_ldnt4u_add_X1.stderr.exp \
	 insn_test_ldnt4u_add_X1.vgtest \
	insn_test_ldnt_add_X1.stdout.exp insn_test_ldnt_add_X1.stderr.exp \
	 insn_test_ldnt_add_X1.vgtest \
	insn_test_lnk_X1.stdout.exp insn_test_lnk_X1.stderr.exp \
	 insn_test_lnk_X1.vgtest \
	insn_test_lnk_Y1.stdout.exp insn_test_lnk_Y1.stderr.exp \
	 insn_test_lnk_Y1.vgtest \
	insn_test_mf_X1.stdout.exp insn_test_mf_X1.stderr.exp \
	 insn_test_mf_X1.vgtest \
	insn_test_mm_X0.stdout.exp insn_test_mm_X0.stderr.exp \
	 insn_test_mm_X0.vgtest \
	insn_test_mnz_X0.stdout.exp insn_test_mnz_X0.stderr.exp \
	 insn_test_mnz_X0.vgtest \
	insn_test_mnz_X1.stdout.exp insn_test_mnz_X1.stderr.exp \
	 insn_test_mnz_X1.vgtest \
	insn_test_mnz_Y0.stdout.exp insn_test_mnz_Y0.stderr.exp \
	 insn_test_mnz_Y0.vgtest \
	insn_test_mnz_Y1.stdout.exp insn_test_mnz_Y1.stderr.exp \
	 insn_test_mnz_Y1.vgtest \
	insn_test_mul_hs_hs_X0.stdout.exp insn_test_mul_hs_hs_X0.stderr.exp \
	 insn_test_mul_hs_hs_X0.vgtest \
	insn_test_mul_hs_hs_Y0.stdout.exp insn_test_mul_hs_hs_Y0.stderr.exp \
	 insn_test_mul_hs_hs_Y0.vgtest \
	insn_test_mul_hs_hu_X0.stdout.exp insn_test_mul_hs_hu_X0.stderr.exp \
	 insn_test_mul_hs_hu_X0.vgtest \
	insn_test_mul_hs_ls_X0.stdout.exp insn_test_mul_hs_ls_X0.stderr.exp \
	 insn_test_mul_hs_ls_X0.vgtest \
	insn_test_mul_hs_lu_X0.stdout.exp insn_test_mul_hs_lu_X0.stderr.exp \
	 insn_test_mul_hs_lu_X0.vgtest \
	insn_test_mul_hu_hu_X0.stdout.exp insn_test_mul_hu_hu_X0.stderr.exp \
	 insn_test_mul_hu_hu_X0.vgtest \
	insn_test_mul_hu_hu_Y0.stdout.exp insn_test_mul_hu_hu_Y0.stderr.exp \
	 insn_test_mul_hu_hu_Y0.vgtest \
	insn_test_mul_hu_lu_X0.stdout.exp insn_test_mul_hu_lu_X0.stderr.exp \
	 insn_test_mul_hu_lu_X0.vgtest \
	insn_test_mul_ls_ls_X0.stdout.exp insn_test_mul_ls_ls_X0.stderr.exp \
	 insn_test_mul_ls_ls_X0.vgtest \
	insn_test_mul_ls_ls_Y0.stdout.exp insn_test_mul_ls_ls_Y0.stderr.exp \
	 insn_test_mul_ls_ls_Y0.vgtest \
	insn_test_mul_ls_lu_X0.stdout.exp insn_test_mul_ls_lu_X0.stderr.exp \
	 insn_test_mul_ls_lu_X0.vgtest \
	insn_test_mul_lu_lu_X0.stdout.exp insn_test_mul_lu_lu_X0.stderr.exp \
	 insn_test_mul_lu_lu_X0.vgtest \
	insn_test_mul_lu_lu_Y0.stdout.exp insn_test_mul_lu_lu_Y0.stderr.exp \
	 insn_test_mul_lu_lu_Y0.vgtest \
	insn_test_mula_hs_hs_X0.stdout.exp insn_test_mula_hs_hs_X0.stderr.exp \
	 insn_test_mula_hs_hs_X0.vgtest \
	insn_test_mula_hs_hs_Y0.stdout.exp insn_test_mula_hs_hs_Y0.stderr.exp \
	 insn_test_mula_hs_hs_Y0.vgtest \
	insn_test_mula_hs_hu_X0.stdout.exp insn_test_mula_hs_hu_X0.stderr.exp \
	 insn_test_mula_hs_hu_X0.vgtest \
	insn_test_mula_hs_ls_X0.stdout.exp insn_test_mula_hs_ls_X0.stderr.exp \
	 insn_test_mula_hs_ls_X0.vgtest \
	insn_test_mula_hs_lu_X0.stdout.exp insn_test_mula_hs_lu_X0.stderr.exp \
	 insn_test_mula_hs_lu_X0.vgtest \
	insn_test_mula_hu_hu_X0.stdout.exp insn_test_mula_hu_hu_X0.stderr.exp \
	 insn_test_mula_hu_hu_X0.vgtest \
	insn_test_mula_hu_hu_Y0.stdout.exp insn_test_mula_hu_hu_Y0.stderr.exp \
	 insn_test_mula_hu_hu_Y0.vgtest \
	insn_test_mula_hu_ls_X0.stdout.exp insn_test_mula_hu_ls_X0.stderr.exp \
	 insn_test_mula_hu_ls_X0.vgtest \
	insn_test_mula_hu_lu_X0.stdout.exp insn_test_mula_hu_lu_X0.stderr.exp \
	 insn_test_mula_hu_lu_X0.vgtest \
	insn_test_mula_ls_ls_X0.stdout.exp insn_test_mula_ls_ls_X0.stderr.exp \
	 insn_test_mula_ls_ls_X0.vgtest \
	insn_test_mula_ls_ls_Y0.stdout.exp insn_test_mula_ls_ls_Y0.stderr.exp \
	 insn_test_mula_ls_ls_Y0.vgtest \
	insn_test_mula_ls_lu_X0.stdout.exp insn_test_mula_ls_lu_X0.stderr.exp \
	 insn_test_mula_ls_lu_X0.vgtest \
	insn_test_mula_lu_lu_X0.stdout.exp insn_test_mula_lu_lu_X0.stderr.exp \
	 insn_test_mula_lu_lu_X0.vgtest \
	insn_test_mula_lu_lu_Y0.stdout.exp insn_test_mula_lu_lu_Y0.stderr.exp \
	 insn_test_mula_lu_lu_Y0.vgtest \
	insn_test_mulax_X0.stdout.exp insn_test_mulax_X0.stderr.exp \
	 insn_test_mulax_X0.vgtest \
	insn_test_mulax_Y0.stdout.exp insn_test_mulax_Y0.stderr.exp \
	 insn_test_mulax_Y0.vgtest \
	insn_test_mulx_X0.stdout.exp insn_test_mulx_X0.stderr.exp \
	 insn_test_mulx_X0.vgtest \
	insn_test_mulx_Y0.stdout.exp insn_test_mulx_Y0.stderr.exp \
	 insn_test_mulx_Y0.vgtest \
	insn_test_mz_X0.stdout.exp insn_test_mz_X0.stderr.exp \
	 insn_test_mz_X0.vgtest \
	insn_test_mz_X1.stdout.exp insn_test_mz_X1.stderr.exp \
	 insn_test_mz_X1.vgtest \
	insn_test_mz_Y0.stdout.exp insn_test_mz_Y0.stderr.exp \
	 insn_test_mz_Y0.vgtest \
	insn_test_mz_Y1.stdout.exp insn_test_mz_Y1.stderr.exp \
	 insn_test_mz_Y1.vgtest \
	insn_test_nop_X0.stdout.exp insn_test_nop_X0.stderr.exp \
	 insn_test_nop_X0.vgtest \
	insn_test_nop_X1.stdout.exp insn_test_nop_X1.stderr.exp \
	 insn_test_nop_X1.vgtest \
	insn_test_nop_Y0.stdout.exp insn_test_nop_Y0.stderr.exp \
	 insn_test_nop_Y0.vgtest \
	insn_test_nop_Y1.stdout.exp insn_test_nop_Y1.stderr.exp \
	 insn_test_nop_Y1.vgtest \
	insn_test_nor_X0.stdout.exp insn_test_nor_X0.stderr.exp \
	 insn_test_nor_X0.vgtest \
	insn_test_nor_X1.stdout.exp insn_test_nor_X1.stderr.exp \
	 insn_test_nor_X1.vgtest \
	insn_test_nor_Y0.stdout.exp insn_test_nor_Y0.stderr.exp \
	 insn_test_nor_Y0.vgtest \
	insn_test_nor_Y1.stdout.exp insn_test_nor_Y1.stderr.exp \
	 insn_test_nor_Y1.vgtest \
	insn_test_or_X0.stdout.exp insn_test_or_X0.stderr.exp \
	 insn_test_or_X0.vgtest \
	insn_test_or_X1.stdout.exp insn_test_or_X1.stderr.exp \
	 insn_test_or_X1.vgtest \
	insn_test_or_Y0.stdout.exp insn_test_or_Y0.stderr.exp \
	 insn_test_or_Y0.vgtest \
	insn_test_or_Y1.stdout.exp insn_test_or_Y1.stderr.exp \
	 insn_test_or_Y1.vgtest \
	insn_test_ori_X0.stdout.exp insn_test_ori_X0.stderr.exp \
	 insn_test_ori_X0.vgtest \
	insn_test_ori_X1.stdout.exp insn_test_ori_X1.stderr.exp \
	 insn_test_ori_X1.vgtest \
	insn_test_pcnt_X0.stdout.exp insn_test_pcnt_X0.stderr.exp \
	 insn_test_pcnt_X0.vgtest \
	insn_test_pcnt_Y0.stdout.exp insn_test_pcnt_Y0.stderr.exp \
	 insn_test_pcnt_Y0.vgtest \
	insn_test_revbits_X0.stdout.exp insn_test_revbits_X0.stderr.exp \
	 insn_test_revbits_X0.vgtest \
	insn_test_revbits_Y0.stdout.exp insn_test_revbits_Y0.stderr.exp \
	 insn_test_revbits_Y0.vgtest \
	insn_test_revbytes_X0.stdout.exp insn_test_revbytes_X0.stderr.exp \
	 insn_test_revbytes_X0.vgtest \
	insn_test_revbytes_Y0.stdout.exp insn_test_revbytes_Y0.stderr.exp \
	 insn_test_revbytes_Y0.vgtest \
	insn_test_rotl_X0.stdout.exp insn_test_rotl_X0.stderr.exp \
	 insn_test_rotl_X0.vgtest \
	insn_test_rotl_X1.stdout.exp insn_test_rotl_X1.stderr.exp \
	 insn_test_rotl_X1.vgtest \
	insn_test_rotl_Y0.stdout.exp insn_test_rotl_Y0.stderr.exp \
	 insn_test_rotl_Y0.vgtest \
	insn_test_rotl_Y1.stdout.exp insn_test_rotl_Y1.stderr.exp \
	 insn_test_rotl_Y1.vgtest \
	insn_test_rotli_X0.stdout.exp insn_test_rotli_X0.stderr.exp \
	 insn_test_rotli_X0.vgtest \
	insn_test_rotli_X1.stdout.exp insn_test_rotli_X1.stderr.exp \
	 insn_test_rotli_X1.vgtest \
	insn_test_rotli_Y0.stdout.exp insn_test_rotli_Y0.stderr.exp \
	 insn_test_rotli_Y0.vgtest \
	insn_test_rotli_Y1.stdout.exp insn_test_rotli_Y1.stderr.exp \
	 insn_test_rotli_Y1.vgtest \
	insn_test_shl_X0.stdout.exp insn_test_shl_X0.stderr.exp \
	 insn_test_shl_X0.vgtest \
	insn_test_shl_X1.stdout.exp insn_test_shl_X1.stderr.exp \
	 insn_test_shl_X1.vgtest \
	insn_test_shl_Y0.stdout.exp insn_test_shl_Y0.stderr.exp \
	 insn_test_shl_Y0.vgtest \
	insn_test_shl_Y1.stdout.exp insn_test_shl_Y1.stderr.exp \
	 insn_test_shl_Y1.vgtest \
	insn_test_shl16insli_X0.stdout.exp insn_test_shl16insli_X0.stderr.exp \
	 insn_test_shl16insli_X0.vgtest \
	insn_test_shl16insli_X1.stdout.exp insn_test_shl16insli_X1.stderr.exp \
	 insn_test_shl16insli_X1.vgtest \
	insn_test_shl1add_X0.stdout.exp insn_test_shl1add_X0.stderr.exp \
	 insn_test_shl1add_X0.vgtest \
	insn_test_shl1add_X1.stdout.exp insn_test_shl1add_X1.stderr.exp \
	 insn_test_shl1add_X1.vgtest \
	insn_test_shl1add_Y0.stdout.exp insn_test_shl1add_Y0.stderr.exp \
	 insn_test_shl1add_Y0.vgtest \
	insn_test_shl1add_Y1.stdout.exp insn_test_shl1add_Y1.stderr.exp \
	 insn_test_shl1add_Y1.vgtest \
	insn_test_shl1addx_X0.stdout.exp insn_test_shl1addx_X0.stderr.exp \
	 insn_test_shl1addx_X0.vgtest \
	insn_test_shl1addx_X1.stdout.exp insn_test_shl1addx_X1.stderr.exp \
	 insn_test_shl1addx_X1.vgtest \
	insn_test_shl1addx_Y0.stdout.exp insn_test_shl1addx_Y0.stderr.exp \
	 insn_test_shl1addx_Y0.vgtest \
	insn_test_shl1addx_Y1.stdout.exp insn_test_shl1addx_Y1.stderr.exp \
	 insn_test_shl1addx_Y1.vgtest \
	insn_test_shl2add_X0.stdout.exp insn_test_shl2add_X0.stderr.exp \
	 insn_test_shl2add_X0.vgtest \
	insn_test_shl2add_X1.stdout.exp insn_test_shl2add_X1.stderr.exp \
	 insn_test_shl2add_X1.vgtest \
	insn_test_shl2add_Y0.stdout.exp insn_test_shl2add_Y0.stderr.exp \
	 insn_test_shl2add_Y0.vgtest \
	insn_test_shl2add_Y1.stdout.exp insn_test_shl2add_Y1.stderr.exp \
	 insn_test_shl2add_Y1.vgtest \
	insn_test_shl2addx_X0.stdout.exp insn_test_shl2addx_X0.stderr.exp \
	 insn_test_shl2addx_X0.vgtest \
	insn_test_shl2addx_X1.stdout.exp insn_test_shl2addx_X1.stderr.exp \
	 insn_test_shl2addx_X1.vgtest \
	insn_test_shl2addx_Y0.stdout.exp insn_test_shl2addx_Y0.stderr.exp \
	 insn_test_shl2addx_Y0.vgtest \
	insn_test_shl2addx_Y1.stdout.exp insn_test_shl2addx_Y1.stderr.exp \
	 insn_test_shl2addx_Y1.vgtest \
	insn_test_shl3add_X0.stdout.exp insn_test_shl3add_X0.stderr.exp \
	 insn_test_shl3add_X0.vgtest \
	insn_test_shl3add_X1.stdout.exp insn_test_shl3add_X1.stderr.exp \
	 insn_test_shl3add_X1.vgtest \
	insn_test_shl3add_Y0.stdout.exp insn_test_shl3add_Y0.stderr.exp \
	 insn_test_shl3add_Y0.vgtest \
	insn_test_shl3add_Y1.stdout.exp insn_test_shl3add_Y1.stderr.exp \
	 insn_test_shl3add_Y1.vgtest \
	insn_test_shl3addx_X0.stdout.exp insn_test_shl3addx_X0.stderr.exp \
	 insn_test_shl3addx_X0.vgtest \
	insn_test_shl3addx_X1.stdout.exp insn_test_shl3addx_X1.stderr.exp \
	 insn_test_shl3addx_X1.vgtest \
	insn_test_shl3addx_Y0.stdout.exp insn_test_shl3addx_Y0.stderr.exp \
	 insn_test_shl3addx_Y0.vgtest \
	insn_test_shl3addx_Y1.stdout.exp insn_test_shl3addx_Y1.stderr.exp \
	 insn_test_shl3addx_Y1.vgtest \
	insn_test_shli_X0.stdout.exp insn_test_shli_X0.stderr.exp \
	 insn_test_shli_X0.vgtest \
	insn_test_shli_X1.stdout.exp insn_test_shli_X1.stderr.exp \
	 insn_test_shli_X1.vgtest \
	insn_test_shli_Y0.stdout.exp insn_test_shli_Y0.stderr.exp \
	 insn_test_shli_Y0.vgtest \
	insn_test_shli_Y1.stdout.exp insn_test_shli_Y1.stderr.exp \
	 insn_test_shli_Y1.vgtest \
	insn_test_shlx_X0.stdout.exp insn_test_shlx_X0.stderr.exp \
	 insn_test_shlx_X0.vgtest \
	insn_test_shlx_X1.stdout.exp insn_test_shlx_X1.stderr.exp \
	 insn_test_shlx_X1.vgtest \
	insn_test_shlxi_X0.stdout.exp insn_test_shlxi_X0.stderr.exp \
	 insn_test_shlxi_X0.vgtest \
	insn_test_shlxi_X1.stdout.exp insn_test_shlxi_X1.stderr.exp \
	 insn_test_shlxi_X1.vgtest \
	insn_test_shrs_X0.stdout.exp insn_test_shrs_X0.stderr.exp \
	 insn_test_shrs_X0.vgtest \
	insn_test_shrs_X1.stdout.exp insn_test_shrs_X1.stderr.exp \
	 insn_test_shrs_X1.vgtest \
	insn_test_shrs_Y0.stdout.exp insn_test_shrs_Y0.stderr.exp \
	 insn_test_shrs_Y0.vgtest \
	insn_test_shrs_Y1.stdout.exp insn_test_shrs_Y1.stderr.exp \
	 insn_test_shrs_Y1.vgtest \
	insn_test_shrsi_X0.stdout.exp insn_test_shrsi_X0.stderr.exp \
	 insn_test_shrsi_X0.vgtest \
	insn_test_shrsi_X1.stdout.exp insn_test_shrsi_X1.stderr.exp \
	 insn_test_shrsi_X1.vgtest \
	insn_test_shrsi_Y0.stdout.exp insn_test_shrsi_Y0.stderr.exp \
	 insn_test_shrsi_Y0.vgtest \
	insn_test_shrsi_Y1.stdout.exp insn_test_shrsi_Y1.stderr.exp \
	 insn_test_shrsi_Y1.vgtest \
	insn_test_shru_X0.stdout.exp insn_test_shru_X0.stderr.exp \
	 insn_test_shru_X0.vgtest \
	insn_test_shru_X1.stdout.exp insn_test_shru_X1.stderr.exp \
	 insn_test_shru_X1.vgtest \
	insn_test_shru_Y0.stdout.exp insn_test_shru_Y0.stderr.exp \
	 insn_test_shru_Y0.vgtest \
	insn_test_shru_Y1.stdout.exp insn_test_shru_Y1.stderr.exp \
	 insn_test_shru_Y1.vgtest \
	insn_test_shrui_X0.stdout.exp insn_test_shrui_X0.stderr.exp \
	 insn_test_shrui_X0.vgtest \
	insn_test_shrui_X1.stdout.exp insn_test_shrui_X1.stderr.exp \
	 insn_test_shrui_X1.vgtest \
	insn_test_shrui_Y0.stdout.exp insn_test_shrui_Y0.stderr.exp \
	 insn_test_shrui_Y0.vgtest \
	insn_test_shrui_Y1.stdout.exp insn_test_shrui_Y1.stderr.exp \
	 insn_test_shrui_Y1.vgtest \
	insn_test_shrux_X0.stdout.exp insn_test_shrux_X0.stderr.exp \
	 insn_test_shrux_X0.vgtest \
	insn_test_shrux_X1.stdout.exp insn_test_shrux_X1.stderr.exp \
	 insn_test_shrux_X1.vgtest \
	insn_test_shufflebytes_X0.stdout.exp \
	 insn_test_shufflebytes_X0.stderr.exp \
	 insn_test_shufflebytes_X0.vgtest \
	insn_test_st_X1.stdout.exp insn_test_st_X1.stderr.exp \
	 insn_test_st_X1.vgtest \
	insn_test_st_Y2.stdout.exp insn_test_st_Y2.stderr.exp \
	 insn_test_st_Y2.vgtest \
	insn_test_st1_X1.stdout.exp insn_test_st1_X1.stderr.exp \
	 insn_test_st1_X1.vgtest \
	insn_test_st1_Y2.stdout.exp insn_test_st1_Y2.stderr.exp \
	 insn_test_st1_Y2.vgtest \
	insn_test_st1_add_X1.stdout.exp insn_test_st1_add_X1.stderr.exp \
	 insn_test_st1_add_X1.vgtest \
	insn_test_st2_X1.stdout.exp insn_test_st2_X1.stderr.exp \
	 insn_test_st2_X1.vgtest \
	insn_test_st2_Y2.stdout.exp insn_test_st2_Y2.stderr.exp \
	 insn_test_st2_Y2.vgtest \
	insn_test_st2_add_X1.stdout.exp insn_test_st2_add_X1.stderr.exp \
	 insn_test_st2_add_X1.vgtest \
	insn_test_st4_X1.stdout.exp insn_test_st4_X1.stderr.exp \
	 insn_test_st4_X1.vgtest \
	insn_test_st4_Y2.stdout.exp insn_test_st4_Y2.stderr.exp \
	 insn_test_st4_Y2.vgtest \
	insn_test_st4_add_X1.stdout.exp insn_test_st4_add_X1.stderr.exp \
	 insn_test_st4_add_X1.vgtest \
	insn_test_st_add_X1.stdout.exp insn_test_st_add_X1.stderr.exp \
	 insn_test_st_add_X1.vgtest \
	insn_test_stnt_X1.stdout.exp insn_test_stnt_X1.stderr.exp \
	 insn_test_stnt_X1.vgtest \
	insn_test_stnt1_X1.stdout.exp insn_test_stnt1_X1.stderr.exp \
	 insn_test_stnt1_X1.vgtest \
	insn_test_stnt2_X1.stdout.exp insn_test_stnt2_X1.stderr.exp \
	 insn_test_stnt2_X1.vgtest \
	insn_test_stnt2_add_X1.stdout.exp insn_test_stnt2_add_X1.stderr.exp \
	 insn_test_stnt2_add_X1.vgtest \
	insn_test_stnt4_X1.stdout.exp insn_test_stnt4_X1.stderr.exp \
	 insn_test_stnt4_X1.vgtest \
	insn_test_stnt4_add_X1.stdout.exp insn_test_stnt4_add_X1.stderr.exp \
	 insn_test_stnt4_add_X1.vgtest \
	insn_test_stnt_add_X1.stdout.exp insn_test_stnt_add_X1.stderr.exp \
	 insn_test_stnt_add_X1.vgtest \
	insn_test_sub_X0.stdout.exp insn_test_sub_X0.stderr.exp \
	 insn_test_sub_X0.vgtest \
	insn_test_sub_X1.stdout.exp insn_test_sub_X1.stderr.exp \
	 insn_test_sub_X1.vgtest \
	insn_test_sub_Y0.stdout.exp insn_test_sub_Y0.stderr.exp \
	 insn_test_sub_Y0.vgtest \
	insn_test_sub_Y1.stdout.exp insn_test_sub_Y1.stderr.exp \
	 insn_test_sub_Y1.vgtest \
	insn_test_subx_X0.stdout.exp insn_test_subx_X0.stderr.exp \
	 insn_test_subx_X0.vgtest \
	insn_test_subx_X1.stdout.exp insn_test_subx_X1.stderr.exp \
	 insn_test_subx_X1.vgtest \
	insn_test_subx_Y0.stdout.exp insn_test_subx_Y0.stderr.exp \
	 insn_test_subx_Y0.vgtest \
	insn_test_subx_Y1.stdout.exp insn_test_subx_Y1.stderr.exp \
	 insn_test_subx_Y1.vgtest \
	insn_test_tblidxb0_X0.stdout.exp insn_test_tblidxb0_X0.stderr.exp \
	 insn_test_tblidxb0_X0.vgtest \
	insn_test_tblidxb0_Y0.stdout.exp insn_test_tblidxb0_Y0.stderr.exp \
	 insn_test_tblidxb0_Y0.vgtest \
	insn_test_tblidxb1_X0.stdout.exp insn_test_tblidxb1_X0.stderr.exp \
	 insn_test_tblidxb1_X0.vgtest \
	insn_test_tblidxb1_Y0.stdout.exp insn_test_tblidxb1_Y0.stderr.exp \
	 insn_test_tblidxb1_Y0.vgtest \
	insn_test_tblidxb2_X0.stdout.exp insn_test_tblidxb2_X0.stderr.exp \
	 insn_test_tblidxb2_X0.vgtest \
	insn_test_tblidxb2_Y0.stdout.exp insn_test_tblidxb2_Y0.stderr.exp \
	 insn_test_tblidxb2_Y0.vgtest \
	insn_test_tblidxb3_X0.stdout.exp insn_test_tblidxb3_X0.stderr.exp \
	 insn_test_tblidxb3_X0.vgtest \
	insn_test_tblidxb3_Y0.stdout.exp insn_test_tblidxb3_Y0.stderr.exp \
	 insn_test_tblidxb3_Y0.vgtest \
	insn_test_v1add_X0.stdout.exp insn_test_v1add_X0.stderr.exp \
	 insn_test_v1add_X0.vgtest \
	insn_test_v1add_X1.stdout.exp insn_test_v1add_X1.stderr.exp \
	 insn_test_v1add_X1.vgtest \
	insn_test_v1adduc_X0.stdout.exp insn_test_v1adduc_X0.stderr.exp \
	 insn_test_v1adduc_X0.vgtest \
	insn_test_v1adduc_X1.stdout.exp insn_test_v1adduc_X1.stderr.exp \
	 insn_test_v1adduc_X1.vgtest \
	insn_test_v1adiffu_X0.stdout.exp insn_test_v1adiffu_X0.stderr.exp \
	 insn_test_v1adiffu_X0.vgtest \
	insn_test_v1avgu_X0.stdout.exp insn_test_v1avgu_X0.stderr.exp \
	 insn_test_v1avgu_X0.vgtest \
	insn_test_v1cmpeq_X0.stdout.exp insn_test_v1cmpeq_X0.stderr.exp \
	 insn_test_v1cmpeq_X0.vgtest \
	insn_test_v1cmpeq_X1.stdout.exp insn_test_v1cmpeq_X1.stderr.exp \
	 insn_test_v1cmpeq_X1.vgtest \
	insn_test_v1cmpeqi_X0.stdout.exp insn_test_v1cmpeqi_X0.stderr.exp \
	 insn_test_v1cmpeqi_X0.vgtest \
	insn_test_v1cmpeqi_X1.stdout.exp insn_test_v1cmpeqi_X1.stderr.exp \
	 insn_test_v1cmpeqi_X1.vgtest \
	insn_test_v1cmples_X0.stdout.exp insn_test_v1cmples_X0.stderr.exp \
	 insn_test_v1cmples_X0.vgtest \
	insn_test_v1cmples_X1.stdout.exp insn_test_v1cmples_X1.stderr.exp \
	 insn_test_v1cmples_X1.vgtest \
	insn_test_v1cmpleu_X0.stdout.exp insn_test_v1cmpleu_X0.stderr.exp \
	 insn_test_v1cmpleu_X0.vgtest \
	insn_test_v1cmpleu_X1.stdout.exp insn_test_v1cmpleu_X1.stderr.exp \
	 insn_test_v1cmpleu_X1.vgtest \
	insn_test_v1cmplts_X0.stdout.exp insn_test_v1cmplts_X0.stderr.exp \
	 insn_test_v1cmplts_X0.vgtest \
	insn_test_v1cmplts_X1.stdout.exp insn_test_v1cmplts_X1.stderr.exp \
	 insn_test_v1cmplts_X1.vgtest \
	insn_test_v1cmpltu_X0.stdout.exp insn_test_v1cmpltu_X0.stderr.exp \
	 insn_test_v1cmpltu_X0.vgtest \
	insn_test_v1cmpltu_X1.stdout.exp insn_test_v1cmpltu_X1.stderr.exp \
	 insn_test_v1cmpltu_X1.vgtest \
	insn_test_v1cmpne_X0.stdout.exp insn_test_v1cmpne_X0.stderr.exp \
	 insn_test_v1cmpne_X0.vgtest \
	insn_test_v1cmpne_X1.stdout.exp insn_test_v1cmpne_X1.stderr.exp \
	 insn_test_v1cmpne_X1.vgtest \
	insn_test_v1ddotpu_X0.stdout.exp insn_test_v1ddotpu_X0.stderr.exp \
	 insn_test_v1ddotpu_X0.vgtest \
	insn_test_v1ddotpua_X0.stdout.exp insn_test_v1ddotpua_X0.stderr.exp \
	 insn_test_v1ddotpua_X0.vgtest \
	insn_test_v1ddotpus_X0.stdout.exp insn_test_v1ddotpus_X0.stderr.exp \
	 insn_test_v1ddotpus_X0.vgtest \
	insn_test_v1ddotpusa_X0.stdout.exp insn_test_v1ddotpusa_X0.stderr.exp \
	 insn_test_v1ddotpusa_X0.vgtest \
	insn_test_v1dotp_X0.stdout.exp insn_test_v1dotp_X0.stderr.exp \
	 insn_test_v1dotp_X0.vgtest \
	insn_test_v1dotpa_X0.stdout.exp insn_test_v1dotpa_X0.stderr.exp \
	 insn_test_v1dotpa_X0.vgtest \
	insn_test_v1dotpu_X0.stdout.exp insn_test_v1dotpu_X0.stderr.exp \
	 insn_test_v1dotpu_X0.vgtest \
	insn_test_v1dotpua_X0.stdout.exp insn_test_v1dotpua_X0.stderr.exp \
	 insn_test_v1dotpua_X0.vgtest \
	insn_test_v1dotpus_X0.stdout.exp insn_test_v1dotpus_X0.stderr.exp \
	 insn_test_v1dotpus_X0.vgtest \
	insn_test_v1dotpusa_X0.stdout.exp insn_test_v1dotpusa_X0.stderr.exp \
	 insn_test_v1dotpusa_X0.vgtest \
	insn_test_v1int_h_X0.stdout.exp insn_test_v1int_h_X0.stderr.exp \
	 insn_test_v1int_h_X0.vgtest \
	insn_test_v1int_h_X1.stdout.exp insn_test_v1int_h_X1.stderr.exp \
	 insn_test_v1int_h_X1.vgtest \
	insn_test_v1int_l_X0.stdout.exp insn_test_v1int_l_X0.stderr.exp \
	 insn_test_v1int_l_X0.vgtest \
	insn_test_v1int_l_X1.stdout.exp insn_test_v1int_l_X1.stderr.exp \
	 insn_test_v1int_l_X1.vgtest \
	insn_test_v1maxu_X0.stdout.exp insn_test_v1maxu_X0.stderr.exp \
	 insn_test_v1maxu_X0.vgtest \
	insn_test_v1maxu_X1.stdout.exp insn_test_v1maxu_X1.stderr.exp \
	 insn_test_v1maxu_X1.vgtest \
	insn_test_v1minu_X0.stdout.exp insn_test_v1minu_X0.stderr.exp \
	 insn_test_v1minu_X0.vgtest \
	insn_test_v1minu_X1.stdout.exp insn_test_v1minu_X1.stderr.exp \
	 insn_test_v1minu_X1.vgtest \
	insn_test_v1mnz_X0.stdout.exp insn_test_v1mnz_X0.stderr.exp \
	 insn_test_v1mnz_X0.vgtest \
	insn_test_v1mnz_X1.stdout.exp insn_test_v1mnz_X1.stderr.exp \
	 insn_test_v1mnz_X1.vgtest \
	insn_test_v1multu_X0.stdout.exp insn_test_v1multu_X0.stderr.exp \
	 insn_test_v1multu_X0.vgtest \
	insn_test_v1mulu_X0.stdout.exp insn_test_v1mulu_X0.stderr.exp \
	 insn_test_v1mulu_X0.vgtest \
	insn_test_v1mulus_X0.stdout.exp insn_test_v1mulus_X0.stderr.exp \
	 insn_test_v1mulus_X0.vgtest \
	insn_test_v1mz_X0.stdout.exp insn_test_v1mz_X0.stderr.exp \
	 insn_test_v1mz_X0.vgtest \
	insn_test_v1mz_X1.stdout.exp insn_test_v1mz_X1.stderr.exp \
	 insn_test_v1mz_X1.vgtest \
	insn_test_v1sadau_X0.stdout.exp insn_test_v1sadau_X0.stderr.exp \
	 insn_test_v1sadau_X0.vgtest \
	insn_test_v1sadu_X0.stdout.exp insn_test_v1sadu_X0.stderr.exp \
	 insn_test_v1sadu_X0.vgtest \
	insn_test_v1shl_X0.stdout.exp insn_test_v1shl_X0.stderr.exp \
	 insn_test_v1shl_X0.vgtest \
	insn_test_v1shl_X1.stdout.exp insn_test_v1shl_X1.stderr.exp \
	 insn_test_v1shl_X1.vgtest \
	insn_test_v1shli_X0.stdout.exp insn_test_v1shli_X0.stderr.exp \
	 insn_test_v1shli_X0.vgtest \
	insn_test_v1shli_X1.stdout.exp insn_test_v1shli_X1.stderr.exp \
	 insn_test_v1shli_X1.vgtest \
	insn_test_v1shrs_X0.stdout.exp insn_test_v1shrs_X0.stderr.exp \
	 insn_test_v1shrs_X0.vgtest \
	insn_test_v1shrs_X1.stdout.exp insn_test_v1shrs_X1.stderr.exp \
	 insn_test_v1shrs_X1.vgtest \
	insn_test_v1shrsi_X0.stdout.exp insn_test_v1shrsi_X0.stderr.exp \
	 insn_test_v1shrsi_X0.vgtest \
	insn_test_v1shrsi_X1.stdout.exp insn_test_v1shrsi_X1.stderr.exp \
	 insn_test_v1shrsi_X1.vgtest \
	insn_test_v1shru_X0.stdout.exp insn_test_v1shru_X0.stderr.exp \
	 insn_test_v1shru_X0.vgtest \
	insn_test_v1shru_X1.stdout.exp insn_test_v1shru_X1.stderr.exp \
	 insn_test_v1shru_X1.vgtest \
	insn_test_v1shrui_X0.stdout.exp insn_test_v1shrui_X0.stderr.exp \
	 insn_test_v1shrui_X0.vgtest \
	insn_test_v1shrui_X1.stdout.exp insn_test_v1shrui_X1.stderr.exp \
	 insn_test_v1shrui_X1.vgtest \
	insn_test_v1sub_X0.stdout.exp insn_test_v1sub_X0.stderr.exp \
	 insn_test_v1sub_X0.vgtest \
	insn_test_v1sub_X1.stdout.exp insn_test_v1sub_X1.stderr.exp \
	 insn_test_v1sub_X1.vgtest \
	insn_test_v1subuc_X0.stdout.exp insn_test_v1subuc_X0.stderr.exp \
	 insn_test_v1subuc_X0.vgtest \
	insn_test_v1subuc_X1.stdout.exp insn_test_v1subuc_X1.stderr.exp \
	 insn_test_v1subuc_X1.vgtest \
	insn_test_v2add_X0.stdout.exp insn_test_v2add_X0.stderr.exp \
	 insn_test_v2add_X0.vgtest \
	insn_test_v2add_X1.stdout.exp insn_test_v2add_X1.stderr.exp \
	 insn_test_v2add_X1.vgtest \
	insn_test_v2addsc_X0.stdout.exp insn_test_v2addsc_X0.stderr.exp \
	 insn_test_v2addsc_X0.vgtest \
	insn_test_v2addsc_X1.stdout.exp insn_test_v2addsc_X1.stderr.exp \
	 insn_test_v2addsc_X1.vgtest \
	insn_test_v2adiffs_X0.stdout.exp insn_test_v2adiffs_X0.stderr.exp \
	 insn_test_v2adiffs_X0.vgtest \
	insn_test_v2avgs_X0.stdout.exp insn_test_v2avgs_X0.stderr.exp \
	 insn_test_v2avgs_X0.vgtest \
	insn_test_v2cmpeq_X0.stdout.exp insn_test_v2cmpeq_X0.stderr.exp \
	 insn_test_v2cmpeq_X0.vgtest \
	insn_test_v2cmpeq_X1.stdout.exp insn_test_v2cmpeq_X1.stderr.exp \
	 insn_test_v2cmpeq_X1.vgtest \
	insn_test_v2cmpeqi_X0.stdout.exp insn_test_v2cmpeqi_X0.stderr.exp \
	 insn_test_v2cmpeqi_X0.vgtest \
	insn_test_v2cmpeqi_X1.stdout.exp insn_test_v2cmpeqi_X1.stderr.exp \
	 insn_test_v2cmpeqi_X1.vgtest \
	insn_test_v2cmples_X0.stdout.exp insn_test_v2cmples_X0.stderr.exp \
	 insn_test_v2cmples_X0.vgtest \
	insn_test_v2cmples_X1.stdout.exp insn_test_v2cmples_X1.stderr.exp \
	 insn_test_v2cmples_X1.vgtest \
	insn_test_v2cmpleu_X0.stdout.exp insn_test_v2cmpleu_X0.stderr.exp \
	 insn_test_v2cmpleu_X0.vgtest \
	insn_test_v2cmpleu_X1.stdout.exp insn_test_v2cmpleu_X1.stderr.exp \
	 insn_test_v2cmpleu_X1.vgtest \
	insn_test_v2cmplts_X0.stdout.exp insn_test_v2cmplts_X0.stderr.exp \
	 insn_test_v2cmplts_X0.vgtest \
	insn_test_v2cmplts_X1.stdout.exp insn_test_v2cmplts_X1.stderr.exp \
	 insn_test_v2cmplts_X1.vgtest \
	insn_test_v2cmpltsi_X0.stdout.exp insn_test_v2cmpltsi_X0.stderr.exp \
	 insn_test_v2cmpltsi_X0.vgtest \
	insn_test_v2cmpltsi_X1.stdout.exp insn_test_v2cmpltsi_X1.stderr.exp \
	 insn_test_v2cmpltsi_X1.vgtest \
	insn_test_v2cmpltu_X0.stdout.exp insn_test_v2cmpltu_X0.stderr.exp \
	 insn_test_v2cmpltu_X0.vgtest \
	insn_test_v2cmpltu_X1.stdout.exp insn_test_v2cmpltu_X1.stderr.exp \
	 insn_test_v2cmpltu_X1.vgtest \
	insn_test_v2cmpltui_X0.stdout.exp insn_test_v2cmpltui_X0.stderr.exp \
	 insn_test_v2cmpltui_X0.vgtest \
	insn_test_v2cmpltui_X1.stdout.exp insn_test_v2cmpltui_X1.stderr.exp \
	 insn_test_v2cmpltui_X1.vgtest \
	insn_test_v2cmpne_X0.stdout.exp insn_test_v2cmpne_X0.stderr.exp \
	 insn_test_v2cmpne_X0.vgtest \
	insn_test_v2cmpne_X1.stdout.exp insn_test_v2cmpne_X1.stderr.exp \
	 insn_test_v2cmpne_X1.vgtest \
	insn_test_v2dotp_X0.stdout.exp insn_test_v2dotp_X0.stderr.exp \
	 insn_test_v2dotp_X0.vgtest \
	insn_test_v2dotpa_X0.stdout.exp insn_test_v2dotpa_X0.stderr.exp \
	 insn_test_v2dotpa_X0.vgtest \
	insn_test_v2int_h_X0.stdout.exp insn_test_v2int_h_X0.stderr.exp \
	 insn_test_v2int_h_X0.vgtest \
	insn_test_v2int_h_X1.stdout.exp insn_test_v2int_h_X1.stderr.exp \
	 insn_test_v2int_h_X1.vgtest \
	insn_test_v2int_l_X0.stdout.exp insn_test_v2int_l_X0.stderr.exp \
	 insn_test_v2int_l_X0.vgtest \
	insn_test_v2int_l_X1.stdout.exp insn_test_v2int_l_X1.stderr.exp \
	 insn_test_v2int_l_X1.vgtest \
	insn_test_v2maxs_X0.stdout.exp insn_test_v2maxs_X0.stderr.exp \
	 insn_test_v2maxs_X0.vgtest \
	insn_test_v2maxs_X1.stdout.exp insn_test_v2maxs_X1.stderr.exp \
	 insn_test_v2maxs_X1.vgtest \
	insn_test_v2mins_X0.stdout.exp insn_test_v2mins_X0.stderr.exp \
	 insn_test_v2mins_X0.vgtest \
	insn_test_v2mins_X1.stdout.exp insn_test_v2mins_X1.stderr.exp \
	 insn_test_v2mins_X1.vgtest \
	insn_test_v2mnz_X0.stdout.exp insn_test_v2mnz_X0.stderr.exp \
	 insn_test_v2mnz_X0.vgtest \
	insn_test_v2mnz_X1.stdout.exp insn_test_v2mnz_X1.stderr.exp \
	 insn_test_v2mnz_X1.vgtest \
	insn_test_v2mulfsc_X0.stdout.exp insn_test_v2mulfsc_X0.stderr.exp \
	 insn_test_v2mulfsc_X0.vgtest \
	insn_test_v2muls_X0.stdout.exp insn_test_v2muls_X0.stderr.exp \
	 insn_test_v2muls_X0.vgtest \
	insn_test_v2mults_X0.stdout.exp insn_test_v2mults_X0.stderr.exp \
	 insn_test_v2mults_X0.vgtest \
	insn_test_v2mz_X0.stdout.exp insn_test_v2mz_X0.stderr.exp \
	 insn_test_v2mz_X0.vgtest \
	insn_test_v2mz_X1.stdout.exp insn_test_v2mz_X1.stderr.exp \
	 insn_test_v2mz_X1.vgtest \
	insn_test_v2packh_X0.stdout.exp insn_test_v2packh_X0.stderr.exp \
	 insn_test_v2packh_X0.vgtest \
	insn_test_v2packh_X1.stdout.exp insn_test_v2packh_X1.stderr.exp \
	 insn_test_v2packh_X1.vgtest \
	insn_test_v2packl_X0.stdout.exp insn_test_v2packl_X0.stderr.exp \
	 insn_test_v2packl_X0.vgtest \
	insn_test_v2packl_X1.stdout.exp insn_test_v2packl_X1.stderr.exp \
	 insn_test_v2packl_X1.vgtest \
	insn_test_v2packuc_X0.stdout.exp insn_test_v2packuc_X0.stderr.exp \
	 insn_test_v2packuc_X0.vgtest \
	insn_test_v2packuc_X1.stdout.exp insn_test_v2packuc_X1.stderr.exp \
	 insn_test_v2packuc_X1.vgtest \
	insn_test_v2sadas_X0.stdout.exp insn_test_v2sadas_X0.stderr.exp \
	 insn_test_v2sadas_X0.vgtest \
	insn_test_v2sadau_X0.stdout.exp insn_test_v2sadau_X0.stderr.exp \
	 insn_test_v2sadau_X0.vgtest \
	insn_test_v2sads_X0.stdout.exp insn_test_v2sads_X0.stderr.exp \
	 insn_test_v2sads_X0.vgtest \
	insn_test_v2sadu_X0.stdout.exp insn_test_v2sadu_X0.stderr.exp \
	 insn_test_v2sadu_X0.vgtest \
	insn_test_v2shl_X0.stdout.exp insn_test_v2shl_X0.stderr.exp \
	 insn_test_v2shl_X0.vgtest \
	insn_test_v2shl_X1.stdout.exp insn_test_v2shl_X1.stderr.exp \
	 insn_test_v2shl_X1.vgtest \
	insn_test_v2shli_X0.stdout.exp insn_test_v2shli_X0.stderr.exp \
	 insn_test_v2shli_X0.vgtest \
	insn_test_v2shli_X1.stdout.exp insn_test_v2shli_X1.stderr.exp \
	 insn_test_v2shli_X1.vgtest \
	insn_test_v2shlsc_X0.stdout.exp insn_test_v2shlsc_X0.stderr.exp \
	 insn_test_v2shlsc_X0.vgtest \
	insn_test_v2shlsc_X1.stdout.exp insn_test_v2shlsc_X1.stderr.exp \
	 insn_test_v2shlsc_X1.vgtest \
	insn_test_v2shrs_X0.stdout.exp insn_test_v2shrs_X0.stderr.exp \
	 insn_test_v2shrs_X0.vgtest \
	insn_test_v2shrs_X1.stdout.exp insn_test_v2shrs_X1.stderr.exp \
	 insn_test_v2shrs_X1.vgtest \
	insn_test_v2shrsi_X0.stdout.exp insn_test_v2shrsi_X0.stderr.exp \
	 insn_test_v2shrsi_X0.vgtest \
	insn_test_v2shrsi_X1.stdout.exp insn_test_v2shrsi_X1.stderr.exp \
	 insn_test_v2shrsi_X1.vgtest \
	insn_test_v2shru_X0.stdout.exp insn_test_v2shru_X0.stderr.exp \
	 insn_test_v2shru_X0.vgtest \
	insn_test_v2shru_X1.stdout.exp insn_test_v2shru_X1.stderr.exp \
	 insn_test_v2shru_X1.vgtest \
	insn_test_v2shrui_X0.stdout.exp insn_test_v2shrui_X0.stderr.exp \
	 insn_test_v2shrui_X0.vgtest \
	insn_test_v2shrui_X1.stdout.exp insn_test_v2shrui_X1.stderr.exp \
	 insn_test_v2shrui_X1.vgtest \
	insn_test_v2sub_X0.stdout.exp insn_test_v2sub_X0.stderr.exp \
	 insn_test_v2sub_X0.vgtest \
	insn_test_v2sub_X1.stdout.exp insn_test_v2sub_X1.stderr.exp \
	 insn_test_v2sub_X1.vgtest \
	insn_test_v2subsc_X0.stdout.exp insn_test_v2subsc_X0.stderr.exp \
	 insn_test_v2subsc_X0.vgtest \
	insn_test_v2subsc_X1.stdout.exp insn_test_v2subsc_X1.stderr.exp \
	 insn_test_v2subsc_X1.vgtest \
	insn_test_v4add_X0.stdout.exp insn_test_v4add_X0.stderr.exp \
	 insn_test_v4add_X0.vgtest \
	insn_test_v4add_X1.stdout.exp insn_test_v4add_X1.stderr.exp \
	 insn_test_v4add_X1.vgtest \
	insn_test_v4addsc_X0.stdout.exp insn_test_v4addsc_X0.stderr.exp \
	 insn_test_v4addsc_X0.vgtest \
	insn_test_v4addsc_X1.stdout.exp insn_test_v4addsc_X1.stderr.exp \
	 insn_test_v4addsc_X1.vgtest \
	insn_test_v4int_h_X0.stdout.exp insn_test_v4int_h_X0.stderr.exp \
	 insn_test_v4int_h_X0.vgtest \
	insn_test_v4int_h_X1.stdout.exp insn_test_v4int_h_X1.stderr.exp \
	 insn_test_v4int_h_X1.vgtest \
	insn_test_v4int_l_X0.stdout.exp insn_test_v4int_l_X0.stderr.exp \
	 insn_test_v4int_l_X0.vgtest \
	insn_test_v4int_l_X1.stdout.exp insn_test_v4int_l_X1.stderr.exp \
	 insn_test_v4int_l_X1.vgtest \
	insn_test_v4packsc_X0.stdout.exp insn_test_v4packsc_X0.stderr.exp \
	 insn_test_v4packsc_X0.vgtest \
	insn_test_v4packsc_X1.stdout.exp insn_test_v4packsc_X1.stderr.exp \
	 insn_test_v4packsc_X1.vgtest \
	insn_test_v4shl_X0.stdout.exp insn_test_v4shl_X0.stderr.exp \
	 insn_test_v4shl_X0.vgtest \
	insn_test_v4shl_X1.stdout.exp insn_test_v4shl_X1.stderr.exp \
	 insn_test_v4shl_X1.vgtest \
	insn_test_v4shlsc_X0.stdout.exp insn_test_v4shlsc_X0.stderr.exp \
	 insn_test_v4shlsc_X0.vgtest \
	insn_test_v4shlsc_X1.stdout.exp insn_test_v4shlsc_X1.stderr.exp \
	 insn_test_v4shlsc_X1.vgtest \
	insn_test_v4shrs_X0.stdout.exp insn_test_v4shrs_X0.stderr.exp \
	 insn_test_v4shrs_X0.vgtest \
	insn_test_v4shrs_X1.stdout.exp insn_test_v4shrs_X1.stderr.exp \
	 insn_test_v4shrs_X1.vgtest \
	insn_test_v4shru_X0.stdout.exp insn_test_v4shru_X0.stderr.exp \
	 insn_test_v4shru_X0.vgtest \
	insn_test_v4shru_X1.stdout.exp insn_test_v4shru_X1.stderr.exp \
	 insn_test_v4shru_X1.vgtest \
	insn_test_v4sub_X0.stdout.exp insn_test_v4sub_X0.stderr.exp \
	 insn_test_v4sub_X0.vgtest \
	insn_test_v4sub_X1.stdout.exp insn_test_v4sub_X1.stderr.exp \
	 insn_test_v4sub_X1.vgtest \
	insn_test_v4subsc_X0.stdout.exp insn_test_v4subsc_X0.stderr.exp \
	 insn_test_v4subsc_X0.vgtest \
	insn_test_v4subsc_X1.stdout.exp insn_test_v4subsc_X1.stderr.exp \
	 insn_test_v4subsc_X1.vgtest \
	insn_test_wh64_X1.stdout.exp insn_test_wh64_X1.stderr.exp \
	 insn_test_wh64_X1.vgtest \
	insn_test_xor_X0.stdout.exp insn_test_xor_X0.stderr.exp \
	 insn_test_xor_X0.vgtest \
	insn_test_xor_X1.stdout.exp insn_test_xor_X1.stderr.exp \
	 insn_test_xor_X1.vgtest \
	insn_test_xor_Y0.stdout.exp insn_test_xor_Y0.stderr.exp \
	 insn_test_xor_Y0.vgtest \
	insn_test_xor_Y1.stdout.exp insn_test_xor_Y1.stderr.exp \
	 insn_test_xor_Y1.vgtest \
	insn_test_xori_X0.stdout.exp insn_test_xori_X0.stderr.exp \
	 insn_test_xori_X0.vgtest \
	insn_test_xori_X1.stdout.exp insn_test_xori_X1.stderr.exp \
	 insn_test_xori_X1.vgtest
endif

bin_PROGRAMS  = gen_insn_test

insn_tests =

insn_tests += \
	insn_test_move_X0  \
	insn_test_move_X1  \
	insn_test_move_Y0  \
	insn_test_move_Y1  \
	insn_test_movei_X0  \
	insn_test_movei_X1  \
	insn_test_movei_Y0  \
	insn_test_movei_Y1  \
	insn_test_moveli_X0  \
	insn_test_moveli_X1  \
	insn_test_prefetch_X1  \
	insn_test_prefetch_Y2  \
	insn_test_prefetch_l1_X1  \
	insn_test_prefetch_l1_Y2  \
	insn_test_prefetch_l2_X1  \
	insn_test_prefetch_l2_Y2  \
	insn_test_prefetch_l3_X1  \
	insn_test_prefetch_l3_Y2  \
	insn_test_add_X0  \
	insn_test_add_X1  \
	insn_test_add_Y0  \
	insn_test_add_Y1  \
	insn_test_addi_X0  \
	insn_test_addi_X1  \
	insn_test_addi_Y0  \
	insn_test_addi_Y1  \
	insn_test_addli_X0  \
	insn_test_addli_X1  \
	insn_test_addx_X0  \
	insn_test_addx_X1  \
	insn_test_addx_Y0  \
	insn_test_addx_Y1  \
	insn_test_addxi_X0  \
	insn_test_addxi_X1  \
	insn_test_addxi_Y0  \
	insn_test_addxi_Y1  \
	insn_test_addxli_X0  \
	insn_test_addxli_X1  \
	insn_test_addxsc_X0  \
	insn_test_addxsc_X1  \
	insn_test_and_X0  \
	insn_test_and_X1  \
	insn_test_and_Y0  \
	insn_test_and_Y1  \
	insn_test_andi_X0  \
	insn_test_andi_X1  \
	insn_test_andi_Y0  \
	insn_test_andi_Y1  \
	insn_test_beqz_X1  \
	insn_test_beqzt_X1  \
	insn_test_bfexts_X0  \
	insn_test_bfextu_X0  \
	insn_test_bfins_X0  \
	insn_test_bgez_X1  \
	insn_test_bgezt_X1  \
	insn_test_bgtz_X1  \
	insn_test_bgtzt_X1  \
	insn_test_blbc_X1  \
	insn_test_blbct_X1  \
	insn_test_blbs_X1  \
	insn_test_blbst_X1  \
	insn_test_blez_X1  \
	insn_test_blezt_X1  \
	insn_test_bltz_X1  \
	insn_test_bltzt_X1  \
	insn_test_bnez_X1  \
	insn_test_bnezt_X1  \
	insn_test_clz_X0  \
	insn_test_clz_Y0  \
	insn_test_cmoveqz_X0  \
	insn_test_cmoveqz_Y0  \
	insn_test_cmovnez_X0  \
	insn_test_cmovnez_Y0  \
	insn_test_cmpeq_X0  \
	insn_test_cmpeq_X1  \
	insn_test_cmpeq_Y0  \
	insn_test_cmpeq_Y1  \
	insn_test_cmpeqi_X0  \
	insn_test_cmpeqi_X1  \
	insn_test_cmpeqi_Y0  \
	insn_test_cmpeqi_Y1  \
	insn_test_cmples_X0  \
	insn_test_cmples_X1  \
	insn_test_cmples_Y0  \
	insn_test_cmples_Y1  \
	insn_test_cmpleu_X0  \
	insn_test_cmpleu_X1  \
	insn_test_cmpleu_Y0  \
	insn_test_cmpleu_Y1  \
	insn_test_cmplts_X0  \
	insn_test_cmplts_X1  \
	insn_test_cmplts_Y0  \
	insn_test_cmplts_Y1  \
	insn_test_cmpltsi_X0  \
	insn_test_cmpltsi_X1  \
	insn_test_cmpltsi_Y0  \
	insn_test_cmpltsi_Y1  \
	insn_test_cmpltu_X0  \
	insn_test_cmpltu_X1  \
	insn_test_cmpltu_Y0  \
	insn_test_cmpltu_Y1  \
	insn_test_cmpltui_X0  \
	insn_test_cmpltui_X1  \
	insn_test_cmpne_X0  \
	insn_test_cmpne_X1  \
	insn_test_cmpne_Y0  \
	insn_test_cmpne_Y1  \
	insn_test_cmul_X0  \
	insn_test_cmula_X0  \
	insn_test_cmulaf_X0  \
	insn_test_cmulf_X0  \
	insn_test_cmulfr_X0  \
	insn_test_cmulh_X0  \
	insn_test_cmulhr_X0  \
	insn_test_crc32_32_X0  \
	insn_test_crc32_8_X0  \
	insn_test_ctz_X0  \
	insn_test_ctz_Y0  \
	insn_test_dblalign_X0  \
	insn_test_dblalign2_X0  \
	insn_test_dblalign2_X1  \
	insn_test_dblalign4_X0  \
	insn_test_dblalign4_X1  \
	insn_test_dblalign6_X0  \
	insn_test_dblalign6_X1  \
	insn_test_dtlbpr_X1  \
	insn_test_fdouble_add_flags_X0  \
	insn_test_fdouble_addsub_X0  \
	insn_test_fdouble_mul_flags_X0  \
	insn_test_fdouble_pack1_X0  \
	insn_test_fdouble_pack2_X0  \
	insn_test_fdouble_sub_flags_X0  \
	insn_test_fdouble_unpack_max_X0  \
	insn_test_fdouble_unpack_min_X0  \
	insn_test_flushwb_X1  \
	insn_test_fnop_X0  \
	insn_test_fnop_X1  \
	insn_test_fnop_Y0  \
	insn_test_fnop_Y1  \
	insn_test_fsingle_add1_X0  \
	insn_test_fsingle_addsub2_X0  \
	insn_test_fsingle_mul1_X0  \
	insn_test_fsingle_mul2_X0  \
	insn_test_fsingle_pack1_X0  \
	insn_test_fsingle_pack1_Y0  \
	insn_test_fsingle_pack2_X0  \
	insn_test_fsingle_sub1_X0  \
	insn_test_icoh_X1  \
	insn_test_j_X1  \
	insn_test_jal_X1  \
	insn_test_jalr_X1  \
	insn_test_jalr_Y1  \
	insn_test_jalrp_X1  \
	insn_test_jalrp_Y1  \
	insn_test_jr_X1  \
	insn_test_jr_Y1  \
	insn_test_jrp_X1  \
	insn_test_jrp_Y1  \
	insn_test_ld_X1  \
	insn_test_ld_Y2  \
	insn_test_ld1s_X1  \
	insn_test_ld1s_Y2  \
	insn_test_ld1s_add_X1  \
	insn_test_ld1u_X1  \
	insn_test_ld1u_Y2  \
	insn_test_ld1u_add_X1  \
	insn_test_ld2s_X1  \
	insn_test_ld2s_Y2  \
	insn_test_ld2u_X1  \
	insn_test_ld2u_Y2  \
	insn_test_ld4s_X1  \
	insn_test_ld4s_add_X1  \
	insn_test_ld4u_X1  \
	insn_test_ld4u_Y2  \
	insn_test_ld4u_add_X1  \
	insn_test_ld_add_X1  \
	insn_test_ldna_X1  \
	insn_test_ldna_add_X1  \
	insn_test_ldnt_X1  \
	insn_test_ldnt1s_X1  \
	insn_test_ldnt1s_add_X1  \
	insn_test_ldnt1u_X1  \
	insn_test_ldnt1u_add_X1  \
	insn_test_ldnt2s_X1  \
	insn_test_ldnt2s_add_X1  \
	insn_test_ldnt2u_add_X1  \
	insn_test_ldnt4s_X1  \
	insn_test_ldnt4s_add_X1  \
	insn_test_ldnt4u_X1  \
	insn_test_ldnt4u_add_X1  \
	insn_test_ldnt_add_X1  \
	insn_test_lnk_X1  \
	insn_test_lnk_Y1  \
	insn_test_mf_X1  \
	insn_test_mm_X0  \
	insn_test_mnz_X0  \
	insn_test_mnz_X1  \
	insn_test_mnz_Y0  \
	insn_test_mnz_Y1  \
	insn_test_mul_hs_hs_X0  \
	insn_test_mul_hs_hs_Y0  \
	insn_test_mul_hs_hu_X0  \
	insn_test_mul_hs_ls_X0  \
	insn_test_mul_hs_lu_X0  \
	insn_test_mul_hu_hu_X0  \
	insn_test_mul_hu_hu_Y0  \
	insn_test_mul_hu_lu_X0  \
	insn_test_mul_ls_ls_X0  \
	insn_test_mul_ls_ls_Y0  \
	insn_test_mul_ls_lu_X0  \
	insn_test_mul_lu_lu_X0  \
	insn_test_mul_lu_lu_Y0  \
	insn_test_mula_hs_hs_X0  \
	insn_test_mula_hs_hs_Y0  \
	insn_test_mula_hs_hu_X0  \
	insn_test_mula_hs_ls_X0  \
	insn_test_mula_hs_lu_X0  \
	insn_test_mula_hu_hu_X0  \
	insn_test_mula_hu_hu_Y0  \
	insn_test_mula_hu_ls_X0  \
	insn_test_mula_hu_lu_X0  \
	insn_test_mula_ls_ls_X0  \
	insn_test_mula_ls_ls_Y0  \
	insn_test_mula_ls_lu_X0  \
	insn_test_mula_lu_lu_X0  \
	insn_test_mula_lu_lu_Y0  \
	insn_test_mulax_X0  \
	insn_test_mulax_Y0  \
	insn_test_mulx_X0  \
	insn_test_mulx_Y0  \
	insn_test_mz_X0  \
	insn_test_mz_X1  \
	insn_test_mz_Y0  \
	insn_test_mz_Y1  \
	insn_test_nop_X0  \
	insn_test_nop_X1  \
	insn_test_nop_Y0  \
	insn_test_nop_Y1  \
	insn_test_nor_X0  \
	insn_test_nor_X1  \
	insn_test_nor_Y0  \
	insn_test_nor_Y1  \
	insn_test_or_X0  \
	insn_test_or_X1  \
	insn_test_or_Y0  \
	insn_test_or_Y1  \
	insn_test_ori_X0  \
	insn_test_ori_X1  \
	insn_test_pcnt_X0  \
	insn_test_pcnt_Y0  \
	insn_test_revbits_X0  \
	insn_test_revbits_Y0  \
	insn_test_revbytes_X0  \
	insn_test_revbytes_Y0  \
	insn_test_rotl_X0  \
	insn_test_rotl_X1  \
	insn_test_rotl_Y0  \
	insn_test_rotl_Y1  \
	insn_test_rotli_X0  \
	insn_test_rotli_X1  \
	insn_test_rotli_Y0  \
	insn_test_rotli_Y1  \
	insn_test_shl_X0  \
	insn_test_shl_X1  \
	insn_test_shl_Y0  \
	insn_test_shl_Y1  \
	insn_test_shl16insli_X0  \
	insn_test_shl16insli_X1  \
	insn_test_shl1add_X0  \
	insn_test_shl1add_X1  \
	insn_test_shl1add_Y0  \
	insn_test_shl1add_Y1  \
	insn_test_shl1addx_X0  \
	insn_test_shl1addx_X1  \
	insn_test_shl1addx_Y0  \
	insn_test_shl1addx_Y1  \
	insn_test_shl2add_X0  \
	insn_test_shl2add_X1  \
	insn_test_shl2add_Y0  \
	insn_test_shl2add_Y1  \
	insn_test_shl2addx_X0  \
	insn_test_shl2addx_X1  \
	insn_test_shl2addx_Y0  \
	insn_test_shl2addx_Y1  \
	insn_test_shl3add_X0  \
	insn_test_shl3add_X1  \
	insn_test_shl3add_Y0  \
	insn_test_shl3add_Y1  \
	insn_test_shl3addx_X0  \
	insn_test_shl3addx_X1  \
	insn_test_shl3addx_Y0  \
	insn_test_shl3addx_Y1  \
	insn_test_shli_X0  \
	insn_test_shli_X1  \
	insn_test_shli_Y0  \
	insn_test_shli_Y1  \
	insn_test_shlx_X0  \
	insn_test_shlx_X1  \
	insn_test_shlxi_X0  \
	insn_test_shlxi_X1  \
	insn_test_shrs_X0  \
	insn_test_shrs_X1  \
	insn_test_shrs_Y0  \
	insn_test_shrs_Y1  \
	insn_test_shrsi_X0  \
	insn_test_shrsi_X1  \
	insn_test_shrsi_Y0  \
	insn_test_shrsi_Y1  \
	insn_test_shru_X0  \
	insn_test_shru_X1  \
	insn_test_shru_Y0  \
	insn_test_shru_Y1  \
	insn_test_shrui_X0  \
	insn_test_shrui_X1  \
	insn_test_shrui_Y0  \
	insn_test_shrui_Y1  \
	insn_test_shrux_X0  \
	insn_test_shrux_X1  \
	insn_test_shufflebytes_X0  \
	insn_test_st_X1  \
	insn_test_st_Y2  \
	insn_test_st1_X1  \
	insn_test_st1_Y2  \
	insn_test_st1_add_X1  \
	insn_test_st2_X1  \
	insn_test_st2_Y2  \
	insn_test_st2_add_X1  \
	insn_test_st4_X1  \
	insn_test_st4_Y2  \
	insn_test_st4_add_X1  \
	insn_test_st_add_X1  \
	insn_test_stnt_X1  \
	insn_test_stnt1_X1  \
	insn_test_stnt2_X1  \
	insn_test_stnt2_add_X1  \
	insn_test_stnt4_X1  \
	insn_test_stnt4_add_X1  \
	insn_test_stnt_add_X1  \
	insn_test_sub_X0  \
	insn_test_sub_X1  \
	insn_test_sub_Y0  \
	insn_test_sub_Y1  \
	insn_test_subx_X0  \
	insn_test_subx_X1  \
	insn_test_subx_Y0  \
	insn_test_subx_Y1  \
	insn_test_tblidxb0_X0  \
	insn_test_tblidxb0_Y0  \
	insn_test_tblidxb1_X0  \
	insn_test_tblidxb1_Y0  \
	insn_test_tblidxb2_X0  \
	insn_test_tblidxb2_Y0  \
	insn_test_tblidxb3_X0  \
	insn_test_tblidxb3_Y0  \
	insn_test_v1add_X0  \
	insn_test_v1add_X1  \
	insn_test_v1adduc_X0  \
	insn_test_v1adduc_X1  \
	insn_test_v1adiffu_X0  \
	insn_test_v1avgu_X0  \
	insn_test_v1cmpeq_X0  \
	insn_test_v1cmpeq_X1  \
	insn_test_v1cmpeqi_X0  \
	insn_test_v1cmpeqi_X1  \
	insn_test_v1cmples_X0  \
	insn_test_v1cmples_X1  \
	insn_test_v1cmpleu_X0  \
	insn_test_v1cmpleu_X1  \
	insn_test_v1cmplts_X0  \
	insn_test_v1cmplts_X1  \
	insn_test_v1cmpltu_X0  \
	insn_test_v1cmpltu_X1  \
	insn_test_v1cmpne_X0  \
	insn_test_v1cmpne_X1  \
	insn_test_v1ddotpu_X0  \
	insn_test_v1ddotpua_X0  \
	insn_test_v1ddotpus_X0  \
	insn_test_v1ddotpusa_X0  \
	insn_test_v1dotp_X0  \
	insn_test_v1dotpa_X0  \
	insn_test_v1dotpu_X0  \
	insn_test_v1dotpua_X0  \
	insn_test_v1dotpus_X0  \
	insn_test_v1dotpusa_X0  \
	insn_test_v1int_h_X0  \
	insn_test_v1int_h_X1  \
	insn_test_v1int_l_X0  \
	insn_test_v1int_l_X1  \
	insn_test_v1maxu_X0  \
	insn_test_v1maxu_X1  \
	insn_test_v1minu_X0  \
	insn_test_v1minu_X1  \
	insn_test_v1mnz_X0  \
	insn_test_v1mnz_X1  \
	insn_test_v1multu_X0  \
	insn_test_v1mulu_X0  \
	insn_test_v1mulus_X0  \
	insn_test_v1mz_X0  \
	insn_test_v1mz_X1  \
	insn_test_v1sadau_X0  \
	insn_test_v1sadu_X0  \
	insn_test_v1shl_X0  \
	insn_test_v1shl_X1  \
	insn_test_v1shli_X0  \
	insn_test_v1shli_X1  \
	insn_test_v1shrs_X0  \
	insn_test_v1shrs_X1  \
	insn_test_v1shrsi_X0  \
	insn_test_v1shrsi_X1  \
	insn_test_v1shru_X0  \
	insn_test_v1shru_X1  \
	insn_test_v1shrui_X0  \
	insn_test_v1shrui_X1  \
	insn_test_v1sub_X0  \
	insn_test_v1sub_X1  \
	insn_test_v1subuc_X0  \
	insn_test_v1subuc_X1  \
	insn_test_v2add_X0  \
	insn_test_v2add_X1  \
	insn_test_v2addsc_X0  \
	insn_test_v2addsc_X1  \
	insn_test_v2adiffs_X0  \
	insn_test_v2avgs_X0  \
	insn_test_v2cmpeq_X0  \
	insn_test_v2cmpeq_X1  \
	insn_test_v2cmpeqi_X0  \
	insn_test_v2cmpeqi_X1  \
	insn_test_v2cmples_X0  \
	insn_test_v2cmples_X1  \
	insn_test_v2cmpleu_X0  \
	insn_test_v2cmpleu_X1  \
	insn_test_v2cmplts_X0  \
	insn_test_v2cmplts_X1  \
	insn_test_v2cmpltsi_X0  \
	insn_test_v2cmpltsi_X1  \
	insn_test_v2cmpltu_X0  \
	insn_test_v2cmpltu_X1  \
	insn_test_v2cmpltui_X0  \
	insn_test_v2cmpltui_X1  \
	insn_test_v2cmpne_X0  \
	insn_test_v2cmpne_X1  \
	insn_test_v2dotp_X0  \
	insn_test_v2dotpa_X0  \
	insn_test_v2int_h_X0  \
	insn_test_v2int_h_X1  \
	insn_test_v2int_l_X0  \
	insn_test_v2int_l_X1  \
	insn_test_v2maxs_X0  \
	insn_test_v2maxs_X1  \
	insn_test_v2mins_X0  \
	insn_test_v2mins_X1  \
	insn_test_v2mnz_X0  \
	insn_test_v2mnz_X1  \
	insn_test_v2mulfsc_X0  \
	insn_test_v2muls_X0  \
	insn_test_v2mults_X0  \
	insn_test_v2mz_X0  \
	insn_test_v2mz_X1  \
	insn_test_v2packh_X0  \
	insn_test_v2packh_X1  \
	insn_test_v2packl_X0  \
	insn_test_v2packl_X1  \
	insn_test_v2packuc_X0  \
	insn_test_v2packuc_X1  \
	insn_test_v2sadas_X0  \
	insn_test_v2sadau_X0  \
	insn_test_v2sads_X0  \
	insn_test_v2sadu_X0  \
	insn_test_v2shl_X0  \
	insn_test_v2shl_X1  \
	insn_test_v2shli_X0  \
	insn_test_v2shli_X1  \
	insn_test_v2shlsc_X0  \
	insn_test_v2shlsc_X1  \
	insn_test_v2shrs_X0  \
	insn_test_v2shrs_X1  \
	insn_test_v2shrsi_X0  \
	insn_test_v2shrsi_X1  \
	insn_test_v2shru_X0  \
	insn_test_v2shru_X1  \
	insn_test_v2shrui_X0  \
	insn_test_v2shrui_X1  \
	insn_test_v2sub_X0  \
	insn_test_v2sub_X1  \
	insn_test_v2subsc_X0  \
	insn_test_v2subsc_X1  \
	insn_test_v4add_X0  \
	insn_test_v4add_X1  \
	insn_test_v4addsc_X0  \
	insn_test_v4addsc_X1  \
	insn_test_v4int_h_X0  \
	insn_test_v4int_h_X1  \
	insn_test_v4int_l_X0  \
	insn_test_v4int_l_X1  \
	insn_test_v4packsc_X0  \
	insn_test_v4packsc_X1  \
	insn_test_v4shl_X0  \
	insn_test_v4shl_X1  \
	insn_test_v4shlsc_X0  \
	insn_test_v4shlsc_X1  \
	insn_test_v4shrs_X0  \
	insn_test_v4shrs_X1  \
	insn_test_v4shru_X0  \
	insn_test_v4shru_X1  \
	insn_test_v4sub_X0  \
	insn_test_v4sub_X1  \
	insn_test_v4subsc_X0  \
	insn_test_v4subsc_X1  \
	insn_test_wh64_X1  \
	insn_test_xor_X0  \
	insn_test_xor_X1  \
	insn_test_xor_Y0  \
	insn_test_xor_Y1  \
	insn_test_xori_X0  \
	insn_test_xori_X1

check_PROGRAMS = \
	allexec \
	$(insn_tests)

AM_CFLAGS    += @FLAG_M64@ -w
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS          = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
gen_insn_test_CFLAGS    = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@ -I$(top_srcdir)/VEX/priv
gen_insn_test_LDADD     = ../../../VEX/priv/tilegx_disasm.o

$(addsuffix .c, $(insn_tests)) : gen_insn_test
	@$(srcdir)/gen_test.sh $@

$(addsuffix .stdout.exp, $(insn_tests)) : $(insn_tests)
	./$(basename $(basename $@)) > $@

$(addsuffix .stderr.exp, $(insn_tests)) :
	touch  $@

$(addsuffix .vgtest, $(insn_tests)) :
	echo -e "prog: $(basename $@)\nvgopts: -q" > $@

check-am : $(addsuffix .stdout.exp, $(insn_tests))  $(addsuffix .stderr.exp, $(insn_tests)) $(addsuffix .vgtest, $(insn_tests))

clean-am :
	@rm -f *.stderr.exp *.stdout.exp *.vgtest $(addsuffix .c, $(insn_tests))  $(addsuffix .o, $(insn_tests)) $(insn_tests)
	@rm -f *.o  $(bin_PROGRAMS)

distclean-am : clean-am
