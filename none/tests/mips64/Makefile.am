
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	arithmetic_instruction.stdout.exp-mips64 \
	arithmetic_instruction.stdout.exp-mips64r2 arithmetic_instruction.stderr.exp \
	arithmetic_instruction.vgtest arithmetic_instruction.stdout.exp-mips64r6 \
	branch_and_jump_instructions.stdout.exp \
	branch_and_jump_instructions.stderr.exp branch_and_jump_instructions.vgtest \
	branches.stdout.exp branches.stderr.exp branches.vgtest branches.stdout.exp-r6 \
	branches_r6.stderr.exp branches_r6.stdout.exp branches_r6.vgtest\
	branch_pc.stderr.exp branch_pc.stdout.exp branch_pc.vgtest \
	change_fp_mode.stderr.exp change_fp_mode.stdout.exp change_fp_mode.vgtest \
	cvm_bbit.stdout.exp cvm_bbit.stdout.exp-non-octeon \
	cvm_bbit.stderr.exp cvm_bbit.vgtest \
	cvm_ins.stdout.exp cvm_ins.stdout.exp-non-octeon \
	cvm_ins.stderr.exp cvm_ins.vgtest \
	cvm_lx_ins.stdout.exp-LE cvm_lx_ins.stdout.exp-BE \
	cvm_lx_ins.stdout.exp-non-octeon \
	cvm_lx_ins.stderr.exp cvm_lx_ins.vgtest \
	cvm_atomic.stdout.exp-LE cvm_atomic.stdout.exp-BE \
	cvm_atomic.stdout.exp-non-octeon \
	cvm_atomic.stderr.exp cvm_atomic.vgtest \
	cvm_atomic_thread.stdout.exp cvm_atomic_thread.stdout.exp-non-octeon \
	cvm_atomic_thread.stderr.exp cvm_atomic_thread.vgtest \
	extract_insert_bit_field.stdout.exp-mips64 \
	extract_insert_bit_field.stdout.exp-mips64r2 \
	extract_insert_bit_field.stderr.exp extract_insert_bit_field.vgtest \
	fpu_arithmetic.stdout.exp fpu_arithmetic.stderr.exp fpu_arithmetic.vgtest \
	fpu_arithmetic.stdout.exp-r6 \
	fpu_branches.stdout.exp fpu_branches.stderr.exp fpu_branches.vgtest \
	fpu_control_word.stdout.exp fpu_control_word.stderr.exp \
	fpu_control_word.vgtest \
	fpu_load_store.stdout.exp-BE fpu_load_store.stdout.exp-LE \
	fpu_load_store.stderr.exp fpu_load_store.vgtest \
	fpu_load_store.stdout.exp-BE-r2 fpu_load_store.stdout.exp-LE-r2 \
	fpu_load_store.stdout.exp-BE-r6 fpu_load_store.stdout.exp-LE-r6 \
	fp_r6.stderr.exp fp_r6.stdout.exp fp_r6.vgtest \
	load_indexed_instructions.stdout.exp-BE \
	load_indexed_instructions.stdout.exp-LE \
	load_indexed_instructions.stdout.exp-non-octeon \
	load_indexed_instructions.stderr.exp load_indexed_instructions.vgtest \
	load_store.stdout.exp-BE load_store.stdout.exp-LE load_store.stderr.exp \
	load_store.vgtest load_store.stdout.exp-LE-r6 load_store.stdout.exp-BE-r6 \
	load_store_multiple.stdout.exp-BE load_store_multiple.stdout.exp-LE \
	load_store_multiple.stderr.exp load_store_multiple.vgtest \
	load_store_multiple.stdout.exp-LE-r6 load_store_multiple.stdout.exp-BE-r6 \
	load_store_unaligned.stdout.exp load_store_unaligned.stderr.exp \
	logical_instructions.stdout.exp logical_instructions.stderr.exp \
	load_store_unaligned.vgtest \
	logical_instructions.vgtest \
	move_instructions.stdout.exp-BE move_instructions.stdout.exp-LE \
	move_instructions.stderr.exp move_instructions.vgtest \
	move_instructions.stdout.exp-r6 \
	MIPS64r6int.stderr.exp MIPS64r6int.stdout.exp MIPS64r6int.vgtest \
	msa_data_transfer.stdout.exp msa_data_transfer.stderr.exp \
	msa_data_transfer.vgtest msa_data_transfer.stdout.exp-BE \
	msa_arithmetic.stderr.exp msa_arithmetic.stdout.exp msa_arithmetic.vgtest \
	msa_comparison.stderr.exp msa_comparison.stdout.exp msa_comparison.vgtest \
	msa_fpu.stderr.exp msa_fpu.stdout.exp msa_fpu.vgtest \
	msa_logical_and_shift.stderr.exp msa_logical_and_shift.stdout.exp \
	msa_logical_and_shift.vgtest msa_shuffle.stderr.exp msa_shuffle.stdout.exp \
	msa_shuffle.vgtest\
	rotate_swap.stdout.exp-mips64 rotate_swap.stdout.exp-mips64r2 \
	rotate_swap.stderr.exp rotate_swap.vgtest \
	round.stdout.exp round.stderr.exp round.vgtest \
	r6_instructions.stdout.exp-LE r6_instructions.stdout.exp-BE r6_instructions.stderr.exp \
	r6_instructions.vgtest \
	pc_instructions_r6.stderr.exp pc_instructions_r6.stdout.exp pc_instructions_r6.vgtest \
	shift_instructions.stdout.exp-mips64 shift_instructions.stdout.exp-mips64r2 \
	shift_instructions.stderr.exp shift_instructions.vgtest \
	test_block_size.stdout.exp test_block_size.stderr.exp \
	test_block_size.vgtest \
	test_fcsr.stdout.exp test_fcsr.stderr.exp \
	test_fcsr.vgtest \
	test_math.stdout.exp test_math.stdout.exp-older-gcc \
	test_math.stderr.exp test_math.vgtest \
	unaligned_load.stdout.exp-BE unaligned_load.stdout.exp-LE \
	unaligned_load.stderr.exp unaligned_load.vgtest \
	unaligned_load_store.stdout.exp-LE unaligned_load_store.stdout.exp-BE \
	unaligned_load_store.stderr.exp unaligned_load_store.vgtest \
	const.h macro_fpu.h macro_int.h macro_load_store.h rounding_mode.h

check_PROGRAMS = \
	allexec \
	arithmetic_instruction \
	branch_and_jump_instructions \
	branches \
	branches_r6 \
	branch_pc \
	change_fp_mode \
	cvm_bbit \
	cvm_ins \
	cvm_lx_ins \
	cvm_atomic \
	cvm_atomic_thread \
	extract_insert_bit_field \
	fpu_arithmetic \
	fpu_branches \
	fpu_control_word \
	fpu_load_store \
	fp_r6 \
	load_indexed_instructions \
	load_store \
	load_store_multiple \
	load_store_unaligned \
	logical_instructions \
	move_instructions \
	msa_data_transfer \
	msa_arithmetic \
	msa_comparison \
	msa_fpu \
	msa_logical_and_shift \
	msa_shuffle \
	r6_instructions \
	MIPS64r6int \
	pc_instructions_r6 \
	rotate_swap \
	round \
	shift_instructions \
	test_block_size \
	test_fcsr \
	test_math \
	unaligned_load \
	unaligned_load_store

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS          = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

cvm_bbit_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON@
cvm_ins_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON@
cvm_lx_ins_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON2@
cvm_atomic_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON2@
cvm_atomic_thread_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON2@
load_indexed_instructions_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON2@
fpu_arithmetic_CFLAGS = $(AM_CFLAGS) -lm
msa_arithmetic_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_comparison_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_data_transfer_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_fpu_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@ -lm
msa_logical_and_shift_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_shuffle_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@


# C++ tests
test_math_SOURCES          = test_math.cpp
