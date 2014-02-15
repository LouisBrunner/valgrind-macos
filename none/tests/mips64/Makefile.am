
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	arithmetic_instruction.stdout.exp-mips64 \
	arithmetic_instruction.stdout.exp-mips64r2 arithmetic_instruction.stderr.exp \
	arithmetic_instruction.vgtest \
	branch_and_jump_instructions.stdout.exp \
	branch_and_jump_instructions.stderr.exp branch_and_jump_instructions.vgtest \
	branches.stdout.exp branches.stderr.exp branches.vgtest \
	cvm_ins.stdout.exp cvm_ins.stdout.exp-non-octeon \
	cvm_ins.stderr.exp cvm_ins.vgtest \
	cvm_lx_ins.stdout.exp-LE cvm_lx_ins.stdout.exp-BE \
	cvm_lx_ins.stdout.exp-non-octeon \
	cvm_lx_ins.stderr.exp cvm_lx_ins.vgtest \
	extract_insert_bit_field.stdout.exp-mips64 \
	extract_insert_bit_field.stdout.exp-mips64r2 \
	extract_insert_bit_field.stderr.exp extract_insert_bit_field.vgtest \
	fpu_arithmetic.stdout.exp fpu_arithmetic.stderr.exp fpu_arithmetic.vgtest \
	fpu_branches.stdout.exp fpu_branches.stderr.exp fpu_branches.vgtest \
	fpu_control_word.stdout.exp fpu_control_word.stderr.exp \
	fpu_control_word.vgtest \
	fpu_load_store.stdout.exp-BE fpu_load_store.stdout.exp-LE \
	fpu_load_store.stderr.exp fpu_load_store.vgtest \
	load_indexed_instructions.stdout.exp-BE \
	load_indexed_instructions.stdout.exp-LE \
	load_indexed_instructions.stdout.exp-non-octeon \
	load_indexed_instructions.stderr.exp load_indexed_instructions.vgtest \
	load_store.stdout.exp-BE load_store.stdout.exp-LE load_store.stderr.exp \
	load_store.vgtest \
	load_store_multiple.stdout.exp-BE load_store_multiple.stdout.exp-LE \
	load_store_multiple.stderr.exp load_store_multiple.vgtest \
	load_store_unaligned.stdout.exp load_store_unaligned.stderr.exp \
	load_store_unaligned.vgtest \
	logical_instructions.stdout.exp logical_instructions.stderr.exp \
	logical_instructions.vgtest \
	move_instructions.stdout.exp-BE move_instructions.stdout.exp-LE \
	move_instructions.stderr.exp move_instructions.vgtest \
	rotate_swap.stdout.exp-mips64 rotate_swap.stdout.exp-mips64r2 \
	rotate_swap.stderr.exp rotate_swap.vgtest \
	round.stdout.exp round.stderr.exp round.vgtest \
	shift_instructions.stdout.exp-mips64 shift_instructions.stdout.exp-mips64r2 \
	shift_instructions.stderr.exp shift_instructions.vgtest \
	test_block_size.stdout.exp test_block_size.stderr.exp \
	test_block_size.vgtest \
	test_fcsr.stdout.exp test_fcsr.stderr.exp \
	test_fcsr.vgtest \
	test_math.stdout.exp test_math.stderr.exp test_math.vgtest \
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
	cvm_ins \
	cvm_lx_ins \
	extract_insert_bit_field \
	fpu_arithmetic \
	fpu_branches \
	fpu_control_word \
	fpu_load_store \
	load_indexed_instructions \
	load_store \
	load_store_multiple \
	load_store_unaligned \
	logical_instructions \
	move_instructions \
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

cvm_ins_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON@
cvm_lx_ins_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON2@
load_indexed_instructions_CFLAGS = $(AM_CFLAGS) -g -O0 @FLAG_OCTEON2@
fpu_arithmetic_CFLAGS = $(AM_CFLAGS) -lm

# C++ tests
test_math_SOURCES          = test_math.cpp
