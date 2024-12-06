
/*--------------------------------------------------------------------*/
/*--- Basic definitions and helper functions for DWARF3.           ---*/
/*---                                              priv_d3basics.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2017 OpenWorks LLP and others; see below
      info@open-works.co.uk

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.

   -------------

   Some of this code (DWARF3 enumerations) is taken from FSF's
   gdb-6.6/include/elf/dwarf2.h, which is Copyright (C) 1992 to 2006
   Free Software Foundation, Inc and is also GPL-2-or-later.
*/

#ifndef __PRIV_D3BASICS_H
#define __PRIV_D3BASICS_H

#include "pub_core_basics.h"       // Addr
#include "pub_core_debuginfo.h"    // DebugInfo

/* This stuff is taken from gdb-6.6/include/elf/dwarf2.h, which is
   GPL2+.
*/
/* Tag names and codes.  */
typedef enum 
  {
    DW_TAG_padding = 0x00,
    DW_TAG_array_type = 0x01,
    DW_TAG_class_type = 0x02,
    DW_TAG_entry_point = 0x03,
    DW_TAG_enumeration_type = 0x04,
    DW_TAG_formal_parameter = 0x05,
    DW_TAG_imported_declaration = 0x08,
    DW_TAG_label = 0x0a,
    DW_TAG_lexical_block = 0x0b,
    DW_TAG_member = 0x0d,
    DW_TAG_pointer_type = 0x0f,
    DW_TAG_reference_type = 0x10,
    DW_TAG_compile_unit = 0x11,
    DW_TAG_string_type = 0x12,
    DW_TAG_structure_type = 0x13,
    DW_TAG_subroutine_type = 0x15,
    DW_TAG_typedef = 0x16,
    DW_TAG_union_type = 0x17,
    DW_TAG_unspecified_parameters = 0x18,
    DW_TAG_variant = 0x19,
    DW_TAG_common_block = 0x1a,
    DW_TAG_common_inclusion = 0x1b,
    DW_TAG_inheritance = 0x1c,
    DW_TAG_inlined_subroutine = 0x1d,
    DW_TAG_module = 0x1e,
    DW_TAG_ptr_to_member_type = 0x1f,
    DW_TAG_set_type = 0x20,
    DW_TAG_subrange_type = 0x21,
    DW_TAG_with_stmt = 0x22,
    DW_TAG_access_declaration = 0x23,
    DW_TAG_base_type = 0x24,
    DW_TAG_catch_block = 0x25,
    DW_TAG_const_type = 0x26,
    DW_TAG_constant = 0x27,
    DW_TAG_enumerator = 0x28,
    DW_TAG_file_type = 0x29,
    DW_TAG_friend = 0x2a,
    DW_TAG_namelist = 0x2b,
    DW_TAG_namelist_item = 0x2c,
    DW_TAG_packed_type = 0x2d,
    DW_TAG_subprogram = 0x2e,
    DW_TAG_template_type_param = 0x2f,
    DW_TAG_template_value_param = 0x30,
    DW_TAG_thrown_type = 0x31,
    DW_TAG_try_block = 0x32,
    DW_TAG_variant_part = 0x33,
    DW_TAG_variable = 0x34,
    DW_TAG_volatile_type = 0x35,
    /* DWARF 3.  */
    DW_TAG_dwarf_procedure = 0x36,
    DW_TAG_restrict_type = 0x37,
    DW_TAG_interface_type = 0x38,
    DW_TAG_namespace = 0x39,
    DW_TAG_imported_module = 0x3a,
    DW_TAG_unspecified_type = 0x3b,
    DW_TAG_partial_unit = 0x3c,
    DW_TAG_imported_unit = 0x3d,
    DW_TAG_condition = 0x3f,
    DW_TAG_shared_type = 0x40,
    /* DWARF 4.  */
    DW_TAG_type_unit = 0x41,
    DW_TAG_rvalue_reference_type = 0x42,
    DW_TAG_template_alias = 0x43,
    /* DWARF 5.  */
    DW_TAG_coarray_type = 0x44,
    DW_TAG_generic_subrange = 0x45,
    DW_TAG_dynamic_type = 0x46,
    DW_TAG_atomic_type = 0x47,
    DW_TAG_call_site = 0x48,
    DW_TAG_call_site_parameter = 0x49,
    DW_TAG_skeleton_unit = 0x4a,
    DW_TAG_immutable_type = 0x4b,
    /* SGI/MIPS Extensions.  */
    DW_TAG_MIPS_loop = 0x4081,
    /* HP extensions.  See: ftp://ftp.hp.com/pub/lang/tools/WDB/wdb-4.0.tar.gz .  */
    DW_TAG_HP_array_descriptor = 0x4090,
    /* GNU extensions.  */
    DW_TAG_format_label = 0x4101,	/* For FORTRAN 77 and Fortran 90.  */
    DW_TAG_function_template = 0x4102,	/* For C++.  */
    DW_TAG_class_template = 0x4103,	/* For C++.  */
    DW_TAG_GNU_BINCL = 0x4104,
    DW_TAG_GNU_EINCL = 0x4105,
    DW_TAG_GNU_template_template_param = 0x4106,
    DW_TAG_GNU_template_parameter_pack = 0x4107,
    DW_TAG_GNU_formal_parameter_pack = 0x4108,
    DW_TAG_GNU_call_site = 0x4109,
    DW_TAG_GNU_call_site_parameter = 0x410a,
    /* Extensions for UPC.  See: http://upc.gwu.edu/~upc.  */
    DW_TAG_upc_shared_type = 0x8765,
    DW_TAG_upc_strict_type = 0x8766,
    DW_TAG_upc_relaxed_type = 0x8767,
    /* PGI (STMicroelectronics) extensions.  No documentation available.  */
    DW_TAG_PGI_kanji_type      = 0xA000,
    DW_TAG_PGI_interface_block = 0xA020
  }
  DW_TAG;

#define DW_TAG_lo_user	0x4080
#define DW_TAG_hi_user	0xffff

/* Flag that tells whether entry has a child or not.  */
typedef enum
  {
    DW_children_no = 0,
    DW_children_yes = 1
  }
  DW_children;

/* Source language names and codes.  */
typedef enum dwarf_source_language
  {
    DW_LANG_C89 = 0x0001,
    DW_LANG_C = 0x0002,
    DW_LANG_Ada83 = 0x0003,
    DW_LANG_C_plus_plus = 0x0004,
    DW_LANG_Cobol74 = 0x0005,
    DW_LANG_Cobol85 = 0x0006,
    DW_LANG_Fortran77 = 0x0007,
    DW_LANG_Fortran90 = 0x0008,
    DW_LANG_Pascal83 = 0x0009,
    DW_LANG_Modula2 = 0x000a,
    /* DWARF 3.  */
    DW_LANG_Java = 0x000b,
    DW_LANG_C99 = 0x000c,
    DW_LANG_Ada95 = 0x000d,
    DW_LANG_Fortran95 = 0x000e,
    DW_LANG_PLI = 0x000f,
    DW_LANG_ObjC = 0x0010,
    DW_LANG_ObjC_plus_plus = 0x0011,
    DW_LANG_UPC = 0x0012,
    DW_LANG_D = 0x0013,
    /* DWARF 4.  */
    DW_LANG_Python = 0x0014,
    /* DWARF 5.  */
    DW_LANG_OpenCL = 0x0015,
    DW_LANG_Go = 0x0016,
    DW_LANG_Modula3 = 0x0017,
    DW_LANG_Haskell = 0x0018,
    DW_LANG_C_plus_plus_03 = 0x0019,
    DW_LANG_C_plus_plus_11 = 0x001a,
    DW_LANG_OCaml = 0x001b,
    DW_LANG_Rust = 0x001c,
    DW_LANG_C11 = 0x001d,
    DW_LANG_Swift = 0x001e,
    DW_LANG_Julia = 0x001f,
    DW_LANG_Dylan = 0x0020,
    DW_LANG_C_plus_plus_14 = 0x0021,
    DW_LANG_Fortran03 = 0x0022,
    DW_LANG_Fortran08 = 0x0023,
    DW_LANG_RenderScript = 0x0024,
    DW_LANG_BLISS = 0x0025,
    /* Language codes added since DWARF 5.
       https://dwarfstd.org/languages.html */
    DW_LANG_Kotlin = 0x0026,
    DW_LANG_Zig = 0x0027,
    DW_LANG_Crystal = 0x0028,
    DW_LANG_C_plus_plus_17 = 0x002a,
    DW_LANG_C_plus_plus_20 = 0x002b,
    DW_LANG_C17 = 0x002c,
    DW_LANG_Fortran18 = 0x002d,
    DW_LANG_Ada2005 = 0x002e,
    DW_LANG_Ada2012 = 0x002f,
    DW_LANG_HIP = 0x0030,
    DW_LANG_Assembly = 0x0031,
    DW_LANG_C_sharp = 0x0032,
    DW_LANG_Mojo = 0x0033,
    DW_LANG_GLSL = 0x0034,
    DW_LANG_GLSL_ES = 0x0035,
    DW_LANG_HLSL = 0x0036,
    DW_LANG_OpenCL_CPP = 0x0037,
    DW_LANG_CPP_for_OpenCL = 0x0038,
    DW_LANG_SYCL = 0x0039,
    DW_LANG_C_plus_plus_23 = 0x003a,
    DW_LANG_Odin = 0x003b,
    DW_LANG_P4 = 0x003c,
    DW_LANG_Metal = 0x003d,
    DW_LANG_C23 = 0x003e,
    DW_LANG_Fortran23 = 0x003f,
    DW_LANG_Ruby = 0x0040,
    DW_LANG_Move = 0x0041,
    DW_LANG_Hylo = 0x0042,
    /* MIPS.  */
    DW_LANG_Mips_Assembler = 0x8001,
    /* UPC.  */
    DW_LANG_Upc = 0x8765
  }
  DW_LANG;

/* Form names and codes.  */
typedef enum
  {
    DW_FORM_addr = 0x01,
    DW_FORM_block2 = 0x03,
    DW_FORM_block4 = 0x04,
    DW_FORM_data2 = 0x05,
    DW_FORM_data4 = 0x06,
    DW_FORM_data8 = 0x07,
    DW_FORM_string = 0x08,
    DW_FORM_block = 0x09,
    DW_FORM_block1 = 0x0a,
    DW_FORM_data1 = 0x0b,
    DW_FORM_flag = 0x0c,
    DW_FORM_sdata = 0x0d,
    DW_FORM_strp = 0x0e,
    DW_FORM_udata = 0x0f,
    DW_FORM_ref_addr = 0x10,
    DW_FORM_ref1 = 0x11,
    DW_FORM_ref2 = 0x12,
    DW_FORM_ref4 = 0x13,
    DW_FORM_ref8 = 0x14,
    DW_FORM_ref_udata = 0x15,
    DW_FORM_indirect = 0x16,
    /* DWARF 4 values.  */
    DW_FORM_sec_offset = 0x17,
    DW_FORM_exprloc = 0x18,
    DW_FORM_flag_present = 0x19,
    DW_FORM_ref_sig8 = 0x20,
    /* DWARF 5 values.  */
    DW_FORM_strx = 0x1a,
    DW_FORM_addrx = 0x1b,
    DW_FORM_ref_sup4 = 0x1c,
    DW_FORM_strp_sup = 0x1d,
    DW_FORM_data16 = 0x1e,
    DW_FORM_line_strp = 0x1f,
    DW_FORM_implicit_const = 0x21,
    DW_FORM_loclistx = 0x22,
    DW_FORM_rnglistx = 0x23,
    DW_FORM_ref_sup8 = 0x24,
    DW_FORM_strx1 = 0x25,
    DW_FORM_strx2 = 0x26,
    DW_FORM_strx3 = 0x27,
    DW_FORM_strx4 = 0x28,
    DW_FORM_addrx1 = 0x29,
    DW_FORM_addrx2 = 0x2a,
    DW_FORM_addrx3 = 0x2b,
    DW_FORM_addrx4 = 0x2c,
    /* GNU Debug Fission extensions.  */
    DW_FORM_GNU_addr_index = 0x1f01,
    DW_FORM_GNU_str_index = 0x1f02,
    /* Extensions for DWZ multifile.
       See http://www.dwarfstd.org/ShowIssue.php?issue=120604.1&type=open .  */
    DW_FORM_GNU_ref_alt = 0x1f20,
    DW_FORM_GNU_strp_alt = 0x1f21
  }
  DW_FORM;

/* Attribute names and codes.  */
typedef enum
  {
    DW_AT_sibling = 0x01,
    DW_AT_location = 0x02,
    DW_AT_name = 0x03,
    DW_AT_ordering = 0x09,
    DW_AT_subscr_data = 0x0a,
    DW_AT_byte_size = 0x0b,
    DW_AT_bit_offset = 0x0c,
    DW_AT_bit_size = 0x0d,
    DW_AT_element_list = 0x0f,
    DW_AT_stmt_list = 0x10,
    DW_AT_low_pc = 0x11,
    DW_AT_high_pc = 0x12,
    DW_AT_language = 0x13,
    DW_AT_member = 0x14,
    DW_AT_discr = 0x15,
    DW_AT_discr_value = 0x16,
    DW_AT_visibility = 0x17,
    DW_AT_import = 0x18,
    DW_AT_string_length = 0x19,
    DW_AT_common_reference = 0x1a,
    DW_AT_comp_dir = 0x1b,
    DW_AT_const_value = 0x1c,
    DW_AT_containing_type = 0x1d,
    DW_AT_default_value = 0x1e,
    DW_AT_inline = 0x20,
    DW_AT_is_optional = 0x21,
    DW_AT_lower_bound = 0x22,
    DW_AT_producer = 0x25,
    DW_AT_prototyped = 0x27,
    DW_AT_return_addr = 0x2a,
    DW_AT_start_scope = 0x2c,
    DW_AT_stride_size = 0x2e,
    DW_AT_upper_bound = 0x2f,
    DW_AT_abstract_origin = 0x31,
    DW_AT_accessibility = 0x32,
    DW_AT_address_class = 0x33,
    DW_AT_artificial = 0x34,
    DW_AT_base_types = 0x35,
    DW_AT_calling_convention = 0x36,
    DW_AT_count = 0x37,
    DW_AT_data_member_location = 0x38,
    DW_AT_decl_column = 0x39,
    DW_AT_decl_file = 0x3a,
    DW_AT_decl_line = 0x3b,
    DW_AT_declaration = 0x3c,
    DW_AT_discr_list = 0x3d,
    DW_AT_encoding = 0x3e,
    DW_AT_external = 0x3f,
    DW_AT_frame_base = 0x40,
    DW_AT_friend = 0x41,
    DW_AT_identifier_case = 0x42,
    DW_AT_macro_info = 0x43,
    DW_AT_namelist_items = 0x44,
    DW_AT_priority = 0x45,
    DW_AT_segment = 0x46,
    DW_AT_specification = 0x47,
    DW_AT_static_link = 0x48,
    DW_AT_type = 0x49,
    DW_AT_use_location = 0x4a,
    DW_AT_variable_parameter = 0x4b,
    DW_AT_virtuality = 0x4c,
    DW_AT_vtable_elem_location = 0x4d,
    /* DWARF 3 values.  */
    DW_AT_allocated     = 0x4e,
    DW_AT_associated    = 0x4f,
    DW_AT_data_location = 0x50,
    DW_AT_stride        = 0x51,
    DW_AT_entry_pc      = 0x52,
    DW_AT_use_UTF8      = 0x53,
    DW_AT_extension     = 0x54,
    DW_AT_ranges        = 0x55,
    DW_AT_trampoline    = 0x56,
    DW_AT_call_column   = 0x57,
    DW_AT_call_file     = 0x58,
    DW_AT_call_line     = 0x59,
    DW_AT_description   = 0x5a,
    DW_AT_binary_scale  = 0x5b,
    DW_AT_decimal_scale = 0x5c,
    DW_AT_small         = 0x5d,
    DW_AT_decimal_sign  = 0x5e,
    DW_AT_digit_count   = 0x5f,
    DW_AT_picture_string = 0x60,
    DW_AT_mutable       = 0x61,
    DW_AT_threads_scaled = 0x62,
    DW_AT_explicit      = 0x63,
    DW_AT_object_pointer = 0x64,
    DW_AT_endianity     = 0x65,
    DW_AT_elemental     = 0x66,
    DW_AT_pure          = 0x67,
    DW_AT_recursive     = 0x68,
    /* DWARF 4 values.  */
    DW_AT_signature       = 0x69,
    DW_AT_main_subprogram = 0x6a,
    DW_AT_data_bit_offset = 0x6b,
    DW_AT_const_expr      = 0x6c,
    DW_AT_enum_class      = 0x6d,
    DW_AT_linkage_name    = 0x6e,
    DW_AT_string_length_bit_size = 0x6f,
    DW_AT_string_length_byte_size = 0x70,
    DW_AT_rank = 0x71,
    DW_AT_str_offsets_base = 0x72,
    DW_AT_addr_base = 0x73,
    DW_AT_rnglists_base = 0x74,
    /* 0x75 reserved.  */
    DW_AT_dwo_name = 0x76,
    DW_AT_reference = 0x77,
    DW_AT_rvalue_reference = 0x78,
    DW_AT_macros = 0x79,
    DW_AT_call_all_calls = 0x7a,
    DW_AT_call_all_source_calls = 0x7b,
    DW_AT_call_all_tail_calls = 0x7c,
    DW_AT_call_return_pc = 0x7d,
    DW_AT_call_value = 0x7e,
    DW_AT_call_origin = 0x7f,
    DW_AT_call_parameter = 0x80,
    DW_AT_call_pc = 0x81,
    DW_AT_call_tail_call = 0x82,
    DW_AT_call_target = 0x83,
    DW_AT_call_target_clobbered = 0x84,
    DW_AT_call_data_location = 0x85,
    DW_AT_call_data_value = 0x86,
    DW_AT_noreturn = 0x87,
    DW_AT_alignment = 0x88,
    DW_AT_export_symbols = 0x89,
    DW_AT_deleted = 0x8a,
    DW_AT_defaulted = 0x8b,
    DW_AT_loclists_base = 0x8c,
    /* SGI/MIPS extensions.  */
    DW_AT_MIPS_fde = 0x2001,
    DW_AT_MIPS_loop_begin = 0x2002,
    DW_AT_MIPS_tail_loop_begin = 0x2003,
    DW_AT_MIPS_epilog_begin = 0x2004,
    DW_AT_MIPS_loop_unroll_factor = 0x2005,
    DW_AT_MIPS_software_pipeline_depth = 0x2006,
    DW_AT_MIPS_linkage_name = 0x2007,
    DW_AT_MIPS_stride = 0x2008,
    DW_AT_MIPS_abstract_name = 0x2009,
    DW_AT_MIPS_clone_origin = 0x200a,
    DW_AT_MIPS_has_inlines = 0x200b,
    /* HP extensions.  */
    DW_AT_HP_block_index         = 0x2000,
    DW_AT_HP_unmodifiable        = 0x2001, /* Same as DW_AT_MIPS_fde.  */
    DW_AT_HP_actuals_stmt_list   = 0x2010,
    DW_AT_HP_proc_per_section    = 0x2011,
    DW_AT_HP_raw_data_ptr        = 0x2012,
    DW_AT_HP_pass_by_reference   = 0x2013,
    DW_AT_HP_opt_level           = 0x2014,
    DW_AT_HP_prof_version_id     = 0x2015,
    DW_AT_HP_opt_flags           = 0x2016,
    DW_AT_HP_cold_region_low_pc  = 0x2017,
    DW_AT_HP_cold_region_high_pc = 0x2018,
    DW_AT_HP_all_variables_modifiable = 0x2019,
    DW_AT_HP_linkage_name        = 0x201a,
    DW_AT_HP_prof_flags          = 0x201b,  /* In comp unit of procs_info for -g.  */
    /* GNU extensions.  */
    DW_AT_sf_names   = 0x2101,
    DW_AT_src_info   = 0x2102,
    DW_AT_mac_info   = 0x2103,
    DW_AT_src_coords = 0x2104,
    DW_AT_body_begin = 0x2105,
    DW_AT_body_end   = 0x2106,
    DW_AT_GNU_vector = 0x2107,
    DW_AT_GNU_guarded_by = 0x2108,
    DW_AT_GNU_pt_guarded_by = 0x2109,
    DW_AT_GNU_guarded = 0x210a,
    DW_AT_GNU_pt_guarded = 0x210b,
    DW_AT_GNU_locks_excluded = 0x210c,
    DW_AT_GNU_exclusive_locks_required = 0x210d,
    DW_AT_GNU_shared_locks_required = 0x210e,
    DW_AT_GNU_odr_signature = 0x210f,
    DW_AT_GNU_template_name = 0x2110,
    DW_AT_GNU_call_site_value = 0x2111,
    DW_AT_GNU_call_site_data_value = 0x2112,
    DW_AT_GNU_call_site_target = 0x2113,
    DW_AT_GNU_call_site_target_clobbered = 0x2114,
    DW_AT_GNU_tail_call = 0x2115,
    DW_AT_GNU_all_tail_call_sites = 0x2116,
    DW_AT_GNU_all_call_sites = 0x2117,
    DW_AT_GNU_all_source_call_sites = 0x2118,
    DW_AT_GNU_locviews = 0x2137,
    DW_AT_GNU_entry_view = 0x2138,
    DW_AT_GNU_macros = 0x2119,
    DW_AT_GNU_deleted = 0x211a,
    /* GNU Debug Fission extensions.  */
    DW_AT_GNU_dwo_name = 0x2130,
    DW_AT_GNU_dwo_id = 0x2131,
    DW_AT_GNU_ranges_base = 0x2132,
    DW_AT_GNU_addr_base = 0x2133,
    DW_AT_GNU_pubnames = 0x2134,
    DW_AT_GNU_pubtypes = 0x2135,
    /* https://gcc.gnu.org/wiki/DW_AT_GNU_numerator_denominator  */
    DW_AT_GNU_numerator = 0x2303,
    DW_AT_GNU_denominator = 0x2304,
    /* https://gcc.gnu.org/wiki/DW_AT_GNU_bias  */
    DW_AT_GNU_bias = 0x2305,
    /* VMS extensions.  */
    DW_AT_VMS_rtnbeg_pd_address = 0x2201,
    /* UPC extension.  */
    DW_AT_upc_threads_scaled = 0x3210,
    /* PGI (STMicroelectronics) extensions.  */
    DW_AT_PGI_lbase    = 0x3a00,
    DW_AT_PGI_soffset  = 0x3a01,
    DW_AT_PGI_lstride  = 0x3a02
  }
  DW_AT;

#define DW_AT_lo_user	0x2000	/* Implementation-defined range start.  */
#define DW_AT_hi_user	0x3ff0	/* Implementation-defined range end.  */

/* Type encodings.  */
typedef enum
  {
    DW_ATE_void = 0x0,
    DW_ATE_address = 0x1,
    DW_ATE_boolean = 0x2,
    DW_ATE_complex_float = 0x3,
    DW_ATE_float = 0x4,
    DW_ATE_signed = 0x5,
    DW_ATE_signed_char = 0x6,
    DW_ATE_unsigned = 0x7,
    DW_ATE_unsigned_char = 0x8,
    /* DWARF 3.  */
    DW_ATE_imaginary_float = 0x9,
    DW_ATE_packed_decimal = 0xa,
    DW_ATE_numeric_string = 0xb,
    DW_ATE_edited = 0xc,
    DW_ATE_signed_fixed = 0xd,
    DW_ATE_unsigned_fixed = 0xe,
    DW_ATE_decimal_float = 0xf,
    /* DWARF 4.  */
    DW_ATE_UTF = 0x10,
    /* DWARF 5.  */
    DW_ATE_UCS = 0x11,
    DW_ATE_ASCII = 0x12,
    /* HP extensions.  */
    DW_ATE_HP_float80            = 0x80, /* Floating-point (80 bit).  */
    DW_ATE_HP_complex_float80    = 0x81, /* Complex floating-point (80 bit).  */
    DW_ATE_HP_float128           = 0x82, /* Floating-point (128 bit).  */
    DW_ATE_HP_complex_float128   = 0x83, /* Complex floating-point (128 bit).  */
    DW_ATE_HP_floathpintel       = 0x84, /* Floating-point (82 bit IA64).  */
    DW_ATE_HP_imaginary_float80  = 0x85,
    DW_ATE_HP_imaginary_float128 = 0x86
  }
  DW_ATE;


/* Expression operations. */
typedef enum
  {
    DW_OP_addr = 0x03,
    DW_OP_deref = 0x06,
    DW_OP_const1u = 0x08,
    DW_OP_const1s = 0x09,
    DW_OP_const2u = 0x0a,
    DW_OP_const2s = 0x0b,
    DW_OP_const4u = 0x0c,
    DW_OP_const4s = 0x0d,
    DW_OP_const8u = 0x0e,
    DW_OP_const8s = 0x0f,
    DW_OP_constu = 0x10,
    DW_OP_consts = 0x11,
    DW_OP_dup = 0x12,
    DW_OP_drop = 0x13,
    DW_OP_over = 0x14,
    DW_OP_pick = 0x15,
    DW_OP_swap = 0x16,
    DW_OP_rot = 0x17,
    DW_OP_xderef = 0x18,
    DW_OP_abs = 0x19,
    DW_OP_and = 0x1a,
    DW_OP_div = 0x1b,
    DW_OP_minus = 0x1c,
    DW_OP_mod = 0x1d,
    DW_OP_mul = 0x1e,
    DW_OP_neg = 0x1f,
    DW_OP_not = 0x20,
    DW_OP_or = 0x21,
    DW_OP_plus = 0x22,
    DW_OP_plus_uconst = 0x23,
    DW_OP_shl = 0x24,
    DW_OP_shr = 0x25,
    DW_OP_shra = 0x26,
    DW_OP_xor = 0x27,
    DW_OP_bra = 0x28,
    DW_OP_eq = 0x29,
    DW_OP_ge = 0x2a,
    DW_OP_gt = 0x2b,
    DW_OP_le = 0x2c,
    DW_OP_lt = 0x2d,
    DW_OP_ne = 0x2e,
    DW_OP_skip = 0x2f,
    DW_OP_lit0 = 0x30,
    DW_OP_lit1 = 0x31,
    DW_OP_lit2 = 0x32,
    DW_OP_lit3 = 0x33,
    DW_OP_lit4 = 0x34,
    DW_OP_lit5 = 0x35,
    DW_OP_lit6 = 0x36,
    DW_OP_lit7 = 0x37,
    DW_OP_lit8 = 0x38,
    DW_OP_lit9 = 0x39,
    DW_OP_lit10 = 0x3a,
    DW_OP_lit11 = 0x3b,
    DW_OP_lit12 = 0x3c,
    DW_OP_lit13 = 0x3d,
    DW_OP_lit14 = 0x3e,
    DW_OP_lit15 = 0x3f,
    DW_OP_lit16 = 0x40,
    DW_OP_lit17 = 0x41,
    DW_OP_lit18 = 0x42,
    DW_OP_lit19 = 0x43,
    DW_OP_lit20 = 0x44,
    DW_OP_lit21 = 0x45,
    DW_OP_lit22 = 0x46,
    DW_OP_lit23 = 0x47,
    DW_OP_lit24 = 0x48,
    DW_OP_lit25 = 0x49,
    DW_OP_lit26 = 0x4a,
    DW_OP_lit27 = 0x4b,
    DW_OP_lit28 = 0x4c,
    DW_OP_lit29 = 0x4d,
    DW_OP_lit30 = 0x4e,
    DW_OP_lit31 = 0x4f,
    DW_OP_reg0 = 0x50,
    DW_OP_reg1 = 0x51,
    DW_OP_reg2 = 0x52,
    DW_OP_reg3 = 0x53,
    DW_OP_reg4 = 0x54,
    DW_OP_reg5 = 0x55,
    DW_OP_reg6 = 0x56,
    DW_OP_reg7 = 0x57,
    DW_OP_reg8 = 0x58,
    DW_OP_reg9 = 0x59,
    DW_OP_reg10 = 0x5a,
    DW_OP_reg11 = 0x5b,
    DW_OP_reg12 = 0x5c,
    DW_OP_reg13 = 0x5d,
    DW_OP_reg14 = 0x5e,
    DW_OP_reg15 = 0x5f,
    DW_OP_reg16 = 0x60,
    DW_OP_reg17 = 0x61,
    DW_OP_reg18 = 0x62,
    DW_OP_reg19 = 0x63,
    DW_OP_reg20 = 0x64,
    DW_OP_reg21 = 0x65,
    DW_OP_reg22 = 0x66,
    DW_OP_reg23 = 0x67,
    DW_OP_reg24 = 0x68,
    DW_OP_reg25 = 0x69,
    DW_OP_reg26 = 0x6a,
    DW_OP_reg27 = 0x6b,
    DW_OP_reg28 = 0x6c,
    DW_OP_reg29 = 0x6d,
    DW_OP_reg30 = 0x6e,
    DW_OP_reg31 = 0x6f,
    DW_OP_breg0 = 0x70,
    DW_OP_breg1 = 0x71,
    DW_OP_breg2 = 0x72,
    DW_OP_breg3 = 0x73,
    DW_OP_breg4 = 0x74,
    DW_OP_breg5 = 0x75,
    DW_OP_breg6 = 0x76,
    DW_OP_breg7 = 0x77,
    DW_OP_breg8 = 0x78,
    DW_OP_breg9 = 0x79,
    DW_OP_breg10 = 0x7a,
    DW_OP_breg11 = 0x7b,
    DW_OP_breg12 = 0x7c,
    DW_OP_breg13 = 0x7d,
    DW_OP_breg14 = 0x7e,
    DW_OP_breg15 = 0x7f,
    DW_OP_breg16 = 0x80,
    DW_OP_breg17 = 0x81,
    DW_OP_breg18 = 0x82,
    DW_OP_breg19 = 0x83,
    DW_OP_breg20 = 0x84,
    DW_OP_breg21 = 0x85,
    DW_OP_breg22 = 0x86,
    DW_OP_breg23 = 0x87,
    DW_OP_breg24 = 0x88,
    DW_OP_breg25 = 0x89,
    DW_OP_breg26 = 0x8a,
    DW_OP_breg27 = 0x8b,
    DW_OP_breg28 = 0x8c,
    DW_OP_breg29 = 0x8d,
    DW_OP_breg30 = 0x8e,
    DW_OP_breg31 = 0x8f,
    DW_OP_regx = 0x90,
    DW_OP_fbreg = 0x91,
    DW_OP_bregx = 0x92,
    DW_OP_piece = 0x93,
    DW_OP_deref_size = 0x94,
    DW_OP_xderef_size = 0x95,
    DW_OP_nop = 0x96,
    /* DWARF 3 extensions.  */
    DW_OP_push_object_address = 0x97,
    DW_OP_call2 = 0x98,
    DW_OP_call4 = 0x99,
    DW_OP_call_ref = 0x9a,
    DW_OP_form_tls_address = 0x9b,
    DW_OP_call_frame_cfa = 0x9c,
    DW_OP_bit_piece = 0x9d,
    /* DWARF 4 extensions.  */
    DW_OP_implicit_value = 0x9e,
    DW_OP_stack_value = 0x9f,
    /* DWARF 5 extensions.  */
    DW_OP_implicit_pointer = 0xa0,
    DW_OP_addrx = 0xa1,
    DW_OP_constx = 0xa2,
    DW_OP_entry_value = 0xa3,
    DW_OP_const_type = 0xa4,
    DW_OP_regval_type = 0xa5,
    DW_OP_deref_type = 0xa6,
    DW_OP_xderef_type = 0xa7,
    DW_OP_convert = 0xa8,
    DW_OP_reinterpret = 0xa9,
    /* GNU extensions.  */
    DW_OP_GNU_push_tls_address = 0xe0,
    DW_OP_GNU_uninit = 0xf0,
    DW_OP_GNU_encoded_addr = 0xf1,
    DW_OP_GNU_implicit_pointer = 0xf2,
    DW_OP_GNU_entry_value = 0xf3,
    DW_OP_GNU_const_type = 0xf4,
    DW_OP_GNU_regval_type = 0xf5,
    DW_OP_GNU_deref_type = 0xf6,
    DW_OP_GNU_convert = 0xf7,
    DW_OP_GNU_reinterpret = 0xf9,
    DW_OP_GNU_parameter_ref = 0xfa,
    /* GNU Debug Fission extensions.  */
    DW_OP_GNU_addr_index = 0xfb,
    DW_OP_GNU_const_index = 0xfc,
    /* The GNU variable value extension.
       See http://dwarfstd.org/ShowIssue.php?issue=161109.2 . */
    DW_OP_GNU_variable_value = 0xfd,
    /* HP extensions.  */
    DW_OP_HP_unknown     = 0xe0, /* Ouch, the same as GNU_push_tls_address.  */
    DW_OP_HP_is_value    = 0xe1,
    DW_OP_HP_fltconst4   = 0xe2,
    DW_OP_HP_fltconst8   = 0xe3,
    DW_OP_HP_mod_range   = 0xe4,
    DW_OP_HP_unmod_range = 0xe5,
    DW_OP_HP_tls         = 0xe6
  }
  DW_OP;

typedef enum
  {
    DW_UT_compile = 0x01,
    DW_UT_type = 0x02,
    DW_UT_partial = 0x03,
    DW_UT_skeleton = 0x04,
    DW_UT_split_compile = 0x05,
    DW_UT_split_type = 0x06,

    DW_UT_lo_user = 0x80,
    DW_UT_hi_user = 0xff
  }
  DW_UT;

typedef enum
  {
    DW_LNCT_path = 0x1,
    DW_LNCT_directory_index = 0x2,
    DW_LNCT_timestamp = 0x3,
    DW_LNCT_size = 0x4,
    DW_LNCT_MD5 = 0x5,

    DW_LNCT_lo_user = 0x2000,
    DW_LNCT_hi_user = 0x3fff
  }
  DW_LNCT;

typedef enum
  {
    DW_RLE_end_of_list = 0x00,
    DW_RLE_base_addressx = 0x01,
    DW_RLE_startx_endx = 0x02,
    DW_RLE_startx_length = 0x03,
    DW_RLE_offset_pair = 0x04,
    DW_RLE_base_address = 0x05,
    DW_RLE_start_end = 0x06,
    DW_RLE_start_length = 0x07
  }
  DW_RLE;

typedef enum
  {
    DW_LLE_end_of_list = 0x00,
    DW_LLE_base_addressx = 0x01,
    DW_LLE_startx_endx = 0x02,
    DW_LLE_startx_length = 0x03,
    DW_LLE_offset_pair = 0x04,
    DW_LLE_default_location = 0x05,
    DW_LLE_base_address = 0x06,
    DW_LLE_start_end = 0x07,
    DW_LLE_start_length = 0x08,

    DW_LLE_GNU_view_pair = 0x09
  }
  DW_LLE;

const HChar* ML_(pp_DW_children) ( DW_children hashch );
const HChar* ML_(pp_DW_TAG)      ( DW_TAG tag );
const HChar* ML_(pp_DW_FORM)     ( DW_FORM form );
const HChar* ML_(pp_DW_AT)       ( DW_AT attr );
const HChar* ML_(pp_DW_LLE)      ( DW_LLE entry );
const HChar* ML_(pp_DW_RLE)      ( DW_RLE entry );


/* --- To do with evaluation of Dwarf expressions --- */

/* Guarded Dwarf3 expressions, which can be linked together to form a
   list.  The payload field contains a variable length array of bytes
   which hold the guarded expressions.  The length can be inferred by
   inspecting the payload bytes and so does not need to be stored
   explicitly.

   Guarded-Expression format is similar but not identical to the
   DWARF3 location-list format.  The format of each returned block is:

      UChar biasMe;
      UChar isEnd;
      followed by zero or more of

      (Addr aMin;  Addr aMax;  UShort nbytes;  ..bytes..;  UChar isEnd)

   '..bytes..' is an standard DWARF3 location expression which is
   valid when aMin <= pc <= aMax (possibly after suitable biasing).

   The number of bytes in '..bytes..' is nbytes.

   The end of the sequence is marked by an isEnd == 1 value.  All
   previous isEnd values must be zero.

   biasMe is 1 if the aMin/aMax fields need this DebugInfo's text_bias
   added before use, and 0 if the GX is this is not necessary (is
   ready to go).

   Hence the block can be quickly parsed and is self-describing.  Note
   that aMax is 1 less than the corresponding value in a DWARF3
   location list.  Zero length ranges, with aMax == aMin-1, are not
   allowed.
*/
typedef
   struct _GExpr { 
      UChar payload[0];
   }
   GExpr;

/* Show a so-called guarded expression */
void ML_(pp_GX) ( const GExpr* gx );

/* Evaluation of a DWARF3 expression (and hence of a GExpr) may
   require knowing a suitably contextualising set of values for the
   instruction, frame and stack pointers (and, in general, all
   registers, though we punt on such generality here).  Here's a
   struct to carry the bare essentials.  ip, fp and sp are expected to
   be provided for all platforms. */
typedef
   struct { Addr ip; Addr sp; Addr fp; }
   RegSummary;

/* This describes the result of evaluating a DWARF3 expression.
   GXR_Failure: failed; .word is an asciiz string summarising why
   GXR_Addr:    evaluated to an address of the object, in .word
   GXR_Value:   evaluated to a value, in .word
   GXR_RegNo:   evaluated to a DWARF3 register number, in .word
*/
typedef
   struct { 
      enum { GXR_Failure, GXR_Addr, GXR_Value, GXR_RegNo } kind;
      UWord word;
   }
   GXResult;

void ML_(pp_GXResult) ( GXResult res );

/* Evaluate a guarded expression.  If regs is NULL, then gx is assumed
   (and checked) to contain just a single guarded expression, with a
   guard which covers the entire address space and so always evaluates
   to True (iow, gx is a single unconditional expression).  If regs is
   non-NULL then its .ip value is used to select which of the
   embedded DWARF3 location expressions to use, and that is duly
   evaluated.

   If as part of the evaluation, a frame base value needs to be
   computed, then fbGX can provide an expression for it.  If fbGX is
   NULL but the frame base is still needed, then evaluation of gx as a
   whole will fail. */
GXResult ML_(evaluate_GX)( const GExpr* gx, const GExpr* fbGX,
                           const RegSummary* regs, const DebugInfo* di );

/* This is a subsidiary of ML_(evaluate_GX), which just evaluates a
   single standard DWARF3 expression.  Conventions w.r.t regs and fbGX
   are as for ML_(evaluate_GX).  If push_initial_zero is True, then an
   initial zero word is pushed on the evaluation stack at the start.
   This is needed for computing structure field offsets.  Note that
   ML_(evaluate_GX) and ML_(evaluate_Dwarf3_Expr) are mutually
   recursive. */
GXResult ML_(evaluate_Dwarf3_Expr) ( const UChar* expr, UWord exprszB, 
                                     const GExpr* fbGX, const RegSummary* regs,
                                     const DebugInfo* di,
                                     Bool push_initial_zero );

/* Evaluate a very simple Guarded (DWARF3) expression.  The expression
   is expected to denote a constant, with no reference to any
   registers nor to any frame base expression.  GXR_Failure is
   returned if there is more than one guard, or none, a register
   location is denoted, a frame base expression is required, or the
   expression is not manifestly a constant.  The range of addresses
   covered by the guard is also ignored. */
GXResult ML_(evaluate_trivial_GX)( const GExpr* gx, const DebugInfo* di );

/* Compute call frame address (CFA) for IP/SP/FP.  */
Addr ML_(get_CFA) ( Addr ip, Addr sp, Addr fp,
                    Addr min_accessible, Addr max_accessible );

#endif /* ndef __PRIV_D3BASICS_H */

/*--------------------------------------------------------------------*/
/*--- end                                          priv_d3basics.h ---*/
/*--------------------------------------------------------------------*/
