
/*--------------------------------------------------------------------*/
/*--- Basic definitions and helper functions for DWARF3.           ---*/
/*---                                                   d3basics.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2011 OpenWorks LLP
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "pub_core_basics.h"
#include "pub_core_debuginfo.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_options.h"
#include "pub_core_xarray.h"

#include "pub_core_vki.h"       /* VKI_PROT_READ */
#include "pub_core_aspacemgr.h" /* VG_(is_valid_for_client) */

#include "priv_misc.h"
#include "priv_d3basics.h"      /* self */
#include "priv_storage.h"

HChar* ML_(pp_DW_children) ( DW_children hashch )
{
   switch (hashch) {
      case DW_children_no:  return "no children";
      case DW_children_yes: return "has children";
      default:              return "DW_children_???";
   }
}

HChar* ML_(pp_DW_TAG) ( DW_TAG tag )
{
   switch (tag) {
      case DW_TAG_padding:            return "DW_TAG_padding";
      case DW_TAG_array_type:         return "DW_TAG_array_type";
      case DW_TAG_class_type:         return "DW_TAG_class_type";
      case DW_TAG_entry_point:        return "DW_TAG_entry_point";
      case DW_TAG_enumeration_type:   return "DW_TAG_enumeration_type";
      case DW_TAG_formal_parameter:   return "DW_TAG_formal_parameter";
      case DW_TAG_imported_declaration: 
         return "DW_TAG_imported_declaration";
      case DW_TAG_label:              return "DW_TAG_label";
      case DW_TAG_lexical_block:      return "DW_TAG_lexical_block";
      case DW_TAG_member:             return "DW_TAG_member";
      case DW_TAG_pointer_type:       return "DW_TAG_pointer_type";
      case DW_TAG_reference_type:     return "DW_TAG_reference_type";
      case DW_TAG_compile_unit:       return "DW_TAG_compile_unit";
      case DW_TAG_string_type:        return "DW_TAG_string_type";
      case DW_TAG_structure_type:     return "DW_TAG_structure_type";
      case DW_TAG_subroutine_type:    return "DW_TAG_subroutine_type";
      case DW_TAG_typedef:            return "DW_TAG_typedef";
      case DW_TAG_union_type:         return "DW_TAG_union_type";
      case DW_TAG_unspecified_parameters: 
         return "DW_TAG_unspecified_parameters";
      case DW_TAG_variant:            return "DW_TAG_variant";
      case DW_TAG_common_block:       return "DW_TAG_common_block";
      case DW_TAG_common_inclusion:   return "DW_TAG_common_inclusion";
      case DW_TAG_inheritance:        return "DW_TAG_inheritance";
      case DW_TAG_inlined_subroutine:
         return "DW_TAG_inlined_subroutine";
      case DW_TAG_module:             return "DW_TAG_module";
      case DW_TAG_ptr_to_member_type: return "DW_TAG_ptr_to_member_type";
      case DW_TAG_set_type:           return "DW_TAG_set_type";
      case DW_TAG_subrange_type:      return "DW_TAG_subrange_type";
      case DW_TAG_with_stmt:          return "DW_TAG_with_stmt";
      case DW_TAG_access_declaration: return "DW_TAG_access_declaration";
      case DW_TAG_base_type:          return "DW_TAG_base_type";
      case DW_TAG_catch_block:        return "DW_TAG_catch_block";
      case DW_TAG_const_type:         return "DW_TAG_const_type";
      case DW_TAG_constant:           return "DW_TAG_constant";
      case DW_TAG_enumerator:         return "DW_TAG_enumerator";
      case DW_TAG_file_type:          return "DW_TAG_file_type";
      case DW_TAG_friend:             return "DW_TAG_friend";
      case DW_TAG_namelist:           return "DW_TAG_namelist";
      case DW_TAG_namelist_item:      return "DW_TAG_namelist_item";
      case DW_TAG_packed_type:        return "DW_TAG_packed_type";
      case DW_TAG_subprogram:         return "DW_TAG_subprogram";
      case DW_TAG_template_type_param:
         return "DW_TAG_template_type_param";
      case DW_TAG_template_value_param:
         return "DW_TAG_template_value_param";
      case DW_TAG_thrown_type:        return "DW_TAG_thrown_type";
      case DW_TAG_try_block:          return "DW_TAG_try_block";
      case DW_TAG_variant_part:       return "DW_TAG_variant_part";
      case DW_TAG_variable:           return "DW_TAG_variable";
      case DW_TAG_volatile_type:      return "DW_TAG_volatile_type";
      /* DWARF 3.  */
      case DW_TAG_dwarf_procedure:    return "DW_TAG_dwarf_procedure";
      case DW_TAG_restrict_type:      return "DW_TAG_restrict_type";
      case DW_TAG_interface_type:     return "DW_TAG_interface_type";
      case DW_TAG_namespace:          return "DW_TAG_namespace";
      case DW_TAG_imported_module:    return "DW_TAG_imported_module";
      case DW_TAG_unspecified_type:   return "DW_TAG_unspecified_type";
      case DW_TAG_partial_unit:       return "DW_TAG_partial_unit";
      case DW_TAG_imported_unit:      return "DW_TAG_imported_unit";
      case DW_TAG_condition:          return "DW_TAG_condition";
      case DW_TAG_shared_type:        return "DW_TAG_shared_type";
      /* DWARF 4.  */
      case DW_TAG_type_unit:          return "DW_TAG_type_unit";
      case DW_TAG_rvalue_reference_type: return "DW_TAG_rvalue_reference_type";
      case DW_TAG_template_alias:     return "DW_TAG_template_alias";
      /* SGI/MIPS Extensions.  */
      case DW_TAG_MIPS_loop:          return "DW_TAG_MIPS_loop";
      /* HP extensions.  See:
         ftp://ftp.hp.com/pub/lang/tools/WDB/wdb-4.0.tar.gz .  */
      case DW_TAG_HP_array_descriptor:
         return "DW_TAG_HP_array_descriptor";
      /* GNU extensions.  */
      case DW_TAG_format_label:       return "DW_TAG_format_label";
      case DW_TAG_function_template:  return "DW_TAG_function_template";
      case DW_TAG_class_template:     return "DW_TAG_class_template";
      case DW_TAG_GNU_BINCL:          return "DW_TAG_GNU_BINCL";
      case DW_TAG_GNU_EINCL:          return "DW_TAG_GNU_EINCL";
      /* Extensions for UPC.  See: http://upc.gwu.edu/~upc.  */
      case DW_TAG_upc_shared_type:    return "DW_TAG_upc_shared_type";
      case DW_TAG_upc_strict_type:    return "DW_TAG_upc_strict_type";
      case DW_TAG_upc_relaxed_type:   return "DW_TAG_upc_relaxed_type";
      /* PGI (STMicroelectronics) extensions.  No documentation available.  */
      case DW_TAG_PGI_kanji_type:     return "DW_TAG_PGI_kanji_type";
      case DW_TAG_PGI_interface_block:
         return "DW_TAG_PGI_interface_block";
      default:                        return "DW_TAG_???";
   }
}

HChar* ML_(pp_DW_FORM) ( DW_FORM form )
{
   switch (form) {
      case DW_FORM_addr:      return "DW_FORM_addr";
      case DW_FORM_block2:    return "DW_FORM_block2";
      case DW_FORM_block4:    return "DW_FORM_block4";
      case DW_FORM_data2:     return "DW_FORM_data2";
      case DW_FORM_data4:     return "DW_FORM_data4";
      case DW_FORM_data8:     return "DW_FORM_data8";
      case DW_FORM_string:    return "DW_FORM_string";
      case DW_FORM_block:     return "DW_FORM_block";
      case DW_FORM_block1:    return "DW_FORM_block1";
      case DW_FORM_data1:     return "DW_FORM_data1";
      case DW_FORM_flag:      return "DW_FORM_flag";
      case DW_FORM_sdata:     return "DW_FORM_sdata";
      case DW_FORM_strp:      return "DW_FORM_strp";
      case DW_FORM_udata:     return "DW_FORM_udata";
      case DW_FORM_ref_addr:  return "DW_FORM_ref_addr";
      case DW_FORM_ref1:      return "DW_FORM_ref1";
      case DW_FORM_ref2:      return "DW_FORM_ref2";
      case DW_FORM_ref4:      return "DW_FORM_ref4";
      case DW_FORM_ref8:      return "DW_FORM_ref8";
      case DW_FORM_ref_udata: return "DW_FORM_ref_udata";
      case DW_FORM_indirect:  return "DW_FORM_indirect";
      case DW_FORM_sec_offset:return "DW_FORM_sec_offset";
      case DW_FORM_exprloc:   return "DW_FORM_exprloc";
      case DW_FORM_flag_present:return "DW_FORM_flag_present";
      case DW_FORM_ref_sig8:  return "DW_FORM_ref_sig8";
      default:                return "DW_FORM_???";
   }
}

HChar* ML_(pp_DW_AT) ( DW_AT attr )
{
   switch (attr) {
      case DW_AT_sibling:             return "DW_AT_sibling";
      case DW_AT_location:            return "DW_AT_location";
      case DW_AT_name: return "DW_AT_name";
      case DW_AT_ordering: return "DW_AT_ordering";
      case DW_AT_subscr_data: return "DW_AT_subscr_data";
      case DW_AT_byte_size: return "DW_AT_byte_size";
      case DW_AT_bit_offset: return "DW_AT_bit_offset";
      case DW_AT_bit_size: return "DW_AT_bit_size";
      case DW_AT_element_list: return "DW_AT_element_list";
      case DW_AT_stmt_list: return "DW_AT_stmt_list";
      case DW_AT_low_pc: return "DW_AT_low_pc";
      case DW_AT_high_pc: return "DW_AT_high_pc";
      case DW_AT_language: return "DW_AT_language";
      case DW_AT_member: return "DW_AT_member";
      case DW_AT_discr: return "DW_AT_discr";
      case DW_AT_discr_value: return "DW_AT_discr_value";
      case DW_AT_visibility: return "DW_AT_visibility";
      case DW_AT_import: return "DW_AT_import";
      case DW_AT_string_length: return "DW_AT_string_length";
      case DW_AT_common_reference: return "DW_AT_common_reference";
      case DW_AT_comp_dir: return "DW_AT_comp_dir";
      case DW_AT_const_value: return "DW_AT_const_value";
      case DW_AT_containing_type: return "DW_AT_containing_type";
      case DW_AT_default_value: return "DW_AT_default_value";
      case DW_AT_inline: return "DW_AT_inline";
      case DW_AT_is_optional: return "DW_AT_is_optional";
      case DW_AT_lower_bound: return "DW_AT_lower_bound";
      case DW_AT_producer: return "DW_AT_producer";
      case DW_AT_prototyped: return "DW_AT_prototyped";
      case DW_AT_return_addr: return "DW_AT_return_addr";
      case DW_AT_start_scope: return "DW_AT_start_scope";
      case DW_AT_stride_size: return "DW_AT_stride_size";
      case DW_AT_upper_bound: return "DW_AT_upper_bound";
      case DW_AT_abstract_origin: return "DW_AT_abstract_origin";
      case DW_AT_accessibility: return "DW_AT_accessibility";
      case DW_AT_address_class: return "DW_AT_address_class";
      case DW_AT_artificial: return "DW_AT_artificial";
      case DW_AT_base_types: return "DW_AT_base_types";
      case DW_AT_calling_convention: return "DW_AT_calling_convention";
      case DW_AT_count: return "DW_AT_count";
      case DW_AT_data_member_location: return "DW_AT_data_member_location";
      case DW_AT_decl_column: return "DW_AT_decl_column";
      case DW_AT_decl_file: return "DW_AT_decl_file";
      case DW_AT_decl_line: return "DW_AT_decl_line";
      case DW_AT_declaration: return "DW_AT_declaration";
      case DW_AT_discr_list: return "DW_AT_discr_list";
      case DW_AT_encoding: return "DW_AT_encoding";
      case DW_AT_external: return "DW_AT_external";
      case DW_AT_frame_base: return "DW_AT_frame_base";
      case DW_AT_friend: return "DW_AT_friend";
      case DW_AT_identifier_case: return "DW_AT_identifier_case";
      case DW_AT_macro_info: return "DW_AT_macro_info";
      case DW_AT_namelist_items: return "DW_AT_namelist_items";
      case DW_AT_priority: return "DW_AT_priority";
      case DW_AT_segment: return "DW_AT_segment";
      case DW_AT_specification: return "DW_AT_specification";
      case DW_AT_static_link: return "DW_AT_static_link";
      case DW_AT_type: return "DW_AT_type";
      case DW_AT_use_location: return "DW_AT_use_location";
      case DW_AT_variable_parameter: return "DW_AT_variable_parameter";
      case DW_AT_virtuality: return "DW_AT_virtuality";
      case DW_AT_vtable_elem_location: return "DW_AT_vtable_elem_location";
      /* DWARF 3 values.  */
      case DW_AT_allocated: return "DW_AT_allocated";
      case DW_AT_associated: return "DW_AT_associated";
      case DW_AT_data_location: return "DW_AT_data_location";
      case DW_AT_stride: return "DW_AT_stride";
      case DW_AT_entry_pc: return "DW_AT_entry_pc";
      case DW_AT_use_UTF8: return "DW_AT_use_UTF8";
      case DW_AT_extension: return "DW_AT_extension";
      case DW_AT_ranges: return "DW_AT_ranges";
      case DW_AT_trampoline: return "DW_AT_trampoline";
      case DW_AT_call_column: return "DW_AT_call_column";
      case DW_AT_call_file: return "DW_AT_call_file";
      case DW_AT_call_line: return "DW_AT_call_line";
      case DW_AT_description: return "DW_AT_description";
      case DW_AT_binary_scale: return "DW_AT_binary_scale";
      case DW_AT_decimal_scale: return "DW_AT_decimal_scale";
      case DW_AT_small: return "DW_AT_small";
      case DW_AT_decimal_sign: return "DW_AT_decimal_sign";
      case DW_AT_digit_count: return "DW_AT_digit_count";
      case DW_AT_picture_string: return "DW_AT_picture_string";
      case DW_AT_mutable: return "DW_AT_mutable";
      case DW_AT_threads_scaled: return "DW_AT_threads_scaled";
      case DW_AT_explicit: return "DW_AT_explicit";
      case DW_AT_object_pointer: return "DW_AT_object_pointer";
      case DW_AT_endianity: return "DW_AT_endianity";
      case DW_AT_elemental: return "DW_AT_elemental";
      case DW_AT_pure: return "DW_AT_pure";
      case DW_AT_recursive: return "DW_AT_recursive";
      /* DWARF 4 values.  */
      case DW_AT_signature: return "DW_AT_signature";
      case DW_AT_main_subprogram: return "DW_AT_main_subprogram";
      case DW_AT_data_bit_offset: return "DW_AT_data_bit_offset";
      case DW_AT_const_expr: return "DW_AT_const_expr";
      case DW_AT_enum_class: return "DW_AT_enum_class";
      case DW_AT_linkage_name: return "DW_AT_linkage_name";
      /* SGI/MIPS extensions.  */
      /* case DW_AT_MIPS_fde: return "DW_AT_MIPS_fde"; */
      /* DW_AT_MIPS_fde == DW_AT_HP_unmodifiable */
      case DW_AT_MIPS_loop_begin: return "DW_AT_MIPS_loop_begin";
      case DW_AT_MIPS_tail_loop_begin: return "DW_AT_MIPS_tail_loop_begin";
      case DW_AT_MIPS_epilog_begin: return "DW_AT_MIPS_epilog_begin";
      case DW_AT_MIPS_loop_unroll_factor: return "DW_AT_MIPS_loop_unroll_factor";
      case DW_AT_MIPS_software_pipeline_depth: return "DW_AT_MIPS_software_pipeline_depth";
      case DW_AT_MIPS_linkage_name: return "DW_AT_MIPS_linkage_name";
      case DW_AT_MIPS_stride: return "DW_AT_MIPS_stride";
      case DW_AT_MIPS_abstract_name: return "DW_AT_MIPS_abstract_name";
      case DW_AT_MIPS_clone_origin: return "DW_AT_MIPS_clone_origin";
      case DW_AT_MIPS_has_inlines: return "DW_AT_MIPS_has_inlines";
      /* HP extensions.  */
      case DW_AT_HP_block_index: return "DW_AT_HP_block_index";
      case DW_AT_HP_unmodifiable: return "DW_AT_HP_unmodifiable";
      case DW_AT_HP_actuals_stmt_list: return "DW_AT_HP_actuals_stmt_list";
      case DW_AT_HP_proc_per_section: return "DW_AT_HP_proc_per_section";
      case DW_AT_HP_raw_data_ptr: return "DW_AT_HP_raw_data_ptr";
      case DW_AT_HP_pass_by_reference: return "DW_AT_HP_pass_by_reference";
      case DW_AT_HP_opt_level: return "DW_AT_HP_opt_level";
      case DW_AT_HP_prof_version_id: return "DW_AT_HP_prof_version_id";
      case DW_AT_HP_opt_flags: return "DW_AT_HP_opt_flags";
      case DW_AT_HP_cold_region_low_pc: return "DW_AT_HP_cold_region_low_pc";
      case DW_AT_HP_cold_region_high_pc: return "DW_AT_HP_cold_region_high_pc";
      case DW_AT_HP_all_variables_modifiable: return "DW_AT_HP_all_variables_modifiable";
      case DW_AT_HP_linkage_name: return "DW_AT_HP_linkage_name";
      case DW_AT_HP_prof_flags: return "DW_AT_HP_prof_flags";
      /* GNU extensions.  */
      case DW_AT_sf_names: return "DW_AT_sf_names";
      case DW_AT_src_info: return "DW_AT_src_info";
      case DW_AT_mac_info: return "DW_AT_mac_info";
      case DW_AT_src_coords: return "DW_AT_src_coords";
      case DW_AT_body_begin: return "DW_AT_body_begin";
      case DW_AT_body_end: return "DW_AT_body_end";
      case DW_AT_GNU_vector: return "DW_AT_GNU_vector";
      /* VMS extensions.  */
      case DW_AT_VMS_rtnbeg_pd_address: return "DW_AT_VMS_rtnbeg_pd_address";
      /* UPC extension.  */
      case DW_AT_upc_threads_scaled: return "DW_AT_upc_threads_scaled";
      /* PGI (STMicroelectronics) extensions.  */
      case DW_AT_PGI_lbase: return "DW_AT_PGI_lbase";
      case DW_AT_PGI_soffset: return "DW_AT_PGI_soffset";
      case DW_AT_PGI_lstride: return "DW_AT_PGI_lstride";
      default: return "DW_AT_???";
   }
}


/* ------ To do with evaluation of Dwarf expressions ------ */

/* FIXME: duplicated in readdwarf.c */
static 
ULong read_leb128 ( UChar* data, Int* length_return, Int sign )
{
  ULong  result = 0;
  UInt   num_read = 0;
  Int    shift = 0;
  UChar  byte;

  vg_assert(sign == 0 || sign == 1);

  do
    {
      byte = * data ++;
      num_read ++;

      result |= ((ULong)(byte & 0x7f)) << shift;

      shift += 7;

    }
  while (byte & 0x80);

  if (length_return != NULL)
    * length_return = num_read;

  if (sign && (shift < 64) && (byte & 0x40))
    result |= -(1ULL << shift);

  return result;
}

/* Small helper functions easier to use
 * value is returned and the given pointer is
 * moved past end of leb128 data */
/* FIXME: duplicated in readdwarf.c */
static ULong read_leb128U( UChar **data )
{
  Int len;
  ULong val = read_leb128( *data, &len, 0 );
  *data += len;
  return val;
}

/* Same for signed data */
/* FIXME: duplicated in readdwarf.c */
static Long read_leb128S( UChar **data )
{
   Int len;
   ULong val = read_leb128( *data, &len, 1 );
   *data += len;
   return (Long)val;
}

/* FIXME: duplicates logic in readdwarf.c: copy_convert_CfiExpr_tree
   and {FP,SP}_REG decls */
static Bool get_Dwarf_Reg( /*OUT*/Addr* a, Word regno, RegSummary* regs )
{
   vg_assert(regs);
#  if defined(VGP_x86_linux) || defined(VGP_x86_darwin)
   if (regno == 5/*EBP*/) { *a = regs->fp; return True; }
   if (regno == 4/*ESP*/) { *a = regs->sp; return True; }
#  elif defined(VGP_amd64_linux) || defined(VGP_amd64_darwin)
   if (regno == 6/*RBP*/) { *a = regs->fp; return True; }
   if (regno == 7/*RSP*/) { *a = regs->sp; return True; }
#  elif defined(VGP_ppc32_linux)
   if (regno == 1/*SP*/) { *a = regs->sp; return True; }
#  elif defined(VGP_ppc64_linux)
   if (regno == 1/*SP*/) { *a = regs->sp; return True; }
#  elif defined(VGP_arm_linux)
   if (regno == 13) { *a = regs->sp; return True; }
   if (regno == 11) { *a = regs->fp; return True; } 
#  elif defined(VGP_s390x_linux)
   if (regno == 15) { *a = regs->sp; return True; }
   if (regno == 11) { *a = regs->fp; return True; }
#  else
#    error "Unknown platform"
#  endif
   return False;
}

/* Convert a stated address to an actual address */
static Bool bias_address( Addr* a, const DebugInfo* di )
{
   if (di->text_present
       && di->text_size > 0
       && *a >= di->text_debug_svma && *a < di->text_debug_svma + di->text_size) {
      *a += di->text_debug_bias;
   }
   else if (di->data_present
            && di->data_size > 0
            && *a >= di->data_debug_svma && *a < di->data_debug_svma + di->data_size) {
      *a += di->data_debug_bias;
   }
   else if (di->sdata_present
            && di->sdata_size > 0
            && *a >= di->sdata_debug_svma && *a < di->sdata_debug_svma + di->sdata_size) {
      *a += di->sdata_debug_bias;
   }
   else if (di->rodata_present
            && di->rodata_size > 0
            && *a >= di->rodata_debug_svma && *a < di->rodata_debug_svma + di->rodata_size) {
      *a += di->rodata_debug_bias;
   }
   else if (di->bss_present
            && di->bss_size > 0
            && *a >= di->bss_debug_svma && *a < di->bss_debug_svma + di->bss_size) {
      *a += di->bss_debug_bias;
   }
   else if (di->sbss_present
            && di->sbss_size > 0
            && *a >= di->sbss_debug_svma && *a < di->sbss_debug_svma + di->sbss_size) {
      *a += di->sbss_debug_bias;
   }
   else {
      return False;
   }

   return True;
}


/* Evaluate a standard DWARF3 expression.  See detailed description in
   priv_d3basics.h.  Doesn't handle DW_OP_piece/DW_OP_bit_piece yet.  */
GXResult ML_(evaluate_Dwarf3_Expr) ( UChar* expr, UWord exprszB, 
                                     GExpr* fbGX, RegSummary* regs,
                                     const DebugInfo* di,
                                     Bool push_initial_zero )
{
#  define N_EXPR_STACK 20

#  define FAIL(_str)                                          \
      do {                                                    \
         res.kind = GXR_Failure;                              \
         res.word = (UWord)(_str);                            \
         return res;                                          \
      } while (0)

#  define PUSH(_arg)                                          \
      do {                                                    \
         vg_assert(sp >= -1 && sp < N_EXPR_STACK);            \
         if (sp == N_EXPR_STACK-1)                            \
            FAIL("evaluate_Dwarf3_Expr: stack overflow(1)");  \
         sp++;                                                \
         stack[sp] = (_arg);                                  \
      } while (0)

#  define POP(_lval)                                          \
      do {                                                    \
         vg_assert(sp >= -1 && sp < N_EXPR_STACK);            \
         if (sp == -1)                                        \
            FAIL("evaluate_Dwarf3_Expr: stack underflow(1)"); \
         _lval = stack[sp];                                   \
         sp--;                                                \
      } while (0)

   UChar    opcode;
   UChar*   limit;
   Int      sp; /* # of top element: valid is -1 .. N_EXPR_STACK-1 */
   Addr     stack[N_EXPR_STACK]; /* stack of addresses, as per D3 spec */
   GXResult fbval, res;
   Addr     a1;
   Word     sw1, sw2;
   UWord    uw1, uw2;
   Bool     ok;

   sp = -1;
   vg_assert(expr);
   vg_assert(exprszB >= 0);
   limit = expr + exprszB;

   /* Deal with the case where the entire expression is a single
      Register Name Operation (D3 spec sec 2.6.1).  Then the
      denotation of the expression as a whole is a register name. */
   if (exprszB == 1
       && expr[0] >= DW_OP_reg0 && expr[0] <= DW_OP_reg31) {
      res.kind = GXR_RegNo;
      res.word = (UWord)(expr[0] - DW_OP_reg0);
      return res;
   }
   if (exprszB > 1
       && expr[0] == DW_OP_regx) {
      /* JRS: 2008Feb20: I believe the following is correct, but would
         like to see a test case show up before enabling it. */
      expr++;
      res.kind = GXR_RegNo;
      res.word = (UWord)read_leb128U( &expr );
      if (expr != limit)
         FAIL("evaluate_Dwarf3_Expr: DW_OP_regx*: invalid expr size");
      else
         return res;
      /*NOTREACHED*/
   }

   /* Evidently this expression denotes a value, not a register name.
      So evaluate it accordingly. */

   if (push_initial_zero)
      PUSH(0);

   while (True) {

      vg_assert(sp >= -1 && sp < N_EXPR_STACK);

      if (expr > limit) 
         /* overrun - something's wrong */
         FAIL("evaluate_Dwarf3_Expr: ran off end of expr");

      if (expr == limit) {
         /* end of expr - return expr on the top of stack. */
         if (sp == -1)
            /* stack empty.  Bad. */
            FAIL("evaluate_Dwarf3_Expr: stack empty at end of expr");
         else
            break;
      }

      opcode = *expr++;
      switch (opcode) {
         case DW_OP_addr:
            /* Presumably what is given in the Dwarf3 is a SVMA (how
               could it be otherwise?)  So we add the appropriate bias
               on before pushing the result. */
            a1 = ML_(read_Addr)(expr);
            if (bias_address(&a1, di)) {
               PUSH( a1 ); 
               expr += sizeof(Addr);
            }
            else {
               FAIL("evaluate_Dwarf3_Expr: DW_OP_addr with address "
                    "in unknown section");
            }
            break;
         case DW_OP_fbreg:
            if (!fbGX)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_fbreg with "
                    "no expr for fbreg present");
            fbval = ML_(evaluate_GX)(fbGX, NULL, regs, di);
            /* Convert fbval into something we can use.  If we got a
               Value, no problem.  However, as per D3 spec sec 3.3.5
               (Low Level Information) sec 2, we could also get a
               RegNo, and that is taken to mean the value in the
               indicated register.  So we have to manually
               "dereference" it. */
            a1 = 0;
            switch (fbval.kind) {
               case GXR_Failure:
                  return fbval; /* propagate failure */
               case GXR_Addr:
                  a1 = fbval.word; break; /* use as-is */
               case GXR_RegNo:
                  ok = get_Dwarf_Reg( &a1, fbval.word, regs );
                  if (!ok) return fbval; /* propagate failure */
                  break;
               case GXR_Value:
                  FAIL("evaluate_Dwarf3_Expr: DW_OP_{implicit,stack}_value "
                       "in DW_AT_frame_base");
               default:
                  vg_assert(0);
            }
            sw1 = (Word)read_leb128S( &expr );
            PUSH( a1 + sw1 );
            break;
         /* DW_OP_breg* denotes 'contents of specified register, plus
            constant offset'.  So provided we know what the register's
            value is, we can evaluate this.  Contrast DW_OP_reg*,
            which indicates that denoted location is in a register
            itself.  If DW_OP_reg* shows up here the expression is
            malformed, since we are evaluating for value now, and
            DW_OP_reg* denotes a register location, not a value.  See
            D3 Spec sec 2.6.1 ("Register Name Operations") for
            details. */
         case DW_OP_breg0 ... DW_OP_breg31:
            if (!regs)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_breg* but no reg info");
            a1 = 0;
            if (!get_Dwarf_Reg( &a1, opcode - DW_OP_breg0, regs ))
               FAIL("evaluate_Dwarf3_Expr: unhandled DW_OP_breg*");
            sw1 = (Word)read_leb128S( &expr );
            a1 += sw1;
            PUSH( a1 );
            break;
         case DW_OP_bregx:
            if (!regs)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_bregx but no reg info");
            a1 = 0;
            uw1 = (UWord)read_leb128U( &expr );
            if (!get_Dwarf_Reg( &a1, uw1, regs ))
               FAIL("evaluate_Dwarf3_Expr: unhandled DW_OP_bregx reg value");
            sw1 = (Word)read_leb128S( &expr );
            a1 += sw1;
            PUSH( a1 );
            break;
         /* As per comment on DW_OP_breg*, the following denote that
            the value in question is in a register, not in memory.  So
            we simply return failure. (iow, the expression is
            malformed). */
         case DW_OP_reg0 ... DW_OP_reg31:
         case DW_OP_regx:
            FAIL("evaluate_Dwarf3_Expr: DW_OP_reg* "
                 "whilst evaluating for a value");
            break;
         case DW_OP_plus_uconst:
            POP(uw1);
            uw1 += (UWord)read_leb128U( &expr );
            PUSH(uw1);
            break;
         case DW_OP_GNU_push_tls_address:
            /* GDB contains the following cryptic comment: */
            /* Variable is at a constant offset in the thread-local
            storage block into the objfile for the current thread and
            the dynamic linker module containing this expression. Here
            we return returns the offset from that base.  The top of the
            stack has the offset from the beginning of the thread
            control block at which the variable is located.  Nothing
            should follow this operator, so the top of stack would be
            returned.  */
            /* But no spec resulting from Googling.  Punt for now. */
            FAIL("warning: evaluate_Dwarf3_Expr: unhandled "         
                 "DW_OP_GNU_push_tls_address");
            /*NOTREACHED*/
         case DW_OP_deref:
            POP(uw1);
            if (VG_(am_is_valid_for_client)( (Addr)uw1, sizeof(Addr),
                                             VKI_PROT_READ )) {
               uw1 = ML_(read_UWord)((void *)uw1);
               PUSH(uw1);
            } else {
               FAIL("warning: evaluate_Dwarf3_Expr: DW_OP_deref: "
                    "address not valid for client");
            }
            break;
         case DW_OP_deref_size:
            POP(uw1);
            uw2 = *expr++;
            if (VG_(am_is_valid_for_client)( (Addr)uw1, uw2,
                                             VKI_PROT_READ )) {
               switch (uw2) {
                 case 1: uw1 = ML_(read_UChar)((void*)uw1); break;
                 case 2: uw1 = ML_(read_UShort)((void*)uw1); break;
                 case 4: uw1 = ML_(read_UInt)((void*)uw1); break;
                 case 8: uw1 = ML_(read_ULong)((void*)uw1); break;
                 default:
                    FAIL("warning: evaluate_Dwarf3_Expr: unhandled "
                         "DW_OP_deref_size size");
               }
               PUSH(uw1);
            } else {
               FAIL("warning: evaluate_Dwarf3_Expr: DW_OP_deref_size: "
                    "address not valid for client");
            }
            break;
         case DW_OP_lit0 ... DW_OP_lit31:
            PUSH(opcode - DW_OP_lit0);
            break;
         case DW_OP_const1u:
	    uw1 = *expr++;
	    PUSH(uw1);
            break;
         case DW_OP_const2u:
	    uw1 = ML_(read_UShort)(expr);
	    expr += 2;
	    PUSH(uw1);
	    break;
         case DW_OP_const4u:
	    uw1 = ML_(read_UInt)(expr);
	    expr += 4;
	    PUSH(uw1);
	    break;
         case DW_OP_const8u:
	    uw1 = ML_(read_ULong)(expr);
	    expr += 8;
	    PUSH(uw1);
	    break;
         case DW_OP_constu:
            uw1 = read_leb128U( &expr );
            PUSH(uw1);
            break;
         case DW_OP_const1s:
	    uw1 = *(Char *)expr;
	    expr++;
	    PUSH(uw1);
            break;
         case DW_OP_const2s:
	    uw1 = ML_(read_Short)(expr);
	    expr += 2;
	    PUSH(uw1);
	    break;
         case DW_OP_const4s:
	    uw1 = ML_(read_Int)(expr);
	    expr += 4;
	    PUSH(uw1);
	    break;
         case DW_OP_const8s:
	    uw1 = ML_(read_Long)(expr);
	    expr += 8;
	    PUSH(uw1);
	    break;
         case DW_OP_consts:
            uw1 = read_leb128S( &expr );
            PUSH(uw1);
            break;
         case DW_OP_dup:
	    POP(uw1);
	    PUSH(uw1);
	    PUSH(uw1);
	    break;
	 case DW_OP_drop:
	    POP(uw1);
	    break;
         case DW_OP_over:
            uw1 = 1;
            goto do_pick;
	 case DW_OP_pick:
	    uw1 = *expr++;
         do_pick:
            if (sp < (Int)uw1)
               FAIL("evaluate_Dwarf3_Expr: stack underflow");
            uw1 = stack[sp - uw1];
            PUSH(uw1);
            break;
         case DW_OP_swap:
            if (sp < 1)
               FAIL("evaluate_Dwarf3_Expr: stack underflow");
            uw1 = stack[sp];
            stack[sp] = stack[sp - 1];
            stack[sp - 1] = uw1;
            break;
         case DW_OP_rot:
            if (sp < 2)
               FAIL("evaluate_Dwarf3_Expr: stack underflow");
            uw1 = stack[sp];
            stack[sp] = stack[sp - 1];
            stack[sp - 1] = stack[sp - 2];
            stack[sp - 2] = uw1;
            break;
         case DW_OP_abs:
            POP(sw1);
            if (sw1 < 0)
               sw1 = -sw1;
            PUSH(sw1);
            break;
         case DW_OP_div:
            POP(sw2);
            if (sw2 == 0)
               FAIL("evaluate_Dwarf3_Expr: division by zero");
            POP(sw1);
            sw1 /= sw2;
            PUSH(sw1);
            break;
         case DW_OP_mod:
            POP(uw2);
            if (uw2 == 0)
               FAIL("evaluate_Dwarf3_Expr: division by zero");
            POP(uw1);
            uw1 %= uw2;
            PUSH(uw1);
            break;
#define BINARY(name, op, s) \
         case DW_OP_##name:		\
            POP(s##w2);			\
            POP(s##w1);			\
            s##w1 = s##w1 op s##w2;	\
            PUSH(s##w1);		\
            break
#define UNARY(name, op, s) \
         case DW_OP_##name:		\
            POP(s##w1);			\
            s##w1 = op s##w1;		\
            PUSH(s##w1);		\
            break
         BINARY (and, &, u);
         BINARY (minus, -, u);
         BINARY (mul, *, u);
         UNARY (neg, -, u);
         UNARY (not, ~, u);
         BINARY (or, |, u);
         BINARY (plus, +, u);
         BINARY (shl, <<, u);
         BINARY (shr, >>, u);
         BINARY (shra, >>, s);
         BINARY (xor, ^, u);
         BINARY (le, <=, s);
         BINARY (lt, <, s);
         BINARY (ge, >=, s);
         BINARY (gt, >, s);
         BINARY (ne, !=, u);
         BINARY (eq, ==, u);
#undef UNARY
#undef BINARY
         case DW_OP_skip:
            sw1 = ML_(read_Short)(expr);
            expr += 2;
            if (expr + sw1 < limit - exprszB)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_skip before start of expr");
            if (expr + sw1 >= limit)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_skip after end of expr");
            expr += sw1;
            break;
         case DW_OP_bra:
            sw1 = ML_(read_Short)(expr);
            expr += 2;
            if (expr + sw1 < limit - exprszB)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_bra before start of expr");
            if (expr + sw1 >= limit)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_bra after end of expr");
            POP(uw1);
            if (uw1)
               expr += sw1;
            break;
         case DW_OP_nop:
            break;
         case DW_OP_call_frame_cfa:
            if (!regs)
               FAIL("evaluate_Dwarf3_Expr: "
                    "DW_OP_call_frame_cfa but no reg info");
#if defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
            /* Valgrind on ppc32/ppc64 currently doesn't use unwind info. */
            uw1 = ML_(read_Addr)((UChar*)regs->sp);
#else
            uw1 = ML_(get_CFA)(regs->ip, regs->sp, regs->fp, 0, ~(UWord) 0);
#endif
            /* we expect this to fail on arm-linux, since ML_(get_CFA)
               always returns zero at present. */
            if (!uw1)
               FAIL("evaluate_Dwarf3_Expr: Could not resolve "
                    "DW_OP_call_frame_cfa");
            PUSH(uw1);
            break;
         case DW_OP_implicit_value:
            sw1 = (Word)read_leb128S( &expr );
            uw1 = 0;
            switch (sw1) {
               case 1:
                  uw1 = ML_(read_UChar)(expr);
                  expr += 1;
                  break;
               case 2:
                  uw1 = ML_(read_UShort)(expr);
                  expr += 2;
                  break;
               case 4:
                  uw1 = ML_(read_UInt)(expr);
                  expr += 4;
                  break;
               case 8:
                  uw1 = ML_(read_ULong)(expr);
                  expr += 8;
                  break;
               default:
                  FAIL("evaluate_Dwarf3_Expr: Unhandled "
                       "DW_OP_implicit_value size");
            }
            if (expr != limit)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_implicit_value "
                    "does not terminate expression");
            res.word = uw1;
            res.kind = GXR_Value;
            return res;
         case DW_OP_stack_value:
            POP (uw1);
            res.word = uw1;
            res.kind = GXR_Value;
            if (expr != limit)
               FAIL("evaluate_Dwarf3_Expr: DW_OP_stack_value "
                    "does not terminate expression");
            break;
         default:
            if (!VG_(clo_xml))
               VG_(message)(Vg_DebugMsg, 
                            "warning: evaluate_Dwarf3_Expr: unhandled "
                            "DW_OP_ 0x%x\n", (Int)opcode); 
            FAIL("evaluate_Dwarf3_Expr: unhandled DW_OP_");
            /*NOTREACHED*/
      }

   }

   vg_assert(sp >= 0 && sp < N_EXPR_STACK);
   res.word = stack[sp];
   res.kind = GXR_Addr;
   return res;
 
#  undef POP
#  undef PUSH
#  undef FAIL
#  undef N_EXPR_STACK
}


/* Evaluate a so-called Guarded (DWARF3) expression.  See detailed
   description in priv_d3basics.h. */
GXResult ML_(evaluate_GX)( GExpr* gx, GExpr* fbGX,
                           RegSummary* regs, const DebugInfo* di )
{
   GXResult res;
   Addr     aMin, aMax;
   UChar    uc;
   UShort   nbytes;
   UWord    nGuards = 0;
   UChar* p = &gx->payload[0];
   uc = *p++; /*biasMe*/
   vg_assert(uc == 0 || uc == 1);
   /* in fact it's senseless to evaluate if the guards need biasing.
      So don't. */
   vg_assert(uc == 0);
   while (True) {
      uc = *p++;
      if (uc == 1) { /*isEnd*/
         /* didn't find any matching range. */
         res.kind = GXR_Failure;
         res.word = (UWord)"no matching range";
         return res;
      }
      vg_assert(uc == 0);
      aMin   = ML_(read_Addr)(p);   p += sizeof(Addr);
      aMax   = ML_(read_Addr)(p);   p += sizeof(Addr);
      nbytes = ML_(read_UShort)(p); p += sizeof(UShort);
      nGuards++;
      if (0) VG_(printf)("           guard %d: %#lx %#lx\n",
                         (Int)nGuards, aMin,aMax);
      if (regs == NULL) {
         vg_assert(aMin == (Addr)0);
         vg_assert(aMax == ~(Addr)0);
         /* Assert this is the first guard. */
         vg_assert(nGuards == 1);
         res = ML_(evaluate_Dwarf3_Expr)(
                  p, (UWord)nbytes, fbGX, regs, di,
                  False/*push_initial_zero*/ );
         /* Now check there are no more guards. */
         p += (UWord)nbytes;
         vg_assert(*p == 1); /*isEnd*/
         return res;
      } else {
         if (aMin <= regs->ip && regs->ip <= aMax) {
            /* found a matching range.  Evaluate the expression. */
            return ML_(evaluate_Dwarf3_Expr)(
                      p, (UWord)nbytes, fbGX, regs, di,
                      False/*push_initial_zero*/ );
         }
      }
      /* else keep searching */
      p += (UWord)nbytes;
   }
}


/* Evaluate a very simple Guarded (DWARF3) expression.  The expression
   is expected to denote a constant, with no reference to any
   registers nor to any frame base expression.  The expression is
   expected to have at least one guard.  If there is more than one
   guard, all the sub-expressions are evaluated and compared.  The
   address ranges on the guards are ignored.  GXR_Failure is returned
   in the following circumstances:
   * no guards
   * any of the subexpressions require a frame base expression
   * any of the subexpressions denote a register location
   * any of the subexpressions do not produce a manifest constant
   * there's more than one subexpression, all of which successfully
     evaluate to a constant, but they don't all produce the same constant.
   JRS 23Jan09: the special-casing in this function is a nasty kludge.
   Really it ought to be pulled out and turned into a general
   constant- expression evaluator.
*/
GXResult ML_(evaluate_trivial_GX)( GExpr* gx, const DebugInfo* di )
{
   GXResult   res;
   Addr       aMin, aMax;
   UChar      uc;
   UShort     nbytes;
   Word       i, nGuards;
   MaybeULong *mul, *mul2;

   HChar*  badness = NULL;
   UChar*  p       = &gx->payload[0]; /* must remain unsigned */
   XArray* results = VG_(newXA)( ML_(dinfo_zalloc), "di.d3basics.etG.1",
                                 ML_(dinfo_free),
                                 sizeof(MaybeULong) );

   uc = *p++; /*biasMe*/
   vg_assert(uc == 0 || uc == 1);
   /* in fact it's senseless to evaluate if the guards need biasing.
      So don't. */
   vg_assert(uc == 0);

   nGuards = 0;
   while (True) {
      MaybeULong thisResult;
      uc = *p++;
      if (uc == 1) /*isEnd*/
         break;
      vg_assert(uc == 0);
      aMin   = ML_(read_Addr)(p);   p += sizeof(Addr);
      aMax   = ML_(read_Addr)(p);   p += sizeof(Addr);
      nbytes = ML_(read_UShort)(p); p += sizeof(UShort);
      nGuards++;
      if (0) VG_(printf)("           guard %ld: %#lx %#lx\n", 
                         nGuards, aMin,aMax);

      thisResult.b  = False;
      thisResult.ul = 0;

      /* Peer at this particular subexpression, to see if it's
         obviously a constant. */
      if (nbytes == 1 + sizeof(Addr) && *p == DW_OP_addr) {
         /* DW_OP_addr a */
         Addr a = ML_(read_Addr)((p+1));
         if (bias_address(&a, di)) {
            thisResult.b = True;
            thisResult.ul = (ULong)a;
         } else {
            if (!badness)
               badness = "trivial GExpr denotes constant address "
                         "in unknown section (1)";
         }
      }
      else 
      if (nbytes == 1 + sizeof(Addr) + 1 + 1
          /* 11 byte block: 3 c0 b6 2b 0 0 0 0 0 23 4
             (DW_OP_addr: 2bb6c0; DW_OP_plus_uconst: 4)
             This is really a nasty kludge - only matches if the
             trailing ULEB denotes a number in the range 0 .. 127
             inclusive. */
          && p[0] == DW_OP_addr
          && p[1 + sizeof(Addr)] == DW_OP_plus_uconst 
          && p[1 + sizeof(Addr) + 1] < 0x80 /*1-byte ULEB*/) {
         Addr a = ML_(read_Addr)(&p[1]);
         if (bias_address(&a, di)) {
            thisResult.b = True;
            thisResult.ul = (ULong)a + (ULong)p[1 + sizeof(Addr) + 1];
         } else {
            if (!badness)
               badness = "trivial GExpr denotes constant address "
                         "in unknown section (2)";
         }
      }
      else
      if (nbytes == 2 + sizeof(Addr) 
          && *p == DW_OP_addr
          && *(p + 1 + sizeof(Addr)) == DW_OP_GNU_push_tls_address) {
         if (!badness)
            badness = "trivial GExpr is DW_OP_addr plus trailing junk";
      }
      else if (nbytes >= 1 && *p >= DW_OP_reg0 && *p <= DW_OP_reg31) {
         if (!badness)
            badness = "trivial GExpr denotes register (1)";
      }
      else if (nbytes >= 1 && *p == DW_OP_fbreg) {
         if (!badness)
            badness = "trivial GExpr requires fbGX";
      }
      else if (nbytes >= 1 && *p >= DW_OP_breg0 && *p <= DW_OP_breg31) {
         if (!badness)
            badness = "trivial GExpr requires register value";
      }
      else if (nbytes >= 1 && *p == DW_OP_regx) {
         if (!badness)
            badness = "trivial GExpr denotes register (2)";
      }
      else if (0) {
         VG_(printf)(" ML_(evaluate_trivial_GX): unhandled:\n   ");
         ML_(pp_GX)( gx );
         VG_(printf)("\n");
         tl_assert(0);
      }
      else
         if (!badness)
            badness = "non-trivial GExpr";

      VG_(addToXA)( results, &thisResult );

      p += (UWord)nbytes;
   }

   res.kind = GXR_Failure;

   tl_assert(nGuards == VG_(sizeXA)( results ));
   tl_assert(nGuards >= 0);
   if (nGuards == 0) {
      tl_assert(!badness);
      res.word = (UWord)"trivial GExpr has no guards (!)";
      VG_(deleteXA)( results );
      return res;
   }

   for (i = 0; i < nGuards; i++) {
      mul = VG_(indexXA)( results, i );
      if (mul->b == False)
         break;
   }

   vg_assert(i >= 0 && i <= nGuards);
   if (i < nGuards) {
      /* at least one subexpression failed to produce a manifest constant. */
      vg_assert(badness);
      res.word = (UWord)badness;
      VG_(deleteXA)( results );
      return res;
   }

   /* All the subexpressions produced a constant, but did they all produce
      the same one? */
   mul = VG_(indexXA)( results, 0 );
   tl_assert(mul->b == True); /* we just established that all exprs are ok */

   for (i = 1; i < nGuards; i++) {
      mul2 = VG_(indexXA)( results, i );
      tl_assert(mul2->b == True);
      if (mul2->ul != mul->ul) {
         res.word = (UWord)"trivial GExpr: subexpressions disagree";
         VG_(deleteXA)( results );
         return res;
      }
   }

   /* Well, we have success.  All subexpressions evaluated, and 
      they all agree.  Hurrah. */
   res.kind = GXR_Addr;
   res.word = (UWord)mul->ul; /* NB: narrowing from ULong */
   VG_(deleteXA)( results );
   return res;
}


void ML_(pp_GXResult) ( GXResult res )
{
   switch (res.kind) {
      case GXR_Failure:
         VG_(printf)("GXR_Failure(%s)", (HChar*)res.word); break;
      case GXR_Addr:
         VG_(printf)("GXR_Addr(0x%lx)", res.word); break;
      case GXR_Value:
         VG_(printf)("GXR_Value(0x%lx)", res.word); break;
      case GXR_RegNo:
         VG_(printf)("GXR_RegNo(%lu)", res.word); break;
      default:
         VG_(printf)("GXR_???"); break;
   }
}


void ML_(pp_GX) ( GExpr* gx ) {
   Addr   aMin, aMax;
   UChar  uc;
   UShort nbytes;
   UChar* p = &gx->payload[0];
   uc = *p++;
   VG_(printf)("GX(%s){", uc == 0 ? "final" : "Breqd" );
   vg_assert(uc == 0 || uc == 1);
   while (True) {
      uc = *p++;
      if (uc == 1)
         break; /*isEnd*/
      vg_assert(uc == 0);
      aMin   = ML_(read_Addr)(p);  p += sizeof(Addr);
      aMax   = ML_(read_Addr)(p);  p += sizeof(Addr);
      nbytes = ML_(read_UShort)(p); p += sizeof(UShort);
      VG_(printf)("[%#lx,%#lx]=", aMin, aMax);
      while (nbytes > 0) {
         VG_(printf)("%02x", (UInt)*p++);
         nbytes--;
      }
      if (*p == 0)
         VG_(printf)(",");
   }
   VG_(printf)("}");
}


/*--------------------------------------------------------------------*/
/*--- end                                               d3basics.c ---*/
/*--------------------------------------------------------------------*/
