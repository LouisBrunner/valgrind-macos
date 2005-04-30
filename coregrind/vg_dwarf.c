/*--------------------------------------------------------------------*/
/*--- Read DWARF2 debug info.                           vg_dwarf.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
      jseward@acm.org

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
*/

#include "core.h"
#include "vg_symtab2.h"


/* Structure found in the .debug_line section.  */
typedef struct
{
  UChar li_length          [4];
  UChar li_version         [2];
  UChar li_prologue_length [4];
  UChar li_min_insn_length [1];
  UChar li_default_is_stmt [1];
  UChar li_line_base       [1];
  UChar li_line_range      [1];
  UChar li_opcode_base     [1];
}
DWARF2_External_LineInfo;

typedef struct
{
  UInt   li_length;
  UShort li_version;
  UInt   li_prologue_length;
  UChar  li_min_insn_length;
  UChar  li_default_is_stmt;
  Int    li_line_base;
  UChar  li_line_range;
  UChar  li_opcode_base;
}
DWARF2_Internal_LineInfo;

/* Line number opcodes.  */
enum dwarf_line_number_ops
  {
    DW_LNS_extended_op = 0,
    DW_LNS_copy = 1,
    DW_LNS_advance_pc = 2,
    DW_LNS_advance_line = 3,
    DW_LNS_set_file = 4,
    DW_LNS_set_column = 5,
    DW_LNS_negate_stmt = 6,
    DW_LNS_set_basic_block = 7,
    DW_LNS_const_add_pc = 8,
    DW_LNS_fixed_advance_pc = 9,
    /* DWARF 3.  */
    DW_LNS_set_prologue_end = 10,
    DW_LNS_set_epilogue_begin = 11,
    DW_LNS_set_isa = 12
  };

/* Line number extended opcodes.  */
enum dwarf_line_number_x_ops
  {
    DW_LNE_end_sequence = 1,
    DW_LNE_set_address = 2,
    DW_LNE_define_file = 3
  };

typedef struct State_Machine_Registers
{
  /* Information for the last statement boundary.
   * Needed to calculate statement lengths. */
  Addr  last_address;
  UInt  last_file;
  UInt  last_line;

  Addr  address;
  UInt  file;
  UInt  line;
  UInt  column;
  Int   is_stmt;
  Int   basic_block;
  Int   end_sequence;
  /* This variable hold the number of the last entry seen
     in the File Table.  */
  UInt  last_file_entry;
} SMR;


static 
UInt read_leb128 ( UChar* data, Int* length_return, Int sign )
{
  UInt   result = 0;
  UInt   num_read = 0;
  Int    shift = 0;
  UChar  byte;

  do
    {
      byte = * data ++;
      num_read ++;

      result |= (byte & 0x7f) << shift;

      shift += 7;

    }
  while (byte & 0x80);

  if (length_return != NULL)
    * length_return = num_read;

  if (sign && (shift < 32) && (byte & 0x40))
    result |= -1 << shift;

  return result;
}


static SMR state_machine_regs;

static 
void reset_state_machine ( Int is_stmt )
{
  if (0) VG_(printf)("smr.a := %p (reset)\n", 0 );
  state_machine_regs.last_address = 0;
  state_machine_regs.last_file = 1;
  state_machine_regs.last_line = 1;
  state_machine_regs.address = 0;
  state_machine_regs.file = 1;
  state_machine_regs.line = 1;
  state_machine_regs.column = 0;
  state_machine_regs.is_stmt = is_stmt;
  state_machine_regs.basic_block = 0;
  state_machine_regs.end_sequence = 0;
  state_machine_regs.last_file_entry = 0;
}

/* Handled an extend line op.  Returns true if this is the end
   of sequence.  */
static 
int process_extended_line_op( SegInfo *si, Char*** fnames, 
                              UChar* data, Int is_stmt)
{
  UChar   op_code;
  Int     bytes_read;
  UInt    len;
  UChar * name;
  Addr    adr;

  len = read_leb128 (data, & bytes_read, 0);
  data += bytes_read;

  if (len == 0)
    {
      VG_(message)(Vg_UserMsg,
         "badly formed extended line op encountered!\n");
      return bytes_read;
    }

  len += bytes_read;
  op_code = * data ++;

  if (0) VG_(printf)("dwarf2: ext OPC: %d\n", op_code);

  switch (op_code)
    {
    case DW_LNE_end_sequence:
      if (0) VG_(printf)("1001: si->o %p, smr.a %p\n", 
                         si->offset, state_machine_regs.address );
      state_machine_regs.end_sequence = 1; /* JRS: added for compliance
         with spec; is pointless due to reset_state_machine below 
      */
      if (state_machine_regs.is_stmt) {
	 if (state_machine_regs.last_address)
	    VG_(addLineInfo) (si, (*fnames)[state_machine_regs.last_file], 
			      si->offset + state_machine_regs.last_address, 
			      si->offset + state_machine_regs.address, 
			      state_machine_regs.last_line, 0);
      }
      reset_state_machine (is_stmt);
      break;

    case DW_LNE_set_address:
      adr = *((Addr *)data);
      if (0) VG_(printf)("smr.a := %p\n", adr );
      state_machine_regs.address = adr;
      break;

    case DW_LNE_define_file:
      ++ state_machine_regs.last_file_entry;
      name = data;
      if (*fnames == NULL)
        *fnames = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof (Char *) * 2);
      else
        *fnames = VG_(arena_realloc)(
                     VG_AR_SYMTAB, *fnames,
                     sizeof(Char *) * (state_machine_regs.last_file_entry + 1));
      (*fnames)[state_machine_regs.last_file_entry] = VG_(addStr) (si,name, -1);
      data += VG_(strlen) ((char *) data) + 1;
      read_leb128 (data, & bytes_read, 0);
      data += bytes_read;
      read_leb128 (data, & bytes_read, 0);
      data += bytes_read;
      read_leb128 (data, & bytes_read, 0);
      break;

    default:
      break;
    }

  return len;
}


void VG_(read_debuginfo_dwarf2) ( SegInfo* si, UChar* dwarf2, Int dwarf2_sz )
{
  DWARF2_External_LineInfo * external;
  DWARF2_Internal_LineInfo   info;
  UChar *            standard_opcodes;
  UChar *            data = dwarf2;
  UChar *            end  = dwarf2 + dwarf2_sz;
  UChar *            end_of_sequence;
  Char  **           fnames = NULL;

  /* Fails due to gcc padding ...
  vg_assert(sizeof(DWARF2_External_LineInfo)
            == sizeof(DWARF2_Internal_LineInfo));
  */

  while (data < end)
    {
      external = (DWARF2_External_LineInfo *) data;

      /* Check the length of the block.  */
      info.li_length = * ((UInt *)(external->li_length));

      if (info.li_length == 0xffffffff)
       {
         VG_(symerr)("64-bit DWARF line info is not supported yet.");
         break;
       }

      if (info.li_length + sizeof (external->li_length) > dwarf2_sz)
       {
        VG_(symerr)("DWARF line info appears to be corrupt "
                  "- the section is too small");
         return;
       }

      /* Check its version number.  */
      info.li_version = * ((UShort *) (external->li_version));
      if (info.li_version != 2)
       {
         VG_(symerr)("Only DWARF version 2 line info "
                   "is currently supported.");
         return;
       }

      info.li_prologue_length = * ((UInt *) (external->li_prologue_length));
      info.li_min_insn_length = * ((UChar *)(external->li_min_insn_length));

      info.li_default_is_stmt = True; 
         /* WAS: = * ((UChar *)(external->li_default_is_stmt)); */
         /* Josef Weidendorfer (20021021) writes:

            It seems to me that the Intel Fortran compiler generates
            bad DWARF2 line info code: It sets "is_stmt" of the state
            machine in the the line info reader to be always
            false. Thus, there is never a statement boundary generated
            and therefore never a instruction range/line number
            mapping generated for valgrind.

            Please have a look at the DWARF2 specification, Ch. 6.2
            (x86.ddj.com/ftp/manuals/tools/dwarf.pdf).  Perhaps I
            understand this wrong, but I don't think so.

            I just had a look at the GDB DWARF2 reader...  They
            completely ignore "is_stmt" when recording line info ;-)
            That's the reason "objdump -S" works on files from the the
            intel fortran compiler.  
         */


      /* JRS: changed (UInt*) to (UChar*) */
      info.li_line_base       = * ((UChar *)(external->li_line_base));

      info.li_line_range      = * ((UChar *)(external->li_line_range));
      info.li_opcode_base     = * ((UChar *)(external->li_opcode_base)); 

      if (0) VG_(printf)("dwarf2: line base: %d, range %d, opc base: %d\n",
		  info.li_line_base, info.li_line_range, info.li_opcode_base);

      /* Sign extend the line base field.  */
      info.li_line_base <<= 24;
      info.li_line_base >>= 24;

      end_of_sequence = data + info.li_length 
                             + sizeof (external->li_length);

      reset_state_machine (info.li_default_is_stmt);

      /* Read the contents of the Opcodes table.  */
      standard_opcodes = data + sizeof (* external);

      /* Read the contents of the Directory table.  */
      data = standard_opcodes + info.li_opcode_base - 1;

      if (* data == 0) 
       {
       }
      else
       {
         /* We ignore the directory table, since gcc gives the entire
            path as part of the filename */
         while (* data != 0)
           {
             data += VG_(strlen) ((char *) data) + 1;
           }
       }

      /* Skip the NUL at the end of the table.  */
      if (*data != 0) {
         VG_(symerr)("can't find NUL at end of DWARF2 directory table");
         return;
      }
      data ++;

      /* Read the contents of the File Name table.  */
      if (* data == 0)
       {
       }
      else
       {
         while (* data != 0)
           {
             UChar * name;
             Int bytes_read;

             ++ state_machine_regs.last_file_entry;
             name = data;
             /* Since we don't have realloc (0, ....) == malloc (...)
		semantics, we need to malloc the first time. */

             if (fnames == NULL)
               fnames = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof (Char *) * 2);
             else
               fnames = VG_(arena_realloc)(VG_AR_SYMTAB, fnames,
                           sizeof(Char *) 
                              * (state_machine_regs.last_file_entry + 1));
             data += VG_(strlen) ((Char *) data) + 1;
             fnames[state_machine_regs.last_file_entry] = VG_(addStr) (si,name, -1);

             read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
           }
       }

      /* Skip the NUL at the end of the table.  */
      if (*data != 0) {
         VG_(symerr)("can't find NUL at end of DWARF2 file name table");
         return;
      }
      data ++;

      /* Now display the statements.  */

      while (data < end_of_sequence)
       {
         UChar op_code;
         Int           adv;
         Int           bytes_read;

         op_code = * data ++;

	 if (0) VG_(printf)("dwarf2: OPC: %d\n", op_code);

         if (op_code >= info.li_opcode_base)
           {
             Int advAddr;
             op_code -= info.li_opcode_base;
             adv      = (op_code / info.li_line_range) 
                           * info.li_min_insn_length;
             advAddr = adv;
             state_machine_regs.address += adv;
             if (0) VG_(printf)("smr.a += %p\n", adv );
             adv = (op_code % info.li_line_range) + info.li_line_base;
             if (0) VG_(printf)("1002: si->o %p, smr.a %p\n", 
                                si->offset, state_machine_regs.address );
             state_machine_regs.line += adv;

	     if (state_machine_regs.is_stmt) {
		 /* only add a statement if there was a previous boundary */
		 if (state_machine_regs.last_address) 
		     VG_(addLineInfo) (si, fnames[state_machine_regs.last_file], 
				       si->offset + state_machine_regs.last_address, 
				       si->offset + state_machine_regs.address, 
				       state_machine_regs.last_line, 0);
		 state_machine_regs.last_address = state_machine_regs.address;
		 state_machine_regs.last_file = state_machine_regs.file;
		 state_machine_regs.last_line = state_machine_regs.line;
	     }
           }
         else switch (op_code)
           {
           case DW_LNS_extended_op:
             data += process_extended_line_op (
                        si, &fnames, data, 
                        info.li_default_is_stmt);
             break;

           case DW_LNS_copy:
             if (0) VG_(printf)("1002: si->o %p, smr.a %p\n", 
                                si->offset, state_machine_regs.address );
	     if (state_machine_regs.is_stmt) {
		/* only add a statement if there was a previous boundary */
		if (state_machine_regs.last_address) 
		     VG_(addLineInfo) (si, fnames[state_machine_regs.last_file], 
				       si->offset + state_machine_regs.last_address, 
				       si->offset + state_machine_regs.address,
				       state_machine_regs.last_line, 0);
		 state_machine_regs.last_address = state_machine_regs.address;
		 state_machine_regs.last_file = state_machine_regs.file;
		 state_machine_regs.last_line = state_machine_regs.line;
	     }
             state_machine_regs.basic_block = 0; /* JRS added */
             break;

           case DW_LNS_advance_pc:
             adv = info.li_min_insn_length 
                      * read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             state_machine_regs.address += adv;
             if (0) VG_(printf)("smr.a += %p\n", adv );
             break;

           case DW_LNS_advance_line:
             adv = read_leb128 (data, & bytes_read, 1);
             data += bytes_read;
             state_machine_regs.line += adv;
             break;

           case DW_LNS_set_file:
             adv = read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             state_machine_regs.file = adv;
             break;

           case DW_LNS_set_column:
             adv = read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             state_machine_regs.column = adv;
             break;

           case DW_LNS_negate_stmt:
             adv = state_machine_regs.is_stmt;
             adv = ! adv;
             state_machine_regs.is_stmt = adv;
             break;

           case DW_LNS_set_basic_block:
             state_machine_regs.basic_block = 1;
             break;

           case DW_LNS_const_add_pc:
             adv = (((255 - info.li_opcode_base) / info.li_line_range)
                    * info.li_min_insn_length);
             state_machine_regs.address += adv;
             if (0) VG_(printf)("smr.a += %p\n", adv );
             break;

           case DW_LNS_fixed_advance_pc:
             /* XXX: Need something to get 2 bytes */
             adv = *((UShort *)data);
             data += 2;
             state_machine_regs.address += adv;
             if (0) VG_(printf)("smr.a += %p\n", adv );
             break;

           case DW_LNS_set_prologue_end:
             break;

           case DW_LNS_set_epilogue_begin:
             break;

           case DW_LNS_set_isa:
             adv = read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             break;

           default:
             {
               int j;
               for (j = standard_opcodes[op_code - 1]; j > 0 ; --j)
                 {
                   read_leb128 (data, &bytes_read, 0);
                   data += bytes_read;
                 }
             }
             break;
           }
       }
      VG_(arena_free)(VG_AR_SYMTAB, fnames);
      fnames = NULL;
    }
}


/*------------------------------------------------------------*/
/*--- Read DWARF1 format line number info.                 ---*/
/*------------------------------------------------------------*/

/* DWARF1 appears to be redundant, but nevertheless the Lahey Fortran
   compiler generates it.
*/

/* The following three enums (dwarf_tag, dwarf_form, dwarf_attribute)
   are taken from the file include/elf/dwarf.h in the GNU gdb-6.0
   sources, which are Copyright 1992, 1993, 1995, 1999 Free Software
   Foundation, Inc and naturally licensed under the GNU General Public
   License version 2 or later. 
*/

/* Tag names and codes.  */

enum dwarf_tag {
    TAG_padding			= 0x0000,
    TAG_array_type		= 0x0001,
    TAG_class_type		= 0x0002,
    TAG_entry_point		= 0x0003,
    TAG_enumeration_type	= 0x0004,
    TAG_formal_parameter	= 0x0005,
    TAG_global_subroutine	= 0x0006,
    TAG_global_variable		= 0x0007,
    				/* 0x0008 -- reserved */
				/* 0x0009 -- reserved */
    TAG_label			= 0x000a,
    TAG_lexical_block		= 0x000b,
    TAG_local_variable		= 0x000c,
    TAG_member			= 0x000d,
				/* 0x000e -- reserved */
    TAG_pointer_type		= 0x000f,
    TAG_reference_type		= 0x0010,
    TAG_compile_unit		= 0x0011,
    TAG_string_type		= 0x0012,
    TAG_structure_type		= 0x0013,
    TAG_subroutine		= 0x0014,
    TAG_subroutine_type		= 0x0015,
    TAG_typedef			= 0x0016,
    TAG_union_type		= 0x0017,
    TAG_unspecified_parameters	= 0x0018,
    TAG_variant			= 0x0019,
    TAG_common_block		= 0x001a,
    TAG_common_inclusion	= 0x001b,
    TAG_inheritance		= 0x001c,
    TAG_inlined_subroutine	= 0x001d,
    TAG_module			= 0x001e,
    TAG_ptr_to_member_type	= 0x001f,
    TAG_set_type		= 0x0020,
    TAG_subrange_type		= 0x0021,
    TAG_with_stmt		= 0x0022,

    /* GNU extensions */

    TAG_format_label		= 0x8000,  /* for FORTRAN 77 and Fortran 90 */
    TAG_namelist		= 0x8001,  /* For Fortran 90 */
    TAG_function_template	= 0x8002,  /* for C++ */
    TAG_class_template		= 0x8003   /* for C++ */
};

/* Form names and codes.  */

enum dwarf_form {
    FORM_ADDR	= 0x1,
    FORM_REF	= 0x2,
    FORM_BLOCK2	= 0x3,
    FORM_BLOCK4	= 0x4,
    FORM_DATA2	= 0x5,
    FORM_DATA4	= 0x6,
    FORM_DATA8	= 0x7,
    FORM_STRING	= 0x8
};

/* Attribute names and codes.  */

enum dwarf_attribute {
    AT_sibling			= (0x0010|FORM_REF),
    AT_location			= (0x0020|FORM_BLOCK2),
    AT_name			= (0x0030|FORM_STRING),
    AT_fund_type		= (0x0050|FORM_DATA2),
    AT_mod_fund_type		= (0x0060|FORM_BLOCK2),
    AT_user_def_type		= (0x0070|FORM_REF),
    AT_mod_u_d_type		= (0x0080|FORM_BLOCK2),
    AT_ordering			= (0x0090|FORM_DATA2),
    AT_subscr_data		= (0x00a0|FORM_BLOCK2),
    AT_byte_size		= (0x00b0|FORM_DATA4),
    AT_bit_offset		= (0x00c0|FORM_DATA2),
    AT_bit_size			= (0x00d0|FORM_DATA4),
				/* (0x00e0|FORM_xxxx) -- reserved */
    AT_element_list		= (0x00f0|FORM_BLOCK4),
    AT_stmt_list		= (0x0100|FORM_DATA4),
    AT_low_pc			= (0x0110|FORM_ADDR),
    AT_high_pc			= (0x0120|FORM_ADDR),
    AT_language			= (0x0130|FORM_DATA4),
    AT_member			= (0x0140|FORM_REF),
    AT_discr			= (0x0150|FORM_REF),
    AT_discr_value		= (0x0160|FORM_BLOCK2),
				/* (0x0170|FORM_xxxx) -- reserved */
				/* (0x0180|FORM_xxxx) -- reserved */
    AT_string_length		= (0x0190|FORM_BLOCK2),
    AT_common_reference		= (0x01a0|FORM_REF),
    AT_comp_dir			= (0x01b0|FORM_STRING),
        AT_const_value_string	= (0x01c0|FORM_STRING),
        AT_const_value_data2	= (0x01c0|FORM_DATA2),
        AT_const_value_data4	= (0x01c0|FORM_DATA4),
        AT_const_value_data8	= (0x01c0|FORM_DATA8),
        AT_const_value_block2	= (0x01c0|FORM_BLOCK2),
        AT_const_value_block4	= (0x01c0|FORM_BLOCK4),
    AT_containing_type		= (0x01d0|FORM_REF),
        AT_default_value_addr	= (0x01e0|FORM_ADDR),
        AT_default_value_data2	= (0x01e0|FORM_DATA2),
        AT_default_value_data4	= (0x01e0|FORM_DATA4),
        AT_default_value_data8	= (0x01e0|FORM_DATA8),
        AT_default_value_string	= (0x01e0|FORM_STRING),
    AT_friends			= (0x01f0|FORM_BLOCK2),
    AT_inline			= (0x0200|FORM_STRING),
    AT_is_optional		= (0x0210|FORM_STRING),
        AT_lower_bound_ref	= (0x0220|FORM_REF),
        AT_lower_bound_data2	= (0x0220|FORM_DATA2),
        AT_lower_bound_data4	= (0x0220|FORM_DATA4),
        AT_lower_bound_data8	= (0x0220|FORM_DATA8),
    AT_private			= (0x0240|FORM_STRING),
    AT_producer			= (0x0250|FORM_STRING),
    AT_program			= (0x0230|FORM_STRING),
    AT_protected		= (0x0260|FORM_STRING),
    AT_prototyped		= (0x0270|FORM_STRING),
    AT_public			= (0x0280|FORM_STRING),
    AT_pure_virtual		= (0x0290|FORM_STRING),
    AT_return_addr		= (0x02a0|FORM_BLOCK2),
    AT_abstract_origin		= (0x02b0|FORM_REF),
    AT_start_scope		= (0x02c0|FORM_DATA4),
    AT_stride_size		= (0x02e0|FORM_DATA4),
        AT_upper_bound_ref	= (0x02f0|FORM_REF),
        AT_upper_bound_data2	= (0x02f0|FORM_DATA2),
        AT_upper_bound_data4	= (0x02f0|FORM_DATA4),
        AT_upper_bound_data8	= (0x02f0|FORM_DATA8),
    AT_virtual			= (0x0300|FORM_STRING),

    /* GNU extensions.  */

    AT_sf_names			= (0x8000|FORM_DATA4),
    AT_src_info			= (0x8010|FORM_DATA4),
    AT_mac_info			= (0x8020|FORM_DATA4),
    AT_src_coords		= (0x8030|FORM_DATA4),
    AT_body_begin		= (0x8040|FORM_ADDR),
    AT_body_end			= (0x8050|FORM_ADDR)
};

/* end of enums taken from gdb-6.0 sources */

void VG_(read_debuginfo_dwarf1) ( 
        SegInfo* si, 
        UChar* dwarf1d, Int dwarf1d_sz, 
        UChar* dwarf1l, Int dwarf1l_sz )
{
   UInt   stmt_list;
   Bool   stmt_list_found;
   Int    die_offset, die_szb, at_offset;
   UShort die_kind, at_kind;
   UChar* at_base;
   UChar* src_filename;

   if (0) 
      VG_(printf)("read_debuginfo_dwarf1 ( %p, %d, %p, %d )\n",
	          dwarf1d, dwarf1d_sz, dwarf1l, dwarf1l_sz );

   /* This loop scans the DIEs. */
   die_offset = 0;
   while (True) {
      if (die_offset >= dwarf1d_sz) break;

      die_szb  = *(Int*)(dwarf1d + die_offset);
      die_kind = *(UShort*)(dwarf1d + die_offset + 4);

      /* We're only interested in compile_unit DIEs; ignore others. */
      if (die_kind != TAG_compile_unit) {
         die_offset += die_szb;
         continue; 
      }

      if (0) 
         VG_(printf)("compile-unit DIE: offset %d, tag 0x%x, size %d\n", 
                     die_offset, (Int)die_kind, die_szb );

      /* We've got a compile_unit DIE starting at (dwarf1d +
         die_offset+6).  Try and find the AT_name and AT_stmt_list
         attributes.  Then, finally, we can read the line number info
         for this source file. */

      /* The next 3 are set as we find the relevant attrs. */
      src_filename    = NULL;
      stmt_list_found = False;
      stmt_list       = 0;

      /* This loop scans the Attrs inside compile_unit DIEs. */
      at_base = dwarf1d + die_offset + 6;
      at_offset = 0;
      while (True) {
         if (at_offset >= die_szb-6) break;

         at_kind = *(UShort*)(at_base + at_offset);
         if (0) VG_(printf)("atoffset %d, attag 0x%x\n", 
                            at_offset, (Int)at_kind );
         at_offset += 2; /* step over the attribute itself */
	 /* We have to examine the attribute to figure out its
            length. */
         switch (at_kind) {
            case AT_stmt_list:
            case AT_language:
            case AT_sibling:
               if (at_kind == AT_stmt_list) {
                  stmt_list_found = True;
                  stmt_list = *(Int*)(at_base+at_offset);
               }
               at_offset += 4; break;
            case AT_high_pc:
            case AT_low_pc: 
               at_offset += sizeof(void*); break;
            case AT_name: 
            case AT_producer:
            case AT_comp_dir:
               /* Zero terminated string, step over it. */
               if (at_kind == AT_name)
                  src_filename = at_base + at_offset;
               while (at_offset < die_szb-6 && at_base[at_offset] != 0)
                  at_offset++;
               at_offset++;
               break;
            default: 
               VG_(printf)("Unhandled DWARF-1 attribute 0x%x\n", 
                           (Int)at_kind );
               VG_(core_panic)("Unhandled DWARF-1 attribute");
         } /* switch (at_kind) */
      } /* looping over attributes */

      /* So, did we find the required stuff for a line number table in
         this DIE?  If yes, read it. */
      if (stmt_list_found /* there is a line number table */
          && src_filename != NULL /* we know the source filename */
         ) {
         /* Table starts:
               Length: 
                  4 bytes, includes the entire table
               Base address: 
                  unclear (4? 8?), assuming native pointer size here.
            Then a sequence of triples
               (source line number -- 32 bits
                source line column -- 16 bits
                address delta -- 32 bits)
	 */
         Addr   base;
	 Int    len;
         Char*  curr_filenm;
         UChar* ptr;
         UInt   prev_line, prev_delta;

         curr_filenm = VG_(addStr) ( si, src_filename, -1 );
         prev_line = prev_delta = 0;

         ptr = dwarf1l + stmt_list;
         len  =        *(Int*)ptr;    ptr += sizeof(Int);
         base = (Addr)(*(void**)ptr); ptr += sizeof(void*);
         len -= (sizeof(Int) + sizeof(void*));
         while (len > 0) {
            UInt   line;
            UShort col;
            UInt   delta;
            line = *(UInt*)ptr;  ptr += sizeof(UInt);
            col = *(UShort*)ptr;  ptr += sizeof(UShort);
            delta = *(UShort*)ptr;  ptr += sizeof(UInt);
	    if (0) VG_(printf)("line %d, col %d, delta %d\n", 
                               line, (Int)col, delta );
            len -= (sizeof(UInt) + sizeof(UShort) + sizeof(UInt));

	    if (delta > 0 && prev_line > 0) {
	       if (0) VG_(printf) ("     %d  %d-%d\n",
                                   prev_line, prev_delta, delta-1);
	       VG_(addLineInfo) ( si, curr_filenm, 
		 	          base + prev_delta, base + delta,
			          prev_line, 0 );
	    }
	    prev_line = line;
	    prev_delta = delta;
	 }        
      }  

      /* Move on the the next DIE. */
      die_offset += die_szb;

   } /* Looping over DIEs */

}


/*------------------------------------------------------------*/
/*--- Read call-frame info from an .eh_frame section       ---*/
/*------------------------------------------------------------*/

/* the number of regs we are prepared to unwind */
#define N_CFI_REGS 20

/* Instructions for the automaton */
enum dwarf_cfa_primary_ops
  {
    DW_CFA_use_secondary = 0,
    DW_CFA_advance_loc   = 1,
    DW_CFA_offset        = 2,
    DW_CFA_restore       = 3
  };

enum dwarf_cfa_secondary_ops
  {
    DW_CFA_nop              = 0x00,
    DW_CFA_set_loc          = 0x01,
    DW_CFA_advance_loc1     = 0x02,
    DW_CFA_advance_loc2     = 0x03,
    DW_CFA_advance_loc4     = 0x04,
    DW_CFA_offset_extended  = 0x05,
    DW_CFA_restore_extended = 0x06,
    DW_CFA_undefined        = 0x07,
    DW_CFA_same_value       = 0x08,
    DW_CFA_register         = 0x09,
    DW_CFA_remember_state   = 0x0a,
    DW_CFA_restore_state    = 0x0b,
    DW_CFA_def_cfa          = 0x0c,
    DW_CFA_def_cfa_register = 0x0d,
    DW_CFA_def_cfa_offset   = 0x0e,
    DW_CFA_lo_user          = 0x1c,
    DW_CFA_GNU_args_size    = 0x2e,
    DW_CFA_hi_user          = 0x3f
  };


typedef
   struct {
      enum { RR_Undef, RR_Same, RR_CFAoff, RR_Reg, RR_Arch } tag;

      /* CFA offset if tag==RR_CFAoff */
      Int coff;

      /* reg, if tag==RR_Reg */
      Int reg;
   }
   RegRule;

typedef
   struct {
      /* Read-only fields. */
      Int code_a_f;
      Int data_a_f;
      /* The rest of these fields can be modifed by
         run_CF_instruction. */
      /* The LOC entry */
      Addr loc;
      /* The CFA entry */
      Int cfa_reg;
      Int cfa_offset; /* in bytes */
      /* register unwind rules */
      RegRule reg[N_CFI_REGS];
   }
   UnwindContext;

static void initUnwindContext ( /*OUT*/UnwindContext* ctx )
{
   Int i;
   ctx->code_a_f = 0;
   ctx->data_a_f = 0;
   ctx->loc = 0;
   ctx->cfa_reg = 0;
   ctx->cfa_offset = 0;
   for (i = 0; i < N_CFI_REGS; i++) {
      ctx->reg[i].tag = RR_Undef;
      ctx->reg[i].coff = 0;
      ctx->reg[i].reg = 0;
   }
}

static void ppUnwindContext ( UnwindContext* ctx )
{
   Int i;
   VG_(printf)("0x%llx: ", (ULong)ctx->loc);
   VG_(printf)("%d(r%d) ",  ctx->cfa_offset, ctx->cfa_reg);
   for (i = 0; i < N_CFI_REGS; i++) {
      switch (ctx->reg[i].tag) {
         case RR_Undef:  VG_(printf)("u  "); break;
         case RR_Same:   VG_(printf)("s  "); break;
         case RR_CFAoff: VG_(printf)("c%d ", ctx->reg[i].coff); break;
         case RR_Reg:    VG_(printf)("r%d ", ctx->reg[i].reg); break;
         case RR_Arch:   VG_(printf)("a  "); break;
         default:        VG_(core_panic)("ppUnwindContext");
      }
   }
   VG_(printf)("\n");
}

static inline Bool host_is_little_endian ( void )
{
   UInt x = 0x76543210;
   UChar* p = (UChar*)(&x);
   return toBool(*p == 0x10);
}


static UShort read_UShort ( UChar* data )
{
   vg_assert(host_is_little_endian());
   UInt r = 0;
   r = data[0] 
       | ( ((UInt)data[1]) << 8 );
   return r;
}

static UInt read_UInt ( UChar* data )
{
   vg_assert(host_is_little_endian());
   UInt r = 0;
   r = data[0] 
       | ( ((UInt)data[1]) << 8 ) 
       | ( ((UInt)data[2]) << 16 ) 
       | ( ((UInt)data[3]) << 24 );
   return r;
}

static Addr read_Addr ( UChar* data )
{
  if (sizeof(Addr) == 4)
    return read_UInt(data);
  vg_assert(0);
}

static UChar read_UChar ( UChar* data )
{
   return data[0];
}


/* Run a CFI instruction, and also return its length.
   Returns 0 if the instruction could not be executed. 
*/
static Int run_CF_instruction ( /*MOD*/UnwindContext* ctx, 
                                UChar* instr )
{
   Int   off, reg, nleb;
   UInt  delta;
   Int   i   = 0;
   UChar hi2 = (instr[i] >> 6) & 3;
   UChar lo6 = instr[i] & 0x3F;
   i++;

   if (hi2 == DW_CFA_advance_loc) {
      delta = (UInt)lo6;
      ctx->loc += delta;
      return i;
   }

   if (hi2 == DW_CFA_offset) {
      /* Set rule for reg 'lo6' to CFAoffset(off * data_af) */
      off = read_leb128( &instr[i], &nleb, 0 );
      i += nleb;
      reg = (Int)lo6;
      if (reg < 0 || reg >= N_CFI_REGS) 
         return 0; /* fail */
      ctx->reg[reg].tag = RR_CFAoff;
      ctx->reg[reg].coff = off * ctx->data_a_f;
      return i;
   }

   if (hi2 == DW_CFA_restore) {
      vg_assert(0);
      VG_(printf)("DW_CFA_restore(%d)\n", (Int)lo6);
      return i;
   }

   vg_assert(hi2 == DW_CFA_use_secondary);

   switch (lo6) {
      case DW_CFA_nop: 
         break;
      case DW_CFA_advance_loc1:
         vg_assert(0);
         delta = (UInt)read_UChar(&instr[i]); i+= sizeof(UChar);
         VG_(printf)("DW_CFA_advance_loc1(%d)\n", delta); 
         break;

      case DW_CFA_def_cfa:
         reg = read_leb128( &instr[i], &nleb, 0 );
         i += nleb;
         off = read_leb128( &instr[i], &nleb, 0 );
         i += nleb;
         if (reg < 0 || reg >= N_CFI_REGS) 
            return 0; /* fail */
         ctx->cfa_reg    = reg;
         ctx->cfa_offset = off;
         break;

      case DW_CFA_def_cfa_register: {
         reg = read_leb128( &instr[i], &nleb, 0);
         i += nleb;
         if (reg < 0 || reg >= N_CFI_REGS) 
            return 0; /* fail */
         ctx->cfa_reg = reg;
         break;
      }
      case DW_CFA_def_cfa_offset: {
         off = read_leb128( &instr[i], &nleb, 0);
         i += nleb;
         ctx->cfa_offset = off;
         break;
      }
      case DW_CFA_GNU_args_size: {
         /* No idea what is supposed to happen.  gdb-6.3 simply
            ignores these. */
         off = read_leb128( &instr[i], &nleb, 0 );
         i += nleb;
         break;
      }
      default: 
         vg_assert(0);
         VG_(printf)("0:%d\n", (Int)lo6); 
         break;
   }

   return i;   
}


/* Show a CFI instruction, and also return its length. */
static Int show_CF_instruction ( UChar* instr )
{
   UInt  delta;
   Int   off, reg, nleb;
   Int   i   = 0;
   UChar hi2 = (instr[i] >> 6) & 3;
   UChar lo6 = instr[i] & 0x3F;
   i++;

   if (hi2 == DW_CFA_advance_loc) {
      VG_(printf)("DW_CFA_advance_loc(%d)\n", (Int)lo6);
      return i;
   }

   if (hi2 == DW_CFA_offset) {
      off = read_leb128( &instr[i], &nleb, 0 );
      i += nleb;
      VG_(printf)("DW_CFA_offset(r%d + %d x data_af)\n", (Int)lo6, off);
      return i;
   }

   if (hi2 == DW_CFA_restore) {
      VG_(printf)("DW_CFA_restore(%d)\n", (Int)lo6);
      return i;
   }

   vg_assert(hi2 == DW_CFA_use_secondary);

   switch (lo6) {

      case DW_CFA_nop: 
         VG_(printf)("DW_CFA_nop\n"); 
         break;

      case DW_CFA_advance_loc1:
         delta = (UInt)read_UChar(&instr[i]); i+= sizeof(UChar);
         VG_(printf)("DW_CFA_advance_loc1(%d)\n", delta); 
         break;

      case DW_CFA_def_cfa:
         reg = read_leb128( &instr[i], &nleb, 0 );
         i += nleb;
         off = read_leb128( &instr[i], &nleb, 0 );
         i += nleb;
         VG_(printf)("DW_CFA_def_cfa(r%d, off %d)\n", reg, off); 
         break;

      case DW_CFA_def_cfa_register:
         reg = read_leb128( &instr[i], &nleb, 0);
         i += nleb;
         VG_(printf)("DW_CFA_def_cfa_register(r%d)\n", reg); 
         break;

      case DW_CFA_def_cfa_offset: 
         off = read_leb128( &instr[i], &nleb, 0);
         i += nleb;
         VG_(printf)("DW_CFA_def_cfa_offset(%d)\n", off); 
         break;

      case DW_CFA_GNU_args_size:
         off = read_leb128( &instr[i], &nleb, 0 );
         i += nleb;
         VG_(printf)("DW_CFA_GNU_args_size(%d)\n", off ); 
         break;

      default: 
         VG_(printf)("0:%d\n", (Int)lo6); 
         break;
   }

   return i;
}


static void show_CF_instructions ( UChar* instrs, Int ilen )
{
   Int i = 0;
   while (True) {
      if (i >= ilen) break;
      i += show_CF_instruction( &instrs[i] );
   }
}

/* Run the CF instructions in instrs[0 .. ilen-1], until the end is
   reached, or until there is a failure.  Return True iff success. 
*/
static 
Bool run_CF_instructions ( UnwindContext* ctx, UChar* instrs, Int ilen )
{
   Int j, i = 0;
   ppUnwindContext(ctx);
   while (True) {
      if (i >= ilen) break;
      if (0) (void)show_CF_instruction( &instrs[i] );
      j = run_CF_instruction( ctx, &instrs[i] );
      if (j == 0)
         return False; /* execution failed */
      i += j;
      ppUnwindContext(ctx);
   }
   return True;
}


void VG_(read_callframe_info_dwarf2) 
        ( /*OUT*/SegInfo* si, UChar* ehframe, Int ehframe_sz )
{
   UnwindContext ctx;
   Int nbytes;
   HChar* how = NULL;
   Int cie_codeaf = 0;
   Int cie_dataaf = 0;
   Bool ok;

   UChar* current_cie = NULL;

   if (ehframe_sz != 240) return;
   VG_(printf)("\n\n\neh_frame %p %d\n", ehframe, ehframe_sz);

   UChar* data = ehframe;

   UChar* cie_instrs = NULL;
   Int cie_ilen = 0;

   /* Loop over CIEs/FDEs */

   while (True) {

     /* Are we done? */
     if (data == ehframe + ehframe_sz)
       return;

     /* Overshot the end?  Means something is wrong */
     if  (data > ehframe + ehframe_sz) {
       how = "overran the end of .eh_frame";
       goto bad;
     }

     /* Ok, we must be looking at the start of a new CIE or FDE.
        Figure out which it is. */

      UChar* ciefde_start = data;
      VG_(printf)("\ncie/fde.start   = %p\n", ciefde_start);

      UInt ciefde_len = read_UInt(data); data += sizeof(UInt);
      VG_(printf)("cie/fde.length  = %d\n", ciefde_len);

      /* Apparently, if the .length field is zero, we are at the end
	 of the sequence.  ?? Neither the DWARF2 spec not the AMD64
	 ABI spec say this, though. */
      if (ciefde_len == 0) {
         if (data == ehframe + ehframe_sz)
            return;
         how = "zero-sized CIE/FDE but not at section end\n";
         goto bad;
      }

      UInt cie_pointer = read_UInt(data); data += sizeof(UInt);
      VG_(printf)("cie.pointer     = %d\n", cie_pointer);

      /* If cie_pointer is zero, we've got a CIE; else it's an FDE. */
      if (cie_pointer == 0) {

         /* Remember the start proper of the current CIE */
         current_cie = ciefde_start + sizeof(UInt);

         /* --------- CIE --------- */
         UChar cie_version = read_UChar(data); data += sizeof(UChar);
         VG_(printf)("cie.version     = %d\n", (Int)cie_version);

         UChar* cie_augmentation = data;
         data += 1 + VG_(strlen)(cie_augmentation);
         VG_(printf)("cie.augment     = \"%s\"\n", cie_augmentation);

         if (0 != VG_(strcmp)(cie_augmentation, "")) {
            how = "non-NULL cie.augmentation";
            goto bad;
         }

         cie_codeaf = read_leb128( data, &nbytes, 0);
         data += nbytes;
         VG_(printf)("cie.code_af     = %d\n", cie_codeaf);

         cie_dataaf = read_leb128( data, &nbytes, 1);
         data += nbytes;
         VG_(printf)("cie.data_af     = %d\n", cie_dataaf);

         UChar cie_rareg = read_UChar(data); data += sizeof(UChar);
         VG_(printf)("cie.ra_reg      = %d\n", (Int)cie_rareg);

         cie_instrs = data;
         cie_ilen   = ciefde_start + ciefde_len + sizeof(UInt) - data;
         VG_(printf)("cie.instrs      = %p\n", (Int)cie_instrs);
         VG_(printf)("cie.ilen        = %d\n", (Int)cie_ilen);

         if (cie_ilen < 0 || cie_ilen > ehframe_sz) {
            how = "implausible # cie initial insns";
            goto bad;
         }

	 data += cie_ilen;

         show_CF_instructions(cie_instrs, cie_ilen);

      } else {

         /* --------- FDE --------- */

	/* Ensure that (1) we have a valid CIE, and (2) that 
	   it is indeed the CIE referred to by this FDE. */
	if (current_cie == NULL) {
	  how = "FDE with no preceding CIE";
	  goto bad;
	}
	if (cie_pointer != data - current_cie) {
	  how = "FDE does not refer to preceding CIE";
	  goto bad;
	}

         Addr fde_initloc = read_Addr(data); data += sizeof(Addr);
         VG_(printf)("fde.initloc     = %p\n", (void*)fde_initloc);

         UWord fde_arange = read_Addr(data); data += sizeof(Addr);
         VG_(printf)("fde.arangec     = %p\n", (void*)fde_arange);

         UChar* fde_instrs = data;
         Int    fde_ilen   = ciefde_start + ciefde_len + sizeof(UInt) - data;
         VG_(printf)("fde.instrs      = %p\n", (Int)fde_instrs);
         VG_(printf)("fde.ilen        = %d\n", (Int)fde_ilen);

         if (fde_ilen < 0 || fde_ilen > ehframe_sz) {
            how = "implausible # fde initial insns";
            goto bad;
         }

	 data += fde_ilen;

	 initUnwindContext(&ctx);
         ctx.code_a_f = cie_codeaf;
         ctx.data_a_f = cie_dataaf;
	 ok = run_CF_instructions(&ctx, cie_instrs, cie_ilen);
         if (ok)
	    ok = run_CF_instructions(&ctx, fde_instrs, fde_ilen);
      }
   }

   /* Read a CIE and remember important bits */



   return;

   bad:
    VG_(message)(Vg_UserMsg, "Warning: %s in DWARF2 CIE reading", how);
    return;
}


/*--------------------------------------------------------------------*/
/*--- end                                               vg_dwarf.c ---*/
/*--------------------------------------------------------------------*/
