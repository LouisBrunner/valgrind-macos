/*--------------------------------------------------------------------*/
/*--- Read DWARF2 debug info.                    vg_symtab_dwarf.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward
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

#include "vg_include.h"
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
                              UChar* data, Int is_stmt, Int pointer_size)
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
      /* XXX: Pointer size could be 8 */
      vg_assert(pointer_size == 4);
      adr = *((Addr *)data);
      if (0) VG_(printf)("smr.a := %p\n", adr );
      state_machine_regs.address = adr;
      break;

    case DW_LNE_define_file:
      ++ state_machine_regs.last_file_entry;
      name = data;
      if (*fnames == NULL)
        *fnames = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof (UInt) * 2);
      else
        *fnames = VG_(arena_realloc)(
                     VG_AR_SYMTAB, *fnames, /*alignment*/4,
                     sizeof(UInt) 
                        * (state_machine_regs.last_file_entry + 1));
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
               fnames = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof (UInt) * 2);
             else
               fnames = VG_(arena_realloc)(VG_AR_SYMTAB, fnames, /*alignment*/4,
                           sizeof(UInt) 
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
                        info.li_default_is_stmt, sizeof (Addr));
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

/*--------------------------------------------------------------------*/
/*--- end                                        vg_symtab_dwarf.c ---*/
/*--------------------------------------------------------------------*/
