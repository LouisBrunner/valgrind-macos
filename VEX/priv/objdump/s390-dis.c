/* s390-dis.c -- Disassemble S390 instructions
   Copyright (C) 2000-2025 Free Software Foundation, Inc.
   Contributed by Martin Schwidefsky (schwidefsky@de.ibm.com).

   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the
   Free Software Foundation, 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include <stdarg.h>
#include "s390.h"
#include "dis-asm.h"
#undef NULL
#include "main_util.h"     // vex_strlen
#include "s390_defs.h"     // S390_MAX_MNEMONIC_LEN
#include "s390_disasm.h"

static int opc_index[256];
static int current_arch_mask = 0;
static int option_use_insn_len_bits_p = 0;
static int option_print_insn_desc = 0;
static int pad_mnemonic;

static const char *
padmnm(const char *mnm)
{
  static char buf[S390_MAX_MNEMONIC_LEN + 1];

  if (pad_mnemonic) {
     if (vex_strlen(mnm) > S390_MAX_MNEMONIC_LEN)
        return "failed: buf too small";

     vex_sprintf(buf, "%-*s", S390_MAX_MNEMONIC_LEN, mnm);
  } else {
     vex_sprintf(buf, "%s", mnm);
  }

  return buf;
}


#if 0
typedef struct
{
  const char *name;
  const char *description;
} s390_options_t;

static const s390_options_t options[] =
{
  { "esa" ,       N_("Disassemble in ESA architecture mode") },
  /* TRANSLATORS: Please do not translate 'z/Architecture' as this is a technical name.  */
  { "zarch",      N_("Disassemble in z/Architecture mode") },
  { "insnlength", N_("Print unknown instructions according to "
		     "length from first two bits") },
  { "insndesc",   N_("Print instruction description as comment") },
};
#endif

/* Set up index table for first opcode byte.  */

void
disassemble_init_s390 (struct disassemble_info *info)
{
  int i;

  __builtin_memset (opc_index, 0, sizeof (opc_index));

  /* Reverse order, such that each opc_index ends up pointing to the
     first matching entry instead of the last.  */
  for (i = s390_num_opcodes; i--; )
    opc_index[s390_opcodes[i].opcode[0]] = i;

  current_arch_mask = 1 << S390_OPCODE_ZARCH;
  option_use_insn_len_bits_p = 0;
  option_print_insn_desc = 0;
#if 0
  for (p = info->disassembler_options; p != NULL; )
    {
      if (startswith (p, "esa"))
	current_arch_mask = 1 << S390_OPCODE_ESA;
      else if (startswith (p, "zarch"))
	current_arch_mask = 1 << S390_OPCODE_ZARCH;
      else if (startswith (p, "insnlength"))
	option_use_insn_len_bits_p = 1;
      else if (startswith (p, "insndesc"))
	option_print_insn_desc = 1;
      else
	/* xgettext:c-format */
	opcodes_error_handler (_("unknown S/390 disassembler option: %s"), p);

      p = strchr (p, ',');
      if (p != NULL)
	p++;
    }
#endif
}

/* Derive the length of an instruction from its first byte.  */

static inline int
s390_insn_length (const bfd_byte *buffer)
{
  /* 00xxxxxx -> 2, 01xxxxxx/10xxxxxx -> 4, 11xxxxxx -> 6.  */
  return ((buffer[0] >> 6) + 3) & ~1U;
}

/* Match the instruction in BUFFER against the given OPCODE, excluding
   the first byte.  */

static inline int
s390_insn_matches_opcode (const bfd_byte *buffer,
			  const struct s390_opcode *opcode)
{
  return (buffer[1] & opcode->mask[1]) == opcode->opcode[1]
    && (buffer[2] & opcode->mask[2]) == opcode->opcode[2]
    && (buffer[3] & opcode->mask[3]) == opcode->opcode[3]
    && (buffer[4] & opcode->mask[4]) == opcode->opcode[4]
    && (buffer[5] & opcode->mask[5]) == opcode->opcode[5];
}

union operand_value
{
  int i;
  unsigned int u;
};

/* Extracts an operand value from an instruction.  */
/* We do not perform the shift operation for larl-type address
   operands here since that would lead to an overflow of the 32 bit
   integer value.  Instead the shift operation is done when printing
   the operand.  */

static inline union operand_value
s390_extract_operand (const bfd_byte *insn,
		      const struct s390_operand *operand)
{
  union operand_value ret;
  unsigned int val;
  int bits;
  const bfd_byte *orig_insn = insn;

  /* Extract fragments of the operand byte for byte.  */
  insn += operand->shift / 8;
  bits = (operand->shift & 7) + operand->bits;
  val = 0;
  do
    {
      val <<= 8;
      val |= (unsigned int) *insn++;
      bits -= 8;
    }
  while (bits > 0);
  val >>= -bits;
  val &= ((1U << (operand->bits - 1)) << 1) - 1;

  /* Check for special long displacement case.  */
  if (operand->bits == 20 && operand->shift == 20)
    val = (val & 0xff) << 12 | (val & 0xfff00) >> 8;

  /* Sign extend value if the operand is signed or pc relative.  Avoid
     integer overflows.  */
  if (operand->flags & (S390_OPERAND_SIGNED | S390_OPERAND_PCREL))
    {
      unsigned int m = 1U << (operand->bits - 1);

      if (val >= m)
	ret.i = (int) (val - m) - 1 - (int) (m - 1U);
      else
	ret.i = (int) val;
    }
  else if (operand->flags & S390_OPERAND_LENGTH)
    /* Length x in an instruction has real length x + 1.  */
    ret.u = val + 1;

  else if (operand->flags & S390_OPERAND_VR)
    {
      /* Extract the extra bits for a vector register operand stored
	 in the RXB field.  */
      unsigned vr = operand->shift == 32 ? 3
	: (unsigned) operand->shift / 4 - 2;

      ret.u = val | ((orig_insn[4] & (1 << (3 - vr))) << (vr + 1));
    }
  else
    ret.u = val;

  return ret;
}

/* Return remaining operand count.  */

static unsigned int
operand_count (const unsigned char *opindex_ptr)
{
  unsigned int count = 0;

  for (; *opindex_ptr != 0; opindex_ptr++)
    {
      /* Count D(X,B), D(B), and D(L,B) as one operand.  Assuming correct
	 instruction operand definitions simply do not count D, X, and L.  */
      if (!(s390_operands[*opindex_ptr].flags & (S390_OPERAND_DISP
						| S390_OPERAND_INDEX
						| S390_OPERAND_LENGTH)))
	count++;
    }

  return count;
}

/* Return true if all remaining instruction operands are optional.  */

static bool
skip_optargs_p (unsigned int opcode_flags, const unsigned char *opindex_ptr)
{
  if ((opcode_flags & (S390_INSTR_FLAG_OPTPARM | S390_INSTR_FLAG_OPTPARM2)))
    {
      unsigned int opcount = operand_count (opindex_ptr);

      if (opcount == 1)
	return true;

      if ((opcode_flags & S390_INSTR_FLAG_OPTPARM2) && opcount == 2)
	return true;
    }

  return false;
}

/* Return true if all remaining instruction operands are optional
   and their values are zero.  */

static bool
skip_optargs_zero_p (const bfd_byte *buffer, unsigned int opcode_flags,
		     const unsigned char *opindex_ptr)
{
  /* Test if remaining operands are optional.  */
  if (!skip_optargs_p (opcode_flags, opindex_ptr))
    return false;

  /* Test if remaining operand values are zero.  */
  for (; *opindex_ptr != 0; opindex_ptr++)
    {
      const struct s390_operand *operand = &s390_operands[*opindex_ptr];
      union operand_value value = s390_extract_operand (buffer, operand);

      if (value.u != 0)
	return false;
    }

  return true;
}

/* Print the S390 instruction in BUFFER, assuming that it matches the
   given OPCODE.  */

static void
s390_print_insn_with_opcode (bfd_vma memaddr,
			     struct disassemble_info *info,
			     const bfd_byte *buffer,
			     const struct s390_opcode *opcode)
{
  const unsigned char *opindex;
  char separator;

  /* Mnemonic.  */
  info->fprintf_styled_func (info->stream, dis_style_mnemonic,
			     "%s", padmnm(opcode->name));

  /* Operands.  */
  separator = ' ';  // mnemonics are padded to the right and have same width
  for (opindex = opcode->operands; *opindex != 0; opindex++)
    {
      const struct s390_operand *operand = s390_operands + *opindex;
      union operand_value val = s390_extract_operand (buffer, operand);
      unsigned long flags = operand->flags;

      /* Omit index register 0, except for vector index register 0.  */
      if ((flags & S390_OPERAND_INDEX) && !(flags & S390_OPERAND_VR)
	  && val.u == 0)
	continue;
      /* Omit base register 0, if no or omitted index register 0.  */
      if ((flags & S390_OPERAND_BASE) && val.u == 0 && separator == '(')
	{
	  separator = ',';
	  continue;
	}

      /* Omit optional last operands with a value of zero, except if
	 within an addressing operand sequence D(X,B), D(B), and D(L,B).
	 Index and base register operands with a value of zero are
	 handled separately, as they may not be omitted unconditionally.  */
      if (!(operand->flags & (S390_OPERAND_BASE
			      | S390_OPERAND_INDEX
			      | S390_OPERAND_LENGTH))
	  && skip_optargs_zero_p (buffer, opcode->flags, opindex))
	break;

      if (flags & S390_OPERAND_GPR)
	{
	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  if ((flags & (S390_OPERAND_BASE | S390_OPERAND_INDEX))
	      && val.u == 0)
	    info->fprintf_styled_func (info->stream, dis_style_register,
				       "%u", val.u);
	  else
	    info->fprintf_styled_func (info->stream, dis_style_register,
				       "%%r%u", val.u);
	}
      else if (flags & S390_OPERAND_FPR)
	{
	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  info->fprintf_styled_func (info->stream, dis_style_register,
				     "%%f%u", val.u);
	}
      else if (flags & S390_OPERAND_VR)
	{
	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  info->fprintf_styled_func (info->stream, dis_style_register,
				     "%%v%u", val.u);
	}
      else if (flags & S390_OPERAND_AR)
	{
	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  info->fprintf_styled_func (info->stream, dis_style_register,
				     "%%a%u", val.u);
	}
      else if (flags & S390_OPERAND_CR)
	{
	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  info->fprintf_styled_func (info->stream, dis_style_register,
				     "%%c%u", val.u);
	}
      else if (flags & S390_OPERAND_PCREL)
	{
	  bfd_vma target = memaddr + val.i + val.i;

	  /* Provide info for jump visualization.  May be evaluated by p_a_f().  */
	  info->target = target;

	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  info->print_address_func (target, info);
	}
      else if (flags & S390_OPERAND_SIGNED)
	{
	  enum disassembler_style style;

	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  style = ((flags & S390_OPERAND_DISP)
		   ? dis_style_address_offset : dis_style_immediate);
	  info->fprintf_styled_func (info->stream, style, "%i", val.i);
	}
      else
	{
	  enum disassembler_style style;

	  if (!(flags & S390_OPERAND_LENGTH))
	    {
	      union operand_value insn_opval;

	      /* Mask any constant operand bits set in insn template.  */
	      insn_opval = s390_extract_operand (opcode->opcode, operand);
	      val.u &= ~insn_opval.u;
	    }

	  if ((opcode->flags & S390_INSTR_FLAG_OPTPARM)
	      && val.u == 0
	      && opindex[1] == 0)
	    break;

	  info->fprintf_styled_func (info->stream, dis_style_text,
				     "%c", separator);
	  style = ((flags & S390_OPERAND_DISP)
		   ? dis_style_address_offset : dis_style_immediate);
	  info->fprintf_styled_func (info->stream, style, "%u", val.u);
	}

      if (flags & S390_OPERAND_DISP)
	separator = '(';
      else if (flags & S390_OPERAND_BASE)
	{
	  info->fprintf_styled_func (info->stream, dis_style_text, ")");
	  separator = ',';
	}
      else
	separator = ',';
    }

  /* Optional: instruction name.  */
  if (option_print_insn_desc && opcode->description
      && opcode->description[0] != '\0')
    info->fprintf_styled_func (info->stream, dis_style_comment_start, "\t# %s",
			       opcode->description);
}

/* Check whether opcode A's mask is more specific than that of B.  */

static int
opcode_mask_more_specific (const struct s390_opcode *a,
			   const struct s390_opcode *b)
{
  return (((int) a->mask[0] + a->mask[1] + a->mask[2]
	   + a->mask[3] + a->mask[4] + a->mask[5])
	  > ((int) b->mask[0] + b->mask[1] + b->mask[2]
	     + b->mask[3] + b->mask[4] + b->mask[5]));
}

/* Print a S390 instruction.  */

static int
print_insn_s390 (bfd_vma memaddr, const bfd_byte *buffer,
                 struct disassemble_info *info)
{
  const struct s390_opcode *opcode = NULL;

  /* Set some defaults for the insn info.  */
  info->insn_info_valid    = 0;
  info->branch_delay_insns = 0;
  info->data_size          = 0;
  info->insn_type          = dis_nonbranch;
  info->target             = 0;
  info->target2            = 0;

#if 0
  /* Every S390 instruction is max 6 bytes long.  */
  memset (buffer, 0, 6);
  status = info->read_memory_func (memaddr, buffer, 6, info);
  if (status != 0)
    {
      for (bufsize = 0; bufsize < 6; bufsize++)
	if (info->read_memory_func (memaddr, buffer, bufsize + 1, info) != 0)
	  break;
      if (bufsize <= 0)
	{
	  info->memory_error_func (status, memaddr, info);
	  return -1;
	}
      opsize = s390_insn_length (buffer);
      status = opsize > bufsize;
    }
  else
    {
      bufsize = 6;
      opsize = s390_insn_length (buffer);
    }
#endif
  int status = 0;
  int opsize = s390_insn_length (buffer);
  
  if (status == 0)
    {
      const struct s390_opcode *op;

      /* Find the "best match" in the opcode table.  */
      for (op = s390_opcodes + opc_index[buffer[0]];
	   op != s390_opcodes + s390_num_opcodes
	     && op->opcode[0] == buffer[0];
	   op++)
	{
	  if ((op->modes & current_arch_mask)
	      && s390_insn_matches_opcode (buffer, op)
	      && (opcode == NULL
		  || opcode_mask_more_specific (op, opcode)))
	    opcode = op;
	}

      if (opcode != NULL)
	{
	  /* Provide info for jump visualization.  Must be done before print.  */
	  switch (opcode->flags & S390_INSTR_FLAG_CLASS_MASK)
	    {
	    case S390_INSTR_FLAGS_CLASS_JUMP:
	      info->insn_type = dis_branch;
	      break;
	    case S390_INSTR_FLAGS_CLASS_CONDJUMP:
	      info->insn_type = dis_condbranch;
	      break;
	    case S390_INSTR_FLAGS_CLASS_JUMPSR:
	      info->insn_type = dis_jsr;
	      break;
	    default:
	      info->insn_type = dis_nonbranch;
	    }
	  info->insn_info_valid = 1;

	  /* The instruction is valid.  Print it and return its size.  */
          s390_print_insn_with_opcode (memaddr, info, buffer, opcode);
	  return opsize;
	}
    }
  return 0;   // failed
}

#if 0
  /* For code sections it makes sense to skip unknown instructions
     according to their length bits.  */
  if (status == 0
      && option_use_insn_len_bits_p
      && info->section != NULL
      && (info->section->flags & SEC_CODE))
    bytes_to_dump = opsize;
  else
    /* By default unknown instructions are printed as .long's/.short'
       depending on how many bytes are available.  */
    bytes_to_dump = bufsize >= 4 ? 4 : bufsize;

  if (bytes_to_dump == 0)
    return 0;

  info->insn_type = dis_noninsn;
  info->insn_info_valid = 1;

  /* Fall back to hex print.  */
  switch (bytes_to_dump)
    {
    case 4:
      value = (unsigned int) buffer[0];
      value = (value << 8) + (unsigned int) buffer[1];
      value = (value << 8) + (unsigned int) buffer[2];
      value = (value << 8) + (unsigned int) buffer[3];
      info->fprintf_styled_func (info->stream, dis_style_assembler_directive,
				 ".long");
      info->fprintf_styled_func (info->stream, dis_style_text,
				 "\t");
      info->fprintf_styled_func (info->stream, dis_style_immediate,
				 "0x%08x", value);
      return 4;
    case 2:
      value = (unsigned int) buffer[0];
      value = (value << 8) + (unsigned int) buffer[1];
      info->fprintf_styled_func (info->stream, dis_style_assembler_directive,
				 ".short");
      info->fprintf_styled_func (info->stream, dis_style_text,
				 "\t");
      info->fprintf_styled_func (info->stream, dis_style_immediate,
				 "0x%04x", value);
      return 2;
    default:
      info->fprintf_styled_func (info->stream, dis_style_assembler_directive,
				 ".byte");
      info->fprintf_styled_func (info->stream, dis_style_text,
				 "\t");
      info->fprintf_styled_func (info->stream, dis_style_immediate,
				 "0x%02x", (unsigned int) buffer[0]);
      for (i = 1; i < bytes_to_dump; i++)
	info->fprintf_styled_func (info->stream, dis_style_immediate,
				   "0x%02x", (unsigned int) buffer[i]);
      return bytes_to_dump;
    }
  return 0;
}

const disasm_options_and_args_t *
disassembler_options_s390 (void)
{
  static disasm_options_and_args_t *opts_and_args;

  if (opts_and_args == NULL)
    {
      size_t i, num_options = ARRAY_SIZE (options);
      disasm_options_t *opts;

      opts_and_args = XNEW (disasm_options_and_args_t);
      opts_and_args->args = NULL;

      opts = &opts_and_args->options;
      opts->name = XNEWVEC (const char *, num_options + 1);
      opts->description = XNEWVEC (const char *, num_options + 1);
      opts->arg = NULL;
      for (i = 0; i < num_options; i++)
	{
	  opts->name[i] = options[i].name;
	  opts->description[i] = _(options[i].description);
	}
      /* The array we return must be NULL terminated.  */
      opts->name[i] = NULL;
      opts->description[i] = NULL;
    }

  return opts_and_args;
}

void
print_s390_disassembler_options (FILE *stream)
{
  unsigned int i, max_len = 0;
  fprintf (stream, _("\n\
The following S/390 specific disassembler options are supported for use\n\
with the -M switch (multiple options should be separated by commas):\n"));

  for (i = 0; i < ARRAY_SIZE (options); i++)
    {
      unsigned int len = strlen (options[i].name);
      if (max_len < len)
	max_len = len;
    }

  for (i = 0, max_len++; i < ARRAY_SIZE (options); i++)
    fprintf (stream, "  %s%*c %s\n",
	     options[i].name,
	     (int)(max_len - strlen (options[i].name)), ' ',
	     _(options[i].description));
}
#endif


/* Pseudo FILE object for strings.  */
typedef struct {
  char   buf[128];   // holds the disassembled insn; large enough
  size_t pos;
  size_t alloc;
  unsigned (*val_vsprintf)(char *, const char *, va_list);  // == vex_vsprintf
  struct disassemble_info *info;
} SFILE;

static unsigned
sprintf_cb(SFILE *f, enum disassembler_style style, const char *fmt, ...)
{
  va_list args;
  unsigned space = f->alloc - f->pos;
  unsigned n;
  
  va_start(args, fmt);
  n = f->val_vsprintf(f->buf + f->pos, fmt, args);
  va_end(args);

  if (space <= n)
    vassert(0);

  f->pos += n;

  return n;
}

static void
objdump_print_pcrel(bfd_vma addr, struct disassemble_info *info)
{
  /* this really is a signed offset in bytes */
  long long offset = (long long)addr;

  (*info->fprintf_styled_func)(info->stream, dis_style_text,
                               (offset < 0) ? ".%lld" : ".+%lld", offset);
}


static void
s390_disasm_init(SFILE *dis, unsigned (*fp)(char *, const char *, va_list))
{
  static struct disassemble_info info;

  init_disassemble_info(&info, dis, NULL,
                        (fprintf_styled_ftype) sprintf_cb);
  disassemble_init_s390(&info);
  info.print_address_func = objdump_print_pcrel;

  /* Initialise the SFILE object */
  dis->val_vsprintf = fp;
  dis->pos = 0;
  dis->alloc = sizeof dis->buf;
  dis->buf[0] = '\0';
  dis->info = &info;
}


/* Watch out: returned string is allocated in a static buffer and will
   be overwritten in the next invocation. */
HChar *
s390_disasm(const unsigned char insn[], int pad_mnm)
{
  static int initialised = 0;
  static SFILE disasm;

  if (! initialised) {
     initialised = 1;
     s390_disasm_init(&disasm, vex_vsprintf);
  }

  pad_mnemonic = pad_mnm;
  disasm.pos = 0;        /* reset */
  disasm.buf[0] = '\0';
  
  /* Passing 0 as an address which is only used for symbolically print
     a PC-relative operand like so  <name+offset>. That means: in
     objdump_print_pcrel we get the offset and can write it out relative to
     the current address:  .+offset of .-offset */
  int rc = print_insn_s390(/* address */ 0, insn, disasm.info);

  return rc == 0 ? NULL : disasm.buf;
}
