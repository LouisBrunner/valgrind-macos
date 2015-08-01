//gcc a.c ../../../VEX/priv/tilegx_disasm.c   -I ../../../  -I ../../../VEX/priv/  -I ../../../VEX/pub/

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "tilegx_disasm.h"

#undef DGB

static unsigned char op_abnorm[TILEGX_OPC_NONE] = {
  /* Black list */
  [ TILEGX_OPC_BPT      ] = 1,
  [ TILEGX_OPC_INFO     ] = 1,
  [ TILEGX_OPC_INFOL    ] = 1,
  [ TILEGX_OPC_DRAIN    ] = 1,
  [ TILEGX_OPC_IRET     ] = 1,
  [ TILEGX_OPC_SWINT0   ] = 1,
  [ TILEGX_OPC_SWINT1   ] = 1,
  [ TILEGX_OPC_SWINT2   ] = 1,
  [ TILEGX_OPC_SWINT3   ] = 1,
  [ TILEGX_OPC_LD4S_TLS ] = 1,
  [ TILEGX_OPC_LD_TLS   ] = 1,
  [ TILEGX_OPC_MFSPR    ] = 1,
  [ TILEGX_OPC_MTSPR    ] = 1,
  [ TILEGX_OPC_ILL      ] = 1,
  [ TILEGX_OPC_NAP      ] = 1,

  /* mem load */
  [ TILEGX_OPC_LD         ] = 2,
  [ TILEGX_OPC_LD_ADD     ] = 2,
  [ TILEGX_OPC_LD1S       ] = 2,
  [ TILEGX_OPC_LD1S_ADD   ] = 2,
  [ TILEGX_OPC_LD1U       ] = 2,
  [ TILEGX_OPC_LD1U_ADD   ] = 2,
  [ TILEGX_OPC_LD2S       ] = 2,
  [ TILEGX_OPC_LD2S_ADD   ] = 2,
  [ TILEGX_OPC_LD2U       ] = 2,
  [ TILEGX_OPC_LD2U_ADD   ] = 2,
  [ TILEGX_OPC_LD4S       ] = 2,
  [ TILEGX_OPC_LD4S_ADD   ] = 2,
  [ TILEGX_OPC_LD4U       ] = 2,
  [ TILEGX_OPC_LD4U_ADD   ] = 2,
  [ TILEGX_OPC_LDNA       ] = 2,
  [ TILEGX_OPC_LDNA_ADD   ] = 2,
  [ TILEGX_OPC_LDNT       ] = 2,
  [ TILEGX_OPC_LDNT1S     ] = 2,
  [ TILEGX_OPC_LDNT1S_ADD ] = 2,
  [ TILEGX_OPC_LDNT1U     ] = 2,
  [ TILEGX_OPC_LDNT1U_ADD ] = 2,
  [ TILEGX_OPC_LDNT2S     ] = 2,
  [ TILEGX_OPC_LDNT2S_ADD ] = 2,
  [ TILEGX_OPC_LDNT2U     ] = 2,
  [ TILEGX_OPC_LDNT2U_ADD ] = 2,
  [ TILEGX_OPC_LDNT4S     ] = 2,
  [ TILEGX_OPC_LDNT4S_ADD ] = 2,
  [ TILEGX_OPC_LDNT4U     ] = 2,
  [ TILEGX_OPC_LDNT4U_ADD ] = 2,
  [ TILEGX_OPC_LDNT_ADD   ] = 2,

  /* mem store */
  [ TILEGX_OPC_ST         ] = 4,
  [ TILEGX_OPC_ST1        ] = 4,
  [ TILEGX_OPC_ST1_ADD    ] = 4,
  [ TILEGX_OPC_ST2        ] = 4,
  [ TILEGX_OPC_ST2_ADD    ] = 4,
  [ TILEGX_OPC_ST4        ] = 4,
  [ TILEGX_OPC_ST4_ADD    ] = 4,
  [ TILEGX_OPC_ST_ADD     ] = 4,
  [ TILEGX_OPC_STNT       ] = 4,
  [ TILEGX_OPC_STNT1      ] = 4,
  [ TILEGX_OPC_STNT1_ADD  ] = 4,
  [ TILEGX_OPC_STNT2      ] = 4,
  [ TILEGX_OPC_STNT2_ADD  ] = 4,
  [ TILEGX_OPC_STNT4      ] = 4,
  [ TILEGX_OPC_STNT4_ADD  ] = 4,
  [ TILEGX_OPC_STNT_ADD   ] = 4,

  /* conditional branch */
  [ TILEGX_OPC_BEQZ       ] = 8,
  [ TILEGX_OPC_BEQZT      ] = 8,
  [ TILEGX_OPC_BGEZ       ] = 8,
  [ TILEGX_OPC_BGEZT      ] = 8,
  [ TILEGX_OPC_BGTZ       ] = 8,
  [ TILEGX_OPC_BGTZT      ] = 8,
  [ TILEGX_OPC_BLBC       ] = 8,
  [ TILEGX_OPC_BLBCT      ] = 8,
  [ TILEGX_OPC_BLBS       ] = 8,
  [ TILEGX_OPC_BLBST      ] = 8,
  [ TILEGX_OPC_BLEZ       ] = 8,
  [ TILEGX_OPC_BLEZT      ] = 8,
  [ TILEGX_OPC_BLTZ       ] = 8,
  [ TILEGX_OPC_BLTZT      ] = 8,
  [ TILEGX_OPC_BNEZ       ] = 8,
  [ TILEGX_OPC_BNEZT      ] = 8,
};


static tilegx_bundle_bits
encode_insn_tilegx_X (int p, struct tilegx_decoded_instruction decoded);

static tilegx_bundle_bits
encode_insn_tilegx_Y (int p, struct tilegx_decoded_instruction decoded);

static int decode( tilegx_bundle_bits *p, int count, ULong pc );

static uint64_t
RAND(int round) {
  static volatile uint64_t rand_seed = 0;
  while (round-- > 0)
    rand_seed = (rand_seed >> 8) * 201520052007 + 1971;
#ifdef DBG
  printf("RAND: %d\n", (int)rand_seed);
#endif
  return rand_seed;
}


int main(int argc, char* argv[])
{
  int i, start, end, pipe;
  struct tilegx_decoded_instruction decoded;
  if (argc == 1) {
    pipe = 0x1F;
    start = 0;
    end = TILEGX_OPC_NONE;
  } else if (argc == 3) {
    start = atoi(argv[1]);

    if (start >= TILEGX_OPC_NONE)
      return -1;

    end = start + 1;
    /* pipes: X: bit 0,1; Y: bit 2-4 */
    pipe = atoi(argv[2]);
  } else {
    return -1;
  }

  for (i = start; i < end; i++) {
    memset(&decoded, 0, sizeof(decoded));
    const struct tilegx_opcode *opcode = &tilegx_opcodes[i];
    decoded.opcode = opcode;
#ifdef DBG
    const char *op_name = decoded.opcode->name;
    printf("\n\n%d) %s\n", i, op_name);
#endif

    if (op_abnorm[i] & 1)
      continue;

    /* X0 pipeline */
    if (tilegx_opcodes[i].pipes & 1 & pipe)
      encode_insn_tilegx_X(0, decoded);

    /* X1 pipeline */
    if (tilegx_opcodes[i].pipes & 2 & pipe)
      encode_insn_tilegx_X(1, decoded);

    /* Y0 pipleline */
    if (tilegx_opcodes[i].pipes & 4 & pipe)
      encode_insn_tilegx_Y(0, decoded);

    /* Y1 pipleline */
    if (tilegx_opcodes[i].pipes & 8 & pipe)
      encode_insn_tilegx_Y(1, decoded);

    /* Y2 pipleline */
    if (tilegx_opcodes[i].pipes & 16 & pipe)
      encode_insn_tilegx_Y(2, decoded);
  }

  return 0;
}

static tilegx_bundle_bits
encode_insn_tilegx_X(int p, struct tilegx_decoded_instruction decoded)
{
  const struct tilegx_opcode *opc =
    decoded.opcode;
  int op_idx =  decoded.opcode->mnemonic;

  tilegx_bundle_bits insn = 0;
  //int pipeX01 = (opc->pipes & 0x01) ? 0 : 1;
  int op_num  = opc->num_operands;

  /* Assume either X0 or X1. */
  if ((opc->pipes & 3) == 0)
    return -1;

  /* Insert fnop in other pipe. */
  insn = tilegx_opcodes[TILEGX_OPC_FNOP].
    fixed_bit_values[p ? 0 : 1];
#ifdef DBG
  printf(" X%d, ", p);
#endif

  insn |= opc->fixed_bit_values[p];

  printf("//file: _insn_test_%s_X%d.c\n", decoded.opcode->name, p);
  printf("//op=%d\n", op_idx);
  printf("#include <stdio.h>\n");
  printf("#include <stdlib.h>\n");

  printf("\n"
	 "void func_exit(void) {\n"
	 "     printf(\"%cs\\n\", __func__);\n"
	 "     exit(0);\n"
	 "}\n"
	 "\n"
	 "void func_call(void) {\n"
	 "     printf(\"%cs\\n\", __func__);\n"
	 "     exit(0);\n"
	 "}\n"
	 "\n"
	 "unsigned long mem[2] = { 0x%lx, 0x%lx };\n"
	 "\n", '%', '%', RAND(op_idx), RAND(op_idx));

  printf("int main(void) {\n");
  printf("    unsigned long a[4] = { 0, 0 };\n");

  printf("    asm __volatile__ (\n");

  int i, n = 0;

  if (op_abnorm[op_idx] & 6)
    {
      /* loop for each operand. */
      for (i = 0 ; i < op_num; i++)
	{
	  const struct tilegx_operand *opd =
	    &tilegx_operands[opc->operands[p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand, pick register 0-50 randomly. */
	    decoded.operand_values[i] = RAND(op_idx) % 51;
	    int r = decoded.operand_values[i];
	    int64_t d = RAND(op_idx);
#ifdef DBG
	    printf(" %d) r%-2d %016lx\n", i, (int)r, (unsigned long)d);
#endif
	    int k = 0;
	    for (k = 3; k >= 0 ; k--) {
	      if (d >> (16 * k) || k == 0) {
		printf("                      \"moveli r%d, %d\\n\"\n", r, (int)(d >> (16 * k)));
		for (k--; k >= 0; k--)
		  printf("                      \"shl16insli r%d, r%d, %d\\n\"\n", r, r, (int)(int16_t)(d >> (16 * k)));
		break;
	      }
	    }
	  } else {
	    /* An immediate operand, pick a random value. */
	    decoded.operand_values[i] = RAND(op_idx);
#ifdef DBG
	    printf(" %d) %016lx\n", (int)i, (unsigned long)decoded.operand_values[i]);
#endif
	  }

	  Long  op = decoded.operand_values[i];
	  decoded.operands[i] = opd;
	  ULong x = opd->insert(op);
	  insn |= x;
	}
      printf("                      \"");
      if (op_abnorm[op_idx] & 2)
	printf("move r%d, %c2\\n\"\n",  (int)decoded.operand_values[1], '%');
      else
	printf("move r%d, %c2\\n\"\n",  (int)decoded.operand_values[0], '%');

      printf("                      \"");
      decode(&insn, 2, 0);
      printf("\\n\"\n");

      /* loop for each operand. */
      n = 0;
      for (i = 0 ; i < op_num; i++)
	{
	  const struct tilegx_operand *opd =
	    &tilegx_operands[opc->operands[p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand */
	    printf("                      \"move %c%d, r%d\\n\"\n", '%', n, (int)decoded.operand_values[i]);
	    n++;
	  }
	}

      printf("                      ");
      if (n)
	printf(":");
      for (i = 0; i < n; i++)
	{
	  printf("\"=r\"(a[%d])", i);
	  if (i != n - 1)
	    printf(",");
	}
      printf(" : \"r\"(mem)");

      printf(");\n");

      printf("    printf(\"%c016lx %c016lx\\n\", mem[0], mem[1]);\n", '%', '%');

    }
  else if (op_idx == TILEGX_OPC_J)
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"i\"(func_exit));\n");
    }
  else if (op_idx == TILEGX_OPC_JAL)
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"i\"(func_call));\n");
    }
  else if (op_idx == TILEGX_OPC_JR || op_idx == TILEGX_OPC_JRP)
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"r\"(func_exit));\n");
    }
  else if (op_idx == TILEGX_OPC_JALR || op_idx == TILEGX_OPC_JALRP )
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"r\"(func_call));\n");
    }
  else if (op_abnorm[op_idx] & 8)
    {
      // OPC_BXXX  conditional branch
      int r = RAND(op_idx) % 51;
      int d = RAND(op_idx) & 1;
      printf("                     \"movei r%d, %d\\n\"\n", r, d);
      printf("                     \"%s r%d,  %c0\\n\"\n", decoded.opcode->name, r, '%');
      printf("                     \"jal %c1\\n\"\n", '%');
      printf("                     :: \"i\"(func_exit), \"i\"(func_call));\n");
    }
  else
    {
      /* loop for each operand. */
      for (i = 0 ; i < op_num; i++)
	{
	  const struct tilegx_operand *opd =
	    &tilegx_operands[opc->operands[p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand, pick register 0-50 randomly. */
	    decoded.operand_values[i] = RAND(op_idx) % 51;
	    int r = decoded.operand_values[i];
	    int64_t d = RAND(op_idx);

#ifdef DBG
	    printf(" %d) r%-2d %016lx\n", i, (int)r, (unsigned long)d);
#endif
	    int k = 0;
	    for (k = 3; k >= 0 ; k--) {
	      if (d >> (16 * k) || k == 0) {
		printf("                      \"moveli r%d, %d\\n\"\n", r, (int)(d >> (16 * k)));
		for (k--; k >= 0; k--)
		  printf("                      \"shl16insli r%d, r%d, %d\\n\"\n", r, r, (int)(int16_t)(d >> (16 * k)));
		break;
	      }
	    }
	  } else {
	    /* An immediate operand, pick a random value. */
	    decoded.operand_values[i] = RAND(op_idx);
#ifdef DBG
	    printf(" %d) %016lx\n", (int)i, (unsigned long)decoded.operand_values[i]);
#endif
	  }

	  Long  op = decoded.operand_values[i];
	  decoded.operands[i] = opd;
	  ULong x = opd->insert(op);
	  insn |= x;
	}
      printf("                      \"");
      decode(&insn, 2, 0);
      printf("\\n\"\n");

      /* loop for each operand. */
      n = 0;
      for (i = 0 ; i < op_num; i++)
	{
	  const struct tilegx_operand *opd =
	    &tilegx_operands[opc->operands[p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand */
	    printf("                      \"move %c%d, r%d\\n\"\n", '%', n, (int)decoded.operand_values[i]);
	    n++;
	  }
	}

      printf("                      ");
      if (n)
	printf(":");
      for (i = 0; i < n; i++)
	{
	  printf("\"=r\"(a[%d])", i);
	  if (i != n - 1)
	    printf(",");
	}

      printf(");\n");
    }

  for (i = 0; i < n; i++)
    {
      printf("    printf(\"%c016lx\\n\", a[%d]);\n", '%', i);
    }
  printf("    return 0;\n");
  printf("}\n");
  return insn;
}

static tilegx_bundle_bits
encode_insn_tilegx_Y (int p, struct tilegx_decoded_instruction decoded )
{
  int i;
  const struct tilegx_opcode *opc =
    decoded.opcode;
  int op_idx =  decoded.opcode->mnemonic;

  const struct tilegx_operand *opd;

  tilegx_bundle_bits insn = 0;
  Int op_num  = opc->num_operands;

  /* Insert fnop in Y0 and Y1 pipeline. */
  if (p != 0)
    insn |= tilegx_opcodes[TILEGX_OPC_FNOP].
      fixed_bit_values[2];

  if (p != 1)
    insn |= tilegx_opcodes[TILEGX_OPC_FNOP].
      fixed_bit_values[3];

  /* Fill-in Y2 as dumy load "ld zero, sp" */
  if (p != 2) {
    insn |= tilegx_opcodes[TILEGX_OPC_LD].
      fixed_bit_values[4];
    opd = &tilegx_operands[tilegx_opcodes[TILEGX_OPC_LD].operands[4][0]];
    insn |= opd->insert(63);
    opd = &tilegx_operands[tilegx_opcodes[TILEGX_OPC_LD].operands[4][1]];
    insn |= opd->insert(54);
  }
#ifdef DBG
  printf(" Y%d, ", p);
#endif

  insn |= opc->fixed_bit_values[2 + p];

  printf("//file: _insn_test_%s_Y%d.c\n", decoded.opcode->name, p);
  printf("//op=%d\n", op_idx);
  printf("#include <stdio.h>\n");
  printf("#include <stdlib.h>\n");

  printf("\n"
	 "void func_exit(void) {\n"
	 "     printf(\"%cs\\n\", __func__);\n"
	 "     exit(0);\n"
	 "}\n"
	 "\n"
	 "void func_call(void) {\n"
	 "     printf(\"%cs\\n\", __func__);\n"
	 "     exit(0);\n"
	 "}\n"
	 "\n"
	 "unsigned long mem[2] = { 0x%lx, 0x%lx };\n"
	 "\n", '%', '%', RAND(op_idx), RAND(op_idx));

  printf("int main(void) {\n");
  printf("    unsigned long a[4] = { 0, 0 };\n");

  printf("    asm __volatile__ (\n");

  int n = 0;

  if (op_abnorm[op_idx] & 6)
    {
      /* loop for each operand. */
      for (i = 0 ; i < op_num; i++)
	{
          opd = &tilegx_operands[opc->operands[2 + p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand, pick register 0-53 randomly. */
	    decoded.operand_values[i] = RAND(op_idx) % 53;
	    int r = decoded.operand_values[i];
	    int64_t d = RAND(op_idx);
#ifdef DBG
	    printf(" %d) r%-2d %016lx\n", i, (int)r, (unsigned long)d);
#endif
	    int k = 0;
	    for (k = 3; k >= 0 ; k--) {
	      if (d >> (16 * k) || k == 0) {
		printf("                      \"moveli r%d, %d\\n\"\n", r, (int)(d >> (16 * k)));
		for (k--; k >= 0; k--)
		  printf("                      \"shl16insli r%d, r%d, %d\\n\"\n", r, r, (int)(int16_t)(d >> (16 * k)));
		break;
	      }
	    }
	  } else {
	    /* An immediate operand, pick a random value. */
	    decoded.operand_values[i] = RAND(op_idx);
#ifdef DBG
	    printf(" %d) %016lx\n", (int)i, (unsigned long)decoded.operand_values[i]);
#endif
	  }

	  Long  op = decoded.operand_values[i];
	  decoded.operands[i] = opd;
	  ULong x = opd->insert(op);
	  insn |= x;
	}
      printf("                      \"");
      if (op_abnorm[op_idx] & 2)
	printf("move r%d, %c2\\n\"\n",  (int)decoded.operand_values[1], '%');
      else
	printf("move r%d, %c2\\n\"\n",  (int)decoded.operand_values[0], '%');

      printf("                      \"");
      decode(&insn, 3, 0);
      printf("\\n\"\n");

      /* loop for each operand. */
      n = 0;
      for (i = 0 ; i < op_num; i++)
	{
          opd = &tilegx_operands[opc->operands[2 + p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand */
	    printf("                      \"move %c%d, r%d\\n\"\n", '%', n, (int)decoded.operand_values[i]);
	    n++;
	  }
	}

      printf("                      ");
      if (n)
	printf(":");
      for (i = 0; i < n; i++)
	{
	  printf("\"=r\"(a[%d])", i);
	  if (i != n - 1)
	    printf(",");
	}
      printf(" : \"r\"(mem)");

      printf(");\n");

      printf("    printf(\"%c016lx %c016lx\\n\", mem[0], mem[1]);\n", '%', '%');

    }
  else if (op_idx == TILEGX_OPC_J)
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"i\"(func_exit));\n");
    }
  else if (op_idx == TILEGX_OPC_JAL)
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"i\"(func_call));\n");
    }
  else if (op_idx == TILEGX_OPC_JR || op_idx == TILEGX_OPC_JRP)
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"r\"(func_exit));\n");
    }
  else if (op_idx == TILEGX_OPC_JALR || op_idx == TILEGX_OPC_JALRP )
    {
      printf("                     \"%s %c0\\n\"\n", decoded.opcode->name, '%');
      printf("                     :: \"r\"(func_call));\n");
    }
  else if (op_abnorm[op_idx] & 8)
    {
      // OPC_BXXX  conditional branch
      int r = RAND(op_idx) % 51;
      int d = RAND(op_idx) & 1;
      printf("                     \"movei r%d, %d\\n\"\n", r, d);
      printf("                     \"%s r%d,  %c0\\n\"\n", decoded.opcode->name, r, '%');
      printf("                     \"jal %c1\\n\"\n", '%');
      printf("                     :: \"i\"(func_exit), \"i\"(func_call));\n");
    }
  else
    {
      /* loop for each operand. */
      for (i = 0 ; i < op_num; i++)
	{
          opd = &tilegx_operands[opc->operands[2 + p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand, pick register 0-50 randomly. */
	    decoded.operand_values[i] = RAND(op_idx) % 51;
	    int r = decoded.operand_values[i];
	    int64_t d = RAND(op_idx);

#ifdef DBG
	    printf(" %d) r%-2d %016lx\n", i, (int)r, (unsigned long)d);
#endif
	    int k = 0;
	    for (k = 3; k >= 0 ; k--) {
	      if (d >> (16 * k) || k == 0) {
		printf("                      \"moveli r%d, %d\\n\"\n", r, (int)(d >> (16 * k)));
		for (k--; k >= 0; k--)
		  printf("                      \"shl16insli r%d, r%d, %d\\n\"\n", r, r, (int)(int16_t)(d >> (16 * k)));
		break;
	      }
	    }
	  } else {
	    /* An immediate operand, pick a random value. */
	    decoded.operand_values[i] = RAND(op_idx);
#ifdef DBG
	    printf(" %d) %016lx\n", (int)i, (unsigned long)decoded.operand_values[i]);
#endif
	  }

	  Long  op = decoded.operand_values[i];
	  decoded.operands[i] = opd;
	  ULong x = opd->insert(op);
	  insn |= x;
	}
      printf("                      \"");
      decode(&insn, 3, 0);
      printf("\\n\"\n");

      /* loop for each operand. */
      n = 0;
      for (i = 0 ; i < op_num; i++)
	{
          opd = &tilegx_operands[opc->operands[2 + p][i]];

	  if (opd->type == TILEGX_OP_TYPE_REGISTER) {
	    /* A register operand */
	    printf("                      \"move %c%d, r%d\\n\"\n", '%', n, (int)decoded.operand_values[i]);
	    n++;
	  }
	}

      printf("                      ");
      if (n)
	printf(":");
      for (i = 0; i < n; i++)
	{
	  printf("\"=r\"(a[%d])", i);
	  if (i != n - 1)
	    printf(",");
	}

      printf(");\n");
    }

  for (i = 0; i < n; i++)
    {
      printf("    printf(\"%c016lx\\n\", a[%d]);\n", '%', i);
    }
  printf("    return 0;\n");
  printf("}\n");
  return insn;
}

static int display_insn ( struct tilegx_decoded_instruction
                          decoded[1] )
{
  int i;
  for (i = 0;
       decoded[i].opcode && (i < 1);
       i++) {
    int n;
    printf("%s ", decoded[i].opcode->name);

    for (n = 0; n < decoded[i].opcode->num_operands; n++) {
      const struct tilegx_operand *op = decoded[i].operands[n];

      if (op->type == TILEGX_OP_TYPE_REGISTER)
        printf("r%d", (int) decoded[i].operand_values[n]);
      else
        printf("%ld", (unsigned long)decoded[i].operand_values[n]);

      if (n != (decoded[i].opcode->num_operands - 1))
        printf(", ");
    }
    printf(" ");
  }
  return i;
}

int decode( tilegx_bundle_bits *p, int count, ULong pc )
{
  struct tilegx_decoded_instruction
    decode[TILEGX_MAX_INSTRUCTIONS_PER_BUNDLE];

  if (pc) {
    printf("%012llx %016llx  ", pc, (ULong)p[0]);
    pc += 8;
  }
  parse_insn_tilegx(p[0], 0, decode);

  int k;

  printf("{ ");

  for(k = 0; decode[k].opcode && (k <TILEGX_MAX_INSTRUCTIONS_PER_BUNDLE);
      k++) {

    display_insn(&decode[k]);
    if (--count > 0)
      printf("; ");
  }

  printf(" }");

  return count;
}
