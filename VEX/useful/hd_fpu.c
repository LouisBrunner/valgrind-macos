
/*--------------------------------------------------------------------*/
/*--- Implementation of the floating point instruction set.        ---*/
/*---                                                     hd_fpu.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Heimdall, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "hd_include.h"


/* ---------------------------------------------------------------------
   Packing and unpacking the FPU data registers.
   ------------------------------------------------------------------ */

INLINE
UInt fp_get_tos ( void )
{
   return (m_fpu_state.env[FP_ENV_STAT] >> FP_F_TOS_LO) & 7;
}

static
UInt read_bit_array ( UChar* arr, UInt n )
{
   UChar c = arr[n >> 3];
   c >>= (n&7);
   return c & 1;
}

static
void write_bit_array ( UChar* arr, UInt n, UInt b )
{
   UChar c = arr[n >> 3];
   c &= ~(1 << (n&7));
   b &= 1;
   c |=  (b << (n&7));
   arr[n >> 3] = c;
}

/* Read an IEEE double from the memory image of an Intel 80-bit
   extended floating-point number.
*/
static
double fp_double_from_extended ( UChar* e_lsb )
{
   int i;
   double d;
   UChar* d_lsb = (UChar*)(&d);

   UInt sign = e_lsb[9] >> 7;
   Int bexp = ((UInt)e_lsb[9] << 8) | (UInt)e_lsb[8];
   bexp &= 0x7fff;

   if (bexp == 0) 
      bexp = 0;  /* preserve zeroes */
   else 
   if (bexp == 0x7FFF) 
      bexp = 0x7FF; /* preserve Infs/Nans */
   else {
      bexp -= (16383 - 1023);
      if (bexp < 0) bexp = 0;
      if (bexp > 0x7FF) bexp = 0x7FF;
   }

   d_lsb[6] = (bexp & 0xF) << 4;
   d_lsb[7] = ((bexp >> 4) & 0x7F) | ((sign & 0x1) << 7);

   for (i = 0; i < 52; i++)
      write_bit_array ( d_lsb,
                        i,
                        read_bit_array ( e_lsb, i+11 ) );
   return d;
}

/* Given an IEEE double, create the memory image of an Intel 80-bit
   extended floating-point number.
*/
static
void fp_extended_from_double ( UChar* e_lsb, double d )
{
   int i;
   UChar* d_lsb = (UChar*)(&d);

   UInt sign = d_lsb[7] >> 7;
   Int bexp = ((UInt)d_lsb[7] << 4) |
               ((((UInt)d_lsb[6]) >> 4) & 0xF);
   bexp &= 0x7ff;

   if (bexp == 0) 
      bexp = 0;  /* preserve zeroes */
   else 
   if (bexp == 0x7FF) 
      bexp = 0x7FFF; /* preserve Infs/Nans */
   else
      bexp += (16383 - 1023);

   e_lsb[9] = ((bexp >> 8) & 0x7F) | ((sign & 0x1) << 7);
   e_lsb[8] = bexp & 0xFF;

   for (i = 0; i < 52; i++)
      write_bit_array ( e_lsb,
                        i+11,
                        read_bit_array ( d_lsb, i ) );
   for (i = 0; i < 11; i++)
      write_bit_array ( e_lsb, i, 0 );

   /* this isn't really right, but I can't get fpclassify to work. */
   i = 0;
   if (isnan(d) || isinf(d) || d != 0.0) i = 1;
   write_bit_array ( e_lsb, 63, i );
}

/* For the transition Real CPU -> Simulated CPU, copy the 
   .reg values in m_fpu_state, which are in stack order, to
   the m_fpu_data_regs array, in register (non-stack) order.
*/
void fp_unpack_data_regs ( void )
{
   Int reg, st;
   reg = fp_get_tos();
   for (st = 0; st < 8; st++) {
      m_fpu_data_regs[reg]
         = fp_double_from_extended ( &m_fpu_state.reg[FP_REG(st)] );
      if (reg == 7) reg = 0; else reg++;
   }
}

void fp_repack_data_regs ( void )
{
   Int reg, st;
   st = fp_get_tos();
   for (reg = 0; reg < 8; reg++) {
      fp_extended_from_double ( &m_fpu_state.reg[FP_REG(reg)], 
                                m_fpu_data_regs[st] );
      if (st == 7) st = 0; else st++;
   }
}

/* ---------------------------------------------------------------------
   Helper functions for the floating point unit.
   ------------------------------------------------------------------ */

static
INLINE
void setFMem ( UInt addr, double f )
{
   * ((float*)addr) = (float)f;
}

static
INLINE
double getFMem ( UInt addr )
{
   return (double) (* ((float*)addr));
}

static
INLINE
void setDMem ( UInt addr, double f )
{
   * ((double*)addr) = f;
}

static
INLINE
double getDMem ( UInt addr )
{
   return (* ((double*)addr));
}

static
INLINE
void setTMem ( UInt addr, double f )
{
   fp_extended_from_double ( (Addr)addr, f );
}

static
INLINE
double getTMem ( UInt addr )
{
   return fp_double_from_extended ( (Addr)addr );
}

#define fp_extended_from_double ERROR__fp_extended_from_double_used
#define fp_double_from_extended ERROR__fp_double_from_extended_used

static
INLINE
UInt fp_get_statusword_flag ( UInt flagno )
{
   if (flagno < 0 || flagno > 15) panic("fp_get_statusword_flag");
   return (m_fpu_state.env[FP_ENV_STAT] >> flagno) & 0x1;
}

#if DEBUG
static
UInt fp_get_controlword_flag ( UInt flagno )
{
   if (flagno < 0 || flagno > 15) panic("fp_get_controlword_flag");
   return (m_fpu_state.env[FP_ENV_CTRL] >> flagno) & 0x1;
}
#endif

static
INLINE
void fp_set_statusword_flag_to ( UInt flagno, UInt bit )
{
   if (flagno < 0 || flagno > 15) panic("fp_set_statusword_flag_to");
   if (bit)
      m_fpu_state.env[FP_ENV_STAT] |= (1 << flagno);
   else
      m_fpu_state.env[FP_ENV_STAT] &= ~(1 << flagno);
}

static
void fp_set_stack_overflow ( void )
{
   fprintf(stderr, "--- FP STACK OVERFLOW!\n" );
   fp_set_statusword_flag_to(FP_E_INVAL,1);
   fp_set_statusword_flag_to(FP_E_STACKF,1);
   fp_set_statusword_flag_to(FP_F_C1,1);
}

static
void fp_set_stack_underflow ( void )
{
   fprintf(stderr, "--- FP STACK UNDERFLOW!\n" );
   fp_set_statusword_flag_to(FP_E_INVAL,1);
   fp_set_statusword_flag_to(FP_E_STACKF,1);
   fp_set_statusword_flag_to(FP_F_C1,0);
}

static
INLINE
void fp_set_tos ( UInt tos )
{
   if (tos < 0 || tos > 7) panic("fp_set_tos");
   fp_set_statusword_flag_to(FP_F_TOS_LO,0);
   fp_set_statusword_flag_to(FP_F_TOS_LO+1,0);
   fp_set_statusword_flag_to(FP_F_TOS_HI,0);
   m_fpu_state.env[FP_ENV_STAT] |= (tos << FP_F_TOS_LO);
}

static
INLINE
UInt fp_STno_to_regno ( UInt stregno )
{
   UInt regno = fp_get_tos();
   assert(regno >= 0 && regno < 8);
   regno += stregno;
   if (regno >= 8) regno -= 8;
   assert(regno >= 0 && regno < 8);
   return regno;
}

static
INLINE
void fp_dec_tos ( void )
{
   fp_set_tos ( fp_STno_to_regno ( 7 ));
}

static
INLINE
void fp_inc_tos ( void )
{
   fp_set_tos ( fp_STno_to_regno ( 1 ));
}

static
INLINE
Bool fp_is_empty_tag ( UInt tag )
{
   return tag == FP_TAG_EMPTY;
}

static
INLINE
UInt fp_get_tag ( UInt regno )
{
   if (regno < 0 || regno > 7) panic("fp_get_tag");
   return (m_fpu_state.env[FP_ENV_TAG] >> (2*regno)) & 3;
}

static
INLINE
UInt fp_get_tag_ST ( UInt stregno )
{
   if (stregno < 0 || stregno > 7) panic("fp_get_tag_ST");
   return fp_get_tag ( fp_STno_to_regno(stregno) );
}

static
INLINE
void fp_set_tag ( UInt regno, UInt val )
{
   if (regno < 0 || regno > 7 ||
       val < 0 || val > 3) panic("fp_get_tag");
   m_fpu_state.env[FP_ENV_TAG] &= ~(3 << (2*regno));
   m_fpu_state.env[FP_ENV_TAG] |=  (val << (2*regno));
}

static
INLINE
void fp_set_tag_ST ( UInt stregno, UInt val )
{
   if (stregno < 0 || stregno > 7) panic("fp_set_tag_ST");
   fp_set_tag ( fp_STno_to_regno(stregno), val );
}


static
INLINE
void fp_set_reg ( UInt r, double d )
{
   if (r < 0 || r > 7) panic("fp_set_reg");
   m_fpu_data_regs[r] = d;
   fp_set_tag ( r, d==0.0 ? FP_TAG_ZERO 
                          : (finite(d) ? FP_TAG_VALID : FP_TAG_SPEC) );
}

static
INLINE
void fp_set_reg_ST ( UInt str, double d )
{
   UInt r;
   if (str < 0 || str > 7) panic("fp_set_reg_ST");
   r = fp_STno_to_regno(str);
   fp_set_reg ( r, d );
}

static
INLINE
double fp_get_reg ( UInt r )
{
   double d;
   if (r < 0 || r > 7) panic("fp_get_reg");
   d = m_fpu_data_regs[r];
   return d;
}

static
INLINE
double fp_get_reg_ST ( UInt str )
{
   UInt r;
   if (str < 0 || str > 7) panic("fp_get_reg_ST");
   r = fp_STno_to_regno(str);
   return fp_get_reg(r);
}

static
INLINE
void fp_set_tos_reg ( double d )
{
   fp_set_reg ( fp_get_tos(), d );
}

static
INLINE
double fp_get_tos_reg ( void )
{
   return fp_get_reg ( fp_get_tos() );
}

static
INLINE
void fp_set_tos_reg_QNaN ( void )
{
   fp_set_reg ( fp_get_tos(), NAN /* see <nan.h> */ );
}

static
INLINE
double fp_pop ( void )
{
   double d = fp_get_tos_reg();
   fp_set_tag ( fp_get_tos(), FP_TAG_EMPTY );
   fp_inc_tos();
   return d;
}

/* Push d and update flags. */
static
INLINE
void fp_push ( double d )
{
   if (fp_is_empty_tag(fp_get_tag_ST(7))) {
      fp_dec_tos();
      fp_set_tos_reg(d);
      fp_set_statusword_flag_to(FP_F_C1, d == 0.0);
   } else {
      fp_dec_tos();
      fp_set_tos_reg_QNaN();
      fp_set_stack_overflow();
   }
}

static
void fp_set_statusword_flags_COM ( double vd_dst, double vd_src )
{
   UInt vis_dst;
   if (isnan(vd_src) || isnan(vd_dst))  vis_dst = 7;
   else if (vd_dst > vd_src)            vis_dst = 0;
   else if (vd_dst < vd_src)            vis_dst = 1; 
   else if (vd_dst == vd_src)           vis_dst = 4;
   else vis_dst = 7;
   fp_set_statusword_flag_to(FP_F_C3, (vis_dst >> 2) & 1);
   fp_set_statusword_flag_to(FP_F_C2, (vis_dst >> 1) & 1);
   fp_set_statusword_flag_to(FP_F_C0, vis_dst & 1);
}

static
void fp_set_statusword_flags_COM_STACKF ( void )
{
   UInt vis_dst = 7;
   fp_set_statusword_flag_to(FP_F_C3, (vis_dst >> 2) & 1);
   fp_set_statusword_flag_to(FP_F_C2, (vis_dst >> 1) & 1);
   fp_set_statusword_flag_to(FP_F_C0, vis_dst & 1);
}

static
double fp_calc_yl2xp1 ( double st_0, double st_1 )
{
   st_0 += 1.0;
   st_0 = log(st_0) / log(2.0);
   st_0 *= st_1;
   return st_0;
}

static
double fp_calc_yl2x ( double st_0, double st_1 )
{
   st_0 = log(st_0) / log(2.0);
   st_0 *= st_1;
   return st_0;
}

static
double fp_calc_2xm1 ( double st_0 )
{
   st_0 = st_0 * 0.69314718055994530942;
   st_0 = exp(st_0);
   st_0 = st_0 - 1.0;
   return st_0;
}

static
double fp_calc_scale ( double st_0, double st_1 )
{
   Int n = 0;
   if (st_1 > 0.0) {
      if (st_1 > 2.0*308.0) st_1 = 2.0*308.0;
      n = (Int)(floor(st_1));
      if (n < 0) n = 0;          /* impossible, but ... */
      if (n > 2*308) n = 2*308;  /* limit exponent change */
      while (n > 0) { n--; st_0 *= 2.0; };
   } 
   else 
   if (st_1 < 0.0) {
      if (st_1 < -2.0*308.0) st_1 = -2.0*308.0;
      n = ((Int)(floor(-st_1)));
      if (n < 0) n = 0;
      if (n > 2*308) n = 2*308;
      while (n > 0) { n--; st_0 *= 0.5; };
   }
   return st_0;
}

static
void fp_calc_fprem ( Int* qq, double* result, double st_0, double st_1 )
{
   double tmp = st_0 / st_1;
   if (tmp < 0)
      *qq = - (Int)floor(-tmp);
   else
      *qq = (Int)floor(tmp);
   *result = st_0 - (st_1 * (double)(*qq));
}

#if DEBUG
static 
void printFpuState ( void )
{
   Int i;
   assert(sizeof(Fpu_State)==108);
   for (i = 7; i >= 0; i--) {
      printf ( " %s fpreg%d: 0x", 
               (UInt)i == fp_get_tos() ? "**" : "  ", i );
      //for (j = FP_REG(i+1)-1; j >= FP_REG(i); j--)
      //   printf ( "%2x", (UInt)m_fpu_state.reg[j]);
      printf ( "  %5s  ", fp_tag_names[fp_get_tag(i)] );
      printf ( "%20.16e\n", fp_get_reg(i) );
   }
   printf("     fctrl:     0x%4x  masked: ", 
          (UInt)m_fpu_state.env[FP_ENV_CTRL] );
   for (i = FP_E_INVAL; i <= FP_E_LOS; i++)
      if (fp_get_controlword_flag(i))
         printf ( "%s ", fp_exception_names[i] );
   printf ( "\n" );

   printf("     fstat:     0x%4x  except:", 
          (UInt)m_fpu_state.env[FP_ENV_STAT] );
   for (i = FP_E_INVAL; i <= FP_E_LOS; i++)
      if (fp_get_statusword_flag(i))
         printf ( "%s ", fp_exception_names[i] );
   printf ( "  top: %d  ", fp_get_tos() );
   printf ( "c3210: %d%d%d%d",
            fp_get_statusword_flag(FP_F_C3),
            fp_get_statusword_flag(FP_F_C2),
            fp_get_statusword_flag(FP_F_C1),
            fp_get_statusword_flag(FP_F_C0) );
   printf ( "  STACKF: %d\n", fp_get_statusword_flag(FP_E_STACKF) );

   printf("      ftag:     0x%4x  ", (UInt)m_fpu_state.env[FP_ENV_TAG] );
   for (i = 7; i >= 0; i--)
      printf ( "%s ", fp_tag_names[fp_get_tag(i)] );
   printf("\n");

   printf("       fip: 0x%8x\n", 
           (((UInt)m_fpu_state.env[FP_ENV_IP+1]) << 16) |
            ((UInt)m_fpu_state.env[FP_ENV_IP]) );
   printf("       fcs:     0x%4x\n", 
           ((UInt)m_fpu_state.env[FP_ENV_CS]) );
   printf("    fopoff: 0x%8x\n", 
           (((UInt)m_fpu_state.env[FP_ENV_OPOFF+1]) << 16) |
            ((UInt)m_fpu_state.env[FP_ENV_OPOFF]) );
   printf("    fopsel:     0x%4x\n", 
           ((UInt)m_fpu_state.env[FP_ENV_OPSEL]) );
}
#endif

/* ---------------------------------------------------------------------
   Implementation of the floating point instruction set.
   ------------------------------------------------------------------ */

/* A pretty nasty kludge.  Arithmetic is done using standard IEEE 
   doubles, which means that programs which rely on the extra accuracy
   supplied by Intel's internal 80-bit format will get different
   results.

   To make exception handling tractable, we assume that the FPU is
   running with all exceptions masked, so we do the "default fixup"
   action for all exceptions.  Fortunately that's fairly simple.

   Support for non-normal numbers (infinities, nans, denorms, etc) is 
   minimal and probably wrong.
*/

typedef
   enum { Fp_Add, Fp_Sub, Fp_Mul, Fp_Div, Fp_SubR, Fp_DivR }
   Fp_Op;

#if DEBUG
char* fp_Op_name ( Fp_Op op )
{
   switch (op) {
      case Fp_Add:  return "add";   case Fp_Sub:  return "sub";
      case Fp_Mul:  return "mul";   case Fp_Div:  return "div";
      case Fp_SubR: return "subr";  case Fp_DivR: return "divr";
      default: panic("fp_Op_name");
   }
   return NULL; /*notreached*/
}
#endif

static
void fp_do_op_ST_ST ( UInt a_src, UInt a_dst, Fp_Op op, Bool pop )
{
   double vd_src, vd_dst;
   IFDB( if (dis) printf("\tf%s%s\t%%st(%d),%%st(%d)\n",
                         fp_Op_name(op), pop?"p":"",
                         a_src, a_dst ); )
   if (!fp_is_empty_tag(fp_get_tag_ST(a_src)) &&
       !fp_is_empty_tag(fp_get_tag_ST(a_dst))) {
      vd_dst = fp_get_reg_ST(a_dst);
      vd_src = fp_get_reg_ST(a_src);
      switch (op) {
         case Fp_Add:  vd_dst = vd_dst + vd_src; break;
         case Fp_Sub:  vd_dst = vd_dst - vd_src; break;
         case Fp_Mul:  vd_dst = vd_dst * vd_src; break;
         case Fp_Div:  vd_dst = vd_dst / vd_src; break;
         case Fp_SubR: vd_dst = vd_src - vd_dst; break;
         case Fp_DivR: vd_dst = vd_src / vd_dst; break;
         default: panic("fp_do_op_ST_ST");
      }      
   } else {
      vd_dst = NAN;
      fp_set_stack_underflow();
   }
   fp_set_reg_ST(a_dst,vd_dst);
   if (pop) (void)fp_pop();
}

static
void fp_do_COM_ST_ST ( UInt a_src, UInt a_dst, UInt nPops )
{
   double vd_src, vd_dst;
   IFDB( if (dis) printf("\tfcom%s\t%%st(%d),%%st(%d)\n",
                         nPops==0 ? "" : (nPops==1 ? "p" : "pp"),
                         a_src, a_dst ); )
   if (!fp_is_empty_tag(fp_get_tag_ST(a_src)) &&
       !fp_is_empty_tag(fp_get_tag_ST(a_dst))) {
      vd_dst = fp_get_reg_ST(a_dst);
      vd_src = fp_get_reg_ST(a_src);
      fp_set_statusword_flags_COM(vd_dst,vd_src);
   } else {
      fp_set_statusword_flags_COM_STACKF();
      fp_set_stack_underflow();
   }
   while (nPops > 0) { 
      (void)fp_pop();
      nPops--;
   }
}

static
void fp_do_op_mem_ST_0 ( UInt a_src,
                         IFDB(Text t_src CC)
                         Fp_Op op, Bool dbl )
{
   double vd_src, vd_dst;
   IFDB( if (dis) printf("\tf%s%c\t%s,%%st(0)\n",
                         fp_Op_name(op), dbl?'D':'F', t_src ); )
   if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
      vd_dst = fp_get_reg_ST(0);
      vd_src = dbl ? getDMem(a_src) : getFMem(a_src);
      switch (op) {
         case Fp_Add:  vd_dst = vd_dst + vd_src; break;
         case Fp_Sub:  vd_dst = vd_dst - vd_src; break;
         case Fp_Mul:  vd_dst = vd_dst * vd_src; break;
         case Fp_Div:  vd_dst = vd_dst / vd_src; break;
         case Fp_SubR: vd_dst = vd_src - vd_dst; break;
         case Fp_DivR: vd_dst = vd_src / vd_dst; break;
         default: panic("fp_do_op_mem_ST_0");
      }      
   } else {
      vd_dst = NAN;
      fp_set_stack_underflow();
   }
   fp_set_reg_ST(0,vd_dst);
}

static
void fp_do_COM_mem_ST_0 ( UInt a_src, 
                          IFDB( Text t_src CC)
                          Bool dbl, Bool pop )
{
   double vd_src, vd_dst;
   IFDB( if (dis) printf("\tfcom%s%c\t%s,%%st(0)\n",
                         pop?"p":"", dbl?'D':'F', t_src ); )
   if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
      vd_dst = fp_get_reg_ST(0);
      vd_src = dbl ? getDMem(a_src) : getFMem(a_src);
      fp_set_statusword_flags_COM(vd_dst,vd_src);
   } else {
      fp_set_statusword_flags_COM_STACKF();
      fp_set_stack_underflow();
   }
   if (pop) (void)fp_pop();
}


Addr do_one_insn_fp ( Addr r_eip, UChar first_opcode )
{
   UChar  modrm;
   UInt   a_addr, a_src, a_dst;
   UInt   opc_aux;
   Bool   isreg;
   Int    vis_addr;
   Int    vis_dst;
   double vd_addr, vd_src, vd_dst;

#  if DEBUG
   Text   t_opc_aux;
   Text   t_addr, t_dst;
   Bool ppFpuState = False;

   if (ppFpuState) {
      printf("\n\nBEFORE\n");
      printFpuState();
      printf("\n");
   }
#  endif

   /* assert that we are running with all exceptions masked */
   assert( (m_fpu_state.env[FP_ENV_CTRL] & 0x3F) == 0x3F );
   /* and the implication is that there are no unmasked exceptions
      reported by the exception status flag. */
   assert( fp_get_statusword_flag(FP_E_SUMMARY) == 0 );

   modrm = *r_eip;

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xD8 opcodes +-+-+-+-+-+-+-+ */

   if (first_opcode == 0xD8) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr 
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {

            case 0: /* FADD single-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                   Fp_Add, False );
               break;

            case 1: /* FMUL single-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                   Fp_Mul, False );
               break;

            case 2: /* FCOM single-real */
               fp_do_COM_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                    False, False );
               break;

            case 3: /* FCOMP single-real */
               fp_do_COM_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                    False, True );
               break;

            case 4: /* FSUB single-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                   Fp_Sub, False );
               break;

            case 5: /* FSUBR single-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                   Fp_SubR, False );
               break;

            case 6: /* FDIV single-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                   Fp_Div, False );
               break;

            case 7: /* FDIVR single-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) 
                                   Fp_DivR, False );
               break;

            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xD8");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FADD %st(?),%st(0) */
               fp_do_op_ST_ST ( modrm - 0xC0, 0, Fp_Add, False );
               break;

            case 0xC8 ... 0xCF: /* FMUL %st(?),%st(0) */
               fp_do_op_ST_ST ( modrm - 0xC8, 0, Fp_Mul, False );
               break;

            case 0xD0 ... 0xD7: /* FCOM %st(?),%st(0) */
               fp_do_COM_ST_ST ( modrm - 0xD0, 0, 0 );
               break;

            case 0xD8 ... 0xDF: /* FCOMP %st(?),%st(0) */
               fp_do_COM_ST_ST ( modrm - 0xD8, 0, 1 );
               break;

            case 0xE0 ... 0xE7: /* FSUB %st(?),%st(0) */
               fp_do_op_ST_ST ( modrm - 0xE0, 0, Fp_Sub, False );
               break;

            case 0xE8 ... 0xEF: /* FSUBR %st(?),%st(0) */
               fp_do_op_ST_ST ( modrm - 0xE8, 0, Fp_SubR, False );
               break;

            case 0xF0 ... 0xF7: /* FDIV %st(?),%st(0) */
               fp_do_op_ST_ST ( modrm - 0xF0, 0, Fp_Div, False );
               break;

            case 0xF8 ... 0xFF: /* FDIVR %st(?),%st(0) */
               fp_do_op_ST_ST ( modrm - 0xF8, 0, Fp_DivR, False );
               break;

            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xD9 opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xD9) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr 
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {

            case 0: /* FLD single-real */
               IFDB( if (dis) printf("\tfldF\t%s\n",t_addr); )
               vd_addr = getFMem(a_addr);
               fp_push(vd_addr);
               break;

            case 2: /* FST single-real */
               IFDB( if (dis) printf("\tfstF\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_get_reg_ST(0);
               } else {
                  vd_addr = NAN;
                  fp_set_stack_underflow();
               }
               setFMem(a_addr,vd_addr);
               break;

            case 3: /* FSTP single-real */
               IFDB( if (dis) printf("\tfstpF\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_pop();
               } else {
                  vd_addr = fp_pop(); /* then throw away result */
                  vd_addr = NAN;
                  fp_set_stack_underflow();
               }
               setFMem(a_addr,vd_addr);
               break;

            case 5: /* FLDCW */
               IFDB( if (dis) printf("\tfldcw\t%s\n",t_addr); )
               m_fpu_state.env[FP_ENV_CTRL] = (UShort)getIMem2(a_addr);
               break;

            case 7: /* FNSTCW */
               IFDB( if (dis) printf("\tfnstcw\t%s\n",t_addr); )
               setIMem2(a_addr,(UInt)m_fpu_state.env[FP_ENV_CTRL]);
               break;

            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xD9");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FLD %st(?) */
               a_dst = (UInt)modrm - 0xC0;
               IFDB( if (dis) printf("\tfld\t%%st(%d)\n",a_dst); )
               if (!fp_is_empty_tag(fp_get_tag_ST(a_dst)) &&
                   fp_is_empty_tag(fp_get_tag_ST(7))) {
                  vd_dst = fp_get_reg_ST(a_dst);
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_push(vd_dst);
               break;

            case 0xC8 ... 0xCF: /* FXCH %st(?) */
               a_dst = (UInt)modrm - 0xC8;
               IFDB( if (dis) printf("\tfxch\t%%st(%d)\n",a_dst); )
               if (!fp_is_empty_tag(fp_get_tag_ST(a_dst)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fp_get_reg_ST(a_dst);
                  vd_src = fp_get_reg_ST(0);
               } else {
                  vd_dst = NAN;
                  vd_src = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(a_dst,vd_src);
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xE0: /* FCHS */
               IFDB( if (dis) printf("\tfchs\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = - fp_get_reg_ST(0);
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xE1: /* FABS */
               IFDB( if (dis) printf("\tfabs\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fabs(fp_get_reg_ST(0));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xE5:
               /* An approximation to the correct behaviour */
               IFDB( if (dis) printf("\tfxam\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fabs(fp_get_reg_ST(0));
                  if (isnan(vd_dst)) 
                     vis_dst = 1; /* C320 = 001 */ 
                  else if (isinf(vd_dst)) 
                     vis_dst = 3; /* C320 = 011 */
                  else if (vd_dst == 0.0 || vd_dst == -0.0)  
                     vis_dst = 4; /* C320 = 100 */
                  else
                     vis_dst = 2; /* C320 = 010 */
                  fp_set_statusword_flag_to(FP_F_C1, 
                                            vd_dst < 0.0 ? 1 : 0);
               } else {
                  vis_dst = 5; /* C320 = 101 */
                  /* no idea if this is right */
                  fp_set_statusword_flag_to(FP_F_C1, 0);
               }
               fp_set_statusword_flag_to(FP_F_C3, (vis_dst >> 2) & 1);
               fp_set_statusword_flag_to(FP_F_C2, (vis_dst >> 1) & 1);
               fp_set_statusword_flag_to(FP_F_C0, vis_dst & 1);
               break;
               
            case 0xE8: /* FLD1 */
               IFDB( t_dst = "1";  )
               vd_dst = 1.0;
               goto do_fld_CONST;
            case 0xEC: /* FLDLG2 */
               IFDB( t_dst = "lg2";  )
               vd_dst = 0.301029995663981143;
               goto do_fld_CONST;
            case 0xED: /* FLDLN2 */
               IFDB( t_dst = "ln2";  )
               vd_dst = 0.69314718055994530942;
               goto do_fld_CONST;
            case 0xEE: /* FLDZ */
               IFDB( t_dst = "z";  )
               vd_dst = 0.0;
               goto do_fld_CONST;
            do_fld_CONST:
               IFDB( if (dis) printf("\tfld%s\n",t_dst); )
               fp_push(vd_dst);
               break;

            case 0xF0: /* F2XM1 */
               IFDB( if (dis) printf("\tf2xm1\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fp_calc_2xm1(fp_get_reg_ST(0));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xF1: /* FYL2X */
               IFDB( if (dis) printf("\tfyl2x\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(1))) {
                  vd_dst = fp_calc_yl2x(
                              fp_get_reg_ST(0), fp_get_reg_ST(1));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(1,vd_dst);
               (void)fp_pop();
               break;

            case 0xF3: /* FPATAN */
               IFDB( if (dis) printf("\tfpatan\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(1))) {
                  vd_dst = atan2(
                              fp_get_reg_ST(1), fp_get_reg_ST(0));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(1,vd_dst);
               (void)fp_pop();
               break;

            case 0xF8: { /* FPREM */
               /* Very incomplete implementation.  */
               Int qq;
               IFDB( if (dis) printf("\tfprem\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(1))) {
                  fp_calc_fprem( &qq, &vd_dst, 
                                 fp_get_reg_ST(0), fp_get_reg_ST(1) );
                  fp_set_statusword_flag_to(FP_F_C0, (qq & 4) ? 1 : 0);
                  fp_set_statusword_flag_to(FP_F_C1, (qq & 1) ? 1 : 0);
                  fp_set_statusword_flag_to(FP_F_C2, 0); /* reduction complete */
                  fp_set_statusword_flag_to(FP_F_C3, (qq & 2) ? 1 : 0);
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
                  fp_set_statusword_flag_to(FP_F_C1, 0); /* stack underflow */
               }
               fp_set_reg_ST(0,vd_dst);
               break;
            }
            case 0xF9: /* FYL2XP1 */
               IFDB( if (dis) printf("\tfyl2xp1\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(1))) {
                  vd_dst = fp_calc_yl2xp1(
                              fp_get_reg_ST(0), fp_get_reg_ST(1));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(1,vd_dst);
               (void)fp_pop();
               break;

            case 0xFA: /* FSQRT */
               IFDB( if (dis) printf("\tfsqrt\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = sqrt(fp_get_reg_ST(0));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xFC: /* FRNDINT */
               IFDB( if (dis) printf("\tfrndint\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = rint(fp_get_reg_ST(0));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xFD: /* FSCALE */
               IFDB( if (dis) printf("\tfscale\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(1))) {
                  vd_dst = fp_calc_scale(
                              fp_get_reg_ST(0), fp_get_reg_ST(1));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xFE: /* FSIN */
               IFDB( if (dis) printf("\tfsin\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = sin(fp_get_reg_ST(0));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            case 0xFF: /* FCOS */
               IFDB( if (dis) printf("\tfcos\n"); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = cos(fp_get_reg_ST(0));
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(0,vd_dst);
               break;

            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDA opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDA) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {

            case 0: /* FIADD m32int */
               IFDB( if (dis) printf("\tfiaddl\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_get_reg_ST(0) + (double)vis_addr;
                  fp_set_reg_ST(0, vd_addr);
                  /* we should set C1 here */
               } else {
                  fp_set_reg_ST(0, NAN);
                  fp_set_stack_underflow();
               }
               break;

            case 1: /* FIMUL m32int */
               IFDB( if (dis) printf("\tfimull\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_get_reg_ST(0) * (double)vis_addr;
                  fp_set_reg_ST(0, vd_addr);
                  /* we should set C1 here */
               } else {
                  fp_set_reg_ST(0, NAN);
                  fp_set_stack_underflow();
               }
               break;

            case 2: /* FICOM m32int */
               IFDB( if (dis) printf("\tficoml\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fp_get_reg_ST(0);
                  vd_src = (double)vis_addr;
                  fp_set_statusword_flags_COM(vd_dst,vd_src);
                  /* we should set C1 here */
               } else {
                  fp_set_statusword_flags_COM_STACKF();
                  fp_set_stack_underflow();
               }
               break;

            case 3: /* FICOMP m32int */
               IFDB( if (dis) printf("\tficompl\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fp_get_reg_ST(0);
                  vd_src = (double)vis_addr;
                  fp_set_statusword_flags_COM(vd_dst,vd_src);
                  /* we should set C1 here */
               } else {
                  fp_set_statusword_flags_COM_STACKF();
                  fp_set_stack_underflow();
               }
               (void)fp_pop();
               break;

            case 4: /* FISUB m32int */
               IFDB( if (dis) printf("\tfisubl\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_get_reg_ST(0) - (double)vis_addr;
                  fp_set_reg_ST(0, vd_addr);
                  /* we should set C1 here */
               } else {
                  fp_set_reg_ST(0, NAN);
                  fp_set_stack_underflow();
               }
               break;

            case 5: /* FISUBR m32int */
               IFDB( if (dis) printf("\tfisubrl\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = (double)vis_addr - fp_get_reg_ST(0);
                  fp_set_reg_ST(0, vd_addr);
                  /* we should set C1 here */
               } else {
                  fp_set_reg_ST(0, NAN);
                  fp_set_stack_underflow();
               }
               break;

            case 6: /* FIDIV m32int */
               IFDB( if (dis) printf("\tfidivl\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_get_reg_ST(0) / (double)vis_addr;
                  fp_set_reg_ST(0, vd_addr);
                  /* we should set C1 here */
               } else {
                  fp_set_reg_ST(0, NAN);
                  fp_set_stack_underflow();
               }
               break;

            case 7: /* FIDIVR m32int */
               IFDB( if (dis) printf("\tfidivl\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = (double)vis_addr / fp_get_reg_ST(0);
                  fp_set_reg_ST(0, vd_addr);
                  /* we should set C1 here */
               } else {
                  fp_set_reg_ST(0, NAN);
                  fp_set_stack_underflow();
               }
               break;

            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xDA");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {

            case 0xE9: /* FUCOMPP %st(0),%st(1) */
               /* seems the wrong way round. */
               fp_do_COM_ST_ST ( 1, 0, 2 );
               break;

            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDB opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDB) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr 
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {

            case 0: /* FILD m32int */
               IFDB( if (dis) printf("\tfildl\t%s\n",t_addr); )
               vis_addr = getIMem4(a_addr);
               fp_push ( (double)vis_addr );
               break;

            case 2: /* FIST m32 */
               IFDB( if (dis) printf("\tfistl\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_get_reg_ST(0);
                  if (vd_addr <= -2147483648.5 || 
                      vd_addr >= 2147483647.5) 
                     vis_addr = 0x80000000; /* 32-bit int indefinite */
                  else
                     vis_addr = (Int)vd_addr;
               } else {
                  vis_addr = 0x80000000; /* 32-bit indefinite */
                  fp_set_stack_underflow();
               }
               setIMem4(a_addr,vis_addr);
               break;

            case 3: /* FISTP m32 */
               IFDB( if (dis) printf("\tfistpl\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_pop();
                  if (vd_addr <= -2147483648.5 || 
                      vd_addr >= 2147483647.5) 
                     vis_addr = 0x80000000; /* 32-bit int indefinite */
                  else
                     vis_addr = (Int)vd_addr;
               } else {
                  vd_addr = fp_pop(); /* then throw away result */
                  vis_addr = 0x80000000; /* 32-bit indefinite */
                  fp_set_stack_underflow();
               }
               setIMem4(a_addr,vis_addr);
               break;

            case 5: /* FLD extended-real */
               IFDB( if (dis) printf("\tfldT\t%s\n",t_addr); )
               vd_addr = getTMem(a_addr);
               fp_push(vd_addr);
               break;

            case 7: /* FSTP extended-real */
               IFDB( if (dis) printf("\tfstpT\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_pop();
               } else {
                  vd_addr = fp_pop(); /* then throw away result */
                  vd_addr = NAN;
                  fp_set_stack_underflow();
               }
               setTMem(a_addr,vd_addr);
               break;

            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xDB");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {
            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDC opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDC) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr 
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {

            case 0: /* FADD double-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) Fp_Add, True );
               break;

            case 1: /* FMUL double-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) Fp_Mul, True );
               break;

            case 2: /* FCOM double-real */
               fp_do_COM_mem_ST_0 ( a_addr, IFDB(t_addr CC) True, False );
               break;

            case 3: /* FCOMP double-real */
               fp_do_COM_mem_ST_0 ( a_addr, IFDB(t_addr CC) True, True );
               break;

            case 4: /* FSUB double-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) Fp_Sub, True );
               break;

            case 5: /* FSUBR double-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) Fp_SubR, True );
               break;

            case 6: /* FDIV double-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) Fp_Div, True );
               break;

            case 7: /* FDIVR double-real */
               fp_do_op_mem_ST_0 ( a_addr, IFDB(t_addr CC) Fp_DivR, True );
               break;

            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xDC");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FADD %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xC0, Fp_Add, False );
               break;

            case 0xC8 ... 0xCF: /* FMUL %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xC8, Fp_Mul, False );
               break;

            case 0xE0 ... 0xE7: /* FSUBR %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xE0, Fp_SubR, False );
               break;

            case 0xE8 ... 0xEF: /* FSUB %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xE8, Fp_Sub, False );
               break;

            case 0xF8 ... 0xFF: /* FDIV %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xF8, Fp_Div, False );
               break;

            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDD opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDD) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr 
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {

            case 0: /* FLD double-real */
               IFDB( if (dis) printf("\tfldD\t%s\n",t_addr); )
               vd_addr = getDMem(a_addr);
               fp_push(vd_addr);
               break;

            case 2: /* FST double-real */
               IFDB( if (dis) printf("\tfstD\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_get_reg_ST(0);
               } else {
                  vd_addr = NAN;
                  fp_set_stack_underflow();
               }
               setDMem(a_addr,vd_addr);
               break;

            case 3: /* FSTP double-real */
               IFDB( if (dis) printf("\tfstpD\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_pop();
               } else {
                  vd_addr = fp_pop(); /* then throw away result */
                  vd_addr = NAN;
                  fp_set_stack_underflow();
               }
               setDMem(a_addr,vd_addr);
               break;
            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xDD");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FFREE %st(?) */
               a_dst = (UInt)modrm - 0xC0;
               IFDB( if (dis) printf("\tffree\t%%st(%d)\n", a_dst); )
               fp_set_tag_ST( a_dst, FP_TAG_EMPTY );
               break;

            case 0xD0 ... 0xD7: /* FST %st(0),%st(?) */
               a_dst = (UInt)modrm - 0xD0;
               IFDB( if (dis) printf("\tfst\t%%st(0),%%st(%d)\n",
                                     a_dst); )
               if ( /* don't check the destination tag */
                    !fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fp_get_reg_ST(0);
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(a_dst,vd_dst);
               break;

            case 0xD8 ... 0xDF: /* FSTP %st(0),%st(?) */
               a_dst = (UInt)modrm - 0xD8;
               IFDB( if (dis) printf("\tfstp\t%%st(0),%%st(%d)\n",
                                     a_dst); )
               if ( /* don't check the destination tag */
                    !fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_dst = fp_get_reg_ST(0);
               } else {
                  vd_dst = NAN;
                  fp_set_stack_underflow();
               }
               fp_set_reg_ST(a_dst,vd_dst);
               (void)fp_pop();
               break;

            case 0xE0 ... 0xE7: /* FUCOM %st(0),%st(?) */
               a_src = (UInt)modrm - 0xE0;
               IFDB( if (dis) printf("\tfucom\t%%st(0),%%st(%d)\n",
                                     a_src); )
               if (!fp_is_empty_tag(fp_get_tag_ST(a_src)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_src = fp_get_reg_ST(a_src);
                  vd_dst = fp_get_reg_ST(0);
                  fp_set_statusword_flags_COM(vd_dst,vd_src);
               } else {
                  fp_set_statusword_flags_COM_STACKF();
                  fp_set_stack_underflow();
               }
               break;

            case 0xE8 ... 0xEF: /* FUCOMP %st(0),%st(?) */
               a_src = (UInt)modrm - 0xE8;
               IFDB( if (dis) printf("\tfucomp\t%%st(0),%%st(%d)\n",
                                     a_src); )
               if (!fp_is_empty_tag(fp_get_tag_ST(a_src)) &&
                   !fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_src = fp_get_reg_ST(a_src);
                  vd_dst = fp_get_reg_ST(0);
                  fp_set_statusword_flags_COM(vd_dst,vd_src);
               } else {
                  fp_set_statusword_flags_COM_STACKF();
                  fp_set_stack_underflow();
               }
               (void)fp_pop();
               break;

            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDE opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDE) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr 
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {
            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xDE");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FADDP %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xC0, Fp_Add, True );
               break;

            case 0xC8 ... 0xCF: /* FMULP %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xC8, Fp_Mul, True );
               break;

            case 0xD9: /* FCOMPP %st(0),%st(1) */
               /* seems the wrong way round. */
               fp_do_COM_ST_ST ( 1, 0, 2 );
               break;

            case 0xE0 ... 0xE7: /* FSUBRP %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xE0, Fp_SubR, True );
               break;

            case 0xE8 ... 0xEF: /* FSUBP %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xE8, Fp_Sub, True );
               break;

            case 0xF0 ... 0xF7: /* FDIVRP %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xF0, Fp_DivR, True );
               break;

            case 0xF8 ... 0xFF: /* FDIVP %st(0),%st(?) */
               fp_do_op_ST_ST ( 0, modrm - 0xF8, Fp_Div, True );
               break;

            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDF opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDF) {
      if (modrm < 0xC0) {
	/* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         opc_aux = regno_from_modRM ( r_eip, 4 IFDB(CC &t_opc_aux) );
         r_eip = amode_from_modRM ( r_eip, 4, &a_addr 
                                    IFDB(CC &t_addr), &isreg );
         assert(!isreg);
         switch (opc_aux) {

            case 0: /* FILD m16int */
               IFDB( if (dis) printf("\tfildw\t%s\n",t_addr); )
               vis_addr = extend_s_16to32(getIMem2(a_addr));
               fp_push ( (double) vis_addr );
               break;

            case 3: /* FISTP m16 */
               IFDB( if (dis) printf("\tfistpw\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_pop();
                  if (vd_addr <= -32768.50 || 
                      vd_addr >= 32767.50) 
                     vis_addr = 0x00008000; /* 16-bit int indefinite */
                  else
                     vis_addr = (Short)vd_addr;
               } else {
                  vd_addr = fp_pop(); /* then throw away result */
                  vis_addr = 0x00008000; /* 32-bit indefinite */
                  fp_set_stack_underflow();
               }
               setIMem2(a_addr,vis_addr);
               break;

            case 5: { /* FILD m64int */
               ULong vis_addr64;
               IFDB( if (dis) printf("\tfildq\t%s\n",t_addr); )
               vis_addr   = getIMem4(a_addr+4);
               vis_addr64 = ((ULong)vis_addr) << 32;
               vis_addr   = getIMem4(a_addr);
               vis_addr64 += (ULong)vis_addr;
               fp_push ( (double) ((Long)vis_addr64) );
               break;
            }

            case 7: { /* FISTP m64int */
               ULong vis_addr64;
               IFDB( if (dis) printf("\tfistpq\t%s\n",t_addr); )
               if (!fp_is_empty_tag(fp_get_tag_ST(0))) {
                  vd_addr = fp_pop();
                  if (vd_addr <= -9223372036854775808.5 ||
                      vd_addr >= 9223372036854775807.5) 
                     vis_addr64 = 0x8000000000000000LL;
                         /* 64-bit int indefinite */
                  else
                     vis_addr64 = (Long)vd_addr;
               } else {
                  vd_addr = fp_pop(); /* then throw away result */
                  vis_addr64 = 0x8000000000000000LL; /* 64-bit indefinite */
                  fp_set_stack_underflow();
               }
               setIMem4(a_addr,vis_addr64 & 0xFFFFFFFFLL);
               setIMem4(a_addr+4, (((Long)vis_addr64) >> 32) 
                                   & 0xFFFFFFFFLL);
               break;
            }

            default:
               printf("unhandled opc_aux = 0x%2x\n", opc_aux);
               panic("do_one_insn_fp: first_opcode == 0xDF");
               break;
	 }
      } else {
         /* The entire modRM byte is an opcode extension. */
         r_eip++;
         switch (modrm) {

            case 0xE0: /* FNSTSW %ax */
               IFDB( if (dis) printf("\tfnstsw\t%%ax\n"); )
               setIReg2(R_EAX, (UInt)m_fpu_state.env[FP_ENV_STAT]);
               break;

            default:
               goto unhandled;
	 }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ Unhandled ESC opcode +-+-+-+ */
   else goto unhandled;

#  if DEBUG
   if (ppFpuState) {
      printf("\nAFTER\n");
      printFpuState();
      printf("\n");
   }
#  endif

   return r_eip;

  unhandled:
   hd_message(Hd_DebugMsg,
              "first opcode = 0x%x, modRM = 0x%x",
              (UInt)first_opcode, (UInt)modrm );
   panic("do_one_insn_fp: unhandled first_opcode/modrm combination");
   assert(0);
}

/*--------------------------------------------------------------------*/
/*--- end                                                 hd_fpu.c ---*/
/*--------------------------------------------------------------------*/
