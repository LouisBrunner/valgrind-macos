
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (x86h_defs.h) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBVEX_X86H_DEFS_H
#define __LIBVEX_X86H_DEFS_H


/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 8 real int regs,
   6 real float regs, and 0 real vector regs. 
*/

extern void ppHRegX86 ( HReg );

extern HReg hregX86_EAX ( void );
extern HReg hregX86_EBX ( void );
extern HReg hregX86_ECX ( void );
extern HReg hregX86_EDX ( void );
extern HReg hregX86_ESP ( void );
extern HReg hregX86_EBP ( void );
extern HReg hregX86_ESI ( void );
extern HReg hregX86_EDI ( void );


/* --------- Condition codes, Intel encoding. --------- */

typedef
   enum {
      Xcc_O      = 0,  /* overflow           */
      Xcc_NO     = 1,  /* no overflow        */

      Xcc_B      = 2,  /* below              */
      Xcc_NB     = 3,  /* not below          */

      Xcc_Z      = 4,  /* zero               */
      Xcc_NZ     = 5,  /* not zero           */

      Xcc_BE     = 6,  /* below or equal     */
      Xcc_NBE    = 7,  /* not below or equal */

      Xcc_S      = 8,  /* negative           */
      Xcc_NS     = 9,  /* not negative       */

      Xcc_P      = 10, /* parity even        */
      Xcc_NP     = 11, /* not parity even    */

      Xcc_L      = 12, /* jump less          */
      Xcc_NL     = 13, /* not less           */

      Xcc_LE     = 14, /* less or equal      */
      Xcc_NLE    = 15, /* not less or equal  */

      Xcc_ALWAYS = 16  /* the usual hack     */
   }
   X86CondCode;

extern Char* showX86CondCode ( X86CondCode );


/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
     Xam_IR,        /* Immediate + Reg */
     Xam_IRRS       /* Immediate + Reg1 + (Reg2 << Shift) */
   }
   X86AModeTag;

typedef
   struct {
      X86AModeTag tag;
      union {
         struct {
            UInt imm;
            HReg reg;
         } IR;
         struct {
            UInt imm;
            HReg base;
            HReg index;
            Int  shift; /* 0, 1, 2 or 3 only */
         } IRRS;
      } Xam;
   }
   X86AMode;

extern X86AMode* X86AMode_IR   ( UInt, HReg );
extern X86AMode* X86AMode_IRRS ( UInt, HReg, HReg, Int );

extern void ppX86AMode ( X86AMode* );


/* --------- Operand, which can be reg, immediate or memory. --------- */

typedef 
   enum {
      Xrmi_Imm,
      Xrmi_Reg,
      Xrmi_Mem
   }
   X86RMITag;

typedef
   struct {
      X86RMITag tag;
      union {
         struct {
            UInt imm32;
         } Imm;
         struct {
            HReg reg;
         } Reg;
         struct {
            X86AMode* am;
         } Mem;
      }
      Xrmi;
   }
   X86RMI;

extern X86RMI* X86RMI_Imm ( UInt );
extern X86RMI* X86RMI_Reg ( HReg );
extern X86RMI* X86RMI_Mem ( X86AMode* );

extern void ppX86RMI ( X86RMI* );


/* --------- Operand, which can be reg or immediate only. --------- */

typedef 
   enum {
      Xri_Imm,
      Xri_Reg
   }
   X86RITag;

typedef
   struct {
      X86RITag tag;
      union {
         struct {
            UInt imm32;
         } Imm;
         struct {
            HReg reg;
         } Reg;
      }
      Xri;
   }
   X86RI;

extern X86RI* X86RI_Imm ( UInt );
extern X86RI* X86RI_Reg ( HReg );

extern void ppX86RI ( X86RI* );


/* --------- Operand, which can be reg or memory only. --------- */

typedef 
   enum {
      Xrm_Reg,
      Xrm_Mem
   }
   X86RMTag;

typedef
   struct {
      X86RMTag tag;
      union {
         struct {
            HReg reg;
         } Reg;
         struct {
            X86AMode* am;
         } Mem;
      }
      Xrm;
   }
   X86RM;

extern X86RM* X86RM_Reg ( HReg );
extern X86RM* X86RM_Mem ( X86AMode* );

extern void ppX86RM ( X86RM* );


/* --------- Instructions. --------- */

/* --------- */
typedef
   enum {
      Xss_16,
      Xss_32
   }
   X86ScalarSz;

extern Char* showX86ScalarSz ( X86ScalarSz );


/* --------- */
typedef
   enum {
      Xun_Neg,
      Xun_Not
   }
   X86UnaryOp;

extern Char* showX86UnaryOp ( X86UnaryOp );


/* --------- */
typedef 
   enum {
      Xalu_INVALID,
      Xalu_MOV,
      Xalu_CMP,
      Xalu_ADD, Xalu_SUB, Xalu_ADC, Xalu_SBB, 
      Xalu_AND, Xalu_OR, Xalu_XOR,
      Xalu_MUL
   }
   X86AluOp;

extern Char* showX86AluOp ( X86AluOp );


/* --------- */
typedef
   enum {
      Xsh_INVALID,
      Xsh_SHL, Xsh_SHR, Xsh_SAR, 
      Xsh_ROL, Xsh_ROR
   }
   X86ShiftOp;

extern Char* showX86ShiftOp ( X86ShiftOp );


/* --------- */
typedef
   enum {
      Xfp_INVALID,
      /* Binary */
      Xfp_ADD, Xfp_SUB, Xfp_MUL, Xfp_DIV, 
      /* Unary */
      Xfp_SQRT, Xfp_NEGATE, Xfp_MOV
   }
   X86FpOp;

extern Char* showX86FpOp ( X86FpOp );


/* --------- */
typedef
   enum {
      Xin_Alu32R,    /* 32-bit mov/arith/logical, dst=REG */
      Xin_Alu32M,    /* 32-bit mov/arith/logical, dst=MEM */
      Xin_Sh32,      /* 32-bit shift/rotate, dst=REG or MEM */
      Xin_Test32,    /* 32-bit test (AND, set flags, discard result) */
      Xin_Unary32,   /* 32-bit not and neg */
      Xin_MulL,      /* widening multiply */
      Xin_Div,       /* div and mod */
      Xin_Sh3232,    /* shldl or shrdl */
      Xin_Push,      /* push (32-bit?) value on stack */
      Xin_Call,      /* call to address in register */
      Xin_Goto,      /* conditional/unconditional jmp to dst */
      Xin_CMov32,    /* conditional move */
      Xin_LoadEX,    /* mov{s,z}{b,w}l from mem to reg */
      Xin_Store,     /* store 16/8 bit value in memory */
      Xin_Set32,     /* convert condition code to 32-bit value */
      Xin_FpUnary,   /* FP fake unary op */
      Xin_FpBinary,  /* FP fake binary op */
      Xin_FpLdSt,    /* FP fake load/store */
      Xin_FpI64,     /* FP fake to/from 64-bit signed int */
      Xin_FpCMov     /* FP fake floating point (un)conditional move */
   }
   X86InstrTag;

/* Destinations are on the RIGHT (second operand) */

typedef
   struct {
      X86InstrTag tag;
      union {
         struct {
            X86AluOp op;
            X86RMI*  src;
            HReg     dst;
         } Alu32R;
         struct {
            X86AluOp  op;
            X86RI*    src;
            X86AMode* dst;
         } Alu32M;
         struct {
            X86ShiftOp op;
            UInt       src;  /* shift amount, or 0 means %cl */
            X86RM*     dst;
         } Sh32;
         struct {
            X86RI* src;
            X86RM* dst;
         } Test32;
         /* Not and Neg */
         struct {
            X86UnaryOp op;
            X86RM*     dst;
         } Unary32;
         /* DX:AX = AX *s/u r/m16,  or EDX:EAX = EAX *s/u r/m32 */
         struct {
            Bool        syned;
            X86ScalarSz ssz;
            X86RM*      src;
         } MulL;
         /* x86 div/idiv instruction.  Modifies EDX and EAX and reads src. */
         struct {
            Bool        syned;
            X86ScalarSz ssz;
            X86RM*      src;
         } Div;
         /* shld/shrd.  op may only be Xsh_SHL or Xsh_SHR */
         struct {
            X86ShiftOp op;
            UInt       amt;   /* shift amount, or 0 means %cl */
            HReg       src;
            HReg       dst;
         } Sh3232;
         struct {
            X86RMI* src;
         } Push;
         struct {
            HReg target;
         } Call;
         /* Pseudo-insn.  Goto dst, on given condition (which could be
            Xcc_ALWAYS).  Note importantly that if the jump is 
            conditional (not Xcc_ALWAYS) the jump kind *must* be
            Ijk_Boring.  Ie non-Boring conditional jumps are
            not allowed. */
         struct {
            IRJumpKind  jk;
            X86CondCode cond;
            X86RI*      dst;
         } Goto;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            X86CondCode cond;
            X86RM*      src;
            HReg        dst;
         } CMov32;
         /* Sign/Zero extending loads.  Dst size is always 32 bits. */
         struct {
            UChar     szSmall;
            Bool      syned;
            X86AMode* src;
            HReg      dst;
         } LoadEX;
         /* 16/8 bit stores, which are troublesome (particularly
            8-bit) */
         struct {
            UChar     sz; /* only 1 or 2 */
            HReg      src;
            X86AMode* dst;
         } Store;
         /* Convert a x86 condition code to a 32-bit value (0 or 1). */
         struct {
            X86CondCode cond;
            HReg        dst;
         } Set32;
         /* X86 Floating point (fake 3-operand, "flat reg file" insns) */
         struct {
            X86FpOp op;
            HReg    src;
            HReg    dst;
         } FpUnary;
         struct {
            X86FpOp op;
            HReg    srcL;
            HReg    srcR;
            HReg    dst;
         } FpBinary;
         struct {
            Bool      isLoad;
            UChar     sz; /* only 4 (IEEE single) or 8 (IEEE double) */
            HReg      reg;
            X86AMode* addr;
         } FpLdSt;
         struct {
            Bool toInt; /* True: F64->I64; False: I64->64 */
            HReg freg;
            HReg iregHi;
            HReg iregLo;
         } FpI64;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            X86CondCode cond;
            HReg        src;
            HReg        dst;
         } FpCMov;
      } Xin;
   }
   X86Instr;

extern X86Instr* X86Instr_Alu32R   ( X86AluOp, X86RMI*, HReg );
extern X86Instr* X86Instr_Alu32M   ( X86AluOp, X86RI*,  X86AMode* );
extern X86Instr* X86Instr_Unary32  ( X86UnaryOp op, X86RM* dst );
extern X86Instr* X86Instr_Sh32     ( X86ShiftOp, UInt, X86RM* );
extern X86Instr* X86Instr_Test32   ( X86RI* src, X86RM* dst );
extern X86Instr* X86Instr_MulL     ( Bool syned, X86ScalarSz, X86RM* );
extern X86Instr* X86Instr_Div      ( Bool syned, X86ScalarSz, X86RM* );
extern X86Instr* X86Instr_Sh3232   ( X86ShiftOp, UInt amt, HReg src, HReg dst );
extern X86Instr* X86Instr_Push     ( X86RMI* );
extern X86Instr* X86Instr_Call     ( HReg );
extern X86Instr* X86Instr_Goto     ( IRJumpKind, X86CondCode cond, X86RI* dst );
extern X86Instr* X86Instr_CMov32   ( X86CondCode, X86RM* src, HReg dst );
extern X86Instr* X86Instr_LoadEX   ( UChar szSmall, Bool syned,
                                     X86AMode* src, HReg dst );
extern X86Instr* X86Instr_Store    ( UChar sz, HReg src, X86AMode* dst );
extern X86Instr* X86Instr_Set32    ( X86CondCode cond, HReg dst );
extern X86Instr* X86Instr_FpUnary  ( X86FpOp op, HReg src, HReg dst );
extern X86Instr* X86Instr_FpBinary ( X86FpOp op, HReg srcL, HReg srcR, HReg dst );
extern X86Instr* X86Instr_FpLdSt   ( Bool isLoad, UChar sz, HReg reg, X86AMode* );
extern X86Instr* X86Instr_FpI64    ( Bool toInt, HReg freg, 
                                     HReg iregHi, HReg iregLo );
extern X86Instr* X86Instr_FpCMov   ( X86CondCode, HReg src, HReg dst );

extern void ppX86Instr ( X86Instr* );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void         getRegUsage_X86Instr ( HRegUsage*, X86Instr* );
extern void         mapRegs_X86Instr     ( HRegRemap*, X86Instr* );
extern Bool         isMove_X86Instr      ( X86Instr*, HReg*, HReg* );
extern Int          emit_X86Instr        ( UChar* buf, Int nbuf, X86Instr* );
extern X86Instr*    genSpill_X86         ( HReg rreg, Int offset );
extern X86Instr*    genReload_X86        ( HReg rreg, Int offset );
extern void         getAllocableRegs_X86 ( Int*, HReg** );
extern HInstrArray* iselBB_X86           ( IRBB*, Addr64(*)(Char*) );

#endif /* ndef __LIBVEX_X86H_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                         x86h_defs.h ---*/
/*---------------------------------------------------------------*/
