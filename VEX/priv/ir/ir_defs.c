
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir_defs.c) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "vex_util.h"

/*---------------------------------------------------------------*/
/*--- Printing the IR                                         ---*/
/*---------------------------------------------------------------*/

void ppIRType ( IRType ty )
{
  switch (ty) {
    case Ity_Bit: vex_printf( "Bit"); break;
    case Ity_I8:  vex_printf( "I8");  break;
    case Ity_I16: vex_printf( "I16"); break;
    case Ity_I32: vex_printf( "I32"); break;
    case Ity_I64: vex_printf( "I64"); break;
    default: vpanic("ppIRType");
  }
}

void ppIRConst ( IRConst* con )
{
  switch (con->tag) {
    case Ico_U8:  vex_printf( "0x%x",   (UInt)(con->Ico.U8)); break;
    case Ico_U16: vex_printf( "0x%x",   (UInt)(con->Ico.U16)); break;
    case Ico_U32: vex_printf( "0x%x",   (UInt)(con->Ico.U32)); break;
    case Ico_U64: vex_printf( "0x%llx", (ULong)(con->Ico.U64)); break;
    default: vpanic("ppIRConst");
  }
}

void ppIRTemp ( IRTemp tmp )
{
   if (tmp == INVALID_IRTEMP)
      vex_printf("INVALID_IRTEMP");
   else
      vex_printf( "t%d", (Int)tmp);
}

void ppIROp ( IROp op )
{
   Char* str; 
   IROp  base;
   switch (op) {
      case Iop_Add8 ... Iop_Add64:
         str = "Add"; base = Iop_Add8; break;
      case Iop_Sub8 ... Iop_Sub64:
         str = "Sub"; base = Iop_Sub8; break;
      case Iop_Mul8 ... Iop_Mul64:
         str = "Mul"; base = Iop_Mul8; break;
      case Iop_Or8 ... Iop_Or64:
         str = "Or"; base = Iop_Or8; break;
      case Iop_And8 ... Iop_And64:
         str = "And"; base = Iop_And8; break;
      case Iop_Xor8 ... Iop_Xor64:
         str = "Xor"; base = Iop_Xor8; break;
      case Iop_Shl8 ... Iop_Shl64:
         str = "Shl"; base = Iop_Shl8; break;
      case Iop_Shr8 ... Iop_Shr64:
         str = "Shr"; base = Iop_Shr8; break;
      case Iop_Sar8 ... Iop_Sar64:
         str = "Sar"; base = Iop_Sar8; break;
      case Iop_CmpEQ8 ... Iop_CmpEQ64:
         str = "CmpEQ"; base = Iop_CmpEQ8; break;
      case Iop_CmpNE8 ... Iop_CmpNE64:
         str = "CmpNE"; base = Iop_CmpNE8; break;
      case Iop_Neg8 ... Iop_Neg64:
         str = "Neg"; base = Iop_Neg8; break;
      case Iop_Not8 ... Iop_Not64:
         str = "Not"; base = Iop_Not8; break;
      /* other cases must explicitly "return;" */
      case Iop_8Uto16:   vex_printf("8Uto16");  return;
      case Iop_8Uto32:   vex_printf("8Uto32");  return;
      case Iop_16Uto32:  vex_printf("16Uto32"); return;
      case Iop_8Sto16:   vex_printf("8Sto16");  return;
      case Iop_8Sto32:   vex_printf("8Sto32");  return;
      case Iop_16Sto32:  vex_printf("16Sto32"); return;
      case Iop_32to1:    vex_printf("32to1");   return;
      case Iop_1Uto8:    vex_printf("1Uto8");   return;

      case Iop_MullS8:   vex_printf("MullS8");  return;
      case Iop_MullS16:  vex_printf("MullS16"); return;
      case Iop_MullS32:  vex_printf("MullS32"); return;
      case Iop_MullU8:   vex_printf("MullU8");  return;
      case Iop_MullU16:  vex_printf("MullU16"); return;
      case Iop_MullU32:  vex_printf("MullU32"); return;

      case Iop_DivModU64to32: vex_printf("DivModU64to32"); return;
      case Iop_DivModS64to32: vex_printf("DivModS64to32"); return;

      case Iop_64HIto32: vex_printf("64HIto32"); return;
      case Iop_64LOto32: vex_printf("64LOto32"); return;
      case Iop_32HLto64: vex_printf("32HLto64"); return;

      default:           vpanic("ppIROp(1)");
   }
  
   switch (op - base) {
      case 0: vex_printf(str); vex_printf("8"); break;
      case 1: vex_printf(str); vex_printf("16"); break;
      case 2: vex_printf(str); vex_printf("32"); break;
      case 3: vex_printf(str); vex_printf("64"); break;
      default: vpanic("ppIROp(2)");
   }
}

void ppIRExpr ( IRExpr* e )
{
  Int i;
  switch (e->tag) {
    case Iex_Get:
      vex_printf( "GET(%d,", e->Iex.Get.offset);
      ppIRType(e->Iex.Get.ty);
      vex_printf(")");
      break;
    case Iex_Tmp:
      ppIRTemp(e->Iex.Tmp.tmp);
      break;
    case Iex_Binop:
      ppIROp(e->Iex.Binop.op);
      vex_printf( "(" );
      ppIRExpr(e->Iex.Binop.arg1);
      vex_printf( "," );
      ppIRExpr(e->Iex.Binop.arg2);
      vex_printf( ")" );
      break;
    case Iex_Unop:
      ppIROp(e->Iex.Unop.op);
      vex_printf( "(" );
      ppIRExpr(e->Iex.Unop.arg);
      vex_printf( ")" );
      break;
    case Iex_LDle:
      vex_printf( "LDle:" );
      ppIRType(e->Iex.LDle.ty);
      vex_printf( "(" );
      ppIRExpr(e->Iex.LDle.addr);
      vex_printf( ")" );
      break;
    case Iex_Const:
      ppIRConst(e->Iex.Const.con);
      break;
    case Iex_CCall:
      vex_printf("%s(", e->Iex.CCall.name);
      for (i = 0; e->Iex.CCall.args[i] != NULL; i++) {
        ppIRExpr(e->Iex.CCall.args[i]);
        if (e->Iex.CCall.args[i+1] != NULL)
          vex_printf(",");
      }
      vex_printf("):");
      ppIRType(e->Iex.CCall.retty);
      break;
    default:
      vpanic("ppIExpr");
  }
}

void ppIRStmt ( IRStmt* s )
{
  switch (s->tag) {
    case Ist_Put:
      if (s->Ist.Put.guard) {
         vex_printf("if (");
         ppIRExpr(s->Ist.Put.guard);
         vex_printf( ") PUT(%d) = ", s->Ist.Put.offset);
         ppIRExpr(s->Ist.Put.expr);
      } else {
         vex_printf( "PUT(%d) = ", s->Ist.Put.offset);
         ppIRExpr(s->Ist.Put.expr);
      }
      break;
    case Ist_Tmp:
      ppIRTemp(s->Ist.Tmp.tmp);
      vex_printf( " = " );
      ppIRExpr(s->Ist.Tmp.expr);
      break;
    case Ist_STle:
      vex_printf( "STle(");
      ppIRExpr(s->Ist.STle.addr);
      vex_printf( ") = ");
      ppIRExpr(s->Ist.STle.data);
      break;
    case Ist_Exit:
      vex_printf( "if (" );
      ppIRExpr(s->Ist.Exit.cond);
      vex_printf( ") goto ");
      ppIRConst(s->Ist.Exit.dst);
      break;
    default: 
      vpanic("ppIRStmt");
  }
}

void ppIRJumpKind ( IRJumpKind kind )
{
   switch (kind) {
      case Ijk_Boring:    vex_printf("Boring"); break;
      case Ijk_Call:      vex_printf("Call"); break;
      case Ijk_Ret:       vex_printf("Return"); break;
      case Ijk_ClientReq: vex_printf("ClientReq"); break;
      case Ijk_Syscall:   vex_printf("Syscall"); break;
      case Ijk_Yield:     vex_printf("Yield"); break;
      default:            vpanic("ppIRJumpKind");
  }
}

void ppIRTypeEnv ( IRTypeEnv* env ) {
   UInt i;
   for (i = 0; i < env->types_used; i++) {
      if (i % 8 == 0)
         vex_printf( "   ");
      ppIRTemp(i);
      vex_printf( ":");
      ppIRType(env->types[i]);
      if (i % 8 == 7) 
         vex_printf( "\n"); 
      else 
         vex_printf( "   ");
   }
   if (env->types_used > 0 && env->types_used % 8 != 7) 
      vex_printf( "\n"); 
}


void ppIRBB ( IRBB* bb )
{
   IRStmt* s;
   vex_printf("IRBB {\n");
   ppIRTypeEnv(bb->tyenv);
   vex_printf("\n");
   for (s = bb->stmts; s; s = s->link) {
      vex_printf( "   ");
      ppIRStmt(s);
      vex_printf( "\n");
   }
   vex_printf( "   goto {");
   ppIRJumpKind(bb->jumpkind);
   vex_printf( "} ");
   ppIRExpr( bb->next );
   vex_printf( "\n}\n");
}


/*---------------------------------------------------------------*/
/*--- Constructors                                            ---*/
/*---------------------------------------------------------------*/


/* Constructors -- IRConst */

IRConst* IRConst_U8 ( UChar u8 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U8;
   c->Ico.U8  = u8;
   return c;
}
IRConst* IRConst_U16 ( UShort u16 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U16;
   c->Ico.U16 = u16;
   return c;
}
IRConst* IRConst_U32 ( UInt u32 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U32;
   c->Ico.U32 = u32;
   return c;
}
IRConst* IRConst_U64 ( ULong u64 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U64;
   c->Ico.U64 = u64;
   return c;
}


/* Constructors -- IRExpr */

IRExpr* IRExpr_Get ( Int off, IRType ty ) {
   IRExpr* e         = LibVEX_Alloc(sizeof(IRExpr));
   e->tag            = Iex_Get;
   e->Iex.Get.offset = off;
   e->Iex.Get.ty     = ty;
   return e;
}
IRExpr* IRExpr_Tmp ( IRTemp tmp ) {
   IRExpr* e      = LibVEX_Alloc(sizeof(IRExpr));
   e->tag         = Iex_Tmp;
   e->Iex.Tmp.tmp = tmp;
   return e;
}
IRExpr* IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr* e         = LibVEX_Alloc(sizeof(IRExpr));
   e->tag            = Iex_Binop;
   e->Iex.Binop.op   = op;
   e->Iex.Binop.arg1 = arg1;
   e->Iex.Binop.arg2 = arg2;
   return e;
}
IRExpr* IRExpr_Unop ( IROp op, IRExpr* arg ) {
   IRExpr* e       = LibVEX_Alloc(sizeof(IRExpr));
   e->tag          = Iex_Unop;
   e->Iex.Unop.op  = op;
   e->Iex.Unop.arg = arg;
   return e;
}
IRExpr* IRExpr_LDle  ( IRType ty, IRExpr* addr ) {
   IRExpr* e        = LibVEX_Alloc(sizeof(IRExpr));
   e->tag           = Iex_LDle;
   e->Iex.LDle.ty   = ty;
   e->Iex.LDle.addr = addr;
   return e;
}
IRExpr* IRExpr_Const ( IRConst* con ) {
   IRExpr* e        = LibVEX_Alloc(sizeof(IRExpr));
   e->tag           = Iex_Const;
   e->Iex.Const.con = con;
   return e;
}
IRExpr* IRExpr_CCall ( Char* name, IRType retty, IRExpr** args ) {
   IRExpr* e          = LibVEX_Alloc(sizeof(IRExpr));
   e->tag             = Iex_CCall;
   e->Iex.CCall.name  = name;
   e->Iex.CCall.retty = retty;
   e->Iex.CCall.args  = args;
   return e;
}


/* Constructors -- IRStmt */

IRStmt* IRStmt_Put ( IRExpr* guard, Int off, IRExpr* value ) {
   IRStmt* s         = LibVEX_Alloc(sizeof(IRStmt));
   s->tag            = Ist_Put;
   s->link           = NULL;
   s->Ist.Put.guard  = guard;
   s->Ist.Put.offset = off;
   s->Ist.Put.expr   = value;
   return s;
}
IRStmt* IRStmt_Tmp ( IRTemp tmp, IRExpr* expr ) {
   IRStmt* s       = LibVEX_Alloc(sizeof(IRStmt));
   s->tag          = Ist_Tmp;
   s->link         = NULL;
   s->Ist.Tmp.tmp  = tmp;
   s->Ist.Tmp.expr = expr;
   return s;
}
IRStmt* IRStmt_STle ( IRExpr* addr, IRExpr* value ) {
   IRStmt* s        = LibVEX_Alloc(sizeof(IRStmt));
   s->tag           = Ist_STle;
   s->link          = NULL;
   s->Ist.STle.addr = addr;
   s->Ist.STle.data = value;
   return s;
}
IRStmt* IRStmt_Exit ( IRExpr* cond, IRConst* dst ) {
   IRStmt* s        = LibVEX_Alloc(sizeof(IRStmt));
   s->tag           = Ist_Exit;
   s->link          = NULL;
   s->Ist.Exit.cond = cond;
   s->Ist.Exit.dst  = dst;
   return s;
}

/* Constructors -- IRBB */

IRBB* mkIRBB ( IRTypeEnv* env, IRStmt* stmts, 
               IRExpr* next, IRJumpKind jumpkind ) {
   IRBB* bb     = LibVEX_Alloc(sizeof(IRBB));
   bb->tyenv    = env;
   bb->stmts    = stmts;
   bb->next     = next;
   bb->jumpkind = jumpkind;
   return bb;
}


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR                             ---*/
/*---------------------------------------------------------------*/

IRTypeEnv* newIRTypeEnv ( void )
{
   IRTypeEnv* env   = LibVEX_Alloc(sizeof(IRTypeEnv));
   env->types       = LibVEX_Alloc(8 * sizeof(IRType));
   env->types_size  = 8;
   env->types_used  = 0;
   return env;
}


IRTemp newIRTemp ( IRTypeEnv* env, IRType ty )
{
   vassert(env);
   vassert(env->types_used >= 0);
   vassert(env->types_size >= 0);
   vassert(env->types_used <= env->types_size);
   if (env->types_used < env->types_size) {
      env->types[env->types_used] = ty;
      return env->types_used++;
   } else {
      Int i;
      Int new_size = env->types_size==0 ? 8 : 2*env->types_size;
      IRType* new_types 
         = LibVEX_Alloc(new_size * sizeof(IRType));
      for (i = 0; i < env->types_used; i++)
         new_types[i] = env->types[i];
      env->types      = new_types;
      env->types_size = new_size;
      return newIRTemp(env, ty);
   }
}


IRType lookupIRTypeEnv ( IRTypeEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->types_used);
   return env->types[tmp];
}


IRType typeOfIRExpr ( IRTypeEnv* tyenv, IRExpr* e )
{
   switch (e->tag) {
      case Iex_LDle:
         return e->Iex.LDle.ty;
      case Iex_Get:
         return e->Iex.Get.ty;
      case Iex_Tmp:
         return lookupIRTypeEnv(tyenv, e->Iex.Tmp.tmp);
      case Iex_Const:
         switch (e->Iex.Const.con->tag) {
            case Ico_U8:  return Ity_I8;
            case Ico_U16: return Ity_I16;
            case Ico_U32: return Ity_I32;
            case Ico_U64: return Ity_I64;
            default: vpanic("typeOfIRExpr:Iex_Const");
         }
         break;
      case Iex_Binop:
         switch (e->Iex.Binop.op) {
            case Iop_Add32: case Iop_Sub32: case Iop_Mul32: 
            case Iop_Or32:  case Iop_And32: case Iop_Xor32:
            case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
               return Ity_I32;
            default: break;
         }
         break;
      default:
         break;
   }
   ppIRExpr(e);
   vpanic("typeOfIRExpr");
}

/*---------------------------------------------------------------*/
/*--- Sanity checking                                         ---*/
/*---------------------------------------------------------------*/

/* Checks:

   Everything is type-consistent.  No ill-typed anything.
   The target address at the end of the BB is a 32- or 64-
   bit expression, depending on the guest's word size.

   Each temp is assigned only once, before its uses.
 */

static
__attribute((noreturn))
void sanityCheckFail ( IRBB* bb, IRStmt* stmt, Char* what )
{
   vex_printf("\nIR SANITY CHECK FAILURE\n\n");
   ppIRBB(bb);
   if (stmt) {
      vex_printf("\nIN STATEMENT:\n\n");
      ppIRStmt(stmt);
   }
   vex_printf("\n\nERROR = %s\n\n", what );
   vpanic("sanityCheckFail: exiting due to bad IR");
}


/* Traverse a Stmt/Expr, inspecting IRTemp uses.  Report any out of
   range ones.  Report any which are read and for which the current
   def_count is zero. */

static
void useBeforeDef_Expr ( IRBB* bb, IRStmt* stmt, IRExpr* expr, Int* def_counts )
{
   Int i;
   switch (expr->tag) {
      case Iex_Get: 
         break;
      case Iex_Tmp: 
         if (expr->Iex.Tmp.tmp < 0 || expr->Iex.Tmp.tmp >= bb->tyenv->types_used)
            sanityCheckFail(bb,stmt, "out of range Temp in IRExpr");
         if (def_counts[expr->Iex.Tmp.tmp] < 1)
            sanityCheckFail(bb,stmt, "IRTemp def before use in IRExpr");
         break;
      case Iex_Binop:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Binop.arg1,def_counts);
         useBeforeDef_Expr(bb,stmt,expr->Iex.Binop.arg2,def_counts);
         break;
      case Iex_Unop:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Unop.arg,def_counts);
         break;
      case Iex_LDle:
         useBeforeDef_Expr(bb,stmt,expr->Iex.LDle.addr,def_counts);
         break;
      case Iex_Const:
         break;
      case Iex_CCall:
         for (i = 0; expr->Iex.CCall.args[i]; i++)
            useBeforeDef_Expr(bb,stmt,expr->Iex.CCall.args[i],def_counts);
         break;
   }
}

static
void useBeforeDef_Stmt ( IRBB* bb, IRStmt* stmt, Int* def_counts )
{
   switch (stmt->tag) {
      case Ist_Put:
         if (stmt->Ist.Put.guard)
            useBeforeDef_Expr(bb,stmt,stmt->Ist.Put.guard,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Put.expr,def_counts);
         break;
      case Ist_Tmp:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Tmp.expr,def_counts);
         break;
      case Ist_STle:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.STle.addr,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.STle.data,def_counts);
         break;
      case Ist_Exit:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Exit.cond,def_counts);
         break;
      default: 
         vpanic("useBeforeDef_Stmt");
   }
}


void sanityCheckIRBB ( IRBB* bb, IRType guest_word_size )
{
   Int     i;
   IRStmt* stmt;
   Int     n_temps    = bb->tyenv->types_used;
   Int*    def_counts = LibVEX_Alloc(n_temps * sizeof(Int));

   vassert(guest_word_size == Ity_I32
	   || guest_word_size == Ity_I64);

   for (i = 0; i < n_temps; i++)
      def_counts[i] = 0;

   /* Count the defs of each temp.  Only one def is allowed.
      Also, check that each used temp has already been defd. */
   for (stmt = bb->stmts; stmt; stmt=stmt->link) {
      useBeforeDef_Stmt(bb,stmt,def_counts);
      if (stmt->tag == Ist_Tmp) {
         if (stmt->Ist.Tmp.tmp < 0 || stmt->Ist.Tmp.tmp >= n_temps)
            sanityCheckFail(bb, stmt, "Invalid temp in Tmp assignment");
         def_counts[stmt->Ist.Tmp.tmp]++;
         if (def_counts[stmt->Ist.Tmp.tmp] > 1)
            sanityCheckFail(bb, stmt, "Tmp assigned more than once");
      }
   }

   for (i = 0; i < n_temps; i++)
     vex_printf("%d ", def_counts[i]);
   vex_printf("\n");

}

/*---------------------------------------------------------------*/
/*--- end                                           ir_defs.c ---*/
/*---------------------------------------------------------------*/
