
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (test_main.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "libvex_basictypes.h"
#include "libvex.h"

#include "test_main.h"


/*---------------------------------------------------------------*/
/*--- Test                                                    ---*/
/*---------------------------------------------------------------*/


__attribute__ ((noreturn))
static
void failure_exit ( void )
{
   fprintf(stdout, "VEX did failure_exit.  Bye.\n");
   exit(1);
}

static
void log_bytes ( Char* bytes, Int nbytes )
{
   fwrite ( bytes, 1, nbytes, stdout );
}

#define N_LINEBUF 10000
static Char linebuf[N_LINEBUF];

#define N_ORIGBUF 1000
#define N_TRANSBUF 5000

static UChar origbuf[N_ORIGBUF];
static UChar transbuf[N_TRANSBUF];

static Bool verbose = True;

/* Forwards */
static IRBB* ac_instrument ( IRBB*, VexGuestLayout* );


int main ( int argc, char** argv )
{
   FILE* f;
   Int i;
   UInt u, sum;
   Addr32 orig_addr;
   Int bb_number, n_bbs_done = 0;
   Int orig_nbytes, trans_used, orig_used;
   TranslateResult tres;
   VexControl vcon;

   if (argc != 2) {
      fprintf(stderr, "usage: vex file.org\n");
      exit(1);
   }
   f = fopen(argv[1], "r");
   if (!f) {
      fprintf(stderr, "can't open `%s'\n", argv[1]);
      exit(1);
   }

   /* Run with default params.  However, we can't allow bb chasing
      since that causes the front end to get segfaults when it tries
      to read code outside the initial BB we hand it. */
   LibVEX_default_VexControl ( &vcon );
   vcon.guest_chase_thresh = 0;
   vcon.iropt_level = 2;

   LibVEX_Init ( &failure_exit, &log_bytes, 
                 1,  /* debug_paranoia */ 
                 TEST_VSUPPORT, /* valgrind support */
                 &vcon );


   while (!feof(f)) {

      fgets(linebuf, N_LINEBUF,f);
      if (linebuf[0] == 0) continue;
      if (linebuf[0] != '.') continue;

      if (n_bbs_done == TEST_N_BBS) break;
      n_bbs_done++;

      /* first line is:   . bb-number bb-addr n-bytes */
      assert(3 == sscanf(&linebuf[1], " %d %x %d\n", 
                                 & bb_number,
                                 & orig_addr, & orig_nbytes ));
      assert(orig_nbytes >= 1);
      assert(!feof(f));
      fgets(linebuf, N_LINEBUF,f);
      assert(linebuf[0] == '.');

      /* second line is:   . byte byte byte etc */
      if (verbose)
         printf("============ Basic Block %d, "
                "Start %x, nbytes %2d ============", 
                n_bbs_done-1, orig_addr, orig_nbytes);

      assert(orig_nbytes >= 1 && orig_nbytes <= N_ORIGBUF);
      for (i = 0; i < orig_nbytes; i++) {
         assert(1 == sscanf(&linebuf[2 + 3*i], "%x", &u));
         origbuf[i] = (UChar)u;
      }

      for (i = 0; i < TEST_N_ITERS; i++)
         tres
            = LibVEX_Translate ( 
                 InsnSetX86, InsnSetX86,
                 origbuf, (Addr64)orig_addr, &orig_used,
                 transbuf, N_TRANSBUF, &trans_used,
                 ac_instrument, //NULL, /* instrument1 */
                 NULL, /* instrument2 */
                 NULL, /* access checker */
                 TEST_FLAGS 
              );

      if (tres != TransOK)
         printf("\ntres = %d\n", (Int)tres);
      assert(tres == TransOK);
      assert(orig_used == orig_nbytes);

      sum = 0;
      for (i = 0; i < trans_used; i++)
         sum += (UInt)transbuf[i];
      printf ( " %6.2f ... %d\n", (double)trans_used / (double)orig_used, sum );
   }

   fclose(f);
   printf("\n");
   LibVEX_ClearTemporary(True);

   return 0;
}

//////////////////////////////////////////////////////////////////////

static
void panic ( Char* s )
{
  printf("\npanic: %s\n", s);
  failure_exit();
}

static
IRBB* ac_instrument (IRBB* bb_in, VexGuestLayout* layout)
{
/* Use this rather than eg. -1 because it's a UInt. */
#define INVALID_DATA_SIZE   999999

   Int         i;
   Int         sz;
   IRCallee*   helper;
   IRStmt*    st;
   IRExpr* data;
   IRExpr* addr;
   Bool needSz;

   /* Set up BB */
   IRBB* bb     = emptyIRBB();
   bb->tyenv    = dopyIRTypeEnv(bb_in->tyenv);
   bb->next     = dopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

   /* No loads to consider in ->next. */
   assert(isAtom(bb_in->next));

   for (i = 0; i <  bb_in->stmts_used; i++) {
      st = bb_in->stmts[i];
      if (!st) continue;

      switch (st->tag) {

         case Ist_Tmp:
            data = st->Ist.Tmp.data;
            if (data->tag == Iex_LDle) {
               addr = data->Iex.LDle.addr;
               sz = sizeofIRType(data->Iex.LDle.ty);
               needSz = False;
               switch (sz) {
                  case 4: helper = mkIRCallee(1, "ac_helperc_LOAD4", 
                                                 (void*)0x12345601); break;
                  case 2: helper = mkIRCallee(0, "ac_helperc_LOAD2",
                                                 (void*)0x12345602); break;
                  case 1: helper = mkIRCallee(1, "ac_helperc_LOAD1",
                                                 (void*)0x12345603); break;
                  default: helper = mkIRCallee(0, "ac_helperc_LOADN",
                                                  (void*)0x12345604);
                                                  needSz = True; break;
               }
               if (needSz) {
                  addStmtToIRBB( 
                     bb,
                     IRStmt_Dirty(
                        unsafeIRDirty_0_N( helper->regparms, 
					   helper->name, helper->addr,
                                           mkIRExprVec_2(addr, mkIRExpr_HWord(sz)))
                  ));
               } else {
                  addStmtToIRBB( 
                     bb,
                     IRStmt_Dirty(
                        unsafeIRDirty_0_N( helper->regparms, 
					   helper->name, helper->addr, 
                                           mkIRExprVec_1(addr) )
                  ));
               }
            }
            break;

         case Ist_STle:
            data = st->Ist.STle.data;
            addr = st->Ist.STle.addr;
            assert(isAtom(data));
            assert(isAtom(addr));
            sz = sizeofIRType(typeOfIRExpr(bb_in->tyenv, data));
            needSz = False;
            switch (sz) {
               case 4: helper = mkIRCallee(1, "ac_helperc_STORE4", 
                                              (void*)0x12345605); break;
               case 2: helper = mkIRCallee(0, "ac_helperc_STORE2", 
                                              (void*)0x12345606); break;
               case 1: helper = mkIRCallee(1, "ac_helperc_STORE1", 
                                              (void*)0x12345607); break;
               default: helper = mkIRCallee(0, "ac_helperc_STOREN", 
                                               (void*)0x12345608);
                                               needSz = True; break;
            }
            if (needSz) {
               addStmtToIRBB( 
                  bb,
                  IRStmt_Dirty(
                     unsafeIRDirty_0_N( helper->regparms, 
    				        helper->name, helper->addr, 
                                        mkIRExprVec_2(addr, mkIRExpr_HWord(sz)))
               ));
            } else {
               addStmtToIRBB( 
                  bb,
                  IRStmt_Dirty(
                     unsafeIRDirty_0_N( helper->regparms,
                                        helper->name, helper->addr, 
                                        mkIRExprVec_1(addr) )
               ));
            }
            break;

         case Ist_Put:
            assert(isAtom(st->Ist.Put.data));
            break;

         case Ist_PutI:
            assert(isAtom(st->Ist.PutI.ix));
            assert(isAtom(st->Ist.PutI.data));
            break;

         case Ist_Exit:
            assert(isAtom(st->Ist.Exit.cond));
            break;

         case Ist_Dirty:
            /* If the call doesn't interact with memory, we ain't
               interested. */
            if (st->Ist.Dirty.details->mFx == Ifx_None)
               break;
            goto unhandled;

         default:
         unhandled:
            printf("\n");
            ppIRStmt(st);
            printf("\n");
            panic("addrcheck: unhandled IRStmt");
      }

      addStmtToIRBB( bb, dopyIRStmt(st));
   }

   return bb;
}
