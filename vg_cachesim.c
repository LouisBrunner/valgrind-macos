
/*--------------------------------------------------------------------*/
/*--- The cache simulation framework: instrumentation, recording   ---*/
/*--- and results printing.                                        ---*/
/*---                                                vg_cachesim.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2002 Nicholas Nethercote
      njn25@cam.ac.uk

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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"

#include "vg_cachesim_L2.c"
#include "vg_cachesim_I1.c"
#include "vg_cachesim_D1.c"


/* According to IA-32 Intel Architecture Software Developer's Manual: Vol 2 */
#define MAX_x86_INSTR_SIZE              16

/* Size of various buffers used for storing strings */
#define FILENAME_LEN                    256
#define FN_NAME_LEN                     256
#define BUF_LEN                         512
#define COMMIFY_BUF_LEN                 128
#define RESULTS_BUF_LEN                 128
#define LINE_BUF_LEN                     64


/*------------------------------------------------------------*/
/*--- Generic utility stuff                                ---*/
/*------------------------------------------------------------*/

Int VG_(log2) ( Int x ) 
{
   Int i;
   /* Any more than 32 and we overflow anyway... */
   for (i = 0; i < 32; i++) {
      if (1 << i == x) return i;
   }
   return -1;
}


/*------------------------------------------------------------*/
/*--- Output file related stuff                            ---*/
/*------------------------------------------------------------*/

#define OUT_FILE        "cachegrind.out"

static void file_err()
{
   VG_(message)(Vg_UserMsg,
                "error: can't open cache simulation output file `%s'",
                OUT_FILE );
   VG_(exit)(1);
}

/*------------------------------------------------------------*/
/*--- Cost center types, operations                        ---*/
/*------------------------------------------------------------*/

typedef struct _CC CC;
struct _CC {
   ULong a;
   ULong m1;
   ULong m2;
};

static __inline__ void initCC(CC* cc) {
    cc->a  = 0;
    cc->m1 = 0;
    cc->m2 = 0;
}

typedef enum { INSTR_CC, READ_CC, WRITE_CC, MOD_CC } CC_type;

/* Instruction-level cost-centres.  The typedefs for these structs are in
 * vg_include.c 
 *
 * WARNING:  the 'tag' field *must* be the first byte of both CC types.
 *
 * This is because we use it to work out what kind of CC we're dealing with.
 */ 
struct _iCC {
   /* word 1 */
   UChar tag;
   UChar instr_size;
   /* 2 bytes padding */

   /* words 2+ */
   Addr instr_addr;
   CC I;
};

struct _idCC {
   /* word 1 */
   UChar tag;
   UChar instr_size;
   UChar data_size;
   /* 1 byte padding */

   /* words 2+ */
   Addr instr_addr;
   CC I;
   CC D;
};

static void init_iCC(iCC* cc, Addr instr_addr, UInt instr_size)
{
   cc->tag        = INSTR_CC;
   cc->instr_size = instr_size;
   cc->instr_addr = instr_addr;
   initCC(&cc->I);
}

static void init_idCC(CC_type X_CC, idCC* cc, Addr instr_addr,
                      UInt instr_size, UInt data_size)
{
   cc->tag        = X_CC;
   cc->instr_size = instr_size;
   cc->data_size  = data_size;
   cc->instr_addr = instr_addr;
   initCC(&cc->I);
   initCC(&cc->D);
}

#define ADD_CC_TO(CC_type, cc, total)           \
   total.a  += ((CC_type*)BBCC_ptr)->cc.a;      \
   total.m1 += ((CC_type*)BBCC_ptr)->cc.m1;     \
   total.m2 += ((CC_type*)BBCC_ptr)->cc.m2;
          
/* If 1, address of each instruction is printed as a comment after its counts
 * in cachegrind.out */
#define PRINT_INSTR_ADDRS 0

static __inline__ void sprint_iCC(Char buf[BUF_LEN], iCC* cc)
{
#if PRINT_INSTR_ADDRS
   VG_(sprintf)(buf, "%llu %llu %llu # %x\n",
                      cc->I.a, cc->I.m1, cc->I.m2, cc->instr_addr);
#else
   VG_(sprintf)(buf, "%llu %llu %llu\n",
                      cc->I.a, cc->I.m1, cc->I.m2);
#endif
}

static __inline__ void sprint_read_or_mod_CC(Char buf[BUF_LEN], idCC* cc)
{
#if PRINT_INSTR_ADDRS
   VG_(sprintf)(buf, "%llu %llu %llu %llu %llu %llu # %x\n",
                      cc->I.a, cc->I.m1, cc->I.m2, 
                      cc->D.a, cc->D.m1, cc->D.m2, cc->instr_addr);
#else
   VG_(sprintf)(buf, "%llu %llu %llu %llu %llu %llu\n",
                      cc->I.a, cc->I.m1, cc->I.m2, 
                      cc->D.a, cc->D.m1, cc->D.m2);
#endif
}

static __inline__ void sprint_write_CC(Char buf[BUF_LEN], idCC* cc)
{
#if PRINT_INSTR_ADDRS
   VG_(sprintf)(buf, "%llu %llu %llu . . . %llu %llu %llu # %x\n",
                      cc->I.a, cc->I.m1, cc->I.m2, 
                      cc->D.a, cc->D.m1, cc->D.m2, cc->instr_addr);
#else
   VG_(sprintf)(buf, "%llu %llu %llu . . . %llu %llu %llu\n",
                      cc->I.a, cc->I.m1, cc->I.m2, 
                      cc->D.a, cc->D.m1, cc->D.m2);
#endif
}

/*------------------------------------------------------------*/
/*--- BBCC hash table stuff                                ---*/
/*------------------------------------------------------------*/

/* The table of BBCCs is of the form hash(filename, hash(fn_name,
 * hash(BBCCs))).  Each hash table is separately chained.  The sizes below work
 * fairly well for Konqueror. */

#define N_FILE_ENTRIES        251
#define   N_FN_ENTRIES         53
#define N_BBCC_ENTRIES         37

/* The cost centres for a basic block are stored in a contiguous array.
 * They are distinguishable by their tag field. */
typedef struct _BBCC BBCC;
struct _BBCC {
   Addr  orig_addr;
   UInt  array_size;    /* byte-size of variable length array */
   BBCC* next;
   Addr  array[0];      /* variable length array */
};

typedef struct _fn_node fn_node;
struct _fn_node {
   Char*    fn_name;
   BBCC*    BBCCs[N_BBCC_ENTRIES];
   fn_node* next;
};

typedef struct _file_node file_node;
struct _file_node {
   Char*      filename;
   fn_node*   fns[N_FN_ENTRIES];
   file_node* next;
};

/* BBCC_table structure:  list(filename, list(fn_name, list(BBCC))) */
static file_node *BBCC_table[N_FILE_ENTRIES];

static Int  distinct_files      = 0;
static Int  distinct_fns        = 0;

static Int  distinct_instrs     = 0;
static Int  full_debug_BBs      = 0;
static Int  file_line_debug_BBs = 0;
static Int  fn_name_debug_BBs   = 0;
static Int  no_debug_BBs        = 0;

static Int  BB_retranslations   = 0;

static CC Ir_discards;
static CC Dr_discards;
static CC Dw_discards;

static void init_BBCC_table()
{
   Int i;
   for (i = 0; i < N_FILE_ENTRIES; i++)
      BBCC_table[i] = NULL;
}

static void get_debug_info(Addr instr_addr, Char filename[FILENAME_LEN],
                           Char fn_name[FN_NAME_LEN], Int* line_num)
{
   Bool found1, found2, no_demangle = False;

   found1 = VG_(what_line_is_this)(instr_addr, filename,
                                   FILENAME_LEN, line_num);
   found2 = VG_(what_fn_is_this)(no_demangle, instr_addr, fn_name, FN_NAME_LEN);

   if (!found1 && !found2) {
      no_debug_BBs++;
      VG_(strcpy)(filename, "???");
      VG_(strcpy)(fn_name,  "???");
      *line_num = 0;

   } else if ( found1 &&  found2) {
      full_debug_BBs++;

   } else if ( found1 && !found2) {
      file_line_debug_BBs++;
      VG_(strcpy)(fn_name,  "???");

   } else  /*(!found1 &&  found2)*/ {
      fn_name_debug_BBs++;
      VG_(strcpy)(filename, "???");
      *line_num = 0;
   }
}

/* Forward declaration. */
static Int compute_BBCC_array_size(UCodeBlock* cb);

static __inline__ 
file_node* new_file_node(Char filename[FILENAME_LEN], file_node* next)
{
   Int i;
   file_node* new = VG_(malloc)(VG_AR_PRIVATE, sizeof(file_node));
   new->filename  = VG_(strdup)(VG_AR_PRIVATE, filename);
   for (i = 0; i < N_FN_ENTRIES; i++) {
      new->fns[i] = NULL;
   }
   new->next      = next;
   return new;
}

static __inline__ 
fn_node* new_fn_node(Char fn_name[FILENAME_LEN], fn_node* next)
{
   Int i;
   fn_node* new = VG_(malloc)(VG_AR_PRIVATE, sizeof(fn_node));
   new->fn_name = VG_(strdup)(VG_AR_PRIVATE, fn_name);
   for (i = 0; i < N_BBCC_ENTRIES; i++) {
      new->BBCCs[i] = NULL;
   }
   new->next    = next;
   return new;
}

static __inline__ 
BBCC* new_BBCC(Addr bb_orig_addr, UCodeBlock* cb, BBCC* next)
{
   Int BBCC_array_size = compute_BBCC_array_size(cb);
   BBCC* new;

   new = (BBCC*)VG_(malloc)(VG_AR_PRIVATE, sizeof(BBCC) + BBCC_array_size);
   new->orig_addr  = bb_orig_addr;
   new->array_size = BBCC_array_size;
   new->next = next;

   return new;
}

#define HASH_CONSTANT   256

static UInt hash(Char *s, UInt table_size)
{
    int hash_value = 0;
    for ( ; *s; s++)
        hash_value = (HASH_CONSTANT * hash_value + *s) % table_size;
    return hash_value;
}

/* Do a three step traversal: by filename, then fn_name, then instr_addr.
 * In all cases prepends new nodes to their chain.  Returns a pointer to the
 * cost centre.  Also sets BB_seen_before by reference. 
 */ 
static __inline__ BBCC* get_BBCC(Addr bb_orig_addr, UCodeBlock* cb, 
                                 Bool remove, Bool *BB_seen_before)
{
   file_node *curr_file_node;
   fn_node   *curr_fn_node;
   BBCC     **prev_BBCC_next_ptr, *curr_BBCC;
   Char       filename[FILENAME_LEN], fn_name[FN_NAME_LEN];
   UInt       filename_hash, fnname_hash, BBCC_hash;
   Int        dummy_line_num;

   get_debug_info(bb_orig_addr, filename, fn_name, &dummy_line_num);

   VGP_PUSHCC(VgpCacheGetBBCC);
   filename_hash = hash(filename, N_FILE_ENTRIES);
   curr_file_node = BBCC_table[filename_hash];
   while (NULL != curr_file_node && 
          VG_(strcmp)(filename, curr_file_node->filename) != 0) {
      curr_file_node = curr_file_node->next;
   }
   if (NULL == curr_file_node) {
      BBCC_table[filename_hash] = curr_file_node = 
         new_file_node(filename, BBCC_table[filename_hash]);
      distinct_files++;
   }

   fnname_hash = hash(fn_name, N_FN_ENTRIES);
   curr_fn_node = curr_file_node->fns[fnname_hash];
   while (NULL != curr_fn_node && 
          VG_(strcmp)(fn_name, curr_fn_node->fn_name) != 0) {
      curr_fn_node = curr_fn_node->next;
   }
   if (NULL == curr_fn_node) {
      curr_file_node->fns[fnname_hash] = curr_fn_node = 
         new_fn_node(fn_name, curr_file_node->fns[fnname_hash]);
      distinct_fns++;
   }

   BBCC_hash = bb_orig_addr % N_BBCC_ENTRIES;
   prev_BBCC_next_ptr = &(curr_fn_node->BBCCs[BBCC_hash]);
   curr_BBCC = curr_fn_node->BBCCs[BBCC_hash];
   while (NULL != curr_BBCC && bb_orig_addr != curr_BBCC->orig_addr) {
      prev_BBCC_next_ptr = &(curr_BBCC->next);
      curr_BBCC = curr_BBCC->next;
   }
   if (curr_BBCC == NULL) {

      vg_assert(False == remove);

      curr_fn_node->BBCCs[BBCC_hash] = curr_BBCC = 
         new_BBCC(bb_orig_addr, cb, curr_fn_node->BBCCs[BBCC_hash]);
      *BB_seen_before = False;

   } else {
      vg_assert(bb_orig_addr == curr_BBCC->orig_addr);
      vg_assert(curr_BBCC->array_size > 0 && curr_BBCC->array_size < 1000000);
      if (VG_(clo_verbosity) > 2) {
          VG_(message)(Vg_DebugMsg, 
            "BB retranslation, retrieving from BBCC table");
      }
      *BB_seen_before = True;

      if (True == remove) {
          // Remove curr_BBCC from chain;  it will be used and free'd by the
          // caller.
          *prev_BBCC_next_ptr = curr_BBCC->next;

      } else {
          BB_retranslations++;
      }
   }
   VGP_POPCC;
   return curr_BBCC;
}

/*------------------------------------------------------------*/
/*--- Cache simulation instrumentation phase               ---*/
/*------------------------------------------------------------*/

#define uInstr1   VG_(newUInstr1)
#define uInstr2   VG_(newUInstr2)
#define uInstr3   VG_(newUInstr3)
#define dis       VG_(disassemble)
#define uLiteral  VG_(setLiteralField)
#define newTemp   VG_(getNewTemp)

static Int compute_BBCC_array_size(UCodeBlock* cb)
{
   UInstr* u_in;
   Int     i, CC_size, BBCC_size = 0;
   Bool    is_LOAD, is_STORE, is_FPU_R, is_FPU_W;
    
   is_LOAD = is_STORE = is_FPU_R = is_FPU_W = False;

   for (i = 0; i < cb->used; i++) {
      /* VG_(ppUInstr)(0, &cb->instrs[i]); */

      u_in = &cb->instrs[i];
      switch(u_in->opcode) {

         case INCEIP: 
            goto case_for_end_of_instr;
         
         case JMP:
            if (u_in->cond != CondAlways) break;

            goto case_for_end_of_instr;

            case_for_end_of_instr:

            CC_size = (is_LOAD || is_STORE || is_FPU_R || is_FPU_W 
                      ? sizeof(idCC) : sizeof(iCC));

            BBCC_size += CC_size;
            is_LOAD = is_STORE = is_FPU_R = is_FPU_W = False;
            break;

         case LOAD:
            /* Two LDBs are possible for a single instruction */
            /* Also, a STORE can come after a LOAD for bts/btr/btc */
            vg_assert(/*!is_LOAD &&*/ /* !is_STORE && */ 
                      !is_FPU_R && !is_FPU_W);
            is_LOAD = True;
            break;

         case STORE:
            /* Multiple STOREs are possible for 'pushal' */
            vg_assert(            /*!is_STORE &&*/ !is_FPU_R && !is_FPU_W);
            is_STORE = True;
            break;

         case FPU_R:
            vg_assert(!is_LOAD && !is_STORE && !is_FPU_R && !is_FPU_W);
            is_FPU_R = True;
            break;

         case FPU_W:
            vg_assert(!is_LOAD && !is_STORE && !is_FPU_R && !is_FPU_W);
            is_FPU_W = True;
            break;

         default:
            break;
      }
   }

   return BBCC_size;
}

/* Use this rather than eg. -1 because it's stored as a UInt. */
#define INVALID_DATA_SIZE   999999

UCodeBlock* VG_(cachesim_instrument)(UCodeBlock* cb_in, Addr orig_addr)
{
   UCodeBlock* cb;
   Int         i;
   UInstr*     u_in;
   BBCC*       BBCC_node;
   Int         t_CC_addr, t_read_addr, t_write_addr, t_data_addr;
   Int         CC_size = -1;    /* Shut gcc warnings up */
   Addr        instr_addr = orig_addr;
   UInt        instr_size, data_size = INVALID_DATA_SIZE;
   Int         helper = -1;     /* Shut gcc warnings up */
   UInt        stack_used;
   Bool        BB_seen_before       = False;
   Bool        prev_instr_was_Jcond = False;
   Addr        BBCC_ptr0, BBCC_ptr; 

   /* Get BBCC (creating if necessary -- requires a counting pass over the BB
    * if it's the first time it's been seen), and point to start of the 
    * BBCC array.  */
   BBCC_node = get_BBCC(orig_addr, cb_in, False, &BB_seen_before);
   BBCC_ptr0 = BBCC_ptr = (Addr)(BBCC_node->array);

   cb = VG_(allocCodeBlock)();
   cb->nextTemp = cb_in->nextTemp;

   t_CC_addr = t_read_addr = t_write_addr = t_data_addr = INVALID_TEMPREG;

   for (i = 0; i < cb_in->used; i++) {
      u_in = &cb_in->instrs[i];

      //VG_(ppUInstr)(0, u_in);

      /* What this is all about:  we want to instrument each x86 instruction 
       * translation.  The end of these are marked in three ways.  The three
       * ways, and the way we instrument them, are as follows:
       *
       * 1. UCode, INCEIP         --> UCode, Instrumentation, INCEIP
       * 2. UCode, Juncond        --> UCode, Instrumentation, Juncond
       * 3. UCode, Jcond, Juncond --> UCode, Instrumentation, Jcond, Juncond
       *
       * We must put the instrumentation before the jumps so that it is always
       * executed.  We don't have to put the instrumentation before the INCEIP
       * (it could go after) but we do so for consistency.
       *
       * Junconds are always the last instruction in a basic block.  Jconds are
       * always the 2nd last, and must be followed by a Jcond.  We check this
       * with various assertions.
       *
       * Note that in VG_(disBB) we patched the `extra4b' field of the first
       * occurring JMP in a block with the size of its x86 instruction.  This
       * is used now.
       *
       * Note that we don't have to treat JIFZ specially;  unlike JMPs, JIFZ
       * occurs in the middle of a BB and gets an INCEIP after it.
       *
       * The instrumentation is just a call to the appropriate helper function,
       * passing it the address of the instruction's CC.
       */
      if (prev_instr_was_Jcond) vg_assert(u_in->opcode == JMP);

      switch (u_in->opcode) {

         case INCEIP:
            instr_size = u_in->val1;
            goto case_for_end_of_x86_instr;

         case JMP:
            if (u_in->cond == CondAlways) {
               vg_assert(i+1 == cb_in->used); 

               /* Don't instrument if previous instr was a Jcond. */
               if (prev_instr_was_Jcond) {
                  vg_assert(0 == u_in->extra4b);
                  VG_(copyUInstr)(cb, u_in);
                  break;
               }
               prev_instr_was_Jcond = False;

            } else {
               vg_assert(i+2 == cb_in->used);  /* 2nd last instr in block */
               prev_instr_was_Jcond = True;
            }

            /* Ah, the first JMP... instrument, please. */
            instr_size = u_in->extra4b;
            goto case_for_end_of_x86_instr;

            /* Shared code that is executed at the end of an x86 translation
             * block, marked by either an INCEIP or an unconditional JMP. */
            case_for_end_of_x86_instr:

#define IS_(X)      (INVALID_TEMPREG != t_##X##_addr)
             
            /* Initialise the CC in the BBCC array appropriately if it hasn't
             * been initialised before.
             * Then call appropriate sim function, passing it the CC address.
             * Note that CALLM_S/CALL_E aren't required here;  by this point,
             * the checking related to them has already happened. */
            stack_used = 0;

            vg_assert(instr_size >= 1 && instr_size <= MAX_x86_INSTR_SIZE);
            vg_assert(0 != instr_addr);

            /* Save the caller-save registers before we push our args */
            uInstr1(cb, PUSH, 4, RealReg, R_EAX);
            uInstr1(cb, PUSH, 4, RealReg, R_ECX);
            uInstr1(cb, PUSH, 4, RealReg, R_EDX);

            if (!IS_(read) && !IS_(write)) {
               iCC* CC_ptr = (iCC*)(BBCC_ptr);
               vg_assert(INVALID_DATA_SIZE == data_size);
               vg_assert(INVALID_TEMPREG == t_read_addr && 
                         INVALID_TEMPREG == t_write_addr);
               CC_size = sizeof(iCC);
               if (!BB_seen_before)
                   init_iCC(CC_ptr, instr_addr, instr_size);

               helper = VGOFF_(cachesim_log_non_mem_instr);

            } else { 
               CC_type X_CC;
               idCC* CC_ptr = (idCC*)(BBCC_ptr);
                
               vg_assert(4 == data_size || 2  == data_size || 1 == data_size || 
                         8 == data_size || 10 == data_size);
               
               CC_size = sizeof(idCC);
               helper = VGOFF_(cachesim_log_mem_instr);

               if (IS_(read) && !IS_(write)) {
                  X_CC = READ_CC;
                  vg_assert(INVALID_TEMPREG != t_read_addr && 
                            INVALID_TEMPREG == t_write_addr);
                  t_data_addr = t_read_addr;

               } else if (!IS_(read) && IS_(write)) {
                  X_CC = WRITE_CC;
                  vg_assert(INVALID_TEMPREG == t_read_addr && 
                            INVALID_TEMPREG != t_write_addr);
                  t_data_addr = t_write_addr;

               } else {
                  vg_assert(IS_(read) && IS_(write));
                  X_CC = MOD_CC;
                  vg_assert(INVALID_TEMPREG != t_read_addr && 
                            INVALID_TEMPREG != t_write_addr);
                  t_data_addr = t_read_addr;
               }

               if (!BB_seen_before)
                  init_idCC(X_CC, CC_ptr, instr_addr, instr_size, data_size);

               /* 2nd arg: data addr */
               uInstr1(cb, PUSH,  4, TempReg, t_data_addr);
               stack_used += 4;
            }
#undef IS_

            /* 1st arg: CC addr */
            t_CC_addr = newTemp(cb);
            uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_CC_addr);
            uLiteral(cb, BBCC_ptr);
            uInstr1(cb, PUSH,  4, TempReg, t_CC_addr);
            stack_used += 4;

            /* Call function and return. */
            uInstr1(cb, CALLM, 0, Lit16,   helper);
            uInstr1(cb, CLEAR, 0, Lit16,   stack_used);

            /* Restore the caller-save registers now the call is done */
            uInstr1(cb, POP, 4, RealReg, R_EDX);
            uInstr1(cb, POP, 4, RealReg, R_ECX);
            uInstr1(cb, POP, 4, RealReg, R_EAX);

            VG_(copyUInstr)(cb, u_in);

            /* Update BBCC_ptr, EIP, de-init read/write temps for next instr */
            BBCC_ptr   += CC_size; 
            instr_addr += instr_size;
            t_CC_addr = t_read_addr = t_write_addr = 
                                      t_data_addr  = INVALID_TEMPREG;
            data_size = INVALID_DATA_SIZE;
            break;


         /* For memory-ref instrs, copy the data_addr into a temporary to be
          * passed to the cachesim_log_function at the end of the instruction.
          */
         case LOAD: 
            t_read_addr = newTemp(cb);
            uInstr2(cb, MOV, 4, TempReg, u_in->val1,  TempReg, t_read_addr);
            data_size = u_in->size;
            VG_(copyUInstr)(cb, u_in);
            break;

         case FPU_R:
            t_read_addr = newTemp(cb);
            uInstr2(cb, MOV, 4, TempReg, u_in->val2,  TempReg, t_read_addr);
            data_size = u_in->size;
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Note that we must set t_write_addr even for mod instructions;
          * that's how the code above determines whether it does a write;
          * without it, it would think a mod instruction is a read.
          * As for the MOV, if it's a mod instruction it's redundant, but it's
          * not expensive and mod instructions are rare anyway. */
         case STORE:
         case FPU_W:
            t_write_addr = newTemp(cb);
            uInstr2(cb, MOV, 4, TempReg, u_in->val2, TempReg, t_write_addr);
            data_size = u_in->size;
            VG_(copyUInstr)(cb, u_in);
            break;

         case NOP:  case CALLM_E:  case CALLM_S:
            break;

         default:
            VG_(copyUInstr)(cb, u_in);
            break;
      }
   }

   /* Just check everything looks ok */
   vg_assert(BBCC_ptr - BBCC_ptr0 == BBCC_node->array_size);

   VG_(freeCodeBlock)(cb_in);
   return cb;
}

/*------------------------------------------------------------*/
/*--- Cache simulation stuff                               ---*/
/*------------------------------------------------------------*/

#define MIN_LINE_SIZE   16

/* Total reads/writes/misses.  Calculated during CC traversal at the end. */
static CC Ir_total;
static CC Dr_total;
static CC Dw_total;

/* All CPUID info taken from sandpile.org/a32/cpuid.htm */
/* Probably only works for Intel and AMD chips, and probably only for some of
 * them. 
 */

static __inline__ void cpuid(Int n, Int *a, Int *b, Int *c, Int *d)
{
   __asm__ __volatile__ (
    "cpuid"
    : "=a" (*a), "=b" (*b), "=c" (*c), "=d" (*d)      /* output */
    : "0" (n)         /* input */
    );
}

static void micro_ops_warn(Int actual_size, Int used_size, Int line_size)
{
    VG_(message)(Vg_DebugMsg, 
       "warning: Pentium with %d K micro_op instruction trace cache", 
       actual_size);
    VG_(message)(Vg_DebugMsg, 
       "         Simulating a %d KB cache with %d B lines", 
       used_size, line_size);
}

/* Intel method is truly wretched.  We have to do an insane indexing into an
 * array of pre-defined configurations for various parts of the memory
 * hierarchy. 
 */
static
Int Intel_cache_info(Int level, cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   UChar info[16];
   Int   i, trials;

   if (level < 2) {
      VG_(message)(Vg_DebugMsg, 
         "warning: CPUID level < 2 for Intel processor (%d)", 
         level);
      return -1;
   }

   cpuid(2, (Int*)&info[0], (Int*)&info[4], 
            (Int*)&info[8], (Int*)&info[12]);
   trials  = info[0] - 1;   /* AL register - bits 0..7 of %eax */
   info[0] = 0x0;           /* reset AL */

   if (0 != trials) {
      VG_(message)(Vg_DebugMsg, 
         "warning: non-zero CPUID trials for Intel processor (%d)",
         trials);
      return -1;
   }

   for (i = 0; i < 16; i++) {

      switch (info[i]) {

      case 0x0:       /* ignore zeros */
          break;
          
      case 0x01: case 0x02: case 0x03: case 0x04:     /* TLB info, ignore */
      case 0x90: case 0x96: case 0x9b:
          break;      

      case 0x06: *I1c = (cache_t) {  8, 4, 32 }; break;
      case 0x08: *I1c = (cache_t) { 16, 4, 32 }; break;

      case 0x0a: *D1c = (cache_t) {  8, 2, 32 }; break;
      case 0x0c: *D1c = (cache_t) { 16, 4, 32 }; break;

      case 0x22: case 0x23: case 0x25: case 0x29: 
      case 0x88: case 0x89: case 0x8a:
          VG_(message)(Vg_DebugMsg, 
             "warning: L3 cache detected but ignored\n");
          break;

      case 0x40: 
          VG_(message)(Vg_DebugMsg, 
             "warning: L2 cache not installed, ignore L2 results.");
          break;

      case 0x41: *L2c = (cache_t) {  128, 4, 32 };    break;
      case 0x42: *L2c = (cache_t) {  256, 4, 32 };    break;
      case 0x43: *L2c = (cache_t) {  512, 4, 32 };    break;
      case 0x44: *L2c = (cache_t) { 1024, 4, 32 };    break;
      case 0x45: *L2c = (cache_t) { 2048, 4, 32 };    break;

      /* These are sectored, whatever that means */
      case 0x66: *D1c = (cache_t) {  8, 4, 64 };  break;      /* sectored */
      case 0x67: *D1c = (cache_t) { 16, 4, 64 };  break;      /* sectored */
      case 0x68: *D1c = (cache_t) { 32, 4, 64 };  break;      /* sectored */

      /* HACK ALERT: Instruction trace cache -- capacity is micro-ops based.
       * conversion to byte size is a total guess;  treat the 12K and 16K
       * cases the same since the cache byte size must be a power of two for
       * everything to work!.  Also guessing 32 bytes for the line size... 
       */
      case 0x70:    /* 12K micro-ops, 8-way */
         *I1c = (cache_t) { 16, 8, 32 };  
         micro_ops_warn(12, 16, 32);
         break;  
      case 0x71:    /* 16K micro-ops, 8-way */
         *I1c = (cache_t) { 16, 8, 32 };  
         micro_ops_warn(16, 16, 32); 
         break;  
      case 0x72:    /* 32K micro-ops, 8-way */
         *I1c = (cache_t) { 32, 8, 32 };  
         micro_ops_warn(32, 32, 32); 
         break;  

      case 0x79: *L2c = (cache_t) {  128, 8, 64 };    break;  /* sectored */
      case 0x7a: *L2c = (cache_t) {  256, 8, 64 };    break;  /* sectored */
      case 0x7b: *L2c = (cache_t) {  512, 8, 64 };    break;  /* sectored */
      case 0x7c: *L2c = (cache_t) { 1024, 8, 64 };    break;  /* sectored */

      case 0x81: *L2c = (cache_t) {  128, 8, 32 };    break;
      case 0x82: *L2c = (cache_t) {  256, 8, 32 };    break;
      case 0x83: *L2c = (cache_t) {  512, 8, 32 };    break;
      case 0x84: *L2c = (cache_t) { 1024, 8, 32 };    break;
      case 0x85: *L2c = (cache_t) { 2048, 8, 32 };    break;

      default:
          VG_(message)(Vg_DebugMsg, 
             "warning: Unknown Intel cache config value "
             "(0x%x), ignoring\n", info[i]);
          break;
      }
   }
   return 0;
}

/* AMD method is straightforward, just extract appropriate bits from the
 * result registers.
 *
 * Bits, for D1 and I1:
 *  31..24  data L1 cache size in KBs    
 *  23..16  data L1 cache associativity (FFh=full)    
 *  15.. 8  data L1 cache lines per tag    
 *   7.. 0  data L1 cache line size in bytes
 *
 * Bits, for L2:
 *  31..16  unified L2 cache size in KBs
 *  15..12  unified L2 cache associativity (0=off, FFh=full)
 *  11.. 8  unified L2 cache lines per tag    
 *   7.. 0  unified L2 cache line size in bytes
 *
 * #3  The AMD K7 processor's L2 cache must be configured prior to relying 
 *     upon this information. (Whatever that means -- njn)
 *
 * Returns 0 on success, non-zero on failure.
 */
static
Int AMD_cache_info(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   Int dummy, ext_level;
   Int I1i, D1i, L2i;
   
   cpuid(0x80000000, &ext_level, &dummy, &dummy, &dummy);

   if (0 == (ext_level & 0x80000000) || ext_level < 0x80000006) {
      VG_(message)(Vg_UserMsg, 
         "warning: ext_level < 0x80000006 for AMD processor (0x%x)", 
         ext_level);
      return -1;
   }

   cpuid(0x80000005, &dummy, &dummy, &D1i, &I1i);
   cpuid(0x80000006, &dummy, &dummy, &L2i, &dummy);

   D1c->size      = (D1i >> 24) & 0xff;
   D1c->assoc     = (D1i >> 16) & 0xff;
   D1c->line_size = (D1i >>  0) & 0xff;

   I1c->size      = (I1i >> 24) & 0xff;
   I1c->assoc     = (I1i >> 16) & 0xff;
   I1c->line_size = (I1i >>  0) & 0xff;

   L2c->size      = (L2i >> 16) & 0xffff; /* Nb: different bits used for L2 */
   L2c->assoc     = (L2i >> 12) & 0xf;
   L2c->line_size = (L2i >>  0) & 0xff;

   return 0;
}

static jmp_buf cpuid_jmpbuf;

static
void cpuid_SIGILL_handler(int signum)
{
   __builtin_longjmp(cpuid_jmpbuf, 1);
}

static 
Int get_caches_from_CPUID(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   Int  level, res, ret;
   Char vendor_id[13];
   vki_ksigaction sigill_new, sigill_saved;

   /* Install own SIGILL handler */
   sigill_new.ksa_handler  = cpuid_SIGILL_handler;
   sigill_new.ksa_flags    = 0;
   sigill_new.ksa_restorer = NULL;
   res = VG_(ksigemptyset)( &sigill_new.ksa_mask );
   vg_assert(res == 0);

   res = VG_(ksigaction)( VKI_SIGILL, &sigill_new, &sigill_saved );
   vg_assert(res == 0);

   /* Trap for illegal instruction, in case it's a really old processor that
    * doesn't support CPUID. */
   if (__builtin_setjmp(cpuid_jmpbuf) == 0) {
      cpuid(0, &level, (int*)&vendor_id[0], 
                       (int*)&vendor_id[8], (int*)&vendor_id[4]);    
      vendor_id[12] = '\0';

      /* Restore old SIGILL handler */
      res = VG_(ksigaction)( VKI_SIGILL, &sigill_saved, NULL );
      vg_assert(res == 0);

   } else  {
      VG_(message)(Vg_DebugMsg, "CPUID instruction not supported");

      /* Restore old SIGILL handler */
      res = VG_(ksigaction)( VKI_SIGILL, &sigill_saved, NULL );
      vg_assert(res == 0);
      return -1;
   }

   if (0 == level) {
      VG_(message)(Vg_DebugMsg, "CPUID level is 0, early Pentium?\n");
      return -1;
   }

   /* Only handling Intel and AMD chips... no Cyrix, Transmeta, etc */
   if (0 == VG_(strcmp)(vendor_id, "GenuineIntel")) {
      ret = Intel_cache_info(level, I1c, D1c, L2c);

   } else if (0 == VG_(strcmp)(vendor_id, "AuthenticAMD")) {
      ret = AMD_cache_info(I1c, D1c, L2c);

   } else {
      VG_(message)(Vg_DebugMsg, "CPU vendor ID not recognised (%s)",
                   vendor_id);
      return -1;
   }

   /* Successful!  Convert sizes from KB to bytes */
   I1c->size *= 1024;
   D1c->size *= 1024;
   L2c->size *= 1024;
      
   return ret;
}

/* Checks cache config is ok;  makes it so if not. */
static 
void check_cache(cache_t* cache, cache_t* dflt, Char *name)
{
   /* First check they're all powers of two */
   if (-1 == VG_(log2)(cache->size)) {
      VG_(message)(Vg_UserMsg,
         "warning: %s size of %dB not a power of two; "
         "defaulting to %dB", name, cache->size, dflt->size);
      cache->size = dflt->size;
   }

   if (-1 == VG_(log2)(cache->assoc)) {
      VG_(message)(Vg_UserMsg,
         "warning: %s associativity of %d not a power of two; "
         "defaulting to %d-way", name, cache->assoc, dflt->assoc);
      cache->assoc = dflt->assoc;
   }

   if (-1 == VG_(log2)(cache->line_size)) {
      VG_(message)(Vg_UserMsg,
         "warning: %s line size of %dB not a power of two; "
         "defaulting to %dB", 
         name, cache->line_size, dflt->line_size);
      cache->line_size = dflt->line_size;
   }

   /* Then check line size >= 16 -- any smaller and a single instruction could
    * straddle three cache lines, which breaks a simulation assertion and is
    * stupid anyway. */
   if (cache->line_size < MIN_LINE_SIZE) {
      VG_(message)(Vg_UserMsg,
         "warning: %s line size of %dB too small; "
         "increasing to %dB", name, cache->line_size, MIN_LINE_SIZE);
      cache->line_size = MIN_LINE_SIZE;
   }

   /* Then check cache size > line size (causes seg faults if not). */
   if (cache->size <= cache->line_size) {
      VG_(message)(Vg_UserMsg,
         "warning: %s cache size of %dB <= line size of %dB; "
         "increasing to %dB", name, cache->size, cache->line_size,
                              cache->line_size * 2);
      cache->size = cache->line_size * 2;
   }

   /* Then check assoc <= (size / line size) (seg faults otherwise). */
   if (cache->assoc > (cache->size / cache->line_size)) {
      VG_(message)(Vg_UserMsg,
         "warning: %s associativity > (size / line size); "
         "increasing size to %dB", 
            name, cache->assoc * cache->line_size);
      cache->size = cache->assoc * cache->line_size;
   }
}

/* On entry, args are undefined.  Fill them with any info from the
 * command-line, then fill in any remaining with CPUID instruction if possible,
 * otherwise use defaults.  Then check them and fix if not ok. */
static 
void get_caches(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   /* Defaults are for a model 3 or 4 Athlon */
   cache_t I1_dflt = (cache_t) {  65536, 2, 64 };
   cache_t D1_dflt = (cache_t) {  65536, 2, 64 };
   cache_t L2_dflt = (cache_t) { 262144, 8, 64 };

#define CMD_LINE_DEFINED(L)                 \
   (-1 != VG_(clo_##L##_cache).size  ||     \
    -1 != VG_(clo_##L##_cache).assoc ||     \
    -1 != VG_(clo_##L##_cache).line_size)

   /* If any undefined on command-line, try CPUID */
   if (! CMD_LINE_DEFINED(I1) ||
       ! CMD_LINE_DEFINED(D1) ||
       ! CMD_LINE_DEFINED(L2)) { 

      /* Overwrite CPUID result for any cache defined on command-line */
      if (0 == get_caches_from_CPUID(I1c, D1c, L2c)) {
   
         if (CMD_LINE_DEFINED(I1)) *I1c = VG_(clo_I1_cache);
         if (CMD_LINE_DEFINED(D1)) *D1c = VG_(clo_D1_cache);
         if (CMD_LINE_DEFINED(L2)) *L2c = VG_(clo_L2_cache);

      /* CPUID failed, use defaults for each undefined by command-line */
      } else {
         VG_(message)(Vg_DebugMsg, 
                      "Couldn't detect cache configuration, using one "
                      "or more defaults ");

         *I1c = (CMD_LINE_DEFINED(I1) ? VG_(clo_I1_cache) : I1_dflt);
         *D1c = (CMD_LINE_DEFINED(D1) ? VG_(clo_D1_cache) : D1_dflt);
         *L2c = (CMD_LINE_DEFINED(L2) ? VG_(clo_L2_cache) : L2_dflt);
      }
   }
#undef CMD_LINE_DEFINED

   check_cache(I1c, &I1_dflt, "I1");
   check_cache(D1c, &D1_dflt, "D1");
   check_cache(L2c, &L2_dflt, "L2");

   if (VG_(clo_verbosity) > 1) {
      VG_(message)(Vg_UserMsg, "Cache configuration used:");
      VG_(message)(Vg_UserMsg, "  I1: %dB, %d-way, %dB lines",
                               I1c->size, I1c->assoc, I1c->line_size);
      VG_(message)(Vg_UserMsg, "  D1: %dB, %d-way, %dB lines",
                               D1c->size, D1c->assoc, D1c->line_size);
      VG_(message)(Vg_UserMsg, "  L2: %dB, %d-way, %dB lines",
                               L2c->size, L2c->assoc, L2c->line_size);
   }
}

void VG_(init_cachesim)(void)
{
   cache_t I1c, D1c, L2c; 

   /* Make sure the output file can be written. */
   Int fd = VG_(open_write)(OUT_FILE);
   if (-1 == fd) { 
      fd = VG_(create_and_write)(OUT_FILE);
      if (-1 == fd) {
         file_err(); 
      }
   }
   VG_(close)(fd);

   initCC(&Ir_total);
   initCC(&Dr_total);
   initCC(&Dw_total);
   
   initCC(&Ir_discards);
   initCC(&Dr_discards);
   initCC(&Dw_discards);

   get_caches(&I1c, &D1c, &L2c);

   cachesim_I1_initcache(I1c);
   //cachesim_I1_initcache();
   cachesim_D1_initcache(D1c);
   //cachesim_D1_initcache();
   cachesim_L2_initcache(L2c);
   //cachesim_L2_initcache();

   init_BBCC_table();
}

void VG_(cachesim_log_non_mem_instr)(iCC* cc)
{
   //VG_(printf)("sim  I: CCaddr=0x%x, iaddr=0x%x, isize=%u\n",
   //            cc, cc->instr_addr, cc->instr_size)
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(cc->instr_addr, cc->instr_size, &cc->I.m1, &cc->I.m2);
   cc->I.a++;
   VGP_POPCC;
}

void VG_(cachesim_log_mem_instr)(idCC* cc, Addr data_addr)
{
   //VG_(printf)("sim  D: CCaddr=0x%x, iaddr=0x%x, isize=%u, daddr=0x%x, dsize=%u\n",
   //            cc, cc->instr_addr, cc->instr_size, data_addr, cc->data_size)
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(cc->instr_addr, cc->instr_size, &cc->I.m1, &cc->I.m2);
   cc->I.a++;

   cachesim_D1_doref(data_addr,      cc->data_size,  &cc->D.m1, &cc->D.m2);
   cc->D.a++;
   VGP_POPCC;
}

/*------------------------------------------------------------*/
/*--- Printing of output file and summary stats            ---*/
/*------------------------------------------------------------*/

static void fprint_BBCC(Int fd, BBCC* BBCC_node, Char *first_instr_fl, 
                                                 Char *first_instr_fn)
{
   Addr BBCC_ptr0, BBCC_ptr;
   Char buf[BUF_LEN], curr_file[BUF_LEN], 
        fbuf[BUF_LEN+4], lbuf[LINE_BUF_LEN];
   UInt line_num;

   BBCC_ptr0 = BBCC_ptr = (Addr)(BBCC_node->array);

   /* Mark start of basic block in output, just to ease debugging */
   VG_(write)(fd, (void*)"\n", 1);  

   VG_(strcpy)(curr_file, first_instr_fl);
   
   while (BBCC_ptr - BBCC_ptr0 < BBCC_node->array_size) {

      /* We pretend the CC is an iCC for getting the tag.  This is ok
       * because both CC types have tag as their first byte.  Once we know
       * the type, we can cast and act appropriately. */

      Char fl_buf[FILENAME_LEN];
      Char fn_buf[FN_NAME_LEN];

      Addr instr_addr;
      switch ( ((iCC*)BBCC_ptr)->tag ) {

         case INSTR_CC:
            instr_addr = ((iCC*)BBCC_ptr)->instr_addr;
            sprint_iCC(buf, (iCC*)BBCC_ptr);
            ADD_CC_TO(iCC, I, Ir_total);
            BBCC_ptr += sizeof(iCC);
            break;

         case READ_CC:
         case  MOD_CC:
            instr_addr = ((idCC*)BBCC_ptr)->instr_addr;
            sprint_read_or_mod_CC(buf, (idCC*)BBCC_ptr);
            ADD_CC_TO(idCC, I, Ir_total);
            ADD_CC_TO(idCC, D, Dr_total);
            BBCC_ptr += sizeof(idCC);
            break;

         case WRITE_CC:
            instr_addr = ((idCC*)BBCC_ptr)->instr_addr;
            sprint_write_CC(buf, (idCC*)BBCC_ptr);
            ADD_CC_TO(idCC, I, Ir_total);
            ADD_CC_TO(idCC, D, Dw_total);
            BBCC_ptr += sizeof(idCC);
            break;

         default:
            VG_(panic)("Unknown CC type in fprint_BBCC()\n");
            break;
      }
      distinct_instrs++;
      
      get_debug_info(instr_addr, fl_buf, fn_buf, &line_num);

      /* Allow for filename switching in the middle of a BB;  if this happens,
       * must print the new filename with the function name. */
      if (0 != VG_(strcmp)(fl_buf, curr_file)) {
         VG_(strcpy)(curr_file, fl_buf);
         VG_(sprintf)(fbuf, "fi=%s\n", curr_file);
         VG_(write)(fd, (void*)fbuf, VG_(strlen)(fbuf));
      }

      /* If the function name for this instruction doesn't match that of the
       * first instruction in the BB, print warning. */
      if (VG_(clo_trace_symtab) && 0 != VG_(strcmp)(fn_buf, first_instr_fn)) {
         VG_(printf)("Mismatched function names\n");
         VG_(printf)("  filenames: BB:%s, instr:%s;"
                     "  fn_names:  BB:%s, instr:%s;"
                     "  line: %d\n", 
                     first_instr_fl, fl_buf, 
                     first_instr_fn, fn_buf, 
                     line_num);
      }

      VG_(sprintf)(lbuf, "%u ", line_num);
      VG_(write)(fd, (void*)lbuf, VG_(strlen)(lbuf));   /* line number */
      VG_(write)(fd, (void*)buf, VG_(strlen)(buf));     /* cost centre */
   }
   /* If we switched filenames in the middle of the BB without switching back,
    * switch back now because the subsequent BB may be relying on falling under
    * the original file name. */
   if (0 != VG_(strcmp)(first_instr_fl, curr_file)) {
      VG_(sprintf)(fbuf, "fe=%s\n", first_instr_fl);
      VG_(write)(fd, (void*)fbuf, VG_(strlen)(fbuf));
   }

   /* Mark end of basic block */
   /* VG_(write)(fd, (void*)"#}\n", 3); */

   vg_assert(BBCC_ptr - BBCC_ptr0 == BBCC_node->array_size);
}

static void fprint_BBCC_table_and_calc_totals(Int client_argc, 
                                              Char** client_argv)
{
   Int        fd;
   Char       buf[BUF_LEN];
   file_node *curr_file_node;
   fn_node   *curr_fn_node;
   BBCC      *curr_BBCC;
   Int        i,j,k;

   VGP_PUSHCC(VgpCacheDump);
   fd = VG_(open_write)(OUT_FILE);
   if (-1 == fd) { file_err(); }

   /* "desc:" lines (giving I1/D1/L2 cache configuration) */
   VG_(sprintf)(buf, "desc: I1 cache:         %s\n", I1.desc_line);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   VG_(sprintf)(buf, "desc: D1 cache:         %s\n", D1.desc_line);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   VG_(sprintf)(buf, "desc: L2 cache:         %s\n", L2.desc_line);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   /* "cmd:" line */
   VG_(strcpy)(buf, "cmd:");
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   for (i = 0; i < client_argc; i++) {
       VG_(sprintf)(buf, " %s", client_argv[i]);
       VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   }
   /* "events:" line */
   VG_(sprintf)(buf, "\nevents: Ir I1mr I2mr Dr D1mr D2mr Dw D1mw D2mw\n");
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   /* Six loops here:  three for the hash table arrays, and three for the
    * chains hanging off the hash table arrays. */
   for (i = 0; i < N_FILE_ENTRIES; i++) {
      curr_file_node = BBCC_table[i];
      while (curr_file_node != NULL) {
         VG_(sprintf)(buf, "fl=%s\n", curr_file_node->filename);
         VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

         for (j = 0; j < N_FN_ENTRIES; j++) {
            curr_fn_node = curr_file_node->fns[j];
            while (curr_fn_node != NULL) {
               VG_(sprintf)(buf, "fn=%s\n", curr_fn_node->fn_name);
               VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

               for (k = 0; k < N_BBCC_ENTRIES; k++) {
                  curr_BBCC = curr_fn_node->BBCCs[k];
                  while (curr_BBCC != NULL) {
                     fprint_BBCC(fd, curr_BBCC, 
                             
                             curr_file_node->filename,
                             curr_fn_node->fn_name);

                     curr_BBCC = curr_BBCC->next;
                  }
               }
               curr_fn_node = curr_fn_node->next;
            }
         }
         curr_file_node = curr_file_node->next;
      }
   }

   /* Print stats from any discarded basic blocks */
   if (0 != Ir_discards.a) {

      VG_(sprintf)(buf, "fl=(discarded)\n");
      VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
      VG_(sprintf)(buf, "fn=(discarded)\n");
      VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

      /* Use 0 as line number */
      VG_(sprintf)(buf, "0 %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
                   Ir_discards.a, Ir_discards.m1, Ir_discards.m2, 
                   Dr_discards.a, Dr_discards.m1, Dr_discards.m2, 
                   Dw_discards.a, Dw_discards.m1, Dw_discards.m2);
      VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

      Ir_total.a  += Ir_discards.a;
      Ir_total.m1 += Ir_discards.m1;
      Ir_total.m2 += Ir_discards.m2;
      Dr_total.a  += Dr_discards.a;
      Dr_total.m1 += Dr_discards.m1;
      Dr_total.m2 += Dr_discards.m2;
      Dw_total.a  += Dw_discards.a;
      Dw_total.m1 += Dw_discards.m1;
      Dw_total.m2 += Dw_discards.m2;
   }

   /* Summary stats must come after rest of table, since we calculate them
    * during traversal.  */ 
   VG_(sprintf)(buf, "summary: "
                     "%llu %llu %llu "
                     "%llu %llu %llu "
                     "%llu %llu %llu\n", 
                     Ir_total.a, Ir_total.m1, Ir_total.m2,
                     Dr_total.a, Dr_total.m1, Dr_total.m2,
                     Dw_total.a, Dw_total.m1, Dw_total.m2);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   VG_(close)(fd);
}

/* Adds commas to ULong, right justifying in a field field_width wide, returns
 * the string in buf. */
static
Int commify(ULong n, int field_width, char buf[COMMIFY_BUF_LEN])
{
   int len, n_commas, i, j, new_len, space;

   VG_(sprintf)(buf, "%lu", n);
   len = VG_(strlen)(buf);
   n_commas = (len - 1) / 3;
   new_len = len + n_commas;
   space = field_width - new_len;

   /* Allow for printing a number in a field_width smaller than it's size */
   if (space < 0) space = 0;    

   /* Make j = -1 because we copy the '\0' before doing the numbers in groups
    * of three. */
   for (j = -1, i = len ; i >= 0; i--) {
      buf[i + n_commas + space] = buf[i];

      if (3 == ++j) {
         j = 0;
         n_commas--;
         buf[i + n_commas + space] = ',';
      }
   }
   /* Right justify in field. */
   for (i = 0; i < space; i++)  buf[i] = ' ';
   return new_len;
}

static
void percentify(Int n, Int pow, Int field_width, char buf[]) 
{
   int i, len, space;
    
   VG_(sprintf)(buf, "%d.%d%%", n / pow, n % pow);
   len = VG_(strlen)(buf);
   space = field_width - len;
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}

void VG_(do_cachesim_results)(Int client_argc, Char** client_argv)
{
   CC D_total;
   ULong L2_total_m, L2_total_mr, L2_total_mw,
         L2_total, L2_total_r, L2_total_w;
   char buf1[RESULTS_BUF_LEN], 
        buf2[RESULTS_BUF_LEN], 
        buf3[RESULTS_BUF_LEN];
   Int l1, l2, l3;
   Int p;

   fprint_BBCC_table_and_calc_totals(client_argc, client_argv);

   if (VG_(clo_verbosity) == 0) 
      return;

   /* I cache results.  Use the I_refs value to determine the first column
    * width. */
   l1 = commify(Ir_total.a, 0, buf1);
   VG_(message)(Vg_UserMsg, "I   refs:      %s", buf1);

   commify(Ir_total.m1, l1, buf1);
   VG_(message)(Vg_UserMsg, "I1  misses:    %s", buf1);

   commify(Ir_total.m2, l1, buf1);
   VG_(message)(Vg_UserMsg, "L2i misses:    %s", buf1);

   p = 100;

   percentify(Ir_total.m1 * 100 * p / Ir_total.a, p, l1+1, buf1);
   VG_(message)(Vg_UserMsg, "I1  miss rate: %s", buf1);
                
   percentify(Ir_total.m2 * 100 * p / Ir_total.a, p, l1+1, buf1);
   VG_(message)(Vg_UserMsg, "L2i miss rate: %s", buf1);
   VG_(message)(Vg_UserMsg, "");

   /* D cache results.  Use the D_refs.rd and D_refs.wr values to determine the
    * width of columns 2 & 3. */
   D_total.a  = Dr_total.a  + Dw_total.a;
   D_total.m1 = Dr_total.m1 + Dw_total.m1;
   D_total.m2 = Dr_total.m2 + Dw_total.m2;
       
        commify( D_total.a, l1, buf1);
   l2 = commify(Dr_total.a, 0,  buf2);
   l3 = commify(Dw_total.a, 0,  buf3);
   VG_(message)(Vg_UserMsg, "D   refs:      %s  (%s rd + %s wr)",
                buf1,  buf2,  buf3);

   commify( D_total.m1, l1, buf1);
   commify(Dr_total.m1, l2, buf2);
   commify(Dw_total.m1, l3, buf3);
   VG_(message)(Vg_UserMsg, "D1  misses:    %s  (%s rd + %s wr)",
                buf1, buf2, buf3);

   commify( D_total.m2, l1, buf1);
   commify(Dr_total.m2, l2, buf2);
   commify(Dw_total.m2, l3, buf3);
   VG_(message)(Vg_UserMsg, "L2d misses:    %s  (%s rd + %s wr)",
                buf1, buf2, buf3);

   p = 10;
   
   percentify( D_total.m1 * 100 * p / D_total.a,  p, l1+1, buf1);
   percentify(Dr_total.m1 * 100 * p / Dr_total.a, p, l2+1, buf2);
   percentify(Dw_total.m1 * 100 * p / Dw_total.a, p, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "D1  miss rate: %s (%s   + %s  )", buf1, buf2,buf3);

   percentify( D_total.m2 * 100 * p / D_total.a,  p, l1+1, buf1);
   percentify(Dr_total.m2 * 100 * p / Dr_total.a, p, l2+1, buf2);
   percentify(Dw_total.m2 * 100 * p / Dw_total.a, p, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "L2d miss rate: %s (%s   + %s  )", buf1, buf2,buf3);
   VG_(message)(Vg_UserMsg, "");

   /* L2 overall results */

   L2_total   = Dr_total.m1 + Dw_total.m1 + Ir_total.m1;
   L2_total_r = Dr_total.m1 + Ir_total.m1;
   L2_total_w = Dw_total.m1;
   commify(L2_total,   l1, buf1);
   commify(L2_total_r, l2, buf2);
   commify(L2_total_w, l3, buf3);
   VG_(message)(Vg_UserMsg, "L2 refs:       %s  (%s rd + %s wr)",
                buf1, buf2, buf3);

   L2_total_m  = Dr_total.m2 + Dw_total.m2 + Ir_total.m2;
   L2_total_mr = Dr_total.m2 + Ir_total.m2;
   L2_total_mw = Dw_total.m2;
   commify(L2_total_m,  l1, buf1);
   commify(L2_total_mr, l2, buf2);
   commify(L2_total_mw, l3, buf3);
   VG_(message)(Vg_UserMsg, "L2 misses:     %s  (%s rd + %s wr)",
                buf1, buf2, buf3);

   percentify(L2_total_m  * 100 * p / (Ir_total.a + D_total.a),  p, l1+1, buf1);
   percentify(L2_total_mr * 100 * p / (Ir_total.a + Dr_total.a), p, l2+1, buf2);
   percentify(L2_total_mw * 100 * p / Dw_total.a, p, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "L2 miss rate:  %s (%s   + %s  )", buf1, buf2,buf3);
            

   /* Hash table stats */
   if (VG_(clo_verbosity) > 1) {
       int BB_lookups = full_debug_BBs      + fn_name_debug_BBs +
                        file_line_debug_BBs + no_debug_BBs;
      
       VG_(message)(Vg_DebugMsg, "");
       VG_(message)(Vg_DebugMsg, "Distinct files:   %d", distinct_files);
       VG_(message)(Vg_DebugMsg, "Distinct fns:     %d", distinct_fns);
       VG_(message)(Vg_DebugMsg, "BB lookups:       %d", BB_lookups);
       VG_(message)(Vg_DebugMsg, "With full      debug info:%3d%% (%d)", 
                    full_debug_BBs    * 100 / BB_lookups,
                    full_debug_BBs);
       VG_(message)(Vg_DebugMsg, "With file/line debug info:%3d%% (%d)", 
                    file_line_debug_BBs * 100 / BB_lookups,
                    file_line_debug_BBs);
       VG_(message)(Vg_DebugMsg, "With fn name   debug info:%3d%% (%d)", 
                    fn_name_debug_BBs * 100 / BB_lookups,
                    fn_name_debug_BBs);
       VG_(message)(Vg_DebugMsg, "With no        debug info:%3d%% (%d)", 
                    no_debug_BBs      * 100 / BB_lookups,
                    no_debug_BBs);
       VG_(message)(Vg_DebugMsg, "BBs Retranslated: %d", BB_retranslations);
       VG_(message)(Vg_DebugMsg, "Distinct instrs:  %d", distinct_instrs);
   }
   VGP_POPCC;
}


/* Called when a translation is invalidated due to self-modifying code or
 * unloaded of a shared object.
 *
 * Finds the BBCC in the table, removes it, adds the counts to the discard
 * counters, and then frees the BBCC. */
void VG_(cachesim_notify_discard) ( TTEntry* tte )
{
   BBCC *BBCC_node;
   Addr BBCC_ptr0, BBCC_ptr;
   Bool BB_seen_before;
    
   if (0)
   VG_(printf)( "cachesim_notify_discard: %p for %d\n", 
                tte->orig_addr, (Int)tte->orig_size);

   /* 2nd arg won't be used since BB should have been seen before (assertions
    * ensure this). */
   BBCC_node = get_BBCC(tte->orig_addr, NULL, True, &BB_seen_before);
   BBCC_ptr0 = BBCC_ptr = (Addr)(BBCC_node->array);

   vg_assert(True == BB_seen_before);

   while (BBCC_ptr - BBCC_ptr0 < BBCC_node->array_size) {

      /* We pretend the CC is an iCC for getting the tag.  This is ok
       * because both CC types have tag as their first byte.  Once we know
       * the type, we can cast and act appropriately. */

      switch ( ((iCC*)BBCC_ptr)->tag ) {

         case INSTR_CC:
            ADD_CC_TO(iCC, I, Ir_discards);
            BBCC_ptr += sizeof(iCC);
            break;

         case READ_CC:
         case  MOD_CC:
            ADD_CC_TO(idCC, I, Ir_discards);
            ADD_CC_TO(idCC, D, Dr_discards);
            BBCC_ptr += sizeof(idCC);
            break;

         case WRITE_CC:
            ADD_CC_TO(idCC, I, Ir_discards);
            ADD_CC_TO(idCC, D, Dw_discards);
            BBCC_ptr += sizeof(idCC);
            break;

         default:
            VG_(panic)("Unknown CC type in VG_(cachesim_notify_discard)()\n");
            break;
      }
   }

   VG_(free)(VG_AR_PRIVATE, BBCC_node);
}

/*--------------------------------------------------------------------*/
/*--- end                                            vg_cachesim.c ---*/
/*--------------------------------------------------------------------*/
