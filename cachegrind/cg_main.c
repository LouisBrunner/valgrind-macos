
/*--------------------------------------------------------------------*/
/*--- Cachegrind: cache detection; instrumentation, recording and  ---*/
/*--- results printing.                                            ---*/
/*---                                                    cg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind skin for cache
   profiling programs.

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

   The GNU General Public License is contained in the file COPYING.
*/

#include "vg_skin.h"
//#include "vg_profile.c"

/* For cache simulation */
typedef struct {
    int size;       /* bytes */ 
    int assoc;
    int line_size;  /* bytes */ 
} cache_t;

#include "cg_sim_L2.c"
#include "cg_sim_I1.c"
#include "cg_sim_D1.c"

/*------------------------------------------------------------*/
/*--- Constants                                            ---*/
/*------------------------------------------------------------*/

/* According to IA-32 Intel Architecture Software Developer's Manual: Vol 2 */
#define MAX_x86_INSTR_SIZE              16

#define MIN_LINE_SIZE   16

/* Size of various buffers used for storing strings */
#define FILENAME_LEN                    256
#define FN_NAME_LEN                     256
#define BUF_LEN                         512
#define COMMIFY_BUF_LEN                 128
#define RESULTS_BUF_LEN                 128
#define LINE_BUF_LEN                     64

/*------------------------------------------------------------*/
/*--- Profiling events                                     ---*/
/*------------------------------------------------------------*/

typedef 
   enum { 
      VgpGetBBCC = VgpFini+1,
      VgpCacheSimulate,
      VgpCacheResults
   } 
   VgpSkinCC;

/*------------------------------------------------------------*/
/*--- Output file related stuff                            ---*/
/*------------------------------------------------------------*/

Char cachegrind_out_file[FILENAME_LEN];

static void file_err()
{
   VG_(message)(Vg_UserMsg,
                "error: can't open cache simulation output file `%s'",
                cachegrind_out_file );
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

typedef 
   enum {
      InstrCC,         /* eg. mov %eax,   %ebx                      */
      ReadCC,          /* eg. mov (%ecx), %esi                      */
      WriteCC,         /* eg. mov %eax,   (%edx)                    */
      ModCC,           /* eg. incl (%eax) (read+write one addr)     */
      ReadWriteCC,     /* eg. call*l (%esi), pushl 0x4(%ebx), movsw 
                               (read+write two different addrs)      */
   } CC_type;

/* Instruction-level cost-centres.  The typedefs for these structs are in
 * vg_include.c 
 *
 * WARNING:  the 'tag' field *must* be the first byte of both CC types.
 *
 * This is because we use it to work out what kind of CC we're dealing with.
 */ 
typedef
   struct {
      /* word 1 */
      UChar tag;
      UChar instr_size;
      /* 2 bytes padding */

      /* words 2+ */
      Addr instr_addr;
      CC I;
   }
   iCC;

typedef
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
   }
   idCC;

typedef
   struct _iddCC {
      /* word 1 */
      UChar tag;
      UChar instr_size;
      UChar data_size;
      /* 1 byte padding */

      /* words 2+ */
      Addr instr_addr;
      CC I;
      CC Da;
      CC Db;
   }
   iddCC;

static void init_iCC(iCC* cc, Addr instr_addr, UInt instr_size)
{
   cc->tag        = InstrCC;
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

static void init_iddCC(iddCC* cc, Addr instr_addr,
                       UInt instr_size, UInt data_size)
{
   cc->tag        = ReadWriteCC;
   cc->instr_size = instr_size;
   cc->data_size  = data_size;
   cc->instr_addr = instr_addr;
   initCC(&cc->I);
   initCC(&cc->Da);
   initCC(&cc->Db);
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

static __inline__ void sprint_read_write_CC(Char buf[BUF_LEN], iddCC* cc)
{
#if PRINT_INSTR_ADDRS
   VG_(sprintf)(buf, "%llu %llu %llu %llu %llu %llu # %x\n",
                      cc->I.a,  cc->I.m1,  cc->I.m2, 
                      cc->Da.a, cc->Da.m1, cc->Da.m2,
                      cc->Db.a, cc->Db.m1, cc->Db.m2, cc->instr_addr);
#else
   VG_(sprintf)(buf, "%llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
                      cc->I.a,  cc->I.m1,  cc->I.m2, 
                      cc->Da.a, cc->Da.m1, cc->Da.m2,
                      cc->Db.a, cc->Db.m1, cc->Db.m2);
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
   Bool found1, found2;

   found1 = VG_(get_filename_linenum)(instr_addr, filename,
                                      FILENAME_LEN, line_num);
   found2 = VG_(get_fnname)(instr_addr, fn_name, FN_NAME_LEN);

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
   file_node* new = VG_(malloc)(sizeof(file_node));
   new->filename  = VG_(strdup)(filename);
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
   fn_node* new = VG_(malloc)(sizeof(fn_node));
   new->fn_name = VG_(strdup)(fn_name);
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

   new = (BBCC*)VG_(malloc)(sizeof(BBCC) + BBCC_array_size);
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

   VGP_PUSHCC(VgpGetBBCC);
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

      sk_assert(False == remove);

      curr_fn_node->BBCCs[BBCC_hash] = curr_BBCC = 
         new_BBCC(bb_orig_addr, cb, curr_fn_node->BBCCs[BBCC_hash]);
      *BB_seen_before = False;

   } else {
      sk_assert(bb_orig_addr == curr_BBCC->orig_addr);
      sk_assert(curr_BBCC->array_size > 0 && curr_BBCC->array_size < 1000000);
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
   VGP_POPCC(VgpGetBBCC);
   return curr_BBCC;
}

/*------------------------------------------------------------*/
/*--- Cache simulation instrumentation phase               ---*/
/*------------------------------------------------------------*/

static Int compute_BBCC_array_size(UCodeBlock* cb)
{
   UInstr* u_in;
   Int     i, CC_size, BBCC_size = 0;
   Bool    is_LOAD, is_STORE, is_FPU_R, is_FPU_W;
   Int     t_read, t_write;
    
   is_LOAD = is_STORE = is_FPU_R = is_FPU_W = False;
   t_read = t_write = INVALID_TEMPREG;

   for (i = 0; i < cb->used; i++) {
      u_in = &cb->instrs[i];
      switch(u_in->opcode) {

         case INCEIP: 
            goto case_for_end_of_instr;
         
         case JMP:
            if (u_in->cond != CondAlways) break;

            goto case_for_end_of_instr;

            case_for_end_of_instr:

            if (((is_LOAD && is_STORE) || (is_FPU_R && is_FPU_W)) && 
                 t_read != t_write)
               CC_size = sizeof(iddCC);
            else if (is_LOAD || is_STORE || is_FPU_R || is_FPU_W)
               CC_size = sizeof(idCC);
            else
               CC_size = sizeof(iCC);

            BBCC_size += CC_size;
            is_LOAD = is_STORE = is_FPU_R = is_FPU_W = False;
            break;

         case LOAD:
            /* Two LDBs are possible for a single instruction */
            /* Also, a STORE can come after a LOAD for bts/btr/btc */
            sk_assert(/*!is_LOAD &&*/ /* !is_STORE && */ 
                      !is_FPU_R && !is_FPU_W);
            t_read = u_in->val1;
            is_LOAD = True;
            break;

         case STORE:
            /* Multiple STOREs are possible for 'pushal' */
            sk_assert(            /*!is_STORE &&*/ !is_FPU_R && !is_FPU_W);
            t_write = u_in->val2;
            is_STORE = True;
            break;

         case FPU_R:
            sk_assert(!is_LOAD && !is_STORE && !is_FPU_R && !is_FPU_W);
            t_read = u_in->val2;
            is_FPU_R = True;
            break;

         case FPU_W:
            sk_assert(!is_LOAD && !is_STORE && !is_FPU_R && !is_FPU_W);
            t_write = u_in->val2;
            is_FPU_W = True;
            break;

         default:
            break;
      }
   }

   return BBCC_size;
}

static __attribute__ ((regparm (1)))
void log_1I_0D_cache_access(iCC* cc)
{
   //VG_(printf)("1I_0D: CCaddr=0x%x, iaddr=0x%x, isize=%u\n",
   //            cc, cc->instr_addr, cc->instr_size)
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(cc->instr_addr, cc->instr_size, &cc->I.m1, &cc->I.m2);
   cc->I.a++;
   VGP_POPCC(VgpCacheSimulate);
}

/* Difference between this function and log_1I_0D_cache_access() is that
   this one can be passed any kind of CC, not just an iCC.  So we have to
   be careful to make sure we don't make any assumptions about CC layout.
   (As it stands, they would be safe, but this will avoid potential heartache
   if anyone else changes CC layout.)  
   Note that we only do the switch for the JIFZ version because if we always
   called this switching version, things would run about 5% slower. */
static __attribute__ ((regparm (1)))
void log_1I_0D_cache_access_JIFZ(iCC* cc)
{
   UChar instr_size;
   Addr instr_addr;
   CC* I;

   //VG_(printf)("1I_0D: CCaddr=0x%x, iaddr=0x%x, isize=%u\n",
   //            cc, cc->instr_addr, cc->instr_size)
   VGP_PUSHCC(VgpCacheSimulate);

   switch(cc->tag) {
       case InstrCC:
           instr_size = cc->instr_size;
           instr_addr = cc->instr_addr;
           I = &(cc->I);
           break;
       case ReadCC:
       case WriteCC:
       case ModCC:
           instr_size = ((idCC*)cc)->instr_size;
           instr_addr = ((idCC*)cc)->instr_addr;
           I = &( ((idCC*)cc)->I );
           break;
       case ReadWriteCC:
           instr_size = ((iddCC*)cc)->instr_size;
           instr_addr = ((iddCC*)cc)->instr_addr;
           I = &( ((iddCC*)cc)->I );
           break;
       default:
           VG_(skin_panic)("Unknown CC type in log_1I_0D_cache_access_JIFZ()\n");
           break;
   }
   cachesim_I1_doref(instr_addr, instr_size, &I->m1, &I->m2);
   I->a++;
   VGP_POPCC(VgpCacheSimulate);
}

__attribute__ ((regparm (2))) static 
void log_0I_1D_cache_access(idCC* cc, Addr data_addr)
{
   //VG_(printf)("0I_1D: CCaddr=%p, iaddr=%p, isize=%u, daddr=%p, dsize=%u\n",
   //            cc, cc->instr_addr, cc->instr_size, data_addr, cc->data_size)
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_D1_doref(data_addr,      cc->data_size,  &cc->D.m1, &cc->D.m2);
   cc->D.a++;
   VGP_POPCC(VgpCacheSimulate);
}

__attribute__ ((regparm (2))) static
void log_1I_1D_cache_access(idCC* cc, Addr data_addr)
{
   //VG_(printf)("1I_1D: CCaddr=%p, iaddr=%p, isize=%u, daddr=%p, dsize=%u\n",
   //            cc, cc->instr_addr, cc->instr_size, data_addr, cc->data_size)
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(cc->instr_addr, cc->instr_size, &cc->I.m1, &cc->I.m2);
   cc->I.a++;

   cachesim_D1_doref(data_addr,      cc->data_size,  &cc->D.m1, &cc->D.m2);
   cc->D.a++;
   VGP_POPCC(VgpCacheSimulate);
}

__attribute__ ((regparm (3))) static 
void log_0I_2D_cache_access(iddCC* cc, Addr data_addr1, Addr data_addr2)
{
   //VG_(printf)("0I_2D: CCaddr=%p, iaddr=%p, isize=%u, daddr1=0x%x, daddr2=%p, size=%u\n",
   //            cc, cc->instr_addr, cc->instr_size, data_addr1, data_addr2, cc->data_size)
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_D1_doref(data_addr1, cc->data_size,  &cc->Da.m1, &cc->Da.m2);
   cc->Da.a++;
   cachesim_D1_doref(data_addr2, cc->data_size,  &cc->Db.m1, &cc->Db.m2);
   cc->Db.a++;
   VGP_POPCC(VgpCacheSimulate);
}

__attribute__ ((regparm (3))) static
void log_1I_2D_cache_access(iddCC* cc, Addr data_addr1, Addr data_addr2)
{
   //VG_(printf)("1I_2D: CCaddr=%p, iaddr=%p, isize=%u, daddr1=%p, daddr2=%p, dsize=%u\n",
   //            cc, cc->instr_addr, cc->instr_size, data_addr1, data_addr2, cc->data_size)
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(cc->instr_addr, cc->instr_size, &cc->I.m1,  &cc->I.m2);
   cc->I.a++;

   cachesim_D1_doref(data_addr1,     cc->data_size,  &cc->Da.m1, &cc->Da.m2);
   cc->Da.a++;
   cachesim_D1_doref(data_addr2,     cc->data_size,  &cc->Db.m1, &cc->Db.m2);
   cc->Db.a++;
   VGP_POPCC(VgpCacheSimulate);
}

UCodeBlock* SK_(instrument)(UCodeBlock* cb_in, Addr orig_addr)
{
/* Use this rather than eg. -1 because it's a UInt. */
#define INVALID_DATA_SIZE   999999

   UCodeBlock* cb;
   Int         i;
   UInstr*     u_in;
   BBCC*       BBCC_node;
   Int         t_CC_addr, t_read_addr, t_write_addr, t_data_addr1,
               t_data_addr2, t_read, t_write;
   Int         CC_size = -1;    /* Shut gcc warnings up */
   Addr        x86_instr_addr = orig_addr;
   UInt        x86_instr_size, data_size = INVALID_DATA_SIZE;
   Addr        helper;
   Int         argc;
   UInt        stack_used;
   Bool        BB_seen_before     = False;
   Bool        instrumented_Jcond = False;
   Bool        has_rep_prefix     = False;
   Addr        BBCC_ptr0, BBCC_ptr; 

   /* Get BBCC (creating if necessary -- requires a counting pass over the BB
    * if it's the first time it's been seen), and point to start of the 
    * BBCC array.  */
   BBCC_node = get_BBCC(orig_addr, cb_in, /*remove=*/False, &BB_seen_before);
   BBCC_ptr0 = BBCC_ptr = (Addr)(BBCC_node->array);

   cb = VG_(alloc_UCodeBlock)();
   cb->nextTemp = cb_in->nextTemp;

   t_CC_addr = t_read_addr = t_write_addr = t_data_addr1 = t_data_addr2 =
               t_read = t_write = INVALID_TEMPREG;

   for (i = 0; i < cb_in->used; i++) {
      u_in = &cb_in->instrs[i];

      /* What this is all about:  we want to instrument each x86 instruction 
       * translation.  The end of these are marked in three ways.  The three
       * ways, and the way we instrument them, are as follows:
       *
       * 1. UCode, INCEIP         --> UCode, Instrumentation, INCEIP
       * 2. UCode, Juncond        --> UCode, Instrumentation, Juncond
       * 3. UCode, Jcond, Juncond --> UCode, Instrumentation, Jcond, Juncond
       *
       * The last UInstr in a basic block is always a Juncond.  Jconds,
       * when they appear, are always second last.  We check this with 
       * various assertions.
       *
       * We must put the instrumentation before any jumps so that it is always
       * executed.  We don't have to put the instrumentation before the INCEIP
       * (it could go after) but we do so for consistency.
       *
       * x86 instruction sizes are obtained from INCEIPs (for case 1) or
       * from .extra4b field of the final JMP (for case 2 & 3).
       *
       * Note that JIFZ is treated differently.
       *
       * The instrumentation is just a call to the appropriate helper function,
       * passing it the address of the instruction's CC.
       */
      if (instrumented_Jcond) sk_assert(u_in->opcode == JMP);

      switch (u_in->opcode) {
         case NOP:  case CALLM_E:  case CALLM_S:
            break;

         /* For memory-ref instrs, copy the data_addr into a temporary to be
          * passed to the cachesim_* helper at the end of the instruction.
          */
         case LOAD: 
            t_read      = u_in->val1;
            t_read_addr = newTemp(cb);
            uInstr2(cb, MOV, 4, TempReg, u_in->val1,  TempReg, t_read_addr);
            data_size = u_in->size;
            VG_(copy_UInstr)(cb, u_in);
            break;

         case FPU_R:
            t_read      = u_in->val2;
            t_read_addr = newTemp(cb);
            uInstr2(cb, MOV, 4, TempReg, u_in->val2,  TempReg, t_read_addr);
            data_size = ( u_in->size <= MIN_LINE_SIZE
                        ? u_in->size
                        : MIN_LINE_SIZE);
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Note that we must set t_write_addr even for mod instructions;
          * That's how the code above determines whether it does a write.
          * Without it, it would think a mod instruction is a read.
          * As for the MOV, if it's a mod instruction it's redundant, but it's
          * not expensive and mod instructions are rare anyway. */
         case STORE:
         case FPU_W:
            t_write      = u_in->val2;
            t_write_addr = newTemp(cb);
            uInstr2(cb, MOV, 4, TempReg, u_in->val2, TempReg, t_write_addr);
            /* 28 and 108 B data-sized instructions will be done
             * inaccurately but they're very rare and this avoids errors
             * from hitting more than two cache lines in the simulation. */
            data_size = ( u_in->size <= MIN_LINE_SIZE
                        ? u_in->size
                        : MIN_LINE_SIZE);
            VG_(copy_UInstr)(cb, u_in);
            break;


         /* For rep-prefixed instructions, log a single I-cache access
          * before the UCode loop that implements the repeated part, which
          * is where the multiple D-cache accesses are logged. */
         case JIFZ:
            has_rep_prefix = True;

            /* Setup 1st and only arg: CC addr */
            t_CC_addr = newTemp(cb);
            uInstr2(cb, MOV,  4, Literal, 0, TempReg, t_CC_addr);
            uLiteral(cb, BBCC_ptr);

            /* Call helper */
            uInstr1(cb, CCALL, 0, TempReg, t_CC_addr);
            uCCall(cb, (Addr) & log_1I_0D_cache_access_JIFZ, 1, 1, False);
            VG_(copy_UInstr)(cb, u_in);
            break;


         /* INCEIP: insert instrumentation */
         case INCEIP:
            x86_instr_size = u_in->val1;
            goto instrument_x86_instr;

         /* JMP: insert instrumentation if the first JMP */
         case JMP:
            if (instrumented_Jcond) {
               sk_assert(CondAlways == u_in->cond);
               sk_assert(i+1 == cb_in->used);
               VG_(copy_UInstr)(cb, u_in);
               instrumented_Jcond = False;    /* reset */
               break;
            }
            /* The first JMP... instrument. */
            if (CondAlways != u_in->cond) {
               sk_assert(i+2 == cb_in->used);
               instrumented_Jcond = True;
            } else {
               sk_assert(i+1 == cb_in->used);
            }

            /* Get x86 instr size from final JMP. */
            x86_instr_size = LAST_UINSTR(cb_in).extra4b;
            goto instrument_x86_instr;


            /* Code executed at the end of each x86 instruction. */
            instrument_x86_instr:
             
            /* Initialise the CC in the BBCC array appropriately if it
             * hasn't been initialised before.  Then call appropriate sim
             * function, passing it the CC address. */
            stack_used = 0;

            sk_assert(x86_instr_size >= 1 && 
                      x86_instr_size <= MAX_x86_INSTR_SIZE);

#define IS_(X)      (INVALID_TEMPREG != t_##X##_addr)

            if (!IS_(read) && !IS_(write)) {
               sk_assert(INVALID_DATA_SIZE == data_size);
               sk_assert(INVALID_TEMPREG == t_read_addr  && 
                         INVALID_TEMPREG == t_read       && 
                         INVALID_TEMPREG == t_write_addr &&
                         INVALID_TEMPREG == t_write);
               CC_size = sizeof(iCC);
               if (!BB_seen_before)
                   init_iCC((iCC*)BBCC_ptr, x86_instr_addr, x86_instr_size);
               helper = ( has_rep_prefix 
                        ? (Addr)0      /* no extra log needed */
                        : (Addr) & log_1I_0D_cache_access
                        );
               argc = 1;

            } else { 
               sk_assert(4 == data_size || 2  == data_size || 1 == data_size || 
                         8 == data_size || 10 == data_size ||
                         MIN_LINE_SIZE == data_size);
               
               if (IS_(read) && !IS_(write)) {
                  CC_size = sizeof(idCC);
                  /* If it uses 'rep', we've already logged the I-cache 
                   * access at the JIFZ UInstr (see JIFZ case below) so
                   * don't do it here */
                  helper = ( has_rep_prefix 
                           ? (Addr) & log_0I_1D_cache_access
                           : (Addr) & log_1I_1D_cache_access
                           );
                  argc = 2;
                  if (!BB_seen_before)
                     init_idCC(ReadCC, (idCC*)BBCC_ptr, x86_instr_addr,
                               x86_instr_size, data_size);
                  sk_assert(INVALID_TEMPREG != t_read_addr  && 
                            INVALID_TEMPREG != t_read       && 
                            INVALID_TEMPREG == t_write_addr &&
                            INVALID_TEMPREG == t_write);
                  t_data_addr1 = t_read_addr;

               } else if (!IS_(read) && IS_(write)) {
                  CC_size = sizeof(idCC);
                  helper = ( has_rep_prefix 
                           ? (Addr) & log_0I_1D_cache_access
                           : (Addr) & log_1I_1D_cache_access
                           );
                  argc = 2;
                  if (!BB_seen_before)
                     init_idCC(WriteCC, (idCC*)BBCC_ptr, x86_instr_addr,
                               x86_instr_size, data_size);
                  sk_assert(INVALID_TEMPREG == t_read_addr  && 
                            INVALID_TEMPREG == t_read       && 
                            INVALID_TEMPREG != t_write_addr &&
                            INVALID_TEMPREG != t_write);
                  t_data_addr1 = t_write_addr;

               } else {
                  sk_assert(IS_(read) && IS_(write));
                  sk_assert(INVALID_TEMPREG != t_read_addr  && 
                            INVALID_TEMPREG != t_read       && 
                            INVALID_TEMPREG != t_write_addr &&
                            INVALID_TEMPREG != t_write);
                  if (t_read == t_write) {
                     CC_size = sizeof(idCC);
                     helper = ( has_rep_prefix 
                              ? (Addr) & log_0I_1D_cache_access
                              : (Addr) & log_1I_1D_cache_access
                              );
                     argc = 2;
                     if (!BB_seen_before)
                        init_idCC(ModCC, (idCC*)BBCC_ptr, x86_instr_addr,
                                  x86_instr_size, data_size);
                     t_data_addr1 = t_read_addr;
                  } else {
                     CC_size = sizeof(iddCC);
                     helper = ( has_rep_prefix 
                              ? (Addr) & log_0I_2D_cache_access
                              : (Addr) & log_1I_2D_cache_access
                              );
                     argc = 3;
                     if (!BB_seen_before)
                        init_iddCC((iddCC*)BBCC_ptr, x86_instr_addr,
                                    x86_instr_size, data_size);
                     t_data_addr1 = t_read_addr;
                     t_data_addr2 = t_write_addr;
                  }
               }
#undef IS_
            }

            /* Call the helper, if necessary */
            if ((Addr)0 != helper) {

               /* Setup 1st arg: CC addr */
               t_CC_addr = newTemp(cb);
               uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_CC_addr);
               uLiteral(cb, BBCC_ptr);

               /* Call the helper */
               if      (1 == argc)
                  uInstr1(cb, CCALL, 0, TempReg, t_CC_addr);
               else if (2 == argc)
                  uInstr2(cb, CCALL, 0, TempReg, t_CC_addr, 
                                        TempReg, t_data_addr1);
               else if (3 == argc)
                  uInstr3(cb, CCALL, 0, TempReg, t_CC_addr, 
                                        TempReg, t_data_addr1,
                                        TempReg, t_data_addr2);
               else
                  VG_(skin_panic)("argc... not 1 or 2 or 3?");
               
               uCCall(cb, helper, argc, argc, False);
            }

            /* Copy original UInstr (INCEIP or JMP) */
            VG_(copy_UInstr)(cb, u_in);

            /* Update BBCC_ptr, EIP, de-init read/write temps for next instr */
            BBCC_ptr       += CC_size; 
            x86_instr_addr += x86_instr_size;
            t_CC_addr = t_read_addr = t_write_addr = t_data_addr1 = 
                        t_data_addr2 = t_read = t_write = INVALID_TEMPREG;
            data_size = INVALID_DATA_SIZE;
            has_rep_prefix = False; 
            break;

         default:
            VG_(copy_UInstr)(cb, u_in);
            break;
      }
   }

   /* Just check everything looks ok */
   sk_assert(BBCC_ptr - BBCC_ptr0 == BBCC_node->array_size);

   VG_(free_UCodeBlock)(cb_in);
   return cb;

#undef INVALID_DATA_SIZE
}

/*------------------------------------------------------------*/
/*--- Automagic cache initialisation stuff                 ---*/
/*------------------------------------------------------------*/

/* Total reads/writes/misses.  Calculated during CC traversal at the end. */
static CC Ir_total;
static CC Dr_total;
static CC Dw_total;

#define UNDEFINED_CACHE     ((cache_t) { -1, -1, -1 }) 

static cache_t clo_I1_cache = UNDEFINED_CACHE;
static cache_t clo_D1_cache = UNDEFINED_CACHE;
static cache_t clo_L2_cache = UNDEFINED_CACHE;

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
       "warning: Pentium with %d K micro-op instruction trace cache", 
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
   Bool  L2_found = False;

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
          
      /* TLB info, ignore */
      case 0x01: case 0x02: case 0x03: case 0x04:
      case 0x50: case 0x51: case 0x52: case 0x5b: case 0x5c: case 0x5d:
          break;      

      case 0x06: *I1c = (cache_t) {  8, 4, 32 }; break;
      case 0x08: *I1c = (cache_t) { 16, 4, 32 }; break;

      case 0x0a: *D1c = (cache_t) {  8, 2, 32 }; break;
      case 0x0c: *D1c = (cache_t) { 16, 4, 32 }; break;

      /* IA-64 info -- panic! */
      case 0x10: case 0x15: case 0x1a: 
      case 0x88: case 0x89: case 0x8a: case 0x8d:
      case 0x90: case 0x96: case 0x9b:
         VG_(message)(Vg_DebugMsg,
            "error: IA-64 cache stats!  Cachegrind doesn't run on IA-64...");
         VG_(skin_panic)("IA-64 detected");

      case 0x22: case 0x23: case 0x25: case 0x29: 
          VG_(message)(Vg_DebugMsg, 
             "warning: L3 cache detected but ignored\n");
          break;

      /* These are sectored, whatever that means */
      case 0x39: *L2c = (cache_t) {  128, 4, 64 }; L2_found = True; break;
      case 0x3c: *L2c = (cache_t) {  256, 4, 64 }; L2_found = True; break;

      /* If a P6 core, this means "no L2 cache".  
         If a P4 core, this means "no L3 cache".
         We don't know what core it is, so don't issue a warning.  To detect
         a missing L2 cache, we use 'L2_found'. */
      case 0x40:
          break;

      case 0x41: *L2c = (cache_t) {  128, 4, 32 }; L2_found = True; break;
      case 0x42: *L2c = (cache_t) {  256, 4, 32 }; L2_found = True; break;
      case 0x43: *L2c = (cache_t) {  512, 4, 32 }; L2_found = True; break;
      case 0x44: *L2c = (cache_t) { 1024, 4, 32 }; L2_found = True; break;
      case 0x45: *L2c = (cache_t) { 2048, 4, 32 }; L2_found = True; break;

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

      /* These are sectored, whatever that means */
      case 0x79: *L2c = (cache_t) {  128, 8,  64 }; L2_found = True;  break;
      case 0x7a: *L2c = (cache_t) {  256, 8,  64 }; L2_found = True;  break;
      case 0x7b: *L2c = (cache_t) {  512, 8,  64 }; L2_found = True;  break;
      case 0x7c: *L2c = (cache_t) { 1024, 8,  64 }; L2_found = True;  break;
      case 0x7e: *L2c = (cache_t) {  256, 8, 128 }; L2_found = True;  break;

      case 0x81: *L2c = (cache_t) {  128, 8, 32 };  L2_found = True;  break;
      case 0x82: *L2c = (cache_t) {  256, 8, 32 };  L2_found = True;  break;
      case 0x83: *L2c = (cache_t) {  512, 8, 32 };  L2_found = True;  break;
      case 0x84: *L2c = (cache_t) { 1024, 8, 32 };  L2_found = True;  break;
      case 0x85: *L2c = (cache_t) { 2048, 8, 32 };  L2_found = True;  break;

      default:
          VG_(message)(Vg_DebugMsg, 
             "warning: Unknown Intel cache config value "
             "(0x%x), ignoring", info[i]);
          break;
      }
   }

   if (!L2_found)
      VG_(message)(Vg_DebugMsg, 
         "warning: L2 cache not installed, ignore L2 results.");

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
 * Also, according to Cyrille Chepelov, Duron stepping A0 processors (model
 * 0x630) have a bug and misreport their L2 size as 1KB (it's really 64KB),
 * so we detect that.
 * 
 * Returns 0 on success, non-zero on failure.
 */
static
Int AMD_cache_info(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   Int dummy, model, ext_level;
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

   cpuid(0x1, &model, &dummy, &dummy, &dummy);
   /*VG_(message)(Vg_UserMsg,"CPU model %04x",model);*/

   /* Check for Duron bug */
   if (model == 0x630) {
      VG_(message)(Vg_UserMsg,
         "Buggy Duron stepping A0. Assuming L2 size=65536 bytes");
      L2i = (64 << 16) | (L2i & 0xffff);
   }

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
   sk_assert(res == 0);

   res = VG_(ksigaction)( VKI_SIGILL, &sigill_new, &sigill_saved );
   sk_assert(res == 0);

   /* Trap for illegal instruction, in case it's a really old processor that
    * doesn't support CPUID. */
   if (__builtin_setjmp(cpuid_jmpbuf) == 0) {
      cpuid(0, &level, (int*)&vendor_id[0], 
                       (int*)&vendor_id[8], (int*)&vendor_id[4]);    
      vendor_id[12] = '\0';

      /* Restore old SIGILL handler */
      res = VG_(ksigaction)( VKI_SIGILL, &sigill_saved, NULL );
      sk_assert(res == 0);

   } else  {
      VG_(message)(Vg_DebugMsg, "CPUID instruction not supported");

      /* Restore old SIGILL handler */
      res = VG_(ksigaction)( VKI_SIGILL, &sigill_saved, NULL );
      sk_assert(res == 0);
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

#define CMD_LINE_DEFINED(L)            \
   (-1 != clo_##L##_cache.size  ||     \
    -1 != clo_##L##_cache.assoc ||     \
    -1 != clo_##L##_cache.line_size)

   *I1c = clo_I1_cache;
   *D1c = clo_D1_cache;
   *L2c = clo_L2_cache;

   /* If any undefined on command-line, try CPUID */
   if (! CMD_LINE_DEFINED(I1) ||
       ! CMD_LINE_DEFINED(D1) ||
       ! CMD_LINE_DEFINED(L2)) { 

      /* Overwrite CPUID result for any cache defined on command-line */
      if (0 == get_caches_from_CPUID(I1c, D1c, L2c)) {
   
         if (CMD_LINE_DEFINED(I1)) *I1c = clo_I1_cache;
         if (CMD_LINE_DEFINED(D1)) *D1c = clo_D1_cache;
         if (CMD_LINE_DEFINED(L2)) *L2c = clo_L2_cache;

      /* CPUID failed, use defaults for each undefined by command-line */
      } else {
         VG_(message)(Vg_DebugMsg, 
                      "Couldn't detect cache configuration, using one "
                      "or more defaults ");

         *I1c = (CMD_LINE_DEFINED(I1) ? clo_I1_cache : I1_dflt);
         *D1c = (CMD_LINE_DEFINED(D1) ? clo_D1_cache : D1_dflt);
         *L2c = (CMD_LINE_DEFINED(L2) ? clo_L2_cache : L2_dflt);
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

/*------------------------------------------------------------*/
/*--- SK_(fini)() and related function                     ---*/
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

         case InstrCC:
            instr_addr = ((iCC*)BBCC_ptr)->instr_addr;
            sprint_iCC(buf, (iCC*)BBCC_ptr);
            ADD_CC_TO(iCC, I, Ir_total);
            BBCC_ptr += sizeof(iCC);
            break;

         case ReadCC:
         case  ModCC:
            instr_addr = ((idCC*)BBCC_ptr)->instr_addr;
            sprint_read_or_mod_CC(buf, (idCC*)BBCC_ptr);
            ADD_CC_TO(idCC, I, Ir_total);
            ADD_CC_TO(idCC, D, Dr_total);
            BBCC_ptr += sizeof(idCC);
            break;

         case WriteCC:
            instr_addr = ((idCC*)BBCC_ptr)->instr_addr;
            sprint_write_CC(buf, (idCC*)BBCC_ptr);
            ADD_CC_TO(idCC, I, Ir_total);
            ADD_CC_TO(idCC, D, Dw_total);
            BBCC_ptr += sizeof(idCC);
            break;

         case ReadWriteCC:
            instr_addr = ((iddCC*)BBCC_ptr)->instr_addr;
            sprint_read_write_CC(buf, (iddCC*)BBCC_ptr);
            ADD_CC_TO(iddCC, I,  Ir_total);
            ADD_CC_TO(iddCC, Da, Dr_total);
            ADD_CC_TO(iddCC, Db, Dw_total);
            BBCC_ptr += sizeof(iddCC);
            break;

         default:
            VG_(skin_panic)("Unknown CC type in fprint_BBCC()\n");
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
      if (VG_(clo_verbosity > 2) && 0 != VG_(strcmp)(fn_buf, first_instr_fn)) {
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

   sk_assert(BBCC_ptr - BBCC_ptr0 == BBCC_node->array_size);
}

static void fprint_BBCC_table_and_calc_totals(void)
{
   Int        fd;
   Char       buf[BUF_LEN];
   file_node *curr_file_node;
   fn_node   *curr_fn_node;
   BBCC      *curr_BBCC;
   Int        i,j,k;

   VGP_PUSHCC(VgpCacheResults);
   fd = VG_(open)(cachegrind_out_file, VKI_O_WRONLY|VKI_O_TRUNC, 0);
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
   for (i = 0; i < VG_(client_argc); i++) {
       VG_(sprintf)(buf, " %s", VG_(client_argv)[i]);
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
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}

void SK_(fini)(void)
{
   CC D_total;
   ULong L2_total_m, L2_total_mr, L2_total_mw,
         L2_total, L2_total_r, L2_total_w;
   char buf1[RESULTS_BUF_LEN], 
        buf2[RESULTS_BUF_LEN], 
        buf3[RESULTS_BUF_LEN];
   Int l1, l2, l3;
   Int p;

   fprint_BBCC_table_and_calc_totals();

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

   if (0 == Ir_total.a) Ir_total.a = 1;
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
   
   if (0 == D_total.a)   D_total.a = 1;
   if (0 == Dr_total.a) Dr_total.a = 1;
   if (0 == Dw_total.a) Dw_total.a = 1;
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
   VGP_POPCC(VgpCacheResults);
}


/* Called when a translation is invalidated due to self-modifying code or
 * unloaded of a shared object.
 *
 * Finds the BBCC in the table, removes it, adds the counts to the discard
 * counters, and then frees the BBCC. */
void SK_(discard_basic_block_info) ( Addr a, UInt size )
{
   BBCC *BBCC_node;
   Addr BBCC_ptr0, BBCC_ptr;
   Bool BB_seen_before;
    
   if (0)
      VG_(printf)( "discard_basic_block_info: addr %p, size %u\n", a, size);

   /* 2nd arg won't be used since BB should have been seen before (assertions
    * ensure this). */
   BBCC_node = get_BBCC(a, NULL, /*remove=*/True, &BB_seen_before);
   BBCC_ptr0 = BBCC_ptr = (Addr)(BBCC_node->array);

   sk_assert(True == BB_seen_before);

   while (BBCC_ptr - BBCC_ptr0 < BBCC_node->array_size) {

      /* We pretend the CC is an iCC for getting the tag.  This is ok
       * because both CC types have tag as their first byte.  Once we know
       * the type, we can cast and act appropriately. */

      switch ( ((iCC*)BBCC_ptr)->tag ) {

         case InstrCC:
            ADD_CC_TO(iCC, I, Ir_discards);
            BBCC_ptr += sizeof(iCC);
            break;

         case ReadCC:
         case  ModCC:
            ADD_CC_TO(idCC, I, Ir_discards);
            ADD_CC_TO(idCC, D, Dr_discards);
            BBCC_ptr += sizeof(idCC);
            break;

         case WriteCC:
            ADD_CC_TO(idCC, I, Ir_discards);
            ADD_CC_TO(idCC, D, Dw_discards);
            BBCC_ptr += sizeof(idCC);
            break;

         case ReadWriteCC:
            ADD_CC_TO(iddCC, I, Ir_discards);
            ADD_CC_TO(iddCC, Da, Dr_discards);
            ADD_CC_TO(iddCC, Db, Dw_discards);
            BBCC_ptr += sizeof(iddCC);
            break;

         default:
            VG_(skin_panic)("Unknown CC type in VG_(discard_basic_block_info)()\n");
            break;
      }
   }
   VG_(free)(BBCC_node);
}

/*--------------------------------------------------------------------*/
/*--- Command line processing                                      ---*/
/*--------------------------------------------------------------------*/

static void parse_cache_opt ( cache_t* cache, char* orig_opt, int opt_len )
{
   int   i1, i2, i3;
   int   i;
   char *opt = VG_(strdup)(orig_opt);

   i = i1 = opt_len;

   /* Option looks like "--I1=65536,2,64".
    * Find commas, replace with NULs to make three independent 
    * strings, then extract numbers.  Yuck. */
   while (VG_(isdigit)(opt[i])) i++;
   if (',' == opt[i]) {
      opt[i++] = '\0';
      i2 = i;
   } else goto bad;
   while (VG_(isdigit)(opt[i])) i++;
   if (',' == opt[i]) {
      opt[i++] = '\0';
      i3 = i;
   } else goto bad;
   while (VG_(isdigit)(opt[i])) i++;
   if ('\0' != opt[i]) goto bad;

   cache->size      = (Int)VG_(atoll)(opt + i1);
   cache->assoc     = (Int)VG_(atoll)(opt + i2);
   cache->line_size = (Int)VG_(atoll)(opt + i3);

   VG_(free)(opt);

   return;

  bad:
   VG_(bad_option)(orig_opt);
}

Bool SK_(process_cmd_line_option)(Char* arg)
{
   /* 5 is length of "--I1=" */
   if      (0 == VG_(strncmp)(arg, "--I1=", 5))
      parse_cache_opt(&clo_I1_cache, arg,   5);
   else if (0 == VG_(strncmp)(arg, "--D1=", 5))
      parse_cache_opt(&clo_D1_cache, arg,   5);
   else if (0 == VG_(strncmp)(arg, "--L2=", 5))
      parse_cache_opt(&clo_L2_cache, arg,   5);
   else
      return False;

   return True;
}

Char* SK_(usage)(void)
{
   return 
"    --I1=<size>,<assoc>,<line_size>  set I1 cache manually\n"
"    --D1=<size>,<assoc>,<line_size>  set D1 cache manually\n"
"    --L2=<size>,<assoc>,<line_size>  set L2 cache manually\n";
}

/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

void SK_(pre_clo_init)(VgDetails* details, VgNeeds* needs,
                       VgTrackEvents* not_used)
{
   details->name             = "Cachegrind";
   details->version          = NULL;
   details->description      = "an I1/D1/L2 cache profiler";
   details->copyright_author =
      "Copyright (C) 2002, and GNU GPL'd, by Nicholas Nethercote.";
   details->bug_reports_to   = "njn25@cam.ac.uk";

   needs->basic_block_discards = True;
   needs->command_line_options = True;

   VG_(register_compact_helper)((Addr) & log_1I_0D_cache_access);
   VG_(register_compact_helper)((Addr) & log_1I_0D_cache_access_JIFZ);
   VG_(register_compact_helper)((Addr) & log_0I_1D_cache_access);
   VG_(register_compact_helper)((Addr) & log_1I_1D_cache_access);
   VG_(register_compact_helper)((Addr) & log_0I_2D_cache_access);
   VG_(register_compact_helper)((Addr) & log_1I_2D_cache_access);
}

void SK_(post_clo_init)(void)
{
   cache_t I1c, D1c, L2c; 
   Int fd;

   /* Set output file name: cachegrind.<pid>.out */
   VG_(sprintf)(cachegrind_out_file, "cachegrind.out.%d", VG_(getpid)());

   /* Make sure the output file can be written. */
   fd = VG_(open)(cachegrind_out_file, VKI_O_WRONLY|VKI_O_TRUNC, 0);
   if (-1 == fd) { 
      fd = VG_(open)(cachegrind_out_file, VKI_O_CREAT|VKI_O_WRONLY,
                                          VKI_S_IRUSR|VKI_S_IWUSR);
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
   cachesim_D1_initcache(D1c);
   cachesim_L2_initcache(L2c);

   VGP_(register_profile_event)(VgpGetBBCC,       "get-BBCC");
   VGP_(register_profile_event)(VgpCacheSimulate, "cache-simulate");
   VGP_(register_profile_event)(VgpCacheResults,  "cache-results");
   
   init_BBCC_table();
}

#if 0
Bool SK_(cheap_sanity_check)(void) { return True; }

extern TTEntry* vg_tt;

Bool SK_(expensive_sanity_check)(void)
{ 
   Int i;
   Bool dummy;
   for (i = 0; i < 200191; i++) {
      if (vg_tt[i].orig_addr != (Addr)1 &&
          vg_tt[i].orig_addr != (Addr)3) {
         VG_(printf)(".");
         get_BBCC(vg_tt[i].orig_addr, NULL, /*remove=*/True, &dummy);
      }
   }
   return True;
}
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                cg_main.c ---*/
/*--------------------------------------------------------------------*/
