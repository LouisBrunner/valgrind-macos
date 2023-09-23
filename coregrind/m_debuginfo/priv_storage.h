
/*--------------------------------------------------------------------*/
/*--- Format-neutral storage of and querying of info acquired from ---*/
/*--- ELF/XCOFF stabs/dwarf1/dwarf2 debug info.                    ---*/
/*---                                               priv_storage.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/
/*
   Stabs reader greatly improved by Nick Nethercote, Apr 02.
   This module was also extensively hacked on by Jeremy Fitzhardinge
   and Tom Hughes.
*/
/* See comment at top of debuginfo.c for explanation of
   the _svma / _avma / _image / _bias naming scheme.
*/
/* Note this is not freestanding; needs pub_core_xarray.h and
   priv_tytypes.h to be included before it. */

#ifndef __PRIV_STORAGE_H
#define __PRIV_STORAGE_H

#include "pub_core_basics.h"   // Addr
#include "pub_core_xarray.h"   // XArray
#include "pub_core_deduppoolalloc.h" // DedupPoolAlloc
#include "priv_d3basics.h"     // GExpr et al.
#include "priv_image.h"        // DiCursor

/* --------------------- SYMBOLS --------------------- */

/* A structure to hold an ELF/MachO symbol (very crudely).  Usually
   the symbol only has one name, which is stored in ::pri_name, and
   ::sec_names is NULL.  If there are other names, these are stored in
   ::sec_names, which is a NULL terminated vector holding the names.
   The vector is allocated in VG_AR_DINFO, the names themselves live
   in DebugInfo::strpool.

   From the point of view of ELF, the primary vs secondary distinction
   is artificial: they are all just names associated with the address,
   none of which has higher precedence than any other.  However, from
   the point of view of mapping an address to a name to display to the
   user, we need to choose one "preferred" name, and so that might as
   well be installed as the pri_name, whilst all others can live in
   sec_names[].  This has the convenient side effect that, in the
   common case where there is only one name for the address,
   sec_names[] does not need to be allocated.
*/
typedef 
   struct {
      SymAVMAs avmas;    /* Symbol Actual VMAs: lowest address of entity,
                            + platform specific fields, to access with
                            the macros defined in pub_core_debuginfo.h */
      const HChar*  pri_name;  /* primary name, never NULL */
      const HChar** sec_names; /* NULL, or a NULL term'd array of other names */
      // XXX: DiSym could be shrunk (on 32-bit platforms to exactly 16
      // bytes, on 64-bit platforms the first 3 pointers already add
      // up to 24 bytes, so size plus bits will extend to 32 bytes
      // anyway) by using 29 bits for the size and 1 bit each for
      // isText, isIFunc and isGlobal.  If you do this, make sure that
      // all assignments to the latter two use 0 or 1 (or True or
      // False), and that a positive number larger than 1 is never
      // used to represent True.
      UInt    size;    /* size in bytes */
      Bool    isText;
      Bool    isIFunc; /* symbol is an indirect function? */
      Bool    isGlobal; /* Is this symbol globally visible? */
   }
   DiSym;

/* --------------------- SRCLOCS --------------------- */

/* Line count at which overflow happens, due to line numbers being
   stored as shorts in `struct nlist' in a.out.h. */
#define LINENO_OVERFLOW (1 << (sizeof(short) * 8))

#define LINENO_BITS     20
#define LOC_SIZE_BITS  (32 - LINENO_BITS)
#define MAX_LINENO     ((1 << LINENO_BITS) - 1)

/* Unlikely to have any lines with instruction ranges > 4096 bytes */
#define MAX_LOC_SIZE   ((1 << LOC_SIZE_BITS) - 1)

/* Number used to detect line number overflows; if one line is
   60000-odd smaller than the previous, it was probably an overflow.
 */
#define OVERFLOW_DIFFERENCE     (LINENO_OVERFLOW - 5000)

/* Filename and Dirname pair. FnDn are stored in di->fndnpool
   and are allocated using VG_(allocFixedEltDedupPA).
   The filename/dirname strings are themselves stored in di->strpool. */
typedef
   struct {
      const HChar* filename;     /* source filename */
      const HChar* dirname;      /* source directory name */
   } FnDn;

/* A structure to hold addr-to-source info for a single line.  There
  can be a lot of these, hence the dense packing. */
typedef
   struct {
      /* Word 1 */
      Addr   addr;               /* lowest address for this line */
      /* Word 2 */
      UShort size:LOC_SIZE_BITS; /* # bytes; we catch overflows of this */
      UInt   lineno:LINENO_BITS; /* source line number, or zero */
   }
   DiLoc;

#define LEVEL_BITS  (32 - LINENO_BITS)
#define MAX_LEVEL     ((1 << LEVEL_BITS) - 1)

/* A structure to hold addr-to-inlined fn info.  There
   can be a lot of these, hence the dense packing.
   Only caller source filename and lineno are stored.
   Handling dirname should be done using fndn_ix technique
   similar to  ML_(addLineInfo). */
typedef
   struct {
      /* Word 1 */
      Addr   addr_lo;            /* lowest address for inlined fn */
      /* Word 2 */
      Addr   addr_hi;            /* highest address following the inlined fn */
      /* Word 3 */
      const HChar* inlinedfn;    /* inlined function name */
      /* Word 4 and 5 */
      UInt   fndn_ix;            /* index in di->fndnpool of caller source
                                    dirname/filename */
      UInt   lineno:LINENO_BITS; /* caller line number */
      UShort level:LEVEL_BITS;   /* level of inlining */
   }
   DiInlLoc;

/* --------------------- CF INFO --------------------- */

/* DiCfSI: a structure to summarise DWARF2/3 CFA info for the code
   address range [base .. base+len-1].

   On x86 and amd64 ("IA"), if you know ({e,r}sp, {e,r}bp, {e,r}ip) at
   some point and {e,r}ip is in the range [base .. base+len-1], it
   tells you how to calculate ({e,r}sp, {e,r}bp) for the caller of the
   current frame and also ra, the return address of the current frame.

   First off, calculate CFA, the Canonical Frame Address, thusly:

     cfa = case cfa_how of
              CFIC_IA_SPREL -> {e,r}sp + cfa_off
              CFIC_IA_BPREL -> {e,r}bp + cfa_off
              CFIC_EXPR     -> expr whose index is in cfa_off

   Once that is done, the previous frame's {e,r}sp/{e,r}bp values and
   this frame's {e,r}ra value can be calculated like this:

     old_{e,r}sp/{e,r}bp/ra
         = case {e,r}sp/{e,r}bp/ra_how of
              CFIR_UNKNOWN   -> we don't know, sorry
              CFIR_SAME      -> same as it was before (sp/fp only)
              CFIR_CFAREL    -> cfa + sp/bp/ra_off
              CFIR_MEMCFAREL -> *( cfa + sp/bp/ra_off )
              CFIR_EXPR      -> expr whose index is in sp/bp/ra_off

   On ARM it's pretty much the same, except we have more registers to
   keep track of:

     cfa = case cfa_how of
              CFIC_ARM_R13REL -> r13 + cfa_off
              CFIC_ARM_R12REL -> r12 + cfa_off
              CFIC_ARM_R11REL -> r11 + cfa_off
              CFIC_ARM_R7REL  -> r7  + cfa_off
              CFIR_EXPR       -> expr whose index is in cfa_off

     old_r14/r13/r12/r11/r7/ra
         = case r14/r13/r12/r11/r7/ra_how of
              CFIR_UNKNOWN   -> we don't know, sorry
              CFIR_SAME      -> same as it was before (r14/r13/r12/r11/r7 only)
              CFIR_CFAREL    -> cfa + r14/r13/r12/r11/r7/ra_off
              CFIR_MEMCFAREL -> *( cfa + r14/r13/r12/r11/r7/ra_off )
              CFIR_EXPR      -> expr whose index is in r14/r13/r12/r11/r7/ra_off

   On ARM64:

     cfa = case cfa_how of
              CFIC_ARM64_SPREL  -> sp + cfa_off
              CFIC_ARM64_X29REL -> x29 + cfa_off
              CFIC_EXPR         -> expr whose index is in cfa_off

     old_sp/x30/x29/ra
         = case sp/x30/x29/ra_how of
              CFIR_UNKNOWN   -> we don't know, sorry
              CFIR_SAME      -> same as it was before
              CFIR_CFAREL    -> cfa + sp/x30/x29/ra_how
              CFIR_MEMCFAREL -> *( cfa + sp/x30/x29/ra_how )
              CFIR_EXPR      -> expr whose index is in sp/x30/x29/ra_off

   On s390x we have a similar logic as x86 or amd64. We need the stack pointer
   (r15), the frame pointer r11 (like BP) and together with the instruction
   address in the PSW we can calculate the previous values:
     cfa = case cfa_how of
              CFIC_IA_SPREL -> r15 + cfa_off
              CFIC_IA_BPREL -> r11 + cfa_off
              CFIC_EXPR     -> expr whose index is in cfa_off

     old_sp/fp/ra
         = case sp/fp/ra_how of
              CFIR_UNKNOWN   -> we don't know, sorry
              CFIR_SAME      -> same as it was before (sp/fp only)
              CFIR_CFAREL    -> cfa + sp/fp/ra_off
              CFIR_MEMCFAREL -> *( cfa + sp/fp/ra_off )
              CFIR_EXPR      -> expr whose index is in sp/fp/ra_off
              CFIR_S390X_F0  -> old value of %f0
              CFIR_S390X_F1  -> old value of %f1
              CFIR_S390X_F2  -> old value of %f2
              CFIR_S390X_F3  -> old value of %f3
              CFIR_S390X_F4  -> old value of %f4
              CFIR_S390X_F5  -> old value of %f5
              CFIR_S390X_F6  -> old value of %f6
              CFIR_S390X_F7  -> old value of %f7
*/

#define CFIC_IA_SPREL     ((UChar)1)
#define CFIC_IA_BPREL     ((UChar)2)
#define CFIC_ARM_R13REL   ((UChar)3)
#define CFIC_ARM_R12REL   ((UChar)4)
#define CFIC_ARM_R11REL   ((UChar)5)
#define CFIC_ARM_R7REL    ((UChar)6)
#define CFIC_ARM64_SPREL  ((UChar)7)
#define CFIC_ARM64_X29REL ((UChar)8)
#define CFIC_EXPR         ((UChar)9)  /* all targets */

#define CFIR_UNKNOWN      ((UChar)64)
#define CFIR_SAME         ((UChar)65)
#define CFIR_CFAREL       ((UChar)66)
#define CFIR_MEMCFAREL    ((UChar)67)
#define CFIR_EXPR         ((UChar)68)
#define CFIR_S390X_F0     ((UChar)69)
#define CFIR_S390X_F1     ((UChar)70)
#define CFIR_S390X_F2     ((UChar)71)
#define CFIR_S390X_F3     ((UChar)72)
#define CFIR_S390X_F4     ((UChar)73)
#define CFIR_S390X_F5     ((UChar)74)
#define CFIR_S390X_F6     ((UChar)75)
#define CFIR_S390X_F7     ((UChar)76)

/* Definition of the DiCfSI_m DiCfSI machine dependent part.
   These are highly duplicated, and are stored in a pool. */
#if defined(VGA_x86) || defined(VGA_amd64)
typedef
   struct {
      UChar cfa_how; /* a CFIC_IA value */
      UChar ra_how;  /* a CFIR_ value */
      UChar sp_how;  /* a CFIR_ value */
      UChar bp_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   ra_off;
      Int   sp_off;
      Int   bp_off;
   }
   DiCfSI_m;
#elif defined(VGA_arm)
typedef
   struct {
      UChar cfa_how; /* a CFIC_ value */
      UChar ra_how;  /* a CFIR_ value */
      UChar r14_how; /* a CFIR_ value */
      UChar r13_how; /* a CFIR_ value */
      UChar r12_how; /* a CFIR_ value */
      UChar r11_how; /* a CFIR_ value */
      UChar r7_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   ra_off;
      Int   r14_off;
      Int   r13_off;
      Int   r12_off;
      Int   r11_off;
      Int   r7_off;
      // If you add additional fields, don't forget to update the
      // initialisation of this in readexidx.c accordingly.
   }
   DiCfSI_m;
#elif defined(VGA_arm64)
typedef
   struct {
      UChar cfa_how; /* a CFIC_ value */
      UChar ra_how;  /* a CFIR_ value */
      UChar sp_how;  /* a CFIR_ value */ /*dw31=SP*/
      UChar x30_how; /* a CFIR_ value */ /*dw30=LR*/
      UChar x29_how; /* a CFIR_ value */ /*dw29=FP*/
      Int   cfa_off;
      Int   ra_off;
      Int   sp_off;
      Int   x30_off;
      Int   x29_off;
   }
   DiCfSI_m;
#elif defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le)
/* Just have a struct with the common fields in, so that code that
   processes the common fields doesn't have to be ifdef'd against
   VGP_/VGA_ symbols.  These are not used in any way on ppc32/64-linux
   at the moment. */
typedef
   struct {
      UChar cfa_how; /* a CFIC_ value */
      UChar ra_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   ra_off;
   }
   DiCfSI_m;
#elif defined(VGA_s390x)
typedef
   struct {
      UChar cfa_how; /* a CFIC_ value */
      UChar sp_how;  /* a CFIR_ value */
      UChar ra_how;  /* a CFIR_ value */
      UChar fp_how;  /* a CFIR_ value */
      UChar f0_how;  /* a CFIR_ value */
      UChar f1_how;  /* a CFIR_ value */
      UChar f2_how;  /* a CFIR_ value */
      UChar f3_how;  /* a CFIR_ value */
      UChar f4_how;  /* a CFIR_ value */
      UChar f5_how;  /* a CFIR_ value */
      UChar f6_how;  /* a CFIR_ value */
      UChar f7_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   sp_off;
      Int   ra_off;
      Int   fp_off;
      Int   f0_off;
      Int   f1_off;
      Int   f2_off;
      Int   f3_off;
      Int   f4_off;
      Int   f5_off;
      Int   f6_off;
      Int   f7_off;
   }
   DiCfSI_m;
#elif defined(VGA_mips32) || defined(VGA_mips64) || defined(VGA_nanomips)
typedef
   struct {
      UChar cfa_how; /* a CFIC_ value */
      UChar ra_how;  /* a CFIR_ value */
      UChar sp_how;  /* a CFIR_ value */
      UChar fp_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   ra_off;
      Int   sp_off;
      Int   fp_off;
   }
   DiCfSI_m;
#else
#  error "Unknown arch"
#endif

typedef
   struct {
      Addr  base;
      UInt  len;
      UInt  cfsi_m_ix;
   }
   DiCfSI;

typedef
   enum {
      Cunop_Abs=0x231,
      Cunop_Neg,
      Cunop_Not
   }
   CfiUnop;

typedef
   enum {
      Cbinop_Add=0x321,
      Cbinop_Sub,
      Cbinop_And,
      Cbinop_Mul,
      Cbinop_Shl,
      Cbinop_Shr,
      Cbinop_Eq,
      Cbinop_Ge,
      Cbinop_Gt,
      Cbinop_Le,
      Cbinop_Lt,
      Cbinop_Ne
   }
   CfiBinop;

typedef
   enum {
      Creg_INVALID=0x213,
      Creg_IA_SP,
      Creg_IA_BP,
      Creg_IA_IP,
      Creg_ARM_R13,
      Creg_ARM_R12,
      Creg_ARM_R15,
      Creg_ARM_R14,
      Creg_ARM_R7,
      Creg_ARM64_SP,
      Creg_ARM64_X30,
      Creg_ARM64_X29,
      Creg_S390_IA,
      Creg_S390_SP,
      Creg_S390_FP,
      Creg_S390_LR,
      Creg_MIPS_RA
   }
   CfiReg;

typedef
   enum {
      Cex_Undef=0x123,
      Cex_Deref,
      Cex_Const,
      Cex_Unop,
      Cex_Binop,
      Cex_CfiReg,
      Cex_DwReg
   }
   CfiExprTag;

typedef 
   struct {
      CfiExprTag tag;
      union {
         struct {
         } Undef;
         struct {
            Int ixAddr;
         } Deref;
         struct {
            UWord con;
         } Const;
         struct {
            CfiUnop op;
            Int ix;
         } Unop;
         struct {
            CfiBinop op;
            Int ixL;
            Int ixR;
         } Binop;
         struct {
            CfiReg reg;
         } CfiReg;
         struct {
            Int reg;
         } DwReg;
      }
      Cex;
   }
   CfiExpr;

extern Int ML_(CfiExpr_Undef) ( XArray* dst );
extern Int ML_(CfiExpr_Deref) ( XArray* dst, Int ixAddr );
extern Int ML_(CfiExpr_Const) ( XArray* dst, UWord con );
extern Int ML_(CfiExpr_Unop)  ( XArray* dst, CfiUnop op, Int ix );
extern Int ML_(CfiExpr_Binop) ( XArray* dst, CfiBinop op, Int ixL, Int ixR );
extern Int ML_(CfiExpr_CfiReg)( XArray* dst, CfiReg reg );
extern Int ML_(CfiExpr_DwReg) ( XArray* dst, Int reg );

extern void ML_(ppCfiExpr)( const XArray* src, Int ix );

/* ---------------- FPO INFO (Windows PE) -------------- */

/* for apps using Wine: MSVC++ PDB FramePointerOmitted: somewhat like
   a primitive CFI */
typedef
   struct _FPO_DATA {  /* 16 bytes */
      UInt   ulOffStart; /* offset of 1st byte of function code */
      UInt   cbProcSize; /* # bytes in function */
      UInt   cdwLocals;  /* # bytes/4 in locals */
      UShort cdwParams;  /* # bytes/4 in params */
      UChar  cbProlog;   /* # bytes in prolog */
      UChar  cbRegs :3;  /* # regs saved */
      UChar  fHasSEH:1;  /* Structured Exception Handling */
      UChar  fUseBP :1;  /* EBP has been used */
      UChar  reserved:1;
      UChar  cbFrame:2;  /* frame type */
   }
   FPO_DATA;

#define PDB_FRAME_FPO  0
#define PDB_FRAME_TRAP 1
#define PDB_FRAME_TSS  2

/* --------------------- VARIABLES --------------------- */

typedef
   struct {
      Addr    aMin;
      Addr    aMax;
      XArray* /* of DiVariable */ vars;
   }
   DiAddrRange;

typedef
   struct {
      const  HChar* name;  /* in DebugInfo.strpool */
      UWord  typeR; /* a cuOff */
      const GExpr* gexpr; /* on DebugInfo.gexprs list */
      const GExpr* fbGX;  /* SHARED. */
      UInt   fndn_ix; /* where declared; may be zero. index
                         in DebugInfo.fndnpool */
      Int    lineNo;   /* where declared; may be zero. */
   }
   DiVariable;

Word 
ML_(cmp_for_DiAddrRange_range) ( const void* keyV, const void* elemV );

/* --------------------- DEBUGINFO --------------------- */

/* This is the top-level data type.  It's a structure which contains
   information pertaining to one mapped ELF object.  This type is
   exported only abstractly - in pub_tool_debuginfo.h. */

/* First though, here's an auxiliary data structure.  It is only ever
   used as part of a struct _DebugInfo.  We use it to record
   observations about mappings and permission changes to the
   associated file, so as to decide when to read debug info.  It's
   essentially an ultra-trivial finite state machine which, when it
   reaches an accept state, signals that we should now read debug info
   from the object into the associated struct _DebugInfo.  The accept
   state is arrived at when have_rx_map is true and rw_map_count
   is 1 or 2.  The initial state is one in which we have no observations,
   so have_rx_map is false and rw_map_count is 0.

   This all started as a rather ad-hoc solution, but was further
   expanded to handle weird object layouts, e.g. more than one rw
   or rx mapping for one binary.

   The normal sequence of events is one of

   start  -->  r-x mapping  -->  rw- mapping  -->  accept
   start  -->  rw- mapping  -->  r-x mapping  -->  accept

   that is, take the first r-x and rw- mapping we see, and we're done.

   On MacOSX >= 10.7, 32-bit, there appears to be a new variant:

   start  -->  r-- mapping  -->  rw- mapping  
          -->  upgrade r-- mapping to r-x mapping  -->  accept

   where the upgrade is done by a call to mach_vm_protect (OSX 10.7)
   or kernelrpc_mach_vm_protect_trap (OSX 10.9 and possibly 10.8).
   Hence we need to also track this possibility.

   From perusal of dyld sources, it appears that this scheme could
   also be used 64 bit libraries, although that doesn't seem to happen
   in practice.  dyld uses this scheme when the text section requires
   relocation, which only appears to be the case for 32 bit objects.
*/

typedef struct
{
   Addr  avma; /* these fields record the file offset, length */
   SizeT size; /* and map address of each mapping             */
   OffT  foff;
   Bool  rx, rw, ro;  /* memory access flags for this mapping */
#if defined(VGO_freebsd)
   Bool ignore_foff;
#endif
} DebugInfoMapping;

struct _DebugInfoFSM
{
   HChar*  filename;  /* in mallocville (VG_AR_DINFO)               */
   HChar*  dbgname;   /* in mallocville (VG_AR_DINFO)               */
   XArray* maps;      /* XArray of DebugInfoMapping structs         */
   Bool  have_rx_map; /* did we see a r?x mapping yet for the file? */
   Int   rw_map_count; /* count of w? mappings seen (may be > 1 )   */
   Bool  have_ro_map; /* did we see a r-- mapping yet for the file? */
};


/* To do with the string table in struct _DebugInfo (::strpool) */
#define SEGINFO_STRPOOLSIZE (64*1024)


/* We may encounter more than one .eh_frame section in an object --
   unusual but apparently allowed by ELF.  See
   http://sourceware.org/bugzilla/show_bug.cgi?id=12675
*/
#define N_EHFRAME_SECTS 2


/* So, the main structure for holding debug info for one object. */

struct _DebugInfo {

   /* Admin stuff */

   struct _DebugInfo* next;   /* list of DebugInfos */
   Bool               mark;   /* marked for deletion? */

   /* An abstract handle, which can be used by entities outside of
      m_debuginfo to (in an abstract datatype sense) refer to this
      struct _DebugInfo.  A .handle of zero is invalid; valid handles
      are 1 and above.  The same handle is never issued twice (in any
      given run of Valgrind), so a handle becomes invalid when the
      associated struct _DebugInfo is discarded, and remains invalid
      forever thereafter.  The .handle field is set as soon as this
      structure is allocated. */
   ULong handle;

   /* The range of epochs for which this DebugInfo is valid.  These also
      divide the DebugInfo's lifetime into three parts:

      (1) Allocated: but with only .fsm holding useful info -- in
          particular, not yet holding any debug info.
          .first_epoch == DebugInfoEpoch_INVALID
          .last_epoch  == DebugInfoEpoch_INVALID

      (2) Active: containing debug info, and current.
          .first_epoch != DebugInfoEpoch_INVALID
          .last_epoch  == DebugInfoEpoch_INVALID

      (3) Archived: containing debug info, but no longer current.
          .first_epoch != DebugInfoEpoch_INVALID
          .last_epoch  != DebugInfoEpoch_INVALID

      State (2) corresponds to an object which is currently mapped.  When
      the object is unmapped, what happens depends on the setting of
      --keep-debuginfo:

      * when =no, the DebugInfo is removed from debugInfo_list and
        deleted.

      * when =yes, the DebugInfo is retained in debugInfo_list, but its
        .last_epoch field is filled in, and current_epoch is advanced.  This
        effectively moves the DebugInfo into state (3).
   */
   DiEpoch first_epoch;
   DiEpoch last_epoch;

   /* Used for debugging only - indicate what stuff to dump whilst
      reading stuff into the seginfo.  Are computed as early in the
      lifetime of the DebugInfo as possible -- at the point when it is
      created.  Use these when deciding what to spew out; do not use
      the global VG_(clo_blah) flags. */

   Bool trace_symtab; /* symbols, our style */
   Bool trace_cfi;    /* dwarf frame unwind, our style */
   Bool ddump_syms;   /* mimic /usr/bin/readelf --syms */
   Bool ddump_line;   /* mimic /usr/bin/readelf --debug-dump=line */
   Bool ddump_frames; /* mimic /usr/bin/readelf --debug-dump=frames */

   /* The "decide when it is time to read debuginfo" state machine.
      This structure must get filled in before we can start reading
      anything from the ELF/MachO file.  This structure is filled in
      by VG_(di_notify_mmap) and its immediate helpers. */
   struct _DebugInfoFSM fsm;

   /* Once the ::fsm has reached an accept state -- typically, when
      both a rw? and r?x mapping for .filename have been observed --
      we can go on to read the symbol tables and debug info.
      .have_dinfo changes from False to True when the debug info has
      been completely read in and postprocessed (canonicalised) and is
      now suitable for querying. */
   /* If have_dinfo is False, then all fields below this point are
      invalid and should not be consulted. */
   Bool  have_dinfo; /* initially False */

   /* If true then the reading of .debug_* section has been deferred
      until it this information is required (such as when printing
      a stacktrace).  Additionally, if true then the reading of any
      separate debuginfo files associated with this object has also
      been deferred. */
   Bool deferred;

   /* All the rest of the fields in this structure are filled in once
      we have committed to reading the symbols and debug info (that
      is, at the point where .have_dinfo is set to True). */

   /* The file's soname. */
   HChar* soname;

   /* Description of some important mapped segments.  The presence or
      absence of the mapping is denoted by the _present field, since
      in some obscure circumstances (to do with data/sdata/bss) it is
      possible for the mapping to be present but have zero size.
      Certainly text_ is mandatory on all platforms; not sure about
      the rest though. 

      --------------------------------------------------------

      Comment_on_IMPORTANT_CFSI_REPRESENTATIONAL_INVARIANTS: we require that
 
      either (size of all rx maps == 0 && cfsi == NULL) (the degenerate case)

      or the normal case, which is the AND of the following:
      (0) size of at least one rx mapping > 0
      (1) no two non-archived DebugInfos with some rx mapping of size > 0
          have overlapping rx mappings
      (2) Each address in [cfsi_minavma,cfsi_maxavma] is in an rx mapping
          or else no cfsi can cover this address.
          The typical case is a single rx mapping covering the full range.
          In some cases, the union of several rx mappings covers the range,
          with possibly some holes between the rx mappings, and no cfsi fall
          within such an hole.
      (3) all DiCfSI in the cfsi array all have ranges that fall within
          [avma,+size) of that rx mapping.
      (4) all DiCfSI in the cfsi array are non-overlapping

      The cumulative effect of these restrictions is to ensure that
      all the DiCfSI records in the entire system are non overlapping.
      Hence any address falls into either exactly one DiCfSI record,
      or none.  Hence it is safe to cache the results of searches for
      DiCfSI records.  This is the whole point of these restrictions.
      The caching of DiCfSI searches is done in VG_(use_CF_info).  The
      cache is flushed after any change to debugInfo_list.  DiCfSI
      searches are cached because they are central to stack unwinding
      on amd64-linux.

      Where are these invariants imposed and checked?

      They are checked after a successful read of debuginfo into
      a DebugInfo*, in check_CFSI_related_invariants.

      (1) is not really imposed anywhere.  We simply assume that the
      kernel will not map the text segments from two different objects
      into the same space.  Sounds reasonable.

      (2) follows from (4) and (3).  It is ensured by canonicaliseCFI.
      (3) is ensured by ML_(addDiCfSI).
      (4) is ensured by canonicaliseCFI.

      --------------------------------------------------------

      Comment_on_DEBUG_SVMA_and_DEBUG_BIAS_fields:

      The _debug_{svma,bias} fields were added as part of a fix to
      #185816.  The problem encompassed in that bug report was that it
      wasn't correct to use apply the bias values deduced for a
      primary object to its associated debuginfo object, because the
      debuginfo object (or the primary) could have been prelinked to a
      different SVMA.  Hence debuginfo and primary objects need to
      have their own biases.

      ------ JRS: (referring to r9329): ------
      Let me see if I understand the workings correctly.  Initially
      the _debug_ values are set to the same values as the "normal"
      ones, as there's a bunch of bits of code like this (in
      readelf.c)

         di->text_svma = svma;
         ...
         di->text_bias = rx_bias;
         di->text_debug_svma = svma;
         di->text_debug_bias = rx_bias;

      If a debuginfo object subsequently shows up then the
      _debug_svma/bias are set for the debuginfo object.  Result is
      that if there's no debuginfo object then the values are the same
      as the primary-object values, and if there is a debuginfo object
      then they will (or at least may) be different.

      Then when we need to actually bias something, we'll have to
      decide whether to use the primary bias or the debuginfo bias.
      And the strategy is to use the primary bias for ELF symbols but
      the debuginfo bias for anything pulled out of Dwarf.

      ------ THH: ------
      Correct - the debug_svma and bias values apply to any address
      read from the debug data regardless of where that debug data is
      stored and the other values are used for addresses from other
      places (primarily the symbol table).

      ------ JRS: ------ 
      Ok; so this was my only area of concern.  Are there any
      corner-case scenarios where this wouldn't be right?  It sounds
      like we're assuming the ELF symbols come from the primary object
      and, if there is a debug object, then all the Dwarf comes from
      there.  But what if (eg) both symbols and Dwarf come from the
      debug object?  Is that even possible or allowable?

      ------ THH: ------
      You may have a point...

      The current logic is to try and take any one set of data from
      either the base object or the debug object. There are four sets
      of data we consider:

         - Symbol Table
         - Stabs
         - DWARF1
         - DWARF2

      If we see the primary section for a given set in the base object
      then we ignore all sections relating to that set in the debug
      object.

      Now in principle if we saw a secondary section (like debug_line
      say) in the base object, but not the main section (debug_info in
      this case) then we would take debug_info from the debug object
      but would use the debug_line from the base object unless we saw
      a replacement copy in the debug object. That's probably unlikely
      however.

      A bigger issue might be, as you say, the symbol table as we will
      pick that up from the debug object if it isn't in the base. The
      dynamic symbol table will always have to be in the base object
      though so we will have to be careful when processing symbols to
      know which table we are reading in that case.

      What we probably need to do is tell read_elf_symtab which object
      the symbols it is being asked to read came from.

      (A followup patch to deal with this was committed in r9469).
   */
   /* .text */
   Bool     text_present;
   Addr     text_avma;
   Addr     text_svma;
   SizeT    text_size;
   PtrdiffT text_bias;
   Addr     text_debug_svma;
   PtrdiffT text_debug_bias;
   /* .data */
   Bool     data_present;
   Addr     data_svma;
   Addr     data_avma;
   SizeT    data_size;
   PtrdiffT data_bias;
   Addr     data_debug_svma;
   PtrdiffT data_debug_bias;
   /* .sdata */
   Bool     sdata_present;
   Addr     sdata_svma;
   Addr     sdata_avma;
   SizeT    sdata_size;
   PtrdiffT sdata_bias;
   Addr     sdata_debug_svma;
   PtrdiffT sdata_debug_bias;
   /* .rodata */
   Bool     rodata_present;
   Addr     rodata_svma;
   Addr     rodata_avma;
   SizeT    rodata_size;
   PtrdiffT rodata_bias;
   Addr     rodata_debug_svma;
   PtrdiffT rodata_debug_bias;
   /* .bss */
   Bool     bss_present;
   Addr     bss_svma;
   Addr     bss_avma;
   SizeT    bss_size;
   PtrdiffT bss_bias;
   Addr     bss_debug_svma;
   PtrdiffT bss_debug_bias;
   /* .sbss */
   Bool     sbss_present;
   Addr     sbss_svma;
   Addr     sbss_avma;
   SizeT    sbss_size;
   PtrdiffT sbss_bias;
   Addr     sbss_debug_svma;
   PtrdiffT sbss_debug_bias;
   /* .ARM.exidx -- sometimes present on arm32, containing unwind info. */
   Bool     exidx_present;
   Addr     exidx_avma;
   Addr     exidx_svma;
   SizeT    exidx_size;
   PtrdiffT exidx_bias;
   /* .ARM.extab -- sometimes present on arm32, containing unwind info. */
   Bool     extab_present;
   Addr     extab_avma;
   Addr     extab_svma;
   SizeT    extab_size;
   PtrdiffT extab_bias;
   /* .plt */
   Bool   plt_present;
   Addr	  plt_avma;
   SizeT  plt_size;
   /* .got */
   Bool   got_present;
   Addr   got_avma;
   SizeT  got_size;
   /* .got.plt */
   Bool   gotplt_present;
   Addr   gotplt_avma;
   SizeT  gotplt_size;
   /* .opd -- needed on ppc64be-linux for finding symbols */
   Bool   opd_present;
   Addr   opd_avma;
   SizeT  opd_size;
   /* .ehframe -- needed on amd64-linux for stack unwinding.  We might
      see more than one, hence the arrays. */
   UInt   n_ehframe;  /* 0 .. N_EHFRAME_SECTS */
   Addr   ehframe_avma[N_EHFRAME_SECTS];
   SizeT  ehframe_size[N_EHFRAME_SECTS];

   /* Sorted tables of stuff we snarfed from the file.  This is the
      eventual product of reading the debug info.  All this stuff
      lives in VG_AR_DINFO. */

   /* An expandable array of symbols. */
   DiSym*  symtab;
   UWord   symtab_used;
   UWord   symtab_size;
   /* Two expandable arrays, storing locations and their filename/dirname. */
   DiLoc*  loctab;
   UInt    sizeof_fndn_ix;  /* Similar use as sizeof_cfsi_m_ix below. */
   void*   loctab_fndn_ix;  /* loctab[i] filename/dirname is identified by
                               loctab_fnindex_ix[i] (an index in di->fndnpool)
                               0 means filename/dirname unknown.
                               The void* is an UChar* or UShort* or UInt*
                               depending on sizeof_fndn_ix. */
   UWord   loctab_used;
   UWord   loctab_size;
   /* An expandable array of inlined fn info.
      maxinl_codesz is the biggest inlined piece of code
      in inltab (i.e. the max of 'addr_hi - addr_lo'. */
   DiInlLoc* inltab;
   UWord   inltab_used;
   UWord   inltab_size;
   SizeT   maxinl_codesz;

   /* A set of expandable arrays to store CFI summary info records.
      The machine specific information (i.e. the DiCfSI_m struct)
      are stored in cfsi_m_pool, as these are highly duplicated.
      The DiCfSI_m are allocated in cfsi_m_pool and identified using
      a (we hope) small integer : often one byte is enough, sometimes
      2 bytes are needed.

      cfsi_base contains the bases of the code address ranges.
      cfsi_size is the size of the cfsi_base array.
      The elements cfsi_base[0] till cfsi_base[cfsi_used-1] are used.
      Following elements are not used (yet).

      For each base in cfsi_base, an index into cfsi_m_pool is stored
      in cfsi_m_ix array. The size of cfsi_m_ix is equal to
      cfsi_size*sizeof_cfsi_m_ix. The used portion of cfsi_m_ix is
      cfsi_m_ix[0] till cfsi_m_ix[(cfsi_used-1)*sizeof_cfsi_m_ix].

      cfsi_base[i] gives the base address of a code range covered by
      some CF Info. The corresponding CF Info is identified by an index
      in cfsi_m_pool. The DiCfSI_m index in cfsi_m_pool corresponding to
      cfsi_base[i] is given
        by ((UChar*) cfsi_m_ix)[i] if sizeof_cfsi_m_ix == 1
        by ((UShort*)cfsi_m_ix)[i] if sizeof_cfsi_m_ix == 2
        by ((UInt*)  cfsi_m_ix)[i] if sizeof_cfsi_m_ix == 4.

      The end of the code range starting at cfsi_base[i] is given by
      cfsi_base[i+1]-1 (or cfsi_maxavma for  cfsi_base[cfsi_used-1]).
      Some code ranges between cfsi_minavma and cfsi_maxavma might not
      be covered by cfi information. Such not covered ranges are stored by
      a base in cfsi_base and a corresponding 0 index in cfsi_m_ix.

      A variable size representation has been chosen for the elements of
      cfsi_m_ix as in many case, one byte is good enough. For big
      objects, 2 bytes are needed. No object has yet been found where
      4 bytes are needed (but the code is ready to handle this case).
      Not covered ranges ('cfi holes') are stored explicitly in
      cfsi_base/cfsi_m_ix as this is more memory efficient than storing
      a length for each covered range : on x86 or amd64, we typically have
      a hole every 8 covered ranges. On arm64, we have very few holes
      (1 every 50 or 100 ranges).
      
      The cfsi information is read and prepared in the cfsi_rd array.
      Once all the information has been read, the cfsi_base and cfsi_m_ix
      arrays will be filled in from cfsi_rd. cfsi_rd will then be freed.
      This is all done by ML_(finish_CFSI_arrays).

      Also includes summary address bounds, showing the min and max address
      covered by any of the records, as an aid to fast searching.  And, if the
      records require any expression nodes, they are stored in
      cfsi_exprs. */
   Addr* cfsi_base;
   UInt  sizeof_cfsi_m_ix; /* size in byte of indexes stored in cfsi_m_ix. */
   void* cfsi_m_ix; /* Each index occupies sizeof_cfsi_m_ix bytes.
                       The void* is an UChar* or UShort* or UInt*
                       depending on sizeof_cfsi_m_ix.  */

   DiCfSI* cfsi_rd; /* Only used during reading, NULL once info is read. */
                                   
   UWord   cfsi_used;
   UWord   cfsi_size;

   DedupPoolAlloc *cfsi_m_pool;
   Addr    cfsi_minavma;
   Addr    cfsi_maxavma;
   XArray* cfsi_exprs; /* XArray of CfiExpr */

   /* Optimized code under Wine x86: MSVC++ PDB FramePointerOmitted
      data.  Non-expandable array, hence .size == .used. */
   FPO_DATA* fpo;
   UWord     fpo_size;
   Addr      fpo_minavma;
   Addr      fpo_maxavma;
   Addr      fpo_base_avma;

   /* Pool of strings -- the string table.  Pointers
      into this are stable (the memory is not reallocated). */
   DedupPoolAlloc *strpool;

   /* Pool of FnDn -- filename and dirname.
      Elements in the pool are allocated using VG_(allocFixedEltDedupPA). */
   DedupPoolAlloc *fndnpool;

   /* Variable scope information, as harvested from Dwarf3 files.

      In short it's an

         array of (array of PC address ranges and variables)

      The outer array indexes over scopes, with Entry 0 containing
      information on variables which exist for any value of the program
      counter (PC) -- that is, the outermost scope.  Entries 1, 2, 3,
      etc contain information on increasinly deeply nested variables.

      Each inner array is an array of (an address range, and a set
      of variables that are in scope over that address range).  

      The address ranges may not overlap.
 
      Since Entry 0 in the outer array holds information on variables
      that exist for any value of the PC (that is, global vars), it
      follows that Entry 0's inner array can only have one address
      range pair, one that covers the entire address space.
   */
   XArray* /* of OSet of DiAddrRange */varinfo;

   /* These are arrays of the relevant typed objects, held here
      partially for the purposes of visiting each object exactly once
      when we need to delete them. */

   /* An array of TyEnts.  These are needed to make sense of any types
      in the .varinfo.  Also, when deleting this DebugInfo, we must
      first traverse this array and throw away malloc'd stuff hanging
      off it -- by calling ML_(TyEnt__make_EMPTY) on each entry. */
   XArray* /* of TyEnt */ admin_tyents;

   /* An array of guarded DWARF3 expressions. */
   XArray* admin_gexprs;

   /* Cached last rx mapping matched and returned by ML_(find_rx_mapping).
      This helps performance a lot during ML_(addLineInfo) etc., which can
      easily be invoked hundreds of thousands of times. */
   DebugInfoMapping* last_rx_map;
};

/* --------------------- functions --------------------- */

/* ------ Adding ------ */

/* Add a symbol to si's symbol table.  The contents of 'sym' are
   copied.  It is assumed (and checked) that 'sym' only contains one
   name, so there is no auxiliary ::sec_names vector to duplicate.
   IOW, the copy is a shallow copy, and there are assertions in place
   to ensure that's OK. */
extern void ML_(addSym) ( struct _DebugInfo* di, DiSym* sym );

/* Add a filename/dirname pair to a DebugInfo and returns the index
   in the fndnpool fixed pool. */
extern UInt ML_(addFnDn) (struct _DebugInfo* di,
                          const HChar* filename, 
                          const HChar* dirname);  /* NULL is allowable */

/* Returns the filename of the fndn pair identified by fndn_ix.
   Returns "???" if fndn_ix is 0. */
extern const HChar* ML_(fndn_ix2filename) (const DebugInfo* di,
                                           UInt fndn_ix);

/* Returns the dirname of the fndn pair identified by fndn_ix.
   Returns "" if fndn_ix is 0 or fndn->dirname is NULL. */
extern const HChar* ML_(fndn_ix2dirname) (const DebugInfo* di,
                                          UInt fndn_ix);

/* Returns the fndn_ix for the LineInfo locno in di->loctab.
   0 if filename/dirname are unknown. */
extern UInt ML_(fndn_ix) (const DebugInfo* di, Word locno);

/* Add a line-number record to a DebugInfo.
   fndn_ix is an index in di->fndnpool, allocated using  ML_(addFnDn).
   Give a 0 index for a unknown filename/dirname pair. */
extern
void ML_(addLineInfo) ( struct _DebugInfo* di, 
                        UInt fndn_ix,
                        Addr this, Addr next, Int lineno, Int entry);

/* Add a call inlined record to a DebugInfo.
   A call to the below means that inlinedfn code has been
   inlined, resulting in code from [addr_lo, addr_hi[.
   Note that addr_hi is excluded, i.e. is not part of the inlined code.
   fndn_ix and lineno identifies the location of the call that caused
   this inlining.
   fndn_ix is an index in di->fndnpool, allocated using  ML_(addFnDn).
   Give a 0 index for an unknown filename/dirname pair.
   In case of nested inlining, a small level indicates the call
   is closer to main that a call with a higher level. */
extern
void ML_(addInlInfo) ( struct _DebugInfo* di, 
                       Addr addr_lo, Addr addr_hi,
                       const HChar* inlinedfn,
                       UInt fndn_ix,
                       Int lineno, UShort level);

/* Add a CFI summary record.  The supplied DiCfSI_m is copied. */
extern void ML_(addDiCfSI) ( struct _DebugInfo* di, 
                             Addr base, UInt len, DiCfSI_m* cfsi_m );

/* Given a position in the di->cfsi_base/cfsi_m_ix arrays, return
   the corresponding cfsi_m*. Return NULL if the position corresponds
   to a cfsi hole. */
DiCfSI_m* ML_(get_cfsi_m) (const DebugInfo* di, UInt pos);

/* Add a string to the string table of a DebugInfo.  If len==-1,
   ML_(addStr) will itself measure the length of the string. */
extern const HChar* ML_(addStr) ( DebugInfo* di, const HChar* str, Int len );

/* Add a string to the string table of a DebugInfo, by copying the
   string from the given DiCursor.  Measures the length of the string
   itself. */
extern const HChar* ML_(addStrFromCursor)( DebugInfo* di, DiCursor c );

extern void ML_(addVar)( struct _DebugInfo* di,
                         Int    level,
                         Addr   aMin,
                         Addr   aMax,
                         const  HChar* name,
                         UWord  typeR, /* a cuOff */
                         const GExpr* gexpr,
                         const GExpr* fbGX, /* SHARED. */
                         UInt   fndn_ix, /* where decl'd - may be zero */
                         Int    lineNo, /* where decl'd - may be zero */
                         Bool   show );
/* Note: fndn_ix identifies a filename/dirname pair similarly to
   ML_(addInlInfo) and ML_(addLineInfo). */

/* Canonicalise the tables held by 'di', in preparation for use.  Call
   this after finishing adding entries to these tables. */
extern void ML_(canonicaliseTables) ( struct _DebugInfo* di );

/* Canonicalise the call-frame-info table held by 'di', in preparation
   for use. This is called by ML_(canonicaliseTables) but can also be
   called on it's own to sort just this table. */
extern void ML_(canonicaliseCFI) ( struct _DebugInfo* di );

/* ML_(finish_CFSI_arrays) fills in the cfsi_base and cfsi_m_ix arrays
   from cfsi_rd array. cfsi_rd is then freed. */
extern void ML_(finish_CFSI_arrays) ( struct _DebugInfo* di );

/* ------ Searching ------ */

/* Find a symbol-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Word ML_(search_one_symtab) ( DebugInfo* di, Addr ptr,
                                     Bool findText );

/* Find a location-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Word ML_(search_one_loctab) ( DebugInfo* di, Addr ptr );

/* Find a CFI-table index containing the specified pointer, or -1 if
   not found.  Binary search.  */
extern Word ML_(search_one_cfitab) ( DebugInfo* di, Addr ptr );

/* Find a FPO-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Word ML_(search_one_fpotab) ( const DebugInfo* di, Addr ptr );

/* Helper function for the most often needed searching for an rx
   mapping containing the specified address range.  The range must
   fall entirely within the mapping to be considered to be within it.
   Asserts if lo > hi; caller must ensure this doesn't happen. */
extern DebugInfoMapping* ML_(find_rx_mapping) ( DebugInfo* di,
                                                Addr lo, Addr hi );

/* ------ Misc ------ */

/* Show a non-fatal debug info reading error.  Use VG_(core_panic) for
   fatal errors.  'serious' errors are always shown, not 'serious' ones
   are shown only at verbosity level 2 and above. */
extern 
void ML_(symerr) ( const DebugInfo* di, Bool serious, const HChar* msg );

/* Print a symbol. */
extern void ML_(ppSym) ( Int idx, const DiSym* sym );

/* Print a call-frame-info summary. */
extern void ML_(ppDiCfSI) ( const XArray* /* of CfiExpr */ exprs,
                            Addr base, UInt len,
                            const DiCfSI_m* si_m );


#define TRACE_SYMTAB_ENABLED (di->trace_symtab)
#define TRACE_SYMTAB(format, args...) \
   if (TRACE_SYMTAB_ENABLED) { VG_(printf)(format, ## args); }


#endif /* ndef __PRIV_STORAGE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
