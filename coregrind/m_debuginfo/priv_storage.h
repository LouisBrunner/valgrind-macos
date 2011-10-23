
/*--------------------------------------------------------------------*/
/*--- Format-neutral storage of and querying of info acquired from ---*/
/*--- ELF/XCOFF stabs/dwarf1/dwarf2 debug info.                    ---*/
/*---                                               priv_storage.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2011 Julian Seward 
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

/* --------------------- SYMBOLS --------------------- */

/* A structure to hold an ELF/MachO symbol (very crudely).  Usually
   the symbol only has one name, which is stored in ::pri_name, and
   ::sec_names is NULL.  If there are other names, these are stored in
   ::sec_names, which is a NULL terminated vector holding the names.
   The vector is allocated in VG_AR_DINFO, the names themselves live
   in DebugInfo::strchunks.

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
      Addr    addr;    /* lowest address of entity */
      Addr    tocptr;  /* ppc64-linux only: value that R2 should have */
      UChar*  pri_name;  /* primary name, never NULL */
      UChar** sec_names; /* NULL, or a NULL term'd array of other names */
      // XXX: this could be shrunk (on 32-bit platforms) by using 30
      // bits for the size and 1 bit each for isText and isIFunc.  If you
      // do this, make sure that all assignments to the latter two use
      // 0 or 1 (or True or False), and that a positive number larger
      // than 1 is never used to represent True.
      UInt    size;    /* size in bytes */
      Bool    isText;
      Bool    isIFunc; /* symbol is an indirect function? */
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

/* A structure to hold addr-to-source info for a single line.  There
  can be a lot of these, hence the dense packing. */
typedef
   struct {
      /* Word 1 */
      Addr   addr;               /* lowest address for this line */
      /* Word 2 */
      UShort size:LOC_SIZE_BITS; /* # bytes; we catch overflows of this */
      UInt   lineno:LINENO_BITS; /* source line number, or zero */
      /* Word 3 */
      UChar*  filename;          /* source filename */
      /* Word 4 */
      UChar*  dirname;           /* source directory name */
   }
   DiLoc;

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
              CFIR_IA_EXPR  -> expr whose index is in cfa_off

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
              CFIC_R13REL -> r13 + cfa_off
              CFIC_R12REL -> r12 + cfa_off
              CFIC_R11REL -> r11 + cfa_off
              CFIC_R7REL  -> r7  + cfa_off
              CFIR_EXPR   -> expr whose index is in cfa_off

     old_r14/r13/r12/r11/r7/ra
         = case r14/r13/r12/r11/r7/ra_how of
              CFIR_UNKNOWN   -> we don't know, sorry
              CFIR_SAME      -> same as it was before (r14/r13/r12/r11/r7 only)
              CFIR_CFAREL    -> cfa + r14/r13/r12/r11/r7/ra_off
              CFIR_MEMCFAREL -> *( cfa + r14/r13/r12/r11/r7/ra_off )
              CFIR_EXPR      -> expr whose index is in r14/r13/r12/r11/r7/ra_off

   On s390x we have a similar logic as x86 or amd64. We need the stack pointer
   (r15), the frame pointer r11 (like BP) and together with the instruction
   address in the PSW we can calculate the previous values:
     cfa = case cfa_how of
              CFIC_IA_SPREL -> r15 + cfa_off
              CFIC_IA_BPREL -> r11 + cfa_off
              CFIR_IA_EXPR  -> expr whose index is in cfa_off

     old_sp/fp/ra
         = case sp/fp/ra_how of
              CFIR_UNKNOWN   -> we don't know, sorry
              CFIR_SAME      -> same as it was before (sp/fp only)
              CFIR_CFAREL    -> cfa + sp/fp/ra_off
              CFIR_MEMCFAREL -> *( cfa + sp/fp/ra_off )
              CFIR_EXPR      -> expr whose index is in sp/fp/ra_off
*/

#define CFIC_IA_SPREL     ((UChar)1)
#define CFIC_IA_BPREL     ((UChar)2)
#define CFIC_IA_EXPR      ((UChar)3)
#define CFIC_ARM_R13REL   ((UChar)4)
#define CFIC_ARM_R12REL   ((UChar)5)
#define CFIC_ARM_R11REL   ((UChar)6)
#define CFIC_ARM_R7REL    ((UChar)7)
#define CFIC_EXPR         ((UChar)8)  /* all targets */

#define CFIR_UNKNOWN      ((UChar)64)
#define CFIR_SAME         ((UChar)65)
#define CFIR_CFAREL       ((UChar)66)
#define CFIR_MEMCFAREL    ((UChar)67)
#define CFIR_EXPR         ((UChar)68)

#if defined(VGA_x86) || defined(VGA_amd64)
typedef
   struct {
      Addr  base;
      UInt  len;
      UChar cfa_how; /* a CFIC_IA value */
      UChar ra_how;  /* a CFIR_ value */
      UChar sp_how;  /* a CFIR_ value */
      UChar bp_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   ra_off;
      Int   sp_off;
      Int   bp_off;
   }
   DiCfSI;
#elif defined(VGA_arm)
typedef
   struct {
      Addr  base;
      UInt  len;
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
   }
   DiCfSI;
#elif defined(VGA_ppc32) || defined(VGA_ppc64)
/* Just have a struct with the common fields in, so that code that
   processes the common fields doesn't have to be ifdef'd against
   VGP_/VGA_ symbols.  These are not used in any way on ppc32/64-linux
   at the moment. */
typedef
   struct {
      Addr  base;
      UInt  len;
      UChar cfa_how; /* a CFIC_ value */
      UChar ra_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   ra_off;
   }
   DiCfSI;
#elif defined(VGA_s390x)
typedef
   struct {
      Addr  base;
      UInt  len;
      UChar cfa_how; /* a CFIC_ value */
      UChar sp_how;  /* a CFIR_ value */
      UChar ra_how;  /* a CFIR_ value */
      UChar fp_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   sp_off;
      Int   ra_off;
      Int   fp_off;
   }
   DiCfSI;
#else
#  error "Unknown arch"
#endif


typedef
   enum {
      Cop_Add=0x321,
      Cop_Sub,
      Cop_And,
      Cop_Mul,
      Cop_Shl,
      Cop_Shr,
      Cop_Eq,
      Cop_Ge,
      Cop_Gt,
      Cop_Le,
      Cop_Lt,
      Cop_Ne
   }
   CfiOp;

typedef
   enum {
      Creg_IA_SP=0x213,
      Creg_IA_BP,
      Creg_IA_IP,
      Creg_ARM_R13,
      Creg_ARM_R12,
      Creg_ARM_R15,
      Creg_ARM_R14,
      Creg_S390_R14
   }
   CfiReg;

typedef
   enum {
      Cex_Undef=0x123,
      Cex_Deref,
      Cex_Const,
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
            CfiOp op;
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
extern Int ML_(CfiExpr_Binop) ( XArray* dst, CfiOp op, Int ixL, Int ixR );
extern Int ML_(CfiExpr_CfiReg)( XArray* dst, CfiReg reg );
extern Int ML_(CfiExpr_DwReg) ( XArray* dst, Int reg );

extern void ML_(ppCfiExpr)( XArray* src, Int ix );

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
      UChar* name;  /* in DebugInfo.strchunks */
      UWord  typeR; /* a cuOff */
      GExpr* gexpr; /* on DebugInfo.gexprs list */
      GExpr* fbGX;  /* SHARED. */
      UChar* fileName; /* where declared; may be NULL. in
                          DebugInfo.strchunks */
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
   state is arrived at when have_rx_map and have_rw_map both become
   true.  The initial state is one in which we have no observations,
   so have_rx_map and have_rw_map are both false.

   This is all rather ad-hoc; for example it has no way to record more
   than one rw or rx mapping for a given object, not because such
   events have never been observed, but because we've never needed to
   note more than the first one of any such in order when to decide to
   read debug info.  It may be that in future we need to track more
   state in order to make the decision, so this struct would then get
   expanded.

   The normal sequence of events is one of

   start  -->  r-x mapping  -->  rw- mapping  -->  accept
   start  -->  rw- mapping  -->  r-x mapping  -->  accept

   that is, take the first r-x and rw- mapping we see, and we're done.

   On MacOSX 10.7, 32-bit, there appears to be a new variant:

   start  -->  r-- mapping  -->  rw- mapping  
          -->  upgrade r-- mapping to r-x mapping  -->  accept

   where the upgrade is done by a call to vm_protect.  Hence we
   need to also track this possibility.
*/
struct _DebugInfoFSM
{
   /* --- all targets --- */
   UChar* filename; /* in mallocville (VG_AR_DINFO) */

   Bool  have_rx_map; /* did we see a r?x mapping yet for the file? */
   Bool  have_rw_map; /* did we see a rw? mapping yet for the file? */

   Addr  rx_map_avma; /* these fields record the file offset, length */
   SizeT rx_map_size; /* and map address of the r?x mapping we believe */
   OffT  rx_map_foff; /* is the .text segment mapping */

   Addr  rw_map_avma; /* ditto, for the rw? mapping we believe is the */
   SizeT rw_map_size; /* .data segment mapping */
   OffT  rw_map_foff;

   /* --- OSX 10.7, 32-bit only --- */
   Bool  have_ro_map; /* did we see a r-- mapping yet for the file? */

   Addr  ro_map_avma; /* file offset, length, avma for said mapping */
   SizeT ro_map_size;
   OffT  ro_map_foff;
};


/* To do with the string table in struct _DebugInfo (::strchunks) */
#define SEGINFO_STRCHUNKSIZE (64*1024)


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

   /* All the rest of the fields in this structure are filled in once
      we have committed to reading the symbols and debug info (that
      is, at the point where .have_dinfo is set to True). */

   /* The file's soname.  FIXME: ensure this is always allocated in
      VG_AR_DINFO. */
   UChar* soname;

   /* Description of some important mapped segments.  The presence or
      absence of the mapping is denoted by the _present field, since
      in some obscure circumstances (to do with data/sdata/bss) it is
      possible for the mapping to be present but have zero size.
      Certainly text_ is mandatory on all platforms; not sure about
      the rest though. 

      --------------------------------------------------------

      Comment_on_IMPORTANT_CFSI_REPRESENTATIONAL_INVARIANTS: we require that
 
      either (rx_map_size == 0 && cfsi == NULL) (the degenerate case)

      or the normal case, which is the AND of the following:
      (0) rx_map_size > 0
      (1) no two DebugInfos with rx_map_size > 0 
          have overlapping [rx_map_avma,+rx_map_size)
      (2) [cfsi_minavma,cfsi_maxavma] does not extend 
          beyond [rx_map_avma,+rx_map_size); that is, the former is a 
          subrange or equal to the latter.
      (3) all DiCfSI in the cfsi array all have ranges that fall within
          [rx_map_avma,+rx_map_size).
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
   /* .opd -- needed on ppc64-linux for finding symbols */
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
   /* An expandable array of locations. */
   DiLoc*  loctab;
   UWord   loctab_used;
   UWord   loctab_size;
   /* An expandable array of CFI summary info records.  Also includes
      summary address bounds, showing the min and max address covered
      by any of the records, as an aid to fast searching.  And, if the
      records require any expression nodes, they are stored in
      cfsi_exprs. */
   DiCfSI* cfsi;
   UWord   cfsi_used;
   UWord   cfsi_size;
   Addr    cfsi_minavma;
   Addr    cfsi_maxavma;
   XArray* cfsi_exprs; /* XArray of CfiExpr */

   /* Optimized code under Wine x86: MSVC++ PDB FramePointerOmitted
      data.  Non-expandable array, hence .size == .used. */
   FPO_DATA* fpo;
   UWord     fpo_size;
   Addr      fpo_minavma;
   Addr      fpo_maxavma;

   /* Expandable arrays of characters -- the string table.  Pointers
      into this are stable (the arrays are not reallocated). */
   struct strchunk {
      UInt   strtab_used;
      struct strchunk* next;
      UChar  strtab[SEGINFO_STRCHUNKSIZE];
   } *strchunks;

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
};

/* --------------------- functions --------------------- */

/* ------ Adding ------ */

/* Add a symbol to si's symbol table.  The contents of 'sym' are
   copied.  It is assumed (and checked) that 'sym' only contains one
   name, so there is no auxiliary ::sec_names vector to duplicate.
   IOW, the copy is a shallow copy, and there are assertions in place
   to ensure that's OK. */
extern void ML_(addSym) ( struct _DebugInfo* di, DiSym* sym );

/* Add a line-number record to a DebugInfo. */
extern
void ML_(addLineInfo) ( struct _DebugInfo* di, 
                        UChar*   filename, 
                        UChar*   dirname,  /* NULL is allowable */
                        Addr this, Addr next, Int lineno, Int entry);

/* Add a CFI summary record.  The supplied DiCfSI is copied. */
extern void ML_(addDiCfSI) ( struct _DebugInfo* di, DiCfSI* cfsi );

/* Add a string to the string table of a DebugInfo.  If len==-1,
   ML_(addStr) will itself measure the length of the string. */
extern UChar* ML_(addStr) ( struct _DebugInfo* di, UChar* str, Int len );

extern void ML_(addVar)( struct _DebugInfo* di,
                         Int    level,
                         Addr   aMin,
                         Addr   aMax,
                         UChar* name,
                         UWord  typeR, /* a cuOff */
                         GExpr* gexpr,
                         GExpr* fbGX, /* SHARED. */
                         UChar* fileName, /* where decl'd - may be NULL */
                         Int    lineNo, /* where decl'd - may be zero */
                         Bool   show );

/* Canonicalise the tables held by 'di', in preparation for use.  Call
   this after finishing adding entries to these tables. */
extern void ML_(canonicaliseTables) ( struct _DebugInfo* di );

/* Canonicalise the call-frame-info table held by 'di', in preparation
   for use. This is called by ML_(canonicaliseTables) but can also be
   called on it's own to sort just this table. */
extern void ML_(canonicaliseCFI) ( struct _DebugInfo* di );

/* ------ Searching ------ */

/* Find a symbol-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Word ML_(search_one_symtab) ( struct _DebugInfo* di, Addr ptr,
                                     Bool match_anywhere_in_sym,
                                     Bool findText );

/* Find a location-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Word ML_(search_one_loctab) ( struct _DebugInfo* di, Addr ptr );

/* Find a CFI-table index containing the specified pointer, or -1 if
   not found.  Binary search.  */
extern Word ML_(search_one_cfitab) ( struct _DebugInfo* di, Addr ptr );

/* Find a FPO-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Word ML_(search_one_fpotab) ( struct _DebugInfo* di, Addr ptr );

/* ------ Misc ------ */

/* Show a non-fatal debug info reading error.  Use vg_panic if
   terminal.  'serious' errors are always shown, not 'serious' ones
   are shown only at verbosity level 2 and above. */
extern 
void ML_(symerr) ( struct _DebugInfo* di, Bool serious, HChar* msg );

/* Print a symbol. */
extern void ML_(ppSym) ( Int idx, DiSym* sym );

/* Print a call-frame-info summary. */
extern void ML_(ppDiCfSI) ( XArray* /* of CfiExpr */ exprs, DiCfSI* si );


#define TRACE_SYMTAB(format, args...) \
   if (di->trace_symtab) { VG_(printf)(format, ## args); }


#endif /* ndef __PRIV_STORAGE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
