
/*--------------------------------------------------------------------*/
/*--- Types and macros for writing syscall wrappers.               ---*/
/*---                                        priv_types_n_macros.h ---*/
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

#ifndef __PRIV_TYPES_N_MACROS_H
#define __PRIV_TYPES_N_MACROS_H

/* requires #include "pub_core_options.h" */
/* requires #include "pub_core_signals.h" */

/* This header defines types and macros which are useful for writing
   syscall wrappers.  It does not give prototypes for any such
   headers, though: that is the job of the priv_syswrap-*.h headers.
   This header gets included in any file which defines or declares
   wrappers, and as such should only contain stuff which is relevant
   to all such files.
*/

/* ---------------------------------------------------------------------
   Types that are used in syscall wrappers.
   ------------------------------------------------------------------ */

/* Arguments for a syscall. */
typedef
   struct SyscallArgs {
      Word sysno;
      UWord arg1;
      UWord arg2;
      UWord arg3;
      UWord arg4;
      UWord arg5;
      UWord arg6;
      UWord arg7;
      UWord arg8;
   }
   SyscallArgs;

/* Current status of a syscall being done on behalf of the client. */
typedef
   struct SyscallStatus {
      enum { 
         /* call is complete, result is in 'res' */
         SsComplete=1,
         /* syscall not yet completed; must be handed to the kernel */
         SsHandToKernel, 
         /* not currently handling a syscall for this thread */
         SsIdle 
      } what;
      SysRes sres; /* only meaningful for .what == SsComplete */
   }
   SyscallStatus;

/* Guest state layout info for syscall args. */
typedef
   struct {
      // Note that, depending on the platform, arguments may be found in
      // registers or on the stack.  (See the comment at the top of
      // syswrap-main.c for per-platform details.)  For register arguments
      // (which have o_arg field names) the o_arg value is the offset into
      // the vex register state.  For stack arguments (which have s_arg
      // field names), the s_arg value is the offset from the stack pointer.
      Int o_sysno;
#     if defined(VGP_x86_linux) || defined(VGP_amd64_linux) \
         || defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux) \
         || defined(VGP_arm_linux) || defined(VGP_s390x_linux)
      Int o_arg1;
      Int o_arg2;
      Int o_arg3;
      Int o_arg4;
      Int o_arg5;
      Int o_arg6;
      Int uu_arg7;
      Int uu_arg8;
#     elif defined(VGP_x86_darwin)
      Int s_arg1;
      Int s_arg2;
      Int s_arg3;
      Int s_arg4;
      Int s_arg5;
      Int s_arg6;
      Int s_arg7;
      Int s_arg8;
#     elif defined(VGP_amd64_darwin)
      Int o_arg1;
      Int o_arg2;
      Int o_arg3;
      Int o_arg4;
      Int o_arg5;
      Int o_arg6;
      Int s_arg7;
      Int s_arg8;
#     else
#       error "Unknown platform"
#     endif
   }
   SyscallArgLayout;

/* Flags describing syscall wrappers */
#define SfMayBlock      (1 << 1) /* may block                         */
#define SfPostOnFail    (1 << 2) /* call POST() function on failure   */
#define SfPollAfter     (1 << 3) /* poll for signals on completion    */
#define SfYieldAfter    (1 << 4) /* yield on completion               */
#define SfNoWriteResult (1 << 5) /* don't write result to guest state */


/* ---------------------------------------------------------------------
   The syscall table.
   ------------------------------------------------------------------ */

typedef
   struct {
      void (*before) ( ThreadId,
                       SyscallArgLayout*,
                       /*MOD*/SyscallArgs*,
                       /*OUT*/SyscallStatus*,
                       /*OUT*/UWord*
                     );

      void (*after)  ( ThreadId, 
                       SyscallArgs*,
                       SyscallStatus*
                     );
   }
   SyscallTableEntry;

/* Syscall table entries bind __NR_xxx syscall numbers to the PRE/POST
   wrappers for the relevant syscall used in the OS kernel for that
   number.  Note that the constant names don't always match the
   wrapper names in a straightforward way.  For example, on x86/Linux:
      
      __NR_lchown       --> sys_lchown16()
      __NR_lchown32     --> sys_lchown()
      __NR_select       --> old_select()
      __NR__newselect   --> sys_select()
*/


/* A function to find the syscall table entry for a given sysno.  If
   none is found, return NULL.  This used to be done with a single
   fixed sized table exposed to the caller, but that's too inflexible;
   hence now use a function which can do arbitrary messing around to
   find the required entry. */
#if defined(VGO_linux)
extern
SyscallTableEntry* ML_(get_linux_syscall_entry)( UInt sysno );

#elif defined(VGO_darwin)
/* XXX: Darwin still uses the old scheme of exposing the table
   array(s) and size(s) directly to syswrap-main.c.  This should be
   fixed. */

extern const SyscallTableEntry ML_(syscall_table)[];
extern const UInt ML_(syscall_table_size);

#else
#  error Unknown OS
#endif   

/* ---------------------------------------------------------------------
   Declaring and defining wrappers.
   ------------------------------------------------------------------ */

/* Templates for generating the PRE and POST macros -- that is, the
   formal parameter lists for the definitions of wrapper functions.

   Since these names exist in the global namespace, 'auxstr' should
   give an auxiliary string, eg, "generic", "x86_linux", "linux", etc,
   that ensures the names won't clash with other wrappers.

   You should create corresponding global declarations using
   DECL_TEMPLATE (indirectly) below.  

   Note.  The silly name "arrghs" is used rather than just "args"
   because a few wrappers declare the name "args" themselves, and
   renaming those decls can change the name that comes out in error
   messages (on scalar arg checks).  Hence rename this instead.
*/

#define DEFN_PRE_TEMPLATE(auxstr, name)                          \
   void vgSysWrap_##auxstr##_##name##_before                     \
                                 ( ThreadId tid,                 \
                                   SyscallArgLayout* layout,     \
                                   /*MOD*/SyscallArgs* arrghs,   \
                                   /*OUT*/SyscallStatus* status, \
                                   /*OUT*/UWord* flags           \
                                 )

#define DEFN_POST_TEMPLATE(auxstr, name)                         \
   void vgSysWrap_##auxstr##_##name##_after                      \
                                 ( ThreadId tid,                 \
                                   SyscallArgs* arrghs,          \
                                   SyscallStatus* status         \
                                 )


/* This macro generates declarations (prototypes) for wrappers.  It
   declares both the pre-wrapper and the post-wrapper, even though the
   post-wrapper may not actually exist.
*/
#define DECL_TEMPLATE(auxstr, name)                              \
   extern                                                        \
   void vgSysWrap_##auxstr##_##name##_before                     \
                                 ( ThreadId tid,                 \
                                   SyscallArgLayout* layout,     \
                                   /*MOD*/SyscallArgs* arrghs,   \
                                   /*OUT*/SyscallStatus* status, \
                                   /*OUT*/UWord* flags           \
                                 );                              \
   extern                                                        \
   void vgSysWrap_##auxstr##_##name##_after                      \
                                 ( ThreadId tid,                 \
                                   SyscallArgs* arrghs,          \
                                   SyscallStatus* status         \
                                 );



/* Macros for conveniently generating entries in the syscall
   tables.  This first pair are not used directly. */

#define WRAPPER_ENTRY_X_(auxstr, sysno, name) \
   [sysno] = { vgSysWrap_##auxstr##_##name##_before, NULL }
#define WRAPPER_ENTRY_XY(auxstr, sysno, name) \
   [sysno] = { vgSysWrap_##auxstr##_##name##_before, \
               vgSysWrap_##auxstr##_##name##_after }

#define WRAPPER_PRE_NAME(auxstr, name) \
    vgSysWrap_##auxstr##_##name##_before
#define WRAPPER_POST_NAME(auxstr, name) \
    vgSysWrap_##auxstr##_##name##_after

/* Add a generic wrapper to a syscall table. */
#if defined(VGO_linux)
#  define GENX_(sysno, name)  WRAPPER_ENTRY_X_(generic, sysno, name)
#  define GENXY(sysno, name)  WRAPPER_ENTRY_XY(generic, sysno, name)
#elif defined(VGO_darwin)
#  define GENX_(sysno, name)  WRAPPER_ENTRY_X_(generic, VG_DARWIN_SYSNO_INDEX(sysno), name)
#  define GENXY(sysno, name)  WRAPPER_ENTRY_XY(generic, VG_DARWIN_SYSNO_INDEX(sysno), name)
#else
#  error Unknown OS
#endif

/* Add a Linux-specific, arch-independent wrapper to a syscall
   table. */
#define LINX_(sysno, name)    WRAPPER_ENTRY_X_(linux, sysno, name) 
#define LINXY(sysno, name)    WRAPPER_ENTRY_XY(linux, sysno, name)


/* ---------------------------------------------------------------------
   Macros useful for writing wrappers concisely.  These refer to the
   parameters declared by DEFN_{PRE,POST}_TEMPLATE and so in a way do
   not help clarity of understanding.  But they are just too useful to
   omit.
   ------------------------------------------------------------------ */

/* Reference to the syscall's arguments -- the ones which the
   pre-wrapper may have modified, not the original copy. */
#define SYSNO  (arrghs->sysno)
#define ARG1   (arrghs->arg1)
#define ARG2   (arrghs->arg2)
#define ARG3   (arrghs->arg3)
#define ARG4   (arrghs->arg4)
#define ARG5   (arrghs->arg5)
#define ARG6   (arrghs->arg6)
#define ARG7   (arrghs->arg7)
#define ARG8   (arrghs->arg8)

/* Reference to the syscall's current result status/value.  General
   paranoia all round. */
#define SUCCESS       (status->what == SsComplete && !sr_isError(status->sres))
#define FAILURE       (status->what == SsComplete &&  sr_isError(status->sres))
#define SWHAT         (status->what)
#define RES           (getRES(status))
#define RESHI         (getRESHI(status))
#define ERR           (getERR(status))

static inline UWord getRES ( SyscallStatus* st ) {
   vg_assert(st->what == SsComplete);
   vg_assert(!sr_isError(st->sres));
   return sr_Res(st->sres);
}

static inline UWord getRESHI ( SyscallStatus* st ) {
   vg_assert(st->what == SsComplete);
   vg_assert(!sr_isError(st->sres));
   return sr_ResHI(st->sres);
}

static inline UWord getERR ( SyscallStatus* st ) {
   vg_assert(st->what == SsComplete);
   vg_assert(sr_isError(st->sres));
   return sr_Err(st->sres);
}


/* Set the current result status/value in various ways. */
#define SET_STATUS_Success(zzz)                      \
   do { status->what = SsComplete;                   \
        status->sres = VG_(mk_SysRes_Success)(zzz);  \
   } while (0)

#define SET_STATUS_Failure(zzz)                      \
   do { Word wzz = (Word)(zzz);                      \
        /* Catch out wildly bogus error values. */   \
        vg_assert(wzz >= 0 && wzz < 10000);          \
        status->what = SsComplete;                   \
        status->sres = VG_(mk_SysRes_Error)(wzz);    \
   } while (0)

#define SET_STATUS_from_SysRes(zzz)                  \
   do {                                              \
     status->what = SsComplete;                      \
     status->sres = (zzz);                           \
   } while (0)


#define PRINT(format, args...)                       \
   if (VG_(clo_trace_syscalls))                      \
      VG_(printf)(format, ## args)

#define FUSE_COMPATIBLE_MAY_BLOCK()                       \
   if (VG_(strstr)(VG_(clo_sim_hints),"fuse-compatible")) \
      *flags |= SfMayBlock


/* Macros used to tell tools about uses of scalar arguments.  Note,
   these assume little-endianness.  These can only be used in
   pre-wrappers, and they refer to the layout parameter passed in. */
/* PRRSN == "pre-register-read-sysno"
   PRRAn == "pre-register-read-argument"
   PSRAn == "pre-stack-read-argument"
   PRAn  == "pre-read-argument"
*/

#if defined(VGO_linux)
   /* Up to 6 parameters, all in registers. */
#  define PRA1(s,t,a) PRRAn(1,s,t,a)
#  define PRA2(s,t,a) PRRAn(2,s,t,a)
#  define PRA3(s,t,a) PRRAn(3,s,t,a)
#  define PRA4(s,t,a) PRRAn(4,s,t,a)
#  define PRA5(s,t,a) PRRAn(5,s,t,a)
#  define PRA6(s,t,a) PRRAn(6,s,t,a)

#elif defined(VGP_x86_darwin)
   /* Up to 8 parameters, all on the stack. */
#  define PRA1(s,t,a) PSRAn(1,s,t,a)
#  define PRA2(s,t,a) PSRAn(2,s,t,a)
#  define PRA3(s,t,a) PSRAn(3,s,t,a)
#  define PRA4(s,t,a) PSRAn(4,s,t,a)
#  define PRA5(s,t,a) PSRAn(5,s,t,a)
#  define PRA6(s,t,a) PSRAn(6,s,t,a)
#  define PRA7(s,t,a) PSRAn(7,s,t,a)
#  define PRA8(s,t,a) PSRAn(8,s,t,a)

#elif defined(VGP_amd64_darwin)
   /* Up to 8 parameters, 6 in registers, 2 on the stack. */
#  define PRA1(s,t,a) PRRAn(1,s,t,a)
#  define PRA2(s,t,a) PRRAn(2,s,t,a)
#  define PRA3(s,t,a) PRRAn(3,s,t,a)
#  define PRA4(s,t,a) PRRAn(4,s,t,a)
#  define PRA5(s,t,a) PRRAn(5,s,t,a)
#  define PRA6(s,t,a) PRRAn(6,s,t,a)
#  define PRA7(s,t,a) PSRAn(7,s,t,a)
#  define PRA8(s,t,a) PSRAn(8,s,t,a)

#else
#  error Unknown platform
#endif


/* Tell the tool that the syscall number is being read. */
#define PRRSN \
      VG_(tdict).track_pre_reg_read(Vg_CoreSysCall, tid, "(syscallno)", \
                                    layout->o_sysno, sizeof(UWord));

/* REGISTER PARAMETERS */

/* PRRAn: Tell the tool that the register holding the n-th syscall
   argument is being read, at type 't' which must be at most the size
   of a register but can be smaller.  In the latter case we need to be
   careful about endianness. */

/* little-endian: the part of the guest state being read is
      let here = offset_of_reg
      in  [here .. here + sizeof(t) - 1]
   since the least significant parts of the guest register are stored
   in memory at the lowest address.
*/
#define PRRAn_LE(n,s,t,a)                          \
   do {                                            \
      Int here = layout->o_arg##n;                 \
      vg_assert(sizeof(t) <= sizeof(UWord));       \
      vg_assert(here >= 0);                        \
      VG_(tdict).track_pre_reg_read(               \
         Vg_CoreSysCall, tid, s"("#a")",           \
         here, sizeof(t)                           \
      );                                           \
   } while (0)

/* big-endian: the part of the guest state being read is
      let next = offset_of_reg + sizeof(reg) 
      in  [next - sizeof(t) .. next - 1]
   since the least significant parts of the guest register are stored
   in memory at the highest address.
*/
#define PRRAn_BE(n,s,t,a)                          \
   do {                                            \
      Int here = layout->o_arg##n;                 \
      Int next = layout->o_arg##n + sizeof(UWord); \
      vg_assert(sizeof(t) <= sizeof(UWord));       \
      vg_assert(here >= 0);                        \
      VG_(tdict).track_pre_reg_read(               \
         Vg_CoreSysCall, tid, s"("#a")",           \
         next-sizeof(t), sizeof(t)                 \
      );                                           \
   } while (0)

#if defined(VG_BIGENDIAN)
#  define PRRAn(n,s,t,a) PRRAn_BE(n,s,t,a)
#elif defined(VG_LITTLEENDIAN)
#  define PRRAn(n,s,t,a) PRRAn_LE(n,s,t,a)
#else
#  error "Unknown endianness"
#endif


/* STACK PARAMETERS */

/* PSRAn: Tell the tool that the memory holding the n-th syscall
   argument is being read, at type 't' which must be at most the size
   of a register but can be smaller.  In the latter case we need to be
   careful about endianness. */

/* little-endian: the part of the guest state being read is
      let here = offset_of_reg
      in  [here .. here + sizeof(t) - 1]
   since the least significant parts of the guest register are stored
   in memory at the lowest address.
*/
#define PSRAn_LE(n,s,t,a)                          \
   do {                                            \
      Addr here = layout->s_arg##n + VG_(get_SP)(tid); \
      vg_assert(sizeof(t) <= sizeof(UWord));       \
      VG_(tdict).track_pre_mem_read(               \
         Vg_CoreSysCallArgInMem, tid, s"("#a")",   \
         here, sizeof(t)                           \
      );                                           \
   } while (0)

/* big-endian: the part of the guest state being read is
      let next = offset_of_reg + sizeof(reg) 
      in  [next - sizeof(t) .. next - 1]
   since the least significant parts of the guest register are stored
   in memory at the highest address.
*/
#define PSRAn_BE(n,s,t,a)                                         \
   do {                                                           \
      Addr next = layout->o_arg##n + sizeof(UWord) +              \
                  VG_(threads)[tid].arch.vex.VG_STACK_PTR;        \
      vg_assert(sizeof(t) <= sizeof(UWord));                      \
      VG_(tdict).track_pre_mem_read(                              \
         Vg_CoreSysCallArgInMem, tid, s"("#a")",                  \
         next-sizeof(t), sizeof(t)                                \
      );                                                          \
   } while (0)

#if defined(VG_BIGENDIAN)
#  define PSRAn(n,s,t,a) PSRAn_BE(n,s,t,a)
#elif defined(VG_LITTLEENDIAN)
#  define PSRAn(n,s,t,a) PSRAn_LE(n,s,t,a)
#else
#  error "Unknown endianness"
#endif


#define PRE_REG_READ0(tr, s) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
   }
#define PRE_REG_READ1(tr, s, t1, a1) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1);                            \
   }
#define PRE_REG_READ2(tr, s, t1, a1, t2, a2) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1); PRA2(s,t2,a2);           \
   }
#define PRE_REG_READ3(tr, s, t1, a1, t2, a2, t3, a3) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1); PRA2(s,t2,a2); PRA3(s,t3,a3);  \
   }
#define PRE_REG_READ4(tr, s, t1, a1, t2, a2, t3, a3, t4, a4) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1); PRA2(s,t2,a2); PRA3(s,t3,a3);  \
      PRA4(s,t4,a4);                                    \
   }
#define PRE_REG_READ5(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1); PRA2(s,t2,a2); PRA3(s,t3,a3);  \
      PRA4(s,t4,a4); PRA5(s,t5,a5);                   \
   }
#define PRE_REG_READ6(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5, t6, a6) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1); PRA2(s,t2,a2); PRA3(s,t3,a3);   \
      PRA4(s,t4,a4); PRA5(s,t5,a5); PRA6(s,t6,a6);   \
   }
#define PRE_REG_READ7(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5, t6, a6, t7, a7) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1); PRA2(s,t2,a2); PRA3(s,t3,a3);   \
      PRA4(s,t4,a4); PRA5(s,t5,a5); PRA6(s,t6,a6);   \
      PRA7(s,t7,a7);                                     \
   }

#define PRE_REG_READ8(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5, t6, a6, t7, a7, t8, a8) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRA1(s,t1,a1); PRA2(s,t2,a2); PRA3(s,t3,a3);   \
      PRA4(s,t4,a4); PRA5(s,t5,a5); PRA6(s,t6,a6);   \
      PRA7(s,t7,a7); PRA8(s,t8,a8);                    \
   }

#define PRE_MEM_READ(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_read, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define PRE_MEM_RASCIIZ(zzname, zzaddr) \
   VG_TRACK( pre_mem_read_asciiz, Vg_CoreSysCall, tid, zzname, zzaddr)

#define PRE_MEM_WRITE(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_write, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define POST_MEM_WRITE(zzaddr, zzlen) \
   VG_TRACK( post_mem_write, Vg_CoreSysCall, tid, zzaddr, zzlen)


#define PRE_FIELD_READ(zzname, zzfield) \
    PRE_MEM_READ(zzname, (UWord)&zzfield, sizeof(zzfield))

#define PRE_FIELD_WRITE(zzname, zzfield) \
    PRE_MEM_WRITE(zzname, (UWord)&zzfield, sizeof(zzfield))

#define POST_FIELD_WRITE(zzfield) \
    POST_MEM_WRITE((UWord)&zzfield, sizeof(zzfield))


#endif   // __PRIV_TYPES_N_MACROS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
