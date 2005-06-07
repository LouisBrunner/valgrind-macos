
/*--------------------------------------------------------------------*/
/*--- Types and macros for writing syscall wrappers.               ---*/
/*---                                              priv_syscalls.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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
   headers, though: that is the job of the priv_syscalls-*.h headers.
   This header gets included in any file which defines or declares
   wrappers, and as such should only contain stuff which is relevant
   to all such files.
*/

/* ---------------------------------------------------------------------
   Types that are used in syscall wrappers.
   ------------------------------------------------------------------ */

/* Arguments for a syscall. */
typedef
   struct {
      UWord sysno;
      UWord arg1;
      UWord arg2;
      UWord arg3;
      UWord arg4;
      UWord arg5;
      UWord arg6;
   }
   SyscallArgs;

/* Current status of a syscall being done on behalf of the client. */
typedef
   struct {
      enum { SsSuccess=1, SsFailure, SsHandToKernel, SsIdle } what;
      UWord val; /* only meaningful for .what == Success or Failure */
   }
   SyscallStatus;

/* Guest state layout info for syscall args. */
typedef
   struct {
      Int o_sysno;
      Int o_arg1;
      Int o_arg2;
      Int o_arg3;
      Int o_arg4;
      Int o_arg5;
      Int o_arg6;
      Int o_retval;
   }
   SyscallArgLayout;

/* Flags describing syscall wrappers */
#define SfMayBlock   (1 << 1)    /* may block                              */
#define SfPostOnFail (1 << 2)    /* call POST() function on failure        */
/* #define SfPadAddr (1 << 3) */ /* pad+unpad address space around syscall */
#define SfPollAfter  (1 << 4)    /* poll for signals on completion         */
#define SfYieldAfter (1 << 5)    /* yield on completion                    */


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


/* These are defined in the relevant platform-specific files --
   syscalls-arch-os.c */

extern const SyscallTableEntry VGP_(syscall_table)[];

extern const UInt VGP_(syscall_table_size);
   

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
*/

#define DEFN_PRE_TEMPLATE(auxstr, name)                          \
   void vgSysWrap_##auxstr##_##name##_before                     \
                                 ( ThreadId tid,                 \
                                   SyscallArgLayout* layout,     \
                                   /*MOD*/SyscallArgs* args,     \
                                   /*OUT*/SyscallStatus* status, \
                                   /*OUT*/UWord* flags           \
                                 )

#define DEFN_POST_TEMPLATE(auxstr, name)                         \
   void vgSysWrap_##auxstr##_##name##_after                      \
                                 ( ThreadId tid,                 \
                                   SyscallArgs* args,            \
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
                                   /*MOD*/SyscallArgs* args,     \
                                   /*OUT*/SyscallStatus* status, \
                                   /*OUT*/UWord* flags           \
                                 );                              \
   extern                                                        \
   void vgSysWrap_##auxstr##_##name##_after                      \
                                 ( ThreadId tid,                 \
                                   SyscallArgs* args,            \
                                   SyscallStatus* status         \
                                 );



/* Macros for conveniently generating entries in the syscall
   tables.  This first pair are not used directly. */

#define WRAPPER_ENTRY_X_(auxstr, sysno, name) \
   [sysno] = { vgSysWrap_##auxstr##_##name##_before, NULL }
#define WRAPPER_ENTRY_XY(auxstr, sysno, name) \
   [sysno] = { vgSysWrap_##auxstr##_##name##_before, \
               vgSysWrap_##auxstr##_##name##_after }

/* Add a generic wrapper to a syscall table. */
#define GENX_(sysno, name)    WRAPPER_ENTRY_X_(generic, sysno, name)
#define GENXY(sysno, name)    WRAPPER_ENTRY_XY(generic, sysno, name)

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
#define SYSNO  (args->sysno)
#define ARG1   (args->arg1)
#define ARG2   (args->arg2)
#define ARG3   (args->arg3)
#define ARG4   (args->arg4)
#define ARG5   (args->arg5)
#define ARG6   (args->arg6)

/* Reference to the syscall's current result status/value.  Note that
   
   (1) status->val by itself is meaningless -- you have to consider it
       together with status->what, which is why RES uses a helper
       function (this also has the desirable effect of turning RES
       into a non-lvalue).

   (2) post-wrappers will not get called in case of failure (unless
       PostOnFail is set, which is rare).  This is why the assertion
       in getRES is viable.

   If you really really want to just get hold of status->val without
   inspecting status->what, use RES_unchecked.  This is dangerous and
   therefore discouraged.  
*/
#define SUCCESS       (status->what == SsSuccess)
#define FAILURE       (status->what == SsFailure)
#define SWHAT         (status->what)
#define RES_unchecked (status->val)     /* do not use! */
#define RES           (getRES(status))  /* use this instead, if possible */

static inline UWord getRES ( SyscallStatus* st ) {
   vg_assert(st->what == SsSuccess);
   return st->val;
}


/* Set the current result status/value in various ways. */
#define SET_STATUS_Success(zzz)                      \
   do { status->what = SsSuccess;                    \
        status->val  = (zzz);                        \
   } while (0)

#define SET_STATUS_Failure(zzz)                      \
   do { Word wzz = (Word)(zzz);                      \
        /* Catch out wildly bogus error values. */   \
        vg_assert(wzz >= 0 && wzz < 10000);          \
        status->what = SsFailure;                    \
        status->val  = wzz;                          \
   } while (0)

#define SET_STATUS_from_SysRes(zzz)                  \
   do {                                              \
      SysRes zres = (zzz);                           \
      if (zres.isError) {                            \
         SET_STATUS_Failure(zres.val);               \
      } else {                                       \
         SET_STATUS_Success(zres.val);               \
      }                                              \
   } while (0)

#define PRINT(format, args...)                       \
   if (VG_(clo_trace_syscalls))                      \
      VG_(printf)(format, ## args)


/* Macros used to tell tools about uses of scalar arguments.  Note,
   these assume little-endianness.  These can only be used in
   pre-wrappers, and they refer to the layout parameter passed in. */
/* PRRAn == "pre-register-read-argument"
   PRRSN == "pre-register-read-syscall"
*/

#define PRRSN \
      VG_(tdict).track_pre_reg_read(Vg_CoreSysCall, tid, "(syscallno)", \
                                    layout->o_sysno, sizeof(UWord));
#define PRRAn(n,s,t,a) \
      VG_(tdict).track_pre_reg_read(Vg_CoreSysCall, tid, s"("#a")", \
                                    layout->o_arg##n, sizeof(t));
#define PRE_REG_READ0(tr, s) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
   }
#define PRE_REG_READ1(tr, s, t1, a1) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); \
   }
#define PRE_REG_READ2(tr, s, t1, a1, t2, a2) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); \
   }
#define PRE_REG_READ3(tr, s, t1, a1, t2, a2, t3, a3) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
   }
#define PRE_REG_READ4(tr, s, t1, a1, t2, a2, t3, a3, t4, a4) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); \
   }
#define PRE_REG_READ5(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); PRRAn(5,s,t5,a5); \
   }
#define PRE_REG_READ6(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5, t6, a6) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); PRRAn(5,s,t5,a5); PRRAn(6,s,t6,a6); \
   }

#define PRE_MEM_READ(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_read, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define PRE_MEM_RASCIIZ(zzname, zzaddr) \
   VG_TRACK( pre_mem_read_asciiz, Vg_CoreSysCall, tid, zzname, zzaddr)

#define PRE_MEM_WRITE(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_write, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define POST_MEM_WRITE(zzaddr, zzlen) \
   VG_TRACK( post_mem_write, Vg_CoreSysCall, tid, zzaddr, zzlen)


#endif   // __PRIV_TYPES_N_MACROS_H

/*--------------------------------------------------------------------*/
/*--- end                                    priv_types_n_macros.h ---*/
/*--------------------------------------------------------------------*/
