
/*--------------------------------------------------------------------*/
/*--- The built-in profiler.                    pub_tool_profile.h ---*/
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

#ifndef __PUB_TOOL_PROFILE_H
#define __PUB_TOOL_PROFILE_H

// Define this to turn on profiling.
//#define  VG_DO_PROFILING   1

/* Override the empty definitions from tool.h */
#ifdef VG_DO_PROFILING
#  define VGP_PUSHCC(x)   if (VG_(clo_profile)) VG_(pushcc)(x)
#  define VGP_POPCC(x)    if (VG_(clo_profile)) VG_(popcc)(x)
#else
#  define VGP_PUSHCC(x)
#  define VGP_POPCC(x)
#endif

/* Nb: VG_(register_profile_event)() relies on VgpUnc being the first one */
#define VGP_CORE_LIST \
   /* These ones depend on the core */                \
   VGP_PAIR(VgpUnc,         "unclassified"),          \
   VGP_PAIR(VgpStartup,     "startup"),               \
   VGP_PAIR(VgpRun,         "running"),               \
   VGP_PAIR(VgpSched,       "scheduler"),             \
   VGP_PAIR(VgpMalloc,      "low-lev malloc/free"),   \
   VGP_PAIR(VgpCliMalloc,   "client  malloc/free"),   \
   VGP_PAIR(VgpTranslate,   "translate-main"),        \
   VGP_PAIR(VgpVexTime,     "Vex-time"),              \
   VGP_PAIR(VgpImprove,     "improve"),               \
   VGP_PAIR(VgpESPUpdate,   "ESP-update"),            \
   VGP_PAIR(VgpSlowFindT,   "slow-search-transtab"),  \
   VGP_PAIR(VgpExeContext,  "exe-context"),           \
   VGP_PAIR(VgpReadSyms,    "read-syms"),             \
   VGP_PAIR(VgpSearchSyms,  "search-syms"),           \
   VGP_PAIR(VgpAddToT,      "add-to-transtab"),       \
   VGP_PAIR(VgpCoreSysWrap, "core-syscall-wrapper"),  \
   VGP_PAIR(VgpDemangle,    "demangle"),              \
   VGP_PAIR(VgpCoreCheapSanity,     "core-cheap-sanity"),     \
   VGP_PAIR(VgpCoreExpensiveSanity, "core-expensive-sanity"), \
   /* These ones depend on the tool */                \
   VGP_PAIR(VgpPreCloInit,  "pre-clo-init"),          \
   VGP_PAIR(VgpPostCloInit, "post-clo-init"),         \
   VGP_PAIR(VgpInstrument,  "instrument"),            \
   VGP_PAIR(VgpToolSysWrap, "tool-syscall-wrapper"),  \
   VGP_PAIR(VgpToolCheapSanity,     "tool-cheap-sanity"),     \
   VGP_PAIR(VgpToolExpensiveSanity, "tool-expensive-sanity"), \
   VGP_PAIR(VgpFini,        "fini")

#define VGP_PAIR(n,name) n
typedef enum { VGP_CORE_LIST } VgpCoreCC;
#undef  VGP_PAIR

/* When registering tool profiling events, ensure that the 'n' value is in
 * the range (VgpFini+1..) */
extern void VG_(register_profile_event) ( Int n, Char* name );

extern void VG_(pushcc) ( UInt cc );
extern void VG_(popcc)  ( UInt cc );

#endif   // __PUB_TOOL_PROFILE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
