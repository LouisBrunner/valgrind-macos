
/*--------------------------------------------------------------------*/
/*--- Startup: create initial process image on Solaris             ---*/
/*---                                            initimg-solaris.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Petr Pavlu
      setup@dagobah.cz

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

/* Copyright 2013-2017, Ivo Raisr <ivosh@ivosh.net>. */

#if defined(VGO_solaris)

/* Note: This file is based on initimg-linux.c. */

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_mallocfree.h"
#include "pub_core_machine.h"
#include "pub_core_ume.h"
#include "pub_core_options.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"       /* VG_TRACK */
#include "pub_core_threadstate.h"     /* ThreadArchState */
#include "pub_core_pathscan.h"        /* find_executable */
#include "pub_core_initimg.h"         /* self */


/*====================================================================*/
/*=== Loading the client                                           ===*/
/*====================================================================*/

/* Load the client whose name is VG_(argv_the_exename). */
static void load_client(/*OUT*/ExeInfo *info,
                        /*OUT*/HChar *out_exe_name, SizeT out_exe_name_size)
{
   const HChar *exe_name;
   Int ret;
   SysRes res;

   vg_assert(VG_(args_the_exename));
   exe_name = VG_(find_executable)(VG_(args_the_exename));

   if (!exe_name) {
      VG_(printf)("valgrind: %s: command not found\n", VG_(args_the_exename));
      /* Return POSIX's NOTFOUND. */
      VG_(exit)(127);
      /*NOTREACHED*/
   }

   VG_(memset)(info, 0, sizeof(*info));
   ret = VG_(do_exec)(exe_name, info);
   if (ret < 0) {
      VG_(printf)("valgrind: could not execute '%s'\n", exe_name);
      VG_(exit)(1);
      /*NOTREACHED*/
   }

   /* The client was successfully loaded!  Continue. */

   /* Save resolved exename. */
   if (VG_(strlen)(exe_name) + 1 > out_exe_name_size) {
      /* This should not really happen. */
      VG_(printf)("valgrind: execname %s is too long\n", exe_name);
      VG_(exit)(1);
      /*NOTREACHED*/
   }
   VG_(strcpy)(out_exe_name, exe_name);

   /* Get hold of a file descriptor which refers to the client executable.
      This is needed for attaching to GDB. */
   res = VG_(open)(exe_name, VKI_O_RDONLY, VKI_S_IRUSR);
   if (!sr_isError(res))
      VG_(cl_exec_fd) = sr_Res(res);

   /* Set initial brk values. */
   if (info->ldsoexec) {
      VG_(brk_base) = VG_(brk_limit) = -1;
   } else {
      VG_(brk_base) = VG_(brk_limit) = info->brkbase;
   }
}


/*====================================================================*/
/*=== Setting up the client's environment                          ===*/
/*====================================================================*/

/* Prepare the client's environment.  This is basically a copy of our
   environment, except:

     LD_PRELOAD=$VALGRIND_LIB/vgpreload_core-PLATFORM.so:
                ($VALGRIND_LIB/vgpreload_TOOL-PLATFORM.so:)?
                $LD_PRELOAD

   If this is missing, then it is added.

   Also, remove any binding for VALGRIND_LAUNCHER=.  The client should not be
   able to see this.

   If this needs to handle any more variables it should be hacked into
   something table driven.  The copy is VG_(malloc)'d space.
*/
static HChar **setup_client_env(HChar **origenv, const HChar *toolname)
{
   const HChar *ld_preload = "LD_PRELOAD=";
   SizeT ld_preload_len = VG_(strlen)(ld_preload);
   Bool ld_preload_done = False;
   SizeT vglib_len = VG_(strlen)(VG_(libdir));

   HChar **cpp;
   HChar **ret;
   HChar *preload_tool_path;
   SizeT envc, i;

   /* Alloc space for the
        <path>/vgpreload_core-<platform>.so and
        <path>/vgpreload_<tool>-<platform>.so
      paths.  We might not need the space for the tool path, but it doesn't
      hurt to over-allocate briefly.  */
   SizeT preload_core_path_size = vglib_len + sizeof("/vgpreload_core-") - 1
                                            + sizeof(VG_PLATFORM) - 1
                                            + sizeof(".so");
   SizeT preload_tool_path_size = vglib_len + sizeof("/vgpreload_") - 1
                                            + VG_(strlen)(toolname) + 1 /*-*/
                                            + sizeof(VG_PLATFORM) - 1
                                            + sizeof(".so");
   SizeT preload_string_size = preload_core_path_size
                               + preload_tool_path_size;
   HChar *preload_string = VG_(malloc)("initimg-solaris.sce.1",
                                       preload_string_size);

   /* Check that the parameters are sane. */
   vg_assert(origenv);
   vg_assert(toolname);

   /* Determine if there's a vgpreload_<tool>-<platform>.so file, and setup
      preload_string. */
   preload_tool_path = VG_(malloc)("initimg-solaris.sce.2",
                                   preload_tool_path_size);
   VG_(sprintf)(preload_tool_path, "%s/vgpreload_%s-%s.so", VG_(libdir),
                toolname, VG_PLATFORM);
   if (!VG_(access)(preload_tool_path, True/*r*/, False/*w*/, False/*x*/)) {
      /* The tool's .so exists, put it into LD_PRELOAD with the core's so. */
      VG_(sprintf)(preload_string, "%s/vgpreload_core-%s.so:%s", VG_(libdir),
                   VG_PLATFORM, preload_tool_path);
   }
   else {
      /* The tool's .so doesn't exist, put only the core's .so into
         LD_PRELOAD. */
      VG_(sprintf)(preload_string, "%s/vgpreload_core-%s.so", VG_(libdir),
                   VG_PLATFORM);
   }
   VG_(free)(preload_tool_path);

   VG_(debugLog)(2, "initimg", "preload_string:\n");
   VG_(debugLog)(2, "initimg", "  \"%s\"\n", preload_string);

   /* Count the original size of the env. */
   envc = 0;
   for (cpp = origenv; *cpp; cpp++)
      envc++;

   /* Allocate a new space, envc + 1 new entry + NULL. */
   ret = VG_(malloc)("initimg-solaris.sce.3", sizeof(HChar*) * (envc + 1 + 1));

   /* Copy it over. */
   for (cpp = ret; *origenv; )
      *cpp++ = *origenv++;
   *cpp = NULL;

   vg_assert(envc == cpp - ret);

   /* Walk over the new environment, mashing as we go. */
   for (cpp = ret; *cpp; cpp++) {
      if (VG_(memcmp)(*cpp, ld_preload, ld_preload_len))
         continue;

      /* LD_PRELOAD entry found, smash it. */
      SizeT size = VG_(strlen)(*cpp) + 1 /*:*/
                                     + preload_string_size;
      HChar *cp = VG_(malloc)("initimg-solaris.sce.4", size);

      VG_(sprintf)(cp, "%s%s:%s", ld_preload, preload_string,
                   (*cpp) + ld_preload_len);
      *cpp = cp;

      ld_preload_done = True;
   }

   /* Add the missing bits. */
   if (!ld_preload_done) {
      SizeT size = ld_preload_len + preload_string_size;
      HChar *cp = VG_(malloc)("initimg-solaris.sce.5", size);

      VG_(sprintf)(cp, "%s%s", ld_preload, preload_string);
      ret[envc++] = cp;
   }

   /* We've got ret[0 .. envc-1] live now. */

   /* Find and remove a binding for VALGRIND_LAUNCHER. */
   {
      const HChar *v_launcher = VALGRIND_LAUNCHER "=";
      SizeT v_launcher_len = VG_(strlen)(v_launcher);

      for (i = 0; i < envc; i++)
         if (!VG_(memcmp)(ret[i], v_launcher, v_launcher_len)) {
            /* VALGRIND_LAUNCHER was found. */
            break;
         }

      if (i < envc) {
         /* VALGRIND_LAUNCHER was found, remove it. */
         for (; i < envc - 1; i++)
            ret[i] = ret[i + 1];
         envc--;
      }
   }

   VG_(free)(preload_string);
   ret[envc] = NULL;

   return ret;
}


/*====================================================================*/
/*=== Setting up the client's stack                                ===*/
/*====================================================================*/

/* Add a string onto the string table, and return its address. */
static HChar *copy_str(HChar **tab, const HChar *str)
{
   HChar *cp = *tab;
   HChar *orig = cp;

   while (*str)
      *cp++ = *str++;
   *cp++ = '\0';

   *tab = cp;

   return orig;
}

#if defined(SOLARIS_RESERVE_SYSSTAT_ADDR) || \
    defined(SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR)
#define ORIG_AUXV_PRESENT 1
#endif

#if defined(ORIG_AUXV_PRESENT)
/* The auxiliary vector might not be present. So we cross-check pointers from
   argv and envp pointing to the string table. */ 
static vki_auxv_t *find_original_auxv(Addr init_sp)
{
   HChar **sp = (HChar **) init_sp;
   HChar *lowest_str_ptr = (HChar *) UINTPTR_MAX; // lowest ptr to string table

   sp++; // skip argc

   while (*sp != NULL) { // skip argv
      if (*sp < lowest_str_ptr)
         lowest_str_ptr = *sp;
      sp++;
   }
   sp++;

   while (*sp != 0) { // skip env
      if (*sp < lowest_str_ptr)
         lowest_str_ptr = *sp;
      sp++;
   }
   sp++;

   if ((Addr) sp < (Addr) lowest_str_ptr) {
      return (vki_auxv_t *) sp;
   } else {
      return NULL;
   }
}

static void copy_auxv_entry(const vki_auxv_t *orig_auxv, Int type,
                            const HChar *type_name, vki_auxv_t *auxv)
{
   vg_assert(auxv != NULL);

   if (orig_auxv == NULL) {
      VG_(printf)("valgrind: Cannot locate auxiliary vector.\n");
      VG_(printf)("valgrind: Cannot continue. Sorry.\n\n");
      VG_(exit)(1);
   }

   for ( ; orig_auxv->a_type != VKI_AT_NULL; orig_auxv++) {
      if (orig_auxv->a_type == type) {
         auxv->a_type = type;
         auxv->a_un.a_val = orig_auxv->a_un.a_val;
         return;
      }
   }

   VG_(printf)("valgrind: Cannot locate %s in the aux\n", type_name);
   VG_(printf)("valgrind: vector. Cannot continue. Sorry.\n\n");
   VG_(exit)(1);
}
#endif /* ORIG_AUXV_PRESENT */

/* This sets up the client's initial stack, containing the args,
   environment and aux vector.

   The format of the stack is:

   higher address +-----------------+ <- clstack_end
                  |                 |
                  : string table    :
                  |                 |
                  +-----------------+
                  | AT_NULL         |
                  -                 -
                  | auxv            |
                  +-----------------+
                  | NULL            |
                  -                 -
                  | envp            |
                  +-----------------+
                  | NULL            |
                  -                 -
                  | argv            |
                  +-----------------+
                  | argc            |
   lower address  +-----------------+ <- sp
                  | undefined       |
                  :                 :

   Allocate and create the initial client stack.  It is allocated down from
   clstack_end, which was previously determined by the address space manager.
   The returned value is the SP value for the client.

   Note that auxiliary vector is *not* created by kernel on illumos and
   Solaris 11 if the program is statically linked (which is our case).
   Although we now taught Solaris 11.4 to create the auxiliary vector, we still
   have to build auxv from scratch, to make the code consistent. */

static Addr setup_client_stack(Addr init_sp,
                               HChar **orig_envp,
                               const ExeInfo *info,
                               Addr clstack_end,
                               SizeT clstack_max_size,
                               const HChar *resolved_exe_name)
{
   SysRes res;
   HChar **cpp;
   HChar *strtab;       /* string table */
   HChar *stringbase;
   Addr *ptr;
   vki_auxv_t *auxv;
   SizeT stringsize;    /* total size of strings in bytes */
   SizeT auxsize;       /* total size of auxv in bytes */
   Int argc;            /* total argc */
   Int envc;            /* total number of env vars */
   SizeT stacksize;     /* total client stack size */
   Addr client_SP;      /* client stack base (initial SP) */
   Addr clstack_start;
   Int i;

   vg_assert(VG_IS_PAGE_ALIGNED(clstack_end + 1));
   vg_assert(VG_(args_the_exename));
   vg_assert(VG_(args_for_client));

#  if defined(ORIG_AUXV_PRESENT)
   /* Get the original auxv (if any). */
   vki_auxv_t *orig_auxv = find_original_auxv(init_sp);
#  endif /* ORIG_AUXV_PRESENT */

   /* ==================== compute sizes ==================== */

   /* First of all, work out how big the client stack will be. */
   stringsize = 0;

   /* Paste on the extra args if the loader needs them (i.e. the #!
      interpreter and its argument). */
   argc = 0;
   if (info->interp_name) {
      argc++;
      stringsize += VG_(strlen)(info->interp_name) + 1;
   }
   if (info->interp_args) {
      argc++;
      stringsize += VG_(strlen)(info->interp_args) + 1;
   }

   /* Now scan the args we're given... */
   argc++;
   stringsize += VG_(strlen)(VG_(args_the_exename)) + 1;
   for (i = 0; i < VG_(sizeXA)(VG_(args_for_client)); i++) {
      argc++;
      stringsize += VG_(strlen)(*(HChar**)
                                  VG_(indexXA)(VG_(args_for_client), i)) + 1;
   }

   /* ...and the environment. */
   envc = 0;
   for (cpp = orig_envp; *cpp; cpp++) {
      envc++;
      stringsize += VG_(strlen)(*cpp) + 1;
   }

   /* Now, how big is the auxv?

      AT_SUN_PLATFORM
      AT_SUN_EXECNAME
      AT_PHDR            (not for elfs with no PT_PHDR, such as ld.so.1)
      AT_BASE
      AT_ENTRY
      AT_FLAGS
      AT_PAGESZ
      AT_SUN_AUXFLAFGS
      AT_SUN_HWCAP
      AT_SUN_SYSSTAT_ADDR      (if supported)
      AT_SUN_SYSSTAT_ZONE_ADDR (if supported)
      AT_NULL

      It would be possible to also add AT_PHENT, AT_PHNUM, AT_SUN_LDDATA,
      but they don't seem to be so important. */
   auxsize = 10 * sizeof(*auxv);
#  if defined(SOLARIS_RESERVE_SYSSTAT_ADDR)
   auxsize += sizeof(*auxv);
#  endif
#  if defined(SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR)
   auxsize += sizeof(*auxv);
#  endif

#  if defined(VGA_x86) || defined(VGA_amd64)
   /* AT_SUN_PLATFORM string. */
   stringsize += VG_(strlen)("i86pc") + 1;
#  else
#    error "Unknown architecture"
#  endif
   /* AT_SUN_EXECNAME string. */
   stringsize += VG_(strlen)(resolved_exe_name) + 1;

   /* Calculate how big the client stack is. */
   stacksize =
      sizeof(Word) +                            /* argc */
      sizeof(HChar**) +                         /* argc[0] == exename */
      sizeof(HChar**) * argc +                  /* argv */
      sizeof(HChar**) +                         /* terminal NULL */
      sizeof(HChar**) * envc +                  /* envp */
      sizeof(HChar**) +                         /* terminal NULL */
      auxsize +                                 /* auxv */
      VG_ROUNDUP(stringsize, sizeof(Word));     /* strings (aligned) */

   /* The variable client_SP is the client's stack pointer. */
   client_SP = clstack_end - stacksize;
   client_SP = VG_ROUNDDN(client_SP, 16); /* Make stack 16 byte aligned. */

   /* Calculate base of the string table (aligned). */
   stringbase = (HChar*)clstack_end - VG_ROUNDUP(stringsize, sizeof(Int));
   strtab = stringbase;

   clstack_start = VG_PGROUNDDN(client_SP);

   /* Calculate the max stack size. */
   clstack_max_size = VG_PGROUNDUP(clstack_max_size);

   /* Record stack extent -- needed for stack-change code. */
   VG_(clstk_start_base) = clstack_start;
   VG_(clstk_end) = clstack_end;
   VG_(clstk_max_size) = clstack_max_size;

   if (0)
      VG_(printf)("stringsize=%lu, auxsize=%lu, stacksize=%lu, maxsize=%#lx\n"
                  "clstack_start %#lx\n"
                  "clstack_end   %#lx\n",
                  stringsize, auxsize, stacksize, clstack_max_size,
                  clstack_start, clstack_end);

   /* ==================== allocate space ==================== */

   {
      SizeT anon_size = clstack_end - clstack_start + 1;
      SizeT resvn_size = clstack_max_size - anon_size;
      Addr anon_start = clstack_start;
      Addr resvn_start = anon_start - resvn_size;
      SizeT inner_HACK = 0;
      Bool ok;

      /* So far we've only accounted for space requirements down to the stack
         pointer.  If this target's ABI requires a redzone below the stack
         pointer, we need to allocate an extra page, to handle the worst case
         in which the stack pointer is almost at the bottom of a page, and so
         there is insufficient room left over to put the redzone in.  In this
         case the simple thing to do is allocate an extra page, by shrinking
         the reservation by one page and growing the anonymous area by a
         corresponding page. */
      vg_assert(VG_STACK_REDZONE_SZB >= 0);
      vg_assert(VG_STACK_REDZONE_SZB < VKI_PAGE_SIZE);
      if (VG_STACK_REDZONE_SZB > 0) {
         vg_assert(resvn_size > VKI_PAGE_SIZE);
         resvn_size -= VKI_PAGE_SIZE;
         anon_start -= VKI_PAGE_SIZE;
         anon_size += VKI_PAGE_SIZE;
      }

      vg_assert(VG_IS_PAGE_ALIGNED(anon_size));
      vg_assert(VG_IS_PAGE_ALIGNED(resvn_size));
      vg_assert(VG_IS_PAGE_ALIGNED(anon_start));
      vg_assert(VG_IS_PAGE_ALIGNED(resvn_start));
      vg_assert(resvn_start == clstack_end + 1 - clstack_max_size);

#     ifdef ENABLE_INNER
      /* Create 1M non-fault-extending stack. */
      inner_HACK = 1024 * 1024;
#     endif

      if (0)
         VG_(printf)("resvn_start=%#lx, resvn_size=%#lx\n"
                     "anon_start=%#lx, anon_size=%#lx\n",
                     resvn_start, resvn_size, anon_start, anon_size);

      /* Create a shrinkable reservation followed by an anonymous segment.
         Together these constitute a growdown stack. */
      ok = VG_(am_create_reservation)(resvn_start,
                                      resvn_size - inner_HACK,
                                      SmUpper,
                                      anon_size + inner_HACK);
      if (ok) {
         /* Allocate a stack - mmap enough space for the stack. */
         res = VG_(am_mmap_anon_fixed_client)(anon_start - inner_HACK,
                                              anon_size + inner_HACK,
                                              info->stack_prot);
      }
      if (!ok || sr_isError(res)) {
         /* Allocation of the stack failed.  We have to stop. */
         VG_(printf)("valgrind: "
                     "I failed to allocate space for the application's stack.\n");
         VG_(printf)("valgrind: "
                     "This may be the result of a very large --main-stacksize=\n");
         VG_(printf)("valgrind: setting.  Cannot continue.  Sorry.\n\n");
         VG_(exit)(1);
         /*NOTREACHED*/
      }
   }

   /* ==================== create client stack ==================== */

   ptr = (Addr*)client_SP;

   /* Copy-out client argc. */
   *ptr++ = argc;

   /* Copy-out client argv. */
   if (info->interp_name)
      *ptr++ = (Addr)copy_str(&strtab, info->interp_name);
   if (info->interp_args)
      *ptr++ = (Addr)copy_str(&strtab, info->interp_args);

   *ptr++ = (Addr)copy_str(&strtab, VG_(args_the_exename));
   for (i = 0; i < VG_(sizeXA)(VG_(args_for_client)); i++)
      *ptr++ = (Addr)copy_str(
                  &strtab, *(HChar**) VG_(indexXA)(VG_(args_for_client), i));
   *ptr++ = 0;

   /* Copy-out envp. */
   VG_(client_envp) = (HChar**)ptr;
   for (cpp = orig_envp; *cpp; ptr++, cpp++)
      *ptr = (Addr)copy_str(&strtab, *cpp);
   *ptr++ = 0;

   /* Create aux vector. */
   auxv = (auxv_t*)ptr;
   VG_(client_auxv) = (UWord*)ptr;

   /* AT_SUN_PLATFORM */
   auxv->a_type = VKI_AT_SUN_PLATFORM;
#  if defined(VGA_x86) || defined(VGA_amd64)
   auxv->a_un.a_ptr = copy_str(&strtab, "i86pc");
#  else
#    error "Unknown architecture"
#  endif
   auxv++;

   /* AT_SUN_EXECNAME */
   auxv->a_type = VKI_AT_SUN_EXECNAME;
   auxv->a_un.a_ptr = copy_str(&strtab, resolved_exe_name);
   auxv++;

   /* AT_PHDR */
   if ((info->real_phdr_present) && (info->phdr != 0)) {
      auxv->a_type = VKI_AT_PHDR;
      auxv->a_un.a_val = info->phdr;
      auxv++;
   }

   /* AT_BASE */
   auxv->a_type = VKI_AT_BASE;
   auxv->a_un.a_val = info->interp_offset;
   auxv++;

   /* AT_ENTRY */
   auxv->a_type = VKI_AT_ENTRY;
   auxv->a_un.a_val = info->entry;
   auxv++;

   /* AT_FLAGS */
   auxv->a_type = VKI_AT_FLAGS;
#  if defined(VGA_x86) || defined(VGA_amd64)
   auxv->a_un.a_val = 0; /* 0 on i86pc */
#  else
#    error "Unknown architecture"
#  endif
   auxv++;

   /* AT_PAGESZ */
   auxv->a_type = VKI_AT_PAGESZ;
   auxv->a_un.a_val = VKI_PAGE_SIZE;
   auxv++;

   /* AT_SUN_AUXFLAFGS */
   auxv->a_type = VKI_AT_SUN_AUXFLAGS;
   /* XXX Handle AF_SUN_SETUGID? */
   auxv->a_un.a_val = VKI_AF_SUN_HWCAPVERIFY;
   auxv++;

   /* AT_SUN_HWCAP */
   {
      VexArch vex_arch;
      VexArchInfo vex_archinfo;
      UInt hwcaps;

      VG_(machine_get_VexArchInfo)(&vex_arch, &vex_archinfo);

#     if defined(VGA_x86)
      vg_assert(vex_arch == VexArchX86);

      /* Set default hwcaps. */
      hwcaps =
           VKI_AV_386_FPU       /* x87-style floating point */
         | VKI_AV_386_TSC       /* rdtsc insn */
         | VKI_AV_386_CX8       /* cmpxchg8b insn */
         | VKI_AV_386_SEP       /* sysenter and sysexit */
         | VKI_AV_386_AMD_SYSC  /* AMD's syscall and sysret */
         | VKI_AV_386_CMOV      /* conditional move insns */
         | VKI_AV_386_MMX       /* MMX insn */
         | VKI_AV_386_AHF;      /* lahf/sahf insns */

      /* Handle additional hwcaps. */
      if (vex_archinfo.hwcaps & VEX_HWCAPS_X86_SSE1)
         hwcaps |=
              VKI_AV_386_FXSR   /* fxsave and fxrstor */
            | VKI_AV_386_SSE;   /* SSE insns and regs  */
      if (vex_archinfo.hwcaps & VEX_HWCAPS_X86_SSE2) {
         vg_assert(vex_archinfo.hwcaps & VEX_HWCAPS_X86_SSE1);
         hwcaps |=
              VKI_AV_386_SSE2;  /* SSE2 insns and regs */
      }
      if (vex_archinfo.hwcaps & VEX_HWCAPS_X86_SSE3) {
         vg_assert(vex_archinfo.hwcaps & VEX_HWCAPS_X86_SSE2);
         hwcaps |=
              VKI_AV_386_SSE3   /* SSE3 insns and regs */
            | VKI_AV_386_SSSE3; /* Intel SSSE3 insns */
      }
      if (vex_archinfo.hwcaps & VEX_HWCAPS_X86_LZCNT)
         hwcaps |=
              VKI_AV_386_AMD_LZCNT; /* AMD's LZCNT insn */

      /* No support for:
         AV_386_AMD_MMX         AMD's MMX insns
         AV_386_AMD_3DNow       AMD's 3Dnow! insns
         AV_386_AMD_3DNowx      AMD's 3Dnow! extended insns
         AV_386_CX16            cmpxchg16b insn
         AV_386_TSCP            rdtscp instruction
         AV_386_AMD_SSE4A       AMD's SSE4A insns
         AV_386_POPCNT          POPCNT insn
         AV_386_SSE4_1          Intel SSE4.1 insns
         AV_386_SSE4_2          Intel SSE4.2 insns
         AV_386_MOVBE           Intel MOVBE insns
         AV_386_AES             Intel AES insns
         AV_386_PCLMULQDQ       Intel PCLMULQDQ insn
         AV_386_XSAVE           Intel XSAVE/XRSTOR insns
         AV_386_AVX             Intel AVX insns
         illumos only:
            AV_386_VMX          Intel VMX support
            AV_386_AMD_SVM      AMD SVM support
         solaris only:
            AV_386_AMD_XOP      AMD XOP insns
            AV_386_AMD_FMA4     AMD FMA4 insns */

#     elif defined(VGA_amd64)
      vg_assert(vex_arch == VexArchAMD64);

      /* Set default hwcaps. */
      hwcaps =
           VKI_AV_386_FPU       /* x87-style floating point */
         | VKI_AV_386_TSC       /* rdtsc insn */
         | VKI_AV_386_CX8       /* cmpxchg8b insn */
         | VKI_AV_386_AMD_SYSC  /* AMD's syscall and sysret */
         | VKI_AV_386_CMOV      /* conditional move insns */
         | VKI_AV_386_MMX       /* MMX insn */
         | VKI_AV_386_AHF       /* lahf/sahf insns */
         | VKI_AV_386_FXSR      /* fxsave and fxrstor */
         | VKI_AV_386_SSE       /* SSE insns and regs  */
         | VKI_AV_386_SSE2;     /* SSE2 insns and regs */

      /* Handle additional hwcaps. */
      if (vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_SSE3)
         hwcaps |=
              VKI_AV_386_SSE3   /* SSE3 insns and regs */
            | VKI_AV_386_SSSE3; /* Intel SSSE3 insns */
      if (vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_CX16)
         hwcaps |=
              VKI_AV_386_CX16;  /* cmpxchg16b insn */
      if (vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_LZCNT)
         hwcaps |=
              VKI_AV_386_AMD_LZCNT; /* AMD's LZCNT insn */
      if (vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_RDTSCP)
         hwcaps |=
              VKI_AV_386_TSCP;  /* rdtscp instruction */
      if ((vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_SSE3) &&
          (vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_CX16)) {
         /* The CPUID simulation provided by VEX claims to have POPCNT, AES
            and SSE4 (SSE4.1/SSE4.2) in the SSE3+CX16 configuration. */
         hwcaps |=
              VKI_AV_386_POPCNT /* POPCNT insn */
            | VKI_AV_386_AES    /* Intel AES insns */
            | VKI_AV_386_SSE4_1 /* Intel SSE4.1 insns */
            | VKI_AV_386_SSE4_2; /* Intel SSE4.2 insns */
      }
      if ((vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_SSE3) &&
          (vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_CX16) &&
          (vex_archinfo.hwcaps & VEX_HWCAPS_AMD64_AVX)) {
         /* The CPUID simulation provided by VEX claims to have PCLMULQDQ and
            XSAVE in the SSE3+CX16+AVX configuration. */
         hwcaps |=
              VKI_AV_386_PCLMULQDQ /* Intel PCLMULQDQ insn */
            | VKI_AV_386_XSAVE; /* Intel XSAVE/XRSTOR insns */
      }
      /* No support for:
         AV_386_SEP             sysenter and sysexit
         AV_386_AMD_MMX         AMD's MMX insns
         AV_386_AMD_3DNow       AMD's 3Dnow! insns
         AV_386_AMD_3DNowx      AMD's 3Dnow! extended insns
         AV_386_AMD_SSE4A       AMD's SSE4A insns
         AV_386_MOVBE           Intel MOVBE insns
         AV_386_AVX             Intel AVX insns
         illumos only:
            AV_386_VMX          Intel VMX support
            AV_386_AMD_SVM      AMD SVM support
         solaris only:
            AV_386_AMD_XOP      AMD XOP insns
            AV_386_AMD_FMA4     AMD FMA4 insns

         TODO VEX supports AVX, BMI and AVX2. Investigate if they can be
         enabled on Solaris/illumos.
       */

#     else
#       error "Unknown architecture"
#     endif

      auxv->a_type = VKI_AT_SUN_HWCAP;
      auxv->a_un.a_val = hwcaps;
      auxv++;
   }

   /* AT_SUN_HWCAP2 */
   {
      /* No support for:
         illumos only:
            AV_386_2_F16C       F16C half percision extensions
            AV_386_2_RDRAND     RDRAND insn
         solaris only:
            AV2_386_RDRAND      Intel RDRAND insns
            AV2_386_FMA         Intel FMA insn
            AV2_386_F16C        IEEE half precn(float) insn
            AV2_386_AMD_TBM     AMD TBM insn
            AV2_386_BMI1        Intel BMI1 insn
            AV2_386_FSGSBASE    Intel RD/WR FS/GSBASE insn
            AV2_386_AVX2        Intel AVX2 insns
            AV2_386_BMI2        Intel BMI2 insns
            AV2_386_HLE         Intel HLE insns
            AV2_386_RTM         Intel RTM insns
            AV2_386_EFS         Intel Enhanced Fast String
            AV2_386_RDSEED      Intel RDSEED insn
            AV2_386_ADX         Intel ADX insns
            AV2_386_PRFCHW      Intel PREFETCHW hint
       */
   }

#  if defined(SOLARIS_RESERVE_SYSSTAT_ADDR)
   /* AT_SUN_SYSSTAT_ADDR */
   copy_auxv_entry(orig_auxv, VKI_AT_SUN_SYSSTAT_ADDR,
                   "AT_SUN_SYSSTAT_ADDR", auxv);
   VG_(change_mapping_ownership)(auxv->a_un.a_val, True);
   auxv++;
#  endif

#  if defined(SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR)
   /* AT_SUN_SYSSTAT_ZONE_ADDR */
   copy_auxv_entry(orig_auxv, VKI_AT_SUN_SYSSTAT_ZONE_ADDR,
                   "AT_SUN_SYSSTAT_ZONE_ADDR", auxv);
   VG_(change_mapping_ownership)(auxv->a_un.a_val, True);
   auxv++;
#  endif

   /* AT_NULL */
   auxv->a_type = VKI_AT_NULL;
   auxv->a_un.a_val = 0;

   vg_assert(strtab - stringbase == stringsize);

   /* The variable client_SP is now pointing at client's argc/argv. */

   if (0)
      VG_(printf)("startup SP = %#lx\n", client_SP);
   return client_SP;
}

/*====================================================================*/
/*=== TOP-LEVEL: VG_(setup_client_initial_image)                   ===*/
/*====================================================================*/

/* Create the client's initial memory image. */
IIFinaliseImageInfo VG_(ii_create_image)(IICreateImageInfo iicii,
                                         const VexArchInfo *vex_archinfo)
{
   ExeInfo info;
   HChar **env = NULL;
   HChar resolved_exe_name[VKI_PATH_MAX];

   IIFinaliseImageInfo iifii;
   VG_(memset)(&iifii, 0, sizeof(iifii));

   //--------------------------------------------------------------
   // Load client executable, finding in $PATH if necessary
   //   p: early_process_cmd_line_options()  [for 'exec', 'need_help']
   //   p: layout_remaining_space            [so there's space]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "initimg", "Loading client\n");

   if (!VG_(args_the_exename)) {
      VG_(err_missing_prog)();
      /*NOTREACHED*/
   }

   load_client(&info, resolved_exe_name, sizeof(resolved_exe_name));
   iifii.initial_client_IP = info.init_ip;
   /* Note: TOC isn't available on Solaris. */
   iifii.initial_client_TOC = info.init_toc;
   iifii.initial_client_TP = info.init_thrptr;
   /* Note that iifii.client_auxv is never set on Solaris, because it isn't
      necessary to have this value in VG_(ii_finalise_image). */

   //--------------------------------------------------------------
   // Set up client's environment
   //   p: set-libdir                       [for VG_(libdir)]
   //   p: early_process_cmd_line_options() [for toolname]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "initimg", "Setup client env\n");
   env = setup_client_env(iicii.envp, iicii.toolname);

   //--------------------------------------------------------------
   // Setup client stack and EIP
   //   p: load_client()     [for 'info']
   //   p: fix_environment() [for 'env']
   //--------------------------------------------------------------
   {
      /* When allocating space for the client stack, take notice of the
         --main-stacksize value.  This makes it possible to run programs with
         very large (primary) stack requirements simply by specifying
         --main-stacksize. */
      /* Logic is as follows:
         - By default, use the client's current stack rlimit.
         - If that exceeds 16M, clamp to 16M.
         - If a larger --main-stacksize value is specified, use that instead.
         - In all situations, the minimum allowed stack size is 1M.
      */
      Addr init_sp = (Addr) (iicii.argv - 1);
      SizeT m1  = 1024 * 1024;
      SizeT m16 = 16 * m1;
      SizeT szB = (SizeT)VG_(client_rlimit_stack).rlim_cur;
      if (szB < m1)
         szB = m1;
      if (szB > m16)
         szB = m16;

      if (VG_(clo_main_stacksize) > 0)
         szB = VG_(clo_main_stacksize);
      if (szB < m1)
         szB = m1;

      szB = VG_PGROUNDUP(szB);
      VG_(debugLog)(1, "initimg",
                       "Setup client stack: size will be %lu\n", szB);

      iifii.clstack_max_size = szB;
      iifii.initial_client_SP = setup_client_stack(init_sp, env, &info,
                                                   iicii.clstack_end,
                                                   iifii.clstack_max_size,
                                                   resolved_exe_name);
      VG_(free)(env);

      VG_(debugLog)(2, "initimg", "Client info: "
                       "initial_IP=%#lx, initial_TOC=%#lx, brk_base=%#lx\n",
                       iifii.initial_client_IP, iifii.initial_client_TOC,
                       VG_(brk_base));
      VG_(debugLog)(2, "initimg", "Client info: "
                       "initial_SP=%#lx, max_stack_size=%lu\n",
                       iifii.initial_client_SP,
                       iifii.clstack_max_size);
   }

   if (info.ldsoexec) {
      /* We are executing the runtime linker itself.
         Initial data (brk) segment is setup on demand, after the target dynamic
         executable has been loaded or when a first brk() syscall is made.
         It cannot be established now because it would conflict with a temporary
         stack which ld.so.1 (when executed directly) uses for loading the
         target dynamic executable. See PRE(sys_brk) in syswrap-solaris.c. */
   } else {
      if (!VG_(setup_client_dataseg)()) {
         VG_(printf)("valgrind: cannot initialize data segment (brk).\n");
         VG_(exit)(1);
      }
   }

   VG_(free)(info.interp_name);
   VG_(free)(info.interp_args);
   return iifii;
}


/*====================================================================*/
/*=== TOP-LEVEL: VG_(finalise_image)                               ===*/
/*====================================================================*/

/* Just before starting the client, we may need to make final adjustments to
   its initial image.  Also we need to set up the VEX guest state for thread 1
   (the root thread) and copy in essential starting values.  This is handed
   the IIFinaliseImageInfo created by VG_(ii_create_image).
*/
void VG_(ii_finalise_image)(IIFinaliseImageInfo iifii)
{
   ThreadArchState *arch = &VG_(threads)[1].arch;

#  if defined(VGA_x86)
   vg_assert(0 == sizeof(VexGuestX86State) % LibVEX_GUEST_STATE_ALIGN);

   /* Zero out the initial state, and set up the simulated FPU in a sane
      way. */
   LibVEX_GuestX86_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestX86State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestX86State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_ESP = iifii.initial_client_SP;
   arch->vex.guest_EIP = iifii.initial_client_IP;
   LibVEX_GuestX86_put_eflags(VKI_PSL_USER, &arch->vex);

   /* Set %cs, %ds, %ss and %es to default values. */
   __asm__ __volatile__ ("movw %%cs, %[cs]" : [cs] "=m" (arch->vex.guest_CS));
   __asm__ __volatile__ ("movw %%ds, %[ds]" : [ds] "=m" (arch->vex.guest_DS));
   __asm__ __volatile__ ("movw %%ss, %[ss]" : [ss] "=m" (arch->vex.guest_SS));
   __asm__ __volatile__ ("movw %%es, %[es]" : [es] "=m" (arch->vex.guest_ES));

   {
      /* Initial thread pointer value will be saved in GDT when the thread is
         started in the syswrap module and a thread's GDT is allocated. */
      ThreadOSstate *os = &VG_(threads)[1].os_state;
      os->thrptr = iifii.initial_client_TP;
   }

#  elif defined(VGA_amd64)
   vg_assert(0 == sizeof(VexGuestAMD64State) % LibVEX_GUEST_STATE_ALIGN);

   /* Zero out the initial state, and set up the simulated FPU in a sane
      way. */
   LibVEX_GuestAMD64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestAMD64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestAMD64State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_RSP = iifii.initial_client_SP;
   arch->vex.guest_RIP = iifii.initial_client_IP;
   arch->vex.guest_FS_CONST = iifii.initial_client_TP;
   LibVEX_GuestAMD64_put_rflags(VKI_PSL_USER, &arch->vex);

#  else
#    error "Unknown platform"
#  endif

   /* Tell the tool that we just wrote to the registers. */
   VG_TRACK(post_reg_write, Vg_CoreStartup, 1/*tid*/, 0/*offset*/,
            sizeof(VexGuestArchState));

   if (VG_(brk_base) != -1 ) {
      /* Make inaccessible/unaddressable the end of the client data segment.
         See PRE(sys_brk) in syswrap-solaris.c for details. */
      VG_(track_client_dataseg)(1 /* tid */);
   }
}

#endif // defined(VGO_solaris)

/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/
