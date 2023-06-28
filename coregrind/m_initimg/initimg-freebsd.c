
/*--------------------------------------------------------------------*/
/*--- Startup: create initial process image on FreeBSD             ---*/
/*---                                            initimg-freebsd.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2009 Julian Seward
      jseward@acm.org
   Copyright (C) 2018-2021 Paul Floyd
      pjfloyd@wanadoo.fr

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

#if defined(VGO_freebsd)

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
#include "pub_core_syscall.h"
#include "pub_core_tooliface.h"       /* VG_TRACK */
#include "pub_core_threadstate.h"     /* ThreadArchState */
#include "pub_core_pathscan.h"
#include "pub_core_initimg.h"         /* self */

/*====================================================================*/
/*=== Loading the client                                           ===*/
/*====================================================================*/

/* Load the client whose name is VG_(argv_the_exename). */

static void load_client ( /*OUT*/ExeInfo* info,
                          /*OUT*/Addr*    client_ip,
                          /*OUT*/Addr*    client_toc)
{
   const HChar* exe_name;
   Int    ret;
   SysRes res;

   vg_assert( VG_(args_the_exename) != NULL);
   exe_name = VG_(find_executable)( VG_(args_the_exename) );

   if (!exe_name) {
      VG_(printf)("valgrind: %s: command not found\n", VG_(args_the_exename));
      VG_(exit)(127);      // 127 is Posix NOTFOUND
   }

   VG_(memset)(info, 0, sizeof(*info));
   ret = VG_(do_exec)(exe_name, info);
   if (ret < 0) {
      VG_(printf)("valgrind: could not execute '%s'\n", exe_name);
      VG_(exit)(1);
   }

   // The client was successfully loaded!  Continue.

   /* Get hold of a file descriptor which refers to the client
      executable.  This is needed for attaching to GDB. */
   res = VG_(open)(exe_name, VKI_O_RDONLY, VKI_S_IRUSR);
   if (!sr_isError(res)) {
      VG_(cl_exec_fd) = sr_Res(res);
   }

   /* Copy necessary bits of 'info' that were filled in */
   *client_ip  = info->init_ip;
   *client_toc = info->init_toc;
   VG_(brk_base) = VG_(brk_limit) = VG_PGROUNDUP(info->brkbase);
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

   Also, remove any binding for VALGRIND_LAUNCHER=.  The client should
   not be able to see this.

   If this needs to handle any more variables it should be hacked
   into something table driven.  The copy is VG_(malloc)'d space.
*/
static HChar** setup_client_env ( HChar** origenv, const HChar* toolname)
{
   vg_assert(origenv);
   vg_assert(toolname);

   const HChar* preload_core    = "vgpreload_core";
   const HChar* ld_preload      = "LD_PRELOAD=";
   const HChar* v_launcher      = VALGRIND_LAUNCHER "=";
   Int    ld_preload_len  = VG_(strlen)( ld_preload );
   Int    v_launcher_len  = VG_(strlen)( v_launcher );
   Bool   ld_preload_done = False;
#if defined(VGP_x86_freebsd)
   const HChar* ld_32_preload    = "LD_32_PRELOAD=";
   Int    ld_32_preload_len = VG_(strlen)( ld_32_preload );
   Bool   ld_32_preload_done = False;
#endif
   Int    vglib_len       = VG_(strlen)(VG_(libdir));
   Bool   debug           = False;

   HChar** cpp;
   HChar** ret;
   HChar*  preload_tool_path;
   Int     envc;
   Int     i;

   /* Alloc space for the vgpreload_core.so path and vgpreload_<tool>.so
      paths.  We might not need the space for vgpreload_<tool>.so, but it
      doesn't hurt to over-allocate briefly.  The 16s are just cautious
      slop. */
   Int preload_core_path_len = vglib_len + sizeof(preload_core)
                               + sizeof(VG_PLATFORM) + 16;
   Int preload_tool_path_len = vglib_len + VG_(strlen)(toolname)
                               + sizeof(VG_PLATFORM) + 16;
   Int preload_string_len    = preload_core_path_len + preload_tool_path_len;
   HChar* preload_string     = VG_(malloc)("initimg-freebsd.sce.1",
                                           preload_string_len);
   /* Determine if there's a vgpreload_<tool>_<platform>.so file, and setup
      preload_string. */
   preload_tool_path = VG_(malloc)("initimg-freebsd.sce.2", preload_tool_path_len);
   VG_(snprintf)(preload_tool_path, preload_tool_path_len,
                 "%s/vgpreload_%s-%s.so", VG_(libdir), toolname, VG_PLATFORM);
   if (VG_(access)(preload_tool_path, True/*r*/, False/*w*/, False/*x*/) == 0) {
      VG_(snprintf)(preload_string, preload_string_len, "%s/%s-%s.so:%s",
                    VG_(libdir), preload_core, VG_PLATFORM, preload_tool_path);
   } else {
      VG_(snprintf)(preload_string, preload_string_len, "%s/%s-%s.so",
                    VG_(libdir), preload_core, VG_PLATFORM);
   }
   VG_(free)(preload_tool_path);

   VG_(debugLog)(2, "initimg", "preload_string:\n");
   VG_(debugLog)(2, "initimg", "  \"%s\"\n", preload_string);

   /* Count the original size of the env */
   if (debug) {
      VG_(printf)("\n\n");
   }
   envc = 0;
   for (cpp = origenv; cpp && *cpp; cpp++) {
      envc++;
      if (debug) {
         VG_(printf)("XXXXXXXXX: BEFORE %s\n", *cpp);
      }
   }

   /* Allocate a new space */
   ret = VG_(malloc) ("initimg-freebsd.sce.3",
                      sizeof(HChar *) * (envc+2+1)); /* 2 new entries + NULL */

   /* copy it over */
   for (cpp = ret; *origenv; ) {
      if (debug) {
         VG_(printf)("XXXXXXXXX: COPY   %s\n", *origenv);
      }
      *cpp++ = *origenv++;
   }
   *cpp = NULL;
   *(cpp + 1) = NULL;

   vg_assert(envc == (cpp - ret));

   /* Walk over the new environment, mashing as we go */
   for (cpp = ret; cpp && *cpp; cpp++) {
      if (VG_(memcmp)(*cpp, ld_preload, ld_preload_len) == 0) {
         Int len = VG_(strlen)(*cpp) + preload_string_len;
         HChar *cp = VG_(malloc)("initimg-freebsd.sce.4", len);

         VG_(snprintf)(cp, len, "%s%s:%s",
                       ld_preload, preload_string, (*cpp)+ld_preload_len);

         *cpp = cp;

         ld_preload_done = True;
      }
      if (debug) {
         VG_(printf)("XXXXXXXXX: MASH   %s\n", *cpp);
      }
   }

   /* Add the missing bits */
   if (!ld_preload_done) {
      Int len = ld_preload_len + preload_string_len;
      HChar *cp = VG_(malloc) ("initimg-freebsd.sce.5", len);

      VG_(snprintf)(cp, len, "%s%s", ld_preload, preload_string);

      ret[envc++] = cp;
      if (debug) {
         VG_(printf)("XXXXXXXXX: ADD    %s\n", cp);
      }
   }

#if defined(VGP_x86_freebsd)
   /* If we're running a 32 bit binary, ld-elf32.so.1 may be looking for
    * a different variable name.  Or it might be a 32 bit ld-elf.so.1 in a
    * chroot.  Cover both cases. */
   if (VG_(is32on64)()) {
      for (cpp = ret; cpp && *cpp; cpp++) {
         if (VG_(memcmp)(*cpp, ld_32_preload, ld_32_preload_len) == 0) {
            Int len = VG_(strlen)(*cpp) + preload_string_len;
            HChar *cp = VG_(malloc)("initimg-freebsd.sce.4a", len);
            vg_assert(cp);

            VG_(snprintf)(cp, len, "%s%s:%s",
                          ld_32_preload, preload_string, (*cpp)+ld_32_preload_len);

            *cpp = cp;

            ld_32_preload_done = True;
         }
      }
      if (!ld_32_preload_done) {
         Int len = ld_32_preload_len + preload_string_len;
         HChar *cp = VG_(malloc) ("initimg-freebsd.sce.5a", len);
         vg_assert(cp);

         VG_(snprintf)(cp, len, "%s%s", ld_32_preload, preload_string);

         ret[envc++] = cp;
      }
   }
#endif

   /* ret[0 .. envc-1] is live now. */
   /* Find and remove a binding for VALGRIND_LAUNCHER. */
   for (i = 0; i < envc; i++) {
      if (0 == VG_(memcmp)(ret[i], v_launcher, v_launcher_len)) {
         break;
      }
   }

   if (i < envc) {
      for (; i < envc-1; i++) {
         ret[i] = ret[i+1];
      }
      envc--;
   }

   VG_(free)(preload_string);
   ret[envc] = NULL;

   for (i = 0; i < envc; i++) {
      if (debug) {
         VG_(printf)("XXXXXXXXX: FINAL  %s\n", ret[i]);
      }
   }

   return ret;
}


/*====================================================================*/
/*=== Setting up the client's stack                                ===*/
/*====================================================================*/

/* Add a string onto the string table, and return its address */
static HChar *copy_str(HChar **tab, const HChar *str)
{
   HChar *cp = *tab;
   HChar *orig = cp;

   while(*str) {
      *cp++ = *str++;
   }
   *cp++ = '\0';

   if (0) {
      VG_(printf)("copied %p \"%s\" len %lld\n", (void*)orig, orig, (Long)(cp-orig));
   }

   *tab = cp;

   return orig;
}

/* Add byte onto the string table, and return its address */
static HChar *copy_bytes(HChar **tab, const HChar *src, SizeT size)
{
   HChar *cp = *tab;
   /*VG_ROUNDUP(cp, sizeof(Word));*/
   HChar *orig = cp;

   VG_(memcpy)(cp, src, size);

   *tab = cp+size;

   return orig;
}

/* ----------------------------------------------------------------

   This sets up the client's initial stack, containing the args,
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

   Allocate and create the initial client stack.  It is allocated down
   from clstack_end, which was previously determined by the address
   space manager.  The returned value is the SP value for the client.

   The client's auxv is created by copying and modifying our own one.

   ---------------------------------------------------------------- */

static struct auxv *find_auxv(UWord* sp)
{
   sp++;                // skip argc (Nb: is word-sized, not int-sized!)

   while (*sp != 0) {   // skip argv
      sp++;
   }
   sp++;

   while (*sp != 0) {   // skip env
      sp++;
   }
   sp++;

   return (struct auxv *)sp;
}

static Addr setup_client_stack(void*  init_sp,
                               HChar** orig_envp,
                               const ExeInfo* info,
                               UInt** client_auxv,
                               Addr   clstack_end,
                               SizeT  clstack_max_size )
{
   SysRes res;
   HChar **cpp;
   HChar *strtab;    /* string table */
   HChar *stringbase;
   Addr *ptr;
   struct auxv *auxv;
   const struct auxv *orig_auxv;
   const struct auxv *cauxv;
   unsigned stringsize;    /* total size of strings in bytes */
   unsigned auxsize;    /* total size of auxv in bytes */
   Int argc;         /* total argc */
   Int envc;         /* total number of env vars */
   unsigned stacksize;     /* total client stack size */
   Addr client_SP;           /* client stack base (initial SP) */
   Addr clstack_start;
   Int i;
   Bool have_exename;
   Word client_argv;

   vg_assert(VG_IS_PAGE_ALIGNED(clstack_end+1));
   vg_assert( VG_(args_for_client) );

   const HChar *exe_name = VG_(find_executable)(VG_(args_the_exename));
   HChar resolved_name[VKI_PATH_MAX];
   VG_(realpath)(exe_name, resolved_name);

   /* use our own auxv as a prototype */
   orig_auxv = find_auxv(init_sp);

   /* ==================== compute sizes ==================== */

   /* first of all, work out how big the client stack will be */
   stringsize   = 0;
   have_exename = VG_(args_the_exename) != NULL;

   /* paste on the extra args if the loader needs them (ie, the #!
      interpreter and its argument) */
   argc = 0;
   if (info->interp_name != NULL) {
      argc++;
      stringsize += VG_(strlen)(info->interp_name) + 1;
   }
   if (info->interp_args != NULL) {
      argc++;
      stringsize += VG_(strlen)(info->interp_args) + 1;
   }

   /* now scan the args we're given... */
   if (have_exename) {
      stringsize += VG_(strlen)( VG_(args_the_exename) ) + 1;
   }

   for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
      argc++;
      stringsize += VG_(strlen)( * (HChar**)
                                 VG_(indexXA)( VG_(args_for_client), i ))
                    + 1;
   }

   /* ...and the environment */
   envc = 0;
   for (cpp = orig_envp; cpp && *cpp; cpp++) {
      envc++;
      stringsize += VG_(strlen)(*cpp) + 1;
   }

   Int canarylen = -1;
   Int pagesizeslen =  -1;

   /* now, how big is the auxv? */
   auxsize = sizeof(*auxv);   /* there's always at least one entry: AT_NULL */
   for (cauxv = orig_auxv; cauxv->a_type != VKI_AT_NULL; cauxv++) {
      auxsize += sizeof(*cauxv);
      switch(cauxv->a_type) {
      case VKI_AT_EXECPATH:
         stringsize += VG_(strlen)(resolved_name) + 1;
         break;
      case VKI_AT_CANARYLEN:
         canarylen = cauxv->u.a_val;
         /*VG_ROUNDUP(stringsize, sizeof(Word));*/
         stringsize += canarylen;
         break;
      case VKI_AT_PAGESIZESLEN:
         pagesizeslen = cauxv->u.a_val;
         /*VG_ROUNDUP(stringsize, sizeof(Word));*/
         stringsize += pagesizeslen;
         break;
#if 0
      case VKI_AT_TIMEKEEP:
         /*VG_ROUNDUP(stringsize, sizeof(Word));*/
         stringsize += sizeof(struct vki_vdso_timehands);
         break;
#endif
#if (FREEBSD_VERS >= FREEBSD_13_0)
      case VKI_AT_PS_STRINGS:
         stringsize += sizeof(struct vki_ps_strings);
         break;
#endif
#if (FREEBSD_VERS >= FREEBSD_13_1)
      // case AT_FXRNG:
      // case AT_KPRELOAD:
#endif
      default:
         break;
      }
   }

   /* OK, now we know how big the client stack is */
   stacksize =
      sizeof(Word) +                          /* argc */
      (have_exename ? sizeof(HChar **) : 0) +  /* argc[0] == exename */
      sizeof(HChar **)*argc +                 /* argv */
      sizeof(HChar **) +                      /* terminal NULL */
      sizeof(HChar **)*envc +                 /* envp */
      sizeof(HChar **) +                      /* terminal NULL */
      auxsize +                               /* auxv */
      VG_ROUNDUP(stringsize, sizeof(Word));   /* strings (aligned) */

   if (0) {
      VG_(printf)("stacksize = %u\n", stacksize);
   }

   /* client_SP is the client's stack pointer */
   client_SP = clstack_end - stacksize;
   client_SP = VG_ROUNDDN(client_SP, 16); /* make stack 16 byte aligned */

   /* base of the string table (aligned) */
   stringbase = strtab = (HChar *)clstack_end
                         - VG_ROUNDUP(stringsize, sizeof(int));

   clstack_start = VG_PGROUNDDN(client_SP);

   /* The max stack size */
   clstack_max_size = VG_PGROUNDUP(clstack_max_size);

   if (0) {
      VG_(printf)("stringsize=%u auxsize=%u stacksize=%u maxsize=0x%lx\n"
                  "clstack_start %p\n"
                  "clstack_end   %p\n",
                  stringsize, auxsize, stacksize, clstack_max_size,
                  (void*)clstack_start, (void*)clstack_end);
   }

   /* ==================== allocate space ==================== */

   {
      SizeT anon_size   = clstack_end - clstack_start + 1;
      SizeT resvn_size  = clstack_max_size - anon_size;
      Addr  anon_start  = clstack_start;
      Addr  resvn_start = anon_start - resvn_size;
      SizeT inner_HACK  = 0;
      Bool  ok;

      /* So far we've only accounted for space requirements down to the
         stack pointer.  If this target's ABI requires a redzone below
         the stack pointer, we need to allocate an extra page, to
         handle the worst case in which the stack pointer is almost at
         the bottom of a page, and so there is insufficient room left
         over to put the redzone in.  In this case the simple thing to
         do is allocate an extra page, by shrinking the reservation by
         one page and growing the anonymous area by a corresponding
         page. */
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

#    ifdef ENABLE_INNER
      inner_HACK = 1024*1024; // create 1M non-fault-extending stack
#    endif

      if (0) {
         VG_(printf)("%#lx 0x%lx  %#lx 0x%lx\n",
                     resvn_start, resvn_size, anon_start, anon_size);
      }

      /* Create a shrinkable reservation followed by an anonymous
         segment.  Together these constitute a growdown stack. */
      res = VG_(mk_SysRes_Error)(0);
      ok = VG_(am_create_reservation)(
              resvn_start,
              resvn_size -inner_HACK,
              SmUpper,
              anon_size +inner_HACK
           );
      if (ok) {
         /* allocate a stack - mmap enough space for the stack */
         res = VG_(am_mmap_anon_fixed_client)(
                  anon_start -inner_HACK,
                  anon_size +inner_HACK,
                  VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC
               );
      }
      if ((!ok) || sr_isError(res)) {
         /* Allocation of the stack failed.  We have to stop. */
         VG_(printf)("valgrind: "
                     "I failed to allocate space for the application's stack.\n");
         VG_(printf)("valgrind: "
                     "This may be the result of a very large --main-stacksize=\n");
         VG_(printf)("valgrind: setting.  Cannot continue.  Sorry.\n\n");
         VG_(exit)(1);
      }

      vg_assert(ok);
      vg_assert(!sr_isError(res));

      /* Record stack extent -- needed for stack-change code. */
      VG_(clstk_start_base) = anon_start -inner_HACK;
      VG_(clstk_end)  = VG_(clstk_start_base) + anon_size +inner_HACK -1;

   }

   /* ==================== create client stack ==================== */

   ptr = (Addr*)client_SP;

   /* --- client argc --- */
   *ptr++ = argc + (have_exename ? 1 : 0);

   /* --- client argv --- */
   client_argv = (Word)ptr;
   if (info->interp_name) {
      *ptr++ = (Addr)copy_str(&strtab, info->interp_name);
   }
   if (info->interp_args) {
      *ptr++ = (Addr)copy_str(&strtab, info->interp_args);
   }

   if (have_exename) {
      *ptr++ = (Addr)copy_str(&strtab, VG_(args_the_exename));
   }

   for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
      *ptr++ = (Addr)copy_str(
                  &strtab,
                  * (HChar**) VG_(indexXA)( VG_(args_for_client), i )
               );
   }
   *ptr++ = 0;

   /* --- envp --- */
   VG_(client_envp) = (HChar **)ptr;
   for (cpp = orig_envp; cpp && *cpp; ptr++, cpp++) {
      *ptr = (Addr)copy_str(&strtab, *cpp);
   }
   *ptr++ = 0;

   /* --- auxv --- */
   auxv = (struct auxv *)ptr;
   *client_auxv = (UInt *)auxv;
   VG_(client_auxv) = (UWord *)*client_auxv;

   for (; orig_auxv->a_type != VKI_AT_NULL; auxv++, orig_auxv++) {

      /* copy the entry... */
      *auxv = *orig_auxv;

      /*
       *  ...and fix up / examine the copy
       * in general there are thee possibilities for these items
       * 1. copy it, a common case for scalars
       * 2. synthesize, if the value that the host gets isn't what we want
       * 3. ignore, usually the case for pointers to memory for the host
       *    the ignored items are just left commented out
       */
      switch(auxv->a_type) {

      case VKI_AT_IGNORE:
      case VKI_AT_PHENT:
      case VKI_AT_PAGESZ:
      case VKI_AT_FLAGS:
      case VKI_AT_NOTELF:
      case VKI_AT_UID:
      case VKI_AT_EUID:
      case VKI_AT_GID:
      case VKI_AT_EGID:
      case VKI_AT_STACKPROT:
      case VKI_AT_NCPUS:
      case VKI_AT_OSRELDATE:
      case VKI_AT_PAGESIZESLEN:
      case VKI_AT_CANARYLEN:

#if (FREEBSD_VERS >= FREEBSD_11)
      // FreeBSD 11+ also have HWCAP and HWCAP2
      case VKI_AT_EHDRFLAGS:
#endif
         /* All these are pointerless, so we don't need to do
            anything about them. */
         break;

      case VKI_AT_EXECPATH:
         auxv->u.a_ptr = copy_str(&strtab, resolved_name);
         VG_(resolved_exename) = auxv->u.a_ptr;
         break;
      case VKI_AT_CANARY:
         if (canarylen >= 1) {
            auxv->u.a_ptr = copy_bytes(&strtab, orig_auxv->u.a_ptr, canarylen);
         } else {
            auxv->a_type = VKI_AT_IGNORE;
         }
         break;
      case VKI_AT_PAGESIZES:
         if (pagesizeslen >= 1) {
            auxv->u.a_ptr = copy_bytes(&strtab, orig_auxv->u.a_ptr, pagesizeslen);
         } else {
            auxv->a_type = VKI_AT_IGNORE;
         }
         break;
#if 0
         /*
          * @todo PJF this crashes intermittently
          */
      case VKI_AT_TIMEKEEP:
         auxv->u.a_ptr = copy_bytes(&strtab, orig_auxv->u.a_ptr, sizeof(struct vki_vdso_timehands));
         break;
#endif

#if (FREEBSD_VERS >= FREEBSD_13_0)
      /* @todo PJF BSDFLAGS causes serveral testcases to crash.
         Not sure why, it seems to be used for sigfastblock */
      // case AT_BSDFLAGS:
      case VKI_AT_ARGC:
      case VKI_AT_ENVC:
         break;
      case VKI_AT_PS_STRINGS:
         auxv->u.a_ptr = copy_bytes(&strtab, orig_auxv->u.a_ptr, sizeof(struct vki_ps_strings));
         ((struct vki_ps_strings*)auxv->u.a_ptr)->ps_envstr = (char**)VG_(client_envp);
         ((struct vki_ps_strings*)auxv->u.a_ptr)->ps_argvstr = (char**)client_argv;
         break;
      case VKI_AT_ARGV:
         auxv->u.a_val = client_argv;
         break;
      case VKI_AT_ENVV:
         auxv->u.a_val = (Word)VG_(client_envp);
         break;
#endif

#if (FREEBSD_VERS >= FREEBSD_13_1)
      // I think that this is a pointer to a "fenestrasX" structture
      // lots of stuff that I don't understand
      // arc4random, passing through VDSO page ...
      // case AT_FXRNG:
      // Again a pointer, to the VDSO base for use by rtld
      // case AT_KPRELOAD:
#endif

#if (FREEBSD_VERS >= FREEBSD_13_2)
      case VKI_AT_USRSTACKBASE:
         auxv->u.a_val = VG_(get_usrstack)();
         break;
      case VKI_AT_USRSTACKLIM:
         auxv->u.a_val = clstack_max_size;
         break;
#endif

      case VKI_AT_PHDR:
         if (info->phdr == 0) {
            auxv->a_type = VKI_AT_IGNORE;
         } else {
            auxv->u.a_val = info->phdr;
         }
         break;

      case VKI_AT_PHNUM:
         if (info->phdr == 0) {
            auxv->a_type = VKI_AT_IGNORE;
         } else {
            auxv->u.a_val = info->phnum;
         }
         break;

      case VKI_AT_BASE:
         auxv->u.a_val = info->interp_offset;
         break;

      case VKI_AT_ENTRY:
         auxv->u.a_val = info->entry;
         break;

      default:
         /* stomp out anything we don't know about */
         VG_(debugLog)(2, "initimg",
                       "stomping auxv entry %llu\n",
                       (ULong)auxv->a_type);
         auxv->a_type = VKI_AT_IGNORE;
         break;
      }
   }
   *auxv = *orig_auxv;
   vg_assert(auxv->a_type == VKI_AT_NULL);

   vg_assert((strtab-stringbase) == stringsize);

   /* client_SP is pointing at client's argc/argv */

   if (0) {
      VG_(printf)("startup SP = %#lx\n", client_SP);
   }

   if (VG_(resolved_exename) == NULL) {
      VG_(resolved_exename) = VG_(strdup)("initimg-freebsd.sre.1", resolved_name);
   }

   return client_SP;
}


/* Allocate the client data segment.  It is an expandable anonymous
   mapping abutting a shrinkable reservation of size max_dseg_size.
   The data segment starts at VG_(brk_base), which is page-aligned,
   and runs up to VG_(brk_limit), which isn't. */

static void setup_client_dataseg ( SizeT max_size )
{
   Bool   ok;
   SysRes sres;
   Addr   anon_start  = VG_(brk_base);
   SizeT  anon_size   = VKI_PAGE_SIZE;
   Addr   resvn_start = anon_start + anon_size;
   SizeT  resvn_size  = max_size - anon_size;

   vg_assert(VG_IS_PAGE_ALIGNED(anon_size));
   vg_assert(VG_IS_PAGE_ALIGNED(resvn_size));
   vg_assert(VG_IS_PAGE_ALIGNED(anon_start));
   vg_assert(VG_IS_PAGE_ALIGNED(resvn_start));

   /* Because there's been no brk activity yet: */
   vg_assert(VG_(brk_base) == VG_(brk_limit));

   /* Try to create the data seg and associated reservation where
      VG_(brk_base) says. */
   ok = VG_(am_create_reservation)(
           resvn_start,
           resvn_size,
           SmLower,
           anon_size
        );

   if (!ok) {
      /* Hmm, that didn't work.  Well, let aspacem suggest an address
         it likes better, and try again with that. */
      anon_start = VG_(am_get_advisory_client_simple)
                   ( 0/*floating*/, anon_size+resvn_size, &ok );
      if (ok) {
         resvn_start = anon_start + anon_size;
         ok = VG_(am_create_reservation)(
                 resvn_start,
                 resvn_size,
                 SmLower,
                 anon_size
              );
         if (ok) {
            VG_(brk_base) = VG_(brk_limit) = anon_start;
         }
      }
      /* that too might have failed, but if it has, we're hosed: there
         is no Plan C. */
   }
   vg_assert(ok);

   sres = VG_(am_mmap_anon_fixed_client)(
             anon_start,
             anon_size,
             VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC
          );
   vg_assert(!sr_isError(sres));
   vg_assert(sr_Res(sres) == anon_start);
}


/*====================================================================*/
/*=== TOP-LEVEL: VG_(setup_client_initial_image)                   ===*/
/*====================================================================*/

/* Create the client's initial memory image. */
IIFinaliseImageInfo VG_(ii_create_image)( IICreateImageInfo iicii,
      const VexArchInfo* vex_archinfo )
{
   ExeInfo info;
   HChar** env = NULL;

   IIFinaliseImageInfo iifii = {
      .clstack_max_size = 0,
      .initial_client_SP = 0,
      .initial_client_IP = 0,
      .initial_client_TOC = 0,
      .client_auxv = NULL,
      .arch_elf_state = VKI_INIT_ARCH_ELF_STATE,
   };

   //--------------------------------------------------------------
   // Load client executable, finding in $PATH if necessary
   //   p: get_helprequest_and_toolname()  [for 'exec', 'need_help']
   //   p: layout_remaining_space          [so there's space]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "initimg", "Loading client\n");

   if (VG_(args_the_exename) == NULL)  {
      VG_(err_missing_prog)();
   }

   VG_(memset)(&info, 0, sizeof(info));

   load_client(&info, &iifii.initial_client_IP, &iifii.initial_client_TOC);

   //--------------------------------------------------------------
   // Set up client's environment
   //   p: set-libdir                   [for VG_(libdir)]
   //   p: get_helprequest_and_toolname [for toolname]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "initimg", "Setup client env\n");
   env = setup_client_env(iicii.envp, iicii.toolname);

   //--------------------------------------------------------------
   // Setup client stack, eip, and VG_(client_arg[cv])
   //   p: load_client()     [for 'info']
   //   p: fix_environment() [for 'env']
   //--------------------------------------------------------------
   {
      /* When allocating space for the client stack, take
         notice of the --main-stacksize value.  This makes it possible
         to run programs with very large (primary) stack requirements
         simply by specifying --main-stacksize. */
      /* Logic is as follows:
         - by default, use the client's current stack rlimit
         - if that exceeds 16M, clamp to 16M
         - if a larger --main-stacksize value is specified, use that instead
         - in all situations, the minimum allowed stack size is 1M
      */
      void* init_sp = iicii.argv - 1;
      SizeT m1  = 1024 * 1024;
      SizeT m16 = 16 * m1;
      SizeT szB = (SizeT)VG_(client_rlimit_stack).rlim_cur;
      if (szB < m1) {
         szB = m1;
      }
      if (szB > m16) {
         szB = m16;
      }
      if (VG_(clo_main_stacksize) > 0) {
         szB = VG_(clo_main_stacksize);
      }
      if (szB < m1) {
         szB = m1;
      }
      szB = VG_PGROUNDUP(szB);
      VG_(debugLog)(1, "initimg",
                    "Setup client stack: size will be %lu\n", szB);

      iifii.clstack_max_size = szB;

      iifii.initial_client_SP
         = setup_client_stack( init_sp, env,
                               &info, &iifii.client_auxv,
                               iicii.clstack_end, iifii.clstack_max_size );

      VG_(free)(env);

      VG_(debugLog)(2, "initimg",
                    "Client info: "
                    "initial_IP=%p initial_TOC=%p brk_base=%p\n",
                    (void*)(iifii.initial_client_IP),
                    (void*)(iifii.initial_client_TOC),
                    (void*)VG_(brk_base) );
      VG_(debugLog)(2, "initimg",
                    "Client info: "
                    "initial_SP=%p max_stack_size=%lu\n",
                    (void*)(iifii.initial_client_SP),
                    (SizeT)iifii.clstack_max_size );
   }

   //--------------------------------------------------------------
   // Setup client data (brk) segment.  Initially a 1-page segment
   // which abuts a shrinkable reservation.
   //     p: load_client()     [for 'info' and hence VG_(brk_base)]
   //--------------------------------------------------------------
   {
      SizeT m1 = 1024 * 1024;
      SizeT m8 = 8 * m1;
      SizeT dseg_max_size = (SizeT)VG_(client_rlimit_data).rlim_cur;
      VG_(debugLog)(1, "initimg", "Setup client data (brk) segment\n");
      if (dseg_max_size < m1) {
         dseg_max_size = m1;
      }
      if (dseg_max_size > m8) {
         dseg_max_size = m8;
      }
      dseg_max_size = VG_PGROUNDUP(dseg_max_size);

      setup_client_dataseg( dseg_max_size );
   }

   VG_(free)(info.interp_name);
   info.interp_name = NULL;
   VG_(free)(info.interp_args);
   info.interp_args = NULL;
   return iifii;
}


/*====================================================================*/
/*=== TOP-LEVEL: VG_(finalise_thread1state)                        ===*/
/*====================================================================*/

/* Just before starting the client, we may need to make final
   adjustments to its initial image.  Also we need to set up the VEX
   guest state for thread 1 (the root thread) and copy in essential
   starting values.  This is handed the IIFinaliseImageInfo created by
   VG_(ii_create_image).
*/
void VG_(ii_finalise_image)( IIFinaliseImageInfo iifii )
{
   ThreadArchState* arch = &VG_(threads)[1].arch;

   /* We get client_{ip/sp/toc}, and start the client with
      all other registers zeroed. */

#  if defined(VGP_x86_freebsd)
   vg_assert(0 == sizeof(VexGuestX86State) % 16);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestX86_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestX86State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestX86State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_ESP = iifii.initial_client_SP;
   arch->vex.guest_EIP = iifii.initial_client_IP;

   /* initialise %cs, %ds and %ss to point at the operating systems
      default code, data and stack segments */
   asm volatile("movw %%cs, %0" : : "m" (arch->vex.guest_CS));
   asm volatile("movw %%ds, %0" : : "m" (arch->vex.guest_DS));
   asm volatile("movw %%ss, %0" : : "m" (arch->vex.guest_SS));

#  elif defined(VGP_amd64_freebsd)
   vg_assert(0 == sizeof(VexGuestAMD64State) % 16);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestAMD64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestAMD64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestAMD64State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_RSP = ((iifii.initial_client_SP - 8) & ~0xFUL) + 8;
   arch->vex.guest_RDI = iifii.initial_client_SP;
   arch->vex.guest_RIP = iifii.initial_client_IP;

#  else
#    error Unknown platform
#  endif

   /* Tell the tool that we just wrote to the registers. */
   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));

   /* Tell the tool about the client data segment and then kill it which will
      make it inaccessible/unaddressable. */
   const NSegment *seg = VG_(am_find_nsegment)(VG_(brk_base));
   vg_assert(seg);
   vg_assert(seg->kind == SkAnonC);
   VG_TRACK(new_mem_brk, VG_(brk_base), seg->end + 1 - VG_(brk_base),
            1/*tid*/);
   VG_TRACK(die_mem_brk, VG_(brk_base), seg->end + 1 - VG_(brk_base));
}

#endif // defined(VGO_freebsd)

/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/
