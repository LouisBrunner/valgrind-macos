
/*--------------------------------------------------------------------*/
/*--- Startup: create initial process image on Linux               ---*/
/*---                                              initimg-linux.c ---*/
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

#if defined(VGO_linux)

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
#include "pub_core_pathscan.h"        /* find_executable */
#include "pub_core_initimg.h"         /* self */

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#define _GNU_SOURCE
#define _FILE_OFFSET_BITS 64
/* This is for ELF types etc, and also the AT_ constants. */
#include <elf.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */


/*====================================================================*/
/*=== Loading the client                                           ===*/
/*====================================================================*/

/* Load the client whose name is VG_(argv_the_exename). */

static void load_client ( /*MOD*/ExeInfo* info, 
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

   ret = VG_(do_exec)(exe_name, info);
   if (ret < 0) {
      VG_(printf)("valgrind: could not execute '%s'\n", exe_name);
      VG_(exit)(1);
   }

   // The client was successfully loaded!  Continue.

   /* Get hold of a file descriptor which refers to the client
      executable.  This is needed for attaching to GDB. */
   res = VG_(open)(exe_name, VKI_O_RDONLY, VKI_S_IRUSR);
   if (!sr_isError(res))
      VG_(cl_exec_fd) = sr_Res(res);

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
static HChar** setup_client_env ( HChar** origenv, const HChar* toolname, Bool use_stack_cache_tunable)
{
   vg_assert(origenv);
   vg_assert(toolname);

   const HChar* preload_core      = "vgpreload_core";
   const HChar* ld_preload        = "LD_PRELOAD=";
   const HChar* v_launcher        = VALGRIND_LAUNCHER "=";
   Int    ld_preload_len          = VG_(strlen)( ld_preload );
   Int    v_launcher_len          = VG_(strlen)( v_launcher );
   Bool   ld_preload_done         = False;
   Int    vglib_len               = VG_(strlen)(VG_(libdir));
   Bool   debug                   = False;

   HChar** cpp;
   HChar** ret;
   HChar*  preload_tool_path;
   Int     envc, i;

   /* Alloc space for the vgpreload_core.so path and vgpreload_<tool>.so
      paths.  We might not need the space for vgpreload_<tool>.so, but it
      doesn't hurt to over-allocate briefly.  The 16s are just cautious
      slop. */
   Int preload_core_path_len = vglib_len + sizeof(preload_core) 
                                         + sizeof(VG_PLATFORM) + 16;
   Int preload_tool_path_len = vglib_len + VG_(strlen)(toolname) 
                                         + sizeof(VG_PLATFORM) + 16;
   Int preload_string_len    = preload_core_path_len + preload_tool_path_len;
   HChar* preload_string     = VG_(malloc)("initimg-linux.sce.1",
                                           preload_string_len);
   /* Determine if there's a vgpreload_<tool>_<platform>.so file, and setup
      preload_string. */
   preload_tool_path = VG_(malloc)("initimg-linux.sce.2", preload_tool_path_len);
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
   if (debug) VG_(printf)("\n\n");
   envc = 0;
   for (cpp = origenv; cpp && *cpp; cpp++) {
      envc++;
      if (debug) VG_(printf)("XXXXXXXXX: BEFORE %s\n", *cpp);
   }

   /* Allocate a new space
    * Size is envc + 1 new entry + maybe one for GLIBC_TUNABLES + NULL */
   ret = VG_(malloc) ("initimg-linux.sce.3",
                      sizeof(HChar *) * (envc+1+1+(use_stack_cache_tunable ? 1 : 0)));

   /* copy it over */
   for (cpp = ret; *origenv; ) {
      if (debug) VG_(printf)("XXXXXXXXX: COPY   %s\n", *origenv);
      *cpp++ = *origenv++;
   }
   *cpp = NULL;
   
   vg_assert(envc == (cpp - ret));

   /* Walk over the new environment, mashing as we go */
   for (cpp = ret; cpp && *cpp; cpp++) {
      if (VG_(memcmp)(*cpp, ld_preload, ld_preload_len) == 0) {
         Int len = VG_(strlen)(*cpp) + preload_string_len;
         HChar *cp = VG_(malloc)("initimg-linux.sce.4", len);

         VG_(snprintf)(cp, len, "%s%s:%s",
                       ld_preload, preload_string, (*cpp)+ld_preload_len);

         *cpp = cp;

         ld_preload_done = True;
      }
      if (use_stack_cache_tunable) {
          /* overwrite value found with zeroes */
          const HChar* search_string = "glibc.pthread.stack_cache_size=";
          HChar* val;
          if ((val = VG_(strstr)(*cpp, search_string))) {
              val += VG_(strlen)(search_string);
              while (*val != '\0' && *val != ':') {
                  *val++ = '0';
              }
              use_stack_cache_tunable = False;
          }
      }
      if (debug) VG_(printf)("XXXXXXXXX: MASH   %s\n", *cpp);
   }

   /* Add the missing bits */
   if (!ld_preload_done) {
      Int len = ld_preload_len + preload_string_len;
      HChar *cp = VG_(malloc) ("initimg-linux.sce.5", len);

      VG_(snprintf)(cp, len, "%s%s", ld_preload, preload_string);

      ret[envc++] = cp;
      if (debug) VG_(printf)("XXXXXXXXX: ADD    %s\n", cp);
   }

   if (use_stack_cache_tunable) {
      ret[envc++] = VG_(strdup)("initimg-linux.sce.6", "GLIBC_TUNABLES=glibc.pthread.stack_cache_size=0");
   }

   /* ret[0 .. envc-1] is live now. */
   /* Find and remove a binding for VALGRIND_LAUNCHER. */
   for (i = 0; i < envc; i++)
      if (0 == VG_(memcmp)(ret[i], v_launcher, v_launcher_len))
         break;

   if (i < envc) {
      for (; i < envc-1; i++)
         ret[i] = ret[i+1];
      envc--;
   }

   VG_(free)(preload_string);
   ret[envc] = NULL;

   for (i = 0; i < envc; i++) {
      if (debug) VG_(printf)("XXXXXXXXX: FINAL  %s\n", ret[i]);
   }

   return ret;
}


/*====================================================================*/
/*=== Setting up the client's stack                                ===*/
/*====================================================================*/

#ifndef AT_DCACHEBSIZE
#define AT_DCACHEBSIZE		19
#endif /* AT_DCACHEBSIZE */

#ifndef AT_ICACHEBSIZE
#define AT_ICACHEBSIZE		20
#endif /* AT_ICACHEBSIZE */

#ifndef AT_UCACHEBSIZE
#define AT_UCACHEBSIZE		21
#endif /* AT_UCACHEBSIZE */

#ifndef AT_BASE_PLATFORM
#define AT_BASE_PLATFORM	24
#endif /* AT_BASE_PLATFORM */

#ifndef AT_RANDOM
#define AT_RANDOM		25
#endif /* AT_RANDOM */

#ifndef AT_HWCAP2
#define AT_HWCAP2		26
#endif /* AT_HWCAP2 */

#ifndef AT_EXECFN
#define AT_EXECFN		31
#endif /* AT_EXECFN */

#ifndef AT_SYSINFO
#define AT_SYSINFO		32
#endif /* AT_SYSINFO */

#ifndef AT_SYSINFO_EHDR
#define AT_SYSINFO_EHDR		33
#endif /* AT_SYSINFO_EHDR */

#ifndef AT_SECURE
#define AT_SECURE 23   /* secure mode boolean */
#endif	/* AT_SECURE */

/* Add a string onto the string table, and return its address */
static HChar *copy_str(HChar **tab, const HChar *str)
{
   HChar *cp = *tab;
   HChar *orig = cp;

   while(*str)
      *cp++ = *str++;
   *cp++ = '\0';

   if (0)
      VG_(printf)("copied %p \"%s\" len %lld\n", orig, orig, (Long)(cp-orig));

   *tab = cp;

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
   As a side effect of scanning our own auxv, some important bits of
   info are collected:

      VG_(cache_line_size_ppc32) // ppc32 only -- cache line size
      VG_(have_altivec_ppc32)    // ppc32 only -- is Altivec supported?

   ---------------------------------------------------------------- */

struct auxv
{
   Word a_type;
   union {
      void *a_ptr;
      Word a_val;
   } u;
};

static
struct auxv *find_auxv(UWord* sp)
{
   sp++;                // skip argc (Nb: is word-sized, not int-sized!)

   while (*sp != 0)     // skip argv
      sp++;
   sp++;

   while (*sp != 0)     // skip env
      sp++;
   sp++;
   
#if defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le)
# if defined AT_IGNOREPPC
   while (*sp == AT_IGNOREPPC)        // skip AT_IGNOREPPC entries
      sp += 2;
# endif
#endif

   return (struct auxv *)sp;
}

static 
Addr setup_client_stack( void*  init_sp,
                         HChar** orig_envp, 
                         const ExeInfo* info,
                         UInt** client_auxv,
                         Addr   clstack_end,
                         SizeT  clstack_max_size,
                         const VexArchInfo* vex_archinfo )
{
  /* The HW configuration setting (hwcaps) of the target can be
   * checked against the Vex settings of the host platform as given
   * by the values in vex_archinfo.
   */

   SysRes res;
   HChar **cpp;
   HChar *strtab;		/* string table */
   HChar *stringbase;
   Addr *ptr;
   struct auxv *auxv;
   const struct auxv *orig_auxv;
   const struct auxv *cauxv;
   unsigned stringsize;		/* total size of strings in bytes */
   unsigned auxsize;		/* total size of auxv in bytes */
   Int argc;			/* total argc */
   Int envc;			/* total number of env vars */
   unsigned stacksize;		/* total client stack size */
   Addr client_SP;	        /* client stack base (initial SP) */
   Addr clstack_start;
   Int i;

   vg_assert(VG_IS_PAGE_ALIGNED(clstack_end+1));
   vg_assert( VG_(args_for_client) );

   /* use our own auxv as a prototype */
   orig_auxv = find_auxv(init_sp);

   /* ==================== compute sizes ==================== */

   /* first of all, work out how big the client stack will be */
   stringsize   = 0;

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
   stringsize += VG_(strlen)( VG_(args_the_exename) ) + 1;

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

   /* now, how big is the auxv? */
   auxsize = sizeof(*auxv);	/* there's always at least one entry: AT_NULL */
   for (cauxv = orig_auxv; cauxv->a_type != AT_NULL; cauxv++) {
      if (cauxv->a_type == AT_PLATFORM ||
          cauxv->a_type == AT_BASE_PLATFORM)
	 stringsize += VG_(strlen)(cauxv->u.a_ptr) + 1;
      else if (cauxv->a_type == AT_RANDOM)
	 stringsize += 16;
      else if (cauxv->a_type == AT_EXECFN)
	 stringsize += VG_(strlen)(VG_(args_the_exename)) + 1;
      auxsize += sizeof(*cauxv);
   }

#  if defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux) \
      || defined(VGP_ppc64le_linux)
   auxsize += 2 * sizeof(*cauxv);
#  endif

   /* OK, now we know how big the client stack is */
   stacksize =
      sizeof(Word) +                          /* argc */
      sizeof(HChar **) +                      /* argc[0] == exename */
      sizeof(HChar **)*argc +                 /* argv */
      sizeof(HChar **) +                      /* terminal NULL */
      sizeof(HChar **)*envc +                 /* envp */
      sizeof(HChar **) +                      /* terminal NULL */
      auxsize +                               /* auxv */
      VG_ROUNDUP(stringsize, sizeof(Word));   /* strings (aligned) */

   if (0) VG_(printf)("stacksize = %u\n", stacksize);

   /* client_SP is the client's stack pointer */
   client_SP = clstack_end - stacksize;
   client_SP = VG_ROUNDDN(client_SP, 16); /* make stack 16 byte aligned */

   /* base of the string table (aligned) */
   stringbase = strtab = (HChar *)clstack_end 
                         - VG_ROUNDUP(stringsize, sizeof(int));

   clstack_start = VG_PGROUNDDN(client_SP);

   /* The max stack size */
   clstack_max_size = VG_PGROUNDUP(clstack_max_size);

   if (0)
      VG_(printf)("stringsize=%u auxsize=%u stacksize=%u maxsize=0x%lx\n"
                  "clstack_start %p\n"
                  "clstack_end   %p\n",
                  stringsize, auxsize, stacksize, clstack_max_size,
                  (void*)clstack_start, (void*)clstack_end);

   /* ==================== allocate space ==================== */

   { SizeT anon_size   = clstack_end - clstack_start + 1;
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

     if (0)
        VG_(printf)("%#lx 0x%lx  %#lx 0x%lx\n",
                    resvn_start, resvn_size, anon_start, anon_size);

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
	         info->stack_prot
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
   *ptr++ = argc + 1;

   /* --- client argv --- */
   if (info->interp_name)
      *ptr++ = (Addr)copy_str(&strtab, info->interp_name);
   if (info->interp_args)
      *ptr++ = (Addr)copy_str(&strtab, info->interp_args);

   *ptr++ = (Addr)copy_str(&strtab, VG_(args_the_exename));

   for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
      *ptr++ = (Addr)copy_str(
                       &strtab, 
                       * (HChar**) VG_(indexXA)( VG_(args_for_client), i )
                     );
   }
   *ptr++ = 0;

   /* --- envp --- */
   VG_(client_envp) = (HChar **)ptr;
   for (cpp = orig_envp; cpp && *cpp; ptr++, cpp++)
      *ptr = (Addr)copy_str(&strtab, *cpp);
   *ptr++ = 0;

   /* --- auxv --- */
   auxv = (struct auxv *)ptr;
   *client_auxv = (UInt *)auxv;
   VG_(client_auxv) = (UWord *)*client_auxv;
   // ??? According to 'man proc', auxv is a array of unsigned long
   // terminated by two zeros. Why is valgrind working with UInt ?
   // We do not take ULong* (as ULong 8 bytes on a 32 bits),
   // => we take UWord*

#  if defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux) \
      || defined(VGP_ppc64le_linux)
   auxv[0].a_type  = AT_IGNOREPPC;
   auxv[0].u.a_val = AT_IGNOREPPC;
   auxv[1].a_type  = AT_IGNOREPPC;
   auxv[1].u.a_val = AT_IGNOREPPC;
   auxv += 2;
#  endif

   for (; orig_auxv->a_type != AT_NULL; auxv++, orig_auxv++) {

      /* copy the entry... */
      *auxv = *orig_auxv;

      /* ...and fix up / examine the copy */
      switch(auxv->a_type) {

         case AT_IGNORE:
         case AT_PHENT:
         case AT_PAGESZ:
         case AT_FLAGS:
         case AT_NOTELF:
         case AT_UID:
         case AT_EUID:
         case AT_GID:
         case AT_EGID:
         case AT_CLKTCK:
#        if !defined(VGPV_arm_linux_android) \
            && !defined(VGPV_x86_linux_android) \
            && !defined(VGPV_mips32_linux_android) \
            && !defined(VGPV_arm64_linux_android)
         case AT_FPUCW: /* missing on android */
#        endif
            /* All these are pointerless, so we don't need to do
               anything about them. */
            break;

         case AT_PHDR:
            if (info->phdr == 0)
               auxv->a_type = AT_IGNORE;
            else
               auxv->u.a_val = info->phdr;
            break;

         case AT_PHNUM:
            if (info->phdr == 0)
               auxv->a_type = AT_IGNORE;
            else
               auxv->u.a_val = info->phnum;
            break;

         case AT_BASE:
            auxv->u.a_val = info->interp_offset;
            break;

         case AT_PLATFORM:
         case AT_BASE_PLATFORM:
            /* points to a platform description string */
            auxv->u.a_ptr = copy_str(&strtab, orig_auxv->u.a_ptr);
            break;

         case AT_ENTRY:
            auxv->u.a_val = info->entry;
            break;

         case AT_HWCAP:
#           if defined(VGP_arm_linux)
            { Bool has_neon = (auxv->u.a_val & VKI_HWCAP_NEON) > 0;
              VG_(debugLog)(2, "initimg",
                               "ARM has-neon from-auxv: %s\n",
                               has_neon ? "YES" : "NO");
              VG_(machine_arm_set_has_NEON)( has_neon );
#             define VKI_HWCAP_TLS 32768
              Bool has_tls = (auxv->u.a_val & VKI_HWCAP_TLS) > 0;
#             undef VKI_HWCAP_TLS
              VG_(debugLog)(2, "initimg",
                               "ARM has-tls from-auxv: %s\n",
                               has_tls ? "YES" : "NO");
              /* If real hw sets properly HWCAP_TLS, we might
                 use this info to decide to really execute set_tls syscall
                 in syswrap-arm-linux.c rather than to base this on
                 conditional compilation. */
            }
#           elif defined(VGP_s390x_linux)
            {
               /* Out of the hardware features available on the platform,
                  advertise those "below" TE, as well as the ones explicitly
                  ORed in the expression below.  Anything else, such as TE
                  itself, is not supported by Valgrind. */
               auxv->u.a_val &= ((VKI_HWCAP_S390_TE - 1)
                                 | VKI_HWCAP_S390_VXRS
                                 | VKI_HWCAP_S390_VXRS_EXT
                                 | VKI_HWCAP_S390_VXRS_EXT2);
            }
#           elif defined(VGP_arm64_linux)
            {
               /* Limit the AT_HWCAP to just those features we explicitly
		  support in VEX.  */
#define ARM64_SUPPORTED_HWCAP (VKI_HWCAP_ATOMICS        \
                               | VKI_HWCAP_AES          \
                               | VKI_HWCAP_PMULL        \
                               | VKI_HWCAP_SHA1         \
                               | VKI_HWCAP_SHA2         \
                               | VKI_HWCAP_CRC32        \
                               | VKI_HWCAP_FP           \
                               | VKI_HWCAP_ASIMD        \
                               | VKI_HWCAP_ASIMDDP)
               auxv->u.a_val &= ARM64_SUPPORTED_HWCAP;
            }
#           endif
            break;
#        if defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
         case AT_HWCAP2:  {
            Bool auxv_2_07, hw_caps_2_07;
            Bool auxv_3_0, hw_caps_3_0;
            Bool auxv_3_1, hw_caps_3_1;
            Bool auxv_scv_supported;

	    /* The HWCAP2 field may contain an arch_2_07 entry that indicates
             * if the processor is compliant with the 2.07 ISA. (i.e. Power 8
             * or beyond).  The Valgrind vai.hwcaps value
             * (coregrind/m_machine.c) has the VEX_HWCAPS_PPC64_ISA2_07
             * flag set so Valgrind knows about Power8.  Need to pass the
             * HWCAP2 value along so the user level programs can detect that
             * the processor supports ISA 2.07 and beyond.
             */
            /*  Power Architecture 64-Bit ELF V2 ABI Specification
                July 21, 2014, version 1.0, Page 124
                www-03.ibm.com/technologyconnect/tgcm/TGCMServlet.wss?alias=OpenPOWER&linkid=1n0000

                AT_HWCAP2
                The a_val member of this entry is a bit map of hardware
                capabilities. Some bit mask values include:

                PPC_FEATURE2_ARCH_2_07        0x80000000
                PPC_FEATURE2_HAS_HTM          0x40000000
                PPC_FEATURE2_HAS_DSCR         0x20000000
                PPC_FEATURE2_HAS_EBB          0x10000000
                PPC_FEATURE2_HAS_ISEL         0x08000000
                PPC_FEATURE2_HAS_TAR          0x04000000
                PPC_FEATURE2_HAS_VCRYPTO      0x02000000
                PPC_FEATURE2_HTM_NOSC         0x01000000
                PPC_FEATURE2_ARCH_3_00        0x00800000
                PPC_FEATURE2_HAS_IEEE128      0x00400000
                PPC_FEATURE2_DARN             0x00200000
                PPC_FEATURE2_SCV              0x00100000
                PPC_FEATURE2_HTM_NO_SUSPEND   0x00080000
                PPC_FEATURE2_ARCH_3_1         0x00040000
                PPC_FEATURE2_MMA              0x00020000
            */
            auxv_2_07 = (auxv->u.a_val & 0x80000000ULL) == 0x80000000ULL;
            hw_caps_2_07 = (vex_archinfo->hwcaps & VEX_HWCAPS_PPC64_ISA2_07)
               == VEX_HWCAPS_PPC64_ISA2_07;

            /* Verify the PPC_FEATURE2_ARCH_2_07 setting in HWCAP2
	     * matches the setting in VEX HWCAPS.
	     */
            vg_assert(auxv_2_07 == hw_caps_2_07);

            /*  Power ISA version 3.0B
                March 29, 2017
                https://ibm.ent.box.com/s/1hzcwkwf8rbju5h9iyf44wm94amnlcrv

                https://openpowerfoundation.org/technical/resource-catalog/
                http://openpowerfoundation.org/wp-content/uploads/resources/leabi/leabi-20170510.pdf
                64-bit ELF V2 ABI specification for Power.  HWCAP2 bit pattern
                for ISA 3.0, page 112.

            */
            /* ISA 3.0 */
            auxv_3_0 = (auxv->u.a_val & 0x00800000ULL) == 0x00800000ULL;
            hw_caps_3_0 = (vex_archinfo->hwcaps & VEX_HWCAPS_PPC64_ISA3_0)
               == VEX_HWCAPS_PPC64_ISA3_0;

            /* Verify the PPC_FEATURE2_ARCH_3_00 setting in HWCAP2
             * matches the setting in VEX HWCAPS.
             */
            vg_assert(auxv_3_0 == hw_caps_3_0);

            /*  Power ISA version 3.1
                https://ibm.ent.box.com/s/hhjfw0x0lrbtyzmiaffnbxh2fuo0fog0

                64-bit ELF V? ABI specification for Power.  HWCAP2 bit pattern
                for ISA 3.0, page ?.

                ADD PUBLIC LINK WHEN AVAILABLE
            */

            /* Check for SCV support, Can not test scv instruction to see
               if the system supports scv.  Issuing an scv intruction on a
               system that does not have scv in the HWCAPS results in a
               message in dmsg  "Facility 'SCV' unavailable (12), exception".
               Will have to just use the scv setting from HWCAPS2 to determine
               if the host supports scv.  */
            auxv_scv_supported = (auxv->u.a_val & 0x00100000ULL)
               == 0x00100000ULL;

            VG_(machine_ppc64_set_scv_support)(auxv_scv_supported);

            /* ISA 3.1 */
            auxv_3_1 = (auxv->u.a_val & 0x00040000ULL) == 0x00040000ULL;
            hw_caps_3_1 = (vex_archinfo->hwcaps & VEX_HWCAPS_PPC64_ISA3_1)
               == VEX_HWCAPS_PPC64_ISA3_1;

            /* Verify the PPC_FEATURE2_ARCH_3_1 setting in HWCAP2
             * matches the setting in VEX HWCAPS.
             */
            vg_assert(auxv_3_1 == hw_caps_3_1);

            /* Mask unrecognized HWCAP bits.  Only keep the bits that have
             * explicit support in VEX. Filter out HTM bits since the
             * transaction begin instruction (tbegin) is always failed in
             * Valgrind causing the code to execute the failure path.
             * The DARN random number (bug #411189) and the SCV syscall
             * (bug #431157) have been fixed.  Can now include them in the
             * HWCAP bits.
             */
            auxv->u.a_val &= (0x80000000ULL     /* ARCH_2_07 */
                              | 0x20000000ULL   /* DSCR */
                              | 0x10000000ULL   /* EBB */
                              | 0x08000000ULL   /* ISEL */
                              | 0x04000000ULL   /* TAR */
                              | 0x04000000ULL   /* VEC_CRYPTO */
                              | 0x00800000ULL   /* ARCH_3_00 */
#if defined(VGP_ppc64le_linux)
   /* Should also be supported on ppc64be,
      but see https://bugs.kde.org/show_bug.cgi?id=469097  */
                              | 0x00100000ULL   /* PPC_FEATURE2_SCV */
#endif
                              | 0x00400000ULL   /* HAS_IEEE128 */
                              | 0x00200000ULL   /* PPC_FEATURE2_DARN */
                              | 0x00040000ULL   /* ARCH_3_1 */
                              | 0x00020000ULL); /* MMA instruction support */
         }

            break;
#           endif

         case AT_ICACHEBSIZE:
         case AT_DCACHEBSIZE:
         case AT_UCACHEBSIZE:
#           if defined(VGP_ppc32_linux)
            /* acquire cache info */
            if (auxv->u.a_val > 0) {
               VG_(machine_ppc32_set_clszB)( auxv->u.a_val );
               VG_(debugLog)(2, "initimg", 
                                "PPC32 icache line size %u (type %u)\n", 
                                (UInt)auxv->u.a_val, (UInt)auxv->a_type );
            }
#           elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
            /* acquire cache info */
            if (auxv->u.a_val > 0) {
               VG_(machine_ppc64_set_clszB)( auxv->u.a_val );
               VG_(debugLog)(2, "initimg", 
                                "PPC64 icache line size %u (type %u)\n", 
                                (UInt)auxv->u.a_val, (UInt)auxv->a_type );
            }
#           endif
            break;

#        if defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux) \
            || defined(VGP_ppc64le_linux)
         case AT_IGNOREPPC:
            break;
#        endif

         case AT_SECURE:
            /* If this is 1, then it means that this program is
               running suid, and therefore the dynamic linker should
               be careful about LD_PRELOAD, etc.  However, since
               stage1 (the thing the kernel actually execve's) should
               never be SUID, and we need LD_PRELOAD to work for the
               client, we set AT_SECURE to 0. */
            auxv->u.a_val = 0;
            break;

         case AT_SYSINFO:
            /* Trash this, because we don't reproduce it */
            auxv->a_type = AT_IGNORE;
            break;

#        if !defined(VGP_ppc32_linux) && !defined(VGP_ppc64be_linux) \
            && !defined(VGP_ppc64le_linux) \
            && !defined(VGP_mips32_linux) && !defined(VGP_mips64_linux) \
            && !defined(VGP_nanomips_linux) \
            && !defined(VGP_s390x_linux)
         case AT_SYSINFO_EHDR: {
            /* Trash this, because we don't reproduce it */
            const NSegment* ehdrseg = VG_(am_find_nsegment)((Addr)auxv->u.a_ptr);
            vg_assert(ehdrseg);
            VG_(am_munmap_valgrind)(ehdrseg->start, ehdrseg->end - ehdrseg->start);
            auxv->a_type = AT_IGNORE;
            break;
         }
#        endif

         case AT_RANDOM:
            /* points to 16 random bytes - we need to ensure this is
               propagated to the client as glibc will assume it is
               present if it is built for kernel 2.6.29 or later */
            auxv->u.a_ptr = strtab;
            VG_(memcpy)(strtab, orig_auxv->u.a_ptr, 16);
            strtab += 16;
            break;

         case AT_EXECFN:
            /* points to the executable filename */
            auxv->u.a_ptr = copy_str(&strtab, VG_(args_the_exename));
            break;

         default:
            /* stomp out anything we don't know about */
            VG_(debugLog)(2, "initimg",
                             "stomping auxv entry %llu\n", 
                             (ULong)auxv->a_type);
            auxv->a_type = AT_IGNORE;
            break;
      }
   }
   *auxv = *orig_auxv;
   vg_assert(auxv->a_type == AT_NULL);

   vg_assert((strtab-stringbase) == stringsize);

   /* client_SP is pointing at client's argc/argv */

   if (0) VG_(printf)("startup SP = %#lx\n", client_SP);
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
         if (ok)
            VG_(brk_base) = VG_(brk_limit) = anon_start;
      }
      /* that too might have failed, but if it has, we're hosed: there
         is no Plan C. */
   }
   vg_assert(ok);

   /* We make the data segment (heap) executable because LinuxThreads on
      ppc32 creates trampolines in this area.  Also, on x86/Linux the data
      segment is RWX natively, at least according to /proc/self/maps.
      Also, having a non-executable data seg would kill any program which
      tried to create code in the data seg and then run it. */
   sres = VG_(am_mmap_anon_fixed_client)( 
             anon_start, 
             anon_size, 
             VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC
          );
   vg_assert(!sr_isError(sres));
   vg_assert(sr_Res(sres) == anon_start);
}

/*
 * In glibc 2.34 we need to use the TUNABLE mechanism to
 * disable stack cache when --sim-hints=no-nptl-pthread-stackcache
 * is specified. This needs to be done in the same manner
 * as LD_PRELOAD.
 *
 * See https://bugs.kde.org/show_bug.cgi?id=444488
 */
static Bool need_stack_cache_tunable(HChar** argv)
{
    while (argv && *argv) {
        if (VG_(strncmp)(*argv, "--sim-hints=", VG_(strlen)("--sim-hints=")) == 0) {
            if (VG_(strstr)(*argv, "no-nptl-pthread-stackcache")) {
                return True;
            }
        }
        ++argv;
    }
    return False;
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

   if (VG_(args_the_exename) == NULL)
      VG_(err_missing_prog)();

   VG_(memset)(&info, 0, sizeof(info));
   info.arch_elf_state = &iifii.arch_elf_state;

   load_client(&info, &iifii.initial_client_IP, &iifii.initial_client_TOC);

   //--------------------------------------------------------------
   // Set up client's environment
   //   p: set-libdir                   [for VG_(libdir)]
   //   p: get_helprequest_and_toolname [for toolname]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "initimg", "Setup client env\n");
   env =  setup_client_env(iicii.envp, iicii.toolname, need_stack_cache_tunable(iicii.argv));

   //--------------------------------------------------------------
   // Setup client stack, eip, and VG_(client_arg[cv])
   //   p: load_client()     [for 'info']
   //   p: fix_environment() [for 'env']
   //--------------------------------------------------------------
   {
      /* When allocating space for the client stack on Linux, take
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
      if (szB < m1) szB = m1;
      if (szB > m16) szB = m16;
      if (VG_(clo_main_stacksize) > 0) szB = VG_(clo_main_stacksize);
      if (szB < m1) szB = m1;
      szB = VG_PGROUNDUP(szB);
      VG_(debugLog)(1, "initimg",
                       "Setup client stack: size will be %lu\n", szB);

      iifii.clstack_max_size = szB;

      iifii.initial_client_SP
         = setup_client_stack( init_sp, env, 
                               &info, &iifii.client_auxv, 
                               iicii.clstack_end, iifii.clstack_max_size,
                               vex_archinfo );

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
                       iifii.clstack_max_size );
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
      if (dseg_max_size < m1) dseg_max_size = m1;
      if (dseg_max_size > m8) dseg_max_size = m8;
      dseg_max_size = VG_PGROUNDUP(dseg_max_size);

      setup_client_dataseg( dseg_max_size );
   }

   VG_(free)(info.interp_name); info.interp_name = NULL;
   VG_(free)(info.interp_args); info.interp_args = NULL;
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

   /* On Linux we get client_{ip/sp/toc}, and start the client with
      all other registers zeroed. */

#  if defined(VGP_x86_linux)
   vg_assert(0 == sizeof(VexGuestX86State) % LibVEX_GUEST_STATE_ALIGN);

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
      default code, data and stack segments.  Also %es (see #291253). */
   asm volatile("movw %%cs, %0" : : "m" (arch->vex.guest_CS));
   asm volatile("movw %%ds, %0" : : "m" (arch->vex.guest_DS));
   asm volatile("movw %%ss, %0" : : "m" (arch->vex.guest_SS));
   asm volatile("movw %%es, %0" : : "m" (arch->vex.guest_ES));

#  elif defined(VGP_amd64_linux)
   vg_assert(0 == sizeof(VexGuestAMD64State) % LibVEX_GUEST_STATE_ALIGN);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestAMD64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestAMD64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestAMD64State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_RSP = iifii.initial_client_SP;
   arch->vex.guest_RIP = iifii.initial_client_IP;

#  elif defined(VGP_ppc32_linux)
   vg_assert(0 == sizeof(VexGuestPPC32State) % LibVEX_GUEST_STATE_ALIGN);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestPPC32_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestPPC32State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestPPC32State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_GPR1 = iifii.initial_client_SP;
   arch->vex.guest_CIA  = iifii.initial_client_IP;

#  elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   vg_assert(0 == sizeof(VexGuestPPC64State) % LibVEX_GUEST_STATE_ALIGN);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestPPC64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestPPC64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestPPC64State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_GPR1 = iifii.initial_client_SP;
   arch->vex.guest_GPR2 = iifii.initial_client_TOC;
   arch->vex.guest_CIA  = iifii.initial_client_IP;
#if defined(VGP_ppc64le_linux)
   arch->vex.guest_GPR12 = iifii.initial_client_IP;
#endif

#  elif defined(VGP_arm_linux)
   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestARM_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestARMState));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestARMState));

   arch->vex.guest_R13  = iifii.initial_client_SP;
   arch->vex.guest_R15T = iifii.initial_client_IP;

   /* This is just EABI stuff. */
   // FIXME jrs: what's this for?
   arch->vex.guest_R1 =  iifii.initial_client_SP;

#  elif defined(VGP_arm64_linux)
   /* Zero out the initial state. */
   LibVEX_GuestARM64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestARM64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestARM64State));

   arch->vex.guest_XSP = iifii.initial_client_SP;
   arch->vex.guest_PC  = iifii.initial_client_IP;

#  elif defined(VGP_s390x_linux)
   vg_assert(0 == sizeof(VexGuestS390XState) % LibVEX_GUEST_STATE_ALIGN);

   /* Zero out the initial state. This also sets the guest_fpc to 0, which
      is also done by the kernel for the fpc during execve. */
   LibVEX_GuestS390X_initialise(&arch->vex);

   /* Mark all registers as undefined ... */
   VG_(memset)(&arch->vex_shadow1, 0xFF, sizeof(VexGuestS390XState));
   VG_(memset)(&arch->vex_shadow2, 0x00, sizeof(VexGuestS390XState));
   /* ... except SP, FPC, and IA */
   arch->vex_shadow1.guest_SP = 0;
   arch->vex_shadow1.guest_fpc = 0;
   arch->vex_shadow1.guest_IA = 0;

   /* Put essential stuff into the new state. */
   arch->vex.guest_SP = iifii.initial_client_SP;
   arch->vex.guest_IA = iifii.initial_client_IP;
   /* See sys_execve in <linux>/arch/s390/kernel/process.c */
   arch->vex.guest_fpc = 0;

   /* Tell the tool about the registers we just wrote */
   VG_TRACK(post_reg_write, Vg_CoreStartup, /*tid*/1, VG_O_STACK_PTR, 8);
   VG_TRACK(post_reg_write, Vg_CoreStartup, /*tid*/1, VG_O_FPC_REG,   4);
   VG_TRACK(post_reg_write, Vg_CoreStartup, /*tid*/1, VG_O_INSTR_PTR, 8);

   /* At the end of this function there is code to mark all guest state
      registers as defined. For s390 that would be wrong, because the ABI
      says that all registers except SP, IA, and FPC are undefined upon
      process startup. */
#define PRECISE_GUEST_REG_DEFINEDNESS_AT_STARTUP 1

#  elif defined(VGP_mips32_linux) || defined(VGP_nanomips_linux)
   vg_assert(0 == sizeof(VexGuestMIPS32State) % LibVEX_GUEST_STATE_ALIGN);
   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestMIPS32_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestMIPS32State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestMIPS32State));

   arch->vex.guest_r29 = iifii.initial_client_SP;
   arch->vex.guest_PC = iifii.initial_client_IP;
   arch->vex.guest_r31 = iifii.initial_client_SP;

#  if !defined(VGP_nanomips_linux)
   if (iifii.arch_elf_state.overall_fp_mode == VKI_FP_FR1) {
      arch->vex.guest_CP0_status |= MIPS_CP0_STATUS_FR;
   }

#  endif
#  elif defined(VGP_mips64_linux)
   vg_assert(0 == sizeof(VexGuestMIPS64State) % LibVEX_GUEST_STATE_ALIGN);
   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestMIPS64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestMIPS64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestMIPS64State));

   arch->vex.guest_r29 = iifii.initial_client_SP;
   arch->vex.guest_PC = iifii.initial_client_IP;
   arch->vex.guest_r31 = iifii.initial_client_SP;

#  else
#    error Unknown platform
#  endif

#  if !defined(PRECISE_GUEST_REG_DEFINEDNESS_AT_STARTUP)
   /* Tell the tool that we just wrote to the registers. */
   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));
#  endif

   /* Tell the tool about the client data segment and then kill it which will
      make it inaccessible/unaddressable. */
   const NSegment *seg = VG_(am_find_nsegment)(VG_(brk_base));
   vg_assert(seg);
   vg_assert(seg->kind == SkAnonC);
   VG_TRACK(new_mem_brk, VG_(brk_base), seg->end + 1 - VG_(brk_base),
            1/*tid*/);
   VG_TRACK(die_mem_brk, VG_(brk_base), seg->end + 1 - VG_(brk_base));
}

#endif // defined(VGO_linux)

/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/
