
/*--------------------------------------------------------------------*/
/*--- Startup: the real stuff                             m_main.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_threadstate.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_commandline.h"
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_core_execontext.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_syscall.h"       // VG_(strerror)
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_profile.h"
#include "pub_core_debuginfo.h"
#include "pub_core_redir.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_stacks.h"        // For VG_(register_stack)
#include "pub_core_syswrap.h"
#include "pub_core_translate.h"     // For VG_(translate)
#include "pub_core_tooliface.h"
#include "pub_core_trampoline.h"
#include "pub_core_transtab.h"
#include "pub_core_ume.h"


/*====================================================================*/
/*=== Counters, for profiling purposes only                        ===*/
/*====================================================================*/

static void print_all_stats ( void )
{
   VG_(print_tt_tc_stats)();
   VG_(print_scheduler_stats)();
   VG_(print_ExeContext_stats)();

   // Memory stats
   if (VG_(clo_verbosity) > 2) {
      VG_(message)(Vg_DebugMsg, "");
      VG_(message)(Vg_DebugMsg, 
         "------ Valgrind's internal memory use stats follow ------" );
      VG_(sanity_check_malloc_all)();
      VG_(message)(Vg_DebugMsg, "------" );
      VG_(print_all_arena_stats)();
      VG_(message)(Vg_DebugMsg, "");
   }
}


/*====================================================================*/
/*=== Setting up the client's environment                          ===*/
/*====================================================================*/

/* Prepare the client's environment.  This is basically a copy of our
   environment, except:

     LD_PRELOAD=$VALGRIND_LIB/vg_preload_core.so:
                ($VALGRIND_LIB/vgpreload_TOOL.so:)?
                $LD_PRELOAD

   If this is missing, then it is added.

   Also, remove any binding for VALGRIND_LAUNCHER=.  The client should
   not be able to see this.

   If this needs to handle any more variables it should be hacked
   into something table driven.  The copy is VG_(malloc)'d space.
*/
static HChar** setup_client_env ( HChar** origenv, const HChar* toolname)
{
   HChar* preload_core_so = "vg_preload_core.so";
   HChar* ld_preload      = "LD_PRELOAD=";
   HChar* v_launcher      = VALGRIND_LAUNCHER "=";
   Int    ld_preload_len  = VG_(strlen)( ld_preload );
   Int    v_launcher_len  = VG_(strlen)( v_launcher );
   Bool   ld_preload_done = False;
   Int    vglib_len       = VG_(strlen)(VG_(libdir));

   HChar** cpp;
   HChar** ret;
   HChar*  preload_tool_path;;
   Int     envc, i;

   /* Alloc space for the vgpreload_core.so path and vgpreload_<tool>.so
      paths.  We might not need the space for vgpreload_<tool>.so, but it
      doesn't hurt to over-allocate briefly.  The 16s are just cautious
      slop. */
   Int preload_core_path_len = vglib_len + sizeof(preload_core_so) + 16;
   Int preload_tool_path_len = vglib_len + VG_(strlen)(toolname)   + 16;
   Int preload_string_len    = preload_core_path_len + preload_tool_path_len;
   HChar* preload_string     = VG_(malloc)(preload_string_len);
   vg_assert(preload_string);

   /* Determine if there's a vgpreload_<tool>.so file, and setup
      preload_string. */
   preload_tool_path = VG_(malloc)(preload_tool_path_len);
   vg_assert(preload_tool_path);
   VG_(snprintf)(preload_tool_path, preload_tool_path_len,
                 "%s/vgpreload_%s.so", VG_(libdir), toolname);
   if (VG_(access)(preload_tool_path, True/*r*/, False/*w*/, False/*x*/) == 0) {
      VG_(snprintf)(preload_string, preload_string_len, "%s/%s:%s", 
                    VG_(libdir), preload_core_so, preload_tool_path);
   } else {
      VG_(snprintf)(preload_string, preload_string_len, "%s/%s", 
                    VG_(libdir), preload_core_so);
   }
   VG_(free)(preload_tool_path);

   VG_(debugLog)(1, "main", "preload_string = %s\n", preload_string);

   /* Count the original size of the env */
   envc = 0;
   for (cpp = origenv; cpp && *cpp; cpp++)
      envc++;

   /* Allocate a new space */
   ret = VG_(malloc) (sizeof(HChar *) * (envc+1+1)); /* 1 new entry + NULL */
   vg_assert(ret);

   /* copy it over */
   for (cpp = ret; *origenv; )
      *cpp++ = *origenv++;
   *cpp = NULL;
   
   vg_assert(envc == (cpp - ret));

   /* Walk over the new environment, mashing as we go */
   for (cpp = ret; cpp && *cpp; cpp++) {
      if (VG_(memcmp)(*cpp, ld_preload, ld_preload_len) == 0) {
         Int len = VG_(strlen)(*cpp) + preload_string_len;
         HChar *cp = VG_(malloc)(len);
         vg_assert(cp);

         VG_(snprintf)(cp, len, "%s%s:%s",
                       ld_preload, preload_string, (*cpp)+ld_preload_len);

         *cpp = cp;

         ld_preload_done = True;
      }
   }

   /* Add the missing bits */
   if (!ld_preload_done) {
      Int len = ld_preload_len + preload_string_len;
      HChar *cp = VG_(malloc) (len);
      vg_assert(cp);

      VG_(snprintf)(cp, len, "%s%s", ld_preload, preload_string);

      ret[envc++] = cp;
   }

   /* ret[0 .. envc-1] is live now. */
   /* Find and remove a binding for VALGRIND_LAUNCHER. */
   for (i = 0; i < envc; i++)
      if (0 == VG_(memcmp(ret[i], v_launcher, v_launcher_len)))
         break;

   if (i < envc) {
      for (; i < envc-1; i++)
         ret[i] = ret[i+1];
      envc--;
   }

   VG_(free)(preload_string);
   ret[envc] = NULL;

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
static char *copy_str(char **tab, const char *str)
{
   char *cp = *tab;
   char *orig = cp;

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

static 
Addr setup_client_stack( void*  init_sp,
                         char** orig_envp, 
                         const struct exeinfo *info,
                         UInt** client_auxv,
                         Addr   clstack_end,
                         SizeT  clstack_max_size )
{
   SysRes res;
   char **cpp;
   char *strtab;		/* string table */
   char *stringbase;
   Addr *ptr;
   struct ume_auxv *auxv;
   const struct ume_auxv *orig_auxv;
   const struct ume_auxv *cauxv;
   unsigned stringsize;		/* total size of strings in bytes */
   unsigned auxsize;		/* total size of auxv in bytes */
   Int argc;			/* total argc */
   Int envc;			/* total number of env vars */
   unsigned stacksize;		/* total client stack size */
   Addr client_SP;	        /* client stack base (initial SP) */
   Addr clstack_start;
   Int i;
   Bool have_exename;

   vg_assert(VG_IS_PAGE_ALIGNED(clstack_end+1));

   /* use our own auxv as a prototype */
   orig_auxv = VG_(find_auxv)(init_sp);

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
   if (have_exename)
      stringsize += VG_(strlen)( VG_(args_the_exename) ) + 1;

   for (i = 0; i < VG_(args_for_client).used; i++) {
      argc++;
      stringsize += VG_(strlen)( VG_(args_for_client).strs[i] ) + 1;
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
      if (cauxv->a_type == AT_PLATFORM)
	 stringsize += VG_(strlen)(cauxv->u.a_ptr) + 1;
      auxsize += sizeof(*cauxv);
   }

#  if defined(VGP_ppc32_linux)
   auxsize += 2 * sizeof(*cauxv);
#  endif

   /* OK, now we know how big the client stack is */
   stacksize =
      sizeof(Word) +                          /* argc */
      (have_exename ? sizeof(char **) : 0) +  /* argc[0] == exename */
      sizeof(char **)*argc +                  /* argv */
      sizeof(char **) +	                      /* terminal NULL */
      sizeof(char **)*envc +                  /* envp */
      sizeof(char **) +	                      /* terminal NULL */
      auxsize +                               /* auxv */
      VG_ROUNDUP(stringsize, sizeof(Word));   /* strings (aligned) */

   if (0) VG_(printf)("stacksize = %d\n", stacksize);

   /* client_SP is the client's stack pointer */
   client_SP = clstack_end - stacksize;
   client_SP = VG_ROUNDDN(client_SP, 16); /* make stack 16 byte aligned */

   /* base of the string table (aligned) */
   stringbase = strtab = (char *)clstack_end 
                         - VG_ROUNDUP(stringsize, sizeof(int));

   clstack_start = VG_PGROUNDDN(client_SP);

   /* The max stack size */
   clstack_max_size = VG_PGROUNDUP(clstack_max_size);

   /* Record stack extent -- needed for stack-change code. */
   VG_(clstk_base) = clstack_start;
   VG_(clstk_end)  = clstack_end;

   if (0)
      VG_(printf)("stringsize=%d auxsize=%d stacksize=%d maxsize=0x%x\n"
                  "clstack_start %p\n"
                  "clstack_end   %p\n",
	          stringsize, auxsize, stacksize, (Int)clstack_max_size,
                  (void*)clstack_start, (void*)clstack_end);

   /* ==================== allocate space ==================== */

   { SizeT anon_size   = clstack_end - clstack_start + 1;
     SizeT resvn_size  = clstack_max_size - anon_size;
     Addr  anon_start  = clstack_start;
     Addr  resvn_start = anon_start - resvn_size;
     SizeT inner_HACK  = 0;

     vg_assert(VG_IS_PAGE_ALIGNED(anon_size));
     vg_assert(VG_IS_PAGE_ALIGNED(resvn_size));
     vg_assert(VG_IS_PAGE_ALIGNED(anon_start));
     vg_assert(VG_IS_PAGE_ALIGNED(resvn_start));
     vg_assert(resvn_start == clstack_end + 1 - clstack_max_size);

#    ifdef ENABLE_INNER
     inner_HACK = 1024*1024; // create 1M non-fault-extending stack
#    endif

     if (0)
        VG_(printf)("%p 0x%x  %p 0x%x\n", 
                    resvn_start, resvn_size, anon_start, anon_size);

     /* Create a shrinkable reservation followed by an anonymous
        segment.  Together these constitute a growdown stack. */
     Bool ok = VG_(am_create_reservation)(
                  resvn_start,
                  resvn_size -inner_HACK,
                  SmUpper, 
                  anon_size +inner_HACK
               );
     vg_assert(ok);
     /* allocate a stack - mmap enough space for the stack */
     res = VG_(am_mmap_anon_fixed_client)(
              anon_start -inner_HACK,
              anon_size +inner_HACK,
	      VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC
	   );
     vg_assert(!res.isError); 
   }

   /* ==================== create client stack ==================== */

   ptr = (Addr*)client_SP;

   /* --- client argc --- */
   *ptr++ = argc + (have_exename ? 1 : 0);

   /* --- client argv --- */
   if (info->interp_name) {
      *ptr++ = (Addr)copy_str(&strtab, info->interp_name);
      VG_(free)(info->interp_name);
   }
   if (info->interp_args) {
      *ptr++ = (Addr)copy_str(&strtab, info->interp_args);
      VG_(free)(info->interp_args);
   }

   if (have_exename)
      *ptr++ = (Addr)copy_str(&strtab, VG_(args_the_exename));

   for (i = 0; i < VG_(args_for_client).used; i++) {
      *ptr++ = (Addr)copy_str(&strtab, VG_(args_for_client).strs[i]);
   }
   *ptr++ = 0;

   /* --- envp --- */
   VG_(client_envp) = (Char **)ptr;
   for (cpp = orig_envp; cpp && *cpp; ptr++, cpp++)
      *ptr = (Addr)copy_str(&strtab, *cpp);
   *ptr++ = 0;

   /* --- auxv --- */
   auxv = (struct ume_auxv *)ptr;
   *client_auxv = (UInt *)auxv;

#  if defined(VGP_ppc32_linux)
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
         case AT_FPUCW:
         case AT_SYSINFO:
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
            auxv->u.a_val = info->interp_base;
            break;

         case AT_PLATFORM:
            /* points to a platform description string */
            auxv->u.a_ptr = copy_str(&strtab, orig_auxv->u.a_ptr);
            break;

         case AT_ENTRY:
            auxv->u.a_val = info->entry;
            break;

         case AT_HWCAP:
#           if defined(VGP_ppc32_linux)
            /* Acquire altivecness info */
            VG_(debugLog)(1, "main", "PPC32 hwcaps: 0x%x\n", 
                                     (UInt)auxv->u.a_val);
            if (auxv->u.a_val & 0x10000000)
               VG_(have_altivec_ppc32) = 1;
            VG_(debugLog)(1, "main", "PPC32 AltiVec support: %u\n", 
                                     VG_(have_altivec_ppc32));
#           endif
            break;

         case AT_DCACHEBSIZE:
         case AT_ICACHEBSIZE:
         case AT_UCACHEBSIZE:
#           if defined(VGP_ppc32_linux)
            /* acquire cache info */
            if (auxv->u.a_val > 0) {
               VG_(cache_line_size_ppc32) = auxv->u.a_val;
               VG_(debugLog)(1, "main", 
                                "PPC32 cache line size %u (type %u)\n", 
                                (UInt)auxv->u.a_val, (UInt)auxv->a_type );
            }
#           endif
            break;

#        if defined(VGP_ppc32_linux)
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

#        if !defined(VGP_ppc32_linux)
         case AT_SYSINFO_EHDR:
            /* Trash this, because we don't reproduce it */
            auxv->a_type = AT_IGNORE;
            break;
#        endif

         default:
            /* stomp out anything we don't know about */
            VG_(debugLog)(2, "main",
                             "stomping auxv entry %lld\n", 
                             (ULong)auxv->a_type);
            auxv->a_type = AT_IGNORE;
            break;
      }
   }
   *auxv = *orig_auxv;
   vg_assert(auxv->a_type == AT_NULL);

   vg_assert((strtab-stringbase) == stringsize);

   /* client_SP is pointing at client's argc/argv */

   if (0) VG_(printf)("startup SP = %p\n", client_SP);
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

   sres = VG_(am_mmap_anon_fixed_client)( 
             anon_start, 
             anon_size, 
             VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC 
          );
   vg_assert(!sres.isError);
   vg_assert(sres.val == anon_start);
}


/*====================================================================*/
/*=== Find executable                                              ===*/
/*====================================================================*/

/* Scan a colon-separated list, and call a function on each element.
   The string must be mutable, because we insert a temporary '\0', but
   the string will end up unmodified.  (*func) should return True if it
   doesn't need to see any more.

   This routine will return True if (*func) returns True and False if
   it reaches the end of the list without that happening.
*/
static Bool scan_colsep(char *colsep, Bool (*func)(const char *))
{
   char *cp, *entry;
   int end;

   if (colsep == NULL ||
       *colsep == '\0')
      return False;

   entry = cp = colsep;

   do {
      end = (*cp == '\0');

      if (*cp == ':' || *cp == '\0') {
	 char save = *cp;

	 *cp = '\0';
	 if ((*func)(entry)) {
            *cp = save;
	    return True;
         }
	 *cp = save;
	 entry = cp+1;
      }
      cp++;
   } while(!end);

   return False;
}

/* Need a static copy because can't use dynamic mem allocation yet */
static HChar executable_name[VKI_PATH_MAX];

static Bool match_executable(const char *entry) 
{
   HChar buf[VG_(strlen)(entry) + VG_(strlen)(executable_name) + 3];

   /* empty PATH element means . */
   if (*entry == '\0')
      entry = ".";

   VG_(snprintf)(buf, sizeof(buf), "%s/%s", entry, executable_name);
   if (VG_(access)(buf, True/*r*/, False/*w*/, True/*x*/) == 0) {
      VG_(strncpy)( executable_name, buf, VKI_PATH_MAX-1 );
      executable_name[VKI_PATH_MAX-1] = 0;
      return True;
   }
   return False;
}

static HChar* find_executable ( HChar* exec )
{
   vg_assert(NULL != exec);
   VG_(strncpy)( executable_name, exec, VKI_PATH_MAX-1 );
   executable_name[VKI_PATH_MAX-1] = 0;

   if (VG_(strchr)(executable_name, '/') == NULL) {
      /* no '/' - we need to search the path */
      HChar *path = VG_(getenv)("PATH");
      scan_colsep(path, match_executable);
   }
   return executable_name;
}


/*====================================================================*/
/*=== Command line errors                                          ===*/
/*====================================================================*/

static void revert_to_stderr ( void )
{
   vg_assert( !VG_(logging_to_socket) );
   VG_(clo_log_fd) = 2; /* stderr */
}

void VG_(bad_option) ( Char* opt )
{
   revert_to_stderr();
   VG_(printf)("valgrind: Bad option '%s'; aborting.\n", opt);
   VG_(printf)("valgrind: Use --help for more information.\n");
   VG_(exit)(1);
}

static void missing_prog ( void  )
{
   revert_to_stderr();
   VG_(printf)("valgrind: no program specified\n");
   VG_(printf)("valgrind: Use --help for more information.\n");
   VG_(exit)(1);
}

static void config_error ( Char* msg )
{
   revert_to_stderr();
   VG_(printf)("valgrind: Startup or configuration error:\n   %s\n", msg);
   VG_(printf)("valgrind: Unable to start up properly.  Giving up.\n");
   VG_(exit)(1);
}


/*====================================================================*/
/*=== Loading the client                                           ===*/
/*====================================================================*/

/* Load the client whose name is VG_(argv_the_exename). */

static void load_client ( /*OUT*/struct exeinfo* info, 
                          /*OUT*/Addr* client_eip)
{
   HChar* exec;
   Int    ret;
   SysRes res;

   vg_assert( VG_(args_the_exename) != NULL);
   exec = find_executable( VG_(args_the_exename) );

   VG_(memset)(info, 0, sizeof(*info));
   info->exe_base = VG_(client_base);
   info->exe_end  = VG_(client_end);

   ret = VG_(do_exec)(exec, info);
   if (ret != 0) {
      VG_(printf)("valgrind: do_exec(%s) failed: %s\n",
                  exec, VG_(strerror)(ret));
      VG_(exit)(127);
   }

   /* Get hold of a file descriptor which refers to the client
      executable.  This is needed for attaching to GDB. */
   res = VG_(open)(exec, VKI_O_RDONLY, VKI_S_IRUSR);
   if (!res.isError)
      VG_(cl_exec_fd) = res.val;

   /* Copy necessary bits of 'info' that were filled in */
   *client_eip = info->init_eip;
   VG_(brk_base) = VG_(brk_limit) = VG_PGROUNDUP(info->brkbase);
}


/*====================================================================*/
/*=== Command-line: variables, processing, etc                     ===*/
/*====================================================================*/

// See pub_{core,tool}_options.h for explanations of all these.

static void usage_NORETURN ( Bool debug_help )
{
   Char* usage1 = 
"usage: valgrind --tool=<toolname> [options] prog-and-args\n"
"\n"
"  common user options for all Valgrind tools, with defaults in [ ]:\n"
"    --tool=<name>             use the Valgrind tool named <name> [memcheck]\n"
"    -h --help                 show this message\n"
"    --help-debug              show this message, plus debugging options\n"
"    --version                 show version\n"
"    -q --quiet                run silently; only print error msgs\n"
"    -v --verbose              be more verbose, incl counts of errors\n"
"    --trace-children=no|yes   Valgrind-ise child processes? [no]\n"
"    --track-fds=no|yes        track open file descriptors? [no]\n"
"    --time-stamp=no|yes       add timestamps to log messages? [no]\n"
"    --log-fd=<number>         log messages to file descriptor [2=stderr]\n"
"    --log-file=<file>         log messages to <file>.pid<pid>\n"
"    --log-file-exactly=<file> log messages to <file>\n"
"    --log-file-qualifier=<VAR> incorporate $VAR in logfile name [none]\n"
"    --log-socket=ipaddr:port  log messages to socket ipaddr:port\n"
"\n"
"  uncommon user options for all Valgrind tools:\n"
"    --run-libc-freeres=no|yes free up glibc memory at exit? [yes]\n"
"    --weird-hacks=hack1,hack2,...  known hacks: lax-ioctls\n"
"                                                enable-outer [none]\n"
"    --pointercheck=no|yes     enforce client address space limits [yes]\n"
"    --show-emwarns=no|yes     show warnings about emulation limits? [no]\n"
"    --smc-check=none|stack|all  checks for self-modifying code: none,\n"
"                              only for code found in stacks, or all [stack]\n"
"\n"
"  user options for Valgrind tools that report errors:\n"
"    --xml=yes                 all output is in XML (Memcheck/Nulgrind only)\n"
"    --xml-user-comment=STR    copy STR verbatim to XML output\n"
"    --demangle=no|yes         automatically demangle C++ names? [yes]\n"
"    --num-callers=<number>    show <num> callers in stack traces [12]\n"
"    --error-limit=no|yes      stop showing new errors if too many? [yes]\n"
"    --show-below-main=no|yes  continue stack traces below main() [no]\n"
"    --suppressions=<filename> suppress errors described in <filename>\n"
"    --gen-suppressions=no|yes|all    print suppressions for errors? [no]\n"
"    --db-attach=no|yes        start debugger when errors detected? [no]\n"
"    --db-command=<command>    command to start debugger [gdb -nw %%f %%p]\n"
"    --input-fd=<number>       file descriptor for input [0=stdin]\n"
"    --max-stackframe=<number> assume stack switch for SP changes larger\n"
"                              than <number> bytes [2000000]\n"
"\n";

   Char* usage2 = 
"\n"
"  debugging options for all Valgrind tools:\n"
"    --sanity-level=<number>   level of sanity checking to do [1]\n"
"    --profile=no|yes          profile? (tool must be built for it) [no]\n"
"    --trace-flags=<XXXXXXXX>   show generated code? (X = 0|1) [00000000]\n"
"    --profile-flags=<XXXXXXXX> ditto, but for profiling (X = 0|1) [00000000]\n"
"    --trace-notbelow=<number>    only show BBs above <number> [0]\n"
"    --trace-syscalls=no|yes   show all system calls? [no]\n"
"    --trace-signals=no|yes    show signal handling details? [no]\n"
"    --trace-symtab=no|yes     show symbol table details? [no]\n"
"    --trace-cfi=no|yes        show call-frame-info details? [no]\n"
"    --trace-sched=no|yes      show thread scheduler details? [no]\n"
"    --wait-for-gdb=yes|no     pause on startup to wait for gdb attach\n"
#if 0
"    --model-pthreads=yes|no   model the pthreads library [no]\n"
#endif
"    --command-line-only=no|yes  only use command line options [no]\n"
"\n"
"    --vex-iropt-verbosity             0 .. 9 [0]\n"
"    --vex-iropt-level                 0 .. 2 [2]\n"
"    --vex-iropt-precise-memory-exns   [no]\n"
"    --vex-iropt-unroll-thresh         0 .. 400 [120]\n"
"    --vex-guest-max-insns             1 .. 100 [50]\n"
"    --vex-guest-chase-thresh          0 .. 99  [10]\n"
"\n"
"    --trace-flags and --profile-flags values (omit the middle space):\n"
"       1000 0000   show conversion into IR\n"
"       0100 0000   show after initial opt\n"
"       0010 0000   show after instrumentation\n"
"       0001 0000   show after second opt\n"
"       0000 1000   show after tree building\n"
"       0000 0100   show selecting insns\n"
"       0000 0010   show after reg-alloc\n"
"       0000 0001   show final assembly\n"
"\n"
"  debugging options for Valgrind tools that report errors\n"
"    --dump-error=<number>     show translation for basic block associated\n"
"                              with <number>'th error context [0=show none]\n"
"\n";

   Char* usage3 =
"\n"
"  Extra options read from ~/.valgrindrc, $VALGRIND_OPTS, ./.valgrindrc\n"
"\n"
"  Valgrind is Copyright (C) 2000-2005 Julian Seward et al.\n"
"  and licensed under the GNU General Public License, version 2.\n"
"  Bug reports, feedback, admiration, abuse, etc, to: %s.\n"
"\n"
"  Tools are copyright and licensed by their authors.  See each\n"
"  tool's start-up message for more information.\n"
"\n";

   // Ensure the message goes to stdout
   VG_(clo_log_fd) = 1;
   vg_assert( !VG_(logging_to_socket) );

   VG_(printf)(usage1);
   if (VG_(details).name) {
      VG_(printf)("  user options for %s:\n", VG_(details).name);
      if (VG_(needs).command_line_options)
	 VG_TDICT_CALL(tool_print_usage);
      else
	 VG_(printf)("    (none)\n");
   }
   if (debug_help) {
      VG_(printf)(usage2);

      if (VG_(details).name) {
         VG_(printf)("  debugging options for %s:\n", VG_(details).name);
      
         if (VG_(needs).command_line_options)
            VG_TDICT_CALL(tool_print_debug_usage);
         else
            VG_(printf)("    (none)\n");
      }
   }
   VG_(printf)(usage3, VG_BUGS_TO);
   VG_(exit)(0);
}


/* Peer at previously set up VG_(args_for_valgrind) and extract any
   request for help and also the tool name. */

static void get_helprequest_and_toolname ( Int* need_help, HChar** tool )
{
   UInt   i;
   HChar* str;

   /* parse the options we have (only the options we care about now) */
   for (i = 0; i < VG_(args_for_valgrind).used; i++) {

      str = VG_(args_for_valgrind).strs[i];
      vg_assert(str);

      if (VG_STREQ(str, "--version")) {
         VG_(printf)("valgrind-" VERSION "\n");
         VG_(exit)(0);

      } else if (VG_CLO_STREQ(str, "--help") ||
                 VG_CLO_STREQ(str, "-h")) {
         *need_help = 1;

      } else if (VG_CLO_STREQ(str, "--help-debug")) {
         *need_help = 2;

      // The tool has already been determined, but we need to know the name
      // here.
      } else if (VG_CLO_STREQN(7, str, "--tool=")) {
         *tool = &str[7];
      }
   }
}

static Bool process_cmd_line_options( UInt* client_auxv, const char* toolname )
{
   SysRes sres;
   Int    i, eventually_log_fd;
   Int    toolname_len = VG_(strlen)(toolname);
   enum {
      VgLogTo_Fd,
      VgLogTo_File,
      VgLogTo_FileExactly,
      VgLogTo_Socket
   } log_to = VgLogTo_Fd;   // Where is logging output to be sent?

   /* log to stderr by default, but usage message goes to stdout */
   eventually_log_fd = 2; 

   /* Check for sane path in ./configure --prefix=... */
   if (VG_LIBDIR[0] != '/') 
     config_error("Please use absolute paths in "
                  "./configure --prefix=... or --libdir=...");

   for (i = 0; i < VG_(args_for_valgrind).used; i++) {

      HChar* arg   = VG_(args_for_valgrind).strs[i];
      HChar* colon = arg;

      /* Look for a colon in the switch name */
      while (*colon && *colon != ':' && *colon != '=')
         colon++;

      /* Look for matching "--toolname:foo" */
      if (*colon == ':') {
         if (VG_CLO_STREQN(2,            arg,                "--") && 
             VG_CLO_STREQN(toolname_len, arg+2,              toolname) &&
             VG_CLO_STREQN(1,            arg+2+toolname_len, ":"))
         {
            // prefix matches, convert "--toolname:foo" to "--foo"
            if (0)
               VG_(printf)("tool-specific arg: %s\n", arg);
            arg = VG_(strdup)(arg + toolname_len + 1);
            arg[0] = '-';
            arg[1] = '-';

         } else {
            // prefix doesn't match, skip to next arg
            continue;
         }
      }
      
      /* Ignore these options - they've already been handled */
      if (VG_CLO_STREQN( 7, arg, "--tool="))              goto skip_arg;
      if (VG_CLO_STREQN(20, arg, "--command-line-only=")) goto skip_arg;

      if (     VG_CLO_STREQ(arg, "--"))                  goto skip_arg;

      else if (VG_CLO_STREQ(arg, "-v") ||
               VG_CLO_STREQ(arg, "--verbose"))
         VG_(clo_verbosity)++;

      else if (VG_CLO_STREQ(arg, "-q") ||
               VG_CLO_STREQ(arg, "--quiet"))
         VG_(clo_verbosity)--;

      else if (VG_CLO_STREQ(arg, "-d")) {
         /* do nothing */
      }

      else VG_BOOL_CLO(arg, "--xml",              VG_(clo_xml))
      else VG_BOOL_CLO(arg, "--db-attach",        VG_(clo_db_attach))
      else VG_BOOL_CLO(arg, "--demangle",         VG_(clo_demangle))
      else VG_BOOL_CLO(arg, "--error-limit",      VG_(clo_error_limit))
      else VG_BOOL_CLO(arg, "--pointercheck",     VG_(clo_pointercheck))
      else VG_BOOL_CLO(arg, "--show-emwarns",     VG_(clo_show_emwarns))
      else VG_NUM_CLO (arg, "--max-stackframe",   VG_(clo_max_stackframe))
      else VG_BOOL_CLO(arg, "--profile",          VG_(clo_profile))
      else VG_BOOL_CLO(arg, "--run-libc-freeres", VG_(clo_run_libc_freeres))
      else VG_BOOL_CLO(arg, "--show-below-main",  VG_(clo_show_below_main))
      else VG_BOOL_CLO(arg, "--time-stamp",       VG_(clo_time_stamp))
      else VG_BOOL_CLO(arg, "--track-fds",        VG_(clo_track_fds))
      else VG_BOOL_CLO(arg, "--trace-children",   VG_(clo_trace_children))
      else VG_BOOL_CLO(arg, "--trace-sched",      VG_(clo_trace_sched))
      else VG_BOOL_CLO(arg, "--trace-signals",    VG_(clo_trace_signals))
      else VG_BOOL_CLO(arg, "--trace-symtab",     VG_(clo_trace_symtab))
      else VG_BOOL_CLO(arg, "--trace-cfi",        VG_(clo_trace_cfi))
      else VG_BOOL_CLO(arg, "--trace-redir",      VG_(clo_trace_redir))
      else VG_BOOL_CLO(arg, "--trace-syscalls",   VG_(clo_trace_syscalls))
      else VG_BOOL_CLO(arg, "--trace-pthreads",   VG_(clo_trace_pthreads))
      else VG_BOOL_CLO(arg, "--wait-for-gdb",     VG_(clo_wait_for_gdb))
      else VG_BOOL_CLO(arg, "--model-pthreads",   VG_(clo_model_pthreads))

      else VG_STR_CLO (arg, "--db-command",       VG_(clo_db_command))
      else VG_STR_CLO (arg, "--weird-hacks",      VG_(clo_weird_hacks))

      else VG_NUM_CLO (arg, "--dump-error",       VG_(clo_dump_error))
      else VG_NUM_CLO (arg, "--input-fd",         VG_(clo_input_fd))
      else VG_NUM_CLO (arg, "--sanity-level",     VG_(clo_sanity_level))
      else VG_BNUM_CLO(arg, "--num-callers",      VG_(clo_backtrace_size), 1,
                                                  VG_DEEPEST_BACKTRACE)

      else if (VG_CLO_STREQ(arg, "--smc-check=none"))
         VG_(clo_smc_check) = Vg_SmcNone;
      else if (VG_CLO_STREQ(arg, "--smc-check=stack"))
         VG_(clo_smc_check) = Vg_SmcStack;
      else if (VG_CLO_STREQ(arg, "--smc-check=all"))
         VG_(clo_smc_check) = Vg_SmcAll;

      else VG_BNUM_CLO(arg, "--vex-iropt-verbosity",
                       VG_(clo_vex_control).iropt_verbosity, 0, 10)
      else VG_BNUM_CLO(arg, "--vex-iropt-level",
                       VG_(clo_vex_control).iropt_level, 0, 2)
      else VG_BOOL_CLO(arg, "--vex-iropt-precise-memory-exns",
                       VG_(clo_vex_control).iropt_precise_memory_exns)
      else VG_BNUM_CLO(arg, "--vex-iropt-unroll-thresh",
                       VG_(clo_vex_control).iropt_unroll_thresh, 0, 400)
      else VG_BNUM_CLO(arg, "--vex-guest-max-insns",
                       VG_(clo_vex_control).guest_max_insns, 1, 100)
      else VG_BNUM_CLO(arg, "--vex-guest-chase-thresh",
                       VG_(clo_vex_control).guest_chase_thresh, 0, 99)

      else if (VG_CLO_STREQN(9,  arg, "--log-fd=")) {
         log_to            = VgLogTo_Fd;
         VG_(clo_log_name) = NULL;
         eventually_log_fd = (Int)VG_(atoll)(&arg[9]);
      }

      else if (VG_CLO_STREQN(11, arg, "--log-file=")) {
         log_to            = VgLogTo_File;
         VG_(clo_log_name) = &arg[11];
      }

      else if (VG_CLO_STREQN(11, arg, "--log-file=")) {
         log_to            = VgLogTo_File;
         VG_(clo_log_name) = &arg[11];
      }

      else if (VG_CLO_STREQN(21, arg, "--log-file-qualifier=")) {
         VG_(clo_log_file_qualifier) = &arg[21];
      }

      else if (VG_CLO_STREQN(19, arg, "--log-file-exactly=")) {
         log_to            = VgLogTo_FileExactly;
         VG_(clo_log_name) = &arg[19];
      }

      else if (VG_CLO_STREQN(13, arg, "--log-socket=")) {
         log_to            = VgLogTo_Socket;
         VG_(clo_log_name) = &arg[13];
      }

      else if (VG_CLO_STREQN(19, arg, "--xml-user-comment=")) {
         VG_(clo_xml_user_comment) = &arg[19];
      }

      else if (VG_CLO_STREQN(15, arg, "--suppressions=")) {
         if (VG_(clo_n_suppressions) >= VG_CLO_MAX_SFILES) {
            VG_(message)(Vg_UserMsg, "Too many suppression files specified.");
            VG_(message)(Vg_UserMsg, 
                         "Increase VG_CLO_MAX_SFILES and recompile.");
            VG_(bad_option)(arg);
         }
         VG_(clo_suppressions)[VG_(clo_n_suppressions)] = &arg[15];
         VG_(clo_n_suppressions)++;
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if (VG_CLO_STREQN(14, arg, "--trace-flags=")) {
         Int j;
         char* opt = & arg[14];
   
         if (8 != VG_(strlen)(opt)) {
            VG_(message)(Vg_UserMsg, 
                         "--trace-flags argument must have 8 digits");
            VG_(bad_option)(arg);
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == opt[j]) { /* do nothing */ }
            else if ('1' == opt[j]) VG_(clo_trace_flags) |= (1 << (7-j));
            else {
               VG_(message)(Vg_UserMsg, "--trace-flags argument can only "
                                        "contain 0s and 1s");
               VG_(bad_option)(arg);
            }
         }
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if (VG_CLO_STREQN(16, arg, "--profile-flags=")) {
         Int j;
         char* opt = & arg[16];
   
         if (8 != VG_(strlen)(opt)) {
            VG_(message)(Vg_UserMsg, 
                         "--profile-flags argument must have 8 digits");
            VG_(bad_option)(arg);
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == opt[j]) { /* do nothing */ }
            else if ('1' == opt[j]) VG_(clo_profile_flags) |= (1 << (7-j));
            else {
               VG_(message)(Vg_UserMsg, "--profile-flags argument can only "
                                        "contain 0s and 1s");
               VG_(bad_option)(arg);
            }
         }
      }

      else VG_NUM_CLO (arg, "--trace-notbelow",   VG_(clo_trace_notbelow))

      else if (VG_CLO_STREQ(arg, "--gen-suppressions=no"))
         VG_(clo_gen_suppressions) = 0;
      else if (VG_CLO_STREQ(arg, "--gen-suppressions=yes"))
         VG_(clo_gen_suppressions) = 1;
      else if (VG_CLO_STREQ(arg, "--gen-suppressions=all"))
         VG_(clo_gen_suppressions) = 2;

      else if ( ! VG_(needs).command_line_options
             || ! VG_TDICT_CALL(tool_process_cmd_line_option, arg) ) {
         VG_(bad_option)(arg);
      }
    skip_arg:
      if (arg != VG_(args_for_valgrind).strs[i]) {
         VG_(free)(arg);
      }
   }

   /* Make VEX control parameters sane */

   if (VG_(clo_vex_control).guest_chase_thresh
       >= VG_(clo_vex_control).guest_max_insns)
      VG_(clo_vex_control).guest_chase_thresh
         = VG_(clo_vex_control).guest_max_insns - 1;

   if (VG_(clo_vex_control).guest_chase_thresh < 0)
      VG_(clo_vex_control).guest_chase_thresh = 0;

   /* Check various option values */

   if (VG_(clo_verbosity) < 0)
      VG_(clo_verbosity) = 0;

   if (VG_(clo_db_attach) && VG_(clo_trace_children)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "--db-attach=yes conflicts with --trace-children=yes");
      VG_(message)(Vg_UserMsg, 
         "Please choose one or the other, but not both.");
      VG_(bad_option)("--db-attach=yes and --trace-children=yes");
   }

   if (VG_(clo_gen_suppressions) > 0 && 
       !VG_(needs).core_errors && !VG_(needs).tool_errors) {
      VG_(message)(Vg_UserMsg, 
                   "Can't use --gen-suppressions= with this tool,");
      VG_(message)(Vg_UserMsg, 
                   "as it doesn't generate errors.");
      VG_(bad_option)("--gen-suppressions=");
   }

   /* If we've been asked to emit XML, mash around various other
      options so as to constrain the output somewhat, and to remove
      any need for user input during the run. */
   if (VG_(clo_xml)) {
      /* Disable suppression generation (requires user input) */
      VG_(clo_gen_suppressions) = 0;
      /* Disable attaching to GDB (requires user input) */
      VG_(clo_db_attach) = False;
      /* Set a known verbosity level */
      VG_(clo_verbosity) = 1;
      /* Disable error limits (this might be a bad idea!) */
      VG_(clo_error_limit) = False;
      /* Disable emulation warnings */
      VG_(clo_show_emwarns) = False;
      /* Disable waiting for GDB to debug Valgrind */
      VG_(clo_wait_for_gdb) = False;
      /* No file-descriptor leak checking yet */
      VG_(clo_track_fds) = False;
      /* Disable timestamped output */
      VG_(clo_time_stamp) = False;
      /* Also, we want to set options for the leak checker, but that
         will have to be done in Memcheck's flag-handling code, not
         here. */
   }

   /* All non-logging-related options have been checked.  If the logging
      option specified is ok, we can switch to it, as we know we won't
      have to generate any other command-line-related error messages.
      (So far we should be still attached to stderr, so we can show on
      the terminal any problems to do with processing command line
      opts.)
   
      So set up logging now.  After this is done, VG_(clo_log_fd)
      should be connected to whatever sink has been selected, and we
      indiscriminately chuck stuff into it without worrying what the
      nature of it is.  Oh the wonder of Unix streams. */

   vg_assert(VG_(clo_log_fd) == 2 /* stderr */);
   vg_assert(VG_(logging_to_socket) == False);

   switch (log_to) {

      case VgLogTo_Fd: 
         vg_assert(VG_(clo_log_name) == NULL);
         VG_(clo_log_fd) = eventually_log_fd;
         break;

      case VgLogTo_File: {
         HChar  logfilename[1000];
	 Int    seq  = 0;
	 Int    pid  = VG_(getpid)();
         HChar* qual = NULL;

         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */

	 if (VG_(clo_log_file_qualifier)) {
            qual = VG_(getenv)(VG_(clo_log_file_qualifier));
	 }

	 for (;;) {
            HChar pidtxt[20], seqtxt[20];

            VG_(sprintf)(pidtxt, "%d", pid);

            if (seq == 0)
               seqtxt[0] = 0;
            else
               VG_(sprintf)(seqtxt, ".%d", seq);

	    seq++;

            /* Result:
                  if (qual)      base_name ++ "." ++ qual ++ seqtxt
                  if (not qual)  base_name ++ "." ++ pid  ++ seqtxt
            */
            VG_(sprintf)( logfilename, 
                          "%s.%s%s",
                          VG_(clo_log_name), 
                          qual ? qual : pidtxt,
                          seqtxt );

            // EXCL: it will fail with EEXIST if the file already exists.
            sres
	       = VG_(open)(logfilename, 
			   VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC, 
			   VKI_S_IRUSR|VKI_S_IWUSR);
	    if (!sres.isError) {
               eventually_log_fd = sres.val;
	       VG_(clo_log_fd) = VG_(safe_fd)(eventually_log_fd);
	       break; /* for (;;) */
	    } else {
               // If the file already existed, we try the next name.  If it
               // was some other file error, we give up.
	       if (sres.val != VKI_EEXIST) {
		  VG_(message)(Vg_UserMsg, 
			       "Can't create/open log file '%s.pid%d'; giving up!", 
			       VG_(clo_log_name), pid);
		  VG_(bad_option)(
		     "--log-file=<file> (didn't work out for some reason.)");
                  /*NOTREACHED*/
	       }
	    }
	 }
         break; /* switch (VG_(clo_log_to)) */
      }

      case VgLogTo_FileExactly: {
         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */

         sres
            = VG_(open)(VG_(clo_log_name),
                        VKI_O_CREAT|VKI_O_WRONLY|VKI_O_TRUNC, 
                        VKI_S_IRUSR|VKI_S_IWUSR);
         if (!sres.isError) {
            eventually_log_fd = sres.val;
            VG_(clo_log_fd) = VG_(safe_fd)(eventually_log_fd);
         } else {
            VG_(message)(Vg_UserMsg, 
                         "Can't create/open log file '%s'; giving up!", 
                         VG_(clo_log_name));
            VG_(bad_option)(
               "--log-file-exactly=<file> (didn't work out for some reason.)");
            /*NOTREACHED*/
	 }
         break; /* switch (VG_(clo_log_to)) */
      }

      case VgLogTo_Socket: {
         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */
         eventually_log_fd = VG_(connect_via_socket)( VG_(clo_log_name) );
         if (eventually_log_fd == -1) {
            VG_(message)(Vg_UserMsg, 
               "Invalid --log-socket=ipaddr or --log-socket=ipaddr:port spec"); 
            VG_(message)(Vg_UserMsg, 
               "of '%s'; giving up!", VG_(clo_log_name) );
            VG_(bad_option)(
               "--log-socket=");
            /*NOTREACHED*/
	 }
         if (eventually_log_fd == -2) {
            VG_(message)(Vg_UserMsg, 
               "valgrind: failed to connect to logging server '%s'.",
               VG_(clo_log_name) ); 
            VG_(message)(Vg_UserMsg, 
                "Log messages will sent to stderr instead." );
            VG_(message)(Vg_UserMsg, 
                "" );
            /* We don't change anything here. */
            vg_assert(VG_(clo_log_fd) == 2);
	 } else {
            vg_assert(eventually_log_fd > 0);
            VG_(clo_log_fd) = eventually_log_fd;
            VG_(logging_to_socket) = True;
         }
         break;
      }
   }


   /* Check that the requested tool actually supports XML output. */
   if (VG_(clo_xml) && !VG_STREQ(toolname, "memcheck")
                    && !VG_STREQ(toolname, "none")) {
      VG_(clo_xml) = False;
      VG_(message)(Vg_UserMsg, 
         "Currently only Memcheck|None supports XML output."); 
      VG_(bad_option)("--xml=yes");
      /*NOTREACHED*/
   }

   // Move log_fd into the safe range, so it doesn't conflict with any app fds.
   // XXX: this is more or less duplicating the behaviour of the calls to
   // VG_(safe_fd)() above, although this does not close the original fd.
   // Perhaps the VG_(safe_fd)() calls above should be removed, and this
   // code should be replaced with a call to VG_(safe_fd)().   --njn
   eventually_log_fd = VG_(fcntl)(VG_(clo_log_fd), VKI_F_DUPFD, VG_(fd_hard_limit));
   if (eventually_log_fd < 0)
      VG_(message)(Vg_UserMsg, "valgrind: failed to move logfile fd into safe range");
   else {
      VG_(clo_log_fd) = eventually_log_fd;
      VG_(fcntl)(VG_(clo_log_fd), VKI_F_SETFD, VKI_FD_CLOEXEC);
   }

   if (VG_(clo_n_suppressions) < VG_CLO_MAX_SFILES-1 &&
       (VG_(needs).core_errors || VG_(needs).tool_errors)) {
      /* If we haven't reached the max number of suppressions, load
         the default one. */
      static const Char default_supp[] = "default.supp";
      Int len = VG_(strlen)(VG_(libdir)) + 1 + sizeof(default_supp);
      Char *buf = VG_(arena_malloc)(VG_AR_CORE, len);
      VG_(sprintf)(buf, "%s/%s", VG_(libdir), default_supp);
      VG_(clo_suppressions)[VG_(clo_n_suppressions)] = buf;
      VG_(clo_n_suppressions)++;
   }

   return (log_to == VgLogTo_Fd);
}


/*====================================================================*/
/*=== Printing the preamble                                        ===*/
/*====================================================================*/

/* Ok, the logging sink is running now.  Print a suitable preamble.
   If logging to file or a socket, write details of parent PID and
   command line args, to help people trying to interpret the
   results of a run which encompasses multiple processes. */
static void print_preamble(Bool logging_to_fd, const char* toolname)
{
   Int i;
   
   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "<?xml version=\"1.0\"?>");
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<valgrindoutput>");
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<protocolversion>1</protocolversion>");
      VG_(message)(Vg_UserMsg, "");
   }

   HChar* xpre  = VG_(clo_xml) ? "  <line>" : "";
   HChar* xpost = VG_(clo_xml) ? "</line>" : "";

   if (VG_(clo_verbosity > 0)) {

      if (VG_(clo_xml))
         VG_(message)(Vg_UserMsg, "<preamble>");

      /* Tool details */
      VG_(message)(Vg_UserMsg, "%s%s%s%s, %s.%s",
                   xpre,
                   VG_(details).name, 
                   NULL == VG_(details).version ? "" : "-",
                   NULL == VG_(details).version 
                      ? (Char*)"" : VG_(details).version,
                   VG_(details).description,
                   xpost);
      VG_(message)(Vg_UserMsg, "%s%s%s", 
                               xpre, VG_(details).copyright_author, xpost);

      /* Core details */
      VG_(message)(Vg_UserMsg,
         "%sUsing LibVEX rev %s, a library for dynamic binary translation.%s",
         xpre, LibVEX_Version(), xpost );
      VG_(message)(Vg_UserMsg, 
         "%sCopyright (C) 2004-2005, and GNU GPL'd, by OpenWorks LLP.%s",
         xpre, xpost );
      VG_(message)(Vg_UserMsg,
         "%sUsing valgrind-%s, a dynamic binary instrumentation framework.%s",
         xpre, VERSION, xpost);
      VG_(message)(Vg_UserMsg, 
         "%sCopyright (C) 2000-2005, and GNU GPL'd, by Julian Seward et al.%s",
         xpre, xpost );

      if (VG_(clo_verbosity) == 1 && !VG_(clo_xml))
         VG_(message)(Vg_UserMsg, "For more details, rerun with: -v");

      if (VG_(clo_xml))
         VG_(message)(Vg_UserMsg, "</preamble>");
   }

   if (!VG_(clo_xml) && VG_(clo_verbosity) > 0 && !logging_to_fd) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "My PID = %d, parent PID = %d.  Prog and args are:",
         VG_(getpid)(), VG_(getppid)() );
      if (VG_(args_the_exename))
         VG_(message)(Vg_UserMsg, "   %s", VG_(args_the_exename));
      for (i = 0; i < VG_(args_for_client).used; i++) 
         VG_(message)(Vg_UserMsg, "   %s", VG_(args_for_client).strs[i]);
      if (VG_(clo_log_file_qualifier)) {
         HChar* val = VG_(getenv)(VG_(clo_log_file_qualifier));
         VG_(message)(Vg_UserMsg, "");
         VG_(message)(Vg_UserMsg, "Log file qualifier: var %s, value %s.",
                                  VG_(clo_log_file_qualifier),
                                  val ? val : "");
      }
   }
   else
   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<pid>%d</pid>", VG_(getpid)());
      VG_(message)(Vg_UserMsg, "<ppid>%d</ppid>", VG_(getppid)());
      VG_(message)(Vg_UserMsg, "<tool>%t</tool>", toolname);
      if (VG_(clo_log_file_qualifier)) {
         HChar* val = VG_(getenv)(VG_(clo_log_file_qualifier));
         VG_(message)(Vg_UserMsg, "<logfilequalifier> <var>%t</var> "
                                  "<value>%t</value> </logfilequalifier>",
                                  VG_(clo_log_file_qualifier),
                                  val ? val : "");
      }
      if (VG_(clo_xml_user_comment)) {
         /* Note: the user comment itself is XML and is therefore to
            be passed through verbatim (%s) rather than escaped
            (%t). */
         VG_(message)(Vg_UserMsg, "<usercomment>%s</usercomment>",
                                  VG_(clo_xml_user_comment));
      }
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<args>");

      VG_(message)(Vg_UserMsg, "  <vargv>");
      if (VG_(name_of_launcher))
         VG_(message)(Vg_UserMsg, "    <exe>%t</exe>", 
                                  VG_(name_of_launcher));
      for (i = 0; i < VG_(args_for_valgrind).used; i++) {
         VG_(message)(Vg_UserMsg, 
                      "    <arg>%t</arg>", 
                      VG_(args_for_valgrind).strs[i]);
      }
      VG_(message)(Vg_UserMsg, "  </vargv>");

      VG_(message)(Vg_UserMsg, "  <argv>");
      if (VG_(args_the_exename))
         VG_(message)(Vg_UserMsg, "    <exe>%t</exe>", 
                                  VG_(args_the_exename));
      for (i = 0; i < VG_(args_for_client).used; i++) {
         VG_(message)(Vg_UserMsg, "    <arg>%t</arg>", 
                                  VG_(args_for_client).strs[i]);
      }
      VG_(message)(Vg_UserMsg, "  </argv>");

      VG_(message)(Vg_UserMsg, "</args>");
   }

   // Empty line after the preamble
   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   if (VG_(clo_verbosity) > 1) {
      SysRes fd;
      if (!logging_to_fd)
         VG_(message)(Vg_DebugMsg, "");
      VG_(message)(Vg_DebugMsg, "Valgrind library directory: %s", VG_(libdir));

      VG_(message)(Vg_DebugMsg, "Command line");
      if (VG_(args_the_exename))
         VG_(message)(Vg_DebugMsg, "   %s", VG_(args_the_exename));
      for (i = 0; i < VG_(args_for_client).used; i++)
         VG_(message)(Vg_DebugMsg, "   %s", VG_(args_for_client).strs[i]);

      VG_(message)(Vg_DebugMsg, "Startup, with flags:");
      for (i = 0; i < VG_(args_for_valgrind).used; i++) {
         VG_(message)(Vg_DebugMsg, "   %s", VG_(args_for_valgrind).strs[i]);
      }

      VG_(message)(Vg_DebugMsg, "Contents of /proc/version:");
      fd = VG_(open) ( "/proc/version", VKI_O_RDONLY, 0 );
      if (fd.isError) {
         VG_(message)(Vg_DebugMsg, "  can't open /proc/version");
      } else {
#        define BUF_LEN    256
         Char version_buf[BUF_LEN];
         Int n = VG_(read) ( fd.val, version_buf, BUF_LEN );
         vg_assert(n <= BUF_LEN);
         if (n > 0) {
            version_buf[n-1] = '\0';
            VG_(message)(Vg_DebugMsg, "  %s", version_buf);
         } else {
            VG_(message)(Vg_DebugMsg, "  (empty?)");
         }
         VG_(close)(fd.val);
#        undef BUF_LEN
      }
   }
}


/*====================================================================*/
/*=== File descriptor setup                                        ===*/
/*====================================================================*/

/* Number of file descriptors that Valgrind tries to reserve for
   it's own use - just a small constant. */
#define N_RESERVED_FDS (10)

static void setup_file_descriptors(void)
{
   struct vki_rlimit rl;

   /* Get the current file descriptor limits. */
   if (VG_(getrlimit)(VKI_RLIMIT_NOFILE, &rl) < 0) {
      rl.rlim_cur = 1024;
      rl.rlim_max = 1024;
   }

   /* Work out where to move the soft limit to. */
   if (rl.rlim_cur + N_RESERVED_FDS <= rl.rlim_max) {
      rl.rlim_cur = rl.rlim_cur + N_RESERVED_FDS;
   } else {
      rl.rlim_cur = rl.rlim_max;
   }

   /* Reserve some file descriptors for our use. */
   VG_(fd_soft_limit) = rl.rlim_cur - N_RESERVED_FDS;
   VG_(fd_hard_limit) = rl.rlim_cur - N_RESERVED_FDS;

   /* Update the soft limit. */
   VG_(setrlimit)(VKI_RLIMIT_NOFILE, &rl);

   if (VG_(cl_exec_fd) != -1)
      VG_(cl_exec_fd) = VG_(safe_fd)( VG_(cl_exec_fd) );
}


/*====================================================================*/
/*===  Initialise the first thread.                                ===*/
/*====================================================================*/

/* Given a pointer to the ThreadArchState for thread 1 (the root
   thread), initialise the VEX guest state, and copy in essential
   starting values.
*/
static void init_thread1state ( Addr client_ip, 
                                Addr client_sp,
                                /*inout*/ ThreadArchState* arch )
{
#if defined(VGA_x86)
   vg_assert(0 == sizeof(VexGuestX86State) % 8);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestX86_initialise(&arch->vex);

   /* Zero out the shadow area. */
   VG_(memset)(&arch->vex_shadow, 0, sizeof(VexGuestX86State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_ESP = client_sp;
   arch->vex.guest_EIP = client_ip;

   /* initialise %cs, %ds and %ss to point at the operating systems
      default code, data and stack segments */
   asm volatile("movw %%cs, %0" : : "m" (arch->vex.guest_CS));
   asm volatile("movw %%ds, %0" : : "m" (arch->vex.guest_DS));
   asm volatile("movw %%ss, %0" : : "m" (arch->vex.guest_SS));

#elif defined(VGA_amd64)
   vg_assert(0 == sizeof(VexGuestAMD64State) % 8);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestAMD64_initialise(&arch->vex);

   /* Zero out the shadow area. */
   VG_(memset)(&arch->vex_shadow, 0, sizeof(VexGuestAMD64State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_RSP = client_sp;
   arch->vex.guest_RIP = client_ip;

#elif defined(VGA_ppc32)
   vg_assert(0 == sizeof(VexGuestPPC32State) % 8);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestPPC32_initialise(&arch->vex);

   /* Zero out the shadow area. */
   VG_(memset)(&arch->vex_shadow, 0, sizeof(VexGuestPPC32State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_GPR1 = client_sp;
   arch->vex.guest_CIA  = client_ip;

#else
#  error Unknown arch
#endif
   // Tell the tool that we just wrote to the registers.
   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));
}


/*====================================================================*/
/*=== BB profiling                                                 ===*/
/*====================================================================*/

static 
void show_BB_profile ( BBProfEntry tops[], UInt n_tops, ULong score_total )
{
   ULong score_cumul,   score_here;
   Char  buf_cumul[10], buf_here[10];
   Char  name[64];
   Int   r;

   VG_(printf)("\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("--- BEGIN BB Profile (summary of scores)                ---\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("\n");

   VG_(printf)("Total score = %lld\n\n", score_total);

   score_cumul = 0;
   for (r = 0; r < n_tops; r++) {
      if (tops[r].addr == 0)
         continue;
      name[0] = 0;
      VG_(get_fnname_w_offset)(tops[r].addr, name, 64);
      name[63] = 0;
      score_here = tops[r].score;
      score_cumul += score_here;
      VG_(percentify)(score_cumul, score_total, 2, 6, buf_cumul);
      VG_(percentify)(score_here,  score_total, 2, 6, buf_here);
      VG_(printf)("%3d: (%9lld %s)   %9lld %s      0x%llx %s\n",
                  r,
                  score_cumul, buf_cumul,
                  score_here,  buf_here, tops[r].addr, name );
   }

   VG_(printf)("\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("--- BB Profile (BB details)                             ---\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("\n");

   score_cumul = 0;
   for (r = 0; r < n_tops; r++) {
      if (tops[r].addr == 0)
         continue;
      name[0] = 0;
      VG_(get_fnname_w_offset)(tops[r].addr, name, 64);
      name[63] = 0;
      score_here = tops[r].score;
      score_cumul += score_here;
      VG_(percentify)(score_cumul, score_total, 2, 6, buf_cumul);
      VG_(percentify)(score_here,  score_total, 2, 6, buf_here);
      VG_(printf)("\n");
      VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-= begin BB rank %d "
                  "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
      VG_(printf)("%3d: (%9lld %s)   %9lld %s      0x%llx %s\n",
                  r,
                  score_cumul, buf_cumul,
                  score_here,  buf_here, tops[r].addr, name );
      VG_(printf)("\n");
      VG_(translate)(0, tops[r].addr, True, VG_(clo_profile_flags), 0);
      VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-=  end BB rank %d  "
                  "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
   }

   VG_(printf)("\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("--- END BB Profile                                      ---\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("\n");
}


/*====================================================================*/
/*=== main()                                                       ===*/
/*====================================================================*/

/* When main() is entered, we should be on the following stack, not
   the one the kernel gave us.  We will run on this stack until
   simulation of the root thread is started, at which point a transfer
   is made to a dynamically allocated stack.  This is for the sake of
   uniform overflow detection for all Valgrind threads.  This is
   marked global even though it isn't, because assembly code below
   needs to reference the name. */

/*static*/ VgStack VG_(interim_stack);

/* This should get some address inside the stack on which we gained
   control (eg, it could be the SP at startup).  It doesn't matter
   exactly where in the stack it is.  This value is passed to the
   address space manager at startup, which uses it to identify the
   initial stack segment and hence the upper end of the usable address
   space. */

static Addr sp_at_startup = 0;

/* --- Forwards decls to do with shutdown --- */

static void final_tidyup(ThreadId tid); 

/* Do everything which needs doing when the last thread exits */
static 
void shutdown_actions_NORETURN( ThreadId tid, 
                                VgSchedReturnCode tids_schedretcode );

/* --- end of Forwards decls to do with shutdown --- */


/* TODO: GIVE THIS A PROPER HOME
   TODO: MERGE THIS WITH DUPLICATE IN mac_leakcheck.c
   Extract from aspacem a vector of the current segment start
   addresses.  The vector is dynamically allocated and should be freed
   by the caller when done.  REQUIRES m_mallocfree to be running.
   Writes the number of addresses required into *n_acquired. */

static Addr* get_seg_starts ( /*OUT*/Int* n_acquired )
{
   Addr* starts;
   Int   n_starts, r;

   n_starts = 1;
   while (True) {
      starts = VG_(malloc)( n_starts * sizeof(Addr) );
      if (starts == NULL)
         break;
      r = VG_(am_get_segment_starts)( starts, n_starts );
      if (r >= 0)
         break;
      VG_(free)(starts);
      n_starts *= 2;
   }

   if (starts == NULL) {
     *n_acquired = 0;
     return NULL;
   }

   *n_acquired = r;
   return starts;
}



Int main(Int argc, HChar **argv, HChar **envp)
{
   HChar*  toolname          = "memcheck";    // default to Memcheck
   HChar** env               = NULL;
   Int     need_help         = 0; // 0 = no, 1 = --help, 2 = --help-debug
   Addr    initial_client_IP = 0;
   Addr    initial_client_SP = 0;
   Addr    clstack_top       = 0;
   SizeT   clstack_max_size  = 0;
   UInt*   client_auxv;
   Int     loglevel, i;
   Bool    logging_to_fd;
   struct vki_rlimit zero = { 0, 0 };
   struct exeinfo info;

   //============================================================
   //
   // Nb: startup is complex.  Prerequisites are shown at every step.
   // *** Be very careful when messing with the order ***
   //
   // The first order of business is to get debug logging, the address
   // space manager and the dynamic memory manager up and running.
   // Once that's done, we can relax a bit.
   //
   //============================================================
   
   /* This is needed to make VG_(getenv) usable early. */
   VG_(client_envp) = (Char**)envp;

   //--------------------------------------------------------------
   // Start up the logging mechanism
   //   p: none
   //--------------------------------------------------------------
   /* Start the debugging-log system ASAP.  First find out how many 
      "-d"s were specified.  This is a pre-scan of the command line. */
   loglevel = 0;
   for (i = 1; i < argc; i++) {
      if (argv[i][0] != '-')
         break;
      if (VG_STREQ(argv[i], "--")) 
         break;
      if (VG_STREQ(argv[i], "-d")) 
         loglevel++;
   }

   /* ... and start the debug logger.  Now we can safely emit logging
      messages all through startup. */
   VG_(debugLog_startup)(loglevel, "Stage 2 (main)");
   VG_(debugLog)(1, "main", "Welcome to Valgrind version " 
                            VERSION " debug logging\n");

   //--------------------------------------------------------------
   // Ensure we're on a plausible stack.
   //   p: logging
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Checking current stack is plausible\n");
   { HChar* limLo  = (HChar*)(&VG_(interim_stack).bytes[0]);
     HChar* limHi  = limLo + sizeof(VG_(interim_stack));
     HChar* aLocal = (HChar*)&zero; /* any auto local will do */
     if (aLocal < limLo || aLocal >= limHi) {
        /* something's wrong.  Stop. */
        VG_(debugLog)(0, "main", "Root stack %p to %p, a local %p\n",
                          limLo, limHi, aLocal );
        VG_(debugLog)(0, "main", "Valgrind: FATAL: "
                                 "Initial stack switched failed.\n");
        VG_(debugLog)(0, "main", "   Cannot continue.  Sorry.\n");
        VG_(exit)(1);
     }
   }

   //--------------------------------------------------------------
   // Ensure we have a plausible pointer to the stack on which
   // we gained control (not the current stack!)
   //   p: logging
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Checking initial stack was noted\n");
   if (sp_at_startup == 0) {
      VG_(debugLog)(0, "main", "Valgrind: FATAL: "
                               "Initial stack was not noted.\n");
      VG_(debugLog)(0, "main", "   Cannot continue.  Sorry.\n");
      VG_(exit)(1);
   }

   //--------------------------------------------------------------
   // Start up the address space manager, and determine the
   // approximate location of the client's stack
   //   p: logging, plausible-stack
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Starting the address space manager\n");
   clstack_top = VG_(am_startup)( sp_at_startup );
   VG_(debugLog)(1, "main", "Address space manager is running\n");

   //--------------------------------------------------------------
   // Start up the dynamic memory manager
   //   p: address space management
   //   In fact m_mallocfree is self-initialising, so there's no
   //   initialisation call to do.  Instead, try a simple malloc/
   //   free pair right now to check that nothing is broken.
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Starting the dynamic memory manager\n");
   { void* p = VG_(malloc)( 12345 );
     if (p) VG_(free)( p );
   }
   VG_(debugLog)(1, "main", "Dynamic memory manager is running\n");

   //============================================================
   //
   // Dynamic memory management is now available.
   //
   //============================================================

   //--------------------------------------------------------------
   // Look for alternative libdir                                  
   { HChar *cp = VG_(getenv)(VALGRIND_LIB);
     if (cp != NULL)
        VG_(libdir) = cp;
   }

   //--------------------------------------------------------------
   // Extract the launcher name from the environment.
   VG_(debugLog)(1, "main", "Getting stage1's name\n");
   VG_(name_of_launcher) = VG_(getenv)(VALGRIND_LAUNCHER);
   if (VG_(name_of_launcher) == NULL) {
      VG_(printf)("valgrind: You cannot run '%s' directly.\n", argv[0]);
      VG_(printf)("valgrind: You should use $prefix/bin/valgrind.\n");
      VG_(exit)(1);
   }

   //--------------------------------------------------------------
   // Get the current process datasize rlimit, and set it to zero.
   // This prevents any internal uses of brk() from having any effect.
   // We remember the old value so we can restore it on exec, so that
   // child processes will have a reasonable brk value.
   VG_(getrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));
   zero.rlim_max = VG_(client_rlimit_data).rlim_max;
   VG_(setrlimit)(VKI_RLIMIT_DATA, &zero);

   // Get the current process stack rlimit.
   VG_(getrlimit)(VKI_RLIMIT_STACK, &VG_(client_rlimit_stack));

   //============================================================
   // Command line argument handling order:
   // * If --help/--help-debug are present, show usage message 
   //   (including the tool-specific usage)
   // * (If no --tool option given, default to Memcheck)
   // * Then, if client is missing, abort with error msg
   // * Then, if any cmdline args are bad, abort with error msg
   //============================================================

   //--------------------------------------------------------------
   // Split up argv into: C args, V args, V extra args, and exename.
   //   p: dynamic memory allocation
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Split up command line\n");
   VG_(split_up_argv)( argc, argv );
   if (0) {
      for (i = 0; i < VG_(args_for_valgrind).used; i++)
         VG_(printf)("varg %s\n", VG_(args_for_valgrind).strs[i]);
      VG_(printf)(" exe %s\n", VG_(args_the_exename));
      for (i = 0; i < VG_(args_for_client).used; i++)
         VG_(printf)("carg %s\n", VG_(args_for_client).strs[i]);
   }

   //--------------------------------------------------------------
   // Extract tool name and whether help has been requested.
   // Note we can't print the help message yet, even if requested,
   // because the tool has not been initialised.
   //   p: split_up_argv [for VG_(args_for_valgrind)]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Preprocess command line opts\n");
   get_helprequest_and_toolname(&need_help, &toolname);

   // Set default vex control params
   LibVEX_default_VexControl(& VG_(clo_vex_control));

   //--------------------------------------------------------------
   // Load client executable, finding in $PATH if necessary
   //   p: get_helprequest_and_toolname()  [for 'exec', 'need_help']
   //   p: layout_remaining_space          [so there's space]
   //--------------------------------------------------------------
   if (!need_help) {
      VG_(debugLog)(1, "main", "Loading client\n");

      if (VG_(args_the_exename) == NULL)
         missing_prog();

      load_client(&info, &initial_client_IP);
   }

   //--------------------------------------------------------------
   // Set up client's environment
   //   p: set-libdir                   [for VG_(libdir)]
   //   p: get_helprequest_and_toolname [for toolname]
   //--------------------------------------------------------------
   if (!need_help) {
      VG_(debugLog)(1, "main", "Setup client env\n");
      env = setup_client_env(envp, toolname);
   }

   //--------------------------------------------------------------
   // Setup client stack, eip, and VG_(client_arg[cv])
   //   p: load_client()     [for 'info']
   //   p: fix_environment() [for 'env']
   //--------------------------------------------------------------
   if (!need_help) {
      void* init_sp = argv - 1;
      SizeT m1 = 1024 * 1024;
      SizeT m8 = 8 * m1;
      VG_(debugLog)(1, "main", "Setup client stack\n");
      clstack_max_size = (SizeT)VG_(client_rlimit_stack).rlim_cur;
      if (clstack_max_size < m1) clstack_max_size = m1;
      if (clstack_max_size > m8) clstack_max_size = m8;
      clstack_max_size = VG_PGROUNDUP(clstack_max_size);

      initial_client_SP
         = setup_client_stack( init_sp, env, 
                               &info, &client_auxv, 
                               clstack_top, clstack_max_size );

      VG_(free)(env);

      VG_(debugLog)(2, "main",
                       "Client info: "
                       "entry=%p client_SP=%p brkbase=%p\n",
                       (void*)initial_client_IP, 
                       (void*)initial_client_SP,
                       (void*)VG_(brk_base) );
   }

   //--------------------------------------------------------------
   // Setup client data (brk) segment.  Initially a 1-page segment
   // which abuts a shrinkable reservation. 
   //     p: load_client()     [for 'info' and hence VG_(brk_base)]
   //--------------------------------------------------------------
   if (!need_help) { 
      SizeT m1 = 1024 * 1024;
      SizeT m8 = 8 * m1;
      SizeT dseg_max_size = (SizeT)VG_(client_rlimit_data).rlim_cur;
      VG_(debugLog)(1, "main", "Setup client data (brk) segment\n");
      if (dseg_max_size < m1) dseg_max_size = m1;
      if (dseg_max_size > m8) dseg_max_size = m8;
      dseg_max_size = VG_PGROUNDUP(dseg_max_size);

      setup_client_dataseg( dseg_max_size );
   }

   //==============================================================
   //
   // Finished loading/setting up the client address space.
   //
   //==============================================================

   //--------------------------------------------------------------
   // setup file descriptors
   //   p: n/a
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Setup file descriptors\n");
   setup_file_descriptors();

   //--------------------------------------------------------------
   // create the fake /proc/<pid>/cmdline file and then unlink it,
   // but hold onto the fd, so we can hand it out to the client
   // when it tries to open /proc/<pid>/cmdline for itself.
   //   p: setup file descriptors
   //--------------------------------------------------------------
   if (!need_help) {
      HChar  buf[50], buf2[50+64];
      HChar  nul[1];
      Int    fd, r;
      HChar* exename;

      VG_(debugLog)(1, "main", "Create fake /proc/<pid>/cmdline\n");

      VG_(sprintf)(buf, "proc_%d_cmdline", VG_(getpid)());
      fd = VG_(mkstemp)( buf, buf2 );
      if (fd == -1)
         config_error("Can't create client cmdline file in /tmp.");

      nul[0] = 0;
      exename = VG_(args_the_exename) ? VG_(args_the_exename)
                                      : "unknown_exename";

      VG_(write)(fd, VG_(args_the_exename), 
                     VG_(strlen)( VG_(args_the_exename) ));
      VG_(write)(fd, nul, 1);

      for (i = 0; i < VG_(args_for_client).used; i++) {
         VG_(write)(fd, VG_(args_for_client).strs[i],
                        VG_(strlen)( VG_(args_for_client).strs[i] ));
         VG_(write)(fd, nul, 1);
      }

      /* Don't bother to seek the file back to the start; instead do
	 it every time a copy of it is given out (by PRE(sys_open)). 
	 That is probably more robust across fork() etc. */

      /* Now delete it, but hang on to the fd. */
      r = VG_(unlink)( buf2 );
      if (r)
         config_error("Can't delete client cmdline file in /tmp.");

      VG_(cl_cmdline_fd) = fd;
   }

   //--------------------------------------------------------------
   // Init tool part 1: pre_clo_init
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   {
      Char* s;
      Bool  ok;
      VG_(debugLog)(1, "main", "Initialise the tool part 1 (pre_clo_init)\n");
      (VG_(tool_info).tl_pre_clo_init)();
      ok = VG_(sanity_check_needs)( &s );
      if (!ok) {
         VG_(tool_panic)(s);
      }
   }

   //--------------------------------------------------------------
   // If --tool and --help/--help-debug was given, now give the core+tool
   // help message
   //   p: get_helprequest_and_toolname() [for 'need_help']
   //   p: tl_pre_clo_init                [for 'VG_(tdict).usage']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print help and quit, if requested\n");
   if (need_help) {
      usage_NORETURN(/*--help-debug?*/2 == need_help);
   }

   //--------------------------------------------------------------
   // Process command line options to Valgrind + tool
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Process Valgrind's command line options, "
                            " setup logging\n");
   logging_to_fd = process_cmd_line_options(client_auxv, toolname);

   //--------------------------------------------------------------
   // Print the preamble
   //   p: tl_pre_clo_init            [for 'VG_(details).name' and friends]
   //   p: process_cmd_line_options() [for VG_(clo_verbosity), VG_(clo_xml),
   //                                      VG_(clo_log_file_qualifier),
   //                                      logging_to_fd]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print the preamble...\n");
   print_preamble(logging_to_fd, toolname);
   VG_(debugLog)(1, "main", "...finished the preamble\n");

   //--------------------------------------------------------------
   // Init tool part 2: post_clo_init
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //   p: print_preamble()          [so any warnings printed in post_clo_init
   //                                 are shown after the preamble]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise the tool part 2 (post_clo_init)\n");
   VG_TDICT_CALL(tool_post_clo_init);

   //--------------------------------------------------------------
   // Initialise translation table and translation cache
   //   p: aspacem         [??]
   //   p: tl_pre_clo_init [for 'VG_(details).avg_translation_sizeB']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise TT/TC\n");
   VG_(init_tt_tc)();

   //--------------------------------------------------------------
   // Initialise the redirect table.
   //   p: init_tt_tc [so it can call VG_(search_transtab) safely]
   //   p: aspacem [so can change ownership of sysinfo pages]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise redirects\n");
   VG_(setup_code_redirect_table)();

   //--------------------------------------------------------------
   // Allow GDB attach
   //   p: process_cmd_line_options()  [for VG_(clo_wait_for_gdb)]
   //--------------------------------------------------------------
   /* Hook to delay things long enough so we can get the pid and
      attach GDB in another shell. */
   if (VG_(clo_wait_for_gdb)) {
      Long q, iters;
      VG_(debugLog)(1, "main", "Wait for GDB\n");
      VG_(printf)("pid=%d, entering delay loop\n", VG_(getpid)());
      /* jrs 20050206: I don't understand why this works on x86.  On
         amd64 the obvious analogues (jump *$rip or jump *$rcx) don't
         work. */
      /* do "jump *$eip" to skip this in gdb (x86) */
      //VG_(do_syscall0)(__NR_pause);

#     if defined(VGP_x86_linux)
      iters = 5;
#     elif defined(VGP_amd64_linux)
      iters = 10;
#     elif defined(VGP_ppc32_linux)
      iters = 5;
#     else
#     error "Unknown plat"
#     endif

      iters *= 1000*1000*1000;
      for (q = 0; q < iters; q++) 
         ;
   }

   //--------------------------------------------------------------
   // Search for file descriptors that are inherited from our parent
   //   p: process_cmd_line_options  [for VG_(clo_track_fds)]
   //--------------------------------------------------------------
   if (VG_(clo_track_fds)) {
      VG_(debugLog)(1, "main", "Init preopened fds\n");
      VG_(init_preopened_fds)();
   }

   //--------------------------------------------------------------
   // Load debug info for the existing segments.
   //   p: setup_code_redirect_table [so that redirs can be recorded]
   //   p: mallocfree
   //   p: probably: setup fds and process CLOs, so that logging works
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Load initial debug info\n");
   { Addr* seg_starts;
     Int   n_seg_starts;

     seg_starts = get_seg_starts( &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts > 0);

     /* show them all to the debug info reader */
     for (i = 0; i < n_seg_starts; i++)
        VG_(di_notify_mmap)( seg_starts[i] );

     VG_(free)( seg_starts );
   }

   //--------------------------------------------------------------
   // Tell aspacem of ownership change of the asm helpers, so that
   // m_translate allows them to be translated.  However, only do this
   // after the initial debug info read, since making a hole in the
   // address range for the stage2 binary confuses the debug info reader.
   //   p: aspacem
   //--------------------------------------------------------------
   { Bool change_ownership_v_c_OK;
     Addr co_start   = VG_PGROUNDDN( (Addr)&VG_(trampoline_stuff_start) );
     Addr co_endPlus = VG_PGROUNDUP( (Addr)&VG_(trampoline_stuff_end) );
     VG_(debugLog)(1,"redir",
                     "transfer ownership V -> C of 0x%llx .. 0x%llx\n",
                     (ULong)co_start, (ULong)co_endPlus-1 );

     change_ownership_v_c_OK 
        = VG_(am_change_ownership_v_to_c)( co_start, co_endPlus - co_start );
     vg_assert(change_ownership_v_c_OK);
   }

   //--------------------------------------------------------------
   // Tell the tool about the initial client memory permissions
   //   p: aspacem
   //   p: mallocfree
   //   p: setup_client_stack
   //   p: setup_client_dataseg
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Tell tool about initial permissions\n");
   { Addr*     seg_starts;
     Int       n_seg_starts;
     NSegment* seg;

     seg_starts = get_seg_starts( &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts > 0);

     /* show interesting ones to the tool */
     for (i = 0; i < n_seg_starts; i++) {
        seg = VG_(am_find_nsegment)( seg_starts[i] );
        vg_assert(seg);
        if (seg->kind == SkFileC || seg->kind == SkAnonC) {
           VG_(debugLog)(2, "main", 
                            "tell tool about %010lx-%010lx %c%c%c\n",
                             seg->start, seg->end,
                             seg->hasR ? 'r' : '-',
                             seg->hasW ? 'w' : '-',
                             seg->hasX ? 'x' : '-' );
           VG_TRACK( new_mem_startup, seg->start, seg->end+1-seg->start, 
                                      seg->hasR, seg->hasW, seg->hasX );
        }
     }

     VG_(free)( seg_starts );

     /* Also do the initial stack permissions. */
     seg = VG_(am_find_nsegment)( initial_client_SP );
     vg_assert(seg);
     vg_assert(seg->kind == SkAnonC);
     vg_assert(initial_client_SP >= seg->start);
     vg_assert(initial_client_SP <= seg->end);

     /* Stuff below the initial SP is unaddressable. */
     /* NB: shouldn't this take into account the VG_STACK_REDZONE_SZB
        bytes below SP?  */
     VG_TRACK( die_mem_stack, seg->start, initial_client_SP - seg->start );
     VG_(debugLog)(2, "main", "mark stack inaccessible %010lx-%010lx\n",
                      seg->start, initial_client_SP-1 );

     /* Also the assembly helpers. */
     VG_TRACK( new_mem_startup,
               (Addr)&VG_(trampoline_stuff_start),
               &VG_(trampoline_stuff_end) - &VG_(trampoline_stuff_start),
               False, /* readable? */
               False, /* writable? */
               True   /* executable? */ );
   }

   //--------------------------------------------------------------
   // Initialise the scheduler
   //   p: setup_file_descriptors() [else VG_(safe_fd)() breaks]
   //   p: setup_client_stack
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise scheduler\n");
   { NSegment* seg = VG_(am_find_nsegment)( initial_client_SP );
     vg_assert(seg);
     vg_assert(seg->kind == SkAnonC);
     vg_assert(initial_client_SP >= seg->start);
     vg_assert(initial_client_SP <= seg->end);
     VG_(scheduler_init)( seg->end, clstack_max_size );
   }

   //--------------------------------------------------------------
   // Initialise the pthread model
   //   p: ?
   //      load_client()          [for 'client_eip']
   //      setup_client_stack()   [for 'sp_at_startup']
   //      setup_scheduler()      [for the rest of state 1 stuff]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise thread 1's state\n");
   init_thread1state( initial_client_IP, initial_client_SP, 
                      &VG_(threads)[1].arch);

   //--------------------------------------------------------------
   // Initialise the pthread model
   //   p: ?
   //--------------------------------------------------------------
   //if (VG_(clo_model_pthreads))
   //   VG_(pthread_init)();

   //--------------------------------------------------------------
   // Initialise the signal handling subsystem
   //   p: n/a
   //--------------------------------------------------------------
   // Nb: temporarily parks the saved blocking-mask in saved_sigmask.
   VG_(debugLog)(1, "main", "Initialise signal management\n");
   VG_(sigstartup_actions)();

   //--------------------------------------------------------------
   // Perhaps we're profiling Valgrind?
   //   p: process_cmd_line_options()  [for VG_(clo_profile)]
   //   p: others?
   //
   // XXX: this seems to be broken?   It always says the tool wasn't built
   // for profiling;  vg_profile.c's functions don't seem to be overriding
   // vg_dummy_profile.c's?
   //
   // XXX: want this as early as possible.  Looking for --profile
   // in get_helprequest_and_toolname() could get it earlier.
   //--------------------------------------------------------------
   if (VG_(clo_profile))
      VG_(init_profiling)();

   VGP_PUSHCC(VgpStartup);

   //--------------------------------------------------------------
   // Read suppression file
   //   p: process_cmd_line_options()  [for VG_(clo_suppressions)]
   //--------------------------------------------------------------
   if (VG_(needs).core_errors || VG_(needs).tool_errors) {
      VG_(debugLog)(1, "main", "Load suppressions\n");
      VG_(load_suppressions)();
   }

   //--------------------------------------------------------------
   // Setup pointercheck
   //   p: layout_remaining_space() [for VG_(client_{base,end})]
   //   p: process_cmd_line_options() [for VG_(clo_pointercheck)]
   //--------------------------------------------------------------
   //if (VG_(clo_pointercheck))
   //   VG_(clo_pointercheck) =
   //      VG_(setup_pointercheck)( VG_(client_base), VG_(client_end));

   //--------------------------------------------------------------
   // register client stack
   //--------------------------------------------------------------
   VG_(clstk_id) = VG_(register_stack)(VG_(clstk_base), VG_(clstk_end));

   //--------------------------------------------------------------
   // Show the address space state so far
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "\n");
   VG_(debugLog)(1, "main", "\n");
   VG_(am_show_nsegments)(1,"Memory layout at client startup");
   VG_(debugLog)(1, "main", "\n");
   VG_(debugLog)(1, "main", "\n");

   //--------------------------------------------------------------
   // Run!
   //--------------------------------------------------------------
   VGP_POPCC(VgpStartup);

   if (VG_(clo_xml)) {
      HChar buf[50];
      VG_(ctime)(buf);
      VG_(message)(Vg_UserMsg, "<status>\n"
                               "  <state>RUNNING</state>\n"
                               "  <time>%t</time>\n"
                               "</status>", 
                               buf);
      VG_(message)(Vg_UserMsg, "");
   }

   VG_(debugLog)(1, "main", "Running thread 1\n");

   /* As a result of the following call, the last thread standing
      eventually winds up running shutdown_actions_NORETURN
      just below.  Unfortunately, simply exporting said function
      causes m_main to be part of a module cycle, which is pretty
      nonsensical.  So instead of doing that, the address of said
      function is stored in a global variable 'owned' by m_syswrap,
      and it uses that function pointer to get back here when it needs
      to. */

   /* Set continuation address. */
   VG_(address_of_m_main_shutdown_actions_NORETURN)
      = & shutdown_actions_NORETURN;

   /* Run the first thread, eventually ending up at the continuation
      address. */
   VG_(main_thread_wrapper_NORETURN)(1);

   /*NOTREACHED*/
   vg_assert(0);
}

/* Do everything which needs doing when the last thread exits. */

static 
void shutdown_actions_NORETURN( ThreadId tid, 
                                VgSchedReturnCode tids_schedretcode )
{
   VG_(debugLog)(1, "main", "entering VG_(shutdown_actions_NORETURN)\n");

   vg_assert( VG_(count_living_threads)() == 1 );
   vg_assert(VG_(is_running_thread)(tid));

   // Wait for all other threads to exit.
   VG_(reap_threads)(tid);

   VG_(clo_model_pthreads) = False;

   // Clean the client up before the final report
   final_tidyup(tid);

   // OK, done
   VG_(exit_thread)(tid);

   /* should be no threads left */
   vg_assert(VG_(count_living_threads)() == 0);

   VG_(threads)[tid].status = VgTs_Empty;
   //--------------------------------------------------------------
   // Finalisation: cleanup, messages, etc.  Order no so important, only
   // affects what order the messages come.
   //--------------------------------------------------------------
   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   if (VG_(clo_xml)) {
      HChar buf[50];
      if (VG_(needs).core_errors || VG_(needs).tool_errors) {
         VG_(show_error_counts_as_XML)();
         VG_(message)(Vg_UserMsg, "");
      }
      VG_(ctime)(buf);
      VG_(message)(Vg_UserMsg, "<status>\n"
                               "  <state>FINISHED</state>\n"
                               "  <time>%t</time>\n"
                               "</status>", 
                               buf);
      VG_(message)(Vg_UserMsg, "");
   }

   /* Print out file descriptor summary and stats. */
   if (VG_(clo_track_fds))
      VG_(show_open_fds)();

   if (VG_(needs).core_errors || VG_(needs).tool_errors)
      VG_(show_all_errors)();

   VG_TDICT_CALL(tool_fini, 0/*exitcode*/);

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "</valgrindoutput>");
      VG_(message)(Vg_UserMsg, "");
   }

   VG_(sanity_check_general)( True /*include expensive checks*/ );

   if (VG_(clo_verbosity) > 1)
      print_all_stats();

   if (VG_(clo_profile))
      VG_(done_profiling)();

   if (VG_(clo_profile_flags) > 0) {
      #define N_MAX 100
      BBProfEntry tops[N_MAX];
      ULong score_total = VG_(get_BB_profile) (tops, N_MAX);
      show_BB_profile(tops, N_MAX, score_total);
   }

   /* Print Vex storage stats */
   if (0)
       LibVEX_ShowAllocStats();

   /* Ok, finally exit in the os-specific way, according to the scheduler's
      return code.  In short, if the (last) thread exited by calling
      sys_exit, do likewise; if the (last) thread stopped due to a fatal
      signal, terminate the entire system with that same fatal signal. */
   VG_(debugLog)(1, "core_os", 
                    "VG_(terminate_NORETURN)(tid=%lld)\n", (ULong)tid);

   vg_assert(VG_(count_living_threads)() == 0);

   switch (tids_schedretcode) {
   case VgSrc_ExitSyscall: /* the normal way out */
      VG_(exit)( VG_(threads)[tid].os_state.exitcode );
      /* NOT ALIVE HERE! */
      VG_(core_panic)("entered the afterlife in main() -- ExitSyscall");
      break; /* what the hell :) */

   case VgSrc_FatalSig:
      /* We were killed by a fatal signal, so replicate the effect */
      vg_assert(VG_(threads)[tid].os_state.fatalsig != 0);
      VG_(kill_self)(VG_(threads)[tid].os_state.fatalsig);
      VG_(core_panic)("main(): signal was supposed to be fatal");
      break;

   default:
      VG_(core_panic)("main(): unexpected scheduler return code");
   }
}

/* -------------------- */

/* Final clean-up before terminating the process.  
   Clean up the client by calling __libc_freeres() (if requested) 
   This is Linux-specific?
*/
static void final_tidyup(ThreadId tid)
{
   Addr __libc_freeres_wrapper;

   vg_assert(VG_(is_running_thread)(tid));
   
   if ( !VG_(needs).libc_freeres ||
        !VG_(clo_run_libc_freeres) ||
        0 == (__libc_freeres_wrapper = VG_(get_libc_freeres_wrapper)()) )
      return;			/* can't/won't do it */

   if (VG_(clo_verbosity) > 2  ||
       VG_(clo_trace_syscalls) ||
       VG_(clo_trace_sched))
      VG_(message)(Vg_DebugMsg, 
		   "Caught __NR_exit; running __libc_freeres()");
      
   /* point thread context to point to libc_freeres_wrapper */
   VG_(set_IP)(tid, __libc_freeres_wrapper);
   // XXX should we use a special stack?

   /* Block all blockable signals by copying the real block state into
      the thread's block state*/
   VG_(sigprocmask)(VKI_SIG_BLOCK, NULL, &VG_(threads)[tid].sig_mask);
   VG_(threads)[tid].tmp_sig_mask = VG_(threads)[tid].sig_mask;

   /* and restore handlers to default */
   VG_(set_default_handler)(VKI_SIGSEGV);
   VG_(set_default_handler)(VKI_SIGBUS);
   VG_(set_default_handler)(VKI_SIGILL);
   VG_(set_default_handler)(VKI_SIGFPE);

   // We were exiting, so assert that...
   vg_assert(VG_(is_exiting)(tid));
   // ...but now we're not again
   VG_(threads)[tid].exitreason = VgSrc_None;

   // run until client thread exits - ideally with LIBC_FREERES_DONE,
   // but exit/exitgroup/signal will do
   VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));
}


/*====================================================================*/
/*=== Getting to main() alive                                      ===*/
/*====================================================================*/

/* If linking of the final executables is done with glibc present,
   then Valgrind starts at main() above as usual, and all of the
   following code is irrelevant.

   However, this is not the intended mode of use.  The plan is to
   avoid linking against glibc, by giving gcc the flags 
   -nodefaultlibs -lgcc -nostartfiles at startup.

   From this derive two requirements:

   1. gcc may emit calls to memcpy and memset to deal with structure
      assignments etc.  Since we have chosen to ignore all the
      "normal" supporting libraries, we have to provide our own
      implementations of them.  No problem.

   2. We have to provide a symbol "_start", to which the kernel
      hands control at startup.  Hence the code below.
*/

/* ---------------- Requirement 1 ---------------- */

void* memcpy(void *dest, const void *src, size_t n);
void* memcpy(void *dest, const void *src, size_t n) {
   return VG_(memcpy)(dest,src,n);
}
void* memset(void *s, int c, size_t n);
void* memset(void *s, int c, size_t n) {
  return VG_(memset)(s,c,n);
}

/* ---------------- Requirement 2 ---------------- */

/* Glibc's sysdeps/i386/elf/start.S has the following gem of a
   comment, which explains how the stack looks right at process start
   (when _start is jumped to).  Hence _start passes %esp to
   _start_in_C, which extracts argc/argv/envp and starts up
   correctly. */

/* This is the canonical entry point, usually the first thing in the text
   segment.  The SVR4/i386 ABI (pages 3-31, 3-32) says that when the entry
   point runs, most registers' values are unspecified, except for:

   %edx         Contains a function pointer to be registered with `atexit'.
                This is how the dynamic linker arranges to have DT_FINI
                functions called for shared libraries that have been loaded
                before this code runs.

   %esp         The stack contains the arguments and environment:
                0(%esp)                 argc
                4(%esp)                 argv[0]
                ...
                (4*argc)(%esp)          NULL
                (4*(argc+1))(%esp)      envp[0]
                ...
                                        NULL
*/

/* The kernel hands control to _start, which extracts the initial
   stack pointer and calls onwards to _start_in_C.  This also switches the new stack.  */
#if defined(VGP_x86_linux)
asm("\n"
    "\t.globl _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    /* set up the new stack in %eax */
    "\tmovl  $vgPlain_interim_stack, %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_ACTIVE_SZB)", %eax\n"
    "\tsubl  $16, %eax\n"
    "\tandl  $~15, %eax\n"
    /* install it, and collect the original one */
    "\txchgl %eax, %esp\n"
    /* call _start_in_C, passing it the startup %esp */
    "\tpushl %eax\n"
    "\tcall  _start_in_C\n"
    "\thlt\n"
);
#elif defined(VGP_amd64_linux)
asm("\n"
    "\t.globl _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    /* set up the new stack in %rdi */
    "\tmovq  $vgPlain_interim_stack, %rdi\n"
    "\taddq  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %rdi\n"
    "\taddq  $"VG_STRINGIFY(VG_STACK_ACTIVE_SZB)", %rdi\n"
    "\tandq  $~15, %rdi\n"
    /* install it, and collect the original one */
    "\txchgq %rdi, %rsp\n"
    /* call _start_in_C, passing it the startup %rsp */
    "\tcall  _start_in_C\n"
    "\thlt\n"
);
#else
#error "_start: needs implementation on this platform"
#endif

/* Avoid compiler warnings: this fn _is_ used, but labelling it
   'static' causes gcc to complain it isn't. */
void _start_in_C ( UWord* pArgc );
void _start_in_C ( UWord* pArgc )
{
   Int     r;
   Word    argc = pArgc[0];
   HChar** argv = (HChar**)&pArgc[1];
   HChar** envp = (HChar**)&pArgc[1+argc+1];
   sp_at_startup = (Addr)pArgc;
   r = main( (Int)argc, argv, envp );
   VG_(exit)(r);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
