
/*--------------------------------------------------------------------*/
/*--- Startup: create initial process image on Darwin              ---*/
/*---                                             initimg-darwin.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward
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

#if defined(VGO_darwin)

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
#include "pub_core_tooliface.h"       /* VG_TRACK */
#include "pub_core_threadstate.h"     /* ThreadArchState */
#include "priv_initimg_pathscan.h"
#include "pub_core_initimg.h"         /* self */


/*====================================================================*/
/*=== Loading the client                                           ===*/
/*====================================================================*/

/* Load the client whose name is VG_(argv_the_exename). */

static void load_client ( /*OUT*/ExeInfo* info, 
                          /*OUT*/Addr*    client_ip)
{
   HChar* exe_name;
   Int    ret;
   SysRes res;

   vg_assert( VG_(args_the_exename) != NULL);
   exe_name = ML_(find_executable)( VG_(args_the_exename) );

   if (!exe_name) {
      VG_(printf)("valgrind: %s: command not found\n", VG_(args_the_exename));
      VG_(exit)(127);      // 127 is Posix NOTFOUND
   }

   VG_(memset)(info, 0, sizeof(*info));
   ret = VG_(do_exec)(exe_name, info);

   // The client was successfully loaded!  Continue.

   /* Get hold of a file descriptor which refers to the client
      executable.  This is needed for attaching to GDB. */
   res = VG_(open)(exe_name, VKI_O_RDONLY, VKI_S_IRUSR);
   if (!sr_isError(res))
      VG_(cl_exec_fd) = sr_Res(res);

   /* Copy necessary bits of 'info' that were filled in */
   *client_ip  = info->init_ip;
}


/*====================================================================*/
/*=== Setting up the client's environment                          ===*/
/*====================================================================*/

/* Prepare the client's environment.  This is basically a copy of our
   environment, except:

     DYLD_INSERT_LIBRARIES=$VALGRIND_LIB/vgpreload_core-PLATFORM.so:
                ($VALGRIND_LIB/vgpreload_TOOL-PLATFORM.so:)?
                DYLD_INSERT_LIBRARIES

   If this is missing, then it is added.

   Also, remove any binding for VALGRIND_LAUNCHER=.  The client should
   not be able to see this.

   Also, add DYLD_SHARED_REGION=avoid, because V doesn't know how 
   to process the dyld shared cache file.

   Also, change VYLD_* (mangled by launcher) back to DYLD_*.

   If this needs to handle any more variables it should be hacked
   into something table driven.  The copy is VG_(malloc)'d space.
*/
static HChar** setup_client_env ( HChar** origenv, const HChar* toolname)
{
   HChar* preload_core    = "vgpreload_core";
   HChar* ld_preload      = "DYLD_INSERT_LIBRARIES=";
   HChar* dyld_cache      = "DYLD_SHARED_REGION=";
   HChar* dyld_cache_value= "avoid";
   HChar* v_launcher      = VALGRIND_LAUNCHER "=";
   Int    ld_preload_len  = VG_(strlen)( ld_preload );
   Int    dyld_cache_len  = VG_(strlen)( dyld_cache );
   Int    v_launcher_len  = VG_(strlen)( v_launcher );
   Bool   ld_preload_done = False;
   Bool   dyld_cache_done = False;
   Int    vglib_len       = VG_(strlen)(VG_(libdir));

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
   HChar* preload_string     = VG_(malloc)("initimg-darwin.sce.1", preload_string_len);
   vg_assert(preload_string);

   /* Determine if there's a vgpreload_<tool>_<platform>.so file, and setup
      preload_string. */
   preload_tool_path = VG_(malloc)("initimg-darwin.sce.2", preload_tool_path_len);
   vg_assert(preload_tool_path);
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
   envc = 0;
   for (cpp = origenv; cpp && *cpp; cpp++)
      envc++;

   /* Allocate a new space */
   ret = VG_(malloc) ("initimg-darwin.sce.3", 
                      sizeof(HChar *) * (envc+2+1)); /* 2 new entries + NULL */
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
         HChar *cp = VG_(malloc)("initimg-darwin.sce.4", len);
         vg_assert(cp);

         VG_(snprintf)(cp, len, "%s%s:%s",
                       ld_preload, preload_string, (*cpp)+ld_preload_len);

         *cpp = cp;

         ld_preload_done = True;
      }
      if (VG_(memcmp)(*cpp, dyld_cache, dyld_cache_len) == 0) {
         Int len = dyld_cache_len + VG_(strlen)(dyld_cache_value) + 1;
         HChar *cp = VG_(malloc)("initimg-darwin.sce.4.2", len);
         vg_assert(cp);

         VG_(snprintf)(cp, len, "%s%s", dyld_cache, dyld_cache_value);

         *cpp = cp;

         ld_preload_done = True;
      }
   }

   /* Add the missing bits */
   if (!ld_preload_done) {
      Int len = ld_preload_len + preload_string_len;
      HChar *cp = VG_(malloc) ("initimg-darwin.sce.5", len);
      vg_assert(cp);

      VG_(snprintf)(cp, len, "%s%s", ld_preload, preload_string);

      ret[envc++] = cp;
   }
   if (!dyld_cache_done) {
      Int len = dyld_cache_len + VG_(strlen)(dyld_cache_value) + 1;
      HChar *cp = VG_(malloc) ("initimg-darwin.sce.5.2", len);
      vg_assert(cp);

      VG_(snprintf)(cp, len, "%s%s", dyld_cache, dyld_cache_value);

      ret[envc++] = cp;
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

   /* Change VYLD_ to DYLD */
   for (i = 0; i < envc; i++) {
      if (0 == VG_(strncmp)(ret[i], "VYLD_", 5)) {
         ret[i][0] = 'D';
      }
   }


   VG_(free)(preload_string);
   ret[envc] = NULL;
   return ret;
}


/*====================================================================*/
/*=== Setting up the client's stack                                ===*/
/*====================================================================*/

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

   The format of the stack on Darwin is:

   higher address +-----------------+ <- clstack_end
                  |                 |
                  : string table    :
                  |                 |
                  +-----------------+
                  | NULL            |
                  +-----------------+
                  | executable_path | (first arg to execve())
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
                  +-----------------+
                  | mach_header *   | (dynamic only)
   lower address  +-----------------+ <- sp
                  | undefined       |
                  :                 :

   Allocate and create the initial client stack.  It is allocated down
   from clstack_end, which was previously determined by the address
   space manager.  The returned value is the SP value for the client.

   ---------------------------------------------------------------- */

static 
Addr setup_client_stack( void*  init_sp,
                         char** orig_envp, 
                         const ExeInfo* info,
                         Addr   clstack_end,
                         SizeT  clstack_max_size )
{
   char **cpp;
   char *strtab;		/* string table */
   char *stringbase;
   Addr *ptr;
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
   vg_assert( VG_(args_for_client) );

   /* ==================== compute sizes ==================== */

   /* first of all, work out how big the client stack will be */
   stringsize   = 0;
   auxsize = 0;
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

   /* Darwin executable_path + NULL */
   auxsize += 2 * sizeof(Word);
   if (info->executable_path) {
       stringsize += 1 + VG_(strlen)(info->executable_path);
   }

   /* Darwin mach_header */
   if (info->dynamic) auxsize += sizeof(Word);

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
   client_SP = VG_ROUNDDN(client_SP, 32); /* make stack 32 byte aligned */

   /* base of the string table (aligned) */
   stringbase = strtab = (char *)clstack_end 
                         - VG_ROUNDUP(stringsize, sizeof(int));

   /* The max stack size */
   clstack_max_size = VG_PGROUNDUP(clstack_max_size);

   /* Darwin stack is chosen by the ume loader */
   clstack_start = clstack_end - clstack_max_size;

   /* Record stack extent -- needed for stack-change code. */
   /* GrP fixme really? */
   VG_(clstk_base) = clstack_start;
   VG_(clstk_end)  = clstack_end;

   if (0)
      VG_(printf)("stringsize=%d auxsize=%d stacksize=%d maxsize=0x%x\n"
                  "clstack_start %p\n"
                  "clstack_end   %p\n",
	          stringsize, auxsize, stacksize, (Int)clstack_max_size,
                  (void*)clstack_start, (void*)clstack_end);

   /* ==================== allocate space ==================== */

   /* Stack was allocated by the ume loader. */

   /* ==================== create client stack ==================== */

   ptr = (Addr*)client_SP;

   /* --- mach_header --- */
   if (info->dynamic) *ptr++ = info->text;

   /* --- client argc --- */
   *ptr++ = (Addr)(argc + (have_exename ? 1 : 0));

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

   for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
      *ptr++ = (Addr)copy_str(
                       &strtab, 
                       * (HChar**) VG_(indexXA)( VG_(args_for_client), i )
                     );
   }
   *ptr++ = 0;

   /* --- envp --- */
   VG_(client_envp) = (Char **)ptr;
   for (cpp = orig_envp; cpp && *cpp; ptr++, cpp++)
      *ptr = (Addr)copy_str(&strtab, *cpp);
   *ptr++ = 0;

   /* --- executable_path + NULL --- */
   if (info->executable_path) 
       *ptr++ = (Addr)copy_str(&strtab, info->executable_path);
   else 
       *ptr++ = 0;
   *ptr++ = 0;

   vg_assert((strtab-stringbase) == stringsize);

   /* client_SP is pointing at client's argc/argv */

   if (0) VG_(printf)("startup SP = %#lx\n", client_SP);
   return client_SP;
}


/*====================================================================*/
/*=== Record system memory regions                                 ===*/
/*====================================================================*/

static void record_system_memory(void)
{
   /* Tell aspacem where the client's kernel commpage is */
#if defined(VGA_amd64)
   /* commpage 0x7fff:ffe00000+ - not in vm_region */
   // GrP fixme check again
   VG_(am_notify_client_mmap)(0x7fffffe00000, 0x7ffffffff000-0x7fffffe00000,
                              VKI_PROT_READ|VKI_PROT_EXEC, 0, -1, 0);

#elif defined(VGA_x86)
   /* commpage 0xfffec000+ - not in vm_region */
   // GrP fixme check again
   VG_(am_notify_client_mmap)(0xfffec000, 0xfffff000-0xfffec000,
                              VKI_PROT_READ|VKI_PROT_EXEC, 0, -1, 0);

#else
#  error unknown architecture
#endif  
}


/*====================================================================*/
/*=== TOP-LEVEL: VG_(ii_create_image)                              ===*/
/*====================================================================*/

/* Create the client's initial memory image. */
IIFinaliseImageInfo VG_(ii_create_image)( IICreateImageInfo iicii )
{
   ExeInfo info;
   HChar** env = NULL;

   IIFinaliseImageInfo iifii;
   VG_(memset)( &iifii, 0, sizeof(iifii) );

   //--------------------------------------------------------------
   // Load client executable, finding in $PATH if necessary
   //   p: get_helprequest_and_toolname()  [for 'exec', 'need_help']
   //   p: layout_remaining_space          [so there's space]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "initimg", "Loading client\n");

   if (VG_(args_the_exename) == NULL)
      VG_(err_missing_prog)();

   load_client(&info, &iifii.initial_client_IP);

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
   iicii.clstack_top = info.stack_end - 1;
   iifii.clstack_max_size = info.stack_end - info.stack_start;
   
   iifii.initial_client_SP = 
       setup_client_stack( iicii.argv - 1, env, &info, 
                           iicii.clstack_top, iifii.clstack_max_size );

   VG_(free)(env);

   VG_(debugLog)(2, "initimg",
                 "Client info: "
                 "initial_IP=%p initial_SP=%p stack=%p..%p\n", 
                 (void*)(iifii.initial_client_IP),
                 (void*)(iifii.initial_client_SP),
                 (void*)(info.stack_start), 
                 (void*)(info.stack_end));


   // Tell aspacem about commpage, etc
   record_system_memory();

   return iifii;
}


/*====================================================================*/
/*=== TOP-LEVEL: VG_(ii_finalise_image)                            ===*/
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

   /* GrP fixme doesn't handle all registers from LC_THREAD or LC_UNIXTHREAD */

#  if defined(VGP_x86_darwin)
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

#  elif defined(VGP_amd64_darwin)
   vg_assert(0 == sizeof(VexGuestAMD64State) % 16);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestAMD64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestAMD64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestAMD64State));

   /* Put essential stuff into the new state. */
   arch->vex.guest_RSP = iifii.initial_client_SP;
   arch->vex.guest_RIP = iifii.initial_client_IP;

#  else
#    error Unknown platform
#  endif

   /* Tell the tool that we just wrote to the registers. */
   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));
}

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
