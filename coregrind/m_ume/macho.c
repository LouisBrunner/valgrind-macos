
/*--------------------------------------------------------------------*/
/*--- User-mode execve() for Mach-O executables      m_ume_macho.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2017 Apple Inc.
      Greg Parker  gparker@apple.com

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

#if defined(VGO_darwin)

#include "pub_core_basics.h"
#include "pub_core_vki.h"

#include "pub_core_aspacemgr.h"     // various mapping fns
#include "pub_core_debuglog.h"
#include "pub_core_libcassert.h"    // VG_(exit), vg_assert
#include "pub_core_libcbase.h"      // VG_(memcmp), etc
#include "pub_core_libcfile.h"      // VG_(open) et al
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_machine.h"       // VG_ELF_CLASS (XXX: which should be moved)
#include "pub_core_mallocfree.h"    // VG_(malloc), VG_(free)
#include "pub_core_syscall.h"       // VG_(strerror)
#include "pub_core_ume.h"           // self

#include "priv_ume.h"

#include <mach/mach.h>

#include <mach-o/dyld.h>
#include <mach-o/fat.h>
#include <mach-o/loader.h>

#if VG_WORDSIZE == 4
#define MAGIC MH_MAGIC
#define MACH_HEADER mach_header
#define LC_SEGMENT_CMD LC_SEGMENT
#define SEGMENT_COMMAND segment_command
#define SECTION section
#else
#define MAGIC MH_MAGIC_64
#define MACH_HEADER mach_header_64
#define LC_SEGMENT_CMD LC_SEGMENT_64
#define SEGMENT_COMMAND segment_command_64
#define SECTION section_64
#endif

typedef struct load_info_t {
  vki_uint8_t *stack_start; // allocated thread stack (hot end)
  vki_uint8_t *stack_end; // allocated thread stack (cold end)
  vki_uint8_t *text; // start of text segment (i.e. the mach headers)
  vki_uint8_t *entry; // static entry point
  vki_uint8_t *linker_entry; // dylinker entry point
  Addr linker_offset; // dylinker text offset
  vki_size_t max_addr; // biggest address reached while loading segments
} load_info_t;

static void print(const HChar *str)
{
   VG_(printf)("%s", str);
}

static void check_mmap(SysRes res, Addr base, SizeT len, const HChar* who)
{
   if (sr_isError(res)) {
      VG_(printf)("valgrind: mmap-FIXED(0x%llx, %lld) failed in UME (%s) "
                  "with error %lu (%s).\n",
                  (ULong)base, (Long)len, who,
                  sr_Err(res), VG_(strerror)(sr_Err(res)) );
      VG_(exit)(1);
   }
}

#if DARWIN_VERS >= DARWIN_10_8
static void check_mmap_float(SysRes res, SizeT len, const HChar* who)
{
   if (sr_isError(res)) {
      VG_(printf)("valgrind: mmap-FLOAT(size=%lld) failed in UME (%s) "
                  "with error %lu (%s).\n",
                  (Long)len, who,
                  sr_Err(res), VG_(strerror)(sr_Err(res)) );
      VG_(exit)(1);
   }
}
#endif

static int 
load_thin_file(int fd, vki_off_t offset, vki_off_t size, unsigned long filetype, 
               const HChar *filename, load_info_t *out_info);

static int 
load_fat_file(int fd, vki_off_t offset, vki_off_t size, unsigned long filetype, 
              const HChar *filename, load_info_t *out_info);

static int 
load_mach_file(int fd, vki_off_t offset, vki_off_t size, unsigned long filetype, 
               const HChar *filename, load_info_t *out_info);


/* Open and map a dylinker file.
   Returns 0 on success, -1 on any failure.
   filename must be an absolute path.
   The dylinker's entry point is returned in out_info->linker_entry.
 */
static int 
open_dylinker(const HChar *filename, load_info_t *out_info)
{
   struct vg_stat sb;
   vki_size_t filesize;
   SysRes res;
   int fd;
   int err;

   if (filename[0] != '/') {
      print("bad executable (dylinker name is not an absolute path)\n");
      return -1;
   }

   res = VG_(open)(filename, VKI_O_RDONLY, 0);
   fd = sr_Res(res);
   if (sr_isError(res)) {
      VG_(printf)("couldn't open dylinker: %s\n", filename);
      return -1;
   }
   err = VG_(fstat)(fd, &sb);
   if (err) {
      VG_(printf)("couldn't stat dylinker: %s\n", filename);
      VG_(close)(fd);
      return -1;
   }
   filesize = sb.size;

   err = load_mach_file(fd, 0, filesize, MH_DYLINKER, filename, out_info);
   if (err) {
      VG_(printf)("...while loading dylinker: %s\n", filename);
   }
   VG_(close)(fd);
   return err;
}


/* 
   Process an LC_SEGMENT command, mapping it into memory if appropriate.
   fd[offset..size) is a Mach-O thin file. 
   Returns 0 on success, -1 on any failure.
   If this segment contains the executable's Mach headers, their 
     loaded address is returned in out_info->text.
   If this segment is a __UNIXSTACK, its start address is returned in 
     out_info->stack_start.
*/
static int
load_segment(int fd, vki_off_t offset, vki_off_t size, 
             struct SEGMENT_COMMAND *segcmd, const HChar *filename,
             load_info_t *out_info)
{
   SysRes res;
   Addr addr;
   vki_size_t filesize; // page-aligned 
   vki_size_t vmsize;   // page-aligned
   vki_size_t vmend;    // page-aligned
   unsigned int prot;
   Addr slided_addr = segcmd->vmaddr + out_info->linker_offset;

   // GrP fixme mark __UNIXSTACK as SF_STACK
    
   // Don't honour the client's request to map PAGEZERO.  Why not?
   // Because when the kernel loaded the valgrind tool executable,
   // it will have mapped pagezero itself.  So further attempts
   // to map it when loading the client are guaranteed to fail.
#if VG_WORDSIZE == 4
   if (segcmd->vmaddr == 0 && 0 == VG_(strcmp)(segcmd->segname, SEG_PAGEZERO)) {
      if (segcmd->vmsize != 0x1000) {
         print("bad executable (__PAGEZERO is not 4 KB)\n");
         return -1;
      }
      return 0;
   }
#endif
#if VG_WORDSIZE == 8
   if (segcmd->vmaddr == 0 && 0 == VG_(strcmp)(segcmd->segname, SEG_PAGEZERO)) {
      if (segcmd->vmsize != 0x100000000) {
         print("bad executable (__PAGEZERO is not 4 GB)\n");
         return -1;
      }
      return 0;
   }
#endif

   // Record the segment containing the Mach headers themselves
   if (segcmd->fileoff == 0  &&  segcmd->filesize != 0) {
      out_info->text = (vki_uint8_t *)slided_addr;
   }

   // Record the __UNIXSTACK start
   if (0 == VG_(strcmp)(segcmd->segname, SEG_UNIXSTACK)) {
      out_info->stack_start = (vki_uint8_t *)slided_addr;
   }

   // Sanity-check the segment
   if (segcmd->fileoff + segcmd->filesize > size) {
      print("bad executable (invalid segment command)\n");
      return -1;
   }

   vmend = VG_PGROUNDUP(slided_addr + segcmd->vmsize);
   if (vmend > out_info->max_addr) {
      out_info->max_addr = vmend;
   }

   if (segcmd->vmsize == 0) {
      return 0;  // nothing to map - ok
   }

   // Get desired memory protection
   // GrP fixme need maxprot too
   prot = (((segcmd->initprot & VM_PROT_READ) ? VKI_PROT_READ : 0) |
           ((segcmd->initprot & VM_PROT_WRITE) ? VKI_PROT_WRITE : 0) |
           ((segcmd->initprot & VM_PROT_EXECUTE) ? VKI_PROT_EXEC : 0));

   // Map the segment    
   filesize = VG_PGROUNDUP(segcmd->filesize);
   vmsize = VG_PGROUNDUP(segcmd->vmsize);
   if (filesize > 0) {
      addr = slided_addr;
      VG_(debugLog)(2, "ume", "mmap fixed (file) (%#lx, %lu)\n", addr, filesize);
      res = VG_(am_mmap_named_file_fixed_client)(addr, filesize, prot, fd, 
                                                 offset + segcmd->fileoff, 
                                                 filename);
      check_mmap(res, addr, filesize, "load_segment1");
   }

   // Zero-fill the remainder of the segment, if any
   if (segcmd->filesize != filesize) {
      // non-page-aligned part
      // GrP fixme kernel doesn't do this?
      //bzero(segcmd->filesize+(vki_uint8_t *)addr, filesize-segcmd->filesize);
   }
   if (filesize != vmsize) {
      // page-aligned part
      SizeT length = vmsize - filesize;
      addr = (Addr)(filesize + slided_addr);
      VG_(debugLog)(2, "ume", "mmap fixed (anon) (%#lx, %lu)\n", addr, length);
      res = VG_(am_mmap_anon_fixed_client)(addr, length, prot);
      check_mmap(res, addr, length, "load_segment2");
   }

   return 0;
}


/* 
   Parse a LC_THREAD or LC_UNIXTHREAD command. 
   Return 0 on success, -1 on any failure.
   If the thread is a LC_UNIXTHREAD, the stack address is returned in out_info->stack_end.
   If the executable requested a non-default stack address,
   *customstack is set to TRUE. The thread's entry point is returned in out_info->entry.
   The stack itself (if any) is not mapped.
   Other custom register settings are silently ignored (GrP fixme).
*/
static int 
load_genericthread(struct thread_command *threadcmd, int type,
                    int *customstack, load_info_t *out_info)
{
   unsigned int flavor;
   unsigned int count;
   unsigned int *p;
   unsigned int left; 

   p = (unsigned int *)(threadcmd + 1);
   left = (threadcmd->cmdsize - sizeof(struct thread_command)) / sizeof(*p);

   while (left > 0) {
      if (left < 2) {
         print("bad executable (invalid thread command)\n");
         return -1;
      }
      flavor = *p++; left--;
      count = *p++; left--;
      
      if (left < count) {
         print("bad executable (invalid thread command 2)\n");
         return -1;
      }

#if defined(VGA_x86)
      if (flavor == i386_THREAD_STATE && count == i386_THREAD_STATE_COUNT) {
         i386_thread_state_t *state = (i386_thread_state_t *)p;
         out_info->entry = (vki_uint8_t *)state->__eip;
         if (type == LC_UNIXTHREAD) {
            out_info->stack_end =
              (vki_uint8_t *)(state->__esp ? state->__esp : VKI_USRSTACK);
            vg_assert(VG_IS_PAGE_ALIGNED(out_info->stack_end));
            out_info->stack_end--;
         }
         if (customstack) *customstack = state->__esp;
         return 0;
      }

#elif defined(VGA_amd64)
      if (flavor == x86_THREAD_STATE64 && count == x86_THREAD_STATE64_COUNT){
         x86_thread_state64_t *state = (x86_thread_state64_t *)p;
         out_info->entry = (vki_uint8_t *)state->__rip;
         if (type == LC_UNIXTHREAD) {
            out_info->stack_end =
              (vki_uint8_t *)(state->__rsp ? state->__rsp : VKI_USRSTACK64);
            vg_assert(VG_IS_PAGE_ALIGNED(out_info->stack_end));
            out_info->stack_end--;
         }
         if (customstack) *customstack = state->__rsp;
         return 0;
      }

#elif defined(VGA_arm64)
      if (flavor == ARM_THREAD_STATE64 && count == ARM_THREAD_STATE64_COUNT){
         arm_thread_state64_t *state = (arm_thread_state64_t *)p;
         out_info->entry = (vki_uint8_t *)state->__pc;
         if (type == LC_UNIXTHREAD) {
            out_info->stack_end =
              (vki_uint8_t *)(state->__sp ? state->__sp : VKI_USRSTACK64);
            vg_assert(VG_IS_PAGE_ALIGNED(out_info->stack_end));
            out_info->stack_end--;
         }
         if (customstack) *customstack = state->__sp;
         return 0;
      }

#else
# error unknown platform
#endif
      p += count;
      left -= count;
   }

   print("bad executable (no arch-compatible thread state)\n");
   return -1;
}


/* Returns the main stack size on this platform, 
   using getrlimit or a fixed size.
   GrP fixme 64-bit? */
static vki_size_t default_stack_size(void)
{
   struct vki_rlimit lim;
   int err = VG_(getrlimit)(VKI_RLIMIT_STACK, &lim);
   if (err) return 8*1024*1024; // 8 MB
   else return lim.rlim_cur;
}


/* 
   Processes a LC_UNIXTHREAD command.
   Returns 0 on success, -1 on any failure.
   The stack is mapped in and returned in out_info->stack_start and out_info->stack_end.
   The thread's entry point is returned in out_info->entry.
*/
static int 
load_unixthread(struct thread_command *threadcmd, load_info_t *out_info)
{
   int err;
   int customstack;

   err = load_genericthread(threadcmd, LC_UNIXTHREAD, &customstack, out_info);
   if (err) return -1;

   if (!out_info->stack_end) {
      print("bad executable (no thread stack)\n");
      return -1;
   }

   if (!customstack) {
      // Map the stack
      vki_size_t stacksize = VG_PGROUNDUP(default_stack_size());
      vm_address_t stackbase = VG_PGROUNDDN(out_info->stack_end+1-stacksize);
      SysRes res;
        
      res = VG_(am_mmap_anon_fixed_client)(stackbase, stacksize, VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC);
      check_mmap(res, stackbase, stacksize, "load_unixthread1");
      out_info->stack_start = (vki_uint8_t *)stackbase;
   } else {
      // custom stack - mapped via __UNIXTHREAD segment
   }

   return 0;
}


/* Allocates a stack mapping at a V-chosen address.  Pertains to
   LC_MAIN commands, which seem to have appeared in OSX 10.8.

   This is a really nasty hack -- allocates 64M+stack size, then
   deallocates the 64M, to guarantee that the stack is at least 64M
   above zero. */
#if DARWIN_VERS >= DARWIN_10_8
static int
handle_lcmain ( vki_size_t requested_size,
                load_info_t *out_info )
{
   if (requested_size == 0) {
      requested_size = default_stack_size();
   }
   requested_size = VG_PGROUNDUP(requested_size);

   const vki_size_t HACK = 64 * 1024 * 1024;
   requested_size += HACK;

   SysRes res = VG_(am_mmap_anon_float_client)(requested_size,
                   VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC);
   check_mmap_float(res, requested_size, "handle_lcmain");
   vg_assert(!sr_isError(res));
   out_info->stack_start = (vki_uint8_t*)sr_Res(res);
   out_info->stack_end   = out_info->stack_start + requested_size - 1;

   Bool need_discard = False;
   res = VG_(am_munmap_client)(&need_discard, (Addr)out_info->stack_start, HACK);
   if (sr_isError(res)) return -1;
   vg_assert(!need_discard); // True == wtf?

   out_info->stack_start += HACK;

   return 0;
}
#endif /* DARWIN_VERS >= DARWIN_10_8 */



/* 
   Processes an LC_LOAD_DYLINKER command. 
   Returns 0 on success, -1 on any error.
   The linker itself is mapped into memory.
   The linker's entry point is returned in out_info->linker_entry.
*/
static int 
load_dylinker(struct dylinker_command *dycmd, load_info_t *out_info)
{
   const HChar *name;
   int ret;
   load_info_t linker_info;
   linker_info.stack_start = NULL;
   linker_info.stack_end = NULL;
   linker_info.text = NULL;
   linker_info.entry = NULL;
   linker_info.linker_entry = NULL;
   linker_info.linker_offset = 0;
   linker_info.max_addr = out_info->max_addr;

   if (dycmd->name.offset >= dycmd->cmdsize) {
      print("bad executable (invalid dylinker command)\n");
      return -1;
   }

   name = dycmd->name.offset + (HChar *)dycmd;
    
   // GrP fixme assumes name is terminated somewhere
   ret = open_dylinker(name, &linker_info);
   if (linker_info.entry) {
      out_info->linker_entry = linker_info.entry + linker_info.linker_offset;
   }
   out_info->max_addr = linker_info.max_addr;
   return ret;
}


/* 
    Process an LC_THREAD command. 
    Returns 0 on success, -1 on any failure.
    The thread's entry point is returned in out_info->entry.
*/
static int 
load_thread(struct thread_command *threadcmd, load_info_t *out_info)
{
   int customstack;
   int err;

   err = load_genericthread(threadcmd, LC_THREAD, &customstack, out_info);
   if (err) return -1;
   if (customstack) {
      print("bad executable (stackless thread has stack)\n");
      return -1;
   }
   return 0;
}


/*
  Loads a Mach-O executable into memory, along with any threads, 
  stacks, and dylinker.
  Returns 0 on success, -1 on any failure.
  fd[offset..offset+size) is a Mach-O thin file.
  filetype is MH_EXECUTE or MH_DYLINKER.
  The mapped but empty stack is returned in out_info->stack_start.
  The executable's Mach headers are returned in out_info->text.
  The executable's entry point is returned in out_info->entry.
  The dylinker's entry point (if any) is returned in out_info->linker_entry.
  The dylinker's offset (macOS 10.12) is returned in out_info->linker_offset.
  GrP fixme need to return whether dylinker was found - stack layout is different
*/
static int 
load_thin_file(int fd, vki_off_t offset, vki_off_t size, unsigned long filetype, 
               const HChar *filename, load_info_t *out_info)
{
   VG_(debugLog)(1, "ume", "load_thin_file: begin:   %s\n", filename);
   struct MACH_HEADER mh;
   vki_uint8_t *headers;
   vki_uint8_t *headers_end;
   struct load_command *lc;
   struct load_command *lcend;
   struct SEGMENT_COMMAND *segcmd;
   struct thread_command *threadcmd;
   struct dylinker_command *dycmd;
   int err;
   SysRes res;
   vki_size_t len;

   // Read Mach-O header
   if (sizeof(mh) > size) {
      print("bad executable (no Mach-O header)\n");
   }
   res = VG_(pread)(fd, &mh, sizeof(mh), offset);
   if (sr_isError(res)  ||  sr_Res(res) != sizeof(mh)) {
      print("bad executable (no Mach-O header)\n");
      return -1;
   }
   

   // Sanity-check the header itself
   if (mh.magic != MAGIC) {
      print("bad executable (no Mach-O magic)\n");
      return -1;
   }

   if (mh.filetype != filetype) {
      // expecting MH_EXECUTE or MH_DYLINKER
      print("bad executable (wrong file type)\n");
      return -1;
   }


   // Map all headers into memory
   len = sizeof(mh) + mh.sizeofcmds;
   if (len > size) {
      print("bad executable (missing load commands)\n");
      return -1;
   }

   headers = VG_(malloc)("ume.macho.headers", len);
   res = VG_(pread)(fd, headers, len, offset);
   if (sr_isError(res)) {
      print("couldn't read load commands from executable\n");
      return -1;
   }
   headers_end = headers + len;

   
   // Map some segments into client memory:
   // LC_SEGMENT    (text, data, etc)
   // UNIXSTACK     (stack)
   // LOAD_DYLINKER (dyld)
   lcend = (struct load_command *)(headers + mh.sizeofcmds + sizeof(mh));
   for (lc = (struct load_command *)(headers + sizeof(mh)); 
        lc < lcend; 
        lc = (struct load_command *)(lc->cmdsize + (vki_uint8_t *)lc))
   {
      if ((vki_uint8_t *)lc < headers  ||  
          lc->cmdsize+(vki_uint8_t *)lc > headers_end) {
          print("bad executable (invalid load commands)\n");
          return -1;
      }

      switch (lc->cmd) {

#if   DARWIN_VERS >= DARWIN_10_8
      case LC_MAIN: { /* New in 10.8 */
         struct entry_point_command* epcmd
            = (struct entry_point_command*)lc;
         if (out_info->stack_start || out_info->stack_end) {
            print("bad executable (multiple indications of stack)");
            return -1;
         }
         err = handle_lcmain(epcmd->stacksize, out_info);
         if (err) return -1;
         VG_(debugLog)(2, "ume", "lc_main: created stack %p-%p\n",
	               out_info->stack_start, out_info->stack_end);
         break;
      }
#     endif

      case LC_SEGMENT_CMD:
         if (lc->cmdsize < sizeof(struct SEGMENT_COMMAND)) {
            print("bad executable (invalid load commands)\n");
            return -1;
         }
         segcmd = (struct SEGMENT_COMMAND *)lc;
#if   DARWIN_VERS >= DARWIN_10_12
         /* dyld text address is relative instead of absolute in 10.12 */
         if (filetype == MH_DYLINKER && segcmd->vmaddr == 0 && segcmd->fileoff == 0) {
            out_info->linker_offset = out_info->max_addr;
         }
#     endif
         err = load_segment(fd, offset, size, segcmd, filename, out_info);
         if (err) return -1;
          
         break;

      case LC_UNIXTHREAD:
         if (out_info->stack_end || out_info->entry) {
            print("bad executable (multiple thread commands)\n");
            return -1;
         }
         if (lc->cmdsize < sizeof(struct thread_command)) {
            print("bad executable (invalid load commands)\n");
            return -1;
         }
         threadcmd = (struct thread_command *)lc;
         err = load_unixthread(threadcmd, out_info);
         if (err) return -1;
         break;

      case LC_LOAD_DYLINKER:
         if (filetype == MH_DYLINKER) {
            print("bad executable (dylinker needs a dylinker)\n");
            return -1;
         }
         if (out_info->linker_entry) {
            print("bad executable (multiple dylinker commands)\n");
         }
         if (lc->cmdsize < sizeof(struct dylinker_command)) {
            print("bad executable (invalid load commands)\n");
            return -1;
         }
         dycmd = (struct dylinker_command *)lc;
         err = load_dylinker(dycmd, out_info);
         if (err) return -1;
         break;

      case LC_THREAD:
         if (filetype == MH_EXECUTE) {
            print("bad executable (stackless thread)\n");
            return -1;
         }
         if (out_info->stack_end || out_info->entry) {
            print("bad executable (multiple thread commands)\n");
            return -1;
         }
         if (lc->cmdsize < sizeof(struct thread_command)) {
            print("bad executable (invalid load commands)\n");
            return -1;
         }
         threadcmd = (struct thread_command *)lc;
         err = load_thread(threadcmd, out_info);
         if (err) return -1;
         break;

      default:
         break;
      }
   }


   // Done with the headers
   VG_(free)(headers);

   if (filetype == MH_EXECUTE) {
      // Verify the necessary pieces for an executable:
      // a stack
      // a text segment
      // an entry point (static or linker)
      if (!out_info->stack_end || !out_info->stack_start) {
         VG_(printf)("bad executable %s (no stack)\n", filename);
         return -1;
      }
      if (!out_info->text) {
         print("bad executable (no text segment)\n");
         return -1;
      }
      if (!out_info->entry && !out_info->linker_entry) {
         print("bad executable (no entry point)\n");
         return -1;
      }
   }
   else if (filetype == MH_DYLINKER) {
      // Verify the necessary pieces for a dylinker:
      // an entry point
      if (!out_info->entry) {
         print("bad executable (no entry point)\n");
         return -1;
      }
   }

   VG_(debugLog)(1, "ume", "load_thin_file: success: %s\n", filename);
   return 0;
}


/*
 Load a fat Mach-O executable.
*/
static int 
load_fat_file(int fd, vki_off_t offset, vki_off_t size, unsigned long filetype, 
             const HChar *filename, load_info_t *out_info)
{
   struct fat_header fh;
   vki_off_t arch_offset;
   int i;
   cpu_type_t good_arch;
   SysRes res;

#if defined(VGA_ppc32)
   good_arch = CPU_TYPE_POWERPC;
#elif defined(VGA_ppc64be)
   good_arch = CPU_TYPE_POWERPC64BE;
#elif defined(VGA_ppc64le)
   good_arch = CPU_TYPE_POWERPC64LE;
#elif defined(VGA_x86)
   good_arch = CPU_TYPE_I386;
#elif defined(VGA_amd64)
   good_arch = CPU_TYPE_X86_64;
#elif defined(VGA_arm64)
   good_arch = CPU_TYPE_ARM64;
#else
# error unknown architecture
#endif

   // Read fat header
   // All fat contents are BIG-ENDIAN
   if (size < sizeof(fh)) {
      print("bad executable (bad fat header)\n");
      return -1;
   }
   res = VG_(pread)(fd, &fh, sizeof(fh), offset);
   if (sr_isError(res)  ||  sr_Res(res) != sizeof(fh)) {
      print("bad executable (bad fat header)\n");
      return -1;
   }
   
   // Scan arch headers looking for a good one
   arch_offset = offset + sizeof(fh);
   fh.nfat_arch = VG_(ntohl)(fh.nfat_arch);
   for (i = 0; i < fh.nfat_arch; i++) {
      struct fat_arch arch;
      if (arch_offset + sizeof(arch) > size) {
          print("bad executable (corrupt fat archs)\n");
          return -1;
      }

      res = VG_(pread)(fd, &arch, sizeof(arch), arch_offset);
      arch_offset += sizeof(arch);
      if (sr_isError(res)  ||  sr_Res(res) != sizeof(arch)) {
         VG_(printf)("bad executable (corrupt fat arch) %x %llu\n", 
                     arch.cputype, (ULong)arch_offset);
         return -1;
      }

      arch.cputype = VG_(ntohl)(arch.cputype);
      arch.cpusubtype = VG_(ntohl)(arch.cpusubtype);
      arch.offset = VG_(ntohl)(arch.offset);
      arch.size = VG_(ntohl)(arch.size);
      arch.align = VG_(ntohl)(arch.align);
      if (arch.cputype == good_arch) {
         // use this arch
         if (arch.offset > size  ||  arch.offset + arch.size > size) {
            print("bad executable (corrupt fat arch 2)\n");
            return -1;
         }
         return load_mach_file(fd, offset+arch.offset, arch.size, filetype, filename, out_info);
      }
   }

   print("bad executable (can't run on this machine)\n");
   return -1;
}

/*
 Load a Mach-O executable or dylinker.
 The file may be fat or thin.
*/
static int 
load_mach_file(int fd, vki_off_t offset, vki_off_t size, unsigned long filetype, 
              const HChar *filename, load_info_t *out_info)
{
   vki_uint32_t magic;
   SysRes res;

   if (size < sizeof(magic)) {
      print("bad executable (no Mach-O magic)\n");
      return -1;
   }
   res = VG_(pread)(fd, &magic, sizeof(magic), offset);
   if (sr_isError(res)  ||  sr_Res(res) != sizeof(magic)) {
      print("bad executable (no Mach-O magic)\n");
      return -1;
   }
   
   if (magic == MAGIC) {
      // thin
      return load_thin_file(fd, offset, size, filetype, filename, out_info);
   } else if (magic == VG_(htonl)(FAT_MAGIC)) {
      // fat
      return load_fat_file(fd, offset, size, filetype, filename, out_info);
   } else {
      // huh?
      print("bad executable (bad Mach-O magic)\n");
      return -1;
   }
}


Bool VG_(match_macho)(const void *hdr, SizeT len)
{
   const vki_uint32_t *magic = hdr;

   // GrP fixme check more carefully for matching fat arch?

   return (len >= VKI_PAGE_SIZE  &&  
           (*magic == MAGIC  ||  *magic == VG_(ntohl)(FAT_MAGIC))) 
      ? True : False;
}


Int VG_(load_macho)(Int fd, const HChar *name, ExeInfo *info)
{
   int err;
   struct vg_stat sb;
   load_info_t load_info;
   load_info.stack_start = NULL;
   load_info.stack_end = NULL;
   load_info.text = NULL;
   load_info.entry = NULL;
   load_info.linker_entry = NULL;
   load_info.linker_offset = 0;
   load_info.max_addr = 0;

   err = VG_(fstat)(fd, &sb);
   if (err) {
      print("couldn't stat executable\n");
      return VKI_ENOEXEC;
   }
   
   err = load_mach_file(fd, 0, sb.size, MH_EXECUTE, name, &load_info);
   if (err) return VKI_ENOEXEC;

   // GrP fixme exe_base
   // GrP fixme exe_end
   info->entry = (Addr) load_info.entry;
   info->init_ip = (Addr)(load_info.linker_entry ? load_info.linker_entry : load_info.entry);
   info->brkbase = 0xffffffff; // GrP fixme hack
   info->init_toc = 0; // GrP fixme unused

   info->stack_start = (Addr) load_info.stack_start;
   info->stack_end = (Addr) load_info.stack_end;
   info->text = (Addr) load_info.text;
   info->dynamic = load_info.linker_entry ? True : False;

   info->executable_path = VG_(strdup)("ume.macho.executable_path", name);

   return 0;
}

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
