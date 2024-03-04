
/*--------------------------------------------------------------------*/
/*--- DYLD Helpers                                  dyld_helpers.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2024 Louis Brunner <louis.brunner.fr@gmail.com>

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

// arm64 is the only arch on which we have access to dyld
#if defined(VGP_arm64_darwin)

#include "pub_core_debuglog.h"              // VG_(debugLog)
#include "pub_tool_libcbase.h"
#include "pub_core_debuginfo.h"             // VG_(di_notify_dsc)
#include "pub_core_mach.h"

#include <mach-o/dyld.h>
#include <mach-o/nlist.h>

#define MACH_HEADER mach_header_64
#define NLIST nlist_64
#define LC_SEGMENT_CMD LC_SEGMENT_64
#define SEGMENT_COMMAND segment_command_64

void* VG_(dyld_dlsym)( const HChar * library, const HChar * symbol) {
  Int i;
  const HChar * name;
  const struct MACH_HEADER* loaded_image = NULL;
  Addr cursor = 0;
  Addr slide = 0;
  Addr linkedit_addr = 0;

  VG_(debugLog)(2, "dyld_helpers", "dlsym: looking for %s in %s\n", symbol, library);

  for (i = 0; i < _dyld_image_count(); ++i) {
    name = _dyld_get_image_name(i);
    if (VG_(strcmp)(name, library) == 0) {
      loaded_image = (const struct MACH_HEADER*) _dyld_get_image_header(i);
      slide = _dyld_get_image_vmaddr_slide(i);
      break;
    }
  }

  if (loaded_image == NULL) {
    VG_(debugLog)(1, "dyld_helpers", "dlsym: could not find library: %s\n", library);
    return NULL;
  }
  cursor = (Addr) loaded_image;

  cursor += sizeof(struct MACH_HEADER);
  const struct load_command * cmd = (const struct load_command *) cursor;
  for (i = 0; i < loaded_image->ncmds; ++i) {
    if (cmd->cmd == LC_SEGMENT_CMD) {
      const struct SEGMENT_COMMAND * seg = (const struct SEGMENT_COMMAND *) cmd;
      VG_(debugLog)(5, "dyld_helpers", "dlsym: found segment %s in %s\n", seg->segname, library);
      if (VG_(strcmp)(seg->segname, SEG_LINKEDIT) == 0) {
        linkedit_addr = seg->vmaddr - seg->fileoff + slide;
        VG_(debugLog)(4, "dyld_helpers", "dlsym: found linkedit segment at %#lx in %s\n", linkedit_addr, library);
      }
    } else if (cmd->cmd == LC_SYMTAB) {
      const struct symtab_command * symtab = (const struct symtab_command *) cmd;

      VG_(debugLog)(4, "dyld_helpers", "dlsym: found symbol table in %s\n", library);
      if (!linkedit_addr) {
        VG_(debugLog)(1, "dyld_helpers", "dlsym: could not find linkedit segment in %s\n", library);
        return NULL;
      }

      const char * strtab = (const HChar*) (linkedit_addr + symtab->stroff);
      const struct NLIST * sym = (const struct NLIST *) (linkedit_addr + symtab->symoff);
      for (i = 0; i < symtab->nsyms; ++i) {
        if ((sym[i].n_type & N_TYPE) != N_SECT) {
          continue;
        }

        VG_(debugLog)(5, "dyld_helpers", "dlsym: found symbol %s in %s symbol table\n", strtab + sym[i].n_un.n_strx, library);
        if (VG_(strcmp)(strtab + sym[i].n_un.n_strx, symbol) != 0) {
          continue;
        }

        void* ptr = (void*) (sym[i].n_value + slide);
        VG_(debugLog)(1, "dyld_helpers", "dlsym: found symbol %s at %p in %s symbol table\n", symbol, ptr, library);
        return ptr;
      }

      VG_(debugLog)(1, "dyld_helpers", "dlsym: no symbol %s in %s symbol table\n", symbol, library);
      return NULL;
    }

    cursor += cmd->cmdsize;
    cmd = (const struct load_command *) cursor;
  }

  VG_(debugLog)(1, "dyld_helpers", "dlsym: could not find symbol table in %s\n", library);
  return NULL;
}

void VG_(dyld_register_existing_libraries)(void) {
  Int i;
  const HChar * name;
  const struct MACH_HEADER* loaded_image;
  ULong res = 0;

  for (i = 0; i < _dyld_image_count(); ++i) {
    name = _dyld_get_image_name(i);

    // we are mostly interested in system libraries
    if (VG_(strstr)(name, "/usr/lib/system/") == NULL) {
      continue;
    }

    loaded_image = (const struct MACH_HEADER*) _dyld_get_image_header(i);
    res = VG_(di_notify_dsc)(name, (Addr)loaded_image, sizeof(struct MACH_HEADER) + loaded_image->sizeofcmds);
    if (res == 0) {
      VG_(debugLog)(2, "dyld_helpers", "failed to load debuginfo from: %s\n", name);
    }
  }
}

#endif
