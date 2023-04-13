
/*--------------------------------------------------------------------*/
/*--- DYLD Cache                                      dyld_cache.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2020 Louis Brunner <louis.brunner.fr@gmail.com>

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

// While dyld_caching as existed for longer than that
// we have used DYLD_SHARED_REGION=avoid in the past
//
// Starting with macOS 11 (Big Sur), it isn't an option anymore
// as some dylib are not provided in file format anymore
#if defined(VGO_darwin) && DARWIN_VERS >= DARWIN_11_00

#include "pub_core_debuginfo.h"             // VG_(di_notify_mmap)
#include "pub_core_debuglog.h"              // VG_(debugLog)
#include "pub_core_mach.h"                  // VG_(dyld_cache_*)
#include "pub_core_syscall.h"               // VG_(do_syscall1)
#include "pub_tool_libcbase.h"              // VG_(strncmp)
#include "pub_tool_libcprint.h"             // VG_(dmsg)
#include "pub_tool_libcfile.h"              // VG_(stat)
#include "vki/vki-scnums-darwin.h"          // __NR_shared_region_check_np
#include "priv_dyld_internals.h"            // CACHE_MAGIC_*, dyld_cache_header, etc

// Required by private headers underneath
#include "pub_core_libcassert.h"            // vg_assert
#include "pub_core_threadstate.h"           // ThreadState

// FIXME: probably shouldn't include this directly?
#include "m_syswrap/priv_syswrap-generic.h" // ML_(notify_core_and_tool_of_mmap)

static void output_debug_info(const dyld_cache_header* dyld_cache);

typedef struct {
  const dyld_cache_header* header;
  Addr slide;
} DYLDCache;

static DYLDCache dyld_cache = {};

static Addr calculate_relative(const dyld_cache_header * header, Addr offset) {
  return (Addr)header + offset;
}

static Addr calculate_unslid(Addr addr) {
  return addr + dyld_cache.slide;
}

static int try_to_init_header(Addr address) {
  const dyld_cache_header* header = (const dyld_cache_header *) address;

  if (
#if defined(VGA_amd64)
    VG_(strcmp)(header->magic, CACHE_MAGIC_x86_64) != 0
    && VG_(strcmp)(header->magic, CACHE_MAGIC_x86_64_HASWELL) != 0
#else
    0
#error "unknown architecture
#endif
  ) {
    VG_(debugLog)(2, "dyld_cache", "ERROR: incompatible shared dyld cache (%s)\n", header->magic);
    return 0;
  }

  if (header->mappingCount < 1) {
    VG_(debugLog)(2, "dyld_cache", "ERROR: no mappings in the dyld cache\n");
    return 0;
  }

  VG_(debugLog)(2, "dyld_cache", "shared dyld cache format: %d / %#x\n", header->formatVersion, header->mappingOffset);
  output_debug_info(header);

  const dyld_cache_mapping_info* mappings = (const dyld_cache_mapping_info*)(calculate_relative(header, header->mappingOffset));
  for (int i = 0; i < header->mappingCount; ++i) {
    const dyld_cache_mapping_info* mapping = &mappings[i];
    Addr map_addr = calculate_unslid(mapping->address);
    VG_(debugLog)(4, "dyld_cache",
      "mapping[%d]{\n"
      "  .address: %#lx,\n"
      "  .size: %llu (%#llx),\n"
      "  .fileOffset: %#lx,\n"
      "  .maxProt: %#x,\n"
      "  .initProt: %#x,\n"
      "}\n",
      i,
      map_addr,
      mapping->size,
      mapping->size,
      calculate_relative(header, mapping->fileOffset),
      mapping->maxProt,
      mapping->initProt
    );
    ML_(notify_core_and_tool_of_mmap)(map_addr, mapping->size, mapping->initProt, VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS, -1, 0);
  }

  if (dyld_cache.header->mappingOffset >= __offsetof(dyld_cache_header, dynamicDataMaxSize) && header->dynamicDataMaxSize > 0) {
    ML_(notify_core_and_tool_of_mmap)(calculate_relative(header, header->dynamicDataOffset), header->dynamicDataMaxSize, VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS, -1, 0);
  }

  return 1;
}

static int try_to_init(void) {
  // Read address of the shared cache which is mapped in our address space
  // and tell Valgrind about it so we avoid false-positives and massive suppression files
  {
    Addr cache_address;
    if (sr_Res(VG_(do_syscall1)(__NR_shared_region_check_np, (UWord)&cache_address)) != 0) {
      VG_(debugLog)(2, "dyld_cache", "ERROR: could not get shared dyld cache address\n");
      return 0;
    }
    VG_(debugLog)(2, "dyld_cache", "shared dyld cache found: %#lx\n", cache_address);

    // FIXME: should be after `try_to_init_header` but we also need the slide calculate _before_
    dyld_cache.header = (const dyld_cache_header *) cache_address;
    const dyld_cache_mapping_info* mappings = (const dyld_cache_mapping_info*)(calculate_relative(dyld_cache.header, dyld_cache.header->mappingOffset));
    dyld_cache.slide = cache_address - mappings[0].address;

    if (!try_to_init_header(cache_address)) {
      return 0;
    }

    if (dyld_cache.header->mappingOffset >= __offsetof(dyld_cache_header, subCacheArrayCount)) {
      Bool sub_cache_v2 = dyld_cache.header->mappingOffset >= __offsetof(dyld_cache_header, cacheSubType);
      Addr sub_caches = calculate_relative(dyld_cache.header, dyld_cache.header->subCacheArrayOffset);

      for (int i = 0; i < dyld_cache.header->subCacheArrayCount; ++i) {
        Addr sub_cache_addr;

        VG_(debugLog)(2, "dyld_cache", "found sub cache %d (v2: %d)\n", i, sub_cache_v2);

        if (sub_cache_v2) {
          const dyld_subcache_entry* sub_cache = &((const dyld_subcache_entry*) sub_caches)[i];
          const uint8_t* u = sub_cache->uuid;
          sub_cache_addr = calculate_relative(dyld_cache.header, sub_cache->cacheVMOffset);
          VG_(debugLog)(4, "dyld_cache",
            "sub_cache_v2[%d]{\n"
            "  .uuid: %02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x,\n"
            "  .cacheVMOffset: %#lx,\n"
            "  .fileSuffix: %s,\n"
            "}\n",
            i,
            u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7], u[8], u[9], u[10], u[11], u[12], u[13], u[14], u[15],
            sub_cache_addr,
            sub_cache->fileSuffix
          );

        } else {
          const dyld_subcache_entry_v1* sub_cache = &((const dyld_subcache_entry_v1*) sub_caches)[i];
          const uint8_t* u = sub_cache->uuid;
          sub_cache_addr = calculate_relative(dyld_cache.header, sub_cache->cacheVMOffset);
          VG_(debugLog)(4, "dyld_cache",
            "sub_cache_v1[%d]{\n"
            "  .uuid: %02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x,\n"
            "  .cacheVMOffset: %#lx,\n"
            "}\n",
            i,
            u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7], u[8], u[9], u[10], u[11], u[12], u[13], u[14], u[15],
            sub_cache_addr
          );
        }

        if (!try_to_init_header(sub_cache_addr)) {
          return 0;
        }
      }
    }
  }

  return 1;
}

void VG_(dyld_cache_init)(void) {
  if (!try_to_init()) {
    VG_(dmsg)(
      "WARNING: could not read from dyld shared cache (DSC)\n"
      "Some reports (especially memory leaks) might be missing or incorrect (false-positives)\n"
    );
    return;
  }
}

static void output_debug_info(const dyld_cache_header* dyld_cache) {
  const uint8_t* u1 = dyld_cache->uuid;
  const uint8_t* u2 = dyld_cache->symbolFileUUID;
  VG_(debugLog)(4, "dyld_cache",
    "shared dyld content: {\n"
    "  .magic: %s,\n"
    "  .mappingOffset: %#x,\n"
    "  .mappingCount: %u,\n"
    "  .imagesOffsetOld: %#x,\n"
    "  .imagesCountOld: %u,\n"
    "  .dyldBaseAddress: %#llx,\n"
    "  .codeSignatureOffset: %#llx,\n"
    "  .codeSignatureSize: %llu,\n"
    "  .slideInfoOffsetUnused: %#llx,\n"
    "  .slideInfoSizeUnused: %llu,\n"
    "  .localSymbolsOffset: %#llx,\n"
    "  .localSymbolsSize: %llu,\n"
    "  .uuid: %02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x,\n"
    "  .cacheType: %llu,\n"
    "  .branchPoolsOffset: %#x,\n"
    "  .branchPoolsCount: %u,\n"
    "  .dyldInCacheMH: %#llx,\n"
    "  .dyldInCacheEntry: %#llx,\n"
    "  .imagesTextOffset: %#llx,\n"
    "  .imagesTextCount: %llu,\n"
    "  .patchInfoAddr: %#llx,\n"
    "  .patchInfoSize: %llu,\n"
    "  .otherImageGroupAddrUnused: %#llx,\n"
    "  .otherImageGroupSizeUnused: %llu,\n"
    "  .progClosuresAddr: %#llx,\n"
    "  .progClosuresSize: %llu,\n"
    "  .progClosuresTrieAddr: %#llx,\n"
    "  .progClosuresTrieSize: %llu,\n"
    "  .platform: %#x,\n"
    "  .formatVersion: %#x,\n"
    "  .dylibsExpectedOnDisk: %d,\n"
    "  .simulator: %d,\n"
    "  .locallyBuiltCache: %d,\n"
    "  .builtFromChainedFixups: %d,\n"
    "  .padding: %d,\n"
    "  .sharedRegionStart: %#llx,\n"
    "  .sharedRegionSize: %llu,\n"
    "  .maxSlide: %#llx,\n"
    "  .dylibsImageArrayAddr: %#llx,\n"
    "  .dylibsImageArraySize: %llu,\n"
    "  .dylibsTrieAddr: %#llx,\n"
    "  .dylibsTrieSize: %llu,\n"
    "  .otherImageArrayAddr: %#llx,\n"
    "  .otherImageArraySize: %llu,\n"
    "  .otherTrieAddr: %#llx,\n"
    "  .otherTrieSize: %llu,\n"
    "  .mappingWithSlideOffset: %#x,\n"
    "  .mappingWithSlideCount: %u,\n"
    "  .dylibsPBLStateArrayAddrUnused: %llu,\n"
    "  .dylibsPBLSetAddr: %llu,\n"
    "  .programsPBLSetPoolAddr: %#llx,\n"
    "  .programsPBLSetPoolSize: %llu,\n"
    "  .programTrieAddr: %#llx,\n"
    "  .programTrieSize: %u,\n"
    "  .osVersion: %#x,\n"
    "  .altPlatform: %#x,\n"
    "  .altOsVersion: %#x,\n"
    "  .swiftOptsOffset: %#llx,\n"
    "  .swiftOptsSize: %llu,\n"
    "  .subCacheArrayOffset: %#x,\n"
    "  .subCacheArrayCount: %u,\n"
    "  .symbolFileUUID: %02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x,\n"
    "  .rosettaReadOnlyAddr: %#llx,\n"
    "  .rosettaReadOnlySize: %llu,\n"
    "  .rosettaReadWriteAddr: %#llx,\n"
    "  .rosettaReadWriteSize: %llu,\n"
    "  .imagesOffset: %#x,\n"
    "  .imagesCount: %u,\n"
    "  .cacheSubType: %#x,\n"
    "  .objcOptsOffset: %#llx,\n"
    "  .objcOptsSize: %llu,\n"
    "  .cacheAtlasOffset: %#llx,\n"
    "  .cacheAtlasSize: %llu,\n"
    "  .dynamicDataOffset: %#llx,\n"
    "  .dynamicDataMaxSize: %llu,\n"
    "}\n",
    dyld_cache->magic,
    dyld_cache->mappingOffset,
    dyld_cache->mappingCount,
    dyld_cache->imagesOffsetOld,
    dyld_cache->imagesCountOld,
    dyld_cache->dyldBaseAddress,
    dyld_cache->codeSignatureOffset,
    dyld_cache->codeSignatureSize,
    dyld_cache->slideInfoOffsetUnused,
    dyld_cache->slideInfoSizeUnused,
    dyld_cache->localSymbolsOffset,
    dyld_cache->localSymbolsSize,
    u1[0], u1[1], u1[2], u1[3], u1[4], u1[5], u1[6], u1[7], u1[8],
    u1[9], u1[10], u1[11], u1[12], u1[13], u1[14], u1[15],
    dyld_cache->cacheType,
    dyld_cache->branchPoolsOffset,
    dyld_cache->branchPoolsCount,
    dyld_cache->dyldInCacheMH,
    dyld_cache->dyldInCacheEntry,
    dyld_cache->imagesTextOffset,
    dyld_cache->imagesTextCount,
    dyld_cache->patchInfoAddr,
    dyld_cache->patchInfoSize,
    dyld_cache->otherImageGroupAddrUnused,
    dyld_cache->otherImageGroupSizeUnused,
    dyld_cache->progClosuresAddr,
    dyld_cache->progClosuresSize,
    dyld_cache->progClosuresTrieAddr,
    dyld_cache->progClosuresTrieSize,
    dyld_cache->platform,
    dyld_cache->formatVersion,
    dyld_cache->dylibsExpectedOnDisk,
    dyld_cache->simulator,
    dyld_cache->locallyBuiltCache,
    dyld_cache->builtFromChainedFixups,
    dyld_cache->padding,
    dyld_cache->sharedRegionStart,
    dyld_cache->sharedRegionSize,
    dyld_cache->maxSlide,
    dyld_cache->dylibsImageArrayAddr,
    dyld_cache->dylibsImageArraySize,
    dyld_cache->dylibsTrieAddr,
    dyld_cache->dylibsTrieSize,
    dyld_cache->otherImageArrayAddr,
    dyld_cache->otherImageArraySize,
    dyld_cache->otherTrieAddr,
    dyld_cache->otherTrieSize,
    dyld_cache->mappingWithSlideOffset,
    dyld_cache->mappingWithSlideCount,
    dyld_cache->dylibsPBLStateArrayAddrUnused,
    dyld_cache->dylibsPBLSetAddr,
    dyld_cache->programsPBLSetPoolAddr,
    dyld_cache->programsPBLSetPoolSize,
    dyld_cache->programTrieAddr,
    dyld_cache->programTrieSize,
    dyld_cache->osVersion,
    dyld_cache->altPlatform,
    dyld_cache->altOsVersion,
    dyld_cache->swiftOptsOffset,
    dyld_cache->swiftOptsSize,
    dyld_cache->subCacheArrayOffset,
    dyld_cache->subCacheArrayCount,
    u2[0], u2[1], u2[2], u2[3], u2[4], u2[5], u2[6], u2[7], u2[8],
    u2[9], u2[10], u2[11], u2[12], u2[13], u2[14], u2[15],
    dyld_cache->rosettaReadOnlyAddr,
    dyld_cache->rosettaReadOnlySize,
    dyld_cache->rosettaReadWriteAddr,
    dyld_cache->rosettaReadWriteSize,
    dyld_cache->imagesOffset,
    dyld_cache->imagesCount,
    dyld_cache->cacheSubType,
    dyld_cache->objcOptsOffset,
    dyld_cache->objcOptsSize,
    dyld_cache->cacheAtlasOffset,
    dyld_cache->cacheAtlasSize,
    dyld_cache->dynamicDataOffset,
    dyld_cache->dynamicDataMaxSize
  );
}

#endif
