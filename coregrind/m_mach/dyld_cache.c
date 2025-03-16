
/*--------------------------------------------------------------------*/
/*--- DYLD Cache                                      dyld_cache.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2020-2024 Louis Brunner <louis.brunner.fr@gmail.com>

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

#include "pub_core_debuginfo.h"             // VG_(di_notify_dsc)
#include "pub_core_debuglog.h"              // VG_(debugLog)
#include "pub_core_mach.h"                  // VG_(dyld_cache_*)
#include "pub_core_syscall.h"               // VG_(do_syscall1)
#include "pub_core_libcbase.h"              // VG_(strncmp)
#include "pub_core_libcprint.h"             // VG_(dmsg)
#include "pub_core_libcfile.h"              // VG_(stat)
#include "vki/vki-scnums-darwin.h"          // __NR_shared_region_check_np
#include "priv_dyld_internals.h"            // CACHE_MAGIC_*, dyld_cache_header, etc

// Required by private headers underneath
#include "pub_core_libcassert.h"            // vg_assert
#include "pub_core_threadstate.h"           // ThreadState

// FIXME: probably shouldn't include this directly?
#include "m_syswrap/priv_syswrap-generic.h" // ML_(notify_core_and_tool_of_mmap)

#include <mach-o/loader.h>
#include <mach-o/fat.h>

// Only supported on macOS 11 onwards which is 64bit only
# define MACH_HEADER mach_header_64
# define MAGIC MH_MAGIC_64

static void output_text_debug_info(const dyld_cache_image_text_info* textInfo);
static void output_debug_info(const dyld_cache_header* dyld_cache);

typedef struct {
  const dyld_cache_header* header;
  Addr slide;
  Bool tried;
} DYLDCache;

static DYLDCache dyld_cache = {
  .header = NULL,
  .slide = 0,
  .tried = False,
};

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
#elif defined(VGA_arm64)
    VG_(strcmp)(header->magic, CACHE_MAGIC_arm64) != 0
    && VG_(strcmp)(header->magic, CACHE_MAGIC_arm64e) != 0
#else
    0
#error "unknown architecture"
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
    VG_(debugLog)(5, "dyld_cache",
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
    VG_(debugLog)(2, "dyld_cache", "dyld cache slide: %#lx\n", dyld_cache.slide);

    if (!try_to_init_header(cache_address)) {
      return 0;
    }

    if (dyld_cache.header->mappingOffset >= __offsetof(dyld_cache_header, subCacheArrayCount)) {
      Bool sub_cache_v2 = dyld_cache.header->mappingOffset > __offsetof(dyld_cache_header, cacheSubType);
      Addr sub_caches = calculate_relative(dyld_cache.header, dyld_cache.header->subCacheArrayOffset);

      for (int i = 0; i < dyld_cache.header->subCacheArrayCount; ++i) {
        Addr sub_cache_addr;

        VG_(debugLog)(2, "dyld_cache", "found sub cache %d (v2: %d)\n", i, sub_cache_v2);

        if (sub_cache_v2) {
          const dyld_subcache_entry* sub_cache = &((const dyld_subcache_entry*) sub_caches)[i];
          const uint8_t* u = sub_cache->uuid;
          sub_cache_addr = calculate_relative(dyld_cache.header, sub_cache->cacheVMOffset);
          VG_(debugLog)(5, "dyld_cache",
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
          VG_(debugLog)(5, "dyld_cache",
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

Addr VG_(dyld_cache_get_slide)(void) {
  return dyld_cache.slide;
}

int ensure_init(void) {
  if (dyld_cache.header != NULL) {
    return 1;
  }

  // FIXME: unlikely race condition?
  if (dyld_cache.tried) {
    return 0;
  }
  dyld_cache.tried = True;

  if (!try_to_init()) {
    VG_(dmsg)(
      "WARNING: could not read from dyld shared cache (DSC)\n"
      "Some reports (especially memory leaks) might be missing or incorrect (false-positives)\n"
    );
    return 0;
  }
  // We currently detect if dyld is loading/using a library by checking if stat64 fails.
  // However, dyld doesn't seem to call stat64 for all of them anymore.
  // All arm64 binaries are executables but some x86 ones might not be so let's avoid them just to be safe.
  VG_(dyld_cache_load_library)("/usr/lib/system/libsystem_kernel.dylib");
  VG_(dyld_cache_load_library)("/usr/lib/system/libsystem_pthread.dylib");
  VG_(dyld_cache_load_library)("/usr/lib/system/libsystem_platform.dylib");
#endif

  return 1;
}

void VG_(dyld_cache_init)(const HChar* tool) {
  // drd crashes if you map memory segments in m_main
  if (VG_(strcmp)(tool, "drd") == 0) {
    return;
  }

  ensure_init();
}

int VG_(dyld_cache_might_be_in)(const HChar* path) {
  // If not init'd, there is no point
  if (!ensure_init()) {
    return 0;
  }

  if (VG_(strncmp)(path, "/usr/lib/", 9) == 0) {
    return 1;
  }
  if (VG_(strncmp)(path, "/System/Library/", 16) == 0) {
    return 1;
  }
  // FIXME: more flexible heuristics around extensions?
  return 0;
}

static struct MACH_HEADER* find_image_text(const dyld_cache_header* header, const char* path, SizeT* len) {
  vg_assert(len);
  *len = 0;

  const dyld_cache_image_text_info* textInfos = (const dyld_cache_image_text_info*) calculate_relative(header, header->imagesTextOffset);

  for (int i = 0; i < header->imagesTextCount; ++i) {
    const dyld_cache_image_text_info* textInfo = &textInfos[i];
    const char* imagePath = (const char*) calculate_relative(header, textInfo->pathOffset);

    if (VG_(strcmp)(imagePath, path) == 0) {
      output_text_debug_info(textInfo);
      *len = textInfo->textSegmentSize;
      return (struct MACH_HEADER*) calculate_unslid(textInfo->loadAddress);
    }
  }

  return NULL;
}

int VG_(dyld_cache_load_library)(const HChar* path) {
  struct MACH_HEADER *image = NULL;
  ULong res = 0;
  SizeT len = 0;

  // If not init'd, there is no point trying
  if (!ensure_init()) {
    return 0;
  }

  VG_(debugLog)(2, "dyld_cache", "potential dylib to check in the cache: %s\n", path);

  image = find_image_text(dyld_cache.header, path, &len);
  if (image == NULL) {
    VG_(debugLog)(2, "dyld_cache", "image not found: %s\n", path);
    return 0;
  }

  if (image->magic != MAGIC) {
    VG_(debugLog)(2, "dyld_cache", "image not mach-o (%#x): %s\n", image->magic, path);
    return 0;
  }

  VG_(debugLog)(2, "dyld_cache", "image (%p) is valid, forwarding to debuginfo: %s\n", image, path);
  res = VG_(di_notify_dsc)(path, (Addr)image, len);
  if (res == 0) {
    VG_(debugLog)(2, "dyld_cache", "failed to load debuginfo from: %s\n", path);
    return 0;
  }

  VG_(debugLog)(2, "dyld_cache", "image fully loaded: %s\n", path);

  return 1;
}

static void output_text_debug_info(const dyld_cache_image_text_info* textInfo) {
  const uint8_t* u = textInfo->uuid;
  VG_(debugLog)(5, "dyld_cache",
    "image_text_info{\n"
    "  .uuid: %02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x,\n"
    "  .loadAddress: %#llx,\n"
    "  .textSegmentSize: %u,\n"
    "  .pathOffset: %#x,\n"
    "}\n",
    u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7], u[8],
    u[9], u[10], u[11], u[12], u[13], u[14], u[15],
    textInfo->loadAddress,
    textInfo->textSegmentSize,
    textInfo->pathOffset
  );
}

static void output_debug_info(const dyld_cache_header* cache) {
  const uint8_t* u1 = cache->uuid;
  const uint8_t* u2 = cache->symbolFileUUID;
  VG_(debugLog)(5, "dyld_cache",
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
    "  .dylibsPBLSetAddr: %llx,\n"
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
    cache->magic,
    cache->mappingOffset,
    cache->mappingCount,
    cache->imagesOffsetOld,
    cache->imagesCountOld,
    cache->dyldBaseAddress,
    cache->codeSignatureOffset,
    cache->codeSignatureSize,
    cache->slideInfoOffsetUnused,
    cache->slideInfoSizeUnused,
    cache->localSymbolsOffset,
    cache->localSymbolsSize,
    u1[0], u1[1], u1[2], u1[3], u1[4], u1[5], u1[6], u1[7], u1[8],
    u1[9], u1[10], u1[11], u1[12], u1[13], u1[14], u1[15],
    cache->cacheType,
    cache->branchPoolsOffset,
    cache->branchPoolsCount,
    cache->dyldInCacheMH,
    cache->dyldInCacheEntry,
    cache->imagesTextOffset,
    cache->imagesTextCount,
    cache->patchInfoAddr,
    cache->patchInfoSize,
    cache->otherImageGroupAddrUnused,
    cache->otherImageGroupSizeUnused,
    cache->progClosuresAddr,
    cache->progClosuresSize,
    cache->progClosuresTrieAddr,
    cache->progClosuresTrieSize,
    cache->platform,
    (UInt)cache->formatVersion,
    cache->dylibsExpectedOnDisk,
    cache->simulator,
    cache->locallyBuiltCache,
    cache->builtFromChainedFixups,
    cache->padding,
    cache->sharedRegionStart,
    cache->sharedRegionSize,
    cache->maxSlide,
    cache->dylibsImageArrayAddr,
    cache->dylibsImageArraySize,
    cache->dylibsTrieAddr,
    cache->dylibsTrieSize,
    cache->otherImageArrayAddr,
    cache->otherImageArraySize,
    cache->otherTrieAddr,
    cache->otherTrieSize,
    cache->mappingWithSlideOffset,
    cache->mappingWithSlideCount,
    cache->dylibsPBLStateArrayAddrUnused,
    cache->dylibsPBLSetAddr,
    cache->programsPBLSetPoolAddr,
    cache->programsPBLSetPoolSize,
    cache->programTrieAddr,
    cache->programTrieSize,
    cache->osVersion,
    cache->altPlatform,
    cache->altOsVersion,
    cache->swiftOptsOffset,
    cache->swiftOptsSize,
    cache->subCacheArrayOffset,
    cache->subCacheArrayCount,
    u2[0], u2[1], u2[2], u2[3], u2[4], u2[5], u2[6], u2[7], u2[8],
    u2[9], u2[10], u2[11], u2[12], u2[13], u2[14], u2[15],
    cache->rosettaReadOnlyAddr,
    cache->rosettaReadOnlySize,
    cache->rosettaReadWriteAddr,
    cache->rosettaReadWriteSize,
    cache->imagesOffset,
    cache->imagesCount,
    cache->cacheSubType,
    cache->objcOptsOffset,
    cache->objcOptsSize,
    cache->cacheAtlasOffset,
    cache->cacheAtlasSize,
    cache->dynamicDataOffset,
    cache->dynamicDataMaxSize
  );
}

#endif
