/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2020-2025 Louis Brunner <louis.brunner.fr@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/
#ifndef __PRIV_DYLD_INTERNALS_H
#define __PRIV_DYLD_INTERNALS_H

#if defined(VGO_darwin)

// This file contains a bunch of structure defined in Apple's dyld

// From Apple's `dyld/dyld/SharedCacheRuntime.cpp`
#define CACHE_MAGIC_x86_64         "dyld_v1  x86_64"
#define CACHE_MAGIC_x86_64_HASWELL "dyld_v1 x86_64h"
#define CACHE_MAGIC_arm64          "dyld_v1   arm64"
#define CACHE_MAGIC_arm64e         "dyld_v1  arm64e"

// From Apple's `dyld/cache-builder/dyld_cache_format.h`
typedef struct {
  char        magic[16];              // e.g. "dyld_v0    i386"
  uint32_t    mappingOffset;          // file offset to first dyld_cache_mapping_info
  uint32_t    mappingCount;           // number of dyld_cache_mapping_info entries
  uint32_t    imagesOffsetOld;        // UNUSED: moved to imagesOffset to prevent older dsc_extarctors from crashing
  uint32_t    imagesCountOld;         // UNUSED: moved to imagesCount to prevent older dsc_extarctors from crashing
  uint64_t    dyldBaseAddress;        // base address of dyld when cache was built
  uint64_t    codeSignatureOffset;    // file offset of code signature blob
  uint64_t    codeSignatureSize;      // size of code signature blob (zero means to end of file)
  uint64_t    slideInfoOffsetUnused;  // unused.  Used to be file offset of kernel slid info
  uint64_t    slideInfoSizeUnused;    // unused.  Used to be size of kernel slid info
  uint64_t    localSymbolsOffset;     // file offset of where local symbols are stored
  uint64_t    localSymbolsSize;       // size of local symbols information
  uint8_t     uuid[16];               // unique value for each shared cache file
  uint64_t    cacheType;              // 0 for development, 1 for production, 2 for multi-cache
  uint32_t    branchPoolsOffset;      // file offset to table of uint64_t pool addresses
  uint32_t    branchPoolsCount;       // number of uint64_t entries
  uint64_t    dyldInCacheMH;          // (unslid) address of mach_header of dyld in cache
  uint64_t    dyldInCacheEntry;       // (unslid) address of entry point (_dyld_start) of dyld in cache
  uint64_t    imagesTextOffset;       // file offset to first dyld_cache_image_text_info
  uint64_t    imagesTextCount;        // number of dyld_cache_image_text_info entries
  uint64_t    patchInfoAddr;          // (unslid) address of dyld_cache_patch_info
  uint64_t    patchInfoSize;          // Size of all of the patch information pointed to via the dyld_cache_patch_info
  uint64_t    otherImageGroupAddrUnused;    // unused
  uint64_t    otherImageGroupSizeUnused;    // unused
  uint64_t    progClosuresAddr;       // (unslid) address of list of program launch closures
  uint64_t    progClosuresSize;       // size of list of program launch closures
  uint64_t    progClosuresTrieAddr;   // (unslid) address of trie of indexes into program launch closures
  uint64_t    progClosuresTrieSize;   // size of trie of indexes into program launch closures
  uint32_t    platform;               // platform number (macOS=1, etc)
  uint32_t    formatVersion          : 8,  // dyld3::closure::kFormatVersion
              dylibsExpectedOnDisk   : 1,  // dyld should expect the dylib exists on disk and to compare inode/mtime to see if cache is valid
              simulator              : 1,  // for simulator of specified platform
              locallyBuiltCache      : 1,  // 0 for B&I built cache, 1 for locally built cache
              builtFromChainedFixups : 1,  // some dylib in cache was built using chained fixups, so patch tables must be used for overrides
              padding                : 20; // TBD
  uint64_t    sharedRegionStart;      // base load address of cache if not slid
  uint64_t    sharedRegionSize;       // overall size required to map the cache and all subCaches, if any
  uint64_t    maxSlide;               // runtime slide of cache can be between zero and this value
  uint64_t    dylibsImageArrayAddr;   // (unslid) address of ImageArray for dylibs in this cache
  uint64_t    dylibsImageArraySize;   // size of ImageArray for dylibs in this cache
  uint64_t    dylibsTrieAddr;         // (unslid) address of trie of indexes of all cached dylibs
  uint64_t    dylibsTrieSize;         // size of trie of cached dylib paths
  uint64_t    otherImageArrayAddr;    // (unslid) address of ImageArray for dylibs and bundles with dlopen closures
  uint64_t    otherImageArraySize;    // size of ImageArray for dylibs and bundles with dlopen closures
  uint64_t    otherTrieAddr;          // (unslid) address of trie of indexes of all dylibs and bundles with dlopen closures
  uint64_t    otherTrieSize;          // size of trie of dylibs and bundles with dlopen closures
  uint32_t    mappingWithSlideOffset; // file offset to first dyld_cache_mapping_and_slide_info
  uint32_t    mappingWithSlideCount;  // number of dyld_cache_mapping_and_slide_info entries
  uint64_t    dylibsPBLStateArrayAddrUnused;    // unused
  uint64_t    dylibsPBLSetAddr;           // (unslid) address of PrebuiltLoaderSet of all cached dylibs
  uint64_t    programsPBLSetPoolAddr;     // (unslid) address of pool of PrebuiltLoaderSet for each program
  uint64_t    programsPBLSetPoolSize;     // size of pool of PrebuiltLoaderSet for each program
  uint64_t    programTrieAddr;            // (unslid) address of trie mapping program path to PrebuiltLoaderSet
  uint32_t    programTrieSize;
  uint32_t    osVersion;                  // OS Version of dylibs in this cache for the main platform
  uint32_t    altPlatform;                // e.g. iOSMac on macOS
  uint32_t    altOsVersion;               // e.g. 14.0 for iOSMac
  uint64_t    swiftOptsOffset;        // VM offset from cache_header* to Swift optimizations header
  uint64_t    swiftOptsSize;          // size of Swift optimizations header
  uint32_t    subCacheArrayOffset;    // file offset to first dyld_subcache_entry
  uint32_t    subCacheArrayCount;     // number of subCache entries
  uint8_t     symbolFileUUID[16];     // unique value for the shared cache file containing unmapped local symbols
  uint64_t    rosettaReadOnlyAddr;    // (unslid) address of the start of where Rosetta can add read-only/executable data
  uint64_t    rosettaReadOnlySize;    // maximum size of the Rosetta read-only/executable region
  uint64_t    rosettaReadWriteAddr;   // (unslid) address of the start of where Rosetta can add read-write data
  uint64_t    rosettaReadWriteSize;   // maximum size of the Rosetta read-write region
  uint32_t    imagesOffset;           // file offset to first dyld_cache_image_info
  uint32_t    imagesCount;            // number of dyld_cache_image_info entries
  uint32_t    cacheSubType;           // 0 for development, 1 for production, when cacheType is multi-cache(2)
  uint64_t    objcOptsOffset;         // VM offset from cache_header* to ObjC optimizations header
  uint64_t    objcOptsSize;           // size of ObjC optimizations header
  uint64_t    cacheAtlasOffset;       // VM offset from cache_header* to embedded cache atlas for process introspection
  uint64_t    cacheAtlasSize;         // size of embedded cache atlas
  uint64_t    dynamicDataOffset;      // VM offset from cache_header* to the location of dyld_cache_dynamic_data_header
  uint64_t    dynamicDataMaxSize;     // maximum size of space reserved from dynamic data
} dyld_cache_header;

// From Apple's `dyld/cache-builder/dyld_cache_format.h`
typedef struct {
  uint64_t        address;
  uint64_t        size;
  uint64_t        fileOffset;
  uint32_t        maxProt;
  uint32_t        initProt;
} dyld_cache_mapping_info;

// From Apple's `dyld/cache-builder/dyld_cache_format.h`
typedef struct {
    uint8_t     uuid[16];           // The UUID of the subCache file
    uint64_t    cacheVMOffset;      // The offset of this subcache from the main cache base address
    char        fileSuffix[32];     // The file name suffix of the subCache file e.g. ".25.data", ".03.development"
} dyld_subcache_entry;

// From Apple's `dyld/cache-builder/dyld_cache_format.h`
typedef struct {
  uint8_t     uuid[16];           // The UUID of the subCache file
  uint64_t    cacheVMOffset;      // The offset of this subcache from the main cache base address
} dyld_subcache_entry_v1;

// From Apple's `dyld/cache-builder/dyld_cache_format.h`
typedef struct {
  uuid_t      uuid;
  uint64_t    loadAddress;            // unslid address of start of __TEXT
  uint32_t    textSegmentSize;
  uint32_t    pathOffset;             // offset from start of cache file
} dyld_cache_image_text_info;

#endif

#endif
