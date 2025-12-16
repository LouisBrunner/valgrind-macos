/*--------------------------------------------------------------------*/
/*--- Dummy libdyld.dylib                           dummy_dyld.cpp ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2024-2025 Louis Brunner <louis.brunner.fr@gmail.com>

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

#include "config.h"

#if DARWIN_VERS < DARWIN_15_04
#error "This file is only for macOS 15.4 and later"
#endif

// Adapted from dyld's source code
#include <mach/mach.h>
#include <mach-o/loader.h>
#include <os/lock.h>
#include <pthread.h>
#include <stdlib.h>

#include "vki/vki-scnums-darwin.h"
#include "priv_dyld_internals.h"

struct ProgramVars {
  const void*      mh             = nullptr;
  int*             NXArgcPtr      = nullptr;
  const char***    NXArgvPtr      = nullptr;
  const char***    environPtr     = nullptr;
  const char**     __prognamePtr  = nullptr;
};

struct Error {
  void* _buffer = nullptr;
};

struct Header {
  mach_header_64 mh;
};

struct DyldSharedCache {
  dyld_cache_header header;
};

// FIXME: genuinely no clue how to implement those
typedef os_unfair_lock_t os_unfair_recursive_lock_t;
typedef int os_unfair_lock_options_t;

typedef bool (*FuncLookup)(const char* name, void** addr);

// Support for our custom helper
extern "C" long do_syscall(long num, ...);

static struct {
  int             NXArgc      = 0;
  const char**    NXArgv      = nullptr;
  const char**    environ     = nullptr;
  const char*     __progname  = nullptr;
} ProgramData;

// FIXME: could not get a simple write(1, "unexpected " x "from dyld\n", strlen) working
#define PANIC(x, n) do { \
  exit(66+n); \
} while (0)

// The helper itself
struct [[clang::ptrauth_vtable_pointer(process_independent, address_discrimination, type_discrimination)]] DummySystemHelpers {
  virtual uintptr_t       version() const {
    return 7;
  }

  virtual void*           malloc(size_t size) const { PANIC("malloc", 0); }
  virtual void            free(void* p) const { PANIC("free", 1); }
  virtual size_t          malloc_size(const void* p) const { PANIC("malloc_size", 2); }
  virtual kern_return_t   vm_allocate(vm_map_t target_task, vm_address_t* address, vm_size_t size, int flags) const { PANIC("vm_allocate", 3); }
  virtual kern_return_t   vm_deallocate(vm_map_t target_task, vm_address_t address, vm_size_t size) const { PANIC("vm_deallocate", 4); }
  virtual int             pthread_key_create_free(pthread_key_t* key) const { PANIC("pthread_key_create_free", 5); }
  virtual void*           pthread_getspecific(pthread_key_t key) const { PANIC("pthread_getspecific", 6); }
  virtual int             pthread_setspecific(pthread_key_t key, const void* value) const { PANIC("pthread_setspecific", 7); }
  virtual void            __cxa_atexit(void (*func)(void*), void* arg, void* dso) const { PANIC("__cxa_atexit", 8); }
  virtual void            __cxa_finalize_ranges(const struct __cxa_range_t ranges[], unsigned int count) const { PANIC("__cxa_finalize_ranges", 9); }
  virtual bool            isLaunchdOwned() const { PANIC("isLaunchdOwned", 10); }
  virtual void            os_unfair_recursive_lock_lock_with_options(os_unfair_recursive_lock_t lock, os_unfair_lock_options_t options) const { PANIC("os_unfair_recursive_lock_lock_with_options", 11); }
  virtual void            os_unfair_recursive_lock_unlock(os_unfair_recursive_lock_t lock) const { PANIC("os_unfair_recursive_lock_unlock", 12); }
  virtual void            exit(int result) const  __attribute__((__noreturn__)) { do_syscall(__NR_exit, result); __builtin_unreachable(); }
  virtual const char*     getenv(const char* key) const { PANIC("getenv", 14); }
  virtual int             mkstemp(char* templatePath) const { PANIC("mkstemp", 15); }

  // Added in version 2
  virtual void            os_unfair_recursive_lock_unlock_forked_child(os_unfair_recursive_lock_t lock) const { PANIC("os_unfair_recursive_lock_unlock_forked_child", 16); }

  // Added in version 3
  virtual void            setDyldPatchedObjCClasses() const { PANIC("setDyldPatchedObjCClasses", 17); }

  // Added in version 5
  virtual void            run_async(void* (*func)(void*), void* context) const { PANIC("run_async", 18); }

  // Added in version 6
  virtual void            os_unfair_lock_lock_with_options(os_unfair_lock_t lock, os_unfair_lock_options_t options) const { PANIC("os_unfair_lock_lock_with_options", 19); }
  virtual void            os_unfair_lock_unlock(os_unfair_lock_t lock) const { PANIC("os_unfair_lock_unlock", 20); }

  // Added in version 7
  virtual void            setDefaultProgramVars(ProgramVars& vars) const { // called during dyld4::prepare
    vars.__prognamePtr = &ProgramData.__progname;
    vars.NXArgcPtr  = &ProgramData.NXArgc;
    vars.NXArgvPtr  = &ProgramData.NXArgv;
    vars.environPtr = &ProgramData.environ;
  }
  virtual FuncLookup      legacyDyldFuncLookup() const { PANIC("legacyDyldFuncLookup", 22); }
  virtual Error           setUpThreadLocals(const DyldSharedCache* cache, const Header* hdr) const { PANIC("setUpThreadLocals", 23); }
};

__attribute__((used,section ("__DATA_CONST,__helper")))
static const DummySystemHelpers helpers;
