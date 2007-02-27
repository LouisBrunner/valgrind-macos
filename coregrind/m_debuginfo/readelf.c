
/*--------------------------------------------------------------------*/
/*--- Reading of syms & debug info from ELF .so/executable files.  ---*/
/*---                                                    readelf.c ---*/
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
/*
   Stabs reader greatly improved by Nick Nethercote, Apr 02.
   This module was also extensively hacked on by Jeremy Fitzhardinge
   and Tom Hughes.
*/

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_aspacemgr.h"    /* for mmaping debuginfo files */
#include "pub_core_machine.h"      /* VG_ELF_CLASS */
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_oset.h"
#include "pub_core_tooliface.h"    /* VG_(needs) */
#include "pub_core_xarray.h"
#include "priv_storage.h"
#include "priv_readelf.h"          /* self */
#include "priv_readdwarf.h"        /* 'cos ELF contains DWARF */
#include "priv_readstabs.h"        /* and stabs, if we're unlucky */

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <elf.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

/*------------------------------------------------------------*/
/*--- 32/64-bit parameterisation                           ---*/
/*------------------------------------------------------------*/

/* For all the ELF macros and types which specify '32' or '64',
   select the correct variant for this platform and give it
   an 'XX' name.  Then use the 'XX' variant consistently in
   the rest of this file. 
*/
#if VG_WORDSIZE == 4
#  define  ElfXX_Ehdr     Elf32_Ehdr
#  define  ElfXX_Shdr     Elf32_Shdr
#  define  ElfXX_Phdr     Elf32_Phdr
#  define  ElfXX_Sym      Elf32_Sym
#  define  ElfXX_Word     Elf32_Word
#  define  ElfXX_Addr     Elf32_Addr
#  define  ElfXX_Dyn      Elf32_Dyn
#  define  ELFXX_ST_BIND  ELF32_ST_BIND
#  define  ELFXX_ST_TYPE  ELF32_ST_TYPE

#elif VG_WORDSIZE == 8
#  define  ElfXX_Ehdr     Elf64_Ehdr
#  define  ElfXX_Shdr     Elf64_Shdr
#  define  ElfXX_Phdr     Elf64_Phdr
#  define  ElfXX_Sym      Elf64_Sym
#  define  ElfXX_Word     Elf64_Word
#  define  ElfXX_Addr     Elf64_Addr
#  define  ElfXX_Dyn      Elf64_Dyn
#  define  ELFXX_ST_BIND  ELF64_ST_BIND
#  define  ELFXX_ST_TYPE  ELF64_ST_TYPE

#else
# error "VG_WORDSIZE should be 4 or 8"
#endif


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Read symbol table and line info from ELF files.      ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* readelf.c parses ELF files and acquires symbol table info from
   them.  It calls onwards to readdwarf.c to read DWARF2/3 line number
   and call frame info found. */


/* Identify an ELF object file by peering at the first few bytes of
   it. */

Bool ML_(is_elf_object_file)( const void* buf )
{
   ElfXX_Ehdr* ehdr = (ElfXX_Ehdr*)buf;
   Int ok = 1;

   ok &= (ehdr->e_ident[EI_MAG0] == 0x7F
          && ehdr->e_ident[EI_MAG1] == 'E'
          && ehdr->e_ident[EI_MAG2] == 'L'
          && ehdr->e_ident[EI_MAG3] == 'F');
   ok &= (ehdr->e_ident[EI_CLASS] == VG_ELF_CLASS
          && ehdr->e_ident[EI_DATA] == VG_ELF_DATA2XXX
          && ehdr->e_ident[EI_VERSION] == EV_CURRENT);
   ok &= (ehdr->e_type == ET_EXEC || ehdr->e_type == ET_DYN);
   ok &= (ehdr->e_machine == VG_ELF_MACHINE);
   ok &= (ehdr->e_version == EV_CURRENT);
   ok &= (ehdr->e_shstrndx != SHN_UNDEF);
   ok &= (ehdr->e_shoff != 0 && ehdr->e_shnum != 0);
   ok &= (ehdr->e_phoff != 0 && ehdr->e_phnum != 0);

   if (ok)
      return True;
   else
      return False;
}


/* Show a raw ELF symbol, given its in-image address and name. */

static
void show_raw_elf_symbol ( Int i, 
                           ElfXX_Sym* sym, Char* sym_name, Addr sym_addr,
                           Bool ppc64_linux_format )
{
   HChar* space = ppc64_linux_format ? "                  " : "";
   VG_(printf)("raw symbol [%4d]: ", i);
   switch (ELFXX_ST_BIND(sym->st_info)) {
      case STB_LOCAL:  VG_(printf)("LOC "); break;
      case STB_GLOBAL: VG_(printf)("GLO "); break;
      case STB_WEAK:   VG_(printf)("WEA "); break;
      case STB_LOPROC: VG_(printf)("lop "); break;
      case STB_HIPROC: VG_(printf)("hip "); break;
      default:         VG_(printf)("??? "); break;
   }
   switch (ELFXX_ST_TYPE(sym->st_info)) {
      case STT_NOTYPE:  VG_(printf)("NOT "); break;
      case STT_OBJECT:  VG_(printf)("OBJ "); break;
      case STT_FUNC:    VG_(printf)("FUN "); break;
      case STT_SECTION: VG_(printf)("SEC "); break;
      case STT_FILE:    VG_(printf)("FIL "); break;
      case STT_LOPROC:  VG_(printf)("lop "); break;
      case STT_HIPROC:  VG_(printf)("hip "); break;
      default:          VG_(printf)("??? "); break;
   }
   VG_(printf)(": val %010p, %ssz %4d  %s\n",
               sym_addr, space, sym->st_size,
               ( sym->st_name ? sym_name : (Char*)"NONAME" ) ); 
}               


/* Decide whether SYM is something we should collect, and if so, copy
   relevant info to the _OUT arguments.  For {x86,amd64,ppc32}-linux
   this is straightforward - the name, address, size are copied out
   unchanged.

   For ppc64-linux it's more complex.  If the symbol is seen to be in
   the .opd section, it is taken to be a function descriptor, and so
   a dereference is attempted, in order to get hold of the real entry
   point address.  Also as part of the dereference, there is an attempt
   to calculate the TOC pointer (R2 value) associated with the symbol.

   To support the ppc64-linux pre-"dotless" ABI (prior to gcc 4.0.0),
   if the symbol is seen to be outside the .opd section and its name
   starts with a dot, an .opd deference is not attempted, and no TOC
   pointer is calculated, but the the leading dot is removed from the
   name.

   As a result, on ppc64-linux, the caller of this function may have
   to piece together the real size, address, name of the symbol from
   multiple calls to this function.  Ugly and confusing.
*/
static 
Bool get_elf_symbol_info ( 
        /* INPUTS */
        struct _SegInfo* si,  /* containing SegInfo */
        ElfXX_Sym* sym,       /* ELF symbol */
        Char*      sym_name,  /* name */
        Addr       sym_addr,  /* declared address */
        UChar*     opd_filea, /* oimage of .opd sec (ppc64-linux only) */
        OffT       opd_offset, /* base address assumed in oimage */
        /* OUTPUTS */
        Char** sym_name_out,   /* name we should record */
        Addr*  sym_addr_out,   /* addr we should record */
        Int*   sym_size_out,   /* symbol size */
        Addr*  sym_tocptr_out, /* ppc64-linux only: R2 value to be
                                  used on entry */
        Bool*  from_opd_out    /* ppc64-linux only: did we deref an
                                  .opd entry? */ 
     )
{
   Bool plausible, is_in_opd;

   /* Set defaults */
   *sym_name_out   = sym_name;
   *sym_addr_out   = sym_addr;
   *sym_size_out   = (Int)sym->st_size;
   *sym_tocptr_out = 0; /* unknown/inapplicable */
   *from_opd_out   = False;

   /* Figure out if we're interested in the symbol.  Firstly, is it of
      the right flavour?  */
   plausible 
      = (ELFXX_ST_BIND(sym->st_info) == STB_GLOBAL 
         || ELFXX_ST_BIND(sym->st_info) == STB_LOCAL 
         || ELFXX_ST_BIND(sym->st_info) == STB_WEAK
        )
        &&
        (ELFXX_ST_TYPE(sym->st_info) == STT_FUNC 
         || (VG_(needs).data_syms 
             && ELFXX_ST_TYPE(sym->st_info) == STT_OBJECT)
        );

#  if defined(VGP_ppc64_linux)
   /* Allow STT_NOTYPE in the very special case where we're running on
      ppc64-linux and the symbol is one which the .opd-chasing hack
      below will chase. */
   if (!plausible
       && ELFXX_ST_TYPE(sym->st_info) == STT_NOTYPE
       && sym->st_size > 0
       && si->opd_start_avma != 0
       && sym_addr >= si->opd_start_avma
       && sym_addr <  si->opd_start_avma + si->opd_size)
      plausible = True;
#  endif

   if (!plausible)
      return False;

   /* Ignore if nameless, or zero-sized. */
   if (sym->st_name == (ElfXX_Word)NULL
       || /* VG_(strlen)(sym_name) == 0 */
          /* equivalent but cheaper ... */
          sym_name[0] == 0
       || sym->st_size == 0) {
      TRACE_SYMTAB("    ignore -- size=0: %s\n", sym_name);
      return False;
   }

   /* This seems to significantly reduce the number of junk
      symbols, and particularly reduces the number of
      overlapping address ranges.  Don't ask me why ... */
   if ((Int)sym->st_value == 0) {
      TRACE_SYMTAB( "    ignore -- valu=0: %s\n", sym_name);
      return False;
   }

   /* If it's apparently in a GOT or PLT, it's really a reference to a
      symbol defined elsewhere, so ignore it. */
   if (si->got_start_avma != 0
       && sym_addr >= si->got_start_avma 
       && sym_addr <  si->got_start_avma + si->got_size) {
      TRACE_SYMTAB("    ignore -- in GOT: %s\n", sym_name);
      return False;
   }
   if (si->plt_start_avma != 0
       && sym_addr >= si->plt_start_avma
       && sym_addr <  si->plt_start_avma + si->plt_size) {
      TRACE_SYMTAB("    ignore -- in PLT: %s\n", sym_name);
      return False;
   }

   /* ppc64-linux nasty hack: if the symbol is in an .opd section,
      then really what we have is the address of a function
      descriptor.  So use the first word of that as the function's
      text.

      See thread starting at
      http://gcc.gnu.org/ml/gcc-patches/2004-08/msg00557.html
   */
   is_in_opd = False;

   if (si->opd_start_avma != 0
       && sym_addr >= si->opd_start_avma
       && sym_addr <  si->opd_start_avma + si->opd_size) {
#     if !defined(VGP_ppc64_linux)
      TRACE_SYMTAB("    ignore -- in OPD: %s\n", sym_name);
      return False;
#     else
      Int    offset_in_opd;
      ULong* fn_descr;

      if (0) VG_(printf)("opdXXX: opd_offset %p, sym_addr %p\n", 
                         (void*)(opd_offset), (void*)sym_addr);

      if (!VG_IS_8_ALIGNED(sym_addr)) {
         TRACE_SYMTAB("    ignore -- not 8-aligned: %s\n", sym_name);
         return False;
      }

      /* sym_addr is a vma pointing into the .opd section.  We know
         the vma of the opd section start, so we can figure out how
         far into the opd section this is. */

      offset_in_opd = (Addr)sym_addr - (Addr)(si->opd_start_avma);
      if (offset_in_opd < 0 || offset_in_opd >= si->opd_size) {
         TRACE_SYMTAB("    ignore -- invalid OPD offset: %s\n", sym_name);
         return False;
      }

      /* Now we want to know what's at that offset in the .opd
         section.  We can't look in the running image since it won't
         necessarily have been mapped.  But we can consult the oimage.
         opd_filea is the start address of the .opd in the oimage.
         Hence: */

      fn_descr = (ULong*)(opd_filea + offset_in_opd);

      if (0) VG_(printf)("opdXXY: offset %d,  fn_descr %p\n", 
                         offset_in_opd, fn_descr);
      if (0) VG_(printf)("opdXXZ: *fn_descr %p\n", (void*)(fn_descr[0]));

      /* opd_offset is the difference between si->start (where the
         library got mapped) and the address space used for addresses
         within the library file. */

      sym_addr        = fn_descr[0] + opd_offset;
      *sym_addr_out   = sym_addr;
      *sym_tocptr_out = fn_descr[1] + opd_offset;
      *from_opd_out   = True;
      is_in_opd = True;

      /* Do a final sanity check: if the symbol falls outside the
         SegInfo's mapped range, ignore it.  Since sym_addr has been
         updated, that can be achieved simply by falling through to
         the test below. */

#     endif /* ppc64-linux nasty hack */
   }

   /* Here's yet another ppc64-linux hack.  Get rid of leading dot if
      the symbol is outside .opd. */
#  if defined(VGP_ppc64_linux)
   if (si->opd_start_avma != 0
       && !is_in_opd
       && sym_name[0] == '.') {
      vg_assert(!(*from_opd_out));
      *sym_name_out = &sym_name[1];
   }
#  endif

   /* If no part of the symbol falls within the mapped range,
      ignore it. */
   if (*sym_addr_out + *sym_size_out <= si->text_start_avma
       || *sym_addr_out >= si->text_start_avma + si->text_size) {
      TRACE_SYMTAB( "ignore -- %p .. %p outside mapped range %p .. %p\n",
                    *sym_addr_out, *sym_addr_out + *sym_size_out,
                    si->text_start_avma,
                    si->text_start_avma + si->text_size);
      return False;
   }

#  if defined(VGP_ppc64_linux)
   /* It's crucial that we never add symbol addresses in the .opd
      section.  This would completely mess up function redirection and
      intercepting.  This assert ensures that any symbols that make it
      into the symbol table on ppc64-linux don't point into .opd. */
   if (si->opd_start_avma != 0) {
      vg_assert(*sym_addr_out + *sym_size_out <= si->opd_start_avma
                || *sym_addr_out >= si->opd_start_avma + si->opd_size);
   }
#  endif

   /* Acquire! */
   return True;
}


/* Read an ELF symbol table (normal or dynamic).  This one is for the
   "normal" case ({x86,amd64,ppc32}-linux). */
static
__attribute__((unused)) /* not referred to on all targets */
void read_elf_symtab__normal( 
        struct _SegInfo* si, UChar* tab_name,
        ElfXX_Sym* o_symtab, UInt o_symtab_sz, OffT o_symtab_offset,
        UChar*     o_strtab, UInt o_strtab_sz,
        UChar*     opd_filea, OffT opd_offset /* ppc64-linux only */ 
     )
{
   Int        i;
   Addr       sym_addr, sym_addr_really;
   Char      *sym_name, *sym_name_really;
   Int        sym_size;
   Addr       sym_tocptr;
   Bool       from_opd;
   DiSym      risym;
   ElfXX_Sym *sym;

   if (o_strtab == NULL || o_symtab == NULL) {
      Char buf[80];
      vg_assert(VG_(strlen)(tab_name) < 40);
      VG_(sprintf)(buf, "   object doesn't have a %s", tab_name);
      ML_(symerr)(buf);
      return;
   }

   TRACE_SYMTAB("\nReading (ELF, standard) %s (%d entries)\n", tab_name, 
                o_symtab_sz/sizeof(ElfXX_Sym) );

   /* Perhaps should start at i = 1; ELF docs suggest that entry
      0 always denotes 'unknown symbol'. */
   for (i = 1; i < (Int)(o_symtab_sz/sizeof(ElfXX_Sym)); i++) {
      sym      = & o_symtab[i];
      sym_name = (Char*)(o_strtab + sym->st_name);
      sym_addr = o_symtab_offset + sym->st_value;

      if (si->trace_symtab)
         show_raw_elf_symbol(i, sym, sym_name, sym_addr, False);

      if (get_elf_symbol_info(si, sym, sym_name, sym_addr,
                              opd_filea, opd_offset,
                              &sym_name_really, 
                              &sym_addr_really,
                              &sym_size,
                              &sym_tocptr,
                              &from_opd)) {

         risym.addr   = sym_addr_really;
         risym.size   = sym_size;
         risym.name   = ML_(addStr) ( si, sym_name_really, -1 );
         risym.tocptr = sym_tocptr;
         vg_assert(risym.name != NULL);
         vg_assert(risym.tocptr == 0); /* has no role except on ppc64-linux */
         ML_(addSym) ( si, &risym );

         if (si->trace_symtab) {
            VG_(printf)("    record [%4d]:          "
                        " val %010p, sz %4d  %s\n",
                        i, (void*)risym.addr, (Int)risym.size, 
                           (HChar*)risym.name
            );
         }

      }
   }
}


/* Read an ELF symbol table (normal or dynamic).  This one is for
   ppc64-linux, which requires special treatment. */

typedef
   struct { 
      Addr   addr; 
      UChar* name; 
   }
   TempSymKey;

typedef
   struct {
      TempSymKey key;
      Addr       tocptr;
      Int        size;
      Bool       from_opd;
   }
   TempSym;

static Word cmp_TempSymKey ( TempSymKey* key1, TempSym* elem2 ) {
   if (key1->addr < elem2->key.addr) return -1;
   if (key1->addr > elem2->key.addr) return 1;
   return (Word)VG_(strcmp)(key1->name, elem2->key.name);
}
static void* oset_malloc ( SizeT szB ) { 
   return VG_(arena_malloc)(VG_AR_SYMTAB, szB);
}
static void oset_free ( void* p ) {
   VG_(arena_free)(VG_AR_SYMTAB, p);
}

static
__attribute__((unused)) /* not referred to on all targets */
void read_elf_symtab__ppc64_linux( 
        struct _SegInfo* si, UChar* tab_name,
        ElfXX_Sym* o_symtab, UInt o_symtab_sz, OffT o_symtab_offset,
        UChar*     o_strtab, UInt o_strtab_sz,
        UChar*     opd_filea, OffT opd_offset /* ppc64-linux only */ 
     )
{
   Int         i, old_size;
   Addr        sym_addr, sym_addr_really;
   Char       *sym_name, *sym_name_really;
   Int         sym_size;
   Addr        sym_tocptr, old_tocptr;
   Bool        from_opd, modify_size, modify_tocptr;
   DiSym       risym;
   ElfXX_Sym  *sym;
   OSet       *oset;
   TempSymKey  key;
   TempSym    *elem;
   TempSym    *prev;

   if (o_strtab == NULL || o_symtab == NULL) {
      Char buf[80];
      vg_assert(VG_(strlen)(tab_name) < 40);
      VG_(sprintf)(buf, "   object doesn't have a %s", tab_name);
      ML_(symerr)(buf);
      return;
   }

   TRACE_SYMTAB("\nReading (ELF, ppc64-linux) %s (%d entries)\n", tab_name, 
                o_symtab_sz/sizeof(ElfXX_Sym) );

   oset = VG_(OSet_Create)( offsetof(TempSym,key), 
                            (OSetCmp_t)cmp_TempSymKey, 
                            oset_malloc, oset_free );
   vg_assert(oset);

   /* Perhaps should start at i = 1; ELF docs suggest that entry
      0 always denotes 'unknown symbol'. */
   for (i = 1; i < (Int)(o_symtab_sz/sizeof(ElfXX_Sym)); i++) {
      sym      = & o_symtab[i];
      sym_name = (Char*)(o_strtab + sym->st_name);
      sym_addr = o_symtab_offset + sym->st_value;

      if (si->trace_symtab)
         show_raw_elf_symbol(i, sym, sym_name, sym_addr, True);

      if (get_elf_symbol_info(si, sym, sym_name, sym_addr,
                              opd_filea, opd_offset,
                              &sym_name_really, 
                              &sym_addr_really,
                              &sym_size,
                              &sym_tocptr,
                              &from_opd)) {

         /* Check if we've seen this (name,addr) key before. */
         key.addr = sym_addr_really;
         key.name = sym_name_really;
         prev = VG_(OSet_Lookup)( oset, &key );

         if (prev) {

            /* Seen it before.  Fold in whatever new info we can. */
            modify_size   = False;
            modify_tocptr = False;
            old_size   = 0;
	    old_tocptr = 0;

            if (prev->from_opd && !from_opd 
                && (prev->size == 24 || prev->size == 16)
                && sym_size != prev->size) {
               /* Existing one is an opd-redirect, with a bogus size,
                  so the only useful new fact we have is the real size
                  of the symbol. */
               modify_size = True;
               old_size = prev->size;
               prev->size = sym_size;
            }
            else
            if (!prev->from_opd && from_opd
                && (sym_size == 24 || sym_size == 16)) {
               /* Existing one is non-opd, new one is opd.  What we
                  can acquire from the new one is the TOC ptr to be
                  used.  Since the existing sym is non-toc, it
                  shouldn't currently have an known TOC ptr. */
               vg_assert(prev->tocptr == 0);
               modify_tocptr = True;
               old_tocptr = prev->tocptr;
               prev->tocptr = sym_tocptr;
            }
            else {
               /* ignore. can we do better here? */
            }

            /* Only one or the other is possible (I think) */
	    vg_assert(!(modify_size && modify_tocptr));

            if (modify_size && si->trace_symtab) {
               VG_(printf)("    modify (old sz %4d)    "
                           " val %010p, toc %010p, sz %4d  %s\n",
                           old_size,
                           (void*) prev->key.addr, 
                           (void*) prev->tocptr,
                           (Int)   prev->size, 
                           (HChar*)prev->key.name
               );
            }
            if (modify_tocptr && si->trace_symtab) {
               VG_(printf)("    modify (upd tocptr)     "
                           " val %010p, toc %010p, sz %4d  %s\n",
                            (void*) prev->key.addr, 
                            (void*) prev->tocptr, 
                            (Int)   prev->size, 
                            (HChar*)prev->key.name
               );
            }

         } else {

            /* A new (name,addr) key.  Add and continue. */
            elem = VG_(OSet_AllocNode)(oset, sizeof(TempSym));
            vg_assert(elem);
            elem->key      = key;
            elem->tocptr   = sym_tocptr;
            elem->size     = sym_size;
            elem->from_opd = from_opd;
            VG_(OSet_Insert)(oset, elem);
            if (si->trace_symtab) {
               VG_(printf)("   to-oset [%4d]:          "
                           " val %010p, toc %010p, sz %4d  %s\n",
                           i, (void*) elem->key.addr,
                              (void*) elem->tocptr,
                              (Int)   elem->size, 
                              (HChar*)elem->key.name
               );
            }

         }
      }
   }

   /* All the syms that matter are in the oset.  Now pull them out,
      build a "standard" symbol table, and nuke the oset. */

   i = 0;
   VG_(OSet_ResetIter)( oset );

   while ( (elem = VG_(OSet_Next)(oset)) ) {
      risym.addr   = elem->key.addr;
      risym.size   = elem->size;
      risym.name   = ML_(addStr) ( si, elem->key.name, -1 );
      risym.tocptr = elem->tocptr;
      vg_assert(risym.name != NULL);

      ML_(addSym) ( si, &risym );
      if (si->trace_symtab) {
         VG_(printf)("    record [%4d]:          "
                     " val %010p, toc %010p, sz %4d  %s\n",
                     i, (void*) risym.addr,
                        (void*) risym.tocptr,
                        (Int)   risym.size, 
                        (HChar*)risym.name
               );
      }
      i++;
   }

   VG_(OSet_Destroy)( oset, NULL );
}


/*
 * This routine for calculating the CRC for a separate debug file
 * is GPLed code borrowed from GNU binutils.
 */
static UInt
calc_gnu_debuglink_crc32(UInt crc, const UChar *buf, Int len)
{
  static const UInt crc32_table[256] =
    {
      0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419,
      0x706af48f, 0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4,
      0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07,
      0x90bf1d91, 0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
      0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856,
      0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
      0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4,
      0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
      0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3,
      0x45df5c75, 0xdcd60dcf, 0xabd13d59, 0x26d930ac, 0x51de003a,
      0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599,
      0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
      0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190,
      0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f,
      0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0x0f00f934, 0x9609a88e,
      0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
      0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed,
      0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
      0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3,
      0xfbd44c65, 0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
      0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a,
      0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5,
      0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa, 0xbe0b1010,
      0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
      0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17,
      0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6,
      0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615,
      0x73dc1683, 0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
      0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1, 0xf00f9344,
      0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
      0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a,
      0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
      0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1,
      0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c,
      0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef,
      0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
      0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe,
      0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31,
      0x2cd99e8b, 0x5bdeae1d, 0x9b64c2b0, 0xec63f226, 0x756aa39c,
      0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
      0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b,
      0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
      0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1,
      0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
      0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45, 0xa00ae278,
      0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7,
      0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66,
      0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
      0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605,
      0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8,
      0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b,
      0x2d02ef8d
    };
  const UChar *end;

  crc = ~crc & 0xffffffff;
  for (end = buf + len; buf < end; ++ buf)
    crc = crc32_table[(crc ^ *buf) & 0xff] ^ (crc >> 8);
  return ~crc & 0xffffffff;;
}

/*
 * Try and open a separate debug file, ignoring any where the CRC does
 * not match the value from the main object file.
 */
static
Addr open_debug_file( Char* name, UInt crc, UInt* size )
{
   SysRes fd, sres;
   struct vki_stat stat_buf;
   UInt calccrc;

   fd = VG_(open)(name, VKI_O_RDONLY, 0);
   if (fd.isError)
      return 0;

   if (VG_(fstat)(fd.res, &stat_buf) != 0) {
      VG_(close)(fd.res);
      return 0;
   }

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, "Reading debug info from %s...", name);

   *size = stat_buf.st_size;
   
   sres = VG_(am_mmap_file_float_valgrind)
             ( *size, VKI_PROT_READ, fd.res, 0 );

   VG_(close)(fd.res);
   
   if (sres.isError)
      return 0;

   calccrc = calc_gnu_debuglink_crc32(0, (UChar*)sres.res, *size);
   if (calccrc != crc) {
      SysRes res = VG_(am_munmap_valgrind)(sres.res, *size);
      vg_assert(!res.isError);
      if (VG_(clo_verbosity) > 1)
	 VG_(message)(Vg_DebugMsg, "... CRC mismatch (computed %08x wanted %08x)", calccrc, crc);
      return 0;
   }
   
   return sres.res;
}

/*
 * Try to find a separate debug file for a given object file.
 */
static
Addr find_debug_file( Char* objpath, Char* debugname, UInt crc, UInt* size )
{
   Char *objdir = VG_(arena_strdup)(VG_AR_SYMTAB, objpath);
   Char *objdirptr;
   Char *debugpath;
   Addr addr = 0;
  
   if ((objdirptr = VG_(strrchr)(objdir, '/')) != NULL)
      *objdirptr = '\0';

   debugpath = VG_(arena_malloc)(VG_AR_SYMTAB, VG_(strlen)(objdir) + VG_(strlen)(debugname) + 16);
   
   VG_(sprintf)(debugpath, "%s/%s", objdir, debugname);

   if ((addr = open_debug_file(debugpath, crc, size)) == 0) {
      VG_(sprintf)(debugpath, "%s/.debug/%s", objdir, debugname);
      if ((addr = open_debug_file(debugpath, crc, size)) == 0) {
         VG_(sprintf)(debugpath, "/usr/lib/debug%s/%s", objdir, debugname);
         addr = open_debug_file(debugpath, crc, size);
      }
   }

   VG_(arena_free)(VG_AR_SYMTAB, debugpath);
   VG_(arena_free)(VG_AR_SYMTAB, objdir);
   
   return addr;
}


/* The central function for reading ELF debug info.  For the
   object/exe specified by the SegInfo, find ELF sections, then read
   the symbols, line number info, file name info, CFA (stack-unwind
   info) and anything else we want, into the tables within the
   supplied SegInfo.
*/
Bool ML_(read_elf_debug_info) ( struct _SegInfo* si )
{
   Bool          res;
   ElfXX_Ehdr*   ehdr;       /* The ELF header                   */
   ElfXX_Shdr*   shdr;       /* The section table                */
   UChar*        sh_strtab;  /* The section table's string table */
   SysRes        fd, sres;
   Int           i;
   Bool          ok;
   Addr          oimage;
   UInt          n_oimage;
   OffT          offset_oimage = 0;
   Addr          dimage = 0;
   UInt          n_dimage = 0;
   OffT          offset_dimage = 0;

   oimage = (Addr)NULL;
   if (VG_(clo_verbosity) > 1 || VG_(clo_trace_redir))
      VG_(message)(Vg_DebugMsg, "Reading syms from %s (%p)", 
                                si->filename, si->text_start_avma );

   /* mmap the object image aboard, so that we can read symbols and
      line number info out of it.  It will be munmapped immediately
      thereafter; it is only aboard transiently. */

   fd = VG_(open)(si->filename, VKI_O_RDONLY, 0);
   if (fd.isError) {
      ML_(symerr)("Can't open .so/.exe to read symbols?!");
      return False;
   }

   n_oimage = VG_(fsize)(fd.res);
   if (n_oimage < 0) {
      ML_(symerr)("Can't stat .so/.exe (to determine its size)?!");
      VG_(close)(fd.res);
      return False;
   }

   sres = VG_(am_mmap_file_float_valgrind)
             ( n_oimage, VKI_PROT_READ, fd.res, 0 );

   VG_(close)(fd.res);

   if (sres.isError) {
      VG_(message)(Vg_UserMsg, "warning: mmap failed on %s", si->filename );
      VG_(message)(Vg_UserMsg, "         no symbols or debug info loaded" );
      return False;
   }

   oimage = sres.res;

   if (0) {
      VG_(printf)("read_elf_debug_info: OIMAGE = %p - %p\n", 
                  (void*)oimage, (void*)(oimage + (UWord)n_oimage));
   }

   /* Ok, the object image is safely in oimage[0 .. n_oimage-1]. 
      Now verify that it is a valid ELF .so or executable image.
   */
   res = False;
   ok = (n_oimage >= sizeof(ElfXX_Ehdr));
   ehdr = (ElfXX_Ehdr*)oimage;

   if (ok)
      ok &= ML_(is_elf_object_file)(ehdr);

   if (!ok) {
      ML_(symerr)("Invalid ELF header, or missing stringtab/sectiontab.");
      goto out;
   }

   /* Walk the LOAD headers in the phdr and update the SegInfo to
      include them all, so that this segment also contains data and
      bss memory.  Also computes correct symbol offset value for this
      ELF file. */
   if (ehdr->e_phoff + ehdr->e_phnum*sizeof(ElfXX_Phdr) > n_oimage) {
      ML_(symerr)("ELF program header is beyond image end?!");
      goto out;
   }
   {
      Bool offset_set = False;
      ElfXX_Addr prev_addr = 0;
      Addr baseaddr = 0;

      vg_assert(si->soname == NULL);

      for (i = 0; i < ehdr->e_phnum; i++) {
	 ElfXX_Phdr *o_phdr;
	 ElfXX_Addr mapped, mapped_end;

	 o_phdr = &((ElfXX_Phdr *)(oimage + ehdr->e_phoff))[i];

         /* Try to get the soname.  If there isn't one, use "NONE".
            The seginfo needs to have some kind of soname in order to
            facilitate writing redirect functions, since all redirect
            specifications require a soname (pattern). */
	 if (o_phdr->p_type == PT_DYNAMIC && si->soname == NULL) {
	    const ElfXX_Dyn *dyn = (const ElfXX_Dyn *)(oimage + o_phdr->p_offset);
	    Int stroff = -1;
	    Char *strtab = NULL;
	    Int j;
	    
	    for(j = 0; dyn[j].d_tag != DT_NULL; j++) {
	       switch(dyn[j].d_tag) {
	       case DT_SONAME:
		  stroff = dyn[j].d_un.d_val;
		  break;

	       case DT_STRTAB:
		  strtab = (Char *)oimage + dyn[j].d_un.d_ptr - baseaddr;
		  break;
	       }
	    }

	    if (stroff != -1 && strtab != 0) {
	       TRACE_SYMTAB("soname=%s\n", strtab+stroff);
	       si->soname = VG_(arena_strdup)(VG_AR_SYMTAB, strtab+stroff);
	    }
	 }

	 if (o_phdr->p_type != PT_LOAD)
	    continue;

	 if (!offset_set) {
	    offset_set = True;
	    offset_oimage = si->text_start_avma - o_phdr->p_vaddr;
	    baseaddr = o_phdr->p_vaddr;
	 }

         // Make sure the Phdrs are in order
	 if (o_phdr->p_vaddr < prev_addr) {
	    ML_(symerr)("ELF Phdrs are out of order!?");
            goto out;
	 }
	 prev_addr = o_phdr->p_vaddr;

         // Get the data and bss start/size if appropriate
	 mapped = o_phdr->p_vaddr + offset_oimage;
	 mapped_end = mapped + o_phdr->p_memsz;
	 if (si->data_start_avma == 0 &&
	     (o_phdr->p_flags & (PF_R|PF_W|PF_X)) == (PF_R|PF_W)) {
	    si->data_start_avma = mapped;
	    si->data_size       = o_phdr->p_filesz;
	    si->bss_start_avma  = mapped + o_phdr->p_filesz;
	    if (o_phdr->p_memsz > o_phdr->p_filesz)
	       si->bss_size = o_phdr->p_memsz - o_phdr->p_filesz;
	    else
	       si->bss_size = 0;
	 }

         mapped = mapped & ~(VKI_PAGE_SIZE-1);
	 mapped_end = (mapped_end + VKI_PAGE_SIZE - 1) & ~(VKI_PAGE_SIZE-1);

	 if (VG_(needs).data_syms 
             && mapped >= si->text_start_avma 
             && mapped <= (si->text_start_avma + si->text_size)
             && mapped_end > (si->text_start_avma + si->text_size)) {
            /* XXX jrs 2007 Jan 11: what's going on here?  If data
               syms are involved, surely we shouldn't be messing with
               the segment's text_size unless there is an assumption
               that the data segment has been mapped immediately after
               the text segment.  Which doesn't sound good to me. */
	    UInt newsz = mapped_end - si->text_start_avma;
	    if (newsz > si->text_size) {
	       if (0)
		  VG_(printf)("extending mapping %p..%p %d -> ..%p %d\n", 
			      si->text_start_avma, 
                              si->text_start_avma + si->text_size, 
                              si->text_size,
			      si->text_start_avma + newsz, newsz);

	       si->text_size = newsz;
	    }
	 }
      }
   }

   si->text_bias = offset_oimage;

   if (VG_(clo_verbosity) > 2 || VG_(clo_trace_redir))
      VG_(message)(Vg_DebugMsg, "   svma %010p, avma %010p", 
                                si->text_start_avma - si->text_bias,
                                si->text_start_avma );

   /* If, after looking at all the program headers, we still didn't 
      find a soname, add a fake one. */
   if (si->soname == NULL) {
      TRACE_SYMTAB("soname(fake)=\"NONE\"\n");
      si->soname = "NONE";
   }

   TRACE_SYMTAB("shoff = %d,  shnum = %d,  size = %d,  n_vg_oimage = %d\n",
                ehdr->e_shoff, ehdr->e_shnum, sizeof(ElfXX_Shdr), n_oimage );

   if (ehdr->e_shoff + ehdr->e_shnum*sizeof(ElfXX_Shdr) > n_oimage) {
      ML_(symerr)("ELF section header is beyond image end?!");
      goto out;
   }

   shdr = (ElfXX_Shdr*)(oimage + ehdr->e_shoff);
   sh_strtab = (UChar*)(oimage + shdr[ehdr->e_shstrndx].sh_offset);

   /* Find interesting sections, read the symbol table(s), read any debug
      information */
   {
      /* IMAGE addresses: pointers to start of sections (in the
         oimage, not in the running image) -- image addresses */
      UChar*     strtab_img      = NULL; /* .strtab */
      ElfXX_Sym* symtab_img      = NULL; /* .symtab */
      UChar*     dynstr_img      = NULL; /* .dynstr */
      ElfXX_Sym* dynsym_img      = NULL; /* .dynsym */
      Char*      debuglink_img   = NULL; /* .gnu_debuglink */
      UChar*     stab_img        = NULL; /* .stab         (stabs)  */
      UChar*     stabstr_img     = NULL; /* .stabstr      (stabs)  */
      UChar*     debug_line_img  = NULL; /* .debug_line   (dwarf2) */
      UChar*     debug_info_img  = NULL; /* .debug_info   (dwarf2) */
      UChar*     debug_abbv_img  = NULL; /* .debug_abbrev (dwarf2) */
      UChar*     debug_str_img   = NULL; /* .debug_str    (dwarf2) */
      UChar*     dwarf1d_img     = NULL; /* .debug        (dwarf1) */
      UChar*     dwarf1l_img     = NULL; /* .line         (dwarf1) */
      UChar*     ehframe_img     = NULL; /* .eh_frame     (dwarf2) */
      UChar*     opd_filea_img   = NULL; /* .opd          (dwarf2, ppc64-linux) */
      UChar*     dummy_filea_img = NULL;

      OffT       symtab_offset   = offset_oimage;
      OffT       dynsym_offset   = offset_oimage;
      OffT       debug_offset    = offset_oimage;
      OffT       opd_offset      = offset_oimage;

      /* Section sizes, in bytes */
      UInt       strtab_sz       = 0;
      UInt       symtab_sz       = 0;
      UInt       dynstr_sz       = 0;
      UInt       dynsym_sz       = 0;
      UInt       debuglink_sz    = 0;
      UInt       stab_sz         = 0;
      UInt       stabstr_sz      = 0;
      UInt       debug_line_sz   = 0;
      UInt       debug_info_sz   = 0;
      UInt       debug_abbv_sz   = 0;
      UInt       debug_str_sz    = 0;
      UInt       dwarf1d_sz      = 0;
      UInt       dwarf1l_sz      = 0;
      UInt       ehframe_sz      = 0;

      /* Section actual virtual addresses */
      Addr       dummy_avma      = 0;
      Addr       ehframe_avma    = 0;

      /* Find all interesting sections */

      /* What FIND does: it finds the section called SEC_NAME.  The
	 size of it is assigned to SEC_SIZE.  The address that it will
	 appear in the running image is assigned to SEC_VMA (note,
	 this will be meaningless for sections which are not marked
	 loadable.  Even for sections which are marked loadable, the
	 client's ld.so may not have loaded them yet, so there is no
	 guarantee that we can safely prod around in any such area)
	 The address of the section in the transiently loaded oimage
	 is assigned to SEC_FILEA.  Because the entire object file is
	 transiently mapped aboard for inspection, it's always safe to
	 inspect that area. */

      for (i = 0; i < ehdr->e_shnum; i++) {

#        define FIND(sec_name, sec_size, sec_filea, sec_vma) \
         if (0 == VG_(strcmp)(sec_name, sh_strtab + shdr[i].sh_name)) { \
            Bool nobits; \
            sec_vma   = (Addr)(offset_oimage + shdr[i].sh_addr); \
            sec_filea = (void*)(oimage + shdr[i].sh_offset); \
            sec_size  = shdr[i].sh_size; \
            nobits = shdr[i].sh_type == SHT_NOBITS; \
            TRACE_SYMTAB( "%18s: filea %p .. %p, vma %p .. %p\n", \
                          sec_name, (UChar*)sec_filea, \
                                    ((UChar*)sec_filea) + sec_size - 1, \
                          sec_vma, sec_vma + sec_size - 1); \
            /* SHT_NOBITS sections have zero size in the file. */ \
            if ( shdr[i].sh_offset + (nobits ? 0 : sec_size) > n_oimage ) { \
               ML_(symerr)("   section beyond image end?!"); \
               goto out; \
            } \
         }

         /* Nb: must find where .got and .plt sections will be in the
          * executable image, not in the object image transiently loaded. */
         /*   NAME              SIZE           IMAGE addr       AVMA */
         FIND(".dynsym",        dynsym_sz,     dynsym_img,      dummy_avma)
         FIND(".dynstr",        dynstr_sz,     dynstr_img,      dummy_avma)
         FIND(".symtab",        symtab_sz,     symtab_img,      dummy_avma)
         FIND(".strtab",        strtab_sz,     strtab_img,      dummy_avma)

         FIND(".gnu_debuglink", debuglink_sz,  debuglink_img,   dummy_avma)

         FIND(".stab",          stab_sz,       stab_img,        dummy_avma)
         FIND(".stabstr",       stabstr_sz,    stabstr_img,     dummy_avma)

         FIND(".debug_line",    debug_line_sz, debug_line_img,  dummy_avma)
         FIND(".debug_info",    debug_info_sz, debug_info_img,  dummy_avma)
         FIND(".debug_abbrev",  debug_abbv_sz, debug_abbv_img,  dummy_avma)
         FIND(".debug_str",     debug_str_sz,  debug_str_img,   dummy_avma)

         FIND(".debug",         dwarf1d_sz,    dwarf1d_img,     dummy_avma)
         FIND(".line",          dwarf1l_sz,    dwarf1l_img,     dummy_avma)
         FIND(".eh_frame",      ehframe_sz,    ehframe_img,     ehframe_avma)

         FIND(".got",           si->got_size,  dummy_filea_img, si->got_start_avma)
         FIND(".plt",           si->plt_size,  dummy_filea_img, si->plt_start_avma)
         FIND(".opd",           si->opd_size,  opd_filea_img,   si->opd_start_avma)

#        undef FIND
      }
         
      /* Did we find a debuglink section? */
      if (debuglink_img != NULL) {
         UInt crc_offset = VG_ROUNDUP(VG_(strlen)(debuglink_img)+1, 4);
         UInt crc;

         vg_assert(crc_offset + sizeof(UInt) <= debuglink_sz);

         /* Extract the CRC from the debuglink section */
         crc = *(UInt *)(debuglink_img + crc_offset);

         /* See if we can find a matching debug file */
         dimage = find_debug_file(si->filename, debuglink_img, crc, &n_dimage);
         if (dimage != 0) {
            ehdr = (ElfXX_Ehdr*)dimage;

            if (n_dimage >= sizeof(ElfXX_Ehdr) 
                && ML_(is_elf_object_file(ehdr))
                && ehdr->e_phoff + ehdr->e_phnum*sizeof(ElfXX_Phdr) <= n_dimage
                && ehdr->e_shoff + ehdr->e_shnum*sizeof(ElfXX_Shdr) <= n_dimage)
            {
               Bool need_symtab = (NULL == symtab_img);

               for (i = 0; i < ehdr->e_phnum; i++) {
                  ElfXX_Phdr *o_phdr = &((ElfXX_Phdr *)(dimage + ehdr->e_phoff))[i];
                  if (o_phdr->p_type == PT_LOAD) {
                     offset_dimage = si->text_start_avma - o_phdr->p_vaddr;
                     break;
                  }
               }

               debug_offset = offset_dimage;
               if (need_symtab)
                  symtab_offset = offset_dimage;

               shdr = (ElfXX_Shdr*)(dimage + ehdr->e_shoff);
               sh_strtab = (UChar*)(dimage + shdr[ehdr->e_shstrndx].sh_offset);

               /* Same deal as previous FIND, except simpler - doesn't
                  look for avma, only oimage address. */

               /* Find all interesting sections */
               for (i = 0; i < ehdr->e_shnum; i++) {

#                 define FIND(condition, sec_name, sec_size, sec_filea)	\
                  if (condition \
                      && 0 == VG_(strcmp)(sec_name, sh_strtab + shdr[i].sh_name)) { \
                     Bool nobits; \
                     if (0 != sec_filea) \
                        VG_(core_panic)("repeated section!\n"); \
                     sec_filea = (void*)(dimage + shdr[i].sh_offset); \
                     sec_size  = shdr[i].sh_size; \
                     nobits = shdr[i].sh_type == SHT_NOBITS; \
                     TRACE_SYMTAB( "%18s: filea %p .. %p\n", \
                                   sec_name, (UChar*)sec_filea, \
                                             ((UChar*)sec_filea) + sec_size - 1); \
                     /* SHT_NOBITS sections have zero size in the file. */ \
                     if ( shdr[i].sh_offset + (nobits ? 0 : sec_size) > n_dimage ) { \
                        ML_(symerr)("   section beyond image end?!"); \
                        goto out; \
                     } \
                  }

                  /*   ??           NAME             SIZE           IMAGE addr */
                  FIND(need_symtab, ".symtab",       symtab_sz,     symtab_img)
                  FIND(need_symtab, ".strtab",       strtab_sz,     strtab_img)
                  FIND(1,           ".stab",         stab_sz,       stab_img)
                  FIND(1,           ".stabstr",      stabstr_sz,    stabstr_img)
                  FIND(1,           ".debug_line",   debug_line_sz, debug_line_img)
                  FIND(1,           ".debug_info",   debug_info_sz, debug_info_img)
                  FIND(1,           ".debug_abbrev", debug_abbv_sz, debug_abbv_img)
                  FIND(1,           ".debug_str",    debug_str_sz,  debug_str_img)
                  FIND(1,           ".debug",        dwarf1d_sz,    dwarf1d_img)
                  FIND(1,           ".line",         dwarf1l_sz,    dwarf1l_img)

#                 undef FIND
               }
            }
         }
      }

      /* Check some sizes */
      vg_assert((dynsym_sz % sizeof(ElfXX_Sym)) == 0);
      vg_assert((symtab_sz % sizeof(ElfXX_Sym)) == 0);

      /* Read symbols */
      {
         void (*read_elf_symtab)(struct _SegInfo*,UChar*,ElfXX_Sym*,
                                 UInt,OffT,UChar*,UInt,UChar*,OffT);
#        if defined(VGP_ppc64_linux)
         read_elf_symtab = read_elf_symtab__ppc64_linux;
#        else
         read_elf_symtab = read_elf_symtab__normal;
#        endif
         read_elf_symtab(si, "symbol table",
                         symtab_img, symtab_sz, symtab_offset,
                         strtab_img, strtab_sz, 
                         opd_filea_img, opd_offset);

         read_elf_symtab(si, "dynamic symbol table",
                         dynsym_img, dynsym_sz, dynsym_offset,
                         dynstr_img, dynstr_sz, 
                         opd_filea_img, opd_offset);
      }

      /* Read .eh_frame (call-frame-info) if any */
      if (ehframe_img) {
         ML_(read_callframe_info_dwarf3)
            ( si, ehframe_img, ehframe_sz, ehframe_avma );
      }

      /* Read the stabs and/or dwarf2 debug information, if any.  It
         appears reading stabs stuff on amd64-linux doesn't work, so
         we ignore it. */
#     if !defined(VGP_amd64_linux)
      if (stab_img && stabstr_img) {
         ML_(read_debuginfo_stabs) ( si, debug_offset, stab_img, stab_sz, 
                                         stabstr_img, stabstr_sz );
      }
#     endif
      /* jrs 2006-01-01: icc-8.1 has been observed to generate
         binaries without debug_str sections.  Don't preclude
         debuginfo reading for that reason, but, in
         read_unitinfo_dwarf2, do check that debugstr is non-NULL
         before using it. */
      if (debug_info_img && debug_abbv_img && debug_line_img
                                           /* && debug_str_img */) {
         ML_(read_debuginfo_dwarf2) ( si, debug_offset, 
                                      debug_info_img,   debug_info_sz,
                                      debug_abbv_img,
                                      debug_line_img,   debug_line_sz,
                                      debug_str_img );
      }
      if (dwarf1d_img && dwarf1l_img) {
         ML_(read_debuginfo_dwarf1) ( si, dwarf1d_img, dwarf1d_sz, 
                                          dwarf1l_img, dwarf1l_sz );
      }
   }
   res = True;

  out: {
   SysRes m_res;
   /* Last, but not least, heave the image(s) back overboard. */
   if (dimage) {
      m_res = VG_(am_munmap_valgrind) ( dimage, n_dimage );
      vg_assert(!m_res.isError);
   }
   m_res = VG_(am_munmap_valgrind) ( oimage, n_oimage );
   vg_assert(!m_res.isError);
   return res;
  } 
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
