
/*--------------------------------------------------------------------*/
/*--- Reading of syms & debug info from ELF .so/executable files.  ---*/
/*---                                                    readelf.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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

#if defined(VGO_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_debuginfo.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_aspacemgr.h"    /* for mmaping debuginfo files */
#include "pub_core_machine.h"      /* VG_ELF_CLASS */
#include "pub_core_options.h"
#include "pub_core_oset.h"
#include "pub_core_tooliface.h"    /* VG_(needs) */
#include "pub_core_xarray.h"
#include "priv_misc.h"             /* dinfo_zalloc/free/strdup */
#include "priv_d3basics.h"
#include "priv_tytypes.h"
#include "priv_storage.h"
#include "priv_readelf.h"          /* self */
#include "priv_readdwarf.h"        /* 'cos ELF contains DWARF */
#include "priv_readdwarf3.h"
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
#  define  ElfXX_Nhdr     Elf32_Nhdr
#  define  ElfXX_Sym      Elf32_Sym
#  define  ElfXX_Off      Elf32_Off
#  define  ElfXX_Word     Elf32_Word
#  define  ElfXX_Addr     Elf32_Addr
#  define  ElfXX_Dyn      Elf32_Dyn
#  define  ELFXX_ST_BIND  ELF32_ST_BIND
#  define  ELFXX_ST_TYPE  ELF32_ST_TYPE

#elif VG_WORDSIZE == 8
#  define  ElfXX_Ehdr     Elf64_Ehdr
#  define  ElfXX_Shdr     Elf64_Shdr
#  define  ElfXX_Phdr     Elf64_Phdr
#  define  ElfXX_Nhdr     Elf64_Nhdr
#  define  ElfXX_Sym      Elf64_Sym
#  define  ElfXX_Off      Elf64_Off
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

Bool ML_(is_elf_object_file)( void* image, SizeT n_image )
{
   ElfXX_Ehdr* ehdr = (ElfXX_Ehdr*)image;
   Int ok = 1;

   if (n_image < sizeof(ElfXX_Ehdr))
      return False;

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
                           ElfXX_Sym* sym, Char* sym_name, Addr sym_svma,
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
   VG_(printf)(": svma %#010lx, %ssz %4ld  %s\n",
               sym_svma, space, sym->st_size + 0UL,
               ( sym->st_name ? sym_name : (Char*)"NONAME" ) ); 
}               


/* Decide whether SYM is something we should collect, and if so, copy
   relevant info to the _OUT arguments.  For {x86,amd64,ppc32}-linux
   this is straightforward - the name, address, size are copied out
   unchanged.

   There is a bit of a kludge re data symbols (see KLUDGED BSS CHECK
   below): we assume that the .bss is mapped immediately after .data,
   and so accept any data symbol which exists in the range [start of
   .data, size of .data + size of .bss).  I don't know if this is
   really correct/justifiable, or not.

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
        struct _DebugInfo* di, /* containing DebugInfo */
        ElfXX_Sym* sym,        /* ELF symbol */
        Char*      sym_name,   /* name */
        Addr       sym_svma,   /* address as stated in the object file */
        Bool       symtab_in_debug, /* symbol table is in the debug file */
        UChar*     opd_img,    /* oimage of .opd sec (ppc64-linux only) */
        PtrdiffT   opd_bias,   /* for biasing AVMAs found in .opd */
        /* OUTPUTS */
        Char** sym_name_out,   /* name we should record */
        Addr*  sym_avma_out,   /* addr we should record */
        Int*   sym_size_out,   /* symbol size */
        Addr*  sym_tocptr_out, /* ppc64-linux only: R2 value to be
                                  used on entry */
        Bool*  from_opd_out,   /* ppc64-linux only: did we deref an
                                  .opd entry? */
        Bool*  is_text_out,    /* is this a text symbol? */
        Bool*  is_ifunc        /* is this a  STT_GNU_IFUNC function ?*/
     )
{
   Bool plausible;
#  if defined(VGP_ppc64_linux)
   Bool is_in_opd;
#  endif
   Bool in_text, in_data, in_sdata, in_rodata, in_bss, in_sbss;
   Addr text_svma, data_svma, sdata_svma, rodata_svma, bss_svma, sbss_svma;
   PtrdiffT text_bias, data_bias, sdata_bias, rodata_bias, bss_bias, sbss_bias;

   /* Set defaults */
   *sym_name_out   = sym_name;
   *sym_avma_out   = sym_svma; /* we will bias this shortly */
   *is_text_out    = True;
   *sym_size_out   = (Int)sym->st_size;
   *sym_tocptr_out = 0; /* unknown/inapplicable */
   *from_opd_out   = False;
   *is_ifunc       = False;

   /* Figure out if we're interested in the symbol.  Firstly, is it of
      the right flavour?  */
   plausible 
      = (ELFXX_ST_BIND(sym->st_info) == STB_GLOBAL 
         || ELFXX_ST_BIND(sym->st_info) == STB_LOCAL 
         || ELFXX_ST_BIND(sym->st_info) == STB_WEAK
        )
        &&
        (ELFXX_ST_TYPE(sym->st_info) == STT_FUNC 
         || ELFXX_ST_TYPE(sym->st_info) == STT_OBJECT
#ifdef STT_GNU_IFUNC
         || ELFXX_ST_TYPE(sym->st_info) == STT_GNU_IFUNC
#endif
        );

   /* Work out the svma and bias for each section as it will appear in
      addresses in the symbol table. */
   if (symtab_in_debug) {
      text_svma = di->text_debug_svma;
      text_bias = di->text_debug_bias;
      data_svma = di->data_debug_svma;
      data_bias = di->data_debug_bias;
      sdata_svma = di->sdata_debug_svma;
      sdata_bias = di->sdata_debug_bias;
      rodata_svma = di->rodata_debug_svma;
      rodata_bias = di->rodata_debug_bias;
      bss_svma = di->bss_debug_svma;
      bss_bias = di->bss_debug_bias;
      sbss_svma = di->sbss_debug_svma;
      sbss_bias = di->sbss_debug_bias;
   } else {
      text_svma = di->text_svma;
      text_bias = di->text_bias;
      data_svma = di->data_svma;
      data_bias = di->data_bias;
      sdata_svma = di->sdata_svma;
      sdata_bias = di->sdata_bias;
      rodata_svma = di->rodata_svma;
      rodata_bias = di->rodata_bias;
      bss_svma = di->bss_svma;
      bss_bias = di->bss_bias;
      sbss_svma = di->sbss_svma;
      sbss_bias = di->sbss_bias;
   }

   /* Now bias sym_avma_out accordingly by figuring out exactly which
      section the symbol is from and bias accordingly.  Screws up if
      the previously deduced section svma address ranges are wrong. */
   if (di->text_present
       && di->text_size > 0
       && sym_svma >= text_svma 
       && sym_svma < text_svma + di->text_size) {
      *is_text_out = True;
      *sym_avma_out += text_bias;
   } else
   if (di->data_present
       && di->data_size > 0
       && sym_svma >= data_svma 
       && sym_svma < data_svma + di->data_size) {
      *is_text_out = False;
      *sym_avma_out += data_bias;
   } else
   if (di->sdata_present
       && di->sdata_size > 0
       && sym_svma >= sdata_svma 
       && sym_svma < sdata_svma + di->sdata_size) {
      *is_text_out = False;
      *sym_avma_out += sdata_bias;
   } else
   if (di->rodata_present
       && di->rodata_size > 0
       && sym_svma >= rodata_svma 
       && sym_svma < rodata_svma + di->rodata_size) {
      *is_text_out = False;
      *sym_avma_out += rodata_bias;
   } else
   if (di->bss_present
       && di->bss_size > 0
       && sym_svma >= bss_svma 
       && sym_svma < bss_svma + di->bss_size) {
      *is_text_out = False;
      *sym_avma_out += bss_bias;
   } else
   if (di->sbss_present
       && di->sbss_size > 0
       && sym_svma >= sbss_svma 
       && sym_svma < sbss_svma + di->sbss_size) {
      *is_text_out = False;
      *sym_avma_out += sbss_bias;
   } else {
      /* Assume it's in .text.  Is this a good idea? */
      *is_text_out = True;
      *sym_avma_out += text_bias;
   }

#  ifdef STT_GNU_IFUNC
   /* Check for indirect functions. */
   if (*is_text_out
       && ELFXX_ST_TYPE(sym->st_info) == STT_GNU_IFUNC) {
       *is_ifunc = True;
   }
#  endif

#  if defined(VGP_ppc64_linux)
   /* Allow STT_NOTYPE in the very special case where we're running on
      ppc64-linux and the symbol is one which the .opd-chasing hack
      below will chase. */
   if (!plausible
       && *is_text_out
       && ELFXX_ST_TYPE(sym->st_info) == STT_NOTYPE
       && sym->st_size > 0
       && di->opd_present
       && di->opd_size > 0
       && *sym_avma_out >= di->opd_avma
       && *sym_avma_out <  di->opd_avma + di->opd_size)
      plausible = True;
#  endif

   if (!plausible)
      return False;

   /* Ignore if nameless, or zero-sized. */
   if (sym->st_name == (ElfXX_Word)0
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
   if (di->got_present
       && di->got_size > 0
       && *sym_avma_out >= di->got_avma 
       && *sym_avma_out <  di->got_avma + di->got_size) {
      TRACE_SYMTAB("    ignore -- in GOT: %s\n", sym_name);
      return False;
   }
   if (di->plt_present
       && di->plt_size > 0
       && *sym_avma_out >= di->plt_avma
       && *sym_avma_out <  di->plt_avma + di->plt_size) {
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
#  if defined(VGP_ppc64_linux)
   is_in_opd = False;
#  endif

   if (di->opd_present
       && di->opd_size > 0
       && *sym_avma_out >= di->opd_avma
       && *sym_avma_out <  di->opd_avma + di->opd_size) {
#     if !defined(VGP_ppc64_linux)
      TRACE_SYMTAB("    ignore -- in OPD: %s\n", sym_name);
      return False;
#     else
      Int    offset_in_opd;
      ULong* fn_descr;
      Bool   details = 1||False;

      if (details)
         TRACE_SYMTAB("opdXXX: opd_bias %p, sym_svma_out %p\n", 
                      (void*)(opd_bias), (void*)*sym_avma_out);

      if (!VG_IS_8_ALIGNED(*sym_avma_out)) {
         TRACE_SYMTAB("    ignore -- not 8-aligned: %s\n", sym_name);
         return False;
      }

      /* *sym_avma_out is a vma pointing into the .opd section.  We
         know the vma of the opd section start, so we can figure out
         how far into the opd section this is. */

      offset_in_opd = (Addr)(*sym_avma_out) - (Addr)(di->opd_avma);
      if (offset_in_opd < 0 || offset_in_opd >= di->opd_size) {
         TRACE_SYMTAB("    ignore -- invalid OPD offset: %s\n", sym_name);
         return False;
      }

      /* Now we want to know what's at that offset in the .opd
         section.  We can't look in the running image since it won't
         necessarily have been mapped.  But we can consult the oimage.
         opd_img is the start address of the .opd in the oimage.
         Hence: */

      fn_descr = (ULong*)(opd_img + offset_in_opd);

      if (details) 
         TRACE_SYMTAB("opdXXY: offset %d,  fn_descr %p\n", 
                      offset_in_opd, fn_descr);
      if (details) 
         TRACE_SYMTAB("opdXXZ: *fn_descr %p\n", (void*)(fn_descr[0]));

      /* opd_bias is the what we have to add to SVMAs found in .opd to
         get plausible .text AVMAs for the entry point, and .data
         AVMAs (presumably) for the TOC locations.  We use the caller
         supplied value (which is di->text_bias) for both of these.
         Not sure why that is correct - it seems to work, and sounds
         OK for fn_descr[0], but surely we need to use the data bias
         and not the text bias for fn_descr[1] ?  Oh Well.
      */
      *sym_avma_out   = fn_descr[0] + opd_bias;
      *sym_tocptr_out = fn_descr[1] + opd_bias;
      *from_opd_out   = True;
      is_in_opd = True;

      /* Do a final sanity check: if the symbol falls outside the
         DebugInfo's mapped range, ignore it.  Since *sym_avma_out has
         been updated, that can be achieved simply by falling through
         to the test below. */

#     endif /* ppc64-linux nasty hack */
   }

   /* Here's yet another ppc64-linux hack.  Get rid of leading dot if
      the symbol is outside .opd. */
#  if defined(VGP_ppc64_linux)
   if (di->opd_size > 0
       && !is_in_opd
       && sym_name[0] == '.') {
      vg_assert(!(*from_opd_out));
      *sym_name_out = &sym_name[1];
   }
#  endif

   /* If no part of the symbol falls within the mapped range,
      ignore it. */
   
   in_text 
      = di->text_present
        && di->text_size > 0
        && !(*sym_avma_out + *sym_size_out <= di->text_avma
             || *sym_avma_out >= di->text_avma + di->text_size);

   in_data 
      = di->data_present
        && di->data_size > 0
        && !(*sym_avma_out + *sym_size_out <= di->data_avma
             || *sym_avma_out >= di->data_avma + di->data_size);

   in_sdata 
      = di->sdata_present
        && di->sdata_size > 0
        && !(*sym_avma_out + *sym_size_out <= di->sdata_avma
             || *sym_avma_out >= di->sdata_avma + di->sdata_size);

   in_rodata 
      = di->rodata_present
        && di->rodata_size > 0
        && !(*sym_avma_out + *sym_size_out <= di->rodata_avma
             || *sym_avma_out >= di->rodata_avma + di->rodata_size);

   in_bss 
      = di->bss_present
        && di->bss_size > 0
        && !(*sym_avma_out + *sym_size_out <= di->bss_avma
             || *sym_avma_out >= di->bss_avma + di->bss_size);

   in_sbss 
      = di->sbss_present
        && di->sbss_size > 0
        && !(*sym_avma_out + *sym_size_out <= di->sbss_avma
             || *sym_avma_out >= di->sbss_avma + di->sbss_size);


   if (*is_text_out) {
      /* This used to reject any symbol falling outside the text
         segment ("if (!in_text) ...").  Now it is relaxed slightly,
         to reject only symbols which fall outside the area mapped
         r-x.  This is in accordance with r7427.  See
         "Comment_Regarding_Text_Range_Checks" in storage.c for
         background. */
      Bool in_rx;
      vg_assert(di->have_rx_map);
      in_rx = (!(*sym_avma_out + *sym_size_out <= di->rx_map_avma
                 || *sym_avma_out >= di->rx_map_avma + di->rx_map_size));
      if (in_text)
         vg_assert(in_rx);
      if (!in_rx) {
         TRACE_SYMTAB(
            "ignore -- %#lx .. %#lx outside .text svma range %#lx .. %#lx\n",
            *sym_avma_out, *sym_avma_out + *sym_size_out,
            di->text_avma,
            di->text_avma + di->text_size);
         return False;
      }
   } else {
     if (!(in_data || in_sdata || in_rodata || in_bss || in_sbss)) {
         TRACE_SYMTAB(
            "ignore -- %#lx .. %#lx outside .data / .sdata / .rodata / .bss / .sbss svma ranges\n",
            *sym_avma_out, *sym_avma_out + *sym_size_out);
         return False;
      }
   }

#  if defined(VGP_ppc64_linux)
   /* It's crucial that we never add symbol addresses in the .opd
      section.  This would completely mess up function redirection and
      intercepting.  This assert ensures that anysymbols that make it
      into the symbol table on ppc64-linux don't point into .opd. */
   if (di->opd_present && di->opd_size > 0) {
      vg_assert(*sym_avma_out + *sym_size_out <= di->opd_avma
                || *sym_avma_out >= di->opd_avma + di->opd_size);
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
        struct _DebugInfo* di, UChar* tab_name,
        ElfXX_Sym* symtab_img, SizeT symtab_szB,
        UChar*     strtab_img, SizeT strtab_szB,
        Bool       symtab_in_debug,
        UChar*     opd_img /* ppc64-linux only */ 
     )
{
   Word       i;
   Addr       sym_svma, sym_avma_really;
   Char      *sym_name, *sym_name_really;
   Int        sym_size;
   Addr       sym_tocptr;
   Bool       from_opd, is_text, is_ifunc;
   DiSym      risym;
   ElfXX_Sym *sym;

   if (strtab_img == NULL || symtab_img == NULL) {
      Char buf[80];
      vg_assert(VG_(strlen)(tab_name) < 40);
      VG_(sprintf)(buf, "   object doesn't have a %s", tab_name);
      ML_(symerr)(di, False, buf);
      return;
   }

   TRACE_SYMTAB("\n--- Reading (ELF, standard) %s (%ld entries) ---\n",
                tab_name, symtab_szB/sizeof(ElfXX_Sym) );

   /* Perhaps should start at i = 1; ELF docs suggest that entry
      0 always denotes 'unknown symbol'. */
   for (i = 1; i < (Word)(symtab_szB/sizeof(ElfXX_Sym)); i++) {
      sym      = & symtab_img[i];
      sym_name = (UChar*)(strtab_img + sym->st_name);
      sym_svma = sym->st_value;

      if (di->trace_symtab)
         show_raw_elf_symbol(i, sym, sym_name, sym_svma, False);

      if (get_elf_symbol_info(di, sym, sym_name, sym_svma,
                              symtab_in_debug,
                              opd_img, di->text_bias,
                              &sym_name_really, 
                              &sym_avma_really,
                              &sym_size,
                              &sym_tocptr,
                              &from_opd, &is_text, &is_ifunc)) {

         risym.addr    = sym_avma_really;
         risym.size    = sym_size;
         risym.name    = ML_(addStr) ( di, sym_name_really, -1 );
         risym.tocptr  = sym_tocptr;
         risym.isText  = is_text;
         risym.isIFunc = is_ifunc;
         vg_assert(risym.name != NULL);
         vg_assert(risym.tocptr == 0); /* has no role except on ppc64-linux */
         ML_(addSym) ( di, &risym );

         if (di->trace_symtab) {
            VG_(printf)("    rec(%c) [%4ld]:          "
                        "  val %#010lx, sz %4d  %s\n",
                        is_text ? 't' : 'd',
                        i,
                        risym.addr,
                        (Int)risym.size,
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
      Bool       is_text;
      Bool       is_ifunc;
   }
   TempSym;

static Word cmp_TempSymKey ( TempSymKey* key1, TempSym* elem2 ) {
   if (key1->addr < elem2->key.addr) return -1;
   if (key1->addr > elem2->key.addr) return 1;
   return (Word)VG_(strcmp)(key1->name, elem2->key.name);
}

static
__attribute__((unused)) /* not referred to on all targets */
void read_elf_symtab__ppc64_linux( 
        struct _DebugInfo* di, UChar* tab_name,
        ElfXX_Sym* symtab_img, SizeT symtab_szB,
        UChar*     strtab_img, SizeT strtab_szB,
        Bool       symtab_in_debug,
        UChar*     opd_img /* ppc64-linux only */ 
     )
{
   Word        i;
   Int         old_size;
   Addr        sym_svma, sym_avma_really;
   Char       *sym_name, *sym_name_really;
   Int         sym_size;
   Addr        sym_tocptr;
   Bool        from_opd, modify_size, modify_tocptr, is_text, is_ifunc;
   DiSym       risym;
   ElfXX_Sym  *sym;
   OSet       *oset;
   TempSymKey  key;
   TempSym    *elem;
   TempSym    *prev;

   if (strtab_img == NULL || symtab_img == NULL) {
      Char buf[80];
      vg_assert(VG_(strlen)(tab_name) < 40);
      VG_(sprintf)(buf, "   object doesn't have a %s", tab_name);
      ML_(symerr)(di, False, buf);
      return;
   }

   TRACE_SYMTAB("\n--- Reading (ELF, ppc64-linux) %s (%ld entries) ---\n",
                tab_name, symtab_szB/sizeof(ElfXX_Sym) );

   oset = VG_(OSetGen_Create)( offsetof(TempSym,key), 
                               (OSetCmp_t)cmp_TempSymKey, 
                               ML_(dinfo_zalloc), "di.respl.1",
                               ML_(dinfo_free) );
   vg_assert(oset);

   /* Perhaps should start at i = 1; ELF docs suggest that entry
      0 always denotes 'unknown symbol'. */
   for (i = 1; i < (Word)(symtab_szB/sizeof(ElfXX_Sym)); i++) {
      sym      = & symtab_img[i];
      sym_name = (Char*)(strtab_img + sym->st_name);
      sym_svma = sym->st_value;

      if (di->trace_symtab)
         show_raw_elf_symbol(i, sym, sym_name, sym_svma, True);

      if (get_elf_symbol_info(di, sym, sym_name, sym_svma,
                              symtab_in_debug,
                              opd_img, di->text_bias,
                              &sym_name_really, 
                              &sym_avma_really,
                              &sym_size,
                              &sym_tocptr,
                              &from_opd, &is_text, &is_ifunc)) {

         /* Check if we've seen this (name,addr) key before. */
         key.addr = sym_avma_really;
         key.name = sym_name_really;
         prev = VG_(OSetGen_Lookup)( oset, &key );

         if (prev) {

            /* Seen it before.  Fold in whatever new info we can. */
            modify_size   = False;
            modify_tocptr = False;
            old_size   = 0;

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
               prev->tocptr = sym_tocptr;
            }
            else {
               /* ignore. can we do better here? */
            }

            /* Only one or the other is possible (I think) */
            vg_assert(!(modify_size && modify_tocptr));

            if (modify_size && di->trace_symtab) {
               VG_(printf)("    modify (old sz %4d)    "
                           " val %#010lx, toc %#010lx, sz %4d  %s\n",
                           old_size,
                           prev->key.addr,
                           prev->tocptr,
                           (Int)   prev->size, 
                           (HChar*)prev->key.name
               );
            }
            if (modify_tocptr && di->trace_symtab) {
               VG_(printf)("    modify (upd tocptr)     "
                           " val %#010lx, toc %#010lx, sz %4d  %s\n",
                           prev->key.addr,
                           prev->tocptr,
                           (Int)   prev->size,
                           (HChar*)prev->key.name
               );
            }

         } else {

            /* A new (name,addr) key.  Add and continue. */
            elem = VG_(OSetGen_AllocNode)(oset, sizeof(TempSym));
            vg_assert(elem);
            elem->key      = key;
            elem->tocptr   = sym_tocptr;
            elem->size     = sym_size;
            elem->from_opd = from_opd;
            elem->is_text  = is_text;
            elem->is_ifunc = is_ifunc;
            VG_(OSetGen_Insert)(oset, elem);
            if (di->trace_symtab) {
               VG_(printf)("   to-oset [%4ld]:          "
                           "  val %#010lx, toc %#010lx, sz %4d  %s\n",
                           i,
                           elem->key.addr,
                           elem->tocptr,
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
   VG_(OSetGen_ResetIter)( oset );

   while ( (elem = VG_(OSetGen_Next)(oset)) ) {
      risym.addr    = elem->key.addr;
      risym.size    = elem->size;
      risym.name    = ML_(addStr) ( di, elem->key.name, -1 );
      risym.tocptr  = elem->tocptr;
      risym.isText  = elem->is_text;
      risym.isIFunc = elem->is_ifunc;
      vg_assert(risym.name != NULL);

      ML_(addSym) ( di, &risym );
      if (di->trace_symtab) {
         VG_(printf)("    rec(%c) [%4ld]:          "
                     "   val %#010lx, toc %#010lx, sz %4d  %s\n",
                     risym.isText ? 't' : 'd',
                     i,
                     risym.addr,
                     risym.tocptr,
                     (Int)   risym.size,
                     (HChar*)risym.name
               );
      }
      i++;
   }

   VG_(OSetGen_Destroy)( oset );
}


/*
 * Look for a build-id in an ELF image. The build-id specification
 * can be found here:
 *
 * http://fedoraproject.org/wiki/RolandMcGrath/BuildID
 */
static
Char *find_buildid(Addr image, UWord n_image)
{
   Char* buildid = NULL;
   ElfXX_Ehdr* ehdr = (ElfXX_Ehdr*)image;

#ifdef NT_GNU_BUILD_ID
   if (n_image >= sizeof(ElfXX_Ehdr) &&
       ML_(is_elf_object_file)(ehdr, n_image)) {
      Word i;

      for (i = 0; i < ehdr->e_phnum; i++) {
         ElfXX_Phdr* phdr = (ElfXX_Phdr*)(image + ehdr->e_phoff + i * ehdr->e_phentsize);

         if (phdr->p_type == PT_NOTE) {
            ElfXX_Off offset =  phdr->p_offset;

            while (offset < phdr->p_offset + phdr->p_filesz) {
               ElfXX_Nhdr* note = (ElfXX_Nhdr*)(image + offset);
               Char* name = (Char *)note + sizeof(ElfXX_Nhdr);
               UChar *desc = (UChar *)name + ((note->n_namesz + 3) & ~3);
               Word j;

               if (VG_(strcmp)(name, ELF_NOTE_GNU) == 0 &&
                   note->n_type == NT_GNU_BUILD_ID) {
                  buildid = ML_(dinfo_zalloc)("di.fbi.1", note->n_descsz * 2 + 1);
                  
                  for (j = 0; j < note->n_descsz; j++) {
                     VG_(sprintf)(buildid + VG_(strlen)(buildid), "%02x", desc[j]);
                  }
               }

               offset = offset + sizeof(ElfXX_Nhdr)
                               + ((note->n_namesz + 3) & ~3)
                               + ((note->n_descsz + 3) & ~3);
            }            
         }
      }    
   }
#endif

   return buildid;
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
Addr open_debug_file( Char* name, Char* buildid, UInt crc, /*OUT*/UWord* size )
{
   SysRes fd, sres;
   struct vg_stat stat_buf;
   UInt calccrc;

   fd = VG_(open)(name, VKI_O_RDONLY, 0);
   if (sr_isError(fd))
      return 0;

   if (VG_(fstat)(sr_Res(fd), &stat_buf) != 0) {
      VG_(close)(sr_Res(fd));
      return 0;
   }

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, "  Considering %s ..\n", name);
   
   *size = stat_buf.size;
   
   sres = VG_(am_mmap_file_float_valgrind)
             ( *size, VKI_PROT_READ, sr_Res(fd), 0 );

   VG_(close)(sr_Res(fd));
   
   if (sr_isError(sres))
      return 0;

   if (buildid) {
      Char* debug_buildid = find_buildid(sr_Res(sres), *size);
      if (debug_buildid == NULL || VG_(strcmp)(buildid, debug_buildid) != 0) {
         SysRes res = VG_(am_munmap_valgrind)(sr_Res(sres), *size);
         vg_assert(!sr_isError(res));
         if (VG_(clo_verbosity) > 1)
            VG_(message)(Vg_DebugMsg, 
               "  .. build-id mismatch (found %s wanted %s)\n", debug_buildid, buildid);
         ML_(dinfo_free)(debug_buildid);
         return 0;
      }
      ML_(dinfo_free)(debug_buildid);

      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg, "  .. build-id is valid\n");
   } else {
      calccrc = calc_gnu_debuglink_crc32(0, (UChar*)sr_Res(sres), *size);
      if (calccrc != crc) {
         SysRes res = VG_(am_munmap_valgrind)(sr_Res(sres), *size);
         vg_assert(!sr_isError(res));
         if (VG_(clo_verbosity) > 1)
            VG_(message)(Vg_DebugMsg, 
               "  .. CRC mismatch (computed %08x wanted %08x)\n", calccrc, crc);
         return 0;
      }

      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg, "  .. CRC is valid\n");
   }
   
   return sr_Res(sres);
}

/*
 * Try to find a separate debug file for a given object file.
 */
static
Addr find_debug_file( struct _DebugInfo* di,
                      Char* objpath, Char* buildid,
                      Char* debugname, UInt crc,
                      /*OUT*/UWord* size )
{
   Char *debugpath = NULL;
   Addr addr = 0;

   if (buildid != NULL) {
      debugpath = ML_(dinfo_zalloc)(
                     "di.fdf.1",
                     VG_(strlen)(buildid) + 33);

      VG_(sprintf)(debugpath, "/usr/lib/debug/.build-id/%c%c/%s.debug",
                   buildid[0], buildid[1], buildid + 2);

      if ((addr = open_debug_file(debugpath, buildid, 0, size)) == 0) {
         ML_(dinfo_free)(debugpath);
         debugpath = NULL;
      }
   }

   if (addr == 0 && debugname != NULL) {
      Char *objdir = ML_(dinfo_strdup)("di.fdf.2", objpath);
      Char *objdirptr;

      if ((objdirptr = VG_(strrchr)(objdir, '/')) != NULL)
         *objdirptr = '\0';

      debugpath = ML_(dinfo_zalloc)(
                     "di.fdf.3",
                     VG_(strlen)(objdir) + VG_(strlen)(debugname) + 32);

      VG_(sprintf)(debugpath, "%s/%s", objdir, debugname);

      if ((addr = open_debug_file(debugpath, NULL, crc, size)) == 0) {
         VG_(sprintf)(debugpath, "%s/.debug/%s", objdir, debugname);
         if ((addr = open_debug_file(debugpath, NULL, crc, size)) == 0) {
            VG_(sprintf)(debugpath, "/usr/lib/debug%s/%s", objdir, debugname);
            addr = open_debug_file(debugpath, NULL, crc, size);
         }
      }

      ML_(dinfo_free)(objdir);
   }

   if (addr) {
      TRACE_SYMTAB("\n");
      TRACE_SYMTAB("------ Found a debuginfo file: %s\n", debugpath);
   }

   ML_(dinfo_free)(debugpath);

   return addr;
}


static Bool contained_within ( Addr outer, UWord n_outer,
                               Addr inner, UWord n_inner )
{
   if (n_outer == 0 || n_inner == 0)
      return False;
   /* Simplistic .. assumes no wraparound (reasonably enough) */
   if (inner >= outer && inner+n_inner <= outer+n_outer)
      return True;
   return False;
}

static void* INDEX_BIS ( void* base, Word idx, Word scale ) {
   return (void*)( ((UChar*)base) + idx * scale );
}


/* Find the file offset corresponding to SVMA by using the program
   headers.  This is taken from binutils-2.17/binutils/readelf.c
   offset_from_vma(). */
static
Word file_offset_from_svma ( /*OUT*/Bool* ok,
                             Addr         svma,
                             ElfXX_Phdr*  phdr_img,
                             Word         phdr_nent,
                             Word         phdr_ent_szB )
{
   Word        i;
   ElfXX_Phdr* seg;
   for (i = 0; i < phdr_nent; i++) {
      seg = INDEX_BIS( phdr_img, i, phdr_ent_szB );
      if (seg->p_type != PT_LOAD)
         continue;
      if (svma >= (seg->p_vaddr & -seg->p_align)
          && svma + 1 <= seg->p_vaddr + seg->p_filesz) {
         *ok = True;
         return svma - seg->p_vaddr + seg->p_offset;
      }
   }
   *ok = False;
   return 0;
}

/* The central function for reading ELF debug info.  For the
   object/exe specified by the DebugInfo, find ELF sections, then read
   the symbols, line number info, file name info, CFA (stack-unwind
   info) and anything else we want, into the tables within the
   supplied DebugInfo.
*/
Bool ML_(read_elf_debug_info) ( struct _DebugInfo* di )
{
   Bool          res, ok;
   SysRes        fd, sres;
   Word          i;
   Bool          dynbss_present = False;
   Bool          sdynbss_present = False;

   /* Image addresses for the ELF file we're working with. */
   Addr          oimage   = 0;
   UWord         n_oimage = 0;

   /* Ditto for any ELF debuginfo file that we might happen to load. */
   Addr          dimage   = 0;
   UWord         n_dimage = 0;

   /* ELF header for the main file.  Should == oimage since is at
      start of file. */
   ElfXX_Ehdr* ehdr_img = NULL;

   /* Program header table image addr, # entries, entry size */
   ElfXX_Phdr* phdr_img     = NULL;
   UWord       phdr_nent    = 0;
   UWord       phdr_ent_szB = 0;

   /* Section header image addr, # entries, entry size.  Also the
      associated string table. */
   ElfXX_Shdr* shdr_img        = NULL;
   UWord       shdr_nent       = 0;
   UWord       shdr_ent_szB    = 0;
   UChar*      shdr_strtab_img = NULL;

   /* SVMAs covered by rx and rw segments and corresponding bias. */
   Addr     rx_svma_base = 0;
   Addr     rx_svma_limit = 0;
   PtrdiffT rx_bias = 0;
   Addr     rw_svma_base = 0;
   Addr     rw_svma_limit = 0;
   PtrdiffT rw_bias = 0;

   /* Build ID */
   Char* buildid = NULL;

   vg_assert(di);
   vg_assert(di->have_rx_map == True);
   vg_assert(di->have_rw_map == True);
   vg_assert(di->rx_map_size > 0);
   vg_assert(di->rw_map_size > 0);
   vg_assert(di->have_dinfo == False);
   vg_assert(di->filename);
   vg_assert(!di->memname);
   vg_assert(!di->symtab);
   vg_assert(!di->loctab);
   vg_assert(!di->cfsi);
   vg_assert(!di->cfsi_exprs);
   vg_assert(!di->strchunks);
   vg_assert(!di->soname);

   /* If these don't hold true, it means that m_syswrap/m_aspacemgr
      managed to do a mapping where the start isn't page aligned.
      Which sounds pretty bogus to me. */
   vg_assert(VG_IS_PAGE_ALIGNED(di->rx_map_avma));
   vg_assert(VG_IS_PAGE_ALIGNED(di->rw_map_avma));

   /* ----------------------------------------------------------
      At this point, there is very little information in the
      DebugInfo.  We only know that something that looks like an ELF
      file has been mapped rx-ishly as recorded with the di->*rx_map*
      fields and has also been mapped rw-ishly as recorded with the
      di->*rw_map* fields.  First we examine the file's ELF Program
      Header, and, by comparing that against the di->*r{w,x}_map*
      info, try to figure out the AVMAs for the sections we care
      about, that should have been mapped: text, data, sdata, bss got,
      plt, and toc.
      ---------------------------------------------------------- */

   res = False;

   oimage = (Addr)NULL;
   if (VG_(clo_verbosity) > 1 || VG_(clo_trace_redir))
      VG_(message)(Vg_DebugMsg, "Reading syms from %s (%#lx)\n",
                                di->filename, di->rx_map_avma );

   /* mmap the object image aboard, so that we can read symbols and
      line number info out of it.  It will be munmapped immediately
      thereafter; it is only aboard transiently. */

   fd = VG_(open)(di->filename, VKI_O_RDONLY, 0);
   if (sr_isError(fd)) {
      ML_(symerr)(di, True, "Can't open .so/.exe to read symbols?!");
      return False;
   }

   { Long n_oimageLL = VG_(fsize)(sr_Res(fd));
     if (n_oimageLL <= 0) {
        ML_(symerr)(di, True, "Can't stat .so/.exe (to determine its size)?!");
        VG_(close)(sr_Res(fd));
        return False;
     }
     n_oimage = (UWord)(ULong)n_oimageLL;
   }

   sres = VG_(am_mmap_file_float_valgrind)
             ( n_oimage, VKI_PROT_READ, sr_Res(fd), 0 );

   VG_(close)(sr_Res(fd));

   if (sr_isError(sres)) {
      VG_(message)(Vg_UserMsg, "warning: mmap failed on %s\n", di->filename );
      VG_(message)(Vg_UserMsg, "         no symbols or debug info loaded\n" );
      return False;
   }

   oimage = sr_Res(sres);
   /* Check against wraparound.  am_mmap_file_float_valgrind should
      not produce a wrapped-around mapping. */
   vg_assert(n_oimage > 0);
   vg_assert(oimage + n_oimage > oimage);

   if (0) {
      VG_(printf)("read_elf_debug_info: OIMAGE = %p - %p\n", 
                  (void*)oimage, (void*)(oimage + (UWord)n_oimage));
   }

   /* Ok, the object image is safely in oimage[0 .. n_oimage-1].  Now
      verify that it is a valid ELF .so or executable image. */
   res      = False;
   ok       = (n_oimage >= sizeof(ElfXX_Ehdr));
   ehdr_img = (ElfXX_Ehdr*)oimage;

   if (ok)
      ok &= ML_(is_elf_object_file)(ehdr_img, n_oimage);

   if (!ok) {
      ML_(symerr)(di, True, "Invalid ELF Header");
      goto out;
   }

   /* Find where the program and section header tables are, and give
      up if either is missing or outside the image (bogus). */
   phdr_img     = (ElfXX_Phdr*)( ((UChar*)ehdr_img) + ehdr_img->e_phoff );
   phdr_nent    = ehdr_img->e_phnum;
   phdr_ent_szB = ehdr_img->e_phentsize;

   shdr_img     = (ElfXX_Shdr*)( ((UChar*)ehdr_img) + ehdr_img->e_shoff );
   shdr_nent    = ehdr_img->e_shnum;
   shdr_ent_szB = ehdr_img->e_shentsize;

   TRACE_SYMTAB("------ Basic facts about the object ------\n");
   TRACE_SYMTAB("object: img %p n_oimage %ld\n",
               (void*)oimage, n_oimage);
   TRACE_SYMTAB("phdr:   img %p nent %ld ent_szB %ld\n",
               phdr_img, phdr_nent, phdr_ent_szB);
   TRACE_SYMTAB("shdr:   img %p nent %ld ent_szB %ld\n",
               shdr_img, shdr_nent, shdr_ent_szB);

   if (phdr_nent == 0
       || !contained_within(
             oimage, n_oimage,
             (Addr)phdr_img, phdr_nent * phdr_ent_szB)) {
      ML_(symerr)(di, True, "Missing or invalid ELF Program Header Table");
      goto out;
   }

   if (shdr_nent == 0
       || !contained_within(
             oimage, n_oimage,
             (Addr)shdr_img, shdr_nent * shdr_ent_szB)) {
      ML_(symerr)(di, True, "Missing or invalid ELF Section Header Table");
      goto out;
   }

   /* Also find the section header's string table, and validate. */
   /* checked previously by is_elf_object_file: */
   vg_assert( ehdr_img->e_shstrndx != SHN_UNDEF );

   shdr_strtab_img
      = (UChar*)( ((UChar*)ehdr_img)
                  + shdr_img[ehdr_img->e_shstrndx].sh_offset);
   if (!contained_within( oimage, n_oimage,
                          (Addr)shdr_strtab_img,
                          1/*bogus, but we don't know the real size*/ )) {
      ML_(symerr)(di, True, "Invalid ELF Section Header String Table");
      goto out;
   }

   TRACE_SYMTAB("shdr:   string table at %p\n", shdr_strtab_img );

   /* Do another amazingly tedious thing: find out the .soname for
      this object.  Apparently requires looking through the program
      header table. */
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("------ Looking for the soname ------\n");
   vg_assert(di->soname == NULL);
   {
      ElfXX_Addr prev_svma = 0;

      for (i = 0; i < phdr_nent; i++) {
         ElfXX_Phdr* phdr = INDEX_BIS( phdr_img, i, phdr_ent_szB );

         /* Make sure the PT_LOADable entries are in order */
         if (phdr->p_type == PT_LOAD) {
            TRACE_SYMTAB("PT_LOAD in order?: %#lx %#lx\n",
                         prev_svma + 0UL,
                         phdr->p_vaddr + 0UL);
            if (phdr->p_vaddr < prev_svma) {
               ML_(symerr)(di, True,
                           "ELF Program Headers are not in ascending order");
               goto out;
            }
            prev_svma = phdr->p_vaddr;
            if (rx_svma_limit == 0
                && phdr->p_offset >= di->rx_map_foff
                && phdr->p_offset < di->rx_map_foff + di->rx_map_size
                && phdr->p_offset + phdr->p_filesz <= di->rx_map_foff + di->rx_map_size) {
               rx_svma_base = phdr->p_vaddr;
               rx_svma_limit = phdr->p_vaddr + phdr->p_memsz;
               rx_bias = di->rx_map_avma - di->rx_map_foff + phdr->p_offset - phdr->p_vaddr;
            }
            else if (rw_svma_limit == 0
                     && phdr->p_offset >= di->rw_map_foff
                     && phdr->p_offset < di->rw_map_foff + di->rw_map_size
                     && phdr->p_offset + phdr->p_filesz <= di->rw_map_foff + di->rw_map_size) {
               rw_svma_base = phdr->p_vaddr;
               rw_svma_limit = phdr->p_vaddr + phdr->p_memsz;
               rw_bias = di->rw_map_avma - di->rw_map_foff + phdr->p_offset - phdr->p_vaddr;
            }
         }

         /* Try to get the soname.  If there isn't one, use "NONE".
            The seginfo needs to have some kind of soname in order to
            facilitate writing redirect functions, since all redirect
            specifications require a soname (pattern). */
         if (phdr->p_type == PT_DYNAMIC && di->soname == NULL) {
            ElfXX_Dyn* dyn_img = (ElfXX_Dyn*)( ((UChar*)ehdr_img)
                                               + phdr->p_offset);
            Word   stroff = -1;
            UChar* strtab = NULL;
            Word   j;
            for (j = 0; dyn_img[j].d_tag != DT_NULL; j++) {
               switch (dyn_img[j].d_tag) {
                  case DT_SONAME: {
                     stroff = dyn_img[j].d_un.d_val;
                     break;
                  }
                  case DT_STRTAB: {
                     Bool ok2 = False;
                     Word offset = file_offset_from_svma(
                                      &ok2,
                                      dyn_img[j].d_un.d_ptr,
                                      phdr_img,
                                      phdr_nent, phdr_ent_szB
                                   );
                     if (ok2 && strtab == NULL) {
                        vg_assert(offset >= 0 && offset <= n_oimage);
                        strtab = ((UChar*)ehdr_img) + offset;
                     }
                     break;
                  }
                  default:
                     break;
               }
            }
            if (stroff != -1 && strtab != NULL) {
               TRACE_SYMTAB("Found soname = %s\n", strtab+stroff);
               di->soname = ML_(dinfo_strdup)("di.redi.1", strtab+stroff);
            }
         }
      } /* for (i = 0; i < phdr_nent; i++) ... */
   } /* look for the soname */

   /* If, after looking at all the program headers, we still didn't 
      find a soname, add a fake one. */
   if (di->soname == NULL) {
      TRACE_SYMTAB("No soname found; using (fake) \"NONE\"\n");
      di->soname = "NONE";
   }

   vg_assert(rx_svma_limit != 0);
   vg_assert(rw_svma_limit != 0);

   /* Now read the section table. */
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("------ Examining the section headers "
                "and program headers ------\n");
   TRACE_SYMTAB("rx: at %#lx are mapped foffsets %ld .. %ld\n",
               di->rx_map_avma,
               di->rx_map_foff, di->rx_map_foff + di->rx_map_size - 1 );
   TRACE_SYMTAB("rx: contains svmas %#lx .. %#lx with bias %#lx\n",
                rx_svma_base, rx_svma_limit - 1, rx_bias );
   TRACE_SYMTAB("rw: at %#lx are mapped foffsets %ld .. %ld\n",
               di->rw_map_avma,
               di->rw_map_foff, di->rw_map_foff + di->rw_map_size - 1 );
   TRACE_SYMTAB("rw: contains svmas %#lx .. %#lx with bias %#lx\n",
                rw_svma_base, rw_svma_limit - 1, rw_bias );

   for (i = 0; i < shdr_nent; i++) {
      ElfXX_Shdr* shdr = INDEX_BIS( shdr_img, i, shdr_ent_szB );
      UChar* name = shdr_strtab_img + shdr->sh_name;
      Addr   svma = shdr->sh_addr;
      OffT   foff = shdr->sh_offset;
      UWord  size = shdr->sh_size;
      UInt   alyn = shdr->sh_addralign;
      Bool   bits = !(shdr->sh_type == SHT_NOBITS);
      Bool   inrx = svma >= rx_svma_base && svma < rx_svma_limit;
      Bool   inrw = svma >= rw_svma_base && svma < rw_svma_limit;

      TRACE_SYMTAB(" [sec %2ld]  %s %s  al%2u  foff %6ld .. %6ld  "
                  "  svma %p  name \"%s\"\n", 
                  i, inrx ? "rx" : "  ", inrw ? "rw" : "  ", alyn,
                  foff, foff+size-1, (void*)svma, name );

      /* Check for sane-sized segments.  SHT_NOBITS sections have zero
         size in the file. */
      if ((foff >= n_oimage) || (foff + (bits ? size : 0) > n_oimage)) {
         ML_(symerr)(di, True, "ELF Section extends beyond image end");
         goto out;
      }

      /* Check for a sane alignment value. */
      if (alyn > 0 && -1 == VG_(log2)(alyn)) {
         ML_(symerr)(di, True, "ELF Section contains invalid "
                               ".sh_addralign value");
         goto out;
      }

#     define BAD(_secname)                                 \
         do { ML_(symerr)(di, True,                        \
                          "Can't make sense of " _secname  \
                          " section mapping");             \
              goto out;                                    \
         } while (0)

      /* Find avma-s for: .text .data .sdata .rodata .bss .sbss .plt .got .opd
         and .eh_frame */

      /* Accept .text where mapped as rx (code), even if zero-sized */
      if (0 == VG_(strcmp)(name, ".text")) {
         if (inrx && size >= 0 && !di->text_present) {
            di->text_present = True;
            di->text_svma = svma;
            di->text_avma = svma + rx_bias;
            di->text_size = size;
            di->text_bias = rx_bias;
            di->text_debug_svma = svma;
            di->text_debug_bias = rx_bias;
            TRACE_SYMTAB("acquiring .text svma = %#lx .. %#lx\n",
                         di->text_svma, 
                         di->text_svma + di->text_size - 1);
            TRACE_SYMTAB("acquiring .text avma = %#lx .. %#lx\n",
                         di->text_avma, 
                         di->text_avma + di->text_size - 1);
            TRACE_SYMTAB("acquiring .text bias = %#lx\n", di->text_bias);
         } else {
            BAD(".text");
         }
      }

      /* Accept .data where mapped as rw (data), even if zero-sized */
      if (0 == VG_(strcmp)(name, ".data")) {
         if (inrw && size >= 0 && !di->data_present) {
            di->data_present = True;
            di->data_svma = svma;
            di->data_avma = svma + rw_bias;
            di->data_size = size;
            di->data_bias = rw_bias;
            di->data_debug_svma = svma;
            di->data_debug_bias = rw_bias;
            TRACE_SYMTAB("acquiring .data svma = %#lx .. %#lx\n",
                         di->data_svma,
                         di->data_svma + di->data_size - 1);
            TRACE_SYMTAB("acquiring .data avma = %#lx .. %#lx\n",
                         di->data_avma,
                         di->data_avma + di->data_size - 1);
            TRACE_SYMTAB("acquiring .data bias = %#lx\n", di->data_bias);
         } else {
            BAD(".data");
         }
      }

      /* Accept .sdata where mapped as rw (data) */
      if (0 == VG_(strcmp)(name, ".sdata")) {
         if (inrw && size > 0 && !di->sdata_present) {
            di->sdata_present = True;
            di->sdata_svma = svma;
            di->sdata_avma = svma + rw_bias;
            di->sdata_size = size;
            di->sdata_bias = rw_bias;
            di->sdata_debug_svma = svma;
            di->sdata_debug_bias = rw_bias;
            TRACE_SYMTAB("acquiring .sdata svma = %#lx .. %#lx\n",
                         di->sdata_svma,
                         di->sdata_svma + di->sdata_size - 1);
            TRACE_SYMTAB("acquiring .sdata avma = %#lx .. %#lx\n",
                         di->sdata_avma,
                         di->sdata_avma + di->sdata_size - 1);
            TRACE_SYMTAB("acquiring .sdata bias = %#lx\n", di->sdata_bias);
         } else {
            BAD(".sdata");
         }
      }

      /* Accept .rodata where mapped as rx (data), even if zero-sized */
      if (0 == VG_(strcmp)(name, ".rodata")) {
         if (inrx && size >= 0 && !di->rodata_present) {
            di->rodata_present = True;
            di->rodata_svma = svma;
            di->rodata_avma = svma + rx_bias;
            di->rodata_size = size;
            di->rodata_bias = rx_bias;
            di->rodata_debug_svma = svma;
            di->rodata_debug_bias = rw_bias;
            TRACE_SYMTAB("acquiring .rodata svma = %#lx .. %#lx\n",
                         di->rodata_svma,
                         di->rodata_svma + di->rodata_size - 1);
            TRACE_SYMTAB("acquiring .rodata avma = %#lx .. %#lx\n",
                         di->rodata_avma,
                         di->rodata_avma + di->rodata_size - 1);
            TRACE_SYMTAB("acquiring .rodata bias = %#lx\n", di->rodata_bias);
         } else {
            BAD(".rodata");
         }
      }

      if (0 == VG_(strcmp)(name, ".dynbss")) {
         if (inrw && size > 0 && !di->bss_present) {
            dynbss_present = True;
            di->bss_present = True;
            di->bss_svma = svma;
            di->bss_avma = svma + rw_bias;
            di->bss_size = size;
            di->bss_bias = rw_bias;
            di->bss_debug_svma = svma;
            di->bss_debug_bias = rw_bias;
            TRACE_SYMTAB("acquiring .dynbss svma = %#lx .. %#lx\n",
                         di->bss_svma,
                         di->bss_svma + di->bss_size - 1);
            TRACE_SYMTAB("acquiring .dynbss avma = %#lx .. %#lx\n",
                         di->bss_avma,
                         di->bss_avma + di->bss_size - 1);
            TRACE_SYMTAB("acquiring .dynbss bias = %#lx\n", di->bss_bias);
         }
      }

      /* Accept .bss where mapped as rw (data), even if zero-sized */
      if (0 == VG_(strcmp)(name, ".bss")) {
         if (inrw && size > 0 && dynbss_present) {
            vg_assert(di->bss_present);
            dynbss_present = False;
            vg_assert(di->bss_svma + di->bss_size == svma);
            di->bss_size += size;
            TRACE_SYMTAB("acquiring .bss svma = %#lx .. %#lx\n",
                         svma, svma + size - 1);
            TRACE_SYMTAB("acquiring .bss avma = %#lx .. %#lx\n",
                         svma + rw_bias, svma + rw_bias + size - 1);
            TRACE_SYMTAB("acquiring .bss bias = %#lx\n", di->bss_bias);
         } else

         if (inrw && size >= 0 && !di->bss_present) {
            di->bss_present = True;
            di->bss_svma = svma;
            di->bss_avma = svma + rw_bias;
            di->bss_size = size;
            di->bss_bias = rw_bias;
            di->bss_debug_svma = svma;
            di->bss_debug_bias = rw_bias;
            TRACE_SYMTAB("acquiring .bss svma = %#lx .. %#lx\n",
                         di->bss_svma,
                         di->bss_svma + di->bss_size - 1);
            TRACE_SYMTAB("acquiring .bss avma = %#lx .. %#lx\n",
                         di->bss_avma,
                         di->bss_avma + di->bss_size - 1);
            TRACE_SYMTAB("acquiring .bss bias = %#lx\n", di->bss_bias);
         } else

         /* Now one from the wtf?! department ... */
         if (inrx && (!inrw) && size >= 0 && !di->bss_present) {
            /* File contains a .bss, but it got mapped as rx only.
               This is very strange.  For now, just pretend we didn't
               see it :-) */
            di->bss_present = False;
            di->bss_svma = 0;
            di->bss_avma = 0;
            di->bss_size = 0;
            di->bss_bias = 0;
            di->bss_debug_svma = 0;
            di->bss_debug_bias = 0;
            if (!VG_(clo_xml)) {
               VG_(message)(Vg_UserMsg,
                            "Warning: the following file's .bss is "
                            "mapped r-x only - ignoring .bss syms\n");
               VG_(message)(Vg_UserMsg,   " %s\n", di->filename 
                                                      ? di->filename
                                                      : (UChar*)"(null?!)" );
            }
         } else

         if ((!inrw) && (!inrx) && size >= 0 && !di->bss_present) {
            /* File contains a .bss, but it didn't get mapped.  Ignore. */
            di->bss_present = False;
            di->bss_svma = 0;
            di->bss_avma = 0;
            di->bss_size = 0;
            di->bss_bias = 0;
         } else {
            BAD(".bss");
         }
      }

      if (0 == VG_(strcmp)(name, ".sdynbss")) {
         if (inrw && size >= 0 && !di->sbss_present) {
            sdynbss_present = True;
            di->sbss_present = True;
            di->sbss_svma = svma;
            di->sbss_avma = svma + rw_bias;
            di->sbss_size = size;
            di->sbss_bias = rw_bias;
            di->sbss_debug_svma = svma;
            di->sbss_debug_bias = rw_bias;
            TRACE_SYMTAB("acquiring .sdynbss svma = %#lx .. %#lx\n",
                         di->sbss_svma,
                         di->sbss_svma + di->sbss_size - 1);
            TRACE_SYMTAB("acquiring .sdynbss avma = %#lx .. %#lx\n",
                         di->sbss_avma,
                         di->sbss_avma + di->sbss_size - 1);
            TRACE_SYMTAB("acquiring .sdynbss bias = %#lx\n", di->sbss_bias);
         }
      }

      /* Accept .sbss where mapped as rw (data) */
      if (0 == VG_(strcmp)(name, ".sbss")) {
         if (inrw && size > 0 && sdynbss_present) {
            vg_assert(di->sbss_present);
            sdynbss_present = False;
            vg_assert(di->sbss_svma + di->sbss_size == svma);
            di->sbss_size += size;
            TRACE_SYMTAB("acquiring .sbss svma = %#lx .. %#lx\n",
                         svma, svma + size - 1);
            TRACE_SYMTAB("acquiring .sbss avma = %#lx .. %#lx\n",
                         svma + rw_bias, svma + rw_bias + size - 1);
            TRACE_SYMTAB("acquiring .sbss bias = %#lx\n", di->sbss_bias);
         } else

         if (inrw && size > 0 && !di->sbss_present) {
            di->sbss_present = True;
            di->sbss_svma = svma;
            di->sbss_avma = svma + rw_bias;
            di->sbss_size = size;
            di->sbss_bias = rw_bias;
            di->sbss_debug_svma = svma;
            di->sbss_debug_bias = rw_bias;
            TRACE_SYMTAB("acquiring .sbss svma = %#lx .. %#lx\n",
                         di->sbss_svma,
                         di->sbss_svma + di->sbss_size - 1);
            TRACE_SYMTAB("acquiring .sbss avma = %#lx .. %#lx\n",
                         di->sbss_avma,
                         di->sbss_avma + di->sbss_size - 1);
            TRACE_SYMTAB("acquiring .sbss bias = %#lx\n", di->sbss_bias);
         } else {
            BAD(".sbss");
         }
      }

      /* Accept .got where mapped as rw (data) */
      if (0 == VG_(strcmp)(name, ".got")) {
         if (inrw && size > 0 && !di->got_present) {
            di->got_present = True;
            di->got_avma = svma + rw_bias;
            di->got_size = size;
            TRACE_SYMTAB("acquiring .got avma = %#lx\n", di->got_avma);
         } else {
            BAD(".got");
         }
      }

      /* Accept .got.plt where mapped as rw (data) */
      if (0 == VG_(strcmp)(name, ".got.plt")) {
         if (inrw && size > 0 && !di->gotplt_present) {
            di->gotplt_present = True;
            di->gotplt_avma = svma + rw_bias;
            di->gotplt_size = size;
            TRACE_SYMTAB("acquiring .got.plt avma = %#lx\n", di->gotplt_avma);
         } else if (size != 0) {
            BAD(".got.plt");
         }
      }

      /* PLT is different on different platforms, it seems. */
#     if defined(VGP_x86_linux) || defined(VGP_amd64_linux) \
         || defined(VGP_arm_linux)
      /* Accept .plt where mapped as rx (code) */
      if (0 == VG_(strcmp)(name, ".plt")) {
         if (inrx && size > 0 && !di->plt_present) {
            di->plt_present = True;
            di->plt_avma = svma + rx_bias;
            di->plt_size = size;
            TRACE_SYMTAB("acquiring .plt avma = %#lx\n", di->plt_avma);
         } else {
            BAD(".plt");
         }
      }
#     elif defined(VGP_ppc32_linux)
      /* Accept .plt where mapped as rw (data) */
      if (0 == VG_(strcmp)(name, ".plt")) {
         if (inrw && size > 0 && !di->plt_present) {
            di->plt_present = True;
            di->plt_avma = svma + rw_bias;
            di->plt_size = size;
            TRACE_SYMTAB("acquiring .plt avma = %#lx\n", di->plt_avma);
         } else {
            BAD(".plt");
         }
      }
#     elif defined(VGP_ppc64_linux)
      /* Accept .plt where mapped as rw (data), or unmapped */
      if (0 == VG_(strcmp)(name, ".plt")) {
         if (inrw && size > 0 && !di->plt_present) {
            di->plt_present = True;
            di->plt_avma = svma + rw_bias;
            di->plt_size = size;
            TRACE_SYMTAB("acquiring .plt avma = %#lx\n", di->plt_avma);
         } else 
         if ((!inrw) && (!inrx) && size > 0 && !di->plt_present) {
            /* File contains a .plt, but it didn't get mapped.
               Presumably it is not required on this platform.  At
               least don't reject the situation as invalid. */
            di->plt_present = True;
            di->plt_avma = 0;
            di->plt_size = 0;
         } else {
            BAD(".plt");
         }
      }
#     else
#       error "Unsupported platform"
#     endif

      /* Accept .opd where mapped as rw (data) */
      if (0 == VG_(strcmp)(name, ".opd")) {
         if (inrw && size > 0 && !di->opd_present) {
            di->opd_present = True;
            di->opd_avma = svma + rw_bias;
            di->opd_size = size;
            TRACE_SYMTAB("acquiring .opd avma = %#lx\n", di->opd_avma);
         } else {
            BAD(".opd");
         }
      }

      /* Accept .eh_frame where mapped as rx (code).  This seems to be
         the common case.  However, if that doesn't pan out, try for
         rw (data) instead. */
      if (0 == VG_(strcmp)(name, ".eh_frame")) {
         if (inrx && size > 0 && !di->ehframe_present) {
            di->ehframe_present = True;
            di->ehframe_avma = svma + rx_bias;
            di->ehframe_size = size;
            TRACE_SYMTAB("acquiring .eh_frame avma = %#lx\n", di->ehframe_avma);
         } else
         if (inrw && size > 0 && !di->ehframe_present) {
            di->ehframe_present = True;
            di->ehframe_avma = svma + rw_bias;
            di->ehframe_size = size;
            TRACE_SYMTAB("acquiring .eh_frame avma = %#lx\n", di->ehframe_avma);
         } else {
            BAD(".eh_frame");
         }
      }

#    undef BAD

   }

   if (0) VG_(printf)("YYYY text_: avma %#lx  size %ld  bias %#lx\n",
                      di->text_avma, di->text_size, di->text_bias);

   if (VG_(clo_verbosity) > 2 || VG_(clo_trace_redir))
      VG_(message)(Vg_DebugMsg, "   svma %#010lx, avma %#010lx\n",
                                di->text_avma - di->text_bias,
                                di->text_avma );

   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("------ Finding image addresses "
                "for debug-info sections ------\n");

   /* Find interesting sections, read the symbol table(s), read any debug
      information */
   {
      /* IMAGE addresses: pointers to start of sections in the
         transiently loaded oimage, not in the fragments of the file
         mapped in by the guest's dynamic linker. */
      UChar*     strtab_img       = NULL; /* .strtab */
      ElfXX_Sym* symtab_img       = NULL; /* .symtab */
      UChar*     dynstr_img       = NULL; /* .dynstr */
      ElfXX_Sym* dynsym_img       = NULL; /* .dynsym */
      UChar*     debuglink_img    = NULL; /* .gnu_debuglink */
      UChar*     stab_img         = NULL; /* .stab         (stabs)  */
      UChar*     stabstr_img      = NULL; /* .stabstr      (stabs)  */
      UChar*     debug_line_img   = NULL; /* .debug_line   (dwarf2) */
      UChar*     debug_info_img   = NULL; /* .debug_info   (dwarf2) */
      UChar*     debug_abbv_img   = NULL; /* .debug_abbrev (dwarf2) */
      UChar*     debug_str_img    = NULL; /* .debug_str    (dwarf2) */
      UChar*     debug_ranges_img = NULL; /* .debug_ranges (dwarf2) */
      UChar*     debug_loc_img    = NULL; /* .debug_loc    (dwarf2) */
      UChar*     debug_frame_img  = NULL; /* .debug_frame  (dwarf2) */
      UChar*     dwarf1d_img      = NULL; /* .debug        (dwarf1) */
      UChar*     dwarf1l_img      = NULL; /* .line         (dwarf1) */
      UChar*     ehframe_img      = NULL; /* .eh_frame     (dwarf2) */
      UChar*     opd_img          = NULL; /* .opd (dwarf2,
                                                   ppc64-linux) */
      /* Section sizes, in bytes */
      SizeT      strtab_sz       = 0;
      SizeT      symtab_sz       = 0;
      SizeT      dynstr_sz       = 0;
      SizeT      dynsym_sz       = 0;
      SizeT      debuglink_sz    = 0;
      SizeT      stab_sz         = 0;
      SizeT      stabstr_sz      = 0;
      SizeT      debug_line_sz   = 0;
      SizeT      debug_info_sz   = 0;
      SizeT      debug_abbv_sz   = 0;
      SizeT      debug_str_sz    = 0;
      SizeT      debug_ranges_sz = 0;
      SizeT      debug_loc_sz    = 0;
      SizeT      debug_frame_sz  = 0;
      SizeT      dwarf1d_sz      = 0;
      SizeT      dwarf1l_sz      = 0;
      SizeT      ehframe_sz      = 0;
      SizeT      opd_sz_unused   = 0;

      /* Find all interesting sections */

      /* What FIND does: it finds the section called SEC_NAME.  The
         size of it is assigned to SEC_SIZE.  The address of the
         section in the transiently loaded oimage is assigned to
         SEC_FILEA.  Even for sections which are marked loadable, the
         client's ld.so may not have loaded them yet, so there is no
         guarantee that we can safely prod around in any such area).
         Because the entire object file is transiently mapped aboard
         for inspection, it's always safe to inspect that area. */

      for (i = 0; i < ehdr_img->e_shnum; i++) {

#        define FIND(sec_name, sec_size, sec_img) \
         do { ElfXX_Shdr* shdr \
                 = INDEX_BIS( shdr_img, i, shdr_ent_szB ); \
            if (0 == VG_(strcmp)(sec_name, shdr_strtab_img \
                                           + shdr->sh_name)) { \
               Bool nobits; \
               sec_img  = (void*)(oimage + shdr->sh_offset); \
               sec_size = shdr->sh_size; \
               nobits   = shdr->sh_type == SHT_NOBITS; \
               TRACE_SYMTAB( "%18s:  img %p .. %p\n", \
                             sec_name, (UChar*)sec_img, \
                             ((UChar*)sec_img) + sec_size - 1); \
               /* SHT_NOBITS sections have zero size in the file. */ \
               if ( shdr->sh_offset \
                    + (nobits ? 0 : sec_size) > n_oimage ) { \
                  ML_(symerr)(di, True, \
                              "   section beyond image end?!"); \
                  goto out; \
               } \
            } \
         } while (0);

         /*   NAME              SIZE             IMAGE addr */
         FIND(".dynsym",        dynsym_sz,       dynsym_img)
         FIND(".dynstr",        dynstr_sz,       dynstr_img)
         FIND(".symtab",        symtab_sz,       symtab_img)
         FIND(".strtab",        strtab_sz,       strtab_img)

         FIND(".gnu_debuglink", debuglink_sz,    debuglink_img)

         FIND(".stab",          stab_sz,         stab_img)
         FIND(".stabstr",       stabstr_sz,      stabstr_img)

         FIND(".debug_line",    debug_line_sz,   debug_line_img)
         FIND(".debug_info",    debug_info_sz,   debug_info_img)
         FIND(".debug_abbrev",  debug_abbv_sz,   debug_abbv_img)
         FIND(".debug_str",     debug_str_sz,    debug_str_img)
         FIND(".debug_ranges",  debug_ranges_sz, debug_ranges_img)
         FIND(".debug_loc",     debug_loc_sz,    debug_loc_img)
         FIND(".debug_frame",   debug_frame_sz,  debug_frame_img)

         FIND(".debug",         dwarf1d_sz,      dwarf1d_img)
         FIND(".line",          dwarf1l_sz,      dwarf1l_img)
         FIND(".eh_frame",      ehframe_sz,      ehframe_img)

         FIND(".opd",           opd_sz_unused,   opd_img)

#        undef FIND
      }

      /* Look for a build-id */
      buildid = find_buildid(oimage, n_oimage);

      /* Look for a debug image */
      if (buildid != NULL || debuglink_img != NULL) {
         /* Do have a debuglink section? */
         if (debuglink_img != NULL) {
            UInt crc_offset = VG_ROUNDUP(VG_(strlen)(debuglink_img)+1, 4);
            UInt crc;

            vg_assert(crc_offset + sizeof(UInt) <= debuglink_sz);

            /* Extract the CRC from the debuglink section */
            crc = *(UInt *)(debuglink_img + crc_offset);

            /* See if we can find a matching debug file */
            dimage = find_debug_file( di, di->filename, buildid,
                                      debuglink_img, crc, &n_dimage );
         } else {
            /* See if we can find a matching debug file */
            dimage = find_debug_file( di, di->filename, buildid, NULL, 0, &n_dimage );
         }

         ML_(dinfo_free)(buildid);

         if (dimage != 0 
             && n_dimage >= sizeof(ElfXX_Ehdr)
             && ML_(is_elf_object_file)((void*)dimage, n_dimage)) {

            /* Pull out and validate program header and section header info */
            ElfXX_Ehdr* ehdr_dimg     = (ElfXX_Ehdr*)dimage;
            ElfXX_Phdr* phdr_dimg     = (ElfXX_Phdr*)( ((UChar*)ehdr_dimg)
                                                       + ehdr_dimg->e_phoff );
            UWord       phdr_dnent    = ehdr_dimg->e_phnum;
            UWord       phdr_dent_szB = ehdr_dimg->e_phentsize;
            ElfXX_Shdr* shdr_dimg     = (ElfXX_Shdr*)( ((UChar*)ehdr_dimg)
                                                       + ehdr_dimg->e_shoff );
            UWord       shdr_dnent       = ehdr_dimg->e_shnum;
            UWord       shdr_dent_szB    = ehdr_dimg->e_shentsize;
            UChar*      shdr_strtab_dimg = NULL;

            /* SVMAs covered by rx and rw segments and corresponding bias. */
            Addr     rx_dsvma_base = 0;
            Addr     rx_dsvma_limit = 0;
            PtrdiffT rx_dbias = 0;
            Addr     rw_dsvma_base = 0;
            Addr     rw_dsvma_limit = 0;
            PtrdiffT rw_dbias = 0;

            Bool need_symtab, need_stabs, need_dwarf2, need_dwarf1;

            if (phdr_dnent == 0
                || !contained_within(
                       dimage, n_dimage,
                       (Addr)phdr_dimg, phdr_dnent * phdr_dent_szB)) {
               ML_(symerr)(di, True,
                           "Missing or invalid ELF Program Header Table"
                           " (debuginfo file)");
               goto out;
            }

            if (shdr_dnent == 0
                || !contained_within(
                       dimage, n_dimage,
                       (Addr)shdr_dimg, shdr_dnent * shdr_dent_szB)) {
               ML_(symerr)(di, True,
                           "Missing or invalid ELF Section Header Table"
                           " (debuginfo file)");
               goto out;
            }

            /* Also find the section header's string table, and validate. */
            /* checked previously by is_elf_object_file: */
            vg_assert( ehdr_dimg->e_shstrndx != SHN_UNDEF );

            shdr_strtab_dimg
               = (UChar*)( ((UChar*)ehdr_dimg)
                           + shdr_dimg[ehdr_dimg->e_shstrndx].sh_offset);
            if (!contained_within( 
                    dimage, n_dimage,
                    (Addr)shdr_strtab_dimg,
                    1/*bogus, but we don't know the real size*/ )) {
               ML_(symerr)(di, True, 
                           "Invalid ELF Section Header String Table"
                           " (debuginfo file)");
               goto out;
            }

            need_symtab = (NULL == symtab_img);
            need_stabs  = (NULL == stab_img);
            need_dwarf2 = (NULL == debug_info_img);
            need_dwarf1 = (NULL == dwarf1d_img);

            for (i = 0; i < ehdr_dimg->e_phnum; i++) {
               ElfXX_Phdr* phdr 
                  = INDEX_BIS( (void*)(dimage + ehdr_dimg->e_phoff), 
                                          i, phdr_ent_szB );
               if (phdr->p_type == PT_LOAD) {
                  if (rx_dsvma_limit == 0
                      && phdr->p_offset >= di->rx_map_foff
                      && phdr->p_offset < di->rx_map_foff + di->rx_map_size
                      && phdr->p_offset + phdr->p_filesz <= di->rx_map_foff + di->rx_map_size) {
                     rx_dsvma_base = phdr->p_vaddr;
                     rx_dsvma_limit = phdr->p_vaddr + phdr->p_memsz;
                     rx_dbias = di->rx_map_avma - di->rx_map_foff + phdr->p_offset - phdr->p_vaddr;
                  }
                  else if (rw_dsvma_limit == 0
                           && phdr->p_offset >= di->rw_map_foff
                           && phdr->p_offset < di->rw_map_foff + di->rw_map_size
                           && phdr->p_offset + phdr->p_filesz <= di->rw_map_foff + di->rw_map_size) {
                     rw_dsvma_base = phdr->p_vaddr;
                     rw_dsvma_limit = phdr->p_vaddr + phdr->p_memsz;
                     rw_dbias = di->rw_map_avma - di->rw_map_foff + phdr->p_offset - phdr->p_vaddr;
                  }
               }
            }

            /* Find all interesting sections */
            for (i = 0; i < ehdr_dimg->e_shnum; i++) {

               /* Find debug svma and bias information for sections
                  we found in the main file. */ 

#              define FIND(sec, seg) \
               do { ElfXX_Shdr* shdr \
                       = INDEX_BIS( shdr_dimg, i, shdr_dent_szB ); \
                  if (di->sec##_present \
                      && 0 == VG_(strcmp)("." #sec, \
                                          shdr_strtab_dimg + shdr->sh_name)) { \
                     vg_assert(di->sec##_size == shdr->sh_size); \
                     vg_assert(di->sec##_avma +  shdr->sh_addr + seg##_dbias); \
                     di->sec##_debug_svma = shdr->sh_addr; \
                     di->sec##_debug_bias = seg##_dbias; \
                     TRACE_SYMTAB("acquiring ." #sec " debug svma = %#lx .. %#lx\n", \
                                  di->sec##_debug_svma, \
                                  di->sec##_debug_svma + di->sec##_size - 1); \
                     TRACE_SYMTAB("acquiring ." #sec " debug bias = %#lx\n", \
                                  di->sec##_debug_bias); \
                  } \
               } while (0);

               /* SECTION   SEGMENT */
               FIND(text,   rx)
               FIND(data,   rw)
               FIND(sdata,  rw)
               FIND(rodata, rw)
               FIND(bss,    rw)
               FIND(sbss,   rw)

#              undef FIND

               /* Same deal as previous FIND, except only do it for those
                  sections for which we didn't find anything useful in
                  the main file. */

#              define FIND(condition, sec_name, sec_size, sec_img) \
               do { ElfXX_Shdr* shdr \
                       = INDEX_BIS( shdr_dimg, i, shdr_dent_szB ); \
                  if (condition \
                      && 0 == VG_(strcmp)(sec_name, \
                                          shdr_strtab_dimg + shdr->sh_name)) { \
                     Bool nobits; \
                     if (0 != sec_img) \
                        VG_(core_panic)("repeated section!\n"); \
                     sec_img  = (void*)(dimage + shdr->sh_offset); \
                     sec_size = shdr->sh_size; \
                     nobits   = shdr->sh_type == SHT_NOBITS; \
                     TRACE_SYMTAB( "%18s: dimg %p .. %p\n", \
                                   sec_name, \
                                   (UChar*)sec_img, \
                                   ((UChar*)sec_img) + sec_size - 1); \
                     /* SHT_NOBITS sections have zero size in the file. */ \
                     if ( shdr->sh_offset \
                          + (nobits ? 0 : sec_size) > n_dimage ) { \
                        ML_(symerr)(di, True, \
                                    "   section beyond image end?!"); \
                        goto out; \
                     } \
                  } \
               } while (0);

               /* NEEDED?        NAME             SIZE           IMAGE addr */
               FIND(need_symtab, ".symtab",       symtab_sz,     symtab_img)
               FIND(need_symtab, ".strtab",       strtab_sz,     strtab_img)
               FIND(need_stabs,  ".stab",         stab_sz,       stab_img)
               FIND(need_stabs,  ".stabstr",      stabstr_sz,    stabstr_img)
               FIND(need_dwarf2, ".debug_line",   debug_line_sz, debug_line_img)
               FIND(need_dwarf2, ".debug_info",   debug_info_sz, debug_info_img)
               FIND(need_dwarf2, ".debug_abbrev", debug_abbv_sz, debug_abbv_img)
               FIND(need_dwarf2, ".debug_str",    debug_str_sz,  debug_str_img)
               FIND(need_dwarf2, ".debug_ranges", debug_ranges_sz, 
                                                               debug_ranges_img)
               FIND(need_dwarf2, ".debug_loc",    debug_loc_sz,  debug_loc_img)
               FIND(need_dwarf2, ".debug_frame",  debug_frame_sz,
                                                               debug_frame_img)
               FIND(need_dwarf1, ".debug",        dwarf1d_sz,    dwarf1d_img)
               FIND(need_dwarf1, ".line",         dwarf1l_sz,    dwarf1l_img)

#              undef FIND
            }
         }
      }

      /* Check some sizes */
      vg_assert((dynsym_sz % sizeof(ElfXX_Sym)) == 0);
      vg_assert((symtab_sz % sizeof(ElfXX_Sym)) == 0);

      /* Read symbols */
      {
         void (*read_elf_symtab)(struct _DebugInfo*,UChar*,
                                 ElfXX_Sym*,SizeT,
                                 UChar*,SizeT,
                                 Bool,UChar*);
         Bool symtab_in_debug;
#        if defined(VGP_ppc64_linux)
         read_elf_symtab = read_elf_symtab__ppc64_linux;
#        else
         read_elf_symtab = read_elf_symtab__normal;
#        endif
         symtab_in_debug = (Addr)symtab_img >= dimage
                           && (Addr)symtab_img < dimage + n_dimage;
         read_elf_symtab(di, "symbol table",
                         symtab_img, symtab_sz,
                         strtab_img, strtab_sz, 
                         symtab_in_debug, opd_img);

         read_elf_symtab(di, "dynamic symbol table",
                         dynsym_img, dynsym_sz,
                         dynstr_img, dynstr_sz, 
                         False, opd_img);
      }

      /* Read .eh_frame and .debug_frame (call-frame-info) if any */
      if (ehframe_img) {
         vg_assert(ehframe_sz == di->ehframe_size);
         ML_(read_callframe_info_dwarf3)( di, ehframe_img, ehframe_sz, True );
      }
      if (debug_frame_sz) {
         ML_(read_callframe_info_dwarf3)( di, debug_frame_img,
                                          debug_frame_sz, False );
      }

      /* Read the stabs and/or dwarf2 debug information, if any.  It
         appears reading stabs stuff on amd64-linux doesn't work, so
         we ignore it. */
#     if !defined(VGP_amd64_linux)
      if (stab_img && stabstr_img) {
         ML_(read_debuginfo_stabs) ( di, stab_img, stab_sz, 
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

         /* The old reader: line numbers and unwind info only */
         ML_(read_debuginfo_dwarf3) ( di,
                                      debug_info_img, debug_info_sz,
                                      debug_abbv_img, debug_abbv_sz,
                                      debug_line_img, debug_line_sz,
                                      debug_str_img,  debug_str_sz );

         /* The new reader: read the DIEs in .debug_info to acquire
            information on variable types and locations.  But only if
            the tool asks for it, or the user requests it on the
            command line. */
         if (VG_(needs).var_info /* the tool requires it */
             || VG_(clo_read_var_info) /* the user asked for it */) {
            ML_(new_dwarf3_reader)(
               di, debug_info_img,   debug_info_sz,
                   debug_abbv_img,   debug_abbv_sz,
                   debug_line_img,   debug_line_sz,
                   debug_str_img,    debug_str_sz,
                   debug_ranges_img, debug_ranges_sz,
                   debug_loc_img,    debug_loc_sz
            );
         }
      }
      if (dwarf1d_img && dwarf1l_img) {
         ML_(read_debuginfo_dwarf1) ( di, dwarf1d_img, dwarf1d_sz, 
                                          dwarf1l_img, dwarf1l_sz );
      }
   }
   res = True;

  out: {
   SysRes m_res;

   /* Last, but not least, heave the image(s) back overboard. */
   if (dimage) {
      m_res = VG_(am_munmap_valgrind) ( dimage, n_dimage );
      vg_assert(!sr_isError(m_res));
   }
   m_res = VG_(am_munmap_valgrind) ( oimage, n_oimage );
   vg_assert(!sr_isError(m_res));
   return res;
  } 
}

#endif // defined(VGO_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
