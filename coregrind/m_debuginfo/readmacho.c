
/*--------------------------------------------------------------------*/
/*--- Reading of syms & debug info from Mach-O files.              ---*/
/*---                                                  readmacho.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2013 Apple Inc.
      Greg Parker gparker@apple.com

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
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcproc.h"
#include "pub_core_aspacemgr.h"    /* for mmaping debuginfo files */
#include "pub_core_machine.h"      /* VG_ELF_CLASS */
#include "pub_core_options.h"
#include "pub_core_oset.h"
#include "pub_core_tooliface.h"    /* VG_(needs) */
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuginfo.h"

#include "priv_misc.h"
#include "priv_image.h"
#include "priv_d3basics.h"
#include "priv_tytypes.h"
#include "priv_storage.h"
#include "priv_readmacho.h"
#include "priv_readdwarf.h"
#include "priv_readdwarf3.h"
#include "priv_readstabs.h"

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/fat.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

#if VG_WORDSIZE == 4
# define MAGIC MH_MAGIC
# define MACH_HEADER mach_header
# define LC_SEGMENT_CMD LC_SEGMENT
# define SEGMENT_COMMAND segment_command
# define SECTION section
# define NLIST nlist
#else
# define MAGIC MH_MAGIC_64
# define MACH_HEADER mach_header_64
# define LC_SEGMENT_CMD LC_SEGMENT_64
# define SEGMENT_COMMAND segment_command_64
# define SECTION section_64
# define NLIST nlist_64
#endif


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Mach-O file mapping/unmapping helpers                ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* A DiSlice is used to handle the thin/fat distinction for MachO images.
   (1) the entire mapped-in ("primary") image, fat headers, kitchen sink,
       whatnot: the entire file.  This is the DiImage* that is the backing
       for the DiSlice.
   (2) the Mach-O object of interest, which is presumably somewhere inside
       the primary image.  map_image_aboard() below, which generates this
       info, will carefully check that the macho_ fields denote a section of
       memory that falls entirely inside the primary image.
*/

Bool ML_(is_macho_object_file)( const void* buf, SizeT szB )
{
   /* (JRS: the Mach-O headers might not be in this mapped data,
      because we only mapped a page for this initial check,
      or at least not very much, and what's at the start of the file
      is in general a so-called fat header.  The Mach-O object we're
      interested in could be arbitrarily far along the image, and so
      we can't assume its header will fall within this page.) */

   /* But we can say that either it's a fat object, in which case it
      begins with a fat header, or it's unadorned Mach-O, in which
      case it starts with a normal header.  At least do what checks we
      can to establish whether or not we're looking at something
      sane. */

   const struct fat_header*  fh_be = buf;
   const struct MACH_HEADER* mh    = buf;

   vg_assert(buf);
   if (szB < sizeof(struct fat_header))
      return False;
   if (VG_(ntohl)(fh_be->magic) == FAT_MAGIC)
      return True;

   if (szB < sizeof(struct MACH_HEADER))
      return False;
   if (mh->magic == MAGIC)
      return True;

   return False;
}


/* Unmap an image mapped in by map_image_aboard. */
static void unmap_image ( /*MOD*/DiSlice* sli )
{
   vg_assert(sli);
   if (ML_(sli_is_valid)(*sli)) {
      ML_(img_done)(sli->img);
      *sli = DiSlice_INVALID;
   }
}


/* Open the given file, find the thin part if necessary, do some
   checks, and return a DiSlice containing details of both the thin
   part and (implicitly, via the contained DiImage*) the fat part.
   returns DiSlice_INVALID if it fails.  If it succeeds, the returned
   slice is guaranteed to refer to a valid(ish) Mach-O image. */
static DiSlice map_image_aboard ( DebugInfo* di, /* only for err msgs */
                                  const HChar* filename )
{
   DiSlice sli = DiSlice_INVALID;

   /* First off, try to map the thing in. */
   DiImage* mimg = ML_(img_from_local_file)(filename);
   if (mimg == NULL) {
      VG_(message)(Vg_UserMsg, "warning: connection to image %s failed\n",
                               filename );
      VG_(message)(Vg_UserMsg, "         no symbols or debug info loaded\n" );
      return DiSlice_INVALID;
   }

   /* Now we have a viable DiImage* for it.  Look for the embedded
      Mach-O object.  If not findable, close the image and fail. */
   DiOffT            fh_be_ioff = 0;
   struct fat_header fh_be;
   struct fat_header fh;
     
   // Assume initially that we have a thin image, and narrow
   // the bounds if it turns out to be fat.  This stores |mimg| as
   // |sli.img|, so NULL out |mimg| after this point, for the sake of
   // clarity.
   sli  = ML_(sli_from_img)(mimg);
   mimg = NULL;

   // Check for fat header.
   if (ML_(img_size)(sli.img) < sizeof(struct fat_header)) {
      ML_(symerr)(di, True, "Invalid Mach-O file (0 too small).");
      goto close_and_fail;
   }

   // Fat header is always BIG-ENDIAN
   ML_(img_get)(&fh_be, sli.img, fh_be_ioff, sizeof(fh_be));
   VG_(memset)(&fh, 0, sizeof(fh));
   fh.magic     = VG_(ntohl)(fh_be.magic);
   fh.nfat_arch = VG_(ntohl)(fh_be.nfat_arch);
   if (fh.magic == FAT_MAGIC) {
      // Look for a good architecture.
      if (ML_(img_size)(sli.img) < sizeof(struct fat_header)
                                   + fh.nfat_arch * sizeof(struct fat_arch)) {
         ML_(symerr)(di, True, "Invalid Mach-O file (1 too small).");
         goto close_and_fail;
      }
      DiOffT arch_be_ioff;
      Int    f;
      for (f = 0, arch_be_ioff = sizeof(struct fat_header);
           f < fh.nfat_arch;
           f++, arch_be_ioff += sizeof(struct fat_arch)) {
#        if defined(VGA_ppc)
         Int cputype = CPU_TYPE_POWERPC;
#        elif defined(VGA_ppc64)
         Int cputype = CPU_TYPE_POWERPC64;
#        elif defined(VGA_x86)
         Int cputype = CPU_TYPE_X86;
#        elif defined(VGA_amd64)
         Int cputype = CPU_TYPE_X86_64;
#        else
#          error "unknown architecture"
#        endif
         struct fat_arch arch_be;
         struct fat_arch arch;
         ML_(img_get)(&arch_be, sli.img, arch_be_ioff, sizeof(arch_be));
         VG_(memset)(&arch, 0, sizeof(arch));
         arch.cputype    = VG_(ntohl)(arch_be.cputype);
         arch.cpusubtype = VG_(ntohl)(arch_be.cpusubtype);
         arch.offset     = VG_(ntohl)(arch_be.offset);
         arch.size       = VG_(ntohl)(arch_be.size);
         if (arch.cputype == cputype) {
            if (ML_(img_size)(sli.img) < arch.offset + arch.size) {
               ML_(symerr)(di, True, "Invalid Mach-O file (2 too small).");
               goto close_and_fail;
            }
            /* Found a suitable arch.  Narrow down the slice accordingly. */
            sli.ioff = arch.offset;
            sli.szB  = arch.size;
            break;
         }
      }
      if (f == fh.nfat_arch) {
         ML_(symerr)(di, True,
                     "No acceptable architecture found in fat file.");
         goto close_and_fail;
      }
   }

   /* Sanity check what we found. */

   /* assured by logic above */
   vg_assert(ML_(img_size)(sli.img) >= sizeof(struct fat_header));

   if (sli.szB < sizeof(struct MACH_HEADER)) {
      ML_(symerr)(di, True, "Invalid Mach-O file (3 too small).");
      goto close_and_fail;
   }

   if (sli.szB > ML_(img_size)(sli.img)) {
      ML_(symerr)(di, True, "Invalid Mach-O file (thin bigger than fat).");
      goto close_and_fail;
   }

   if (sli.ioff >= 0 && sli.ioff + sli.szB <= ML_(img_size)(sli.img)) {
      /* thin entirely within fat, as expected */
   } else {
      ML_(symerr)(di, True, "Invalid Mach-O file (thin not inside fat).");
      goto close_and_fail;
   }

   /* Peer at the Mach header for the thin object, starting at the
      beginning of the slice, to check it's at least marginally
      sane. */
   struct MACH_HEADER mh;
   ML_(cur_read_get)(&mh, ML_(cur_from_sli)(sli), sizeof(mh));
   if (mh.magic != MAGIC) {
      ML_(symerr)(di, True, "Invalid Mach-O file (bad magic).");
      goto close_and_fail;
   }

   if (sli.szB < sizeof(struct MACH_HEADER) + mh.sizeofcmds) {
      ML_(symerr)(di, True, "Invalid Mach-O file (4 too small).");
      goto close_and_fail;
   }

   /* "main image is plausible" */
   vg_assert(sli.img);
   vg_assert(ML_(img_size)(sli.img) > 0);
   /* "thin image exists and is a sub-part (or all) of main image" */
   vg_assert(sli.ioff >= 0);
   vg_assert(sli.szB > 0);
   vg_assert(sli.ioff + sli.szB <= ML_(img_size)(sli.img));
   return sli;  /* success */
   /*NOTREACHED*/

  close_and_fail:
   unmap_image(&sli);
   return DiSlice_INVALID; /* bah! */
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Mach-O symbol table reading                          ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Read a symbol table (nlist).  Add the resulting candidate symbols
   to 'syms'; the caller will post-process them and hand them off to
   ML_(addSym) itself. */
static
void read_symtab( /*OUT*/XArray* /* DiSym */ syms,
                  struct _DebugInfo* di, 
                  DiCursor symtab_cur, UInt symtab_count,
                  DiCursor strtab_cur, UInt strtab_sz )
{
   Int    i;
   DiSym  disym;

   // "start_according_to_valgrind"
   static HChar* s_a_t_v = NULL; /* do not make non-static */

   for (i = 0; i < symtab_count; i++) {
      struct NLIST nl;
      ML_(cur_read_get)(&nl,
                        ML_(cur_plus)(symtab_cur, i * sizeof(struct NLIST)),
                        sizeof(nl));

      Addr sym_addr = 0;
      if ((nl.n_type & N_TYPE) == N_SECT) {
         sym_addr = di->text_bias + nl.n_value;
      /*} else if ((nl.n_type & N_TYPE) == N_ABS) {
         GrP fixme don't ignore absolute symbols?
         sym_addr = nl.n_value; */
      } else {
         continue;
      }
      
      if (di->trace_symtab) {
         HChar* str = ML_(cur_read_strdup)(
                         ML_(cur_plus)(strtab_cur, nl.n_un.n_strx),
                         "di.read_symtab.1");
         VG_(printf)("nlist raw: avma %010lx  %s\n", sym_addr, str );
         ML_(dinfo_free)(str);
      }

      /* If no part of the symbol falls within the mapped range,
         ignore it. */
      if (sym_addr <= di->text_avma
          || sym_addr >= di->text_avma+di->text_size) {
         continue;
      }

      /* skip names which point outside the string table;
         following these risks segfaulting Valgrind */
      if (nl.n_un.n_strx < 0 || nl.n_un.n_strx >= strtab_sz) {
         continue;
      }

      HChar* name
         = ML_(cur_read_strdup)( ML_(cur_plus)(strtab_cur, nl.n_un.n_strx),
                                 "di.read_symtab.2");

      /* skip nameless symbols; these appear to be common, but
         useless */
      if (*name == 0) {
         ML_(dinfo_free)(name);
         continue;
      }

      disym.addr      = sym_addr;
      disym.tocptr    = 0;
      disym.pri_name  = ML_(addStr)(di, name, -1);
      disym.sec_names = NULL;
      disym.size      = // let canonicalize fix it
                        di->text_avma+di->text_size - sym_addr;
      disym.isText    = True;
      disym.isIFunc   = False;
      // Lots of user function names get prepended with an underscore.  Eg. the
      // function 'f' becomes the symbol '_f'.  And the "below main"
      // function is called "start".  So we skip the leading underscore, and
      // if we see 'start' and --show-below-main=no, we rename it as
      // "start_according_to_valgrind", which makes it easy to spot later
      // and display as "(below main)".
      if (disym.pri_name[0] == '_') {
         disym.pri_name++;
      } 
      else if (!VG_(clo_show_below_main) && VG_STREQ(disym.pri_name, "start")) {
         if (s_a_t_v == NULL)
            s_a_t_v = ML_(addStr)(di, "start_according_to_valgrind", -1);
         vg_assert(s_a_t_v);
         disym.pri_name = s_a_t_v;
      }

      vg_assert(disym.pri_name);
      VG_(addToXA)( syms, &disym );
      ML_(dinfo_free)(name);
   }
}


/* Compare DiSyms by their start address, and for equal addresses, use
   the primary name as a secondary sort key. */
static Int cmp_DiSym_by_start_then_name ( const void* v1, const void* v2 )
{
   const DiSym* s1 = (DiSym*)v1;
   const DiSym* s2 = (DiSym*)v2;
   if (s1->addr < s2->addr) return -1;
   if (s1->addr > s2->addr) return 1;
   return VG_(strcmp)(s1->pri_name, s2->pri_name);
}

/* 'cand' is a bunch of candidate symbols obtained by reading
   nlist-style symbol table entries.  Their ends may overlap, so sort
   them and truncate them accordingly.  The code in this routine is
   copied almost verbatim from read_symbol_table() in readxcoff.c. */
static void tidy_up_cand_syms ( /*MOD*/XArray* /* of DiSym */ syms,
                                Bool trace_symtab )
{
   Word nsyms, i, j, k, m;

   nsyms = VG_(sizeXA)(syms);

   VG_(setCmpFnXA)(syms, cmp_DiSym_by_start_then_name);
   VG_(sortXA)(syms);

   /* We only know for sure the start addresses (actual VMAs) of
      symbols, and an overestimation of their end addresses.  So sort
      by start address, then clip each symbol so that its end address
      does not overlap with the next one along.

      There is a small refinement: if a group of symbols have the same
      address, treat them as a group: find the next symbol along that
      has a higher start address, and clip all of the group
      accordingly.  This clips the group as a whole so as not to
      overlap following symbols.  This leaves prefersym() in
      storage.c, which is not nlist-specific, to later decide which of
      the symbols in the group to keep.

      Another refinement is that we need to get rid of symbols which,
      after clipping, have identical starts, ends, and names.  So the
      sorting uses the name as a secondary key.
   */

   for (i = 0; i < nsyms; i++) {
      for (k = i+1;
           k < nsyms
             && ((DiSym*)VG_(indexXA)(syms,i))->addr
                 == ((DiSym*)VG_(indexXA)(syms,k))->addr;
           k++)
         ;
      /* So now [i .. k-1] is a group all with the same start address.
         Clip their ending addresses so they don't overlap [k].  In
         the normal case (no overlaps), k == i+1. */
      if (k < nsyms) {
         DiSym* next = (DiSym*)VG_(indexXA)(syms,k);
         for (m = i; m < k; m++) {
            DiSym* here = (DiSym*)VG_(indexXA)(syms,m);
            vg_assert(here->addr < next->addr);
            if (here->addr + here->size > next->addr)
               here->size = next->addr - here->addr;
         }
      }
      i = k-1;
      vg_assert(i <= nsyms);
   }

   j = 0;
   if (nsyms > 0) {
      j = 1;
      for (i = 1; i < nsyms; i++) {
         DiSym *s_j1, *s_j, *s_i;
         vg_assert(j <= i);
         s_j1 = (DiSym*)VG_(indexXA)(syms, j-1);
         s_j  = (DiSym*)VG_(indexXA)(syms, j);
         s_i  = (DiSym*)VG_(indexXA)(syms, i);
         if (s_i->addr != s_j1->addr
             || s_i->size != s_j1->size
             || 0 != VG_(strcmp)(s_i->pri_name, s_j1->pri_name)) {
            *s_j = *s_i;
            j++;
         } else {
            if (trace_symtab)
               VG_(printf)("nlist cleanup: dump duplicate avma %010lx  %s\n",
                           s_i->addr, s_i->pri_name );
         }
      }
   }
   vg_assert(j >= 0 && j <= nsyms);
   VG_(dropTailXA)(syms, nsyms - j);
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Mach-O top-level processing                          ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

#if !defined(APPLE_DSYM_EXT_AND_SUBDIRECTORY)
#define APPLE_DSYM_EXT_AND_SUBDIRECTORY ".dSYM/Contents/Resources/DWARF/"
#endif


static Bool file_exists_p(const HChar *path)
{
   struct vg_stat sbuf;
   SysRes res = VG_(stat)(path, &sbuf);
   return sr_isError(res) ? False : True;
}


/* Search for an existing dSYM file as a possible separate debug file.  
   Adapted from gdb. */
static HChar *
find_separate_debug_file (const HChar *executable_name)
{
   const HChar *basename_str;
   HChar *dot_ptr;
   HChar *slash_ptr;
   HChar *dsymfile;
    
   /* Make sure the object file name itself doesn't contain ".dSYM" in it or we
      will end up with an infinite loop where after we add a dSYM symbol file,
      it will then enter this function asking if there is a debug file for the
      dSYM file itself.  */
   if (VG_(strcasestr) (executable_name, ".dSYM") == NULL)
   {
      /* Check for the existence of a .dSYM file for a given executable.  */
      basename_str = VG_(basename) (executable_name);
      dsymfile = ML_(dinfo_zalloc)("di.readmacho.dsymfile", 
                    VG_(strlen) (executable_name)
                    + VG_(strlen) (APPLE_DSYM_EXT_AND_SUBDIRECTORY)
                    + VG_(strlen) (basename_str)
                    + 1
                 );
        
      /* First try for the dSYM in the same directory as the original file.  */
      VG_(strcpy) (dsymfile, executable_name);
      VG_(strcat) (dsymfile, APPLE_DSYM_EXT_AND_SUBDIRECTORY);
      VG_(strcat) (dsymfile, basename_str);
        
      if (file_exists_p (dsymfile))
         return dsymfile;
        
      /* Now search for any parent directory that has a '.' in it so we can find
         Mac OS X applications, bundles, plugins, and any other kinds of files. 
         Mac OS X application bundles wil have their program in
         "/some/path/MyApp.app/Contents/MacOS/MyApp" (or replace ".app" with
         ".bundle" or ".plugin" for other types of bundles).  So we look for any
         prior '.' character and try appending the apple dSYM extension and
         subdirectory and see if we find an existing dSYM file (in the above
         MyApp example the dSYM would be at either:
         "/some/path/MyApp.app.dSYM/Contents/Resources/DWARF/MyApp" or
         "/some/path/MyApp.dSYM/Contents/Resources/DWARF/MyApp".  */
      VG_(strcpy) (dsymfile, VG_(dirname) (executable_name));
      while ((dot_ptr = VG_(strrchr) (dsymfile, '.')))
      {
         /* Find the directory delimiter that follows the '.' character since
            we now look for a .dSYM that follows any bundle extension.  */
         slash_ptr = VG_(strchr) (dot_ptr, '/');
         if (slash_ptr)
         {
             /* NULL terminate the string at the '/' character and append
                the path down to the dSYM file.  */
            *slash_ptr = '\0';
            VG_(strcat) (slash_ptr, APPLE_DSYM_EXT_AND_SUBDIRECTORY);
            VG_(strcat) (slash_ptr, basename_str);
            if (file_exists_p (dsymfile))
               return dsymfile;
         }
         
         /* NULL terminate the string at the '.' character and append
            the path down to the dSYM file.  */
         *dot_ptr = '\0';
         VG_(strcat) (dot_ptr, APPLE_DSYM_EXT_AND_SUBDIRECTORY);
         VG_(strcat) (dot_ptr, basename_str);
         if (file_exists_p (dsymfile))
            return dsymfile;
         
         /* NULL terminate the string at the '.' locatated by the strrchr()
            function again.  */
         *dot_ptr = '\0';
         
         /* We found a previous extension '.' character and did not find a
            dSYM file so now find previous directory delimiter so we don't
            try multiple times on a file name that may have a version number
            in it such as "/some/path/MyApp.6.0.4.app".  */
         slash_ptr = VG_(strrchr) (dsymfile, '/');
         if (!slash_ptr)
            break;
         /* NULL terminate the string at the previous directory character
            and search again.  */
         *slash_ptr = '\0';
      }
   }

   return NULL;
}


/* Given a DiSlice covering the entire Mach-O thin image, find the
   DiSlice for the specified (segname, sectname) pairing, if
   possible. */
static DiSlice getsectdata ( DiSlice img,
                             const HChar *segname, const HChar *sectname )
{
   DiCursor cur = ML_(cur_from_sli)(img);

   struct MACH_HEADER mh;
   ML_(cur_step_get)(&mh, &cur, sizeof(mh));

   Int c;
   for (c = 0; c < mh.ncmds; c++) {
      struct load_command cmd;          
      ML_(cur_read_get)(&cmd, cur, sizeof(cmd));
      if (cmd.cmd == LC_SEGMENT_CMD) {
         struct SEGMENT_COMMAND seg;
         ML_(cur_read_get)(&seg, cur, sizeof(seg));
         if (0 == VG_(strncmp(&seg.segname[0],
                              segname, sizeof(seg.segname)))) {
            DiCursor sects_cur = ML_(cur_plus)(cur, sizeof(seg));
            Int s;
            for (s = 0; s < seg.nsects; s++) {
               struct SECTION sect;
               ML_(cur_step_get)(&sect, &sects_cur, sizeof(sect));
               if (0 == VG_(strncmp(sect.sectname, sectname, 
                                    sizeof(sect.sectname)))) {
                  DiSlice res = img;
                  res.ioff = sect.offset;
                  res.szB = sect.size;
                  return res;
               }
            }

         }
      }
      cur = ML_(cur_plus)(cur, cmd.cmdsize);
   }

   return DiSlice_INVALID;
}


/* Brute force just simply search for uuid[0..15] in |sli| */
static Bool check_uuid_matches ( DiSlice sli, UChar* uuid )
{
   if (sli.szB < 16)
      return False;

   /* Work through the slice in 1 KB chunks. */
   UChar  first    = uuid[0];
   DiOffT min_off  = sli.ioff;
   DiOffT max1_off = sli.ioff + sli.szB;
   DiOffT curr_off = min_off;
   vg_assert(min_off < max1_off);
   while (1) {
      vg_assert(curr_off >= min_off && curr_off <= max1_off);
      if (curr_off == max1_off) break;
      DiOffT avail = max1_off - curr_off;
      vg_assert(avail > 0 && avail <= max1_off);
      if (avail > 1024) avail = 1024;
      UChar buf[1024];
      SizeT nGot = ML_(img_get_some)(buf, sli.img, curr_off, avail);
      vg_assert(nGot >= 1 && nGot <= avail);
      UInt i;
      /* Scan through the 1K chunk we got, looking for the start char. */
      for (i = 0; i < (UInt)nGot; i++) {
         if (LIKELY(buf[i] != first))
            continue;
         /* first char matches.  See if we can get 16 bytes at this
            offset, and compare. */
         if (curr_off + i < max1_off && max1_off - (curr_off + i) >= 16) {
            UChar buff16[16];
            ML_(img_get)(&buff16[0], sli.img, curr_off + i, 16);
            if (0 == VG_(memcmp)(&buff16[0], &uuid[0], 16))
               return True;
         }
      }
      curr_off += nGot;
   }
   return False;
}


/* Heuristic kludge: return True if this looks like an installed
   standard library; hence we shouldn't consider automagically running
   dsymutil on it. */
static Bool is_systemish_library_name ( HChar* name )
{
   vg_assert(name);
   if (0 == VG_(strncasecmp)(name, "/usr/", 5)
       || 0 == VG_(strncasecmp)(name, "/bin/", 5)
       || 0 == VG_(strncasecmp)(name, "/sbin/", 6)
       || 0 == VG_(strncasecmp)(name, "/opt/", 5)
       || 0 == VG_(strncasecmp)(name, "/sw/", 4)
       || 0 == VG_(strncasecmp)(name, "/System/", 8)
       || 0 == VG_(strncasecmp)(name, "/Library/", 9)
       || 0 == VG_(strncasecmp)(name, "/Applications/", 14)) {
      return True;
   } else {
      return False;
   }
}


Bool ML_(read_macho_debug_info)( struct _DebugInfo* di )
{
   DiSlice  msli         = DiSlice_INVALID; // the main image
   DiSlice  dsli         = DiSlice_INVALID; // the debuginfo image
   DiCursor sym_cur      = DiCursor_INVALID;
   DiCursor dysym_cur    = DiCursor_INVALID;
   HChar*   dsymfilename = NULL;
   Bool     have_uuid    = False;
   UChar    uuid[16];
   Word     i;
   struct _DebugInfoMapping* rx_map = NULL;
   struct _DebugInfoMapping* rw_map = NULL;

   /* mmap the object file to look for di->soname and di->text_bias 
      and uuid and nlist and STABS */

   /* This should be ensured by our caller (that we're in the accept
      state). */
   vg_assert(di->fsm.have_rx_map);
   vg_assert(di->fsm.have_rw_map);

   for (i = 0; i < VG_(sizeXA)(di->fsm.maps); i++) {
      struct _DebugInfoMapping* map = VG_(indexXA)(di->fsm.maps, i);
      if (map->rx && !rx_map)
         rx_map = map;
      if (map->rw && !rw_map)
         rw_map = map;
      if (rx_map && rw_map)
         break;
   }
   vg_assert(rx_map);
   vg_assert(rw_map);

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg,
                   "%s (rx at %#lx, rw at %#lx)\n", di->fsm.filename,
                   rx_map->avma, rw_map->avma );

   VG_(memset)(&uuid, 0, sizeof(uuid));

   msli = map_image_aboard( di, di->fsm.filename );
   if (!ML_(sli_is_valid)(msli)) {
      ML_(symerr)(di, False, "Connect to main image failed.");
      goto fail;
   }

   vg_assert(msli.img != NULL && msli.szB > 0);

   /* Poke around in the Mach-O header, to find some important
      stuff. */
   // Find LC_SYMTAB and LC_DYSYMTAB, if present.
   // Read di->soname from LC_ID_DYLIB if present, 
   //    or from LC_ID_DYLINKER if present, 
   //    or use "NONE".
   // Get di->text_bias (aka slide) based on the corresponding LC_SEGMENT
   // Get uuid for later dsym search

   di->text_bias = 0;

   { 
      DiCursor cmd_cur = ML_(cur_from_sli)(msli);

      struct MACH_HEADER mh;
      ML_(cur_step_get)(&mh, &cmd_cur, sizeof(mh));

      /* Now cur_cmd points just after the Mach header, right at the
         start of the load commands, which is where we need it to start
         the following loop. */

      Int c;
      for (c = 0; c < mh.ncmds; c++) {
         struct load_command cmd;
         ML_(cur_read_get)(&cmd, cmd_cur, sizeof(cmd));
 
         if (cmd.cmd == LC_SYMTAB) {
            sym_cur = cmd_cur;
         } 
         else if (cmd.cmd == LC_DYSYMTAB) {
            dysym_cur = cmd_cur;
         } 
         else if (cmd.cmd == LC_ID_DYLIB && mh.filetype == MH_DYLIB) {
            // GrP fixme bundle?
            struct dylib_command dcmd;
            ML_(cur_read_get)(&dcmd, cmd_cur, sizeof(dcmd));
            DiCursor dylibname_cur
               = ML_(cur_plus)(cmd_cur, dcmd.dylib.name.offset);
            HChar* dylibname
               = ML_(cur_read_strdup)(dylibname_cur, "di.rmdi.1");
            HChar* soname = VG_(strrchr)(dylibname, '/');
            if (!soname) soname = dylibname;
            else soname++;
            di->soname = ML_(dinfo_strdup)("di.readmacho.dylibname",
                                           soname);
            ML_(dinfo_free)(dylibname);
         }
         else if (cmd.cmd==LC_ID_DYLINKER  &&  mh.filetype==MH_DYLINKER) {
            struct dylinker_command dcmd;
            ML_(cur_read_get)(&dcmd, cmd_cur, sizeof(dcmd));
            DiCursor dylinkername_cur
               = ML_(cur_plus)(cmd_cur, dcmd.name.offset);
            HChar* dylinkername
               = ML_(cur_read_strdup)(dylinkername_cur, "di.rmdi.2");
            HChar* soname = VG_(strrchr)(dylinkername, '/');
            if (!soname) soname = dylinkername;
            else soname++;
            di->soname = ML_(dinfo_strdup)("di.readmacho.dylinkername",
                                           soname);
            ML_(dinfo_free)(dylinkername);
         }

         // A comment from Julian about why varinfo[35] fail:
         //
         // My impression is, from comparing the output of otool -l for these
         // executables with the logic in ML_(read_macho_debug_info),
         // specifically the part that begins "else if (cmd->cmd ==
         // LC_SEGMENT_CMD) {", that it's a complete hack which just happens
         // to work ok for text symbols.  In particular, it appears to assume
         // that in a "struct load_command" of type LC_SEGMENT_CMD, the first
         // "struct SEGMENT_COMMAND" inside it is going to contain the info we
         // need.  However, otool -l shows, and also the Apple docs state,
         // that a struct load_command may contain an arbitrary number of
         // struct SEGMENT_COMMANDs, so I'm not sure why it's OK to merely
         // snarf the first.  But I'm not sure about this.
         //
         // The "Try for __DATA" block below simply adds acquisition of data
         // svma/bias values using the same assumption.  It also needs
         // (probably) to deal with bss sections, but I don't understand how
         // this all ties together really, so it requires further study.
         //
         // If you can get your head around the relationship between MachO
         // segments, sections and load commands, this might be relatively
         // easy to fix properly.
         //
         // Basically we need to come up with plausible numbers for di->
         // {text,data,bss}_{avma,svma}, from which the _bias numbers are
         // then trivially derived.  Then I think the debuginfo reader should
         // work pretty well.
         else if (cmd.cmd == LC_SEGMENT_CMD) {
            struct SEGMENT_COMMAND seg;
            ML_(cur_read_get)(&seg, cmd_cur, sizeof(seg));
            /* Try for __TEXT */
            if (!di->text_present
                && 0 == VG_(strcmp)(&seg.segname[0], "__TEXT")
                /* DDD: is the  next line a kludge? -- JRS */
                && seg.fileoff == 0 && seg.filesize != 0) {
               di->text_present = True;
               di->text_svma = (Addr)seg.vmaddr;
               di->text_avma = rx_map->avma;
               di->text_size = seg.vmsize;
               di->text_bias = di->text_avma - di->text_svma;
               /* Make the _debug_ values be the same as the
                  svma/bias for the primary object, since there is
                  no secondary (debuginfo) object, but nevertheless
                  downstream biasing of Dwarf3 relies on the
                  _debug_ values. */
               di->text_debug_svma = di->text_svma;
               di->text_debug_bias = di->text_bias;
            }
            /* Try for __DATA */
            if (!di->data_present
                && 0 == VG_(strcmp)(&seg.segname[0], "__DATA")
                /* && DDD:seg->fileoff == 0 */ && seg.filesize != 0) {
               di->data_present = True;
               di->data_svma = (Addr)seg.vmaddr;
               di->data_avma = rw_map->avma;
               di->data_size = seg.vmsize;
               di->data_bias = di->data_avma - di->data_svma;
               di->data_debug_svma = di->data_svma;
               di->data_debug_bias = di->data_bias;
            }
         }
         else if (cmd.cmd == LC_UUID) {
             ML_(cur_read_get)(&uuid, cmd_cur, sizeof(uuid));
             have_uuid = True;
         }
         // Move the cursor along
         cmd_cur = ML_(cur_plus)(cmd_cur, cmd.cmdsize);
      }
   }

   if (!di->soname) {
      di->soname = ML_(dinfo_strdup)("di.readmacho.noname", "NONE");
   }

   if (di->trace_symtab) {
      VG_(printf)("\n");
      VG_(printf)("SONAME = %s\n", di->soname);
      VG_(printf)("\n");
   }

   /* Now we have the base object to hand.  Read symbols from it. */

   // We already asserted that ..
   vg_assert(msli.img != NULL && msli.szB > 0);

   if (ML_(cur_is_valid)(sym_cur) && ML_(cur_is_valid)(dysym_cur)) {

      struct symtab_command   symcmd;
      struct dysymtab_command dysymcmd;

      ML_(cur_read_get)(&symcmd,   sym_cur,   sizeof(symcmd));
      ML_(cur_read_get)(&dysymcmd, dysym_cur, sizeof(dysymcmd));

      /* Read nlist symbol table */
      DiCursor syms = DiCursor_INVALID;
      DiCursor strs = DiCursor_INVALID;
      XArray* /* DiSym */ candSyms = NULL;
      Word nCandSyms;

      if (msli.szB < symcmd.stroff + symcmd.strsize
          || msli.szB < symcmd.symoff + symcmd.nsyms
                                        * sizeof(struct NLIST)) {
         ML_(symerr)(di, False, "Invalid Mach-O file (5 too small).");
         goto fail;
      }   
      if (dysymcmd.ilocalsym + dysymcmd.nlocalsym > symcmd.nsyms
          || dysymcmd.iextdefsym + dysymcmd.nextdefsym > symcmd.nsyms) {
         ML_(symerr)(di, False, "Invalid Mach-O file (bad symbol table).");
         goto fail;
      }

      syms = ML_(cur_plus)(ML_(cur_from_sli)(msli), symcmd.symoff);
      strs = ML_(cur_plus)(ML_(cur_from_sli)(msli), symcmd.stroff);
      
      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg,
            "   reading syms   from primary file (%d %d)\n",
            dysymcmd.nextdefsym, dysymcmd.nlocalsym );

      /* Read candidate symbols into 'candSyms', so we can truncate
         overlapping ends and generally tidy up, before presenting
         them to ML_(addSym). */
      candSyms = VG_(newXA)(
                    ML_(dinfo_zalloc), "di.readmacho.candsyms.1",
                    ML_(dinfo_free), sizeof(DiSym)
                 );
      vg_assert(candSyms);

      // extern symbols
      read_symtab(candSyms,
                  di,
                  ML_(cur_plus)(syms,
                                dysymcmd.iextdefsym * sizeof(struct NLIST)),
                  dysymcmd.nextdefsym, strs, symcmd.strsize);
      // static and private_extern symbols
      read_symtab(candSyms,
                  di,
                  ML_(cur_plus)(syms,
                                dysymcmd.ilocalsym * sizeof(struct NLIST)),
                  dysymcmd.nlocalsym, strs, symcmd.strsize);

      /* tidy up the cand syms -- trim overlapping ends.  May resize
         candSyms. */
      tidy_up_cand_syms( candSyms, di->trace_symtab );

      /* and finally present them to ML_(addSym) */
      nCandSyms = VG_(sizeXA)( candSyms );
      for (i = 0; i < nCandSyms; i++) {
         DiSym* cand = (DiSym*) VG_(indexXA)( candSyms, i );
         vg_assert(cand->pri_name != NULL);
         vg_assert(cand->sec_names == NULL);
         if (di->trace_symtab)
            VG_(printf)("nlist final: acquire  avma %010lx-%010lx  %s\n",
                        cand->addr, cand->addr + cand->size - 1,
                        cand->pri_name );
         ML_(addSym)( di, cand );
      }
      VG_(deleteXA)( candSyms );
   }

   /* If there's no UUID in the primary, don't even bother to try and
      read any DWARF, since we won't be able to verify it matches.
      Our policy is not to load debug info unless we can verify that
      it matches the primary.  Just declare success at this point.
      And don't complain to the user, since that would cause us to
      complain on objects compiled without -g.  (Some versions of
      XCode are observed to omit a UUID entry for object linked(?)
      without -g.  Others don't appear to omit it.) */
   if (!have_uuid)
      goto success;

   /* mmap the dSYM file to look for DWARF debug info.  If successful,
      use the .macho_img and .macho_img_szB in dsli. */

   dsymfilename = find_separate_debug_file( di->fsm.filename );

   /* Try to load it. */
   if (dsymfilename) {
      Bool valid;

      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg, "   dSYM= %s\n", dsymfilename);

      dsli = map_image_aboard( di, dsymfilename );
      if (!ML_(sli_is_valid)(dsli)) {
         ML_(symerr)(di, False, "Connect to debuginfo image failed "
                                "(first attempt).");
         goto fail;
      }

      /* check it has the right uuid. */
      vg_assert(have_uuid);
      valid = dsli.img && dsli.szB > 0 && check_uuid_matches( dsli, uuid );
      if (valid)
         goto read_the_dwarf;

      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg, "   dSYM does not have "
                                   "correct UUID (out of date?)\n");
   }

   /* There was no dsym file, or it doesn't match.  We'll have to try
      regenerating it, unless --dsymutil=no, in which case just complain
      instead. */

   /* If this looks like a lib that we shouldn't run dsymutil on, just
      give up.  (possible reasons: is system lib, or in /usr etc, or
      the dsym dir would not be writable by the user, or we're running
      as root) */
   vg_assert(di->fsm.filename);
   if (is_systemish_library_name(di->fsm.filename))
      goto success;

   if (!VG_(clo_dsymutil)) {
      if (VG_(clo_verbosity) == 1) {
         VG_(message)(Vg_DebugMsg, "%s:\n", di->fsm.filename);
      }
      if (VG_(clo_verbosity) > 0)
         VG_(message)(Vg_DebugMsg, "%sdSYM directory %s; consider using "
                      "--dsymutil=yes\n",
                      VG_(clo_verbosity) > 1 ? "   " : "",
                      dsymfilename ? "has wrong UUID" : "is missing"); 
      goto success;
   }

   /* Run dsymutil */

   { Int r;
     const HChar* dsymutil = "/usr/bin/dsymutil ";
     HChar* cmd = ML_(dinfo_zalloc)( "di.readmacho.tmp1", 
                                     VG_(strlen)(dsymutil)
                                     + VG_(strlen)(di->fsm.filename)
                                     + 32 /* misc */ );
     VG_(strcpy)(cmd, dsymutil);
     if (0) VG_(strcat)(cmd, "--verbose ");
     VG_(strcat)(cmd, "\"");
     VG_(strcat)(cmd, di->fsm.filename);
     VG_(strcat)(cmd, "\"");
     VG_(message)(Vg_DebugMsg, "run: %s\n", cmd);
     r = VG_(system)( cmd );
     if (r)
        VG_(message)(Vg_DebugMsg, "run: %s FAILED\n", dsymutil);
     ML_(dinfo_free)(cmd);
     dsymfilename = find_separate_debug_file(di->fsm.filename);
   }

   /* Try again to load it. */
   if (dsymfilename) {
      Bool valid;

      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg, "   dsyms= %s\n", dsymfilename);

      dsli = map_image_aboard( di, dsymfilename );
      if (!ML_(sli_is_valid)(dsli)) {
         ML_(symerr)(di, False, "Connect to debuginfo image failed "
                                "(second attempt).");
         goto fail;
      }

      /* check it has the right uuid. */
      vg_assert(have_uuid);
      vg_assert(have_uuid);
      valid = dsli.img && dsli.szB > 0 && check_uuid_matches( dsli, uuid );
      if (!valid) {
         if (VG_(clo_verbosity) > 0) {
            VG_(message)(Vg_DebugMsg,
               "WARNING: did not find expected UUID %02X%02X%02X%02X"
               "-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X"
               " in dSYM dir\n",
               (UInt)uuid[0], (UInt)uuid[1], (UInt)uuid[2], (UInt)uuid[3],
               (UInt)uuid[4], (UInt)uuid[5], (UInt)uuid[6], (UInt)uuid[7],
               (UInt)uuid[8], (UInt)uuid[9], (UInt)uuid[10],
               (UInt)uuid[11], (UInt)uuid[12], (UInt)uuid[13],
               (UInt)uuid[14], (UInt)uuid[15] );
            VG_(message)(Vg_DebugMsg,
                         "WARNING: for %s\n", di->fsm.filename);
         }
         unmap_image( &dsli );
         /* unmap_image zeroes out dsli, so it's safe for "fail:" to
            re-try unmap_image. */
         goto fail;
      }
   }

   /* Right.  Finally we have our best try at the dwarf image, so go
      on to reading stuff out of it. */

  read_the_dwarf:
   if (ML_(sli_is_valid)(msli) && msli.szB > 0) {
      // "_mscn" is "mach-o section"
      DiSlice debug_info_mscn
         = getsectdata(dsli, "__DWARF", "__debug_info");
      DiSlice debug_abbv_mscn
         = getsectdata(dsli, "__DWARF", "__debug_abbrev");
      DiSlice debug_line_mscn
         = getsectdata(dsli, "__DWARF", "__debug_line");
      DiSlice debug_str_mscn
         = getsectdata(dsli, "__DWARF", "__debug_str");
      DiSlice debug_ranges_mscn
         = getsectdata(dsli, "__DWARF", "__debug_ranges");
      DiSlice debug_loc_mscn
         = getsectdata(dsli, "__DWARF", "__debug_loc");
   
      if (ML_(sli_is_valid)(debug_info_mscn)) {
         if (VG_(clo_verbosity) > 1) {
            if (0)
            VG_(message)(Vg_DebugMsg,
                         "Reading dwarf3 for %s (%#lx) from %s"
                         " (%lld %lld %lld %lld %lld %lld)\n",
                         di->fsm.filename, di->text_avma, dsymfilename,
                         debug_info_mscn.szB, debug_abbv_mscn.szB,
                         debug_line_mscn.szB, debug_str_mscn.szB,
                         debug_ranges_mscn.szB, debug_loc_mscn.szB
                         );
            VG_(message)(Vg_DebugMsg,
               "   reading dwarf3 from dsyms file\n");
         }
         /* The old reader: line numbers and unwind info only */
         ML_(read_debuginfo_dwarf3) ( di,
                                      debug_info_mscn,
				      DiSlice_INVALID, /* .debug_types */
                                      debug_abbv_mscn,
                                      debug_line_mscn,
                                      debug_str_mscn,
                                      DiSlice_INVALID /* ALT .debug_str */ );

         /* The new reader: read the DIEs in .debug_info to acquire
            information on variable types and locations.  But only if
            the tool asks for it, or the user requests it on the
            command line. */
         if (VG_(needs).var_info /* the tool requires it */
             || VG_(clo_read_var_info) /* the user asked for it */) {
            ML_(new_dwarf3_reader)(
               di, debug_info_mscn,
                   DiSlice_INVALID, /* .debug_types */
                   debug_abbv_mscn,
                   debug_line_mscn,
                   debug_str_mscn,
                   debug_ranges_mscn,
                   debug_loc_mscn,
                   DiSlice_INVALID, /* ALT .debug_info */
                   DiSlice_INVALID, /* ALT .debug_abbv */
                   DiSlice_INVALID, /* ALT .debug_line */
                   DiSlice_INVALID  /* ALT .debug_str */
            );
         }
      }
   }

   if (dsymfilename) ML_(dinfo_free)(dsymfilename);

  success:
   unmap_image(&msli);
   unmap_image(&dsli);
   return True;

   /* NOTREACHED */

  fail:
   ML_(symerr)(di, True, "Error reading Mach-O object.");
   unmap_image(&msli);
   unmap_image(&dsli);
   return False;
}

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
