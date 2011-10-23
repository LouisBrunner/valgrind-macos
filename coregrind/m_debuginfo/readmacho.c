
/*--------------------------------------------------------------------*/
/*--- Reading of syms & debug info from Mach-O files.              ---*/
/*---                                                  readmacho.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2011 Apple Inc.
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

#include "priv_d3basics.h"
#include "priv_misc.h"
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

typedef
   struct {
      /* These two describe the entire mapped-in ("primary") image,
         fat headers, kitchen sink, whatnot: the entire file.  The
         image is mapped into img[0 .. img_szB-1]. */
      UChar* img;
      SizeT  img_szB;
      /* These two describe the Mach-O object of interest, which is
         presumably somewhere inside the primary image.
         map_image_aboard() below, which generates this info, will
         carefully check that the macho_ fields denote a section of
         memory that falls entirely inside img[0 .. img_szB-1]. */
      UChar* macho_img;
      SizeT  macho_img_szB;
   }
   ImageInfo;


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
static void unmap_image ( /*MOD*/ImageInfo* ii )
{
   SysRes sres;
   vg_assert(ii->img);
   vg_assert(ii->img_szB > 0);
   sres = VG_(am_munmap_valgrind)( (Addr)ii->img, ii->img_szB );
   /* Do we care if this fails?  I suppose so; it would indicate
      some fairly serious snafu with the mapping of the file. */
   vg_assert( !sr_isError(sres) );
   VG_(memset)(ii, 0, sizeof(*ii));
}


/* Map a given fat or thin object aboard, find the thin part if
   necessary, do some checks, and write details of both the fat and
   thin parts into *ii.  Returns False (and leaves the file unmapped)
   on failure.  Guarantees to return pointers to a valid(ish) Mach-O
   image if it succeeds. */
static Bool map_image_aboard ( DebugInfo* di, /* only for err msgs */
                               /*OUT*/ImageInfo* ii, UChar* filename )
{
   VG_(memset)(ii, 0, sizeof(*ii));

   /* First off, try to map the thing in. */
   { SizeT  size;
     SysRes fd, sres;
     struct vg_stat stat_buf;

     fd = VG_(stat)(filename, &stat_buf);
     if (sr_isError(fd)) {
        ML_(symerr)(di, True, "Can't stat image (to determine its size)?!");
        return False;
     }
     size = stat_buf.size;

     fd = VG_(open)(filename, VKI_O_RDONLY, 0);
     if (sr_isError(fd)) {
       ML_(symerr)(di, True, "Can't open image to read symbols?!");
        return False;
     }

     sres = VG_(am_mmap_file_float_valgrind)
               ( size, VKI_PROT_READ, sr_Res(fd), 0 );
     if (sr_isError(sres)) {
        ML_(symerr)(di, True, "Can't mmap image to read symbols?!");
        return False;
     }

     VG_(close)(sr_Res(fd));

     ii->img     = (UChar*)sr_Res(sres);
     ii->img_szB = size;
   }

   /* Now it's mapped in and we have .img and .img_szB set.  Look for
      the embedded Mach-O object.  If not findable, unmap and fail. */
   { struct fat_header*  fh_be;
     struct fat_header   fh;
     struct MACH_HEADER* mh;
     
     // Assume initially that we have a thin image, and update
     // these if it turns out to be fat.
     ii->macho_img     = ii->img;
     ii->macho_img_szB = ii->img_szB;

     // Check for fat header.
     if (ii->img_szB < sizeof(struct fat_header)) {
        ML_(symerr)(di, True, "Invalid Mach-O file (0 too small).");
        goto unmap_and_fail;
     }

     // Fat header is always BIG-ENDIAN
     fh_be = (struct fat_header *)ii->img;
     fh.magic = VG_(ntohl)(fh_be->magic);
     fh.nfat_arch = VG_(ntohl)(fh_be->nfat_arch);
     if (fh.magic == FAT_MAGIC) {
        // Look for a good architecture.
        struct fat_arch *arch_be;
        struct fat_arch arch;
        Int f;
        if (ii->img_szB < sizeof(struct fat_header)
                          + fh.nfat_arch * sizeof(struct fat_arch)) {
           ML_(symerr)(di, True, "Invalid Mach-O file (1 too small).");
           goto unmap_and_fail;
        }
        for (f = 0, arch_be = (struct fat_arch *)(fh_be+1); 
             f < fh.nfat_arch;
             f++, arch_be++) {
           Int cputype;
#          if defined(VGA_ppc)
           cputype = CPU_TYPE_POWERPC;
#          elif defined(VGA_ppc64)
           cputype = CPU_TYPE_POWERPC64;
#          elif defined(VGA_x86)
           cputype = CPU_TYPE_X86;
#          elif defined(VGA_amd64)
           cputype = CPU_TYPE_X86_64;
#          else
#            error "unknown architecture"
#          endif
           arch.cputype    = VG_(ntohl)(arch_be->cputype);
           arch.cpusubtype = VG_(ntohl)(arch_be->cpusubtype);
           arch.offset     = VG_(ntohl)(arch_be->offset);
           arch.size       = VG_(ntohl)(arch_be->size);
           if (arch.cputype == cputype) {
              if (ii->img_szB < arch.offset + arch.size) {
                 ML_(symerr)(di, True, "Invalid Mach-O file (2 too small).");
                 goto unmap_and_fail;
              }
              ii->macho_img     = ii->img + arch.offset;
              ii->macho_img_szB = arch.size;
              break;
           }
        }
        if (f == fh.nfat_arch) {
           ML_(symerr)(di, True,
                       "No acceptable architecture found in fat file.");
           goto unmap_and_fail;
        }
     }

     /* Sanity check what we found. */

     /* assured by logic above */
     vg_assert(ii->img_szB >= sizeof(struct fat_header));

     if (ii->macho_img_szB < sizeof(struct MACH_HEADER)) {
        ML_(symerr)(di, True, "Invalid Mach-O file (3 too small).");
        goto unmap_and_fail;
     }

     if (ii->macho_img_szB > ii->img_szB) {
        ML_(symerr)(di, True, "Invalid Mach-O file (thin bigger than fat).");
        goto unmap_and_fail;
     }

     if (ii->macho_img >= ii->img
         && ii->macho_img + ii->macho_img_szB <= ii->img + ii->img_szB) {
        /* thin entirely within fat, as expected */
     } else {
        ML_(symerr)(di, True, "Invalid Mach-O file (thin not inside fat).");
        goto unmap_and_fail;
     }

     mh = (struct MACH_HEADER *)ii->macho_img;
     if (mh->magic != MAGIC) {
        ML_(symerr)(di, True, "Invalid Mach-O file (bad magic).");
        goto unmap_and_fail;
     }

     if (ii->macho_img_szB < sizeof(struct MACH_HEADER) + mh->sizeofcmds) {
        ML_(symerr)(di, True, "Invalid Mach-O file (4 too small).");
        goto unmap_and_fail;
     }
   }

   vg_assert(ii->img);
   vg_assert(ii->macho_img);
   vg_assert(ii->img_szB > 0);
   vg_assert(ii->macho_img_szB > 0);
   vg_assert(ii->macho_img >= ii->img);
   vg_assert(ii->macho_img + ii->macho_img_szB <= ii->img + ii->img_szB);
   return True;  /* success */
   /*NOTREACHED*/

  unmap_and_fail:
   unmap_image(ii);
   return False; /* bah! */
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
                  struct NLIST* o_symtab, UInt o_symtab_count,
                  UChar*     o_strtab, UInt o_strtab_sz )
{
   Int    i;
   Addr   sym_addr;
   DiSym  disym;
   UChar* name;

   static UChar* s_a_t_v = NULL; /* do not make non-static */

   for (i = 0; i < o_symtab_count; i++) {
      struct NLIST *nl = o_symtab+i;
      if ((nl->n_type & N_TYPE) == N_SECT) {
         sym_addr = di->text_bias + nl->n_value;
    /*} else if ((nl->n_type & N_TYPE) == N_ABS) {
         GrP fixme don't ignore absolute symbols?
         sym_addr = nl->n_value; */
      } else {
         continue;
      }
      
      if (di->trace_symtab)
         VG_(printf)("nlist raw: avma %010lx  %s\n",
                     sym_addr, o_strtab + nl->n_un.n_strx );

      /* If no part of the symbol falls within the mapped range,
         ignore it. */
      if (sym_addr <= di->text_avma
          || sym_addr >= di->text_avma+di->text_size) {
         continue;
      }

      /* skip names which point outside the string table;
         following these risks segfaulting Valgrind */
      name = o_strtab + nl->n_un.n_strx;
      if (name < o_strtab || name >= o_strtab + o_strtab_sz)
         continue;

      /* skip nameless symbols; these appear to be common, but
         useless */
      if (*name == 0)
         continue;

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
   }
}


/* Compare DiSyms by their start address, and for equal addresses, use
   the primary name as a secondary sort key. */
static Int cmp_DiSym_by_start_then_name ( void* v1, void* v2 )
{
   DiSym* s1 = (DiSym*)v1;
   DiSym* s2 = (DiSym*)v2;
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


static Bool file_exists_p(const Char *path)
{
   struct vg_stat sbuf;
   SysRes res = VG_(stat)(path, &sbuf);
   return sr_isError(res) ? False : True;
}


/* Search for an existing dSYM file as a possible separate debug file.  
   Adapted from gdb. */
static Char *
find_separate_debug_file (const Char *executable_name)
{
   Char *basename_str;
   Char *dot_ptr;
   Char *slash_ptr;
   Char *dsymfile;
    
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


static UChar *getsectdata(UChar* base, SizeT size, 
                          Char *segname, Char *sectname,
                          /*OUT*/Word *sect_size)
{
   struct MACH_HEADER *mh = (struct MACH_HEADER *)base;
   struct load_command *cmd;          
   Int c;

   for (c = 0, cmd = (struct load_command *)(mh+1);
        c < mh->ncmds;
        c++, cmd = (struct load_command *)(cmd->cmdsize + (Addr)cmd))
   {
      if (cmd->cmd == LC_SEGMENT_CMD) {
         struct SEGMENT_COMMAND *seg = (struct SEGMENT_COMMAND *)cmd;
         if (0 == VG_(strncmp(seg->segname, segname, sizeof(seg->segname)))) {
            struct SECTION *sects = (struct SECTION *)(seg+1);
            Int s;
            for (s = 0; s < seg->nsects; s++) {
               if (0 == VG_(strncmp(sects[s].sectname, sectname, 
                                    sizeof(sects[s].sectname)))) 
               {
                  if (sect_size) *sect_size = sects[s].size;
                  return (UChar *)(base + sects[s].offset);
               }
            }
         }
      }
   }

   if (sect_size) *sect_size = 0;
   return 0;
}


/* Brute force just simply search for uuid[0..15] in img[0..n_img-1] */
static Bool check_uuid_matches ( Addr imgA, Word n_img, UChar* uuid )
{
   Word   i;
   UChar* img = (UChar*)imgA;
   UChar  first = uuid[0];
   if (n_img < 16)
      return False;
   for (i = 0; i < n_img-16; i++) {
      if (img[i] != first)
         continue;
      if (0 == VG_(memcmp)( &img[i], &uuid[0], 16 ))
         return True;
   }
   return False;
}


/* Heuristic kludge: return True if this looks like an installed
   standard library; hence we shouldn't consider automagically running
   dsymutil on it. */
static Bool is_systemish_library_name ( UChar* name )
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
   struct symtab_command *symcmd = NULL;
   struct dysymtab_command *dysymcmd = NULL;
   HChar* dsymfilename = NULL;
   Bool have_uuid = False;
   UChar uuid[16];
   ImageInfo ii;  /* main file */
   ImageInfo iid; /* auxiliary .dSYM file */
   Bool ok;

   /* mmap the object file to look for di->soname and di->text_bias 
      and uuid and nlist and STABS */

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg,
                   "%s (%#lx)\n", di->fsm.filename, di->fsm.rx_map_avma );

   /* This should be ensured by our caller (that we're in the accept
      state). */
   vg_assert(di->fsm.have_rx_map);
   vg_assert(di->fsm.have_rw_map);

   VG_(memset)(&ii,   0, sizeof(ii));
   VG_(memset)(&iid,  0, sizeof(iid));
   VG_(memset)(&uuid, 0, sizeof(uuid));

   ok = map_image_aboard( di, &ii, di->fsm.filename );
   if (!ok) goto fail;

   vg_assert(ii.macho_img != NULL && ii.macho_img_szB > 0);

   /* Poke around in the Mach-O header, to find some important
      stuff. */
   // Find LC_SYMTAB and LC_DYSYMTAB, if present.
   // Read di->soname from LC_ID_DYLIB if present, 
   //    or from LC_ID_DYLINKER if present, 
   //    or use "NONE".
   // Get di->text_bias (aka slide) based on the corresponding LC_SEGMENT
   // Get uuid for later dsym search

   di->text_bias = 0;

   { struct MACH_HEADER *mh = (struct MACH_HEADER *)ii.macho_img;
      struct load_command *cmd;
      Int c;

      for (c = 0, cmd = (struct load_command *)(mh+1);
           c < mh->ncmds;
           c++, cmd = (struct load_command *)(cmd->cmdsize
                                              + (unsigned long)cmd)) {
         if (cmd->cmd == LC_SYMTAB) {
            symcmd = (struct symtab_command *)cmd;
         } 
         else if (cmd->cmd == LC_DYSYMTAB) {
            dysymcmd = (struct dysymtab_command *)cmd;
         } 
         else if (cmd->cmd == LC_ID_DYLIB && mh->filetype == MH_DYLIB) {
            // GrP fixme bundle?
            struct dylib_command *dcmd = (struct dylib_command *)cmd;
            UChar *dylibname = dcmd->dylib.name.offset + (UChar *)dcmd;
            UChar *soname = VG_(strrchr)(dylibname, '/');
            if (!soname) soname = dylibname;
            else soname++;
            di->soname = ML_(dinfo_strdup)("di.readmacho.dylibname",
                                           soname);
         }
         else if (cmd->cmd==LC_ID_DYLINKER  &&  mh->filetype==MH_DYLINKER) {
            struct dylinker_command *dcmd = (struct dylinker_command *)cmd;
            UChar *dylinkername = dcmd->name.offset + (UChar *)dcmd;
            UChar *soname = VG_(strrchr)(dylinkername, '/');
            if (!soname) soname = dylinkername;
            else soname++;
            di->soname = ML_(dinfo_strdup)("di.readmacho.dylinkername",
                                           soname);
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
         else if (cmd->cmd == LC_SEGMENT_CMD) {
            struct SEGMENT_COMMAND *seg = (struct SEGMENT_COMMAND *)cmd;
            /* Try for __TEXT */
            if (!di->text_present
                && 0 == VG_(strcmp)(seg->segname, "__TEXT")
                /* DDD: is the  next line a kludge? -- JRS */
                && seg->fileoff == 0 && seg->filesize != 0) {
               di->text_present = True;
               di->text_svma = (Addr)seg->vmaddr;
               di->text_avma = di->fsm.rx_map_avma;
               di->text_size = seg->vmsize;
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
                && 0 == VG_(strcmp)(seg->segname, "__DATA")
                /* && DDD:seg->fileoff == 0 */ && seg->filesize != 0) {
               di->data_present = True;
               di->data_svma = (Addr)seg->vmaddr;
               di->data_avma = di->fsm.rw_map_avma;
               di->data_size = seg->vmsize;
               di->data_bias = di->data_avma - di->data_svma;
               di->data_debug_svma = di->data_svma;
               di->data_debug_bias = di->data_bias;
            }
         }
         else if (cmd->cmd == LC_UUID) {
             struct uuid_command *uuid_cmd = (struct uuid_command *)cmd;
             VG_(memcpy)(uuid, uuid_cmd->uuid, sizeof(uuid));
             have_uuid = True;
         }
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

   if (ii.macho_img && ii.macho_img_szB > 0 && symcmd && dysymcmd) {

      /* Read nlist symbol table */
      struct NLIST *syms;
      UChar *strs;
      XArray* /* DiSym */ candSyms = NULL;
      Word i, nCandSyms;

      if (ii.macho_img_szB < symcmd->stroff + symcmd->strsize
          || ii.macho_img_szB < symcmd->symoff + symcmd->nsyms
                                                 * sizeof(struct NLIST)) {
         ML_(symerr)(di, False, "Invalid Mach-O file (5 too small).");
         goto fail;
      }   
      if (dysymcmd->ilocalsym + dysymcmd->nlocalsym > symcmd->nsyms
          || dysymcmd->iextdefsym + dysymcmd->nextdefsym > symcmd->nsyms) {
         ML_(symerr)(di, False, "Invalid Mach-O file (bad symbol table).");
         goto fail;
      }
      
      syms = (struct NLIST *)(ii.macho_img + symcmd->symoff);
      strs = (UChar *)(ii.macho_img + symcmd->stroff);
      
      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg,
            "   reading syms   from primary file (%d %d)\n",
            dysymcmd->nextdefsym, dysymcmd->nlocalsym );

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
                  syms + dysymcmd->iextdefsym, dysymcmd->nextdefsym, 
                  strs, symcmd->strsize);
      // static and private_extern symbols
      read_symtab(candSyms,
                  di, 
                  syms + dysymcmd->ilocalsym, dysymcmd->nlocalsym, 
                  strs, symcmd->strsize);

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
      use the .macho_img and .macho_img_szB in iid. */

   dsymfilename = find_separate_debug_file( di->fsm.filename );

   /* Try to load it. */
   if (dsymfilename) {
      Bool valid;

      if (VG_(clo_verbosity) > 1)
         VG_(message)(Vg_DebugMsg, "   dSYM= %s\n", dsymfilename);

      ok = map_image_aboard( di, &iid, dsymfilename );
      if (!ok) goto fail;

      /* check it has the right uuid. */
      vg_assert(have_uuid);
      valid = iid.macho_img && iid.macho_img_szB > 0 
              && check_uuid_matches( (Addr)iid.macho_img,
                                     iid.macho_img_szB, uuid );
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
     HChar* dsymutil = "/usr/bin/dsymutil ";
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

      ok = map_image_aboard( di, &iid, dsymfilename );
      if (!ok) goto fail;

      /* check it has the right uuid. */
      vg_assert(have_uuid);
      valid = iid.macho_img && iid.macho_img_szB > 0 
              && check_uuid_matches( (Addr)iid.macho_img,
                                     iid.macho_img_szB, uuid );
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
         unmap_image( &iid );
         /* unmap_image zeroes the fields, so the following test makes
            sense. */
         goto fail;
      }
   }

   /* Right.  Finally we have our best try at the dwarf image, so go
      on to reading stuff out of it. */

  read_the_dwarf:
   if (iid.macho_img && iid.macho_img_szB > 0) {
      UChar* debug_info_img = NULL;
      Word   debug_info_sz;
      UChar* debug_abbv_img;
      Word   debug_abbv_sz;
      UChar* debug_line_img;
      Word   debug_line_sz;
      UChar* debug_str_img;
      Word   debug_str_sz;
      UChar* debug_ranges_img;
      Word   debug_ranges_sz;
      UChar* debug_loc_img;
      Word   debug_loc_sz;
      UChar* debug_name_img;
      Word   debug_name_sz;

      debug_info_img = 
          getsectdata(iid.macho_img, iid.macho_img_szB, 
                      "__DWARF", "__debug_info", &debug_info_sz);
      debug_abbv_img = 
          getsectdata(iid.macho_img, iid.macho_img_szB, 
                      "__DWARF", "__debug_abbrev", &debug_abbv_sz);
      debug_line_img = 
          getsectdata(iid.macho_img, iid.macho_img_szB, 
                      "__DWARF", "__debug_line", &debug_line_sz);
      debug_str_img = 
          getsectdata(iid.macho_img, iid.macho_img_szB, 
                      "__DWARF", "__debug_str", &debug_str_sz);
      debug_ranges_img = 
          getsectdata(iid.macho_img, iid.macho_img_szB, 
                      "__DWARF", "__debug_ranges", &debug_ranges_sz);
      debug_loc_img = 
          getsectdata(iid.macho_img, iid.macho_img_szB, 
                      "__DWARF", "__debug_loc", &debug_loc_sz);
      debug_name_img = 
          getsectdata(iid.macho_img, iid.macho_img_szB, 
                      "__DWARF", "__debug_pubnames", &debug_name_sz);
   
      if (debug_info_img) {
         if (VG_(clo_verbosity) > 1) {
            if (0)
            VG_(message)(Vg_DebugMsg,
                         "Reading dwarf3 for %s (%#lx) from %s"
                         " (%ld %ld %ld %ld %ld %ld)\n",
                         di->fsm.filename, di->text_avma, dsymfilename,
                         debug_info_sz, debug_abbv_sz, debug_line_sz, 
                         debug_str_sz, debug_ranges_sz, debug_loc_sz
                         );
            VG_(message)(Vg_DebugMsg,
               "   reading dwarf3 from dsyms file\n");
         }
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
   }

   if (dsymfilename) ML_(dinfo_free)(dsymfilename);

  success:
   if (ii.img)
      unmap_image(&ii);
   if (iid.img)
      unmap_image(&iid);
   return True;

   /* NOTREACHED */

  fail:
   ML_(symerr)(di, True, "Error reading Mach-O object.");
   if (ii.img)
      unmap_image(&ii);
   if (iid.img)
      unmap_image(&iid);
   return False;
}

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
