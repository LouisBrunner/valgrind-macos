/*
  13 Dec '05
  Linker no longer used - apart from mymalloc().
  Instead, simply compile and link switchback.c with test_xxx.c, e.g.:
  ./> (cd .. && make EXTRA_CFLAGS="-m64" libvex_ppc64_linux.a) && gcc -m64 -Wall -O -g -o switchback switchback.c linker.c ../libvex_ppc64_linux.a test_bzip2.c
*/


#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <elf.h>
#include <fcntl.h>
#include <string.h>
//#include <malloc.h>

#include "linker.h"

#include "../pub/libvex_basictypes.h"

#if 0
#define IF_DEBUG(x,y) /* */
static int debug_linker = 0;
#endif


#if defined(__x86_64__)
#   define x86_64_TARGET_ARCH
#elif defined(__i386__)
#   define i386_TARGET_ARCH
#elif defined (__powerpc__)
#   define ppc32_TARGET_ARCH
#else
#   error "Unknown arch"
#endif


#if 0
#define CALLOC_MAX 10000000
static HChar calloc_area[CALLOC_MAX];
static UInt calloc_used = 0;
static void* calloc_below2G ( Int n, Int m )
{
   void* p;
   int i;
   while ((calloc_used % 16) > 0) calloc_used++;
   assert(calloc_used + n*m < CALLOC_MAX);
   p = &calloc_area[calloc_used];
   for (i = 0; i < n*m; i++)
     calloc_area[calloc_used+i] = 0;
   calloc_used += n*m;
   return p;
}
#endif

#define MYMALLOC_MAX 50*1000*1000
static HChar mymalloc_area[MYMALLOC_MAX];
static UInt  mymalloc_used = 0;
void* mymalloc ( Int n )
{
   void* p;
#if defined(__powerpc64__)
   while ((ULong)(mymalloc_area+mymalloc_used) & 0xFFF)
#else
   while ((UInt)(mymalloc_area+mymalloc_used) & 0xFFF)
#endif
      mymalloc_used++;
   assert(mymalloc_used+n < MYMALLOC_MAX);
   p = (void*)(&mymalloc_area[mymalloc_used]);
   mymalloc_used += n;
   //   printf("mymalloc(%d) = %p\n", n, p);
   return p;
}

void myfree ( void* p )
{
}







#if 0
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// TYPES

#define FALSE 0
#define TRUE  1

typedef enum { OBJECT_LOADED, OBJECT_RESOLVED } OStatus;


#define N_FIXUP_PAGES 1


/* Indication of section kinds for loaded objects.  Needed by
   the GC for deciding whether or not a pointer on the stack
   is a code pointer.
*/
typedef 
   enum { SECTIONKIND_CODE_OR_RODATA,
          SECTIONKIND_RWDATA,
          SECTIONKIND_OTHER,
          SECTIONKIND_NOINFOAVAIL } 
   SectionKind;

typedef 
   struct _Section { 
      void* start; 
      void* end; 
      SectionKind kind;
      struct _Section* next;
   } 
   Section;

typedef 
   struct _ProddableBlock {
      void* start;
      int   size;
      struct _ProddableBlock* next;
   }
   ProddableBlock;

/* Top-level structure for an object module.  One of these is allocated
 * for each object file in use.
 */
typedef struct _ObjectCode {
    OStatus    status;
    char*      fileName;
    int        fileSize;
    char*      formatName;            /* eg "ELF32", "DLL", "COFF", etc. */

    /* An array containing ptrs to all the symbol names copied from
       this object into the global symbol hash table.  This is so that
       we know which parts of the latter mapping to nuke when this
       object is removed from the system. */
    char**     symbols;
    int        n_symbols;

    /* ptr to malloc'd lump of memory holding the obj file */
    void*      image;

    /* Fixup area for long-distance jumps. */
    char*      fixup;
    int        fixup_used;
    int        fixup_size;

    /* The section-kind entries for this object module.  Linked
       list. */
    Section* sections;

    /* A private hash table for local symbols. */
    /* HashTable* */ void* lochash;
    
    /* Allow a chain of these things */
    struct _ObjectCode * next;

    /* SANITY CHECK ONLY: a list of the only memory regions which may
       safely be prodded during relocation.  Any attempt to prod
       outside one of these is an error in the linker. */
    ProddableBlock* proddables;

} ObjectCode;

/*
 * Define a set of types which can be used for both ELF32 and ELF64
 */

#if VEX_HOST_WORDSIZE == 8
#define ELFCLASS    ELFCLASS64
#define Elf_Addr    Elf64_Addr
#define Elf_Word    Elf64_Word
#define Elf_Sword   Elf64_Sword
#define Elf_Ehdr    Elf64_Ehdr
#define Elf_Phdr    Elf64_Phdr
#define Elf_Shdr    Elf64_Shdr
#define Elf_Sym     Elf64_Sym
#define Elf_Rel     Elf64_Rel
#define Elf_Rela    Elf64_Rela
#define ELF_ST_TYPE ELF64_ST_TYPE
#define ELF_ST_BIND ELF64_ST_BIND
#define ELF_R_TYPE  ELF64_R_TYPE
#define ELF_R_SYM   ELF64_R_SYM
#else
#define ELFCLASS    ELFCLASS32
#define Elf_Addr    Elf32_Addr
#define Elf_Word    Elf32_Word
#define Elf_Sword   Elf32_Sword
#define Elf_Ehdr    Elf32_Ehdr
#define Elf_Phdr    Elf32_Phdr
#define Elf_Shdr    Elf32_Shdr
#define Elf_Sym     Elf32_Sym
#define Elf_Rel     Elf32_Rel
#define Elf_Rela    Elf32_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF32_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF32_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF32_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF32_R_SYM
#endif
#endif




///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// PARANOIA

/* -----------------------------------------------------------------------
 * Sanity checking.  For each ObjectCode, maintain a list of address ranges
 * which may be prodded during relocation, and abort if we try and write
 * outside any of these.
 */
static void addProddableBlock ( ObjectCode* oc, void* start, int size )
{
   ProddableBlock* pb
      = mymalloc(sizeof(ProddableBlock));
   if (debug_linker)
      fprintf(stderr, "aPB oc=%p %p %d   (%p .. %p)\n", oc, start, size,
	      start, ((char*)start)+size-1 );
   assert(size > 0);
   pb->start      = start;
   pb->size       = size;
   pb->next       = oc->proddables;
   oc->proddables = pb;
}

static void checkProddableBlock ( ObjectCode* oc, void* addr )
{
   ProddableBlock* pb;
   for (pb = oc->proddables; pb != NULL; pb = pb->next) {
      char* s = (char*)(pb->start);
      char* e = s + pb->size - 1;
      char* a = (char*)addr;
      /* Assumes that the biggest fixup involves a 4-byte write.  This
         probably needs to be changed to 8 (ie, +7) on 64-bit
         plats. */
      if (a >= s && (a+3) <= e) return;
   }
   fprintf(stderr,
           "checkProddableBlock: invalid fixup %p in runtime linker\n",
           addr);
   exit(1);
}



///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// String->Addr mappings

typedef 
   struct { char* mp_name; void* mp_addr; } 
   Maplet;

typedef
   struct {
      int sm_size;
      int sm_used;
      Maplet* maplets;
   }
   StringMap;

static StringMap* new_StringMap ( void )
{
   StringMap* sm = mymalloc(sizeof(StringMap));
   sm->sm_size = 10;
   sm->sm_used = 0;
   sm->maplets = mymalloc(10 * sizeof(Maplet));
   return sm;
}

static void delete_StringMap ( StringMap* sm )
{
   assert(sm->maplets != NULL);
   myfree(sm->maplets);
   sm->maplets = NULL;
   myfree(sm);
}

static void ensure_StringMap ( StringMap* sm )
{
   int i;
   Maplet* mp2;
   assert(sm->maplets != NULL);
   if (sm->sm_used < sm->sm_size)
     return;
   sm->sm_size *= 2;
   mp2 = mymalloc(sm->sm_size * sizeof(Maplet));
   for (i = 0; i < sm->sm_used; i++)
      mp2[i] = sm->maplets[i];
   myfree(sm->maplets);
   sm->maplets = mp2;
}

static void* search_StringMap ( StringMap* sm, char* name )
{
   int i;
   for (i = 0; i < sm->sm_used; i++)
      if (0 == strcmp(name, sm->maplets[i].mp_name))
         return sm->maplets[i].mp_addr;
   return NULL;
}

static void addto_StringMap ( StringMap* sm, char* name, void* addr )
{
   ensure_StringMap(sm);
   sm->maplets[sm->sm_used].mp_name = name;
   sm->maplets[sm->sm_used].mp_addr = addr;
   sm->sm_used++;
}

static void paranoid_addto_StringMap ( StringMap* sm, char* name, void* addr )
{
   if (0)
       fprintf(stderr, "paranoid_addto_StringMap(%s,%p)\n", name, addr);
   if (search_StringMap(sm,name) != NULL) {
      fprintf(stderr, "duplicate: paranoid_addto_StringMap(%s,%p)\n", name, addr);
      exit(1);
   }
   addto_StringMap(sm,name,addr);
}


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// Top-level linker control.

StringMap*  global_symbol_table = NULL;
ObjectCode* global_object_list = NULL;

static void initLinker ( void )
{
   if (global_symbol_table != NULL)
      return;
   global_symbol_table = new_StringMap();
}



///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// SYMBOL TABLE(s)

/* -----------------------------------------------------------------
 * lookup a symbol in the global symbol table
 */
static 
void * lookupSymbol( char *lbl )
{
   void *val;
   initLinker() ;
   assert(global_symbol_table != NULL);
   val = search_StringMap(global_symbol_table, lbl);
   return val;
}


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// HELPERS

/*
 * Generic ELF functions
 */

static char *
findElfSection ( void* objImage, Elf_Word sh_type )
{
   char* ehdrC = (char*)objImage;
   Elf_Ehdr* ehdr = (Elf_Ehdr*)ehdrC;
   Elf_Shdr* shdr = (Elf_Shdr*)(ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   char* ptr = NULL;
   int i;

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == sh_type
          /* Ignore the section header's string table. */
          && i != ehdr->e_shstrndx
	  /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         ptr = ehdrC + shdr[i].sh_offset;
         break;
      }
   }
   return ptr;
}

#ifdef arm_TARGET_ARCH
static
char* alloc_fixup_bytes ( ObjectCode* oc, int nbytes )
{
   char* res;
   assert(nbytes % 4 == 0);
   assert(nbytes > 0);
   res = &(oc->fixup[oc->fixup_used]);
   oc->fixup_used += nbytes;
   if (oc->fixup_used >= oc->fixup_size) {
     fprintf(stderr, "fixup area too small for %s\n", oc->fileName);
     exit(1);
   }
   return res;
}
#endif


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// RESOLVE

static
void* lookup_magic_hacks ( char* sym )
{
   if (0==strcmp(sym, "printf")) return (void*)(&printf);
   return NULL;
}

#ifdef arm_TARGET_ARCH
static
void arm_notify_new_code ( char* start, int length )
{
  __asm __volatile ("mov r1, %0\n\t"
                    "mov r2, %1\n\t"
                    "mov r3, %2\n\t"
                    "swi 0x9f0002\n\t"
                    : 
                    : "ir" (start), "ir" (length), "ir" (0) );
}


static
void gen_armle_goto ( char* fixup, char* dstP )
{
  Elf_Word w = (Elf_Word)dstP;
  /* 
   2                    .text
   3 0000 04F01FE5              ldr     pc, value
   4 0004 44332211      value:  .word   0x11223344
   */
  fprintf(stderr,"at %p generating jump to %p\n", fixup, dstP );
  fixup[0] = 0x04; fixup[1] = 0xF0; fixup[2] = 0x1F; fixup[3] = 0xE5;
  fixup[4] = w & 0xFF; w >>= 8;
  fixup[5] = w & 0xFF; w >>= 8;
  fixup[6] = w & 0xFF; w >>= 8;
  fixup[7] = w & 0xFF; w >>= 8;
  arm_notify_new_code(fixup, 8);
}
#endif /* arm_TARGET_ARCH */


#ifdef ppc32_TARGET_ARCH
static void invalidate_icache(void *ptr, int nbytes)
{
   unsigned long startaddr = (unsigned long) ptr;
   unsigned long endaddr = startaddr + nbytes;
   unsigned long addr;
   unsigned long cls = 16; //VG_(cache_line_size);

   startaddr &= ~(cls - 1);
   for (addr = startaddr; addr < endaddr; addr += cls)
      asm volatile("dcbst 0,%0" : : "r" (addr));
   asm volatile("sync");
   for (addr = startaddr; addr < endaddr; addr += cls)
      asm volatile("icbi 0,%0" : : "r" (addr));
   asm volatile("sync; isync");
}

static UInt compute_ppc_HA ( UInt x ) {
   return 0xFFFF & ( (x >> 16) + ((x & 0x8000) ? 1 : 0) );
}
static UInt compute_ppc_LO ( UInt x ) {
   return 0xFFFF & x;
}
static UInt compute_ppc_HI ( UInt x ) {
   return 0xFFFF & (x >> 16);
}
#endif /* ppc32_TARGET_ARCH */


/* Do ELF relocations which lack an explicit addend.  All x86-linux
   relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         Elf_Shdr* shdr, int shnum,
                         Elf_Sym*  stab, char* strtab )
{
   int j;
   char *symbol = NULL;
   Elf_Word* targ;
   Elf_Rel*  rtab = (Elf_Rel*) (ehdrC + shdr[shnum].sh_offset);
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   targ  = (Elf_Word*)(ehdrC + shdr[ target_shndx ].sh_offset);
   IF_DEBUG(linker,belch( "relocations for section %d using symtab %d",
                          target_shndx, symtab_shndx ));

   for (j = 0; j < nent; j++) {
      Elf_Addr offset = rtab[j].r_offset;
      Elf_Addr info   = rtab[j].r_info;

      Elf_Addr  P  = ((Elf_Addr)targ) + offset;
      Elf_Word* pP = (Elf_Word*)P;
      Elf_Addr  A  = *pP;
      Elf_Addr  S;
      Elf_Addr  value;

      IF_DEBUG(linker,belch( "Rel entry %3d is raw(%6p %6p)",
                             j, (void*)offset, (void*)info ));
      if (!info) {
         IF_DEBUG(linker,belch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
	 /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            S = (Elf_Addr)
                (ehdrC + shdr[ sym.st_shndx ].sh_offset
                       + stab[ELF_R_SYM(info)].st_value);

	 } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            S = (Elf_Addr)lookupSymbol( symbol );
	 }
         if (!S) {
            S = (Elf_Addr)lookup_magic_hacks(symbol);
         }
         if (!S) {
            fprintf(stderr,"%s: unknown symbol `%s'\n", 
                           oc->fileName, symbol);
	    return 0;
         }
         if (debug_linker>1) 
            fprintf(stderr, "\n`%s' resolves to %p\n", symbol, (void*)S );
      }

      if (debug_linker>1)
         fprintf(stderr, "Reloc: P = %p   S = %p   A = %p\n",
			     (void*)P, (void*)S, (void*)A );
      checkProddableBlock ( oc, pP );

      value = S + A;

      switch (ELF_R_TYPE(info)) {
#        ifdef i386_TARGET_ARCH
         case R_386_32:   *pP = value;     break;
         case R_386_PC32: *pP = value - P; break;
#        endif
#        ifdef arm_TARGET_ARCH
         case R_ARM_PC24: {
	    Elf_Word w, delta, deltaTop8;
	    /* Generate a jump sequence into the fixup area
	       and branch to that instead. */
 	    char* fixup = alloc_fixup_bytes(oc, 8);
            /* First of all, figure out where we're really trying to
               jump to. */
            // compensate for pc+8 bias
            Elf_Word real_dst = (A & 0x00FFFFFF) + 2;
	    // sign-extend 24-to-32 of real_dst
            if (real_dst & 0x00800000) 
               real_dst |= 0xFF000000;
            else
               real_dst &= 0x00FFFFFF;

            real_dst <<= 2;
	    real_dst += S;

	    gen_armle_goto(fixup, (char*)real_dst);

	    /* Delta is in bytes .. */
            delta = (((Elf_Word)fixup) - ((Elf_Word)pP) - 8);
            deltaTop8 = (delta >> 24) & 0xFF;
            if (deltaTop8 != 0 && deltaTop8 != 0xFF) {
	      fprintf(stderr,"R_ARM_PC24: out of range delta 0x%x for %s\n",
		      delta, symbol);
	      exit(1);
	    }
            delta >>= 2;
	    w = *pP;
            w &= 0xFF000000;
            w |= (0x00FFFFFF & delta );
            *pP = w;
	    break;
         }
         case R_ARM_ABS32:
	    *pP = value;
	    break;
#        endif
         default:
            fprintf(stderr,
                    "%s: unhandled ELF relocation(Rel) type %d\n\n",
		    oc->fileName, (Int)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

/* Do ELF relocations for which explicit addends are supplied.
   sparc-solaris relocations appear to be of this form. */
static int
do_Elf_Rela_relocations ( ObjectCode* oc, char* ehdrC,
                          Elf_Shdr* shdr, int shnum,
                          Elf_Sym*  stab, char* strtab )
{
   int j;
   char *symbol;
   Elf_Addr targ;
   Elf_Rela* rtab = (Elf_Rela*) (ehdrC + shdr[shnum].sh_offset);
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rela);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   targ  = (Elf_Addr) (ehdrC + shdr[ target_shndx ].sh_offset);
   IF_DEBUG(linker,belch( "relocations for section %d using symtab %d",
                          target_shndx, symtab_shndx ));

   for (j = 0; j < nent; j++) {
#if defined(DEBUG) || defined(sparc_TARGET_ARCH)  \
                   || defined(ia64_TARGET_ARCH)   \
                   || defined(x86_64_TARGET_ARCH) \
                   || defined(ppc32_TARGET_ARCH)
      /* This #ifdef only serves to avoid unused-var warnings. */
      Elf_Addr  offset = rtab[j].r_offset;
      Elf_Addr  P      = targ + offset;
#endif
      Elf_Addr  info   = rtab[j].r_info;
      Elf_Addr  A      = rtab[j].r_addend;
      Elf_Addr  S =0;
      Elf_Addr  value;
#     if defined(sparc_TARGET_ARCH)
      Elf_Word* pP = (Elf_Word*)P;
      Elf_Word  w1, w2;
#     endif
#     if defined(ia64_TARGET_ARCH)
      Elf64_Xword *pP = (Elf64_Xword *)P;
      Elf_Addr addr;
#     endif
#     if defined(x86_64_TARGET_ARCH)
      ULong* pP = (ULong*)P;
#     endif
#     if defined(ppc32_TARGET_ARCH)
      Int sI, sI2;
      Elf_Word* pP = (Elf_Word*)P;
#     endif

      IF_DEBUG(linker,belch( "Rel entry %3d is raw(%6p %6p %6p)   ",
                             j, (void*)offset, (void*)info,
                                (void*)A ));
      if (!info) {
         IF_DEBUG(linker,belch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
	 /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            S = (Elf_Addr)
                (ehdrC + shdr[ sym.st_shndx ].sh_offset
                       + stab[ELF_R_SYM(info)].st_value);
#ifdef ELF_FUNCTION_DESC
	    /* Make a function descriptor for this function */
            if (S && ELF_ST_TYPE(sym.st_info) == STT_FUNC) {
               S = allocateFunctionDesc(S + A);
       	       A = 0;
            }
#endif
	 } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            S = (Elf_Addr)lookupSymbol( symbol );

#ifdef ELF_FUNCTION_DESC
	    /* If a function, already a function descriptor - we would
	       have to copy it to add an offset. */
            if (S && (ELF_ST_TYPE(sym.st_info) == STT_FUNC) && (A != 0))
               belch("%s: function %s with addend %p", oc->fileName, symbol, (void *)A);
#endif
	 }
         if (!S) {
	   fprintf(stderr,"%s: unknown symbol `%s'\n", oc->fileName, symbol);
	   return 0;
         }
         if (0)
            fprintf(stderr, "`%s' resolves to %p\n", symbol, (void*)S );
      }

#if 0
         fprintf ( stderr, "Reloc: offset = %p   P = %p   S = %p   A = %p\n",
                           (void*)offset, (void*)P, (void*)S, (void*)A );
#endif

      /* checkProddableBlock ( oc, (void*)P ); */

      value = S + A;

      switch (ELF_R_TYPE(info)) {
#        if defined(sparc_TARGET_ARCH)
         case R_SPARC_WDISP30:
            w1 = *pP & 0xC0000000;
            w2 = (Elf_Word)((value - P) >> 2);
            ASSERT((w2 & 0xC0000000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_HI22:
            w1 = *pP & 0xFFC00000;
            w2 = (Elf_Word)(value >> 10);
            ASSERT((w2 & 0xFFC00000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_LO10:
            w1 = *pP & ~0x3FF;
            w2 = (Elf_Word)(value & 0x3FF);
            ASSERT((w2 & ~0x3FF) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         /* According to the Sun documentation:
            R_SPARC_UA32
            This relocation type resembles R_SPARC_32, except it refers to an
            unaligned word. That is, the word to be relocated must be treated
            as four separate bytes with arbitrary alignment, not as a word
            aligned according to the architecture requirements.

            (JRS: which means that freeloading on the R_SPARC_32 case
            is probably wrong, but hey ...)
         */
         case R_SPARC_UA32:
         case R_SPARC_32:
            w2 = (Elf_Word)value;
            *pP = w2;
            break;
#        endif
#        if defined(ia64_TARGET_ARCH)
	 case R_IA64_DIR64LSB:
	 case R_IA64_FPTR64LSB:
	    *pP = value;
	    break;
	 case R_IA64_PCREL64LSB:
	    *pP = value - P;
	    break;
	 case R_IA64_SEGREL64LSB:
	    addr = findElfSegment(ehdrC, value);
	    *pP = value - addr;
	    break;
	 case R_IA64_GPREL22:
	    ia64_reloc_gprel22(P, value);
	    break;
	 case R_IA64_LTOFF22:
	 case R_IA64_LTOFF22X:
	 case R_IA64_LTOFF_FPTR22:
	    addr = allocateGOTEntry(value);
	    ia64_reloc_gprel22(P, addr);
	    break;
	 case R_IA64_PCREL21B:
	    ia64_reloc_pcrel21(P, S, oc);
	    break;
	 case R_IA64_LDXMOV:
	    /* This goes with R_IA64_LTOFF22X and points to the load to
	       convert into a move.  We don't implement relaxation. */
	    break;
#        endif
#        if defined(x86_64_TARGET_ARCH)
         case R_X86_64_64: /* 1 *//* Direct 64 bit  */
            *((ULong*)pP) = (ULong)(S + A);
            break;
         case R_X86_64_PC32: /* 2 *//* PC relative 32 bit signed */
            *((UInt*)pP) = (UInt)(S + A - P);
            break;
         case R_X86_64_32: /* 10 *//* Direct 32 bit zero extended */
            *((UInt*)pP) = (UInt)(S + A);
            break;
         case R_X86_64_32S: /* 11 *//* Direct 32 bit sign extended */
            *((UInt*)pP) = (UInt)(S + A);
            break;
#        endif
#        if defined(ppc32_TARGET_ARCH)
         case R_PPC_ADDR32: /* 1 *//* 32bit absolute address */
            *((UInt*)pP) = S+A;
            invalidate_icache(pP,4);
            break;
         case R_PPC_ADDR16_LO: /* 4 *//* lower 16bit of absolute address */
            *((UInt*)pP) &= 0x0000FFFF;
            *((UInt*)pP) |= 0xFFFF0000 & (compute_ppc_LO(S+A) << 16);
            invalidate_icache(pP,4);
            break;
         case R_PPC_ADDR16_HA: /* 6 *//* adjusted high 16bit */
            *((UInt*)pP) &= 0x0000FFFF;
            *((UInt*)pP) |= 0xFFFF0000 & (compute_ppc_HA(S+A) << 16);
            invalidate_icache(pP,4);
            break;
         case R_PPC_REL24: /* 10 *//* PC relative 26 bit */
            sI = S+A-P;
	    sI >>= 2;
	    /* the top 9 bits of sI must be the same (all 0s or
	       all 1s) for this to be valid; else we have to fail. */
            sI2 = sI >> 23; /* 23 == 32 - 9 */
            if (sI2 != 0 && sI2 != 0xFFFFFFFF) {
               fprintf(stderr, "%s: R_PPC_REL24 relocation failed\n", oc->fileName );
	       return 0;
            }
            *((UInt*)pP) &= ~(0x00FFFFFF << 2);
            *((UInt*)pP) |= (0xFFFFFF & sI) << 2;
           invalidate_icache(pP,4);
            break;
         case R_PPC_REL32: /* 26 */
            *((UInt*)pP) = S+A-P;
            invalidate_icache(pP,4);
            break;
#        endif
         default:
            fprintf(stderr,
                    "%s: unhandled ELF relocation(RelA) type %d\n",
		    oc->fileName, (Int)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}


static int
ocResolve_ELF ( ObjectCode* oc )
{
   char *strtab;
   int   shnum, ok;
   Elf_Sym*  stab  = NULL;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;

   /* first find "the" symbol table */
   stab = (Elf_Sym*) findElfSection ( ehdrC, SHT_SYMTAB );

   /* also go find the string table */
   strtab = findElfSection ( ehdrC, SHT_STRTAB );

   if (stab == NULL || strtab == NULL) {
      fprintf(stderr,"%s: can't find string or symbol table\n", oc->fileName);
      return 0;
   }

   /* Process the relocation sections. */
   for (shnum = 0; shnum < ehdr->e_shnum; shnum++) {

      /* Skip sections called ".rel.stab".  These appear to contain
         relocation entries that, when done, make the stabs debugging
         info point at the right places.  We ain't interested in all
         dat jazz, mun. */
      if (0 == memcmp(".rel.stab", sh_strtab + shdr[shnum].sh_name, 9))
         continue;

      if (shdr[shnum].sh_type == SHT_REL ) {
         ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr,
                                       shnum, stab, strtab );
         if (!ok) return ok;
      }
      else
      if (shdr[shnum].sh_type == SHT_RELA) {
         ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr,
                                        shnum, stab, strtab );
         if (!ok) return ok;
      }
   }

   /* Free the local symbol table; we won't need it again. */
   delete_StringMap(oc->lochash);
   oc->lochash = NULL;

   return 1;
}


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// VERIFY

static int
ocVerifyImage_ELF ( ObjectCode* oc )
{
   Elf_Shdr* shdr;
   Elf_Sym*  stab;
   int i, j, nent, nstrtab, nsymtabs;
   char* sh_strtab;
   char* strtab;

   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*)ehdrC;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
       ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
       ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
       ehdr->e_ident[EI_MAG3] != ELFMAG3) {
      fprintf(stderr,"%s: not an ELF object\n", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_CLASS] != ELFCLASS) {
      fprintf(stderr,"%s: unsupported ELF format\n", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_DATA] == ELFDATA2LSB) {
      if (debug_linker)
         fprintf(stderr, "Is little-endian\n" );
   } else
   if (ehdr->e_ident[EI_DATA] == ELFDATA2MSB) {
       if (debug_linker)
          fprintf(stderr, "Is big-endian\n" );
   } else {
       fprintf(stderr,"%s: unknown endiannness\n", oc->fileName);
       return 0;
   }

   if (ehdr->e_type != ET_REL) {
      fprintf(stderr,"%s: not a relocatable object (.o) file\n", oc->fileName);
      return 0;
   }
   if (debug_linker)
      fprintf(stderr, "Is a relocatable object (.o) file\n" );

   if (debug_linker)
      fprintf(stderr, "Architecture is " );
   switch (ehdr->e_machine) {
      case EM_386:    if (debug_linker) fprintf(stderr, "x86\n" ); break;
      case EM_SPARC:  if (debug_linker) fprintf(stderr, "sparc\n" ); break;
      case EM_ARM:    if (debug_linker) fprintf(stderr, "arm\n" ); break;
#ifdef EM_IA_64
      case EM_IA_64:  if (debug_linker) fprintf(stderr, "ia64\n" ); break;
#endif
      case EM_X86_64: if (debug_linker) fprintf(stderr, "x86_64\n" ); break;
      case EM_PPC:    if (debug_linker) fprintf(stderr, "ppc\n" ); break;
      default:        if (debug_linker) fprintf(stderr, "unknown\n" );
                      fprintf(stderr,"%s: unknown architecture\n", oc->fileName);
                      return 0;
   }

   if (debug_linker>1) fprintf(stderr,
             "\nSection header table: start %lld, n_entries %d, ent_size %d\n",
             (Long)ehdr->e_shoff, 
             ehdr->e_shnum, ehdr->e_shentsize  );

   assert (ehdr->e_shentsize == sizeof(Elf_Shdr));

   shdr = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   if (ehdr->e_shstrndx == SHN_UNDEF) {
      fprintf(stderr,"%s: no section header string table\n", oc->fileName);
      return 0;
   } else {
      if (debug_linker>1) 
         fprintf(stderr, "Section header string table is section %d\n",
                          ehdr->e_shstrndx);
      sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   }

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (debug_linker>1) fprintf(stderr, "%2d:  ", i );
      if (debug_linker>1) fprintf(stderr, "type=%2d  ", (int)shdr[i].sh_type );
      if (debug_linker>1) fprintf(stderr, "size=%4d  ", (int)shdr[i].sh_size );
      if (debug_linker>1) fprintf(stderr, "offs=%4d  ", (int)shdr[i].sh_offset );
      if (debug_linker>1) fprintf(stderr, "  (%p .. %p)  ",
               ehdrC + shdr[i].sh_offset,
		      ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1);

      if (shdr[i].sh_type == SHT_REL) {
	  if (debug_linker>1) fprintf(stderr, "Rel  " );
      } else if (shdr[i].sh_type == SHT_RELA) {
	  if (debug_linker>1) fprintf(stderr, "RelA " );
      } else {
	  if (debug_linker>1) fprintf(stderr,"     ");
      }
      if (sh_strtab) {
	  if (debug_linker>1) fprintf(stderr, "sname=%s\n", 
             sh_strtab + shdr[i].sh_name );
      }
   }

   if (debug_linker>1) fprintf(stderr, "\nString tables\n" );
   strtab = NULL;
   nstrtab = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB
          /* Ignore the section header's string table. */
          && i != ehdr->e_shstrndx
	  /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         if (debug_linker>1) 
            fprintf(stderr,"   section %d is a normal string table\n", i );
         strtab = ehdrC + shdr[i].sh_offset;
         nstrtab++;
      }
   }
   if (nstrtab != 1) {
      fprintf(stderr,"%s: no string tables, or too many\n", oc->fileName);
      return 0;
   }

   nsymtabs = 0;
   if (debug_linker>1) fprintf(stderr, "\nSymbol tables\n" );
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type != SHT_SYMTAB) continue;
      if (debug_linker>1) fprintf(stderr, "section %d is a symbol table\n", i );
      nsymtabs++;
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf_Sym);
      if (debug_linker>1) fprintf(stderr,  
            "   number of entries is apparently %d (%lld rem)\n",
               nent,
               (Long)(shdr[i].sh_size % sizeof(Elf_Sym))
             );
      if (0 != shdr[i].sh_size % sizeof(Elf_Sym)) {
         fprintf(stderr,"%s: non-integral number of symbol table entries\n", 
                        oc->fileName);
         return 0;
      }
      for (j = 0; j < nent; j++) {
         if (debug_linker>1) fprintf(stderr, "   %2d  ", j );
         if (debug_linker>1) fprintf(stderr, "  sec=%-5d  size=%-3d  val=%5p  ",
                             (int)stab[j].st_shndx,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value );

         if (debug_linker>1) fprintf(stderr, "type=" );
         switch (ELF_ST_TYPE(stab[j].st_info)) {
            case STT_NOTYPE:  if (debug_linker>1) fprintf(stderr, "notype " ); break;
            case STT_OBJECT:  if (debug_linker>1) fprintf(stderr, "object " ); break;
            case STT_FUNC  :  if (debug_linker>1) fprintf(stderr, "func   " ); break;
            case STT_SECTION: if (debug_linker>1) fprintf(stderr, "section" ); break;
            case STT_FILE:    if (debug_linker>1) fprintf(stderr, "file   " ); break;
            default:          if (debug_linker>1) fprintf(stderr, "?      " ); break;
         }
         if (debug_linker>1) fprintf(stderr, "  " );

         if (debug_linker>1) fprintf(stderr, "bind=" );
         switch (ELF_ST_BIND(stab[j].st_info)) {
            case STB_LOCAL :  if (debug_linker>1) fprintf(stderr, "local " ); break;
            case STB_GLOBAL:  if (debug_linker>1) fprintf(stderr, "global" ); break;
            case STB_WEAK  :  if (debug_linker>1) fprintf(stderr, "weak  " ); break;
            default:          if (debug_linker>1) fprintf(stderr, "?     " ); break;
         }
         if (debug_linker>1) fprintf(stderr, "  " );

         if (debug_linker>1) fprintf(stderr, "name=%s\n", strtab + stab[j].st_name );
      }
   }

   if (nsymtabs == 0) {
      fprintf(stderr,"%s: didn't find any symbol tables\n", oc->fileName);
      return 0;
   }

   return 1;
}


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// GETNAMES

static int
ocGetNames_ELF ( ObjectCode* oc )
{
   int i, j, k, nent;
   Elf_Sym* stab;

   char*     ehdrC     = (char*)(oc->image);
   Elf_Ehdr* ehdr      = (Elf_Ehdr*)ehdrC;
   char*     strtab    = findElfSection ( ehdrC, SHT_STRTAB );
   Elf_Shdr* shdr      = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   char*     sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   char*     sec_name;

   assert(global_symbol_table != NULL);

   if (!strtab) {
      fprintf(stderr,"%s: no strtab\n", oc->fileName);
      return 0;
   }

   k = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      /* Figure out what kind of section it is.  Logic derived from
         Figure 1.14 ("Special Sections") of the ELF document
         ("Portable Formats Specification, Version 1.1"). */
      Elf_Shdr    hdr    = shdr[i];
      SectionKind kind   = SECTIONKIND_OTHER;
      int         is_bss = FALSE;

      if (hdr.sh_type == SHT_PROGBITS
          && (hdr.sh_flags & SHF_ALLOC) && (hdr.sh_flags & SHF_EXECINSTR)) {
         /* .text-style section */
         kind = SECTIONKIND_CODE_OR_RODATA;
      }
      else
      if (hdr.sh_type == SHT_PROGBITS
          && (hdr.sh_flags & SHF_ALLOC) && (hdr.sh_flags & SHF_WRITE)) {
         /* .data-style section */
         kind = SECTIONKIND_RWDATA;
      }
      else
      if (hdr.sh_type == SHT_PROGBITS
          && (hdr.sh_flags & SHF_ALLOC) && !(hdr.sh_flags & SHF_WRITE)) {
         /* .rodata-style section */
         kind = SECTIONKIND_CODE_OR_RODATA;
      }
      else
      if (hdr.sh_type == SHT_NOBITS
          && (hdr.sh_flags & SHF_ALLOC) && (hdr.sh_flags & SHF_WRITE)) {
         /* .bss-style section */
         kind = SECTIONKIND_RWDATA;
         is_bss = TRUE;
      }

      if (is_bss && shdr[i].sh_size > 0) {
         /* This is a non-empty .bss section.  Allocate zeroed space for
            it, and set its .sh_offset field such that
            ehdrC + .sh_offset == addr_of_zeroed_space.  */
         char* zspace = calloc(1, shdr[i].sh_size);
         shdr[i].sh_offset = ((char*)zspace) - ((char*)ehdrC);
	 if (1)
         fprintf(stderr, "BSS section at %p, size %lld\n",
                         zspace, (Long)shdr[i].sh_size);
      }

      /* When loading objects compiled with -g, it seems there are
	 relocations in various debug-info sections.  So we'd better
	 tell addProddableBlock to allow those bits to be prodded. */
      //fprintf(stderr, "ZZZZZZZZZZ %s\n", sh_strtab + hdr.sh_name);
      sec_name = sh_strtab + shdr[i].sh_name;
      if (kind == SECTIONKIND_OTHER
          && (0 == strcmp(".debug_info", sec_name)
              || 0 == strcmp(".debug_line", sec_name)
              || 0 == strcmp(".debug_pubnames", sec_name)
              || 0 == strcmp(".debug_aranges", sec_name)
              || 0 == strcmp(".debug_frame", sec_name))) {
         kind = SECTIONKIND_CODE_OR_RODATA;
      }

      /* fill in the section info */
      if (kind != SECTIONKIND_OTHER && shdr[i].sh_size > 0) {
         addProddableBlock(oc, ehdrC + shdr[i].sh_offset, shdr[i].sh_size);
         //addSection(oc, kind, ehdrC + shdr[i].sh_offset,
         //               ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1);
      }

      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf_Sym);

      oc->n_symbols = nent;
      oc->symbols = mymalloc(oc->n_symbols * sizeof(char*));

      for (j = 0; j < nent; j++) {

         char  isLocal = FALSE; /* avoids uninit-var warning */
         char* ad      = NULL;
         char* nm      = strtab + stab[j].st_name;
         int   secno   = stab[j].st_shndx;

	 /* Figure out if we want to add it; if so, set ad to its
            address.  Otherwise leave ad == NULL. */

         if (secno == SHN_COMMON) {
            isLocal = FALSE;
#           if defined(__x86_64__)
            ad = calloc_below2G(1, stab[j].st_size);
#           else
            ad = calloc(1, stab[j].st_size);
#           endif
    //	    assert( Ptr_to_ULong(ad) < 0xF0000000ULL );

	    if (0)
            fprintf(stderr, "COMMON symbol, size %lld name %s  allocd %p\n",
                            (Long)stab[j].st_size, nm, ad);
	    /* Pointless to do addProddableBlock() for this area,
               since the linker should never poke around in it. */
	 }
         else
         if ( ( ELF_ST_BIND(stab[j].st_info)==STB_GLOBAL
                || ELF_ST_BIND(stab[j].st_info)==STB_LOCAL
              )
              /* and not an undefined symbol */
              && stab[j].st_shndx != SHN_UNDEF
	      /* and not in a "special section" */
              && stab[j].st_shndx < SHN_LORESERVE
              &&
	      /* and it's a not a section or string table or anything silly */
              ( ELF_ST_TYPE(stab[j].st_info)==STT_FUNC ||
                ELF_ST_TYPE(stab[j].st_info)==STT_OBJECT ||
                ELF_ST_TYPE(stab[j].st_info)==STT_NOTYPE
              )
            ) {
	    /* Section 0 is the undefined section, hence > and not >=. */
            assert(secno > 0 && secno < ehdr->e_shnum);
	    /*
            if (shdr[secno].sh_type == SHT_NOBITS) {
               fprintf(stderr, "   BSS symbol, size %d off %d name %s\n",
                               stab[j].st_size, stab[j].st_value, nm);
            }
            */
            ad = ehdrC + shdr[ secno ].sh_offset + stab[j].st_value;
            if (ELF_ST_BIND(stab[j].st_info)==STB_LOCAL) {
               isLocal = TRUE;
            } else {
#ifdef ELF_FUNCTION_DESC
               /* dlsym() and the initialisation table both give us function
		* descriptors, so to be consistent we store function descriptors
		* in the symbol table */
               if (ELF_ST_TYPE(stab[j].st_info) == STT_FUNC)
                   ad = (char *)allocateFunctionDesc((Elf_Addr)ad);
#endif
               if (0|| debug_linker) 
                   fprintf(stderr, "addOTabName(GLOB): %10p  %s %s\n",
                                      ad, oc->fileName, nm );
               isLocal = FALSE;
            }
         }

         /* And the decision is ... */

         if (ad != NULL) {
            assert(nm != NULL);
	    oc->symbols[j] = nm;
            /* Acquire! */
            if (isLocal) {
               /* Ignore entirely. */
            } else {
	      //ghciInsertStrHashTable(oc->fileName, global_symbol_table, nm, ad);
	      paranoid_addto_StringMap(global_symbol_table, nm, ad);
            }
         } else {
            /* Skip. */
            if (debug_linker>1) fprintf(stderr, "skipping `%s'\n",
                                   strtab + stab[j].st_name );
            /*
            fprintf(stderr,
                    "skipping   bind = %d,  type = %d,  shndx = %d   `%s'\n",
                    (int)ELF_ST_BIND(stab[j].st_info),
                    (int)ELF_ST_TYPE(stab[j].st_info),
                    (int)stab[j].st_shndx,
                    strtab + stab[j].st_name
                   );
            */
            oc->symbols[j] = NULL;
         }

      }
   }

   return 1;
}


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
//
// TOP-LEVEL CONTROL OF THE LINKER


/* ---------------------------------------------------------------------
 * Load an obj (populate the global symbol table, but don't resolve yet)
 *
 * Returns: 1 if ok, 0 on error.
 */
static
int loadObj( char *path )
{
   ObjectCode* oc;
   struct stat st;
   int r;
   int fd, pagesize;
   char* p;

   initLinker();

   fprintf(stderr, "==== loadObj %s ====\n", path );

   /* Check that we haven't already loaded this object.  */
   {
       ObjectCode *o;
       int is_dup = 0;
       for (o = global_object_list; o; o = o->next) {
          if (0 == strcmp(o->fileName, path))
             is_dup = 1;
       }
       if (is_dup) {
	 fprintf(stderr,
            "\n\n"
            "GHCi runtime linker: warning: looks like you're trying to load the\n"
            "same object file twice:\n"
            "   %s\n"
            , path);
	 exit(1);
       }
   }

   oc = mymalloc(sizeof(ObjectCode));

   oc->formatName = "ELF";

   r = stat(path, &st);
   if (r == -1) { return 0; }

   /* sigh, strdup() isn't a POSIX function, so do it the long way */
   oc->fileName = mymalloc( strlen(path)+1 );
   strcpy(oc->fileName, path);

   oc->fileSize          = st.st_size;
   oc->symbols           = NULL;
   oc->sections          = NULL;
   oc->lochash           = new_StringMap();
   oc->proddables        = NULL;
   oc->fixup             = NULL;
   oc->fixup_used        = 0;
   oc->fixup_size        = 0;

   /* chain it onto the list of objects */
   oc->next              = global_object_list;
   global_object_list    = oc;

   fd = open(path, O_RDONLY);
   if (fd == -1) {
      fprintf(stderr,"loadObj: can't open `%s'\n", path);
      exit(1);
   }

   /* Allocate a 1-page area just prior to the image, so we can put
      fixup code fragments there.  Used for doing R_ARM_PC24
      relocations for jump distances > 64M. */

   pagesize = getpagesize();
   //   p = memalign(pagesize, N_FIXUP_PAGES * pagesize
   //                          + oc->fileSize);
   p = mymalloc(N_FIXUP_PAGES * pagesize + oc->fileSize);
   if (0) fprintf(stderr,"XXXX p = %p\n", p);
   if (p == NULL) {
      fprintf(stderr,"loadObj: failed to allocate space for `%s'\n", path);
      exit(1);
   }

   oc->fixup = p;
   oc->fixup_size = N_FIXUP_PAGES * pagesize;
   oc->fixup_used = 0;
   oc->image = &(p[ oc->fixup_size ]);

   r = read(fd, oc->image, oc->fileSize);
   if (r != oc->fileSize) {
      fprintf(stderr,"loadObj: failed to read `%s'\n", path);
      exit(1);
   }

   fprintf(stderr, "loaded %s at %p (fixup = %p)\n", 
                   oc->fileName, oc->image, oc->fixup );

   close(fd);

   /* verify the in-memory image */
   r = ocVerifyImage_ELF ( oc );
   if (!r) { return r; }

   /* build the symbol list for this image */
   r = ocGetNames_ELF ( oc );
   if (!r) { return r; }

   /* loaded, but not resolved yet */
   oc->status = OBJECT_LOADED;

#ifdef ppc32_TARGET_ARCH
   invalidate_icache(oc->image, oc->fileSize);
#endif

   return 1;
}



/* ---------------------------------------------------------------------------
 * resolve all the currently unlinked objects in memory
 *
 * Returns: 1 if ok, 0 on error.
 */
static
int resolveObjs( void )
{
    ObjectCode *oc;
    int r;

    initLinker();

    for (oc = global_object_list; oc; oc = oc->next) {
	if (oc->status != OBJECT_RESOLVED) {
	    r = ocResolve_ELF ( oc );
	    if (!r) { return r; }
	    oc->status = OBJECT_RESOLVED;
	}
    }
    return 1;
}


/* ---------------------------------------------------------------------------
 * Top-level linker.
 */

/* Load and link a bunch of .o's, and return the address of
   'entry'.  Or NULL if something borks.
*/
void* linker_top_level_LINK ( int n_object_names, char** object_names )
{
   int   r, i;
   void* mainp;

   initLinker();
   for (i = 0; i < n_object_names; i++) {
      //fprintf(stderr, "linkloop %d %s\n", i, object_names[i] );
      r = loadObj( object_names[i] );
      if (r != 1) return NULL;
   }
   r = resolveObjs();
   if (r != 1) return NULL;
   mainp = search_StringMap ( global_symbol_table, "entry" );
   if (mainp == NULL) return NULL;
   printf("switchback: Linker: success!\n");
   return mainp;
}


#endif
