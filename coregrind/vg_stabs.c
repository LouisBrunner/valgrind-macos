/*--------------------------------------------------------------------*/
/*--- Read stabs debug info.                            vg_stabs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward
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

#include "core.h"
#include "vg_symtab2.h"

#include <a.out.h>        /* stabs defns                    */

/*------------------------------------------------------------*/
/*--- Read STABS format debug info.                        ---*/
/*------------------------------------------------------------*/

/* Stabs entry types, from:
 *   The "stabs" debug format
 *   Menapace, Kingdon and MacKenzie
 *   Cygnus Support
 */
typedef enum { N_UNDEF = 0,	/* undefined symbol, new stringtab  */
	       N_GSYM  = 32,    /* Global symbol                    */
               N_FUN   = 36,    /* Function start or end            */
               N_STSYM = 38,    /* Data segment file-scope variable */
               N_LCSYM = 40,    /* BSS segment file-scope variable  */
               N_RSYM  = 64,    /* Register variable                */
               N_SLINE = 68,    /* Source line number               */
               N_SO    = 100,   /* Source file path and name        */
               N_LSYM  = 128,   /* Stack variable or type           */
	       N_BINCL = 130,	/* Beginning of an include file	    */
               N_SOL   = 132,   /* Include file name                */
	       N_PSYM  = 160,   /* Function parameter               */
	       N_EINCL = 162,	/* End of an include file           */
               N_LBRAC = 192,   /* Start of lexical block           */
	       N_EXCL  = 194,	/* Placeholder for an include file  */
               N_RBRAC = 224    /* End   of lexical block           */
             } stab_types;
      

/* stabs use a two-dimensional numbering scheme for types: the type
   number is either of the form name:N or name:(M,N); name may be
   empty.  N is the type number within a file context; M is the file
   number (an object may have multiple files by inclusion).
*/

typedef struct _StabType {
   Char		*str;		/* string as it appears in file */
   SymType	*type;		/* our type info */
} StabType;

typedef struct _StabFile {
   StabType	*types;
   Int		ntypes;
   UInt		fileidx;	/* for reference, idx of creation */
} StabFile;

typedef struct _StabTypeTab {
   StabFile	**files;
   Int		nfiles;

   /* List of structure tag names, used for mapping them to actual
      definitions of the structures.  There should really be one of
      these per object and a global one to cope with cross-object
      references. */
   struct structlist {
      Char		*name;
      Bool		isstruct; /* struct (or union) */
      SymType		*type;	/* reference */
      struct structlist *next;
   }		*structlist;

#define HEADER_HASHSZ	53
   struct header {
      Char		*filename;	/* header file name */
      StabFile		*types;		/* types for that header */
      UInt		instance;	/* instance */
      struct header	*next;
   }		*headerhash[HEADER_HASHSZ];
} StabTypeTab;

static const Bool stabs_debug = True;

static UInt header_hash(Char *filename, UInt instance)
{
   Char *cp;
   UInt hash = 0;

   for(cp = filename; *cp; cp++) {
      hash += *cp;
      hash = (hash << 17) | (hash >> (32-17));
   }
   hash += instance;

   return hash % HEADER_HASHSZ;
}

/* Look up a struct/union tag name in table, and return reference to
   existing type, or create a new tag entry.
   XXX make this a proper data structure
*/
static SymType *structRef(StabTypeTab *tab, SymType *def, Bool isstruct, Char *name)
{
   static const Bool debug = False || stabs_debug;
   struct structlist *sl;
   SymType *ty;
   static Int warnlen = 0;
   Int len = 0;

   for(sl = tab->structlist; sl != NULL; sl = sl->next) {
      len++;

      if (isstruct == sl->isstruct && VG_(strcmp)(name, sl->name) == 0) {
	 if (debug)
	    VG_(printf)("found %s ref for %s\n",
			isstruct ? "struct" : "union", name);
	 return sl->type;
      }
   }

   if (debug && (len > warnlen*2)) {
      warnlen = len;
      VG_(printf)("struct ref list reached %d entries\n", len);
   }

   sl = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*sl));
   if (isstruct)
      ty = VG_(st_mkstruct)(def, 0, 0);
   else
      ty = VG_(st_mkunion)(def, 0, 0);

   VG_(st_setname)(ty, name);
   sl->isstruct = isstruct;
   sl->type = ty;
   sl->name = name;
   sl->next = tab->structlist;
   tab->structlist = sl;

   if (debug)
      VG_(printf)("created %s ref for %s = %p\n",
		  isstruct ? "struct" : "union", name, ty);

   return ty;
}

/* Add a structural defintion for a struct/union reference */
static SymType *structDef(StabTypeTab *tab, SymType *def, Bool isstruct, Char *name)
{
   static const Bool debug = False || stabs_debug;
   SymType *ref = structRef(tab, NULL, isstruct, name);

   /* it seems that GNAT likes to declare names as both struct tags
      and typedefs so check we aren't about to make a structure a
      reference to itself as that will create a loop */
   if (ref == def) {
      if (debug)
         VG_(printf)("ignoring %s self ref for %s %p -> %p\n",
		     isstruct ? "struct" : "union", name, ref, def);
   }
   else {
      if (debug)
         VG_(printf)("defining %s ref for %s %p -> %p\n",
		     isstruct ? "struct" : "union", name, ref, def);

      def = VG_(st_mktypedef)(ref, name, VG_(st_basetype)(def, False));
      VG_(st_setname)(def, name);
   }
   return def;
}

static StabFile *getStabFile(StabTypeTab *tab, Int file, StabFile *set)
{
   StabFile *sf;
   file++;			/* file == -1 -> no file */

   if (file < 0)
      return NULL;

   if (file >= tab->nfiles) {
      UInt i;
      StabFile **n = VG_(arena_malloc)(VG_AR_SYMTAB, (file+1) * sizeof(*n));

      for(i = 0; i <= file; i++) {
	 if (i < tab->nfiles)
	    n[i] = tab->files[i];
	 else {
	    n[i] = NULL;
	 }
      }

      if (tab->files != NULL)
	 VG_(arena_free)(VG_AR_SYMTAB, tab->files);

      tab->files = n;
      tab->nfiles = file+1;
   }

   if (set != NULL)
      tab->files[file] = set;

   if (tab->files[file] == NULL) {
      sf = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*sf));
      tab->files[file] = sf;
      sf->types = NULL;
      sf->ntypes = 0;
      sf->fileidx = file - 1;	/* compensate for file++ above */
   }

   sf = tab->files[file];
   
   return sf;
}

/* add a new index for a file */
static void addFileAlias(StabTypeTab *tab, Char *filename, UInt instance, Int idx)
{
   static const Bool debug = False || stabs_debug;
   struct header *hp;

   for(hp = tab->headerhash[header_hash(filename, instance)]; hp != NULL; hp = hp->next) {
      if (hp->instance == instance && VG_(strcmp)(filename, hp->filename) == 0) {
	 if (debug)
	    VG_(printf)("adding alias for \"%s\"/%d fileidx %d to fileidx %d\n",
			filename, instance, idx, hp->types->fileidx);
	 getStabFile(tab, idx, hp->types);
	 return;
      }
   }

   VG_(printf)("Couldn't find previous reference to \"%s\"/%d for fileidx %d\n", 
	       filename, instance, idx);
}

static void addHeader(StabTypeTab *tab, Char *filename, UInt instance, Int idx)
{
   static const Bool debug = False || stabs_debug;
   struct header *hp, **bucket;
   
   if (debug)
      VG_(printf)("adding new header %s/%d fileidx %d\n", filename, instance, idx);

   bucket = &tab->headerhash[header_hash(filename, instance)];

   hp = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*hp));
   hp->filename = filename;
   hp->instance = instance;
   hp->types = getStabFile(tab, idx, NULL);
   hp->next = *bucket;
   *bucket = hp;
}

static void clearStabFiles(StabTypeTab *tab)
{
   VG_(arena_free)(VG_AR_SYMTAB, tab->files);

   tab->files = NULL;
   tab->nfiles = 0;
}

static StabType *getStabType(StabTypeTab *tab, Int file, Int sym)
{
   StabFile *sf;
   
   sf = getStabFile(tab, file, NULL);

   if (sf == NULL || sym < 0)
      return NULL;

   if (sym >= sf->ntypes) {
      UInt i;
      StabType *n = VG_(arena_malloc)(VG_AR_SYMTAB, (sym+1) * sizeof(*n));

      for(i = 0; i <= sym; i++) {
	 if (i < sf->ntypes)
	    n[i] = sf->types[i];
	 else {
	    n[i].str = NULL;
	    n[i].type = NULL;
	 }
      }

      if (sf->types != NULL)
	 VG_(arena_free)(VG_AR_SYMTAB, sf->types);

      sf->types = n;
      sf->ntypes = sym+1;
   }

   return &sf->types[sym];
}

static Bool isdigit(Char c, Int base, Int *vp)
{
   Bool ret = False;
   Int v = 0;

   switch(base) {
   case 10:
   case 0:
      v = c - '0';
      ret = (c >= '0' && c <= '9');
      break;

   case 8:
      v = c - '0';
      ret = (c >= '0' && c <= '7');
      break;

   case 16:
      if (c >= '0' && c <= '9') {
	 v = c - '0';
	 ret = True;
      } else if (c >= 'a' && c <= 'f') {
	 v = c - 'a';
	 ret = True;
      } else if (c >= 'A' && c <= 'F') {
	 v = c - 'F';
	 ret = True;
      }
      break;
   }

   if (vp && ret)
      *vp = v;

   return ret;
}

static inline Int getbase(Char **pp)
{
   Char *p = *pp;
   Int base = 10;

   if (p[0] == '0') {
      if (p[1] == 'x') {
	 base = 16;
	 p += 2;
      } else {
	 base = 8;
	 p++;
      }
   }
   *pp = p;

   return base;
}

static Int atoi(Char **pp, Int base)
{
   Char *p = *pp;
   Int ret = 0;
   Int v;
   Bool neg = False;

   if (*p == '-') {
      neg = True;
      p++;
   }

   if (base == 0)
      base = getbase(&p);

   while(isdigit(*p, base, &v)) {
      ret *= base;
      ret += v;
      p++;
   }

   *pp = p;
   if (neg)
      ret = -ret;
   return ret;
}

static UInt atou(Char **pp, Int base)
{
   Char *p = *pp;
   UInt ret = 0;
   Int v;

   if (base == 0)
      base = getbase(&p);

   while(isdigit(*p, base, &v)) {
      ret *= base;
      ret += v;
      p++;
   }

   *pp = p;
   return ret;
}

static Bool isoperator(Char op)
{
   switch(op) {
   case 'a'...'z':
   case 'A'...'Z':
   case '0'...'9':
   case '_':
   case ':':
   case '\'':
   case '"':
   case '$':
      return False;
      
   default:
      return True;
   }
}

/* Skip a ':'-delimited name which may have ::, 'char' or other things in
   <> brackets */
static Char *templ_name(Char *p)
{
   Int brac = 0;

   /* Special case: if the name is "operatorX", where X is not an
      otherwise valid operator name, then just skip to the terminating
      ':' and ignore the '<>' bracketing stuff.  That's because names
      like "operator<" and "operator<=" can appear here, and it can be
      terminated by ::. */
   if (VG_(strncmp)(p, "operator", 8) == 0 && isoperator(p[8])) {
      p += 8;
      while(*p != ':')
	 p++;
      return p;
   }

   for(;;) {
      if (*p == '<')
	 brac++;
      if (*p == '>')
	 brac--;
      /* skip quoted character (note, it could be anything, even a
	 literal \0)

	 XXX This is a complete botch; we can't do anything sane here,
	 like support \-quoting, because gcc doesn't seem to generate
	 it, and even if it did, we wouldn't know what "'\'" means -
	 the begining of '\'' or a char in itself ('\\')?
       */
      if (brac && p[0] == '\'' && p[2] == '\'')
	 p += 3;

      /* If we're within <>, then treat :: as part of the name (a single
	 : still terminates) */
      if (*p == ':') {
	 if (brac && p[1] == ':' && p[-1] != '<')
	    p++;
	 else
	    break;
      }
      p++;
   }

   return p;
}

/* updates pp to point to after parsed typeref */
static void parse_typeref(Char **pp, Int *filep, Int *symp)
{
   Char *p = *pp;
   Int file, sym;

   file = sym = *filep = *symp = -1;

   if (*p == '(') {
      p++;
      file = atoi(&p, 10);
      if (*p++ != ',')
	 return;
      sym = atoi(&p, 10);
      if (*p++ != ')')
	 return;
   } else if (VG_(isdigit)(*p)) {
      sym = atoi(&p, 10);
   }
   
   *pp = p;
   *filep = file;
   *symp = sym;
}

static void stab_resolve(SymType *st, void *data)
{
   static const Bool debug = False || stabs_debug;
   Char *str = (Char *)data;
   vg_assert(!VG_(st_isresolved)(st));

   if (debug)
      VG_(printf)("stab_resolve: failing to do anything useful with symtype %p=%s\n", 
		  st, str);
}

/* Top level of recursive descent parser for stab type information.
   This only extracts the information needed by vg_symtypes.c, which
   is just structure shapes, pointers and arrays.  It is still
   necessary to parse everything else, because there's no way to skip
   it to get to the interesting bits.  Also, new types can be
   introduced anywhere, so we need to scan it all to pick them up. */
static SymType *stabtype_parser(SegInfo *si, SymType *def, Char **pp)
{
   static const Bool debug = False || stabs_debug;
   Char *p = *pp;
   Char t;
   SymType *type;
   StabTypeTab *tab = si->stab_typetab;

/* make sure *p == 'c' and skip over it */
#define EXPECT(c, msg)									\
   do {											\
      if (p == NULL || *p++ != c) {							\
	 VG_(printf)("\n @@ expected '%c' at %s (remains=\"%s\")\n", c, msg, p);	\
	 return NULL;									\
      }											\
   } while(0)

/* return a pointer to just after the next ch after (and including) ptr */
#define SKIPPAST(ptr, ch, msg)								\
   ({											\
      Char *__zz_charptr = VG_(strchr)((ptr), (ch));					\
      if (__zz_charptr == NULL) {							\
	 VG_(printf)("\n @@ expected '%c' at %s (ptr=\"%s\")\n", (ch), (msg), (ptr));	\
	 return NULL;									\
      }											\
      __zz_charptr+1;									\
   })

   t = *p++;

   if (0 && debug)
      VG_(printf)("stabtype_parser: parsing '%c' remains=\"%s\"\n", t, p);

   switch(t) {
   case '(':
   case '0' ... '9': {		/* reference (and perhaps definition) */
      SymType *symtype;
      Int file, sym;
      Char *prev;

      p--;
      prev = p;

      parse_typeref(&p, &file, &sym);

      {
	 /* keep stabtype reference local, because the stabtype table
	    can be rearranged by new insertions, invalidating this
	    pointer; so copy the bits we need and don't hold onto the
	    pointer. */
	 StabType *stabtype = getStabType(tab, file, sym);

	 if (stabtype == NULL) {
	    VG_(printf)(" @@ bad type ref: %s\n", prev);
	    return NULL;
	 }

	 if (stabtype->type == NULL) {
	    stabtype->type = VG_(st_mkunresolved)(def, stab_resolve, NULL);
	    if (debug)
	       VG_(printf)("making (%d,%d) %p unresolved\n", file, sym, stabtype->type);
	 }

	 symtype = stabtype->type;
      }

      if (*p == '=') {
	 /* a type definition */
	 p++;

	 if (VG_(st_isresolved)(symtype)) {
	    /* a redefinition; clear the old type out */
	    StabType *stabtype = getStabType(tab, file, sym);

	    symtype = stabtype->type = VG_(st_mkunresolved)(NULL, stab_resolve, NULL);
	    if (debug)
	       VG_(printf)("creating new type %p for definition (%d,%d)\n",
			   symtype, file, sym);
	 } else
	    VG_(st_unresolved_setdata)(symtype, stab_resolve, p);

	 if (debug)
	    VG_(printf)("defining type %p (%d,%d) = %s\n", symtype, file, sym, p);

	 /* Skip type attributes
	    '@' could also be pointer-to-member, so we need to see if
	    the following character looks like a type reference or not.
	  */
	 while(*p == '@' && !(VG_(isdigit)(p[1]) || p[1] == '-' || p[1] == '(') )
	    p = SKIPPAST(p+1, ';', "type attrib");

	 prev = p;

	 type = stabtype_parser(si, symtype, &p);
	 if (debug)
	    VG_(printf)("parsed definition: type=%p symtype=%p\n", type, symtype);

	 if (type != symtype) {
	    StabType *stabtype = getStabType(tab, file, sym);

	    vg_assert(stabtype->type != NULL);
	    if (0) {
	       /* XXX bogus */
	       vg_assert(!VG_(st_isresolved)(stabtype->type));
	       VG_(arena_free)(VG_AR_SYMTAB, stabtype->type); /* XXX proper free method? */
	    }
	    stabtype->type = type;
	 } else if (!VG_(st_isresolved)(type)) {
	    /* If type is defined in terms of itself, and is
	       therefore not resolved, it is void */
	    if (debug)
	       VG_(printf)("type %p is defined in terms of self - making void\n", type);
	    type = VG_(st_mkvoid)(type);
	 }
      } else {
	 /* just a type reference */
	 type = symtype;
	 if ((0 || debug) && !VG_(st_isresolved)(type))
	    VG_(printf)("type %p (%d,%d) is unresolved\n", type, file, sym);
	 if ((0 || debug) && VG_(st_isresolved)(type))
	    VG_(printf)("reference (%d,%d) -> %p\n", file, sym, type);
      }
      break;
   }

   case '-': {			/* -ve types for builtins? */
      Int n;
      p--;
      n = atoi(&p, 0);
      switch(n) {
      case -1:	type = VG_(st_mkint)(def, 4, True); break;
      case -2:	type = VG_(st_mkint)(def, 1, True); break;
      case -3:	type = VG_(st_mkint)(def, 2, True); break;
      case -4:	type = VG_(st_mkint)(def, 4, True); break;
      case -5:	type = VG_(st_mkint)(def, 1, False); break;
      case -6:	type = VG_(st_mkint)(def, 1, True); break;
      case -7:	type = VG_(st_mkint)(def, 2, False); break;
      case -8:	type = VG_(st_mkint)(def, 4, False); break;
      case -9:	type = VG_(st_mkint)(def, 4, False); break;
      case -10:	type = VG_(st_mkint)(def, 4, False); break;
      case -11:	type = VG_(st_mkvoid)(def); break;
      case -12:	type = VG_(st_mkfloat)(def, 4); break;
      case -13:	type = VG_(st_mkfloat)(def, 8); break;
      case -15:	type = VG_(st_mkint)(def, 4, True); break;
      case -16:	type = VG_(st_mkbool)(def, 4); break;
      case -17:	type = VG_(st_mkfloat)(def, 4); break;
      case -18:	type = VG_(st_mkfloat)(def, 8); break;
      case -20:	type = VG_(st_mkint)(def, 1, False); break;
      case -21:	type = VG_(st_mkint)(def, 1, False); break;
      case -22:	type = VG_(st_mkint)(def, 2, False); break;
      case -23:	type = VG_(st_mkint)(def, 4, False); break;
      case -24:	type = VG_(st_mkint)(def, 4, False); break;
      case -27:	type = VG_(st_mkint)(def, 1, True); break;
      case -28:	type = VG_(st_mkint)(def, 2, True); break;
      case -29:	type = VG_(st_mkint)(def, 4, True); break;
      case -31:	type = VG_(st_mkint)(def, 8, True); break;
      case -32:	type = VG_(st_mkint)(def, 8, False); break;
      case -33:	type = VG_(st_mkint)(def, 8, False); break;
      case -34:	type = VG_(st_mkint)(def, 8, True); break;

      default:
	 VG_(printf)(" @@ unrecognized negative type %d\n", n);
	 type = NULL;
	 break;
      }
      /* Different versions of gcc seem to disagree about whether a
         negative type is followed by a semicolon or not, and the stabs
         spec (susch as it is) is not clear either so we will skip a
         semicolon if there is one. */
      if (*p == ';')
         p++;
      break;
   }

   case 't': {			/* typedef: 't' TYPE */
      SymType *td = stabtype_parser(si, NULL, &p);
      type = VG_(st_mktypedef)(def, NULL, td);
      break;
   }

   case 'R': {			/* FP type: 'R' FP-TYPE ';' BYTES ';' (extra) ';' */
      Int fptype, bytes;

      fptype = atoi(&p, 0);
      EXPECT(';', "FP-TYPE");
      bytes = atoi(&p, 0);
      EXPECT(';', "FP-TYPE bytes");
      atoi(&p, 0);
      EXPECT(';', "FP-TYPE extra");
      
      type = VG_(st_mkfloat)(def, bytes);
      break;
   }

   case 'r': {			/* range: 'r' TYPE ';' MIN ';' MAX ';' */
      Int min, max;
      SymType *rtype = stabtype_parser(si, NULL, &p);

      EXPECT(';', "range TYPE");

      /* MIN and MAX are: (INTEGER | 'A' OFFSET | 'T' OFFSET | 'a' REGNO | 't' REGNO | 'J')
	 only expect INTEGER for now (no way to represent the rest yet, and no need so far)
       */
      min = atoi(&p, 0);
      EXPECT(';', "range MIN");
      max = atoi(&p, 0);
      EXPECT(';', "range MAX");

      if (debug && 0)
	 VG_(printf)("range: rtype=%p def=%p min=%d max=%d remains = \"%s\"\n", 
		     rtype, def, min, max, p);
      
      if (rtype == def) {
	 if (debug)
	    VG_(printf)("type %p is subrange of self - making int\n", def);
	 type = VG_(st_mkint)(def, sizeof(int), False);
      } else if (min > max && max == 0) {
	 if (debug)
	    VG_(printf)("type %p has backwards range %d - %d: making float\n", 
			def, min, max);
	 type = VG_(st_mkfloat)(def, min);
      } else
	 type = VG_(st_mkrange)(def, rtype, min, max);

      vg_assert(VG_(st_isresolved)(type));
      break;
   }

   case '&':			/* reference */
   case '*': {			/* pointer */
      /* ('*' | '&') TYPE */
      type = stabtype_parser(si, NULL, &p);
      type = VG_(st_mkpointer)(def, type);
      break;
   }

   case 'k':                    /* const */
   case 'B': {                  /* volatile */
      /* ('k' | 'B') TYPE */
      type = stabtype_parser(si, NULL, &p);
      break;
   }

   case 'x': {			/* reference to undefined type */
      /* 'x' ('s' | 'u' | 'e') NAME ':' */
      Char kind = *p++;		/* get kind */
      Char *name = p;

      p = templ_name(name);
      EXPECT(':', "struct/union/enum ref");

      name = VG_(addStr)(si, name, p-1-name);

      switch (kind) {
      case 's':			/* struct */
      case 'u':			/* union */
	 type = structRef(tab, def, kind == 's', name);
	 break;

      case 'e':			/* enum */
	 type = VG_(st_mkenum)(def, 0);
	 break;

      default:
	 VG_(printf)(" @@ unexpected type ref %c\n", p[-1]);
	 return NULL;
      };

      break;
   }

   case 'S': {			/* set/bitstring */
      /* 'S' TYPE */
      SymType *typeinfo;

      typeinfo = stabtype_parser(si, NULL, &p);

      type = VG_(st_mkarray)(def, typeinfo, VG_(st_mkint)(NULL, 1, True));
      break;
   }

   case 'P':			/* packed array */
   case 'a': {			/* array */
      /* ( 'a' | 'P' ) IDX-TYPE TYPE */
      SymType *idxtype;
      SymType *artype;

      idxtype = stabtype_parser(si, NULL, &p);
      artype = stabtype_parser(si, NULL, &p);
      
      type = VG_(st_mkarray)(def, idxtype, artype);

      break;
   }

   case 'e': {			/* enum */
      /* 'e' ( NAME ':' N ',' )* ';' */

      type = VG_(st_mkenum)(def, 0);

      /* don't really care about tags; just skip them */
      while(*p != ';') {
	 p = SKIPPAST(p, ':', "enum tag NAME");
	 p = SKIPPAST(p, ',', "enum tag N");
      }
      p++;			/* skip ';' */

      break;
   }

   case 'u':			/* union */
   case 's': {			/* struct */
      /* Gad.  Here we go:

	 ( 's' | 'u' ) SIZE
		( '!' NBASE ',' ( VIRT PUB OFF ',' BASE-TYPE ){NBASE} )?

		( NAME ( ':' ( '/' [0-9] )? TYPE ',' OFFSET ( ',' SIZE )?
		       | '::' ( METHOD-TYPE ':' MANGLE-ARGS ';' 
			      PROT QUAL ( '.' | '*' VIRT | '?' ) )+
		       )
		  ';'
		)*

		( '~%' FIRST-BASE-CLASS )?
	 ';'
      */
      UInt size;
      Bool method = False;

      size = atou(&p, 0);
      type = (t == 's' ? VG_(st_mkstruct) : VG_(st_mkunion))(def, size, 0);

      if (*p == '!') {
	 /* base classes */
	 Int nbase;

	 p++;
	 nbase = atoi(&p, 0);
	 EXPECT(',', "class base class count");
	 while(nbase--) {
	    p++;		/* VIRT flag */
	    p++;		/* PUB flag */
	    atoi(&p, 0);	/* offset */
	    EXPECT(',', "class base class ref");
	    stabtype_parser(si, NULL, &p);

	    if (*p == ';')	/* who eats this? */
	       p++;
	 }
      }
	 
      while(*p != ';') {
	 Char *end;
	 Char *name;
	 UInt off, sz;
	 SymType *fieldty;

	 end = templ_name(p);

	 if (end[1] == ':') {
	    /* c++ method names end in :: */
	    method = True;

	    if (VG_(strncmp)(p, "op$", 3) == 0) {
	       /* According to stabs.info, operators are named
		  ( "op$::" OP '.' ), where OP is +=, etc.  Current
		  gcc doesn't seem to use this; operators just
		  appear as "operator==::" */
	       end = SKIPPAST(end, '.', "op$ name");
	    }
	    name = VG_(addStr)(si, p, end-p);
	    p = end+2;
	 } else {
	    name = VG_(addStr)(si, p, end-p);
	    p = end+1;
	 }

	 if (method) {
	    /* don't care about methods, but we still have to crunch
	       through this goo */
	    fieldty = NULL;
	    off = sz = 0;

	    do {
	       stabtype_parser(si, NULL, &p);	/* METHOD-TYPE */

	       EXPECT(':', "struct method MANGLE-ARGS");
	       p = SKIPPAST(p, ';', "struct method MANGLE-ARGS");
	       
	       p += 1;		/* skip PROT */
	       if (*p >= 'A' && *p <= 'Z')
		  p++;		/* skip QUAL (if present) */

	       switch(*p++) {
	       case '*':	/* VIRT: VTAB-IDX ';' OVERRIDE-CLASS ';' */
		  atoi(&p, 0);	/* skip VTAB-IDX */
		  EXPECT(';', "struct method vtab idx");
		  stabtype_parser(si, NULL, &p);	/* skip OVERRIDE-CLASS */
		  EXPECT(';', "struct method vtab override");
		  break;

	       default:
		  VG_(printf)(" @@ struct method unexpected member-type '%c' \"%s\" remains\n",
			      p[-1], p);
		  /* FALLTHROUGH */
	       case '?':
	       case '.':
		  break;
	       }
	    } while (*p != ';');
	 } else {
	    if (*p == '/') {
	       /* c++ visibility spec: '/' PROT */
	       p += 2;
	    }

	    fieldty = stabtype_parser(si, NULL, &p);

	    if (*p == ':') {
	       /* static member; don't care (it will appear later) */
	       fieldty = NULL;
	       off = sz = 0;

	       p = SKIPPAST(p, ';', "struct static member");
	       p--;		/* point at ';' */
	    } else {
	       EXPECT(',', "struct TYPE");

               /* logic dictates that the offset would always be
	          positive and that atou would work here but GNAT has
	          has other ideas - see bug 90128 for more details */
	       off = atoi(&p, 0);

	       if (*p == ',') {
		  EXPECT(',', "struct OFFSET");
		  sz = atou(&p, 0);
	       } else {
		  /* sometimes the size is missing and assumed to be a
		     pointer (in bits) */
		  sz = sizeof(void *) * 8;
	       }
	    }
	 }

	 if (fieldty != NULL)
	    VG_(st_addfield)(type, name, fieldty, off, sz);

	 EXPECT(';', "struct field end");
      }
      p++;			/* skip final ';' */

      /* one final C++ surprise */
      if (*p == '~') {
	 /* "~%" FIRST-BASE-CLASS ';' */
	 p++;
	 EXPECT('%', "struct first base");
	 stabtype_parser(si, NULL, &p);	/* skip FIRST-BASE-CLASS */
	 EXPECT(';', "struct first base semi");
      }

      break;
   }

   case 'f':			/* function */
      /* 'f' TYPE */
      type = VG_(st_mkvoid)(def); /* approximate functions as void */
      stabtype_parser(si, NULL, &p);
      break;

   case '#':			/* method */
      /* '#' ( '#' RET-TYPE | 
	       CLASS-TYPE ',' RET-TYPE ',' ( ARG-TYPE ( ',' ARG-TYPE )* )? )
	  ';'
      */
      type = VG_(st_mkvoid)(def);	/* methods are really void */

      if (*p == '#') {
	 p++;			/* skip '#' */
	 stabtype_parser(si, NULL, &p); /* RET-TYPE */
      } else {
	 stabtype_parser(si, NULL, &p); /* CLASS-TYPE */
	 EXPECT(',', "method CLASS-TYPE");

	 stabtype_parser(si, NULL, &p); /* RET-TYPE */
	 EXPECT(',', "method RET-TYPE");

	 while (*p != ';') {
	    stabtype_parser(si, NULL, &p);
	    if (*p == ',')
	       p++;
	    else if (*p != ';')
	       VG_(printf)(" @@ method ARG-TYPE list unexpected '%c'\n", *p);
	 }
      }

      EXPECT(';', "method definition");
      break;

   case '@':			/* pointer to member */
      /* '@' CLASS-TYPE ',' MEMBER-TYPE */
      type = VG_(st_mkint)(def, sizeof(int), False); /* make it an int for our use */
      
      stabtype_parser(si, NULL, &p); /* CLASS-TYPE */
      EXPECT(',', "member-pointer CLASS-TYPE");
      stabtype_parser(si, NULL, &p); /* MEMBER-TYPE */
      break;

   default:
      VG_(printf)(" @@ don't know what type '%c' is\n", t);
      type = NULL;
      break;
   }
#undef EXPECT
#undef SKIPPAST

   if (type == NULL)
      VG_(printf)(" @@ parsing %s gave NULL type (%s remains)\n", *pp, p);

   *pp = p;

   return type;
}

/* parse a symbol reference: NAME ':' DESC TYPE */
static Bool initSym(SegInfo *si, Sym *sym, stab_types kind, Char **namep, Int val)
{
   static const Bool debug = False || stabs_debug;
   Char *name = *namep;
   Char *ty;
   Int len;
   Bool isTypedef = False;
   Bool isStruct = False;
   SymType *base;

   if (debug && 0)
      VG_(printf)("initSym(si=%p, tab=%p, sym=%p, kind=%d, name=%p \"%s\", val=%d)\n",
		  si, si->stab_typetab, sym, kind, name, name, val);

   ty = templ_name(name);

   len = ty - name;

   if (debug) {
      Char buf[len+1];
      VG_(strncpy_safely)(buf, name, len+1);
      VG_(printf)("\ninitSym name=\"%s\" type=%s\n", buf, ty+1);
   }

   if (*ty != ':') {
      /* no type info */
      sym->type = VG_(st_mkvoid)(NULL);
   } else {
      ty++;			/* skip ':' */

      /* chew through an initial sequence of
	 type descriptor type describers */
      for(;;) {
	 switch(*ty) {
	 case 'a': case 'b': case 'c': case 'C':
	 case 'd': case 'D': case 'f': case 'F':
	 case 'G': case 'i': case 'I': case 'J':
	 case 'L': case 'm': case 'p': case 'P':
	 case 'Q': case 'R': case 'r': case 'S':
	 case 's': case 'v': case 'V': case 'x':
	 case 'X':
	    break;

	 case 'T':			/* struct/union/enum */
	    isStruct = True;
	    break;

	 case 't':		/* typedef handled within stabtype_parser */
	    isTypedef = True;
	    /* FALLTHROUGH */
	 case '(': case '-': case '0' ... '9': /* type reference */
	 default:
	    goto out;
	 }
	 ty++;
      }

     out:
      sym->type = stabtype_parser(si, NULL, &ty);
      base = VG_(st_basetype)(sym->type, False);
      if (isStruct && (VG_(st_isstruct)(base) || VG_(st_isunion)(base))) {
	 Char *sname = VG_(addStr)(si, name, len);
	 structDef(si->stab_typetab, base, VG_(st_isstruct)(base), sname);
      }

      if (isTypedef) {
	 Char *tname = VG_(addStr)(si, name, len);
	 vg_assert(sym->type != base);
	 if (debug)
	    VG_(printf)(" typedef %p \"%s\"\n", sym->type, tname);
	 VG_(st_setname)(sym->type, tname);
	 VG_(st_setname)(base, tname);
      }
   }
   *namep = ty;

   switch(kind) {
   case N_STSYM:
   case N_LCSYM:
      sym->kind = SyStatic;
      sym->u.addr = si->offset + (Addr)val;
      break;

   case N_PSYM:
      sym->kind = SyEBPrel;	/* +ve offset off EBP (erk, or ESP if no frame pointer) */
      sym->u.offset = val;
      break;

   case N_LSYM:
      if (val < 0)
	 sym->kind = SyEBPrel;	/* -ve off EBP when there's a frame pointer */
      else
	 sym->kind = SyESPrel;	/* +ve off ESP when there's no frame pointer */
      sym->u.offset = val;
      break;

   case N_RSYM:
      sym->kind = SyReg;
      sym->u.regno = val;
      break;

   case N_GSYM:
      sym->kind = SyGlobal;
      sym->u.addr = 0;		/* XXX should really look up global address */
      break;

   default:
      VG_(core_panic)("bad sym kind");
   }

   if (debug)
      VG_(printf)("  %s = type=%p\n", (isStruct || isTypedef) ? "skipping" : "adding", sym->type);
   
   if (isStruct || isTypedef) {
      return True;		/* skip */
   } else {
      sym->name = VG_(addStr)(si, name, len);
      return False;		/* don't skip */
   }
}

/* list of unbound symbols for next scope */
struct symlist {
   Sym	sym;
   struct symlist *next;
};

/* XXX TODO: make sure added syms are unique.  A lot of syms added to
   the global scope are not.  On the other hand, skipping type
   definitions helps a lot. */
static Scope *addSymsToScope(Scope *sc, struct symlist *list, Int nsyms, Scope *outer)
{
   static const Bool debug = False || stabs_debug;
   Int j;
   struct symlist *n;
   Int base;

   if (sc == NULL) {
      sc = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*sc));
      sc->syms = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*sc->syms) * nsyms);
      sc->nsyms = nsyms;
      base = 0;
      sc->outer = outer;
      if (outer == NULL)
	 sc->depth = 0;
      else
	 sc->depth = outer->depth+1;
   } else {
      Sym *s = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*s) * (sc->nsyms + nsyms));

      VG_(memcpy)(s, sc->syms, sc->nsyms * sizeof(*s));
      VG_(arena_free)(VG_AR_SYMTAB, sc->syms);
      sc->syms = s;
      base = sc->nsyms;
      sc->nsyms += nsyms;
   }

   /* bind any unbound syms to new scope */
   for(j = 0; j < nsyms; j++, list = n) {
      if (debug)
	 VG_(printf)("   adding (%p) %s to scope %p depth %d\n", 
		     list->sym.name, list->sym.name, sc, sc->depth);
      n = list->next;
      sc->syms[base+j] = list->sym;
      VG_(arena_free)(VG_AR_SYMTAB, list);
   }
   vg_assert(list == NULL);

   return sc;
}

/* Read stabs-format debug info.  This is all rather horrible because
   stabs is a underspecified, kludgy hack.
*/
void VG_(read_debuginfo_stabs) ( SegInfo* si,
				 UChar* stabC,   Int stab_sz, 
				 UChar* stabstr, Int stabstr_sz )
{
   static const Bool debug = False || stabs_debug;
   Int    i;
   Int    n_stab_entries;
   struct nlist* stab = (struct nlist*)stabC;
   UChar *next_stabstr = NULL;
   /* state for various things */
   struct {
      Addr	start;		/* start address */
      Addr	end;		/* end address */
      Char	*name;		/* name */
      Char	*filename;	/* source file name */
      Int	line;		/* first line */
   } func = { 0, 0, NULL, NULL, -1 };
   struct {
      Char	*name;
      Bool	same;
   } file = { NULL, True };
   struct {
      Int	prev;		/* prev line */
      Int	no;		/* current line */
      Int	ovf;		/* line wrap */
      Addr	addr;		/* start of this line */
      Bool	first;		/* first line in function */
      Bool	jump;		/* was a jump from prev line (inline?) */
   } line = { 0, 0, 0, 0, False };
   struct {
      Scope	*scope;		/* current scope */
      struct symlist *symlist;	/* unbound symbols */
      Int	nsyms;		/* number of unbound scopes */
      Addr	addr;		/* start of range */
      Int	depth;
   } scope = { NULL, NULL, 0, 0 };
   Scope *global;
   Int fileidx = 0;
   StabTypeTab *tab;

   if (si->stab_typetab == NULL) {
      si->stab_typetab = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(StabTypeTab));
      VG_(memset)(si->stab_typetab, 0, sizeof(StabTypeTab));
   }
   tab = si->stab_typetab;

   /* Ok.  It all looks plausible.  Go on and read debug data. 
         stab kinds: 100   N_SO     a source file name
                      68   N_SLINE  a source line number
                      36   N_FUN    start of a function

      In this loop, we maintain a current file name, updated as 
      N_SO/N_SOLs appear, and a current function base address, 
      updated as N_FUNs appear.  Based on that, address ranges for 
      N_SLINEs are calculated, and stuffed into the line info table.

      Finding the instruction address range covered by an N_SLINE is
      complicated;  see the N_SLINE case below.
   */
   file.name     = VG_(addStr)(si,"???", -1);

   n_stab_entries = stab_sz/(int)sizeof(struct nlist);

   /* empty initial file-wide scope */
   global = addSymsToScope(NULL, NULL, 0, NULL);
   scope.scope = global;

   for (i = 0; i < n_stab_entries; i++) {
      const struct nlist *st = &stab[i];
      Char *no_fn_name = "???";
      Char *string;

      if (debug && 1) {
	 VG_(printf) ( "%2d  type=%d   othr=%d   desc=%d   value=0x%x   strx=%d  %s\n", i,
		       st->n_type, st->n_other, st->n_desc, 
		       (int)st->n_value,
		       (int)st->n_un.n_strx, 
		       stabstr + st->n_un.n_strx );
      }

      /* handle continued string stabs */
      {
	 static const Bool contdebug = False || stabs_debug;
	 Int buflen = 0;
	 Int idx = 0;
	 Char *buf = NULL;
	 Int len;
	 Bool continuing = False;
	 UInt stringidx;

	 stringidx = st->n_un.n_strx;
	 string = stabstr + stringidx;
	 len = VG_(strlen)(string);

	 while(string && len > 0 && (continuing || string[len-1] == '\\')) {
	    /* Gak, we have a continuation. Skip forward through
	       subsequent stabs to gather all the parts of the
	       continuation.  Increment i, but keep st pointing at
	       current stab. */

	    continuing = string[len-1] == '\\';

	    /* remove trailing \ */
	    while(string[len-1] == '\\' && len > 0)
	       len--;

	    if (contdebug)
	       VG_(printf)("found extension string: \"%s\" len=%d(%c) idx=%d buflen=%d\n", 
			   string, len, string[len-1], idx, buflen);

	    /* XXX this is silly.  The si->strtab should have a way of
	       appending to the last added string... */
	    if ((idx + len) >= buflen) {
	       Char *n;
	       
	       if (buflen == 0)
		  buflen = 16;
	       while((idx + len) >= buflen)
		  buflen *= 2;
	       n = VG_(arena_malloc)(VG_AR_SYMTAB, buflen);
	       VG_(memcpy)(n, buf, idx);
	       
	       if (buf != NULL)
		  VG_(arena_free)(VG_AR_SYMTAB, buf);
	       buf = n;
	    }

	    VG_(memcpy)(&buf[idx], string, len);
	    idx += len;
	    if (contdebug) {
	       buf[idx] = '\0';
	       VG_(printf)("working buf=\"%s\"\n", buf);
	    }

	    i++;
	    if (i >= n_stab_entries)
	       break;

	    if (stab[i].n_un.n_strx) {
	       string = stabstr + stab[i].n_un.n_strx;
	       len = VG_(strlen)(string);
	    } else {
	       string = NULL;
	       len = 0;
	    }
	 }

	 if (buf != NULL) {
	    i--;			/* overstepped */
	    string = VG_(addStr)(si, buf, idx);
	    VG_(arena_free)(VG_AR_SYMTAB, buf);
	    if (contdebug)
	       VG_(printf)("made composite: \"%s\"\n", string);
	 }
      }

      switch(st->n_type) {
         case N_UNDEF:
	    /* new string table base */
	    if (next_stabstr != NULL) {
	       stabstr_sz -= next_stabstr - stabstr;
	       stabstr = next_stabstr;
	       if (stabstr_sz <= 0) {
		  VG_(printf)(" @@ bad stabstr size %d\n", stabstr_sz);
		  return;
	       }
	    }
	    next_stabstr = stabstr + st->n_value;
	    break;

         case N_BINCL: {
	    fileidx++;
	    addHeader(tab, stabstr + st->n_un.n_strx, st->n_value, fileidx);

	    if (debug)
	       VG_(printf)("BINCL: pushed %s fileidx=%d\n", 
			   stabstr + st->n_un.n_strx, fileidx);
	    break;
	 }

         case N_EINCL:
	    break;

         case N_EXCL:
	    ++fileidx;

	    addFileAlias(tab, stabstr + st->n_un.n_strx, st->n_value, fileidx);

	    if (debug) {
	       VG_(printf)("reference to excluded include file %s; fileidx=%d\n",
			   stabstr + st->n_un.n_strx, fileidx);
	    }
	    break;

	 case N_SOL:		/* sub-source (include) file */
	    if (line.ovf != 0) 
	       VG_(message)(Vg_UserMsg, 
                            "Warning: file %s is very big (> 65535 lines) "
                            "Line numbers and annotation for this file might "
                            "be wrong.  Sorry",
                            file.name);
	    /* FALLTHROUGH */

	 case N_SO: {		/* new source file */
	    UChar *nm = string;
	    UInt len = VG_(strlen)(nm);
	    Addr addr = func.start + st->n_value;

	    if (line.addr != 0) {
	       /* finish off previous line */
	       VG_(addLineInfo)(si, file.name, line.addr,
				addr, line.no + line.ovf * LINENO_OVERFLOW, i);
	    }

	    /* reset line state */
	    line.ovf = 0;	    
	    line.addr = 0;
	    line.prev = 0;
	    line.no = 0;
	    line.jump = True;

	    if (len > 0 && nm[len-1] != '/') {
	       file.name = VG_(addStr)(si, nm, -1);
	       if (debug)
		  VG_(printf)("new source: %s\n", file.name);
	       if (st->n_type == N_SO) {
		  fileidx = 0;
		  clearStabFiles(tab);
	       }
	    } else if (len == 0)
	       file.name = VG_(addStr)(si, "?1\0", -1);

	    if (func.start != 0)
	       line.jump = True;
	    break;
	 }

	 case N_SLINE: {	/* line info */
	    Addr addr = func.start + st->n_value;

	    if (line.addr != 0) {
	       /* there was a previous */
	       VG_(addLineInfo)(si, file.name, line.addr,
				addr, line.no + line.ovf * LINENO_OVERFLOW, i);
	    }

	    line.addr = addr;
	    line.prev = line.no;
	    line.no = (Int)((UShort)st->n_desc);

	    if (line.prev > line.no + OVERFLOW_DIFFERENCE && file.same) {
               VG_(message)(Vg_DebugMsg, 
                  "Line number overflow detected (%d --> %d) in %s", 
                  line.prev, line.no, file.name);
               line.ovf++;
            }
            file.same = True;

	    /* This is pretty horrible.  If this is the first line of
	       the function, then bind any unbound symbols to the arg
	       scope, since they're probably arguments. */
	    if (line.first) {
	       line.first = False;
	       
	       if (scope.nsyms != 0) {
		  addSymsToScope(scope.scope, scope.symlist, scope.nsyms, NULL);
		  scope.symlist = NULL;
		  scope.nsyms = 0;
	       }

	       /* remember first line of function */
	       if (func.start != 0) {
		  func.filename = file.name;
		  func.line = line.no;
	       }
	    } else if (func.start != 0 && (line.no < func.line || func.filename != file.name)) {
	       /* If we're suddenly in code before the function starts
		  or in a different file, then it seems like its
		  probably some inlined code.  Should do something
		  useful with this information. */
	       //VG_(printf)("possible inline?\n");
	       line.jump = True;
	    }
	    break;
	 }

	 case N_FUN: {		/* function start/end */
	    Addr addr = 0;	/* end address for prev line/scope */
	    Bool newfunc = False;

	    if (scope.nsyms != 0) {
	       /* clean up any unbound symbols */
	       addSymsToScope(scope.scope, scope.symlist, scope.nsyms, NULL);
	       scope.symlist = NULL;
	       scope.nsyms = 0;
	    }
	    
	    /* if this the end of the function or we haven't
	       previously finished the previous function... */
	    if (*string == '\0' || func.start != 0) {
	       /* end of function */
	       newfunc = False;
	       line.first = False;

	       /* end line at end of function */
	       addr = func.start + st->n_value;

	       if (debug)
		  VG_(printf)("ending func %s at %p\n", func.name, addr);

	       /* now between functions */
	       func.name = no_fn_name;
	       func.start = 0;

	       if (scope.addr != 0) {
		  /* finish any previous scope range */
		  VG_(addScopeInfo)(si, scope.addr, addr, scope.scope);
	       }

	       /* tidy up arg scope */
	       /* XXX LEAK: free scope if it or any of its inner scopes was
		  never added to a scope range  */

	       if (scope.scope->depth == 0) {
		  VG_(message)(Vg_UserMsg,
			       "It seems there's more scopes closed than opened...\n");
		  break;
	       }

	       scope.scope = scope.scope->outer;
	       scope.addr = addr;
	       scope.addr = 0;
	    }

	    if (*string != '\0') {
	       /* new function */
	       newfunc = True;
	       line.first = True;

	       /* line ends at start of next function */
	       addr = si->offset + st->n_value;

	       func.start = addr;
	       func.name = string;
	       
	       if (debug)
		  VG_(printf)("\nnew func %s at %p\n", func.name, func.start);

	    }

	    if (line.addr) {
	       VG_(addLineInfo)(si, file.name, line.addr,
				addr, line.no + line.ovf * LINENO_OVERFLOW, i);
	       line.addr = 0;
	    }

	    if (scope.addr) {
	       /* finish any previous scope range */
	       VG_(addScopeInfo)(si, scope.addr, addr, scope.scope);
	    }

	    if (newfunc) {
	       /* make little wrapper scope for args */
	       Scope *sc;
	       if (scope.addr) {
		  /* finish any previous scope range */
		  VG_(addScopeInfo)(si, scope.addr, addr, scope.scope);
	       }

	       sc = addSymsToScope(NULL, scope.symlist, scope.nsyms, scope.scope);
	       scope.scope = sc;
	       scope.nsyms = 0;
	       scope.symlist = NULL;
	       scope.addr = addr;
	    }
	    break;
	 }

	 case N_LBRAC: {
	    /* open new scope */
	    Scope *sc;
	    Addr addr = func.start + st->n_value;

	    if (scope.addr) {
	       /* end previous range */
	       VG_(addScopeInfo)(si, scope.addr, addr, scope.scope);
	    }

	    scope.addr = addr;

	    if (debug) {
	       static const Char indent[]=
		  "                                        "
		  "                                        ";
	       Int idx;

	       idx = sizeof(indent)-1 - (scope.depth * 2);
	       scope.depth++;
	       VG_(printf)("%s{\n", &indent[idx >= 0 ? idx : 0]);
	    }
	    /* add unbound syms to scope */
	    sc = addSymsToScope(NULL, scope.symlist, scope.nsyms, scope.scope);
	    scope.scope = sc;
	    scope.nsyms = 0;
	    scope.symlist = NULL;

	    break;
	 }

      case N_RBRAC: {
	 /* close scope */
	 Addr addr = func.start + st->n_value;

	 if (scope.nsyms != 0) {
	    /* If there's any unbound symbols, tidy them up */
	    addSymsToScope(scope.scope, scope.symlist, scope.nsyms, NULL);
	    scope.symlist = NULL;
	    scope.nsyms = 0;
	 }

	 vg_assert(scope.addr != 0);
	 VG_(addScopeInfo)(si, scope.addr, addr, scope.scope);
	 
	 /* XXX LEAK: free scope if it or any of its inner scopes was
	    never added to a scope range  */

	 if (scope.scope->depth == 0) {
	    /* complain */
	    VG_(message)(Vg_UserMsg, "It seems there's more scopes closed than opened...\n");
	    break;
	 }

	 scope.scope = scope.scope->outer;
	 scope.addr = addr;
	 if (debug) {
	    static const Char indent[]=
	       "                                        "
	       "                                        ";
	    Int idx;

	    scope.depth--;
	    idx = sizeof(indent)-1 - (scope.depth * 2);
	    VG_(printf)("%s}\n", &indent[idx >= 0 ? idx : 0]);
	 }

	 break;
      }

      case N_GSYM:		/* global variable */
      case N_STSYM:		/* static in data segment */
      case N_LCSYM:		/* static in bss segment */
      case N_PSYM:		/* function parameter */
      case N_LSYM:		/* stack variable */
      case N_RSYM: {		/* register variable */
	 Char *cp = string;
	 Int val = st->n_value;

	 /* a single string can have multiple definitions nested in it */
	 while(*cp != '\0') {
	    struct symlist *s = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*s));
	 
	    if (initSym(si, &s->sym, st->n_type, &cp, val)) {
	       VG_(arena_free)(VG_AR_SYMTAB, s); /* not interesting */
	    } else {
	       s->next = scope.symlist;
	       scope.symlist = s;
	       scope.nsyms++;
	    }
	    switch(*cp) {
	    case '\0':		/* all done */
	       break;

	    case '0' ... '9':	/* symbol */
	    case 'A' ... 'Z':
	    case 'a' ... 'z':
	    case '_':
	       break;

	    case ' ': case ':':	/* nameless type */
	       break;

	    default:
	       VG_(printf)(" @@ unlikely looking definition in unparsed remains \"%s\"\n", cp);
	       break;
	    }
	 }
	 break;
      }
      }
   }
   
   if (scope.nsyms != 0)
      addSymsToScope(scope.scope, scope.symlist, scope.nsyms, NULL);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
