
/*--------------------------------------------------------------------*/
/*--- A program that merges multiple cachegrind output files.      ---*/
/*---                                                   cg_merge.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Cachegrind, a Valgrind tool for cache
  profiling programs.

  Copyright (C) 2002-2010 Nicholas Nethercote
     njn@valgrind.org

  AVL tree code derived from
  ANSI C Library for maintainance of AVL Balanced Trees
  (C) 2000 Daniel Nagy, Budapest University of Technology and Economics
  Released under GNU General Public License (GPL) version 2

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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

typedef  signed long   Word;
typedef  unsigned long UWord;
typedef  unsigned char Bool;
#define True ((Bool)1)
#define False ((Bool)0)
typedef  signed int    Int;
typedef  unsigned int  UInt;
typedef  unsigned long long int ULong;
typedef  signed char   Char;
typedef  size_t        SizeT;


//------------------------------------------------------------------//
//---                           WordFM                           ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//

typedef  struct _WordFM  WordFM; /* opaque */

/* Initialise a WordFM */
void initFM ( WordFM* t, 
              void*   (*alloc_nofail)( SizeT ),
              void    (*dealloc)(void*),
              Word    (*kCmp)(Word,Word) );

/* Allocate and initialise a WordFM */
WordFM* newFM( void* (*alloc_nofail)( SizeT ),
               void  (*dealloc)(void*),
               Word  (*kCmp)(Word,Word) );

/* Free up the FM.  If kFin is non-NULL, it is applied to keys
   before the FM is deleted; ditto with vFin for vals. */
void deleteFM ( WordFM*, void(*kFin)(Word), void(*vFin)(Word) );

/* Add (k,v) to fm.  If a binding for k already exists, it is updated
   to map to this new v.  In that case we should really return the
   previous v so that caller can finalise it.  Oh well. */
void addToFM ( WordFM* fm, Word k, Word v );

// Delete key from fm, returning associated val if found
Bool delFromFM ( WordFM* fm, /*OUT*/Word* oldV, Word key );

// Look up in fm, assigning found val at spec'd address
Bool lookupFM ( WordFM* fm, /*OUT*/Word* valP, Word key );

Word sizeFM ( WordFM* fm );

// set up FM for iteration
void initIterFM ( WordFM* fm );

// get next key/val pair.  Will assert if fm has been modified
// or looked up in since initIterFM was called.
Bool nextIterFM ( WordFM* fm, /*OUT*/Word* pKey, /*OUT*/Word* pVal );

// clear the I'm iterating flag
void doneIterFM ( WordFM* fm );

// Deep copy a FM.  If dopyK is NULL, keys are copied verbatim.
// If non-null, dopyK is applied to each key to generate the
// version in the new copy.  In that case, if the argument to dopyK
// is non-NULL but the result is NULL, it is assumed that dopyK
// could not allocate memory, in which case the copy is abandoned
// and NULL is returned.  Ditto with dopyV for values.
WordFM* dopyFM ( WordFM* fm, Word(*dopyK)(Word), Word(*dopyV)(Word) );

//------------------------------------------------------------------//
//---                         end WordFM                         ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//


static char* argv0 = "cg_merge";

/* Keep track of source filename/line no so as to be able to
   print decent error messages. */
typedef
   struct {
      FILE* fp;
      UInt  lno;
      char* filename;
   }
   SOURCE;

static void printSrcLoc ( SOURCE* s )
{
   fprintf(stderr, "%s: near %s line %u\n", argv0, s->filename, s->lno-1);
}

__attribute__((noreturn))
static void mallocFail ( SOURCE* s, char* who )
{
   fprintf(stderr, "%s: out of memory in %s\n", argv0, who );
   printSrcLoc( s );
   exit(2);
}

__attribute__((noreturn))
static void parseError ( SOURCE* s, char* msg )
{
   fprintf(stderr, "%s: parse error: %s\n", argv0, msg );
   printSrcLoc( s );
   exit(1);
}

__attribute__((noreturn))
static void barf ( SOURCE* s, char* msg )
{
   fprintf(stderr, "%s: %s\n", argv0, msg );
   printSrcLoc( s );
   exit(1);
}

// Read a line
#define M_LINEBUF 40960
static char line[M_LINEBUF];

// True if anything read, False if at EOF
static Bool readline ( SOURCE* s )
{
   int ch, i = 0;
   line[0] = 0;
   while (1) {
      if (i >= M_LINEBUF-10)
         parseError(s, "Unexpected long line in input file");
      ch = getc(s->fp);
      if (ch != EOF) {
          line[i++] = ch;
          line[i] = 0;
          if (ch == '\n') {
             line[i-1] = 0;
             s->lno++;
             break;
          }
      } else {
         if (ferror(s->fp)) {
            perror(argv0);
            barf(s, "I/O error while reading input file");
         } else {
            // hit EOF
            break;
         }
      }
   }
   return line[0] != 0;
}

static Bool streqn ( char* s1, char* s2, size_t n )
{
   return 0 == strncmp(s1, s2, n);
}

static Bool streq ( char* s1, char* s2 )
{
   return 0 == strcmp(s1, s2 );
}


////////////////////////////////////////////////////////////////

typedef
   struct {
      char* fi_name;
      char* fn_name;
   }
   FileFn;

typedef
   struct {
      Int n_counts;
      ULong* counts;
   }
   Counts;

typedef
   struct {
      // null-terminated vector of desc_lines
      char** desc_lines;

      // Cmd line
      char* cmd_line;

      // Events line
      char* events_line;
      Int   n_events;

      // Summary line (copied from input)
      char* summary_line;

      /* Outermost map is
            WordFM FileFn* innerMap
         where innerMap is   WordFM line-number=UWord Counts */
      WordFM* outerMap;

      // Summary counts (computed whilst parsing)
      // should match .summary_line
      Counts* summary;
   }
   CacheProfFile;

static FileFn* new_FileFn ( char* file_name, char* fn_name )
{
   FileFn* ffn = malloc(sizeof(FileFn));
   if (ffn == NULL)
      return NULL;
   ffn->fi_name = file_name;
   ffn->fn_name = fn_name;
   return ffn;
}

static void ddel_FileFn ( FileFn* ffn )
{
   if (ffn->fi_name)
      free(ffn->fi_name);
   if (ffn->fn_name)
      free(ffn->fn_name);
   memset(ffn, 0, sizeof(FileFn));
   free(ffn);
}

static FileFn* dopy_FileFn ( FileFn* ff )
{
   char* fi2 = strdup(ff->fi_name);
   char* fn2 = strdup(ff->fn_name);
   if ((!fi2) || (!fn2))
      return NULL;
   return new_FileFn( fi2, fn2 );
}

static Counts* new_Counts ( Int n_counts, /*COPIED*/ULong* counts )
{
   Int i;
   Counts* cts = malloc(sizeof(Counts));
   if (cts == NULL)
      return NULL;

   assert(n_counts >= 0);
   cts->counts = malloc(n_counts * sizeof(ULong));
   if (cts->counts == NULL)
      return NULL;

   cts->n_counts = n_counts;
   for (i = 0; i < n_counts; i++)
      cts->counts[i] = counts[i];

   return cts;
}

static Counts* new_Counts_Zeroed ( Int n_counts )
{
   Int i;
   Counts* cts = malloc(sizeof(Counts));
   if (cts == NULL)
      return NULL;

   assert(n_counts >= 0);
   cts->counts = malloc(n_counts * sizeof(ULong));
   if (cts->counts == NULL)
      return NULL;

   cts->n_counts = n_counts;
   for (i = 0; i < n_counts; i++)
      cts->counts[i] = 0;

   return cts;
}

static void sdel_Counts ( Counts* cts )
{
   memset(cts, 0, sizeof(Counts));
   free(cts);
}

static void ddel_Counts ( Counts* cts )
{
   if (cts->counts)
      free(cts->counts);
   memset(cts, 0, sizeof(Counts));
   free(cts);
}

static Counts* dopy_Counts ( Counts* cts )
{
   return new_Counts( cts->n_counts, cts->counts );
}

static
CacheProfFile* new_CacheProfFile ( char**  desc_lines,
                                   char*   cmd_line,
                                   char*   events_line,
                                   Int     n_events,
                                   char*   summary_line,
                                   WordFM* outerMap,
                                   Counts* summary )
{
   CacheProfFile* cpf = malloc(sizeof(CacheProfFile));
   if (cpf == NULL)
      return NULL;
   cpf->desc_lines   = desc_lines;
   cpf->cmd_line     = cmd_line;
   cpf->events_line  = events_line;
   cpf->n_events     = n_events;
   cpf->summary_line = summary_line;
   cpf->outerMap     = outerMap;
   cpf->summary      = summary;
   return cpf;
}

static WordFM* dopy_InnerMap ( WordFM* innerMap )
{
   return dopyFM ( innerMap, NULL,
                             (Word(*)(Word))dopy_Counts );
}

static void ddel_InnerMap ( WordFM* innerMap )
{
   deleteFM( innerMap, NULL, (void(*)(Word))ddel_Counts );
}

static void ddel_CacheProfFile ( CacheProfFile* cpf )
{
   char** p;
   if (cpf->desc_lines) {
      for (p = cpf->desc_lines; *p; p++)
         free(*p);
      free(cpf->desc_lines);
   }
   if (cpf->cmd_line)
      free(cpf->cmd_line);
   if (cpf->events_line)
      free(cpf->events_line);
   if (cpf->summary_line)
      free(cpf->summary_line);
   if (cpf->outerMap)
      deleteFM( cpf->outerMap, (void(*)(Word))ddel_FileFn, 
                               (void(*)(Word))ddel_InnerMap );
   if (cpf->summary)
      ddel_Counts(cpf->summary);

   memset(cpf, 0, sizeof(CacheProfFile));
   free(cpf);
}

static void showCounts ( FILE* f, Counts* c )
{
   Int i;
   for (i = 0; i < c->n_counts; i++) {
      fprintf(f, "%lld ", c->counts[i]);
   }
}

static void show_CacheProfFile ( FILE* f, CacheProfFile* cpf )
{
   Int     i;
   char**  d;
   FileFn* topKey;
   WordFM* topVal;
   UWord   subKey;
   Counts* subVal;  

   for (d = cpf->desc_lines; *d; d++)
      fprintf(f, "%s\n", *d);
   fprintf(f, "%s\n", cpf->cmd_line);
   fprintf(f, "%s\n", cpf->events_line);

   initIterFM( cpf->outerMap );
   while (nextIterFM( cpf->outerMap, (Word*)(&topKey), (Word*)(&topVal) )) {
      fprintf(f, "fl=%s\nfn=%s\n", 
                 topKey->fi_name, topKey->fn_name );
      initIterFM( topVal );
      while (nextIterFM( topVal, (Word*)(&subKey), (Word*)(&subVal) )) {
         fprintf(f, "%ld   ", subKey );
         showCounts( f, subVal );
         fprintf(f, "\n");
      }
      doneIterFM( topVal );
   }
   doneIterFM( cpf->outerMap );

   //fprintf(f, "%s\n", cpf->summary_line);
   fprintf(f, "summary:");
   for (i = 0; i < cpf->summary->n_counts; i++)
      fprintf(f, " %lld", cpf->summary->counts[i]);
   fprintf(f, "\n");
}

////////////////////////////////////////////////////////////////

static Word cmp_FileFn ( Word s1, Word s2 )
{
   FileFn* ff1 = (FileFn*)s1;
   FileFn* ff2 = (FileFn*)s2;
   Word r = strcmp(ff1->fi_name, ff2->fi_name);
   if (r == 0)
      r = strcmp(ff1->fn_name, ff2->fn_name);
   return r;
}

static Word cmp_unboxed_UWord ( Word s1, Word s2 )
{
   UWord u1 = (UWord)s1;
   UWord u2 = (UWord)s2;
   if (u1 < u2) return -1;
   if (u1 > u2) return 1;
   return 0;
}

////////////////////////////////////////////////////////////////

static Bool parse_ULong ( /*OUT*/ULong* res, /*INOUT*/char** pptr)
{
   ULong u64;
   char* ptr = *pptr;
   while (isspace(*ptr)) ptr++;
   if (!isdigit(*ptr)) {
      return False; /* end of string, or junk */
      *pptr = ptr;
   }
   u64 = 0;
   while (isdigit(*ptr)) {
      u64 = (u64 * 10) + (ULong)(*ptr - '0');
      ptr++;
   }
   *res = u64;
   *pptr = ptr;
   return True;
}

// str is a line of digits, starting with a line number.  Parse it,
// returning the first number in *lnno and the rest in a newly
// allocated Counts struct.  If lnno is non-NULL, treat the first
// number as a line number and assign it to *lnno instead of
// incorporating it in the counts array.
static 
Counts* splitUpCountsLine ( SOURCE* s, /*OUT*/UWord* lnno, char* str )
{
#define N_TMPC 50
   Bool    ok;
   Counts* counts;
   ULong   tmpC[N_TMPC];
   Int     n_tmpC = 0;
   while (1) {
      ok = parse_ULong( &tmpC[n_tmpC], &str );
      if (!ok)
         break;
      n_tmpC++;
      if (n_tmpC >= N_TMPC)
         barf(s, "N_TMPC too low.  Increase and recompile.");
   }
   if (*str != 0)
      parseError(s, "garbage in counts line");
   if (lnno ? (n_tmpC < 2) : (n_tmpC < 1))
      parseError(s, "too few counts in count line");

   if (lnno) {
      *lnno = (UWord)tmpC[0];
      counts = new_Counts( n_tmpC-1, /*COPIED*/&tmpC[1] );
   } else {
      counts = new_Counts( n_tmpC, /*COPIED*/&tmpC[0] );
   }

   return counts;
#undef N_TMPC
}

static void addCounts ( SOURCE* s, /*OUT*/Counts* counts1, Counts* counts2 )
{
   Int i;
   if (counts1->n_counts != counts2->n_counts)
      parseError(s, "addCounts: inconsistent number of counts");
   for (i = 0; i < counts1->n_counts; i++)
      counts1->counts[i] += counts2->counts[i];
}

static Bool addCountsToMap ( SOURCE* s,
                             WordFM* counts_map, 
                             UWord lnno, Counts* newCounts )
{
   Counts* oldCounts;
   // look up lnno in the map.  If none present, add a binding
   // lnno->counts.  If present, add counts to the existing entry.
   if (lookupFM( counts_map, (Word*)(&oldCounts), (Word)lnno )) {
      // merge with existing binding
      addCounts( s, oldCounts, newCounts );
      return True;
   } else {
      // create new binding
      addToFM( counts_map, (Word)lnno, (Word)newCounts );
      return False;
   }
}

static
void handle_counts ( SOURCE* s,
                     CacheProfFile* cpf, 
                     char* fi, char* fn, char* newCountsStr )
{
   WordFM* countsMap;
   Bool    freeNewCounts;
   UWord   lnno;
   Counts* newCounts;
   FileFn* topKey; 

   if (0)  printf("%s %s %s\n", fi, fn, newCountsStr );

   // parse the numbers
   newCounts = splitUpCountsLine( s, &lnno, newCountsStr );

   // Did we get the right number?
   if (newCounts->n_counts != cpf->n_events)
      goto oom;

   // allocate the key
   topKey = malloc(sizeof(FileFn));
   if (topKey) {
      topKey->fi_name = strdup(fi);
      topKey->fn_name = strdup(fn);
   }
   if (! (topKey && topKey->fi_name && topKey->fn_name))
      mallocFail(s, "handle_counts:");

   // search for it
   if (lookupFM( cpf->outerMap, (Word*)(&countsMap), (Word)topKey )) {
      // found it.  Merge in new counts
      freeNewCounts = addCountsToMap( s, countsMap, lnno, newCounts );
      ddel_FileFn(topKey);
   } else {
      // not found in the top map.  Create new entry
      countsMap = newFM( malloc, free, cmp_unboxed_UWord );
      if (!countsMap)
         goto oom;
      addToFM( cpf->outerMap, (Word)topKey, (Word)countsMap );
      freeNewCounts = addCountsToMap( s, countsMap, lnno, newCounts );
   }

   // also add to running summary total
   addCounts( s, cpf->summary, newCounts );

   // if safe to do so, free up the count vector
   if (freeNewCounts)
      ddel_Counts(newCounts);

   return;

  oom:
   parseError(s, "# counts doesn't match # events");
}


/* Parse a complete file from the stream in 's'.  If a parse error
   happens, do not return; instead exit via parseError().  If an
   out-of-memory condition happens, do not return; instead exit via
   mallocError().
*/
static CacheProfFile* parse_CacheProfFile ( SOURCE* s )
{
#define M_TMP_DESCLINES 10

   Int            i;
   Bool           b;
   char*          tmp_desclines[M_TMP_DESCLINES];
   char*          p;
   int            n_tmp_desclines = 0;
   CacheProfFile* cpf;
   Counts*        summaryRead; 
   char*          curr_fn_init = "???";
   char*          curr_fl_init = "???";
   char*          curr_fn      = curr_fn_init;
   char*          curr_fl      = curr_fl_init;

   cpf = new_CacheProfFile( NULL, NULL, NULL, 0, NULL, NULL, NULL );
   if (cpf == NULL)
      mallocFail(s, "parse_CacheProfFile(1)");

   // Parse "desc:" lines
   while (1) {
      b = readline(s);
      if (!b) 
         break;
      if (!streqn(line, "desc: ", 6))
         break;
      if (n_tmp_desclines >= M_TMP_DESCLINES)
         barf(s, "M_TMP_DESCLINES too low; increase and recompile");
      tmp_desclines[n_tmp_desclines++] = strdup(line);
   }

   if (n_tmp_desclines == 0)
      parseError(s, "parse_CacheProfFile: no DESC lines present");

   cpf->desc_lines = malloc( (1+n_tmp_desclines) * sizeof(char*) );
   if (cpf->desc_lines == NULL)
      mallocFail(s, "parse_CacheProfFile(2)");

   cpf->desc_lines[n_tmp_desclines] = NULL;
   for (i = 0; i < n_tmp_desclines; i++)
      cpf->desc_lines[i] = tmp_desclines[i];

   // Parse "cmd:" line
   if (!streqn(line, "cmd: ", 5))
      parseError(s, "parse_CacheProfFile: no CMD line present");

   cpf->cmd_line = strdup(line);
   if (cpf->cmd_line == NULL)
      mallocFail(s, "parse_CacheProfFile(3)");

   // Parse "events:" line and figure out how many events there are
   b = readline(s);
   if (!b)
      parseError(s, "parse_CacheProfFile: eof before EVENTS line");
   if (!streqn(line, "events: ", 8))
      parseError(s, "parse_CacheProfFile: no EVENTS line present");

   // figure out how many events there are by counting the number
   // of space-alphanum transitions in the events_line
   cpf->events_line = strdup(line);
   if (cpf->events_line == NULL)
      mallocFail(s, "parse_CacheProfFile(3)");

   cpf->n_events = 0;
   assert(cpf->events_line[6] == ':');
   for (p = &cpf->events_line[6]; *p; p++) {
      if (p[0] == ' ' && isalpha(p[1]))
         cpf->n_events++;
   }

   // create the running cross-check summary
   cpf->summary = new_Counts_Zeroed( cpf->n_events );
   if (cpf->summary == NULL)
      mallocFail(s, "parse_CacheProfFile(4)");

   // create the outer map (file+fn name --> inner map)
   cpf->outerMap = newFM ( malloc, free, cmp_FileFn );
   if (cpf->outerMap == NULL)
      mallocFail(s, "parse_CacheProfFile(5)");

   // process count lines
   while (1) {
      b = readline(s);
      if (!b)
         parseError(s, "parse_CacheProfFile: eof before SUMMARY line");

      if (isdigit(line[0])) {
         handle_counts(s, cpf, curr_fl, curr_fn, line);
         continue;
      }
      else
      if (streqn(line, "fn=", 3)) {
         if (curr_fn != curr_fn_init)
            free(curr_fn);
         curr_fn = strdup(line+3);
         continue;
      }
      else
      if (streqn(line, "fl=", 3)) {
         if (curr_fl != curr_fl_init)
            free(curr_fl);
         curr_fl = strdup(line+3);
         continue;
      }
      else
      if (streqn(line, "summary: ", 9)) {
         break;
      }
      else
         parseError(s, "parse_CacheProfFile: unexpected line in main data");
   }

   // finally, the "summary:" line
   if (!streqn(line, "summary: ", 9))
      parseError(s, "parse_CacheProfFile: missing SUMMARY line");

   cpf->summary_line = strdup(line);
   if (cpf->summary_line == NULL)
      mallocFail(s, "parse_CacheProfFile(6)");

   // there should be nothing more
   b = readline(s);
   if (b)
      parseError(s, "parse_CacheProfFile: "
                    "extraneous content after SUMMARY line");

   // check the summary counts are as expected
   summaryRead = splitUpCountsLine( s, NULL, &cpf->summary_line[8] );
   if (summaryRead == NULL)
      mallocFail(s, "parse_CacheProfFile(7)");
   if (summaryRead->n_counts != cpf->n_events)
      parseError(s, "parse_CacheProfFile: wrong # counts in SUMMARY line");
   for (i = 0; i < summaryRead->n_counts; i++) {
      if (summaryRead->counts[i] != cpf->summary->counts[i]) {
         parseError(s, "parse_CacheProfFile: "
                       "computed vs stated SUMMARY counts mismatch");
      }
   }
   free(summaryRead->counts);
   sdel_Counts(summaryRead);

   // since the summary counts are OK, free up the summary_line text
   // which contains the same info.
   if (cpf->summary_line) {
      free(cpf->summary_line);
      cpf->summary_line = NULL;
   }

   if (curr_fn != curr_fn_init)
      free(curr_fn);
   if (curr_fl != curr_fl_init)
      free(curr_fl);

   // All looks OK
   return cpf;

#undef N_TMP_DESCLINES  
}


static void merge_CacheProfInfo ( SOURCE* s,
                                  /*MOD*/CacheProfFile* dst,
                                  CacheProfFile* src )
{
   /* For each (filefn, innerMap) in src
      if filefn not in dst
         add binding dopy(filefn)->dopy(innerMap) in src
      else
         // merge src->innerMap with dst->innerMap
         for each (lineno, counts) in src->innerMap
         if lineno not in dst->innerMap
            add binding lineno->dopy(counts) to dst->innerMap
         else
            add counts into dst->innerMap[lineno]
   */
   /* Outer iterator:  FileFn* -> WordFM* (inner iterator)
      Inner iterator:  UWord   -> Counts*
   */
   FileFn* soKey;
   WordFM* soVal;
   WordFM* doVal;
   UWord   siKey;
   Counts* siVal;
   Counts* diVal;

   /* First check mundane things: that the events: lines are
      identical. */
   if (!streq( dst->events_line, src->events_line ))
     barf(s, "\"events:\" line of most recent file does "
             "not match those previously processed");

   initIterFM( src->outerMap );

   // for (filefn, innerMap) in src
   while (nextIterFM( src->outerMap, (Word*)&soKey, (Word*)&soVal )) {

      // is filefn in dst?   
      if (! lookupFM( dst->outerMap, (Word*)&doVal, (Word)soKey )) {

         // no .. add dopy(filefn) -> dopy(innerMap) to src
         FileFn* c_soKey = dopy_FileFn(soKey);
         WordFM* c_soVal = dopy_InnerMap(soVal);
         if ((!c_soKey) || (!c_soVal)) goto oom;
         addToFM( dst->outerMap, (Word)c_soKey, (Word)c_soVal );

      } else {

         // yes .. merge the two innermaps
         initIterFM( soVal );

         // for (lno, counts) in soVal (source inner map)
         while (nextIterFM( soVal, (Word*)&siKey, (Word*)&siVal )) {

            // is lno in the corresponding dst inner map?
            if (! lookupFM( doVal, (Word*)&diVal, siKey )) {

               // no .. add lineno->dopy(counts) to dst inner map
               Counts* c_siVal = dopy_Counts( siVal );
               if (!c_siVal) goto oom;
               addToFM( doVal, siKey, (Word)c_siVal );

            } else {

               // yes .. merge counts into dst inner map val
               addCounts( s, diVal, siVal );

            }
         }

      }

   }

   // add the summaries too
   addCounts(s, dst->summary, src->summary );

   return;

  oom:
   mallocFail(s, "merge_CacheProfInfo");
}

static void usage ( void )
{
   fprintf(stderr, "%s: Merges multiple cachegrind output files into one\n", 
                   argv0);
   fprintf(stderr, "%s: usage: %s [-o outfile] [files-to-merge]\n", 
                   argv0, argv0);
   exit(1);
}

int main ( int argc, char** argv )
{
   Int            i;
   SOURCE         src;
   CacheProfFile  *cpf, *cpfTmp;

   FILE*          outfile = NULL;
   char*          outfilename = NULL;
   Int            outfileix = 0;

   if (argv[0])
      argv0 = argv[0];

   if (argc < 2)
      usage();

   for (i = 1; i < argc; i++) {
      if (streq(argv[i], "-h") || streq(argv[i], "--help"))
         usage();
   }

   /* Scan args, looking for '-o outfilename'. */
   for (i = 1; i < argc; i++) {
      if (streq(argv[i], "-o")) {
         if (i+1 < argc) {
            outfilename = argv[i+1];
            outfileix   = i;
            break;
         } else {
            usage();
         }
      }
   }

   cpf = NULL;

   for (i = 1; i < argc; i++) {

      if (i == outfileix) {
         /* Skip '-o' and whatever follows it */
         i += 1;
         continue;
      }

      fprintf(stderr, "%s: parsing %s\n", argv0, argv[i]);
      src.lno      = 1;
      src.filename = argv[i];
      src.fp       = fopen(src.filename, "r");
      if (!src.fp) {
         perror(argv0);
         barf(&src, "Cannot open input file");
      }
      assert(src.fp);
      cpfTmp = parse_CacheProfFile( &src );
      fclose(src.fp);

      /* If this isn't the first file, merge */
      if (cpf == NULL) {
         /* this is the first file */
         cpf = cpfTmp;
      } else {
         /* not the first file; merge */
         fprintf(stderr, "%s: merging %s\n", argv0, argv[i]);
         merge_CacheProfInfo( &src, cpf, cpfTmp );
         ddel_CacheProfFile( cpfTmp );
      }

   }

   /* Now create the output file. */

   if (cpf) {

      fprintf(stderr, "%s: writing %s\n", 
                       argv0, outfilename ? outfilename : "(stdout)" );

      /* Write the output. */
      if (outfilename) {
         outfile = fopen(outfilename, "w");
         if (!outfile) {
            fprintf(stderr, "%s: can't create output file %s\n", 
                            argv0, outfilename);
            perror(argv0);
            exit(1);
         }
      } else {
         outfile = stdout;
      }

      show_CacheProfFile( outfile, cpf );
      if (ferror(outfile)) {
         fprintf(stderr, "%s: error writing output file %s\n", 
                         argv0, outfilename);
         perror(argv0);
         if (outfile != stdout)
            fclose(outfile);
         exit(1);
      }

      fflush(outfile);
      if (outfile != stdout)
         fclose( outfile );

      ddel_CacheProfFile( cpf );
   }

   return 0;
}


//------------------------------------------------------------------//
//---                           WordFM                           ---//
//---                       Implementation                       ---//
//------------------------------------------------------------------//

/* ------------ Implementation ------------ */

/* One element of the AVL tree */
typedef
   struct _AvlNode {
      Word key;
      Word val;
      struct _AvlNode* left;
      struct _AvlNode* right;
      Char balance;
   }
   AvlNode;

typedef 
   struct {
      Word w;
      Bool b;
   }
   MaybeWord;

#define WFM_STKMAX    32    // At most 2**32 entries can be iterated over

struct _WordFM {
   AvlNode* root;
   void*    (*alloc_nofail)( SizeT );
   void     (*dealloc)(void*);
   Word     (*kCmp)(Word,Word);
   AvlNode* nodeStack[WFM_STKMAX]; // Iterator node stack
   Int      numStack[WFM_STKMAX];  // Iterator num stack
   Int      stackTop;              // Iterator stack pointer, one past end
}; 

/* forward */
static Bool avl_removeroot_wrk(AvlNode** t, Word(*kCmp)(Word,Word));

/* Swing to the left.  Warning: no balance maintainance. */
static void avl_swl ( AvlNode** root )
{
   AvlNode* a = *root;
   AvlNode* b = a->right;
   *root    = b;
   a->right = b->left;
   b->left  = a;
}

/* Swing to the right.  Warning: no balance maintainance. */
static void avl_swr ( AvlNode** root )
{
   AvlNode* a = *root;
   AvlNode* b = a->left;
   *root    = b;
   a->left  = b->right;
   b->right = a;
}

/* Balance maintainance after especially nasty swings. */
static void avl_nasty ( AvlNode* root )
{
   switch (root->balance) {
      case -1: 
         root->left->balance  = 0;
         root->right->balance = 1;
         break;
      case 1:
         root->left->balance  = -1;
         root->right->balance = 0;
         break;
      case 0:
         root->left->balance  = 0;
         root->right->balance = 0;
         break;
      default:
         assert(0);
   }
   root->balance=0;
}

/* Find size of a non-NULL tree. */
static Word size_avl_nonNull ( AvlNode* nd )
{
   return 1 + (nd->left  ? size_avl_nonNull(nd->left)  : 0)
            + (nd->right ? size_avl_nonNull(nd->right) : 0);
}

/* Insert element a into the AVL tree t.  Returns True if the depth of
   the tree has grown.  If element with that key is already present,
   just copy a->val to existing node, first returning old ->val field
   of existing node in *oldV, so that the caller can finalize it
   however it wants.
*/
static 
Bool avl_insert_wrk ( AvlNode**         rootp, 
                      /*OUT*/MaybeWord* oldV,
                      AvlNode*          a, 
                      Word              (*kCmp)(Word,Word) )
{
   Word cmpres;

   /* initialize */
   a->left    = 0;
   a->right   = 0;
   a->balance = 0;
   oldV->b    = False;

   /* insert into an empty tree? */
   if (!(*rootp)) {
      (*rootp) = a;
      return True;
   }
 
   cmpres = kCmp( (*rootp)->key, a->key );

   if (cmpres > 0) {
      /* insert into the left subtree */
      if ((*rootp)->left) {
         AvlNode* left_subtree = (*rootp)->left;
         if (avl_insert_wrk(&left_subtree, oldV, a, kCmp)) {
            switch ((*rootp)->balance--) {
               case  1: return False;
               case  0: return True;
               case -1: break;
               default: assert(0);
            }
            if ((*rootp)->left->balance < 0) {
               avl_swr( rootp );
               (*rootp)->balance = 0;
               (*rootp)->right->balance = 0;
            } else {
               avl_swl( &((*rootp)->left) );
               avl_swr( rootp );
               avl_nasty( *rootp );
            }
         } else {
            (*rootp)->left = left_subtree;
         }
         return False;
      } else {
         (*rootp)->left = a;
         if ((*rootp)->balance--) 
            return False;
         return True;
      }
      assert(0);/*NOTREACHED*/
   }
   else 
   if (cmpres < 0) {
      /* insert into the right subtree */
      if ((*rootp)->right) {
         AvlNode* right_subtree = (*rootp)->right;
         if (avl_insert_wrk(&right_subtree, oldV, a, kCmp)) {
            switch((*rootp)->balance++){
               case -1: return False;
               case  0: return True;
               case  1: break;
               default: assert(0);
            }
            if ((*rootp)->right->balance > 0) {
               avl_swl( rootp );
               (*rootp)->balance = 0;
               (*rootp)->left->balance = 0;
            } else {
               avl_swr( &((*rootp)->right) );
               avl_swl( rootp );
               avl_nasty( *rootp );
            }
         } else {
            (*rootp)->right = right_subtree;
         }
         return False;
      } else {
         (*rootp)->right = a;
         if ((*rootp)->balance++) 
            return False;
         return True;
      }
      assert(0);/*NOTREACHED*/
   }
   else {
      /* cmpres == 0, a duplicate - replace the val, but don't
         incorporate the node in the tree */
      oldV->b = True;
      oldV->w = (*rootp)->val;
      (*rootp)->val = a->val;
      return False;
   }
}

/* Remove an element a from the AVL tree t.  a must be part of
   the tree.  Returns True if the depth of the tree has shrunk. 
*/
static
Bool avl_remove_wrk ( AvlNode** rootp, 
                      AvlNode*  a, 
                      Word(*kCmp)(Word,Word) )
{
   Bool ch;
   Word cmpres = kCmp( (*rootp)->key, a->key );

   if (cmpres > 0){
      /* remove from the left subtree */
      AvlNode* left_subtree = (*rootp)->left;
      assert(left_subtree);
      ch = avl_remove_wrk(&left_subtree, a, kCmp);
      (*rootp)->left=left_subtree;
      if (ch) {
         switch ((*rootp)->balance++) {
            case -1: return True;
            case  0: return False;
            case  1: break;
            default: assert(0);
         }
         switch ((*rootp)->right->balance) {
            case 0:
               avl_swl( rootp );
               (*rootp)->balance = -1;
               (*rootp)->left->balance = 1;
               return False;
            case 1: 
               avl_swl( rootp );
               (*rootp)->balance = 0;
               (*rootp)->left->balance = 0;
               return -1;
            case -1:
               break;
            default:
               assert(0);
         }
         avl_swr( &((*rootp)->right) );
         avl_swl( rootp );
         avl_nasty( *rootp );
         return True;
      }
   }
   else
   if (cmpres < 0) {
      /* remove from the right subtree */
      AvlNode* right_subtree = (*rootp)->right;
      assert(right_subtree);
      ch = avl_remove_wrk(&right_subtree, a, kCmp);
      (*rootp)->right = right_subtree;
      if (ch) {
         switch ((*rootp)->balance--) {
            case  1: return True;
            case  0: return False;
            case -1: break;
            default: assert(0);
         }
         switch ((*rootp)->left->balance) {
            case 0:
               avl_swr( rootp );
               (*rootp)->balance = 1;
               (*rootp)->right->balance = -1;
               return False;
            case -1:
               avl_swr( rootp );
               (*rootp)->balance = 0;
               (*rootp)->right->balance = 0;
               return True;
            case 1:
               break;
            default:
               assert(0);
         }
         avl_swl( &((*rootp)->left) );
         avl_swr( rootp );
         avl_nasty( *rootp );
         return True;
      }
   }
   else {
      assert(cmpres == 0);
      assert((*rootp)==a);
      return avl_removeroot_wrk(rootp, kCmp);
   }
   return 0;
}

/* Remove the root of the AVL tree *rootp.
 * Warning: dumps core if *rootp is empty
 */
static 
Bool avl_removeroot_wrk ( AvlNode** rootp, 
                          Word(*kCmp)(Word,Word) )
{
   Bool     ch;
   AvlNode* a;
   if (!(*rootp)->left) {
      if (!(*rootp)->right) {
         (*rootp) = 0;
         return True;
      }
      (*rootp) = (*rootp)->right;
      return True;
   }
   if (!(*rootp)->right) {
      (*rootp) = (*rootp)->left;
      return True;
   }
   if ((*rootp)->balance < 0) {
      /* remove from the left subtree */
      a = (*rootp)->left;
      while (a->right) a = a->right;
   } else {
      /* remove from the right subtree */
      a = (*rootp)->right;
      while (a->left) a = a->left;
   }
   ch = avl_remove_wrk(rootp, a, kCmp);
   a->left    = (*rootp)->left;
   a->right   = (*rootp)->right;
   a->balance = (*rootp)->balance;
   (*rootp)   = a;
   if(a->balance == 0) return ch;
   return False;
}

static 
AvlNode* avl_find_node ( AvlNode* t, Word k, Word(*kCmp)(Word,Word) )
{
   Word cmpres;
   while (True) {
      if (t == NULL) return NULL;
      cmpres = kCmp(t->key, k);
      if (cmpres > 0) t = t->left;  else
      if (cmpres < 0) t = t->right; else
      return t;
   }
}

// Clear the iterator stack.
static void stackClear(WordFM* fm)
{
   Int i;
   assert(fm);
   for (i = 0; i < WFM_STKMAX; i++) {
      fm->nodeStack[i] = NULL;
      fm->numStack[i]  = 0;
   }
   fm->stackTop = 0;
}

// Push onto the iterator stack.
static inline void stackPush(WordFM* fm, AvlNode* n, Int i)
{
   assert(fm->stackTop < WFM_STKMAX);
   assert(1 <= i && i <= 3);
   fm->nodeStack[fm->stackTop] = n;
   fm-> numStack[fm->stackTop] = i;
   fm->stackTop++;
}

// Pop from the iterator stack.
static inline Bool stackPop(WordFM* fm, AvlNode** n, Int* i)
{
   assert(fm->stackTop <= WFM_STKMAX);

   if (fm->stackTop > 0) {
      fm->stackTop--;
      *n = fm->nodeStack[fm->stackTop];
      *i = fm-> numStack[fm->stackTop];
      assert(1 <= *i && *i <= 3);
      fm->nodeStack[fm->stackTop] = NULL;
      fm-> numStack[fm->stackTop] = 0;
      return True;
   } else {
      return False;
   }
}

static 
AvlNode* avl_dopy ( AvlNode* nd, 
                    Word(*dopyK)(Word), 
                    Word(*dopyV)(Word),
                    void*(alloc_nofail)(SizeT) )
{
   AvlNode* nyu;
   if (! nd)
      return NULL;
   nyu = alloc_nofail(sizeof(AvlNode));
   assert(nyu);
   
   nyu->left = nd->left;
   nyu->right = nd->right;
   nyu->balance = nd->balance;

   /* Copy key */
   if (dopyK) {
      nyu->key = dopyK( nd->key );
      if (nd->key != 0 && nyu->key == 0)
         return NULL; /* oom in key dcopy */
   } else {
      /* copying assumedly unboxed keys */
      nyu->key = nd->key;
   }

   /* Copy val */
   if (dopyV) {
      nyu->val = dopyV( nd->val );
      if (nd->val != 0 && nyu->val == 0)
         return NULL; /* oom in val dcopy */
   } else {
      /* copying assumedly unboxed vals */
      nyu->val = nd->val;
   }

   /* Copy subtrees */
   if (nyu->left) {
      nyu->left = avl_dopy( nyu->left, dopyK, dopyV, alloc_nofail );
      if (! nyu->left)
         return NULL;
   }
   if (nyu->right) {
      nyu->right = avl_dopy( nyu->right, dopyK, dopyV, alloc_nofail );
      if (! nyu->right)
         return NULL;
   }

   return nyu;
}

/* --- Public interface functions --- */

/* Initialise a WordFM. */
void initFM ( WordFM* fm,
              void*   (*alloc_nofail)( SizeT ),
              void    (*dealloc)(void*),
              Word    (*kCmp)(Word,Word) )
{
   fm->root         = 0;
   fm->kCmp         = kCmp;
   fm->alloc_nofail = alloc_nofail;
   fm->dealloc      = dealloc;
   fm->stackTop     = 0;
}

/* Allocate and Initialise a WordFM. */
WordFM* newFM( void* (*alloc_nofail)( SizeT ),
               void  (*dealloc)(void*),
               Word  (*kCmp)(Word,Word) )
{
   WordFM* fm = alloc_nofail(sizeof(WordFM));
   assert(fm);
   initFM(fm, alloc_nofail, dealloc, kCmp);
   return fm;
}

static void avl_free ( AvlNode* nd, 
                       void(*kFin)(Word),
                       void(*vFin)(Word),
                       void(*dealloc)(void*) )
{
   if (!nd)
      return;
   if (nd->left)
      avl_free(nd->left, kFin, vFin, dealloc);
   if (nd->right)
      avl_free(nd->right, kFin, vFin, dealloc);
   if (kFin)
      kFin( nd->key );
   if (vFin)
      vFin( nd->val );
   memset(nd, 0, sizeof(AvlNode));
   dealloc(nd);
}

/* Free up the FM.  If kFin is non-NULL, it is applied to keys
   before the FM is deleted; ditto with vFin for vals. */
void deleteFM ( WordFM* fm, void(*kFin)(Word), void(*vFin)(Word) )
{
   void(*dealloc)(void*) = fm->dealloc;
   avl_free( fm->root, kFin, vFin, dealloc );
   memset(fm, 0, sizeof(WordFM) );
   dealloc(fm);
}

/* Add (k,v) to fm. */
void addToFM ( WordFM* fm, Word k, Word v )
{
   MaybeWord oldV;
   AvlNode* node;
   node = fm->alloc_nofail( sizeof(struct _AvlNode) );
   node->key = k;
   node->val = v;
   oldV.b = False;
   oldV.w = 0;
   avl_insert_wrk( &fm->root, &oldV, node, fm->kCmp );
   //if (oldV.b && fm->vFin)
   //   fm->vFin( oldV.w );
   if (oldV.b)
      free(node);
}

// Delete key from fm, returning associated val if found
Bool delFromFM ( WordFM* fm, /*OUT*/Word* oldV, Word key )
{
   AvlNode* node = avl_find_node( fm->root, key, fm->kCmp );
   if (node) {
      avl_remove_wrk( &fm->root, node, fm->kCmp );
      if (oldV)
         *oldV = node->val;
      fm->dealloc(node);
      return True;
   } else {
      return False;
   }
}

// Look up in fm, assigning found val at spec'd address
Bool lookupFM ( WordFM* fm, /*OUT*/Word* valP, Word key )
{
   AvlNode* node = avl_find_node( fm->root, key, fm->kCmp );
   if (node) {
      if (valP)
         *valP = node->val;
      return True;
   } else {
      return False;
   }
}

Word sizeFM ( WordFM* fm )
{
   // Hmm, this is a bad way to do this
   return fm->root ? size_avl_nonNull( fm->root ) : 0;
}

// set up FM for iteration
void initIterFM ( WordFM* fm )
{
   assert(fm);
   stackClear(fm);
   if (fm->root)
      stackPush(fm, fm->root, 1);
}

// get next key/val pair.  Will assert if fm has been modified
// or looked up in since initIterFM was called.
Bool nextIterFM ( WordFM* fm, /*OUT*/Word* pKey, /*OUT*/Word* pVal )
{
   Int i = 0;
   AvlNode* n = NULL;
   
   assert(fm);

   // This in-order traversal requires each node to be pushed and popped
   // three times.  These could be avoided by updating nodes in-situ on the
   // top of the stack, but the push/pop cost is so small that it's worth
   // keeping this loop in this simpler form.
   while (stackPop(fm, &n, &i)) {
      switch (i) {
      case 1: 
         stackPush(fm, n, 2);
         if (n->left)  stackPush(fm, n->left, 1);
         break;
      case 2: 
         stackPush(fm, n, 3);
         if (pKey) *pKey = n->key;
         if (pVal) *pVal = n->val;
         return True;
      case 3:
         if (n->right) stackPush(fm, n->right, 1);
         break;
      default:
         assert(0);
      }
   }

   // Stack empty, iterator is exhausted, return NULL
   return False;
}

// clear the I'm iterating flag
void doneIterFM ( WordFM* fm )
{
}

WordFM* dopyFM ( WordFM* fm, Word(*dopyK)(Word), Word(*dopyV)(Word) )
{
   WordFM* nyu; 

   /* can't clone the fm whilst iterating on it */
   assert(fm->stackTop == 0);

   nyu = fm->alloc_nofail( sizeof(WordFM) );
   assert(nyu);

   *nyu = *fm;

   fm->stackTop = 0;
   memset(fm->nodeStack, 0, sizeof(fm->nodeStack));
   memset(fm->numStack, 0,  sizeof(fm->numStack));

   if (nyu->root) {
      nyu->root = avl_dopy( nyu->root, dopyK, dopyV, fm->alloc_nofail );
      if (! nyu->root)
         return NULL;
   }

   return nyu;
}

//------------------------------------------------------------------//
//---                         end WordFM                         ---//
//---                       Implementation                       ---//
//------------------------------------------------------------------//

/*--------------------------------------------------------------------*/
/*--- end                                               cg_merge.c ---*/
/*--------------------------------------------------------------------*/
