
/* This demonstrates a stack overrun bug that exp-ptrcheck found while
   running Valgrind itself (self hosting).  As at 12 Sept 08 this bug
   is still in Valgrind. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

typedef  unsigned long long int  ULong;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  char             Char;

/* ---------------------------------------------------------------------
   percentify()
   ------------------------------------------------------------------ */

/* This part excerpted from coregrind/m_libcbase.c */

// Percentify n/m with d decimal places.  Includes the '%' symbol at the end.
// Right justifies in 'buf'.
void VG_percentify(ULong n, ULong m, UInt d, Int n_buf, char buf[]) 
{
   Int i, len, space;
   ULong p1;
   Char fmt[32];

   if (m == 0) {
      // Have to generate the format string in order to be flexible about
      // the width of the field.
      sprintf(fmt, "%%-%ds", n_buf);
      // fmt is now "%<n_buf>s" where <d> is 1,2,3...
      sprintf(buf, fmt, "--%");
      return;
   }
   
   p1 = (100*n) / m;
    
   if (d == 0) {
      sprintf(buf, "%lld%%", p1);
   } else {
      ULong p2;
      UInt  ex;
      switch (d) {
      case 1: ex = 10;    break;
      case 2: ex = 100;   break;
      case 3: ex = 1000;  break;
      default: assert(0);
      /* was: VG_(tool_panic)("Currently can only handle 3 decimal places"); */
      }
      p2 = ((100*n*ex) / m) % ex;
      // Have to generate the format string in order to be flexible about
      // the width of the post-decimal-point part.
      sprintf(fmt, "%%lld.%%0%dlld%%%%", d);
      // fmt is now "%lld.%0<d>lld%%" where <d> is 1,2,3...
      sprintf(buf, fmt, p1, p2);
   }

   len = strlen(buf);
   space = n_buf - len;
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}


/*------------------------------------------------------------*/
/*--- Stats                                                ---*/
/*------------------------------------------------------------*/

/* This part excerpted from coregrind/m_translate.c */

static UInt n_SP_updates_fast            = 0;
static UInt n_SP_updates_generic_known   = 0;
static UInt n_SP_updates_generic_unknown = 0;

void VG_print_translation_stats ( void )
{
   Char buf[6];
   UInt n_SP_updates = n_SP_updates_fast + n_SP_updates_generic_known
                                         + n_SP_updates_generic_unknown;
   VG_percentify(n_SP_updates_fast, n_SP_updates, 1, 6, buf);
   printf(
      "translate:            fast SP updates identified: %'u (%s)\n",
      n_SP_updates_fast, buf );

   VG_percentify(n_SP_updates_generic_known, n_SP_updates, 1, 6, buf);
   printf(
      "translate:   generic_known SP updates identified: %'u (%s)\n",
      n_SP_updates_generic_known, buf );

   VG_percentify(n_SP_updates_generic_unknown, n_SP_updates, 1, 6, buf);
   printf(
      "translate: generic_unknown SP updates identified: %'u (%s)\n",
      n_SP_updates_generic_unknown, buf );
}



int main ( void )
{
  VG_print_translation_stats();
  return 0;
}
