
/*--------------------------------------------------------------------*/
/*--- An example skin.                                   ex_main.c ---*/
/*--------------------------------------------------------------------*/

#include "vg_skin.h"

void SK_(pre_clo_init)(VgDetails* details, VgNeeds* needs, VgTrackEvents* track)
{
   details->name             = "example";
   details->version          = "0.0.1";
   details->description      = "an example Valgrind skin";
   details->copyright_author =
      "Copyright (C) 2002, and put in the public domain, by Santa Claus.";
   details->bug_reports_to   = "santa.claus@northpole.org";

   /* No needs, no core events to track */
}

void SK_(post_clo_init)(void)
{
}

UCodeBlock* SK_(instrument)(UCodeBlock* cb, Addr a)
{
    return cb;
}

void SK_(fini)(void)
{
}

/*--------------------------------------------------------------------*/
/*--- end                                                ex_main.c ---*/
/*--------------------------------------------------------------------*/
