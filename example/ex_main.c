
/*--------------------------------------------------------------------*/
/*--- An example tool.                                   ex_main.c ---*/
/*--------------------------------------------------------------------*/

#include "vg_skin.h"

static void SK_(pre_clo_init)()
{
   VG_(details_name)            ("Example");
   VG_(details_version)         ("0.0.1");
   VG_(details_description)     ("an example Valgrind tool");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2003, and put in the public domain, by Santa Claus.");
   VG_(details_bug_reports_to)  ("santa.claus@northpole.org");

   /* No needs, no core events to track */
}

void SK_(post_clo_init)(void)
{
}

UCodeBlock* SK_(instrument)(UCodeBlock* cb, Addr a)
{
    return cb;
}

void SK_(fini)(exitcode)
{
}

/* Does not use shadow memory */
VG_DETERMINE_INTERFACE_VERSION(SK_(pre_clo_init), 0)

/*--------------------------------------------------------------------*/
/*--- end                                                ex_main.c ---*/
/*--------------------------------------------------------------------*/
