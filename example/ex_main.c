
/*--------------------------------------------------------------------*/
/*--- An example skin.                                   ex_main.c ---*/
/*--------------------------------------------------------------------*/

#include "vg_skin.h"

void SK_(pre_clo_init)(VgNeeds* needs, VgTrackEvents* track) 
{
   needs->name           = "example";
   needs->description    = "an example Valgrind skin";
   needs->report_bugs_to = "santa.claus@northpole.org";
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
