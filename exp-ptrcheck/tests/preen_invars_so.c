
/* This file contains a global array.  It is compiled into a .so,
   which is dlopened by preen_invar.c.  That then accesses the global
   array, hence generating Inv_Global invariants in sg_main.c.

   preen_invar.c then dlcloses this object, causing it to get
   unmapped; and we then need to be sure that the Inv_Global is
   removed by preen_Invars (or, at least, that the system doesn't
   crash..). */

char im_a_global_array[10];

