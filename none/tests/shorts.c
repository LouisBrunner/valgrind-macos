
#include <stdio.h>

typedef struct { short ot; short ob; short nt; short nb; } Stuff;

void PaintThumb( Stuff* w )
{
    short oldtop = w->ot;
    short oldbot = w->ob;
    short newtop = w->nt;
    short newbot = w->nb;

        if (newtop < oldtop) { fprintf(stderr,"case1\n");
	//	    FillArea(w, newtop, XawMin(newbot, oldtop), 1);
	}
	if (newtop > oldtop) { fprintf(stderr,"case2\n");
	//	    FillArea(w, oldtop, XawMin(newtop, oldbot), 0);
	}
	if (newbot < oldbot) { fprintf(stderr,"case3\n");
	//	    FillArea(w, XawMax(newbot, oldtop), oldbot, 0);
	}
	if (newbot > oldbot) { fprintf(stderr,"case4\n");
	//	    FillArea(w, XawMax(newtop, oldbot), newbot, 1);
	}
}

int main ( void )
{
  Stuff st;
  st.ot = -332;
  st.ob = -301;
  st.nt = 0;
  st.nb = 31;
  PaintThumb( &st );
  return 0;
}
