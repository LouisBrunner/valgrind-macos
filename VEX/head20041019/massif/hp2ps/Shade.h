/* This file is part of hp2ps, a graph drawer for memory profiles.
   Copyright (C) 2002 The University Court of the University of Glasgow.
   This program is governed by the license contained in the file LICENSE.  */

#ifndef SHADE_H
#define SHADE_H

floatish ShadeOf  PROTO((char *));
void     ShadeFor PROTO((char *, floatish));
void     SetPSColour PROTO((floatish));

#endif /* SHADE_H */
