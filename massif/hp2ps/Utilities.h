/* This file is part of hp2ps, a graph drawer for memory profiles.
   Copyright (C) 2002 The University Court of the University of Glasgow.
   This program is governed by the license contained in the file LICENSE.  */

#ifndef UTILITIES_H
#define UTILITIES_H

char* Basename    PROTO((char *));
void  DropSuffix  PROTO((char *, char *));
FILE* OpenFile    PROTO((char *, char *));
void  CommaPrint  PROTO((FILE *, intish));
char *copystring  PROTO((char *));
char *copystring2 PROTO((char *, char *));
void *xmalloc	 PROTO((size_t));
void *xrealloc	 PROTO((void *, size_t));

#endif /* UTILITIES_H */
