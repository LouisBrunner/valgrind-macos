/* This file is part of hp2ps, a graph drawer for memory profiles.
   Copyright (C) 2002 The University Court of the University of Glasgow.
   This program is governed by the license contained in the file LICENSE.  */

#ifndef ERROR_H
#define ERROR_H

extern void Error    PROTO((const char *, ...));
extern void Disaster PROTO((const char *, ...));
extern void Usage    PROTO((const char *));

#endif /* ERROR_H */
