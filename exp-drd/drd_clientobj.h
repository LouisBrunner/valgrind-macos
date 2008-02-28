/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2008 Bart Van Assche
  bart.vanassche@gmail.com

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


#ifndef __DRD_CLIENTOBJ_H
#define __DRD_CLIENTOBJ_H


#include "drd_clientreq.h"   /* MutexT */
#include "drd_thread.h"      /* DrdThreadId */
#include "pub_tool_basics.h"


// Forward declarations.

union drd_clientobj;


// Type definitions.

typedef enum { ClientMutex, } ObjType;

struct any
{
  Addr    a1;
  Addr    a2;
  ObjType type;
  void    (*cleanup)(union drd_clientobj*);
};

struct mutex_info
{
  Addr        a1;
  Addr        a2;
  ObjType     type;
  void        (*cleanup)(union drd_clientobj*);
  MutexT      mutex_type;      // pthread_mutex_t or pthread_spinlock_t.
  int         recursion_count; // 0 if free, >= 1 if locked.
  DrdThreadId owner;           // owner if locked, last owner if free.
  VectorClock vc;              // vector clock associated with last unlock.
};

typedef union drd_clientobj
{
  struct any        any;
  struct mutex_info mutex;
} DrdClientobj;


// Function declarations.

void drd_clientobj_init(void);
void drd_clientobj_cleanup(void);
DrdClientobj* drd_clientobj_get(const Addr addr, const ObjType t);
Bool drd_clientobj_present(const Addr a1, const Addr a2);
DrdClientobj* drd_clientobj_add(const Addr a1, const Addr a2, const ObjType t);
Bool drd_clientobj_remove(const Addr addr);
void drd_clientobj_stop_using_mem(const Addr a1, const Addr a2);
void drd_clientobj_resetiter(void);
DrdClientobj* drd_clientobj_next(const ObjType t);

#endif /* __DRD_CLIENTOBJ_H */
