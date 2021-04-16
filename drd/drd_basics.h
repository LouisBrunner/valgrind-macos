/*
  This file is part of DRD, a thread error detector.

  Copyright (C) 2009-2020 Bart Van Assche <bvanassche@acm.org>.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, see <http://www.gnu.org/licenses/>.

  The GNU General Public License is contained in the file COPYING.
*/


/* Basic definitions needed by (almost) all DRD source files. */


#ifndef __DRD_BASICS_H
#define __DRD_BASICS_H


#include "pub_tool_basics.h"      // Addr


#define DRD_(str) VGAPPEND(vgDrd_, str)


typedef UInt DrdThreadId;


#endif /* __DRD_BASICS_H */
