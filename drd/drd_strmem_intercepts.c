/*--------------------------------------------------------------------*/
/*--- Replacements for strlen() and strnlen(), which run on the    ---*/
/*--- simulated CPU.                                               ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of DRD, a heavyweight Valgrind tool for
  detecting threading errors. The code below has been extracted
  from memchec/mc_replace_strmem.c, which has the following copyright
  notice:

  Copyright (C) 2000-2017 Julian Seward
  jseward@acm.org

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

#include "../shared/vg_replace_strmem.c"
