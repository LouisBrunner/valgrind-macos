
##--------------------------------------------------------------------##
##--- Support routines for the memory checker.                     ---##
##---                                        	      mc_helpers.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of MemCheck, a heavyweight Valgrind tool for
  detecting memory errors.

  Copyright (C) 2000-2003 Julian Seward 
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
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  02111-1307, USA.

  The GNU General Public License is contained in the file COPYING.
*/

#include "mc_constants.h"

.global MC_(helper_value_check0_fail)
MC_(helper_value_check0_fail):
	pushal
	call	MC_(helperc_value_check0_fail)
	popal
	ret

.global MC_(helper_value_check1_fail)
MC_(helper_value_check1_fail):
	pushal
	call	MC_(helperc_value_check1_fail)
	popal
	ret

.global MC_(helper_value_check2_fail)
MC_(helper_value_check2_fail):
	pushal
	call	MC_(helperc_value_check2_fail)
	popal
	ret

.global MC_(helper_value_check4_fail)
MC_(helper_value_check4_fail):
	pushal
	call	MC_(helperc_value_check4_fail)
	popal
	ret

##--------------------------------------------------------------------##
##--- end                                    	      mc_helpers.S ---##
##--------------------------------------------------------------------##

