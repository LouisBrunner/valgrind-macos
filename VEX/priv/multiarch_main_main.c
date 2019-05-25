/*---------------------------------------------------------------*/
/*--- Begin                             multiarch_main_main.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2015-2017 Philippe Waroquiers

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* This file is used to have main_main.c compiled with VEXMULTIARCH
   defined, so that all host and guest arch are callable from LibVEX_Translate
   and other functions defined in main_main.c.
   The resulting object file will be put in libvexmultiarch-<platform>-os>.a.

   The valgrind tools are making the assumption that host and guest are
   the same. So, no need to drag the full set of archs when
   linking a tool.
   The VEX library is nicely split in arch independent and arch dependent
   objects. Only main_main.c is dragging the various arch specific files.
   So, main_main.c (the main entry point of the VEX library) is compiled
   only for the current guest/host arch.

   This file ensures we recompile main_main.c with all archs activated.
   
   So, a VEX user can decide (at link time) to use a 'single arch' VEX lib,
   or to use a multiarch VEX lib.
   If t1.o is a 'main' that calls LibVEX_Translate, then
   to link with a single arch VEX lib, use e.g. the following :
     gcc -o t1single t1.o -LInst/lib/valgrind  -lvex-amd64-linux -lgcc

   to link with a multi arch VEX lib, you must insert 
     -lvexmultiarch-amd64-linux *before* -lvex-amd64-linux
   i.e;
     gcc -o t1multi t1.o \
        -LInst/lib/valgrind -lvexmultiarch-amd64-linux -lvex-amd64-linux -lgcc

   t1single will only be able to translate from amd64 to amd64.
   t1multi will be able to translate from any arch supported by VEX
   to any other arch supported by VEX.
   Note however that multiarch support is experimental and poorly
   or not tested.
*/

#define VEXMULTIARCH 1
#include "main_main.c"

/*---------------------------------------------------------------*/
/*--- end                               multiarch_main_main.c ---*/
/*---------------------------------------------------------------*/
