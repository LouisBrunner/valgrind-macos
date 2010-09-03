/*  Copyright (C) 2007 IBM

    Author: Pete Eberlein  eberlein@us.ibm.com

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

#include <stdio.h>
#include <string.h>



int main(int argc, char *argv[])
{

   long i;
   double f;

   i = 0;
   f = 100.0;

   printf("%lx %f\n", i, f);

 asm("mftgpr %0, %1\n": "=r"(i):"f"(f));

   f = 0.0;
   printf("%lx %f\n", i, f);

 asm("mffgpr %0, %1\n": "=f"(f):"r"(i));

   printf("%lx %f\n", i, f);

   return 0;
}
