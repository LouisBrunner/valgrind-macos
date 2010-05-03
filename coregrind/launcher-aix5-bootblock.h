
/*--------------------------------------------------------------------*/
/*--- Structure written into the child process by launcher-aix5.c. ---*/
/*---                                    launcher-aix5-bootblock.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2010 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/


/* This is the structure written into the child process by the AIX5
   launcher.  The tool's entry point (see bottom of
   coregrind/m_main.c) must be aware of the layout as it will have to
   read info from it at startup.  This struct is designed to work
   equally well in 32- and 64-bit mode.  It must be placed at an
   8-aligned address in the child. */

#define N_BOOTBLOCK_INSNS    60  /* number of insns */
#define N_BOOTBLOCK_ERRMSG   64  /* max bytes in the error message */
#define N_BOOTBLOCK_TOOLFILE 256 /* max bytes in the tool file name */

typedef

   struct {
      /* Adler32 checksum of the uncompressed data of the compressed
	 page (the second part of which contains this struct. */
      /*   0 */ UInt adler32;
 
      /* The system call numbers for enough critical syscalls that the
         tool can start both debug logging and also read the
         /proc/../sysent file. */
      /*   4 */ UInt __NR_getpid;
      /*   8 */ UInt __NR_write;
      /*  12 */ UInt __NR_exit;
      /*  16 */ UInt __NR_open;
      /*  20 */ UInt __NR_read;
      /*  24 */ UInt __NR_close;
      /*  28 */ UInt __off28;

      /* The 37 integer registers for the client, as they should be at
         startup.  On 32-bit targets the registers are stored in the
         lower half of each quadword, which, since this is a bigendian
         platform, is the higher-addressed 4 bytes. */
      /* MUST BE 8-aligned */
      /*  32 */ ULong iregs_pc_cr_lr_ctr_xer[37];

      /* The instructions for the bootstrap loader. */
      /* 328 */ UInt code[N_BOOTBLOCK_INSNS];

      /* A zero-terminated error message to be used when the bootstrap
         loader fails. */
      /* 628 */ UChar errmsg[N_BOOTBLOCK_ERRMSG];

      /* The name of the tool file, again zero-terminated. */
      /* 692 */ UChar toolfile[N_BOOTBLOCK_TOOLFILE];

      /* 1024 */
   }
   AIX5Bootblock;

/*--------------------------------------------------------------------*/
/*--- end                                launcher-aix5-bootblock.h ---*/
/*--------------------------------------------------------------------*/
