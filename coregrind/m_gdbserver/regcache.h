/* Register support routines for the remote server for GDB.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

   This file is part of GDB.
   It has been modified to integrate it in valgrind

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#ifndef REGCACHE_H
#define REGCACHE_H

struct inferior_list_entry;

/* Create a new register cache for INFERIOR.  */

void *new_register_cache (void);

/* Release all memory associated with the register cache for INFERIOR.  */

void free_register_cache (void *regcache);

/* Invalidate cached registers for one or all threads.  */

void regcache_invalidate_one (struct inferior_list_entry *);
void regcache_invalidate (void);

/* Convert all registers to a string in the currently specified remote
   format.  */

void registers_to_string (char *buf);

/* Convert a string to register values and fill our register cache.  */

void registers_from_string (char *buf);

/* Return the size in bytes of a string-encoded register packet.  */

int registers_length (void);

/* Return a pointer to the description of register ``n''.  */

struct reg *find_register_by_number (int n);

int register_size (int n);

int find_regno (const char *name);

extern const char **gdbserver_expedite_regs;

/* *mod set to True if *buf provides a new value. */
void supply_register (int n, const void *buf, Bool *mod);

/* Reads register data from buf (hex string in target byte order)
   and stores it in the register cache.
   *mod set to True if *buf provides a new value. */
void supply_register_from_string (int n, const char *buf, Bool *mod);

/* *mod set to True if *buf provides a new value. */
void supply_register_by_name (const char *name, const void *buf, Bool *mod);

void collect_register (int n, void *buf);

void collect_register_as_string (int n, char *buf);

void collect_register_by_name (const char *name, void *buf);

#endif /* REGCACHE_H */
