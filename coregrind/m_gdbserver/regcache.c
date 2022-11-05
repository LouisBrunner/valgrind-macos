/* Register support routines for the remote server for GDB.
   Copyright (C) 2001, 2002, 2004, 2005, 2011
   Free Software Foundation, Inc.

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

#include "server.h"
#include "regdef.h"

/* The private data for the register cache.  Note that we have one
   per inferior; this is primarily for simplicity, as the performance
   benefit is minimal.  */

struct inferior_regcache_data
{
   unsigned char *registers;
};

static int register_bytes;

static struct reg *reg_defs;
static int num_registers;

const char **gdbserver_expedite_regs;

static
struct inferior_regcache_data * get_regcache (struct thread_info *inf)
{
   struct inferior_regcache_data *regcache;

   regcache = (struct inferior_regcache_data *) inferior_regcache_data (inf);

   if (regcache == NULL)
      fatal ("no register cache\n");

   return regcache;
}

int registers_length (void)
{
   return 2 * register_bytes;
}

void *new_register_cache (void)
{
   struct inferior_regcache_data *regcache;

   regcache = malloc (sizeof (*regcache));

   /* Make sure to zero-initialize the register cache when it is created,
      in case there are registers the target never fetches.  This way they'll
      read as zero instead of garbage.  */
   regcache->registers = calloc (1, register_bytes);
   if (regcache->registers == NULL)
      fatal ("Could not allocate register cache.\n");

   return regcache;
}

void free_register_cache (void *regcache_p)
{
   struct inferior_regcache_data *regcache
      = (struct inferior_regcache_data *) regcache_p;

   free (regcache->registers);
   free (regcache);
}

/* if a regcache exists for entry, reallocate it.
   This is needed if the shadow registers are added.
   In such a case, a 2nd call to set_register_cache is done
   which will cause the reallocation of already created caches. */
static
void regcache_realloc_one (struct inferior_list_entry *entry)
{
   struct thread_info *thread = (struct thread_info *) entry;
   struct inferior_regcache_data *regcache;

   regcache = (struct inferior_regcache_data *) inferior_regcache_data (thread);

   if (regcache) {
      free_register_cache (regcache);
      set_inferior_regcache_data (thread, new_register_cache ());
   }
}

void set_register_cache (struct reg *regs, int n)
{
   int offset, i;

   reg_defs = regs;
   num_registers = n;

   offset = 0;
   for (i = 0; i < n; i++) {
      regs[i].offset = offset;
      offset += regs[i].size;
   }

   register_bytes = offset / 8;

   for_each_inferior (&all_threads, regcache_realloc_one);
}

void registers_to_string (char *buf)
{
   unsigned char *registers = get_regcache (current_inferior)->registers;

   for (int i = 0; i < num_registers; i++)
      valgrind_fetch_register (i, registers + (reg_defs[i].offset / 8));
   convert_int_to_ascii (registers, buf, register_bytes);
}

void registers_from_string (const char *buf)
{
   int len = strlen (buf);
   unsigned char *registers = get_regcache (current_inferior)->registers;

   if (len != register_bytes * 2) {
      warning ("Wrong sized register packet (expected %d bytes, got %d)\n",
               2*register_bytes, len);
      if (len > register_bytes * 2)
         len = register_bytes * 2;
   }
   convert_ascii_to_int (buf, registers, len / 2);
}

int find_regno (const char *name)
{
   int i;

   for (i = 0; i < num_registers; i++)
      if (!strcmp (name, reg_defs[i].name))
         return i;
   fatal ("Unknown register %s requested\n", name);
   return -1;
}

struct reg *find_register_by_number (int n)
{
   return &reg_defs[n];
}

int register_size (int n)
{
   return reg_defs[n].size / 8;
}

void supply_register (int n, const void *buf)
{
   valgrind_store_register (n, buf);
}

void supply_register_from_string (int n, const char *buf)
{
   unsigned char bytes_register[register_size (n)];
   convert_ascii_to_int (buf, bytes_register, register_size (n));
   valgrind_store_register (n, bytes_register);
}

void supply_register_by_name (const char *name, const void *buf)
{
   supply_register (find_regno (name), buf);
}

void collect_register (int n, void *buf)
{
   valgrind_fetch_register (n, buf);
}

void collect_register_as_string (int n, char *buf)
{
   unsigned char local_buf [register_size (n)];
   valgrind_fetch_register (n, local_buf);
   convert_int_to_ascii (local_buf, buf, register_size (n));
}

void collect_register_by_name (const char *name, void *buf)
{
   collect_register (find_regno (name), buf);
}
