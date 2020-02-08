
/*--------------------------------------------------------------------*/
/*--- Declarations common for vgdb and implementations             ---*/
/*--- of vgdb-invoker.                                      vgdb.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Philippe Waroquiers

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

#ifndef __VGDB_H
#define __VGDB_H

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_gdbserver.h"

#include <sys/types.h>

extern Bool timestamp;
extern char *timestamp_str (Bool produce);
extern int debuglevel;

/* Optionally prints a timestamp, then prints the given info.  This should
   be used only at the beginning of a new line.  */
#define TSFPRINTF(stream, ...) (                                        \
      fprintf(stream, "%s", timestamp_str(timestamp)),                  \
      fprintf(stream,  __VA_ARGS__),fflush(stream))

/* if level <= debuglevel, print timestamp, then prints provided debug info */
#define DEBUG(level, ...) (level <= debuglevel ?                        \
                           fprintf(stderr, "%s", timestamp_str(True)),  \
                           fprintf(stderr, __VA_ARGS__),fflush(stderr)  \
                           : 0)

/* same as DEBUG but does not print time stamp info */
#define PDEBUG(level, ...) (level <= debuglevel ?                       \
                            fprintf(stderr, __VA_ARGS__),fflush(stderr) \
                            : 0)

/* if errno != 0,
   report the errno and fprintf the ... varargs on stderr. */
#define ERROR(errno, ...) ((errno == 0 ? 0 : perror("syscall failed")), \
                           fprintf(stderr, "%s", timestamp_str(timestamp)), \
                           fprintf(stderr, __VA_ARGS__),                \
                           fflush(stderr))
/* same as ERROR, but also exits with status 1 */
#define XERROR(errno, ...) ((errno == 0 ? 0 : perror("syscall failed")), \
                            fprintf(stderr, "%s", timestamp_str(timestamp)), \
                            fprintf(stderr, __VA_ARGS__),               \
                            fflush(stderr),                             \
                            exit(1))

/* Calls malloc (size). Exits if memory can't be allocated. */
extern void *vmalloc(size_t size);
/* Calls realloc (size). Exits if memory can't be allocated. */
extern void *vrealloc(void *ptr,size_t size);

/* Will be set to True when any condition indicating we have to shutdown
   is encountered. */
extern Bool shutting_down;

extern VgdbShared32 *shared32;
extern VgdbShared64 *shared64;

/*--------------------------------------------------------------------*/
/*--- Below is vgdb-invoker interface which must be implemented by ---*/
/*--- all vgdb-invoker implementations.                            ---*/
/*--------------------------------------------------------------------*/

/* Possibly produces additional usage information documenting the
   invoker restrictions. */
void invoker_restrictions_msg(void);

/* Restore the registers to the saved value, then detaches from all threads.
   Used as a cleanup handler for thread cancellation. */
void invoker_cleanup_restore_and_detach(void *v_pid);

/* Ensures that the gdbserver code is invoked by pid.
   If an error occurs, resets the valgrind process
   to the state it had before being invoked.
   Returns True if invoke successful, False otherwise. */
Bool invoker_invoke_gdbserver(pid_t pid);

/* Called when connection with valgrind is lost.  In case we
   have lost the connection, it means that Valgrind has closed the
   connection and is busy exiting. We can't and don't have to stop it in
   this case. */
void invoker_valgrind_dying(void);

#endif // __VGDB_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
