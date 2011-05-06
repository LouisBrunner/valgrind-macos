/* Target operations for the remote server for GDB.
   Copyright (C) 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

   Contributed by MontaVista Software.

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

#ifndef TARGET_H
#define TARGET_H

/* This structure describes how to resume a particular thread (or
   all threads) based on the client's request.  If thread is -1, then
   this entry applies to all threads.  These are generally passed around
   as an array, and terminated by a thread == -1 entry.  */

struct thread_resume
{
  unsigned long thread;

  /* If non-zero, leave this thread stopped.  */
  int leave_stopped;

  /* If non-zero, we want to single-step.  */
  int step;

  /* If non-zero, send this signal when we resume.  */
  int sig;
};

struct target_ops
{
  /* Return 1 iff the thread with process ID PID is alive.  */

  int (*thread_alive) (unsigned long pid);

  /* Resume the inferior process.  */

  void (*resume) (struct thread_resume *resume_info);

  /* Wait for the inferior process to change state.

     STATUS will be filled in with a response code to send to GDB.

     Returns the signal which caused the process to stop, in the
     remote protocol numbering (e.g. TARGET_SIGNAL_STOP), or the
     exit code as an integer if *STATUS is 'W'.  */

  unsigned char (*wait) (char *status);

  /* Fetch registers from the inferior process.

     If REGNO is -1, fetch all registers; otherwise, fetch at least REGNO.  */

  void (*fetch_registers) (int regno);

  /* Store registers to the inferior process.

     If REGNO is -1, store all registers; otherwise, store at least REGNO.  */

  void (*store_registers) (int regno);

  /* Read memory from the inferior process.  This should generally be
     called through read_inferior_memory, which handles breakpoint shadowing.

     Read LEN bytes at MEMADDR into a buffer at MYADDR.
  
     Returns 0 on success and errno on failure.  */

  int (*read_memory) (CORE_ADDR memaddr, unsigned char *myaddr, int len);

  /* Write memory to the inferior process.  This should generally be
     called through write_inferior_memory, which handles breakpoint shadowing.

     Write LEN bytes from the buffer at MYADDR to MEMADDR.

     Returns 0 on success and errno on failure.  */

  int (*write_memory) (CORE_ADDR memaddr, const unsigned char *myaddr,
		       int len);

  /* Send a signal to the inferior process, however is appropriate.  */
  void (*send_signal) (int);

  /* Returns the name of the xml target description file. 
     returns NULL if no xml target description available. */
  char* (*target_xml)(void);

  /* Same but describes also the shadow registers. */
  char* (*shadow_target_xml)(void);

  /* Insert and remove a hardware watchpoint.
     Returns 0 on success, -1 on failure and 1 on unsupported.  
     The type is coded as follows:
       2 = write watchpoint
       3 = read watchpoint
       4 = access watchpoint
  */

  int (*insert_watchpoint) (char type, CORE_ADDR addr, int len);
  int (*remove_watchpoint) (char type, CORE_ADDR addr, int len);

  /* Returns 1 if target was stopped due to a watchpoint hit, 0 otherwise.  */

  int (*stopped_by_watchpoint) (void);

  /* Returns the address associated with the watchpoint that hit, if any;  
     returns 0 otherwise.  */

  CORE_ADDR (*stopped_data_address) (void);

};

extern struct target_ops *the_target;

void set_target_ops (struct target_ops *);

#define detach_inferior() \
  (*the_target->detach) ()

#define mythread_alive(pid) \
  (*the_target->thread_alive) (pid)

#define fetch_inferior_registers(regno) \
  (*the_target->fetch_registers) (regno)

#define store_inferior_registers(regno) \
  (*the_target->store_registers) (regno)

#define mywait(statusp) \
  (*the_target->wait) (statusp)

int read_inferior_memory (CORE_ADDR memaddr, unsigned char *myaddr, int len);

int write_inferior_memory (CORE_ADDR memaddr, const unsigned char *myaddr,
			   int len);

void set_desired_inferior (int id);

/* like memcpy but first check if content of destination and source
   differs. If no difference, no copy is done, *mod set to False.
   If different; copy is done, *mod set to True. */
extern void* VG_(dmemcpy) ( void *d, const void *s, SizeT sz, Bool *mod );

typedef
   enum {
      valgrind_to_gdbserver,
      gdbserver_to_valgrind} transfer_direction;

// According to dir, calls VG_(dmemcpy) 
// to copy data from/to valgrind to/from gdbserver.
// If the transferred data differs from what is currently stored,
// sets *mod to True otherwise set *mod to False.
extern void  VG_(transfer) (void *valgrind,
                            void *gdbserver,
                            transfer_direction dir,
                            SizeT sz,
                            Bool *mod);

#endif /* TARGET_H */
