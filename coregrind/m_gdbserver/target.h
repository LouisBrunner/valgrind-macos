/* Target operations for the Valgrind remote server for GDB.
   Copyright (C) 2002, 2003, 2004, 2005, 2012
   Free Software Foundation, Inc.
   Philippe Waroquiers.

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

#include "pub_core_basics.h"    // Addr
#include "server.h"             // CORE_ADDR

/* This file defines the architecture independent Valgrind gdbserver
   high level operations such as read memory, get/set registers, ...

   These high level operations are called by the gdbserver
   protocol implementation (e.g. typically server.c).
   
   For some of these high level operations, target.c will call
   low level operations dependent on the architecture.
   
   For example, getting or setting the registers will work on a
   register cache. The exact details of the registers (how much,
   their size, etc) is not defined by target.c or the register cache.

   Such architecture dependent information is defined by
   valgrind_low.h/valgrind-low-xxxxx.c providing 'low level operations'
   specific to the xxxxx architecture (for example,
   valgrind-low-x86.c, valgrind-low-armc.c). */
        
/* -------------------------------------------------------------------------- */
/* ------------------------ Initialisation ---------------------------------- */
/* -------------------------------------------------------------------------- */

/* Initialize the Valgrind high target. This will in turn
   initialise the low (architecture specific) target. */
extern void valgrind_initialize_target(void);

/* initialize or re-initialize the register set of the low target.
   if shadow_mode, then (re-)define the normal and valgrind shadow registers
   else (re-)define only the normal registers. */
extern void initialize_shadow_low (Bool shadow_mode);

/* Returns the name of the xml target description file. 
   returns NULL if no xml target description available.
   if shadow_mode, then returns the xml target description
   with the shadow registers
   else returns the xml target description only for
   the normal registers. */
extern const char* valgrind_target_xml (Bool shadow_mode);


/* -------------------------------------------------------------------------- */
/* --------------------------- Execution control ---------------------------- */
/* -------------------------------------------------------------------------- */

/* This structure describes how to resume the execution.
   Currently, there is no way to resume only a specific thread.  */
struct thread_resume
{
  /* If non-zero, we want to single-step.  */
  int step;

  /* If non-zero, send this signal when we resume.  */
  int sig;
};

/* Prepare to Resume (i.e. restart) the guest.
   The resume info indicates how the resume will be done. 
   In case GDB has changed the program counter, valgrind_resume
   will also ensure that the execution will be resumed at this
   new program counter.
   The Resume is really only executed once the gdbserver
   returns (giving back the control to Valgrind). */
extern void valgrind_resume (struct thread_resume *resume_info);

/* When Valgrind gets the control, it will execute the guest
   process till there is a reason to call the gdbserver
   again (e.g. because a breakpoint is encountered or the
   tool reports an error).
   In such case, the executionof guest code  stops, and the
   control is given to gdbserver. Gdbserver will send a resume
   reply packet to GDB.

   valgrind_wait gets from Valgrind data structures the
   information needed produce the resume reply for GDB:
   a.o. OURSTATUS will be filled in with a response code to send to GDB.

   Returns the signal which caused the process to stop, in the
   remote protocol numbering (e.g. TARGET_SIGNAL_STOP), or the
   exit code as an integer if *OURSTATUS is 'W'.  */
extern unsigned char valgrind_wait (char *outstatus);

/* When execution is stopped and gdbserver has control, more
   info about the stop reason can be retrieved using the following
   functions. */

/* gets the addr at which a (possible) break must be ignored once.
   If there is no such break to be ignored once, 0 is returned.
   This is needed for the following case:
   The user sets a break at address AAA.
   The break is encountered. Then the user does stepi 
   (i.e. step one instruction).
   In such a case, the already encountered break must be ignored
   to ensure the stepi will advance by one instruction: a "break"
   is implemented in valgrind by some helper code just after the
   instruction mark at which the break is set. This helper code
   verifies if either there is a break at the current PC
   or if we are in stepping mode. If we are in stepping mode,
   the already encountered break must be ignored once to advance
   to the next instruction.
   ??? need to check if this is *really* needed. */
extern Addr valgrind_get_ignore_break_once(void);

/* When addr > 0, ensures the next resume reply packet informs
   gdb about the encountered watchpoint.
   valgrind_stopped_by_watchpoint() will return 1 till reset.
   Use addr 0x0 to reset. */
extern void VG_(set_watchpoint_stop_address) (Addr addr);

/* Returns 1 if target was stopped due to a watchpoint hit, 0 otherwise.  */
extern int valgrind_stopped_by_watchpoint (void);

/* Returns the address associated with the watchpoint that hit, if any;  
   returns 0 otherwise.  */
extern CORE_ADDR valgrind_stopped_data_address (void);


/* Inform GDB (if needed) that client is before (or after) syscall sysno.
   sysno -1 is used to clear the fact that a syscall has been encountered. */
extern void gdbserver_syscall_encountered (Bool before, Int sysno);

/* >= 0 if valgrind stopped due to syscall, -1 if not stopped due to syscall. */
extern Int valgrind_stopped_by_syscall (void);

/* if valgrind_stopped_by_syscall() >= 0, tells if stopped before or after
   syscall. */
extern Bool valgrind_stopped_before_syscall (void);

/* True if gdbserver is single stepping the valgrind process */
extern Bool valgrind_single_stepping (void);

/* Set Valgrind in single stepping mode or not according to Bool. */
extern void valgrind_set_single_stepping (Bool);

/* -------------------------------------------------------------------------- */
/* ----------------- Examining/modifying data while stopped ----------------- */
/* -------------------------------------------------------------------------- */

/* Return 1 iff the thread with ID tid is alive.  */
extern int valgrind_thread_alive (unsigned long tid);

/* Allows to controls the thread (current_inferior) used for following
   valgrind_(fetch|store)_registers calls.
   If USE_GENERAL,
     current_inferior is set to general_thread
   else
     current_inferior is set to step_thread or else cont_thread.
   If the above gives no valid thread, then current_inferior is
   set to the first valid thread. */
extern void set_desired_inferior (int use_general);

/* Fetch register regno from the current_inferior thread and put its value in buf.  */
extern void valgrind_fetch_register (int regno, unsigned char *buf);

/* Store register REGNO value from BUF to the VEX valgrind state.  */
extern void valgrind_store_register (int regno, const unsigned char *buf);



/* Read memory from the inferior process.
   Read LEN bytes at MEMADDR into a buffer at MYADDR.
   Returns 0 on success and errno on failure.  */
extern int valgrind_read_memory (CORE_ADDR memaddr,
                                 unsigned char *myaddr, int len);

/* Write memory to the inferior process.
   Write LEN bytes from the buffer at MYADDR to MEMADDR.
   Returns 0 on success and errno on failure.  */
extern int valgrind_write_memory (CORE_ADDR memaddr,
                                  const unsigned char *myaddr, int len);


/* Insert and remove a hardware watchpoint.
   Returns 0 on success, -1 on failure and 1 on unsupported.  
   The type is coded as follows:
   2 = write watchpoint
   3 = read watchpoint
   4 = access watchpoint
*/
extern int valgrind_insert_watchpoint (char type, CORE_ADDR addr, int len);
extern int valgrind_remove_watchpoint (char type, CORE_ADDR addr, int len);

/* Get the address of a thread local variable.
   'tst' is the thread for which thread local address is searched for.
   'offset' is the offset of the variable in the tls data of the load
   module identified by 'lm'.
   'lm' is the link_map address of the loaded  module : it is the address
   of the data structure used by the dynamic linker to maintain various
   information about a loaded object.
   
   Returns True if the address of the variable could be found.
      *tls_addr is then set to this address.
   Returns False if tls support is not available for this arch, or
   if an error occurred. *tls_addr is set to NULL. */
extern Bool valgrind_get_tls_addr (ThreadState *tst,
                                   CORE_ADDR offset,
                                   CORE_ADDR lm,
                                   CORE_ADDR *tls_addr);


/* -------------------------------------------------------------------------- */
/* ----------- Utils functions for low level arch specific files ------------ */
/* -------------------------------------------------------------------------- */


/* returns a pointer to the architecture state corresponding to
   the provided register set: 0 => normal guest registers,
                              1 => shadow1
                              2 => shadow2
*/
extern VexGuestArchState* get_arch (int set, ThreadState* tst);

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


// True means gdbserver can access (internal) Valgrind memory.
// Otherwise, only the client memory can be accessed.
extern Bool hostvisibility;

#endif /* TARGET_H */
