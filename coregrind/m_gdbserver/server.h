/* Common definitions for remote server for GDB.
   Copyright (C) 1993, 1995, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
   2006
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

#ifndef SERVER_H
#define SERVER_H

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_syscall.h"
#include "pub_tool_libcproc.h"
#include "pub_core_tooliface.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_options.h"
#include "pub_core_gdbserver.h"
#include "pub_tool_libcsetjmp.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_tool_vki.h"
#include "valgrind.h"

/*------------- interface m_gdbserver <=> low level gdbserver */

/* Initializes gdbserver. After a call to gdbserver_init, vgdb
   can contact the gdbserver embedded in valgrind.
   The rest of the low level gdbserver interface can only
   be called */
extern void gdbserver_init (void);

extern void server_main (void);

/* To be called to indicate that gdbserver usage is finished.
   Resources (e.g. FIFOs) will be destroyed. */
extern void gdbserver_terminate (void);


/* Output string s to the gdb debugging this process or to vgdb.
   Do not call this directly. Rather use VG_(monitor_print) 
   to output something to gdb, use normal valgrind messaging
   (e.g. VG_(umsg)) to send output that can either go
   to gdb or to log. */
extern void monitor_output (char *s);

/* returns 0 if there is no connection or no event on the connection
             with gdb.
   returns 1 if there are some data which has been received from gdb
             and that must (still) be handled.
   returns 2 if remote_desc_activity detected the connection has been
             lost and should be reopened.
   msg is used for debug logging.*/
extern int remote_desc_activity(char *msg);

/* output some status of gdbserver communication */
extern void remote_utils_output_status(void);

/* True if there is a connection with gdb. */
extern Bool remote_connected(void);

/* Finish the connection with gdb and reset_valgrind_sink.
   Keeps the FIFOs and shared mem so as to allow connection
   to be reopened. */
extern void remote_finish(FinishReason reason);

/* If Valgrind sink was changed by gdbserver:
      Resets the valgrind sink to before the changes done by gdbserver,
      and does VG_(umsg). If info != NULL, info added in VG_(usmg). */
extern void reset_valgrind_sink(char* info);

/* For ARM usage.
   Guesses if pc is a thumb pc.
   In this case, returns pc with the thumb bit set (bit0)
   else just returns pc.

   The guess is based on the following set of check:
   if bit0 set      => thumb
   else if bit1 set => thumb
   else uses the debuginfo to guess.
   
   If debug info not found for this pc, assumes arm */
extern Addr thumb_pc (Addr pc);

/* when invoked by vgdb using ptrace, contains the tid chosen
   by vgdb (if vgdb gives a tid different of 0: a 0 tid by
   vgdb means use the running_tid if there is one running
   or tid 1 otherwise). */
extern ThreadId vgdb_interrupted_tid;

/*------------ end of interface to low level gdbserver */


#define dlog(level, ...) \
   do { if (UNLIKELY(VG_(debugLog_getLevel)() >= level))  \
         VG_(debugLog) (level, "gdbsrv",__VA_ARGS__); }   \
   while (0)


/* vki only defines VKI_POLLIN but even not on all OS.
   Below is from linux bits/poll.h */
#ifndef VKI_POLLIN
#define VKI_POLLIN            0x0001
#endif
#define VKI_POLLPRI           0x0002
#define VKI_POLLOUT           0x0004
#define VKI_POLLERR           0x0008
#define VKI_POLLHUP           0x0010
#define VKI_POLLNVAL          0x0020

/* a bunch of macros to avoid libc usage in valgrind-ified gdbserver */ 
#define strcmp(s1,s2)         VG_(strcmp) ((Char *)(s1),(Char *)(s2))
#define strncmp(s1,s2,nmax)   VG_(strncmp) ((Char *)(s1),(Char *)(s2),nmax)
#define strcat(s1,s2)         VG_(strcat) ((Char *)(s1),(Char *)(s2))
#define strcpy(s1,s2)         VG_(strcpy) ((Char *)(s1),(Char *)(s2))
#define strncpy(s1,s2,nmax)   VG_(strncpy) ((Char *)(s1),(Char *)(s2),nmax)
#define strlen(s)             VG_(strlen) ((Char *)(s))
#define strtok(p,s)           (char *) VG_(strtok) ((Char *)(p),(Char *)(s))
#define strtok_r(p,s,ss)      (char *) VG_(strtok_r) ((Char *)(p),(Char *)(s),(Char **)(ss))
#define strchr(s,c)           (char *) VG_(strchr) ((Char *)(s),c)
/* strtol and strtoul supports base 16 or else assumes it is base 10 */
#define strtol(s,r,b)         ((b) == 16 ? \
                               VG_(strtoll16) ((Char *)(s),(Char **)(r)) \
                               : VG_(strtoll10) ((Char *)(s),(Char **)(r)))
#define strtoul(s,r,b)        ((b) == 16 ? \
                               VG_(strtoull16) ((Char *)(s),(Char **)(r)) \
                               : VG_(strtoull10) ((Char *)(s),(Char **)(r)))

#define malloc(sz)            VG_(arena_malloc)  (VG_AR_CORE, "gdbsrv", sz)
#define calloc(n,sz)          VG_(arena_calloc)  (VG_AR_CORE, "gdbsrv", n, sz)
#define realloc(p,size)       VG_(arena_realloc) (VG_AR_CORE, "gdbsrv", p, size)
#define strdup(s)             (char *) VG_(arena_strdup)  (VG_AR_CORE, "gdbsrv", (Char *)(s))
#define free(b)               VG_(arena_free)    (VG_AR_CORE, b)

#ifndef ATTR_NORETURN
#if defined(__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7))
#define ATTR_NORETURN __attribute__ ((noreturn))
#else
#define ATTR_NORETURN           /* nothing */
#endif
#endif

#ifndef ATTR_FORMAT
#if defined(__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 4))
#define ATTR_FORMAT(type, x, y) __attribute__ ((format(type, x, y)))
#else
#define ATTR_FORMAT(type, x, y) /* nothing */
#endif
#endif

/* A type used for binary buffers.  */
typedef unsigned char gdb_byte;

typedef Addr CORE_ADDR;

/* Generic information for tracking a list of ``inferiors'' - threads,
   processes, etc.  */
struct inferior_list
{
   struct inferior_list_entry *head;
   struct inferior_list_entry *tail;
};
struct inferior_list_entry
{
   unsigned long id;
   struct inferior_list_entry *next;
};

/* Opaque type for user-visible threads.  */
struct thread_info;

#include "regcache.h"
#include "gdb/signals.h"

/* signal handling with gdbserver: before delivering a signal,
   call gdbserver_signal_encountered then give control to
   gdbserver by calling call_gdbserver.
   On return, call gdbserver_deliver_signal to effectively
   deliver the signal or not. */
extern void gdbserver_signal_encountered (Int vki_sigNo);
/* between these two calls, call call_gdbserver */
/* If gdbserver_deliver_signal True, then gdb did not ask
   to ignore the signal, so signal can be delivered to the guest. */
extern Bool gdbserver_deliver_signal (Int vki_sigNo);

/* To optimise signal handling, gdb can instruct gdbserver to
   not stop on some signals. In the below, a 1 indicates the gdb_nr signal
   has to be passed directly to the guest, without asking gdb.
   A 0 indicates gdb has to be consulted to see if signal has
   or has not to be passed. The gdb consultation is to
   be done using the above two functions. */
extern int pass_signals[]; /* indexed by gdb signal nr */


#include "target.h"

/* Target-specific functions */

/* From inferiors.c.  */

extern struct inferior_list all_threads;
void add_inferior_to_list (struct inferior_list *list,
			   struct inferior_list_entry *new_inferior);
void for_each_inferior (struct inferior_list *list,
			void (*action) (struct inferior_list_entry *));
extern struct thread_info *current_inferior;
void remove_inferior (struct inferior_list *list,
		      struct inferior_list_entry *entry);
void remove_thread (struct thread_info *thread);
void add_thread (unsigned long thread_id, void *target_data, unsigned int);
unsigned int thread_id_to_gdb_id (unsigned long);
unsigned int thread_to_gdb_id (struct thread_info *);
unsigned long gdb_id_to_thread_id (unsigned int);
struct thread_info *gdb_id_to_thread (unsigned int);
void clear_inferiors (void);
struct inferior_list_entry *find_inferior (struct inferior_list *,
                                           int (*func) (struct 
                                                        inferior_list_entry *,
                                                        void *),
                                           void *arg);
struct inferior_list_entry *find_inferior_id (struct inferior_list *list,
					      unsigned long id);
void *inferior_target_data (struct thread_info *);
void set_inferior_target_data (struct thread_info *, void *);
void *inferior_regcache_data (struct thread_info *);
void set_inferior_regcache_data (struct thread_info *, void *);
void change_inferior_id (struct inferior_list *list,
			 unsigned long new_id);

/* Public variables in server.c */

extern unsigned long cont_thread;
extern unsigned long general_thread;
extern unsigned long step_thread;
extern unsigned long thread_from_wait;
extern unsigned long old_thread_from_wait;

extern VG_MINIMAL_JMP_BUF(toplevel);

/* From remote-utils.c */

extern Bool noack_mode;
int putpkt (char *buf);
int putpkt_binary (char *buf, int len);
int getpkt (char *buf);
void remote_open (char *name);
void remote_close (void);

void sync_gdb_connection (void);
void write_ok (char *buf);
void write_enn (char *buf);
void convert_ascii_to_int (char *from, unsigned char *to, int n);
void convert_int_to_ascii (unsigned char *from, char *to, int n);
void prepare_resume_reply (char *buf, char status, unsigned char sig);

void decode_address (CORE_ADDR *addrp, const char *start, int len);
void decode_m_packet (char *from, CORE_ADDR * mem_addr_ptr,
		      unsigned int *len_ptr);
void decode_M_packet (char *from, CORE_ADDR * mem_addr_ptr,
		      unsigned int *len_ptr, unsigned char *to);
int decode_X_packet (char *from, int packet_len, CORE_ADDR * mem_addr_ptr,
		     unsigned int *len_ptr, unsigned char *to);

int unhexify (char *bin, const char *hex, int count);
int hexify (char *hex, const char *bin, int count);
/* heximage builds an image of bin according to byte order of the architecture 
   Useful for register and int image */
char* heximage (char *buf, char *bin, int count);

/* convert from CORE_ADDR to void* */
void* C2v(CORE_ADDR addr);


int remote_escape_output (const gdb_byte *buffer, int len,
			  gdb_byte *out_buf, int *out_len,
			  int out_maxlen);

/* Functions from ``signals.c''.  */
enum target_signal target_signal_from_host (int hostsig);
int target_signal_to_host_p (enum target_signal oursig);
int target_signal_to_host (enum target_signal oursig);
char *target_signal_to_name (enum target_signal);

/* Functions from utils.c */

/* error is like VG_(umsg), then VG_MINIMAL_LONGJMP to gdbserver toplevel. */
void error (const char *string,...) ATTR_NORETURN ATTR_FORMAT (printf, 1, 2);
/* first output a description of the error inside sr, then like VG_(umsg). */
void sr_perror (SysRes sr,char *string,...) ATTR_FORMAT (printf, 2, 3);
/* fatal is like VG_(umsg), then exit(1). */
void fatal (const char *string,...) ATTR_NORETURN ATTR_FORMAT (printf, 1, 2);
/* warning is like VG_(umsg). */
void warning (const char *string,...) ATTR_FORMAT (printf, 1, 2);

/* Functions from the register cache definition.  */

void init_registers (void);

/* Maximum number of bytes to read/write at once.  The value here
   is chosen to fill up a packet (the headers account for the 32).  */
#define MAXBUFBYTES(N) (((N)-32)/2)

/* PBUFSIZ : Buffers size for transferring memory, registers, etc.
   Must be big enough to hold all the registers, at least.
   Must be at least big as 2*DATASIZ + 5:
      1         : packet begin ($ or %)
    + 2*DATASIZ : encoded string
    + 1         : packet end (#)
    + 2         : packet checksum
    + 1         : \0 

    Max value gdb likes is 16384.

    Note that what is sent/received to/from gdb does
    not have a trailing null byte. We are adding 1 here to allow
    null terminating the strings e.g. for printf.

    => packet Packet OVERHead SIZe is 5:*/

/* keep PBUFSIZ value in sync with vgdb.c */
#define PBUFSIZ 16384
#define POVERHSIZ 5

/* Max size of a string encoded in a packet. Hex Encoding can
   multiply the size by 2 (trailing null byte not sent). */
#define DATASIZ ((PBUFSIZ-POVERHSIZ)/2)

/* Version information, from version.c.  */
extern const char version[];

#endif /* SERVER_H */
