/* Target signal translation functions for GDB.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2011 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

#if defined(VGO_darwin)
// ???? darwin signal.h defines SIGPOLL conditionnally ????
#ifndef SIGPOLL
#define SIGPOLL 7
#endif
#endif

enum target_signal target_signal_from_name (const char *name);
enum target_signal target_signal_from_command (int num);

/* This table must match in order and size the signals in enum target_signal
   in gdb/signals.h. */
/* *INDENT-OFF* */
static struct {
   const char *name;
   const char *string;
} signals [] =
   {
      {"0", "Signal 0"},
      {"SIGHUP", "Hangup"},
      {"SIGINT", "Interrupt"},
      {"SIGQUIT", "Quit"},
      {"SIGILL", "Illegal instruction"},
      {"SIGTRAP", "Trace/breakpoint trap"},
      {"SIGABRT", "Aborted"},
      {"SIGEMT", "Emulation trap"},
      {"SIGFPE", "Arithmetic exception"},
      {"SIGKILL", "Killed"},
      {"SIGBUS", "Bus error"},
      {"SIGSEGV", "Segmentation fault"},
      {"SIGSYS", "Bad system call"},
      {"SIGPIPE", "Broken pipe"},
      {"SIGALRM", "Alarm clock"},
      {"SIGTERM", "Terminated"},
      {"SIGURG", "Urgent I/O condition"},
      {"SIGSTOP", "Stopped (signal)"},
      {"SIGTSTP", "Stopped (user)"},
      {"SIGCONT", "Continued"},
      {"SIGCHLD", "Child status changed"},
      {"SIGTTIN", "Stopped (tty input)"},
      {"SIGTTOU", "Stopped (tty output)"},
      {"SIGIO", "I/O possible"},
      {"SIGXCPU", "CPU time limit exceeded"},
      {"SIGXFSZ", "File size limit exceeded"},
      {"SIGVTALRM", "Virtual timer expired"},
      {"SIGPROF", "Profiling timer expired"},
      {"SIGWINCH", "Window size changed"},
      {"SIGLOST", "Resource lost"},
      {"SIGUSR1", "User defined signal 1"},
      {"SIGUSR2", "User defined signal 2"},
      {"SIGPWR", "Power fail/restart"},
      {"SIGPOLL", "Pollable event occurred"},
      {"SIGWIND", "SIGWIND"},
      {"SIGPHONE", "SIGPHONE"},
      {"SIGWAITING", "Process's LWPs are blocked"},
      {"SIGLWP", "Signal LWP"}, /* FreeBSD SIGTHR */
      {"SIGDANGER", "Swap space dangerously low"},
      {"SIGGRANT", "Monitor mode granted"},
      {"SIGRETRACT", "Need to relinquish monitor mode"},
      {"SIGMSG", "Monitor mode data available"},
      {"SIGSOUND", "Sound completed"},
      {"SIGSAK", "Secure attention"},
      {"SIGPRIO", "SIGPRIO"},
      {"SIG33", "Real-time event 33"},
      {"SIG34", "Real-time event 34"},
      {"SIG35", "Real-time event 35"},
      {"SIG36", "Real-time event 36"},
      {"SIG37", "Real-time event 37"},
      {"SIG38", "Real-time event 38"},
      {"SIG39", "Real-time event 39"},
      {"SIG40", "Real-time event 40"},
      {"SIG41", "Real-time event 41"},
      {"SIG42", "Real-time event 42"},
      {"SIG43", "Real-time event 43"},
      {"SIG44", "Real-time event 44"},
      {"SIG45", "Real-time event 45"},
      {"SIG46", "Real-time event 46"},
      {"SIG47", "Real-time event 47"},
      {"SIG48", "Real-time event 48"},
      {"SIG49", "Real-time event 49"},
      {"SIG50", "Real-time event 50"},
      {"SIG51", "Real-time event 51"},
      {"SIG52", "Real-time event 52"},
      {"SIG53", "Real-time event 53"},
      {"SIG54", "Real-time event 54"},
      {"SIG55", "Real-time event 55"},
      {"SIG56", "Real-time event 56"},
      {"SIG57", "Real-time event 57"},
      {"SIG58", "Real-time event 58"},
      {"SIG59", "Real-time event 59"},
      {"SIG60", "Real-time event 60"},
      {"SIG61", "Real-time event 61"},
      {"SIG62", "Real-time event 62"},
      {"SIG63", "Real-time event 63"},
      {"SIGCANCEL", "LWP internal signal"},
      {"SIG32", "Real-time event 32"},
      {"SIG64", "Real-time event 64"},
      {"SIG65", "Real-time event 65"},
      {"SIG66", "Real-time event 66"},
      {"SIG67", "Real-time event 67"},
      {"SIG68", "Real-time event 68"},
      {"SIG69", "Real-time event 69"},
      {"SIG70", "Real-time event 70"},
      {"SIG71", "Real-time event 71"},
      {"SIG72", "Real-time event 72"},
      {"SIG73", "Real-time event 73"},
      {"SIG74", "Real-time event 74"},
      {"SIG75", "Real-time event 75"},
      {"SIG76", "Real-time event 76"},
      {"SIG77", "Real-time event 77"},
      {"SIG78", "Real-time event 78"},
      {"SIG79", "Real-time event 79"},
      {"SIG80", "Real-time event 80"},
      {"SIG81", "Real-time event 81"},
      {"SIG82", "Real-time event 82"},
      {"SIG83", "Real-time event 83"},
      {"SIG84", "Real-time event 84"},
      {"SIG85", "Real-time event 85"},
      {"SIG86", "Real-time event 86"},
      {"SIG87", "Real-time event 87"},
      {"SIG88", "Real-time event 88"},
      {"SIG89", "Real-time event 89"},
      {"SIG90", "Real-time event 90"},
      {"SIG91", "Real-time event 91"},
      {"SIG92", "Real-time event 92"},
      {"SIG93", "Real-time event 93"},
      {"SIG94", "Real-time event 94"},
      {"SIG95", "Real-time event 95"},
      {"SIG96", "Real-time event 96"},
      {"SIG97", "Real-time event 97"},
      {"SIG98", "Real-time event 98"},
      {"SIG99", "Real-time event 99"},
      {"SIG100", "Real-time event 100"},
      {"SIG101", "Real-time event 101"},
      {"SIG102", "Real-time event 102"},
      {"SIG103", "Real-time event 103"},
      {"SIG104", "Real-time event 104"},
      {"SIG105", "Real-time event 105"},
      {"SIG106", "Real-time event 106"},
      {"SIG107", "Real-time event 107"},
      {"SIG108", "Real-time event 108"},
      {"SIG109", "Real-time event 109"},
      {"SIG110", "Real-time event 110"},
      {"SIG111", "Real-time event 111"},
      {"SIG112", "Real-time event 112"},
      {"SIG113", "Real-time event 113"},
      {"SIG114", "Real-time event 114"},
      {"SIG115", "Real-time event 115"},
      {"SIG116", "Real-time event 116"},
      {"SIG117", "Real-time event 117"},
      {"SIG118", "Real-time event 118"},
      {"SIG119", "Real-time event 119"},
      {"SIG120", "Real-time event 120"},
      {"SIG121", "Real-time event 121"},
      {"SIG122", "Real-time event 122"},
      {"SIG123", "Real-time event 123"},
      {"SIG124", "Real-time event 124"},
      {"SIG125", "Real-time event 125"},
      {"SIG126", "Real-time event 126"},
      {"SIG127", "Real-time event 127"},

      {"SIGINFO", "Information request"},

      {NULL, "Unknown signal"},
      {NULL, "Internal error: printing TARGET_SIGNAL_DEFAULT"},

      /* Mach exceptions */
      {"EXC_BAD_ACCESS", "Could not access memory"},
      {"EXC_BAD_INSTRUCTION", "Illegal instruction/operand"},
      {"EXC_ARITHMETIC", "Arithmetic exception"},
      {"EXC_EMULATION", "Emulation instruction"},
      {"EXC_SOFTWARE", "Software generated exception"},
      {"EXC_BREAKPOINT", "Breakpoint"},

      {"SIGLIBRT", "librt internal signal"},

      /* Last entry, used to check whether the table is the right size.  */
      {NULL, "TARGET_SIGNAL_MAGIC"}
   };
/* *INDENT-ON* */



/* Return the name for a signal.  */
const char *target_signal_to_name (enum target_signal sig)
{
   if ((sig >= TARGET_SIGNAL_FIRST) && (sig <= TARGET_SIGNAL_LAST)
       && signals[sig].name != NULL)
      return signals[sig].name;
   else
      /* I think the code which prints this will always print it along
         with the string, so no need to be verbose (very old comment).  */
      return "?";
}

/* Given a name, return its signal.  */
enum target_signal target_signal_from_name (const char *name)
{
   enum target_signal sig;

   /* It's possible we also should allow "SIGCLD" as well as "SIGCHLD"
      for TARGET_SIGNAL_SIGCHLD.  SIGIOT, on the other hand, is more
      questionable; seems like by now people should call it SIGABRT
      instead.  */

   /* This ugly cast brought to you by the native VAX compiler.  */
   for (sig = TARGET_SIGNAL_HUP;
        sig < TARGET_SIGNAL_LAST;
        sig = (enum target_signal) ((int) sig + 1))
      if (signals[sig].name != NULL
          && strcmp (name, signals[sig].name) == 0)
         return sig;
   return TARGET_SIGNAL_UNKNOWN;
}


/* The following functions are to help certain targets deal
   with the signal/waitstatus stuff.  They could just as well be in
   a file called native-utils.c or unixwaitstatus-utils.c or whatever.  */

/* Convert host signal to our signals.  */
enum target_signal target_signal_from_host (int hostsig)
{
   /* A switch statement would make sense but would require special kludges
      to deal with the cases where more than one signal has the same number.  */

   if (hostsig == 0)
      return TARGET_SIGNAL_0;

#if defined (VKI_SIGHUP)
   if (hostsig == VKI_SIGHUP)
      return TARGET_SIGNAL_HUP;
#endif
#if defined (VKI_SIGINT)
   if (hostsig == VKI_SIGINT)
      return TARGET_SIGNAL_INT;
#endif
#if defined (VKI_SIGQUIT)
   if (hostsig == VKI_SIGQUIT)
      return TARGET_SIGNAL_QUIT;
#endif
#if defined (VKI_SIGILL)
   if (hostsig == VKI_SIGILL)
      return TARGET_SIGNAL_ILL;
#endif
#if defined (VKI_SIGTRAP)
   if (hostsig == VKI_SIGTRAP)
      return TARGET_SIGNAL_TRAP;
#endif
#if defined (VKI_SIGABRT)
   if (hostsig == VKI_SIGABRT)
      return TARGET_SIGNAL_ABRT;
#endif
#if defined (VKI_SIGEMT)
   if (hostsig == VKI_SIGEMT)
      return TARGET_SIGNAL_EMT;
#endif
#if defined (VKI_SIGFPE)
   if (hostsig == VKI_SIGFPE)
      return TARGET_SIGNAL_FPE;
#endif
#if defined (VKI_SIGKILL)
   if (hostsig == VKI_SIGKILL)
      return TARGET_SIGNAL_KILL;
#endif
#if defined (VKI_SIGBUS)
   if (hostsig == VKI_SIGBUS)
      return TARGET_SIGNAL_BUS;
#endif
#if defined (VKI_SIGSEGV)
   if (hostsig == VKI_SIGSEGV)
      return TARGET_SIGNAL_SEGV;
#endif
#if defined (VKI_SIGSYS)
   if (hostsig == VKI_SIGSYS)
      return TARGET_SIGNAL_SYS;
#endif
#if defined (VKI_SIGPIPE)
   if (hostsig == VKI_SIGPIPE)
      return TARGET_SIGNAL_PIPE;
#endif
#if defined (VKI_SIGALRM)
   if (hostsig == VKI_SIGALRM)
      return TARGET_SIGNAL_ALRM;
#endif
#if defined (VKI_SIGTERM)
   if (hostsig == VKI_SIGTERM)
      return TARGET_SIGNAL_TERM;
#endif
#if defined (VKI_SIGUSR1)
   if (hostsig == VKI_SIGUSR1)
      return TARGET_SIGNAL_USR1;
#endif
#if defined (VKI_SIGUSR2)
   if (hostsig == VKI_SIGUSR2)
      return TARGET_SIGNAL_USR2;
#endif
#if defined (VKI_SIGCLD)
   if (hostsig == VKI_SIGCLD)
      return TARGET_SIGNAL_CHLD;
#endif
#if defined (VKI_SIGCHLD)
   if (hostsig == VKI_SIGCHLD)
      return TARGET_SIGNAL_CHLD;
#endif
#if defined (VKI_SIGPWR)
   if (hostsig == VKI_SIGPWR)
      return TARGET_SIGNAL_PWR;
#endif
#if defined (VKI_SIGWINCH)
   if (hostsig == VKI_SIGWINCH)
      return TARGET_SIGNAL_WINCH;
#endif
#if defined (VKI_SIGURG)
   if (hostsig == VKI_SIGURG)
      return TARGET_SIGNAL_URG;
#endif
#if defined (VKI_SIGIO)
   if (hostsig == VKI_SIGIO)
      return TARGET_SIGNAL_IO;
#endif
#if defined (VKI_SIGPOLL)
   if (hostsig == VKI_SIGPOLL)
      return TARGET_SIGNAL_POLL;
#endif
#if defined (VKI_SIGSTOP)
   if (hostsig == VKI_SIGSTOP)
      return TARGET_SIGNAL_STOP;
#endif
#if defined (VKI_SIGTSTP)
   if (hostsig == VKI_SIGTSTP)
      return TARGET_SIGNAL_TSTP;
#endif
#if defined (VKI_SIGCONT)
   if (hostsig == VKI_SIGCONT)
      return TARGET_SIGNAL_CONT;
#endif
#if defined (VKI_SIGTTIN)
   if (hostsig == VKI_SIGTTIN)
      return TARGET_SIGNAL_TTIN;
#endif
#if defined (VKI_SIGTTOU)
   if (hostsig == VKI_SIGTTOU)
      return TARGET_SIGNAL_TTOU;
#endif
#if defined (VKI_SIGVTALRM)
   if (hostsig == VKI_SIGVTALRM)
      return TARGET_SIGNAL_VTALRM;
#endif
#if defined (VKI_SIGPROF)
   if (hostsig == VKI_SIGPROF)
      return TARGET_SIGNAL_PROF;
#endif
#if defined (VKI_SIGXCPU)
   if (hostsig == VKI_SIGXCPU)
      return TARGET_SIGNAL_XCPU;
#endif
#if defined (VKI_SIGXFSZ)
   if (hostsig == VKI_SIGXFSZ)
      return TARGET_SIGNAL_XFSZ;
#endif
#if defined (VKI_SIGWIND)
   if (hostsig == VKI_SIGWIND)
      return TARGET_SIGNAL_WIND;
#endif
#if defined (VKI_SIGPHONE)
   if (hostsig == VKI_SIGPHONE)
      return TARGET_SIGNAL_PHONE;
#endif
#if defined (VKI_SIGLOST)
   if (hostsig == VKI_SIGLOST)
      return TARGET_SIGNAL_LOST;
#endif
#if defined (VKI_SIGWAITING)
   if (hostsig == VKI_SIGWAITING)
      return TARGET_SIGNAL_WAITING;
#endif
#if defined (VKI_SIGCANCEL)
   if (hostsig == VKI_SIGCANCEL)
      return TARGET_SIGNAL_CANCEL;
#endif
#if defined (VKI_SIGLWP)
   if (hostsig == VKI_SIGLWP)
      return TARGET_SIGNAL_LWP;
#endif
#if defined (VKI_SIGDANGER)
   if (hostsig == VKI_SIGDANGER)
      return TARGET_SIGNAL_DANGER;
#endif
#if defined (VKI_SIGGRANT)
   if (hostsig == VKI_SIGGRANT)
      return TARGET_SIGNAL_GRANT;
#endif
#if defined (VKI_SIGRETRACT)
   if (hostsig == VKI_SIGRETRACT)
      return TARGET_SIGNAL_RETRACT;
#endif
#if defined (VKI_SIGMSG)
   if (hostsig == VKI_SIGMSG)
      return TARGET_SIGNAL_MSG;
#endif
#if defined (VKI_SIGSOUND)
   if (hostsig == VKI_SIGSOUND)
      return TARGET_SIGNAL_SOUND;
#endif
#if defined (VKI_SIGSAK)
   if (hostsig == VKI_SIGSAK)
      return TARGET_SIGNAL_SAK;
#endif
#if defined (VKI_SIGPRIO)
   if (hostsig == VKI_SIGPRIO)
      return TARGET_SIGNAL_PRIO;
#endif

   /* Mach exceptions.  Assumes that the values for EXC_ are positive! */
#if defined (EXC_BAD_ACCESS) && defined (_NSIG)
   if (hostsig == _NSIG + EXC_BAD_ACCESS)
      return TARGET_EXC_BAD_ACCESS;
#endif
#if defined (EXC_BAD_INSTRUCTION) && defined (_NSIG)
   if (hostsig == _NSIG + EXC_BAD_INSTRUCTION)
      return TARGET_EXC_BAD_INSTRUCTION;
#endif
#if defined (EXC_ARITHMETIC) && defined (_NSIG)
   if (hostsig == _NSIG + EXC_ARITHMETIC)
      return TARGET_EXC_ARITHMETIC;
#endif
#if defined (EXC_EMULATION) && defined (_NSIG)
   if (hostsig == _NSIG + EXC_EMULATION)
      return TARGET_EXC_EMULATION;
#endif
#if defined (EXC_SOFTWARE) && defined (_NSIG)
   if (hostsig == _NSIG + EXC_SOFTWARE)
      return TARGET_EXC_SOFTWARE;
#endif
#if defined (EXC_BREAKPOINT) && defined (_NSIG)
   if (hostsig == _NSIG + EXC_BREAKPOINT)
      return TARGET_EXC_BREAKPOINT;
#endif

#if defined (VKI_SIGINFO)
   if (hostsig == VKI_SIGINFO)
      return TARGET_SIGNAL_INFO;
#endif
#if defined (VKI_SIGLIBRT)
   if (hostsig == VKI_SIGLIBRT)
      return TARGET_SIGNAL_LIBRT;
#endif
#if defined(VKI_SIGTHR)
   if (hostsig == VKI_SIGTHR)
      return TARGET_SIGNAL_LWP;
#endif

#if defined (VKI_SIGRTMIN)
   if (hostsig >= VKI_SIGRTMIN && hostsig < VKI_SIGRTMAX) {
      /* This block of TARGET_SIGNAL_REALTIME value is in order.  */
      if (33 <= hostsig && hostsig <= 63)
         return (enum target_signal)
            (hostsig - 33 + (int) TARGET_SIGNAL_REALTIME_33);
      else if (hostsig == 32)
         // depending on the platform the first two and the third
         // if branches here may be mutually exclusive, ignore any
         // coverity warnings
         // coverity[DEADCODE:FALSE]
         return TARGET_SIGNAL_REALTIME_32;
      else if (64 <= hostsig && hostsig <= 127)
         return (enum target_signal)
            (hostsig - 64 + (int) TARGET_SIGNAL_REALTIME_64);
   }
#endif

   warning ("Valgrind GDBSERVER bug: (target_signal_from_host):"
            " unrecognized vki signal %d\n", hostsig);
   return TARGET_SIGNAL_UNKNOWN;
}

/* Convert a OURSIG (an enum target_signal) to the form used by the
   target operating system (referred to as the ``host'') or zero if the
   equivalent host signal is not available.  Set/clear OURSIG_OK
   accordingly. */

static
int do_target_signal_to_host (enum target_signal oursig,
                              int *oursig_ok)
{
   *oursig_ok = 1;
   switch (oursig) {
   case TARGET_SIGNAL_0:
      return 0;

#if defined (VKI_SIGHUP)
   case TARGET_SIGNAL_HUP:
      return VKI_SIGHUP;
#endif
#if defined (VKI_SIGINT)
   case TARGET_SIGNAL_INT:
      return VKI_SIGINT;
#endif
#if defined (VKI_SIGQUIT)
   case TARGET_SIGNAL_QUIT:
      return VKI_SIGQUIT;
#endif
#if defined (VKI_SIGILL)
   case TARGET_SIGNAL_ILL:
      return VKI_SIGILL;
#endif
#if defined (VKI_SIGTRAP)
   case TARGET_SIGNAL_TRAP:
      return VKI_SIGTRAP;
#endif
#if defined (VKI_SIGABRT)
   case TARGET_SIGNAL_ABRT:
      return VKI_SIGABRT;
#endif
#if defined (VKI_SIGEMT)
   case TARGET_SIGNAL_EMT:
      return VKI_SIGEMT;
#endif
#if defined (VKI_SIGFPE)
   case TARGET_SIGNAL_FPE:
      return VKI_SIGFPE;
#endif
#if defined (VKI_SIGKILL)
   case TARGET_SIGNAL_KILL:
      return VKI_SIGKILL;
#endif
#if defined (VKI_SIGBUS)
   case TARGET_SIGNAL_BUS:
      return VKI_SIGBUS;
#endif
#if defined (VKI_SIGSEGV)
   case TARGET_SIGNAL_SEGV:
      return VKI_SIGSEGV;
#endif
#if defined (VKI_SIGSYS)
   case TARGET_SIGNAL_SYS:
      return VKI_SIGSYS;
#endif
#if defined (VKI_SIGPIPE)
   case TARGET_SIGNAL_PIPE:
      return VKI_SIGPIPE;
#endif
#if defined (VKI_SIGALRM)
   case TARGET_SIGNAL_ALRM:
      return VKI_SIGALRM;
#endif
#if defined (VKI_SIGTERM)
   case TARGET_SIGNAL_TERM:
      return VKI_SIGTERM;
#endif
#if defined (VKI_SIGUSR1)
   case TARGET_SIGNAL_USR1:
      return VKI_SIGUSR1;
#endif
#if defined (VKI_SIGUSR2)
   case TARGET_SIGNAL_USR2:
      return VKI_SIGUSR2;
#endif
#if defined (VKI_SIGCHLD) || defined (VKI_SIGCLD)
   case TARGET_SIGNAL_CHLD:
#if defined (VKI_SIGCHLD)
      return VKI_SIGCHLD;
#else
      return VKI_SIGCLD;
#endif
#endif /* SIGCLD or SIGCHLD */
#if defined (VKI_SIGPWR)
   case TARGET_SIGNAL_PWR:
      return VKI_SIGPWR;
#endif
#if defined (VKI_SIGWINCH)
   case TARGET_SIGNAL_WINCH:
      return VKI_SIGWINCH;
#endif
#if defined (VKI_SIGURG)
   case TARGET_SIGNAL_URG:
      return VKI_SIGURG;
#endif
#if defined (VKI_SIGIO)
   case TARGET_SIGNAL_IO:
      return VKI_SIGIO;
#endif
#if defined (VKI_SIGPOLL)
   case TARGET_SIGNAL_POLL:
      return VKI_SIGPOLL;
#endif
#if defined (VKI_SIGSTOP)
   case TARGET_SIGNAL_STOP:
      return VKI_SIGSTOP;
#endif
#if defined (VKI_SIGTSTP)
   case TARGET_SIGNAL_TSTP:
      return VKI_SIGTSTP;
#endif
#if defined (VKI_SIGCONT)
   case TARGET_SIGNAL_CONT:
      return VKI_SIGCONT;
#endif
#if defined (VKI_SIGTTIN)
   case TARGET_SIGNAL_TTIN:
      return VKI_SIGTTIN;
#endif
#if defined (VKI_SIGTTOU)
   case TARGET_SIGNAL_TTOU:
      return VKI_SIGTTOU;
#endif
#if defined (VKI_SIGVTALRM)
   case TARGET_SIGNAL_VTALRM:
      return VKI_SIGVTALRM;
#endif
#if defined (VKI_SIGPROF)
   case TARGET_SIGNAL_PROF:
      return VKI_SIGPROF;
#endif
#if defined (VKI_SIGXCPU)
   case TARGET_SIGNAL_XCPU:
      return VKI_SIGXCPU;
#endif
#if defined (VKI_SIGXFSZ)
   case TARGET_SIGNAL_XFSZ:
      return VKI_SIGXFSZ;
#endif
#if defined (VKI_SIGWIND)
   case TARGET_SIGNAL_WIND:
      return VKI_SIGWIND;
#endif
#if defined (VKI_SIGPHONE)
   case TARGET_SIGNAL_PHONE:
      return VKI_SIGPHONE;
#endif
#if defined (VKI_SIGLOST)
   case TARGET_SIGNAL_LOST:
      return VKI_SIGLOST;
#endif
#if defined (VKI_SIGWAITING)
   case TARGET_SIGNAL_WAITING:
      return VKI_SIGWAITING;
#endif
#if defined (VKI_SIGCANCEL)
   case TARGET_SIGNAL_CANCEL:
      return VKI_SIGCANCEL;
#endif
#if defined (VKI_SIGLWP)
   case TARGET_SIGNAL_LWP:
      return VKI_SIGLWP;
#endif
#if defined (VKI_SIGDANGER)
   case TARGET_SIGNAL_DANGER:
      return VKI_SIGDANGER;
#endif
#if defined (VKI_SIGGRANT)
   case TARGET_SIGNAL_GRANT:
      return VKI_SIGGRANT;
#endif
#if defined (VKI_SIGRETRACT)
   case TARGET_SIGNAL_RETRACT:
      return VKI_SIGRETRACT;
#endif
#if defined (VKI_SIGMSG)
   case TARGET_SIGNAL_MSG:
      return VKI_SIGMSG;
#endif
#if defined (VKI_SIGSOUND)
   case TARGET_SIGNAL_SOUND:
      return VKI_SIGSOUND;
#endif
#if defined (VKI_SIGSAK)
   case TARGET_SIGNAL_SAK:
      return VKI_SIGSAK;
#endif
#if defined (VKI_SIGPRIO)
   case TARGET_SIGNAL_PRIO:
      return VKI_SIGPRIO;
#endif

      /* Mach exceptions.  Assumes that the values for EXC_ are positive! */
#if defined (EXC_BAD_ACCESS) && defined (_NSIG)
   case TARGET_EXC_BAD_ACCESS:
      return _NSIG + EXC_BAD_ACCESS;
#endif
#if defined (EXC_BAD_INSTRUCTION) && defined (_NSIG)
   case TARGET_EXC_BAD_INSTRUCTION:
      return _NSIG + EXC_BAD_INSTRUCTION;
#endif
#if defined (EXC_ARITHMETIC) && defined (_NSIG)
   case TARGET_EXC_ARITHMETIC:
      return _NSIG + EXC_ARITHMETIC;
#endif
#if defined (EXC_EMULATION) && defined (_NSIG)
   case TARGET_EXC_EMULATION:
      return _NSIG + EXC_EMULATION;
#endif
#if defined (EXC_SOFTWARE) && defined (_NSIG)
   case TARGET_EXC_SOFTWARE:
      return _NSIG + EXC_SOFTWARE;
#endif
#if defined (EXC_BREAKPOINT) && defined (_NSIG)
   case TARGET_EXC_BREAKPOINT:
      return _NSIG + EXC_BREAKPOINT;
#endif

#if defined (VKI_SIGINFO)
   case TARGET_SIGNAL_INFO:
      return VKI_SIGINFO;
#endif
#if defined (SIGLIBRT)
   case TARGET_SIGNAL_LIBRT:
      return SIGLIBRT;
#endif
#if defined (VKI_SIGTHR)
   case TARGET_SIGNAL_LWP:
      return VKI_SIGTHR;
#endif

   default:
#if defined (VKI_SIGRTMIN)
      {
         int retsig = 0;

         if (oursig >= TARGET_SIGNAL_REALTIME_33
             && oursig <= TARGET_SIGNAL_REALTIME_63) {
            /* This block of signals is continuous, and
               TARGET_SIGNAL_REALTIME_33 is 33 by definition.  */
            retsig = (int) oursig - (int) TARGET_SIGNAL_REALTIME_33 + 33;
         } else if (oursig == TARGET_SIGNAL_REALTIME_32) {
            /* TARGET_SIGNAL_REALTIME_32 isn't contiguous with
               TARGET_SIGNAL_REALTIME_33.  It is 32 by definition.  */
            retsig = 32;
         } else if (oursig >= TARGET_SIGNAL_REALTIME_64
                    && oursig <= TARGET_SIGNAL_REALTIME_127) {
            /* This block of signals is continuous, and
               TARGET_SIGNAL_REALTIME_64 is 64 by definition.  */
            retsig = (int) oursig - (int) TARGET_SIGNAL_REALTIME_64 + 64;
         }
         
         if (retsig >= VKI_SIGRTMIN && retsig < VKI_SIGRTMAX)
            return retsig;
      }
#endif
      warning ("Valgrind GDBSERVER bug: (do_target_signal_to_host):"
               " unrecognized target signal %u\n", oursig);
      *oursig_ok = 0;
      return 0;
   }
}

int target_signal_to_host_p (enum target_signal oursig)
{
   int oursig_ok;
   do_target_signal_to_host (oursig, &oursig_ok);
   return oursig_ok;
}

int target_signal_to_host (enum target_signal oursig)
{
   int oursig_ok;
   int targ_signo = do_target_signal_to_host (oursig, &oursig_ok);
   if (!oursig_ok) {
      /* The user might be trying to do "signal SIGSAK" where this system
         doesn't have SIGSAK.  */
      warning ("Signal %s does not exist on this system.\n",
               target_signal_to_name (oursig));
      return 0;
   } else {
      return targ_signo;
   }
}

/* In some circumstances we allow a command to specify a numeric
   signal.  The idea is to keep these circumstances limited so that
   users (and scripts) develop portable habits.  For comparison,
   POSIX.2 `kill' requires that 1,2,3,6,9,14, and 15 work (and using a
   numeric signal at all is obsolescent.  We are slightly more
   lenient and allow 1-15 which should match host signal numbers on
   most systems.  Use of symbolic signal names is strongly encouraged.  */

enum target_signal target_signal_from_command (int num)
{
   if (num >= 1 && num <= 15)
      return (enum target_signal) num;
   error ("Only signals 1-15 are valid as numeric signals.\n\
Use \"info signals\" for a list of symbolic signals.\n");
}
