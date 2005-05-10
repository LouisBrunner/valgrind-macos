
/*--------------------------------------------------------------------*/
/*--- Reimplementation of some C library stuff, to avoid depending ---*/
/*--- on libc.so.                                                  ---*/
/*---                                                  vg_mylibc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
      jseward@acm.org

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

#include "core.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuglog.h"    /* VG_(debugLog_vprintf) */
#include "pub_core_stacktrace.h"
#include "pub_core_syscalls.h"
#include "pub_core_tooliface.h"


/* ---------------------------------------------------------------------
   Wrappers around system calls, and other stuff, to do with signals.
   ------------------------------------------------------------------ */

/* sigemptyset, sigfullset, sigaddset and sigdelset return 0 on
   success and -1 on error.  
*/
Int VG_(sigfillset)( vki_sigset_t* set )
{
   Int i;
   if (set == NULL)
      return -1;
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      set->sig[i] = ~(UWord)0x0;
   return 0;
}

Int VG_(sigemptyset)( vki_sigset_t* set )
{
   Int i;
   if (set == NULL)
      return -1;
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      set->sig[i] = 0x0;
   return 0;
}

Bool VG_(isemptysigset)( const vki_sigset_t* set )
{
   Int i;
   vg_assert(set != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      if (set->sig[i] != 0x0) return False;
   return True;
}

Bool VG_(isfullsigset)( const vki_sigset_t* set )
{
   Int i;
   vg_assert(set != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      if (set->sig[i] != ~(UWord)0x0) return False;
   return True;
}

Bool VG_(iseqsigset)( const vki_sigset_t* set1, const vki_sigset_t* set2 )
{
   Int i;
   vg_assert(set1 != NULL && set2 != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      if (set1->sig[i] != set2->sig[i]) return False;
   return True;
}


Int VG_(sigaddset)( vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return -1;
   if (signum < 1 || signum > _VKI_NSIG)
      return -1;
   signum--;
   set->sig[signum / _VKI_NSIG_BPW] |= (1UL << (signum % _VKI_NSIG_BPW));
   return 0;
}

Int VG_(sigdelset)( vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return -1;
   if (signum < 1 || signum > _VKI_NSIG)
      return -1;
   signum--;
   set->sig[signum / _VKI_NSIG_BPW] &= ~(1UL << (signum % _VKI_NSIG_BPW));
   return 0;
}

Int VG_(sigismember) ( const vki_sigset_t* set, Int signum )
{
   if (set == NULL)
      return 0;
   if (signum < 1 || signum > _VKI_NSIG)
      return 0;
   signum--;
   if (1 & ((set->sig[signum / _VKI_NSIG_BPW]) >> (signum % _VKI_NSIG_BPW)))
      return 1;
   else
      return 0;
}


/* Add all signals in src to dst. */
void VG_(sigaddset_from_set)( vki_sigset_t* dst, vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] |= src->sig[i];
}

/* Remove all signals in src from dst. */
void VG_(sigdelset_from_set)( vki_sigset_t* dst, vki_sigset_t* src )
{
   Int i;
   vg_assert(dst != NULL && src != NULL);
   for (i = 0; i < _VKI_NSIG_WORDS; i++)
      dst->sig[i] &= ~(src->sig[i]);
}


/* The functions sigaction, sigprocmask, sigpending and sigsuspend
   return 0 on success and -1 on error.  
*/
Int VG_(sigprocmask)( Int how, const vki_sigset_t* set, vki_sigset_t* oldset)
{
   Int res = VG_(do_syscall4)(__NR_rt_sigprocmask, 
                              how, (UWord)set, (UWord)oldset, 
                              _VKI_NSIG_WORDS * sizeof(UWord));
   return VG_(is_kerror)(res) ? -1 : 0;
}


Int VG_(sigaction) ( Int signum, const struct vki_sigaction* act,  
                     struct vki_sigaction* oldact)
{
   Int res = VG_(do_syscall4)(__NR_rt_sigaction,
		              signum, (UWord)act, (UWord)oldact, 
		              _VKI_NSIG_WORDS * sizeof(UWord));
   /* VG_(printf)("res = %d\n",res); */
   return VG_(is_kerror)(res) ? -1 : 0;
}


Int VG_(sigaltstack)( const vki_stack_t* ss, vki_stack_t* oss )
{
   Int res = VG_(do_syscall2)(__NR_sigaltstack, (UWord)ss, (UWord)oss);
   return VG_(is_kerror)(res) ? -1 : 0;
}

Int VG_(sigtimedwait)( const vki_sigset_t *set, vki_siginfo_t *info, 
                       const struct vki_timespec *timeout )
{
   Int res = VG_(do_syscall4)(__NR_rt_sigtimedwait, (UWord)set, (UWord)info, 
                              (UWord)timeout, sizeof(*set));

   return res;
}
 
Int VG_(signal)(Int signum, void (*sighandler)(Int))
{
   Int res;
   struct vki_sigaction sa;
   sa.ksa_handler = sighandler;
   sa.sa_flags = VKI_SA_ONSTACK | VKI_SA_RESTART;
   sa.sa_restorer = NULL;
   res = VG_(sigemptyset)( &sa.sa_mask );
   vg_assert(res == 0);
   res = VG_(do_syscall4)(__NR_rt_sigaction, signum, (UWord)&sa, (UWord)NULL,
			 _VKI_NSIG_WORDS * sizeof(UWord));
   return VG_(is_kerror)(res) ? -1 : 0;
}


Int VG_(kill)( Int pid, Int signo )
{
   Int res = VG_(do_syscall2)(__NR_kill, pid, signo);
   return VG_(is_kerror)(res) ? -1 : 0;
}


Int VG_(tkill)( ThreadId tid, Int signo )
{
   Int ret = -VKI_ENOSYS;

#if 0
   /* This isn't right because the client may create a process
      structure with multiple thread groups */
   ret = VG_(do_syscall)(__NR_tgkill, VG_(getpid)(), tid, signo);
#endif

   ret = VG_(do_syscall2)(__NR_tkill, tid, signo);

   if (ret == -VKI_ENOSYS)
      ret = VG_(do_syscall2)(__NR_kill, tid, signo);

   return VG_(is_kerror)(ret) ? -1 : 0;
}

Int VG_(sigpending) ( vki_sigset_t* set )
{
// Nb: AMD64/Linux doesn't have __NR_sigpending;  it only provides
// __NR_rt_sigpending.  This function will have to be abstracted in some
// way to account for this.  In the meantime, the easy option is to forget
// about it for AMD64 until it's needed.
#ifdef __amd64__
   I_die_here;
#else
   Int res = VG_(do_syscall1)(__NR_sigpending, (UWord)set);
   return VG_(is_kerror)(res) ? -1 : 0;
#endif
}

Int VG_(waitpid)(Int pid, Int *status, Int options)
{
   Int ret = VG_(do_syscall4)(__NR_wait4, pid, (UWord)status, options, 0);

   return VG_(is_kerror)(ret) ? -1 : ret;
}

Int VG_(gettid)(void)
{
   Int ret;

   ret = VG_(do_syscall0)(__NR_gettid);

   if (ret == -VKI_ENOSYS) {
      Char pid[16];
      
      /*
       * The gettid system call does not exist. The obvious assumption
       * to make at this point would be that we are running on an older
       * system where the getpid system call actually returns the ID of
       * the current thread.
       *
       * Unfortunately it seems that there are some systems with a kernel
       * where getpid has been changed to return the ID of the thread group
       * leader but where the gettid system call has not yet been added.
       *
       * So instead of calling getpid here we use readlink to see where
       * the /proc/self link is pointing...
       */
      if ((ret = VG_(do_syscall3)(__NR_readlink, (UWord)"/proc/self",
                                  (UWord)pid, sizeof(pid))) >= 0) 
      {
         pid[ret] = '\0';
         ret = VG_(atoll)(pid);
      }
   }

   return ret;
}



/* ---------------------------------------------------------------------
   mmap/munmap, exit, fcntl
   ------------------------------------------------------------------ */

static Int munmap_inner(void *start, SizeT length)
{
   return VG_(do_syscall2)(__NR_munmap, (UWord)start, length );
}

static Addr mmap_inner(void *start, SizeT length, UInt prot, UInt flags,
                       UInt fd, OffT offset)
{
   Word ret;
   
   VGP_DO_MMAP(ret, start, length, prot,
               flags & ~(VKI_MAP_NOSYMS|VKI_MAP_CLIENT), fd, offset);
   return ret;
}

/* Returns -1 on failure. */
void* VG_(mmap)( void* start, SizeT length,
                 UInt prot, UInt flags, UInt sf_flags, UInt fd, OffT offset)
{
   Addr  res;

   if (!(flags & VKI_MAP_FIXED)) {
      start = (void *)VG_(find_map_space)((Addr)start, length, !!(flags & VKI_MAP_CLIENT));

      flags |= VKI_MAP_FIXED;
   }
   if (start == 0)
      return (void *)-1;

   res = mmap_inner(start, length, prot, flags, fd, offset);

   // Check it ended up in the right place.
   if (!VG_(is_kerror)(res)) {
      if (flags & VKI_MAP_CLIENT) {
         vg_assert(VG_(client_base) <= res && res+length <= VG_(client_end));
      } else {
         vg_assert(VG_(valgrind_base) <= res && res+length-1 <= VG_(valgrind_last));
      }

      sf_flags |= SF_MMAP;
      if (  flags & VKI_MAP_FIXED)      sf_flags |= SF_FIXED;
      if (  flags & VKI_MAP_SHARED)     sf_flags |= SF_SHARED;
      if (!(flags & VKI_MAP_ANONYMOUS)) sf_flags |= SF_FILE;
      if (!(flags & VKI_MAP_CLIENT))    sf_flags |= SF_VALGRIND;
      if (  flags & VKI_MAP_NOSYMS)     sf_flags |= SF_NOSYMS;

      VG_(map_fd_segment)(res, length, prot, sf_flags, fd, offset, NULL);
   }

   return VG_(is_kerror)(res) ? ((void*)(-1)) : (void*)res;
}

/* Returns -1 on failure. */
Int VG_(munmap)( void* start, SizeT length )
{
   Int res = munmap_inner(start, length);
   if (!VG_(is_kerror)(res))
      VG_(unmap_range)((Addr)start, length);
   return VG_(is_kerror)(res) ? -1 : 0;
}

Int VG_(mprotect)( void *start, SizeT length, UInt prot )
{
   Int res = VG_(do_syscall3)(__NR_mprotect, (UWord)start, length, prot );
   if (!VG_(is_kerror)(res))
      VG_(mprotect_range)((Addr)start, length, prot);
   return VG_(is_kerror)(res) ? -1 : 0;
}
Int VG_(mprotect_native)( void *start, SizeT length, UInt prot )
{
   Int res = VG_(do_syscall3)(__NR_mprotect, (UWord)start, length, prot );
   return VG_(is_kerror)(res) ? -1 : 0;
}

/* Pull down the entire world */
void VG_(exit)( Int status )
{
   (void)VG_(do_syscall1)(__NR_exit_group, status );
   (void)VG_(do_syscall1)(__NR_exit, status );
   /* Why are we still alive here? */
   /*NOTREACHED*/
   *(volatile Int *)0 = 'x';
   vg_assert(2+2 == 5);
}

/* Returns -1 on error. */
Int VG_(fcntl) ( Int fd, Int cmd, Int arg )
{
   Int res = VG_(do_syscall3)(__NR_fcntl, fd, cmd, arg);
   return VG_(is_kerror)(res) ? -1 : res;
}

Int VG_(poll)( struct vki_pollfd *ufds, UInt nfds, Int timeout)
{
   Int res = VG_(do_syscall3)(__NR_poll, (UWord)ufds, nfds, timeout);

   return res;
}


/* A general replacement for printf().  Note that only low-level 
   debugging info should be sent via here.  The official route is to
   to use vg_message().  This interface is deprecated.
*/
typedef struct {
   char buf[100];
   int n;
} printf_buf;

static void add_to_myprintf_buf ( HChar c, void *p )
{
   printf_buf *myprintf_buf = (printf_buf *)p;
   
   if (myprintf_buf->n >= 100-10 /*paranoia*/ ) {
      if (VG_(clo_log_fd) >= 0) {
         VG_(send_bytes_to_logging_sink)( 
            myprintf_buf->buf, VG_(strlen)(myprintf_buf->buf) );
      }
      myprintf_buf->n = 0;
      myprintf_buf->buf[myprintf_buf->n] = 0;      
   }
   myprintf_buf->buf[myprintf_buf->n++] = c;
   myprintf_buf->buf[myprintf_buf->n] = 0;
}

UInt VG_(printf) ( const char *format, ... )
{
   UInt ret;
   va_list vargs;
   printf_buf myprintf_buf = {"",0};
   va_start(vargs,format);
   
   ret = VG_(debugLog_vprintf) 
            ( add_to_myprintf_buf, &myprintf_buf, format, vargs );

   if (myprintf_buf.n > 0 && VG_(clo_log_fd) >= 0) {
      VG_(send_bytes_to_logging_sink)( myprintf_buf.buf, myprintf_buf.n );
   }

   va_end(vargs);

   return ret;
}

/* A general replacement for sprintf(). */
static void add_to_vg_sprintf_buf ( HChar c, void *p )
{
   char **vg_sprintf_ptr = p;
   *(*vg_sprintf_ptr)++ = c;
}

UInt VG_(sprintf) ( Char* buf, Char *format, ... )
{
   Int ret;
   va_list vargs;
   Char *vg_sprintf_ptr = buf;

   va_start(vargs,format);

   ret = VG_(debugLog_vprintf) 
            ( add_to_vg_sprintf_buf, &vg_sprintf_ptr, format, vargs );
   add_to_vg_sprintf_buf(0,&vg_sprintf_ptr);

   va_end(vargs);

   vg_assert(VG_(strlen)(buf) == ret);

   return ret;
}


/* ---------------------------------------------------------------------
   Misc str* functions.
   ------------------------------------------------------------------ */

Bool VG_(isspace) ( Char c )
{
   return (c == ' '  || c == '\n' || c == '\t' || 
           c == '\f' || c == '\v' || c == '\r');
}

Bool VG_(isdigit) ( Char c )
{
   return (c >= '0' && c <= '9');
}

Int VG_(strlen) ( const Char* str )
{
   Int i = 0;
   while (str[i] != 0) i++;
   return i;
}


Long VG_(atoll) ( Char* str )
{
   Bool neg = False;
   Long n = 0;
   if (*str == '-') { str++; neg = True; };
   while (*str >= '0' && *str <= '9') {
      n = 10*n + (Long)(*str - '0');
      str++;
   }
   if (neg) n = -n;
   return n;
}


Long VG_(atoll16) ( Char* str )
{
   Bool neg = False;
   Long n = 0;
   if (*str == '-') { str++; neg = True; };
   while (True) {
      if (*str >= '0' && *str <= '9') {
         n = 16*n + (Long)(*str - '0');
      }
      else 
      if (*str >= 'A' && *str <= 'F') {
         n = 16*n + (Long)((*str - 'A') + 10);
      }
      else 
      if (*str >= 'a' && *str <= 'f') {
         n = 16*n + (Long)((*str - 'a') + 10);
      }
      else {
	break;
      }
      str++;
   }
   if (neg) n = -n;
   return n;
}

Long VG_(atoll36) ( UInt base, Char* str )
{
   Bool neg = False;
   Long n = 0;
   vg_assert(base >= 2 && base <= 36);
   if (*str == '-') { str++; neg = True; };
   while (True) {
      if (*str >= '0' 
          && *str <= (Char)('9' - (10 - base))) {
         n = base*n + (Long)(*str - '0');
      }
      else 
      if (base > 10 && *str >= 'A' 
          && *str <= (Char)('Z' - (36 - base))) {
         n = base*n + (Long)((*str - 'A') + 10);
      }
      else 
      if (base > 10 && *str >= 'a' 
          && *str <= (Char)('z' - (36 - base))) {
         n = base*n + (Long)((*str - 'a') + 10);
      }
      else {
	break;
      }
      str++;
   }
   if (neg) n = -n;
   return n;
}


Char* VG_(strcat) ( Char* dest, const Char* src )
{
   Char* dest_orig = dest;
   while (*dest) dest++;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}


Char* VG_(strncat) ( Char* dest, const Char* src, Int n )
{
   Char* dest_orig = dest;
   while (*dest) dest++;
   while (*src && n > 0) { *dest++ = *src++; n--; }
   *dest = 0;
   return dest_orig;
}


Char* VG_(strpbrk) ( const Char* s, const Char* accept )
{
   const Char* a;
   while (*s) {
      a = accept;
      while (*a)
         if (*a++ == *s)
            return (Char *) s;
      s++;
   }
   return NULL;
}


Char* VG_(strcpy) ( Char* dest, const Char* src )
{
   Char* dest_orig = dest;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}


/* Copy bytes, not overrunning the end of dest and always ensuring
   zero termination. */
void VG_(strncpy_safely) ( Char* dest, const Char* src, Int ndest )
{
   Int i;
   vg_assert(ndest > 0);
   i = 0;
   while (True) {
      dest[i] = 0;
      if (src[i] == 0) return;
      if (i >= ndest-1) return;
      dest[i] = src[i];
      i++;
   }
}


Char* VG_(strncpy) ( Char* dest, const Char* src, Int ndest )
{
   Int i = 0;
   while (True) {
      if (i >= ndest) return dest;     /* reached limit */
      dest[i] = src[i];
      if (src[i++] == 0) {
         /* reached NUL;  pad rest with zeroes as required */
         while (i < ndest) dest[i++] = 0;
         return dest;
      }
   }
}


Int VG_(strcmp) ( const Char* s1, const Char* s2 )
{
   while (True) {
      if (*s1 == 0 && *s2 == 0) return 0;
      if (*s1 == 0) return -1;
      if (*s2 == 0) return 1;

      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;

      s1++; s2++;
   }
}

static Bool isterm ( Char c )
{
   return ( VG_(isspace)(c) || 0 == c );
}


Int VG_(strcmp_ws) ( const Char* s1, const Char* s2 )
{
   while (True) {
      if (isterm(*s1) && isterm(*s2)) return 0;
      if (isterm(*s1)) return -1;
      if (isterm(*s2)) return 1;

      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;

      s1++; s2++;
   }
}


Int VG_(strncmp) ( const Char* s1, const Char* s2, Int nmax )
{
   Int n = 0;
   while (True) {
      if (n >= nmax) return 0;
      if (*s1 == 0 && *s2 == 0) return 0;
      if (*s1 == 0) return -1;
      if (*s2 == 0) return 1;

      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;

      s1++; s2++; n++;
   }
}


Int VG_(strncmp_ws) ( const Char* s1, const Char* s2, Int nmax )
{
   Int n = 0;
   while (True) {
      if (n >= nmax) return 0;
      if (isterm(*s1) && isterm(*s2)) return 0;
      if (isterm(*s1)) return -1;
      if (isterm(*s2)) return 1;

      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;

      s1++; s2++; n++;
   }
}


Char* VG_(strstr) ( const Char* haystack, Char* needle )
{
   Int n; 
   if (haystack == NULL)
      return NULL;
   n = VG_(strlen)(needle);
   while (True) {
      if (haystack[0] == 0) 
         return NULL;
      if (VG_(strncmp)(haystack, needle, n) == 0) 
         return (Char*)haystack;
      haystack++;
   }
}


Char* VG_(strchr) ( const Char* s, Char c )
{
   while (True) {
      if (*s == c) return (Char*)s;
      if (*s == 0) return NULL;
      s++;
   }
}


Char* VG_(strrchr) ( const Char* s, Char c )
{
   Int n = VG_(strlen)(s);
   while (--n > 0) {
      if (s[n] == c) return (Char*)s + n;
   }
   return NULL;
}


void* VG_(memcpy) ( void *dest, const void *src, Int sz )
{
   const Char *s = (const Char *)src;
   Char *d = (Char *)dest;
   vg_assert(sz >= 0);

   while (sz--)
      *d++ = *s++;

   return dest;
}


void* VG_(memset) ( void *dest, Int c, Int sz )
{
   Char *d = (Char *)dest;
   vg_assert(sz >= 0);

   while (sz--)
      *d++ = c;

   return dest;
}

Int VG_(memcmp) ( const void* s1, const void* s2, Int n )
{
   Int res;
   UChar a0;
   UChar b0;
   vg_assert(n >= 0);

   while (n != 0) {
      a0 = ((UChar *) s1)[0];
      b0 = ((UChar *) s2)[0];
      s1 += 1;
      s2 += 1;
      res = a0 - b0;
      if (res != 0)
         return res;
      n -= 1;
   }
   return 0;
}

Char VG_(toupper) ( Char c )
{
   if (c >= 'a' && c <= 'z')
      return c + ('A' - 'a'); 
   else
      return c;
}


/* Inline just for the wrapper VG_(strdup) below */
__inline__ Char* VG_(arena_strdup) ( ArenaId aid, const Char* s )
{
   Int   i;
   Int   len;
   Char* res;

   if (s == NULL)
      return NULL;

   len = VG_(strlen)(s) + 1;
   res = VG_(arena_malloc) (aid, len);

   for (i = 0; i < len; i++)
      res[i] = s[i];
   return res;
}

/* Wrapper to avoid exposing tools to ArenaId's */
Char* VG_(strdup) ( const Char* s )
{
   return VG_(arena_strdup) ( VG_AR_TOOL, s ); 
}

/* ---------------------------------------------------------------------
   A simple string matching routine, purloined from Hugs98.
      `*'    matches any sequence of zero or more characters
      `?'    matches any single character exactly 
      `\c'   matches the character c only (ignoring special chars)
      c      matches the character c only
   ------------------------------------------------------------------ */

/* Keep track of recursion depth. */
static Int recDepth;

static Bool string_match_wrk ( const Char* pat, const Char* str )
{
   vg_assert(recDepth >= 0 && recDepth < 500);
   recDepth++;
   for (;;) {
      switch (*pat) {
         case '\0' : recDepth--;
                     return (*str=='\0');
         case '*'  : do {
                        if (string_match_wrk(pat+1,str)) {
                           recDepth--;
                           return True;
                        }
                     } while (*str++);
                     recDepth--;
                     return False;
         case '?'  : if (*str++=='\0') {
                        recDepth--;
                        return False;
                     }
                     pat++;
                     break;
         case '\\' : if (*++pat == '\0') {
                        recDepth--;
                        return False; /* spurious trailing \ in pattern */
                     }
                     /* falls through to ... */
         default   : if (*pat++ != *str++) {
                        recDepth--;
                        return False;
                     }
                     break;
      }
   }
}

Bool VG_(string_match) ( const Char* pat, const Char* str )
{
   Bool b;
   recDepth = 0;
   b = string_match_wrk ( pat, str );
   vg_assert(recDepth == 0);
   /*
   VG_(printf)("%s   %s   %s\n",
	       b?"TRUE ":"FALSE", pat, str);
   */
   return b;
}


/* ---------------------------------------------------------------------
   Assertery.
   ------------------------------------------------------------------ */

/* Fake up an ExeContext which is of our actual real CPU state, so we
   can print a stack trace.  This isn't terribly useful in the case
   where we were killed by a signal, since we just get a backtrace
   into the signal handler.  Also, it could be somewhat risky if we
   actully got the panic/exception within the execontext/stack
   dump/symtab code.  But it's better than nothing. */
static inline void get_and_pp_real_StackTrace(Addr ret)
{
   Addr ips[VG_DEEPEST_BACKTRACE];
   Addr sp, fp;
   Addr stacktop;
   ThreadId tid = VG_(get_lwp_tid)(VG_(gettid)());
   ThreadState *tst = VG_(get_ThreadState)(tid);

   VGA_GET_REAL_STACK_PTR(sp);
   VGA_GET_REAL_FRAME_PTR(fp);

   stacktop = tst->os_state.valgrind_stack_base + 
              tst->os_state.valgrind_stack_szB;

   VG_(get_StackTrace2)(ips, VG_(clo_backtrace_size),
                        ret, sp, fp, sp, stacktop);
   VG_(pp_StackTrace)  (ips, VG_(clo_backtrace_size));
}

__attribute__ ((noreturn))
static void report_and_quit ( const Char* report, StackTrace ips )
{
   if (ips == NULL)
      get_and_pp_real_StackTrace((Addr)__builtin_return_address(0));
   else
      VG_(pp_StackTrace)(ips, VG_(clo_backtrace_size));
   
   VG_(printf)("\nBasic block ctr is approximately %llu\n", VG_(bbs_done) );

   VG_(pp_sched_status)();
   VG_(printf)("\n");
   VG_(printf)("Note: see also the FAQ.txt in the source distribution.\n");
   VG_(printf)("It contains workarounds to several common problems.\n");
   VG_(printf)("\n");
   VG_(printf)("If that doesn't help, please report this bug to: %s\n\n", 
               report);
   VG_(printf)("In the bug report, send all the above text, the valgrind\n");
   VG_(printf)("version, and what Linux distro you are using.  Thanks.\n\n");
   VG_(exit)(1);
}

void VG_(assert_fail) ( Bool isCore, const Char* expr, const Char* file, 
                        Int line, const Char* fn, const Char* format, ... )
{
   va_list vargs;
   Char buf[256];
   Char* bufptr = buf;
   Char* component;
   Char* bugs_to;

   static Bool entered = False;
   if (entered) 
     VG_(exit)(2);
   entered = True;

   va_start(vargs,format);
   VG_(debugLog_vprintf) ( add_to_vg_sprintf_buf, &bufptr, format, vargs );
   add_to_vg_sprintf_buf('\0', &bufptr);
   va_end(vargs);

   if (isCore) {
      component = "valgrind";
      bugs_to   = VG_BUGS_TO;
   } else { 
      component = VG_(details).name;
      bugs_to   = VG_(details).bug_reports_to;
   }

   // Treat vg_assert2(0, "foo") specially, as a panicky abort
   if (VG_STREQ(expr, "0")) {
      VG_(printf)("\n%s: %s:%d (%s): the `impossible' happened.\n",
                  component, file, line, fn, expr );
   } else {
      VG_(printf)("\n%s: %s:%d (%s): Assertion `%s' failed.\n",
                  component, file, line, fn, expr );
   }
   if (!VG_STREQ(buf, ""))
      VG_(printf)("%s: %s\n", component, buf );

   report_and_quit(bugs_to, NULL);
}

__attribute__ ((noreturn))
static void panic ( Char* name, Char* report, Char* str, StackTrace ips )
{
   VG_(printf)("\n%s: the `impossible' happened:\n   %s\n", name, str);
   report_and_quit(report, ips);
}

void VG_(core_panic) ( Char* str )
{
   panic("valgrind", VG_BUGS_TO, str, NULL);
}

void VG_(core_panic_at) ( Char* str, StackTrace ips )
{
   panic("valgrind", VG_BUGS_TO, str, ips);
}

void VG_(tool_panic) ( Char* str )
{
   panic(VG_(details).name, VG_(details).bug_reports_to, str, NULL);
}


/* ---------------------------------------------------------------------
   Primitive support for reading files.
   ------------------------------------------------------------------ */

static inline Bool fd_exists(Int fd)
{
   struct vki_stat st;

   return VG_(fstat)(fd, &st) == 0;
}

/* Move an fd into the Valgrind-safe range */
Int VG_(safe_fd)(Int oldfd)
{
   Int newfd;

   vg_assert(VG_(fd_hard_limit) != -1);

   newfd = VG_(fcntl)(oldfd, VKI_F_DUPFD, VG_(fd_hard_limit));
   if (newfd != -1)
      VG_(close)(oldfd);

   VG_(fcntl)(newfd, VKI_F_SETFD, VKI_FD_CLOEXEC);

   vg_assert(newfd >= VG_(fd_hard_limit));
   return newfd;
}



/* Returns -1 on failure. */
Int VG_(open) ( const Char* pathname, Int flags, Int mode )
{  
   Int fd = VG_(do_syscall3)(__NR_open, (UWord)pathname, flags, mode);
   return fd;
}

Int VG_(pipe) ( Int fd[2] )
{
   Int ret = VG_(do_syscall1)(__NR_pipe, (UWord)fd);
   return VG_(is_kerror)(ret) ? -1 : 0;
}

void VG_(close) ( Int fd )
{
   VG_(do_syscall1)(__NR_close, fd);
}


Int VG_(read) ( Int fd, void* buf, Int count)
{
   Int res;
   res = VG_(do_syscall3)(__NR_read, fd, (UWord)buf, count);
   return res;
}

Int VG_(write) ( Int fd, const void* buf, Int count)
{
   Int res;
   res = VG_(do_syscall3)(__NR_write, fd, (UWord)buf, count);
   return res;
}

OffT VG_(lseek) ( Int fd, OffT offset, Int whence)
{
   Int res;
   res = VG_(do_syscall3)(__NR_lseek, fd, offset, whence);
   if (VG_(is_kerror)(res)) res = -1;
   return res;
}

Int VG_(stat) ( Char* file_name, struct vki_stat* buf )
{
   Int res;
   res = VG_(do_syscall2)(__NR_stat, (UWord)file_name, (UWord)buf);
   return res;			/* return -ve error */
}

Int VG_(fstat) ( Int fd, struct vki_stat* buf )
{
   Int res;
   res = VG_(do_syscall2)(__NR_fstat, fd, (UWord)buf);
   return VG_(is_kerror)(res) ? (-1) : 0;
}

Int VG_(dup2) ( Int oldfd, Int newfd )
{
   Int res;
   res = VG_(do_syscall2)(__NR_dup2, oldfd, newfd);
   return VG_(is_kerror)(res) ? (-1) : res;
}

Int VG_(rename) ( Char* old_name, Char* new_name )
{
   Int res;
   res = VG_(do_syscall2)(__NR_rename, (UWord)old_name, (UWord)new_name);
   return VG_(is_kerror)(res) ? (-1) : 0;
}

Int VG_(unlink) ( Char* file_name )
{
   Int res;
   res = VG_(do_syscall1)(__NR_unlink, (UWord)file_name);
   return VG_(is_kerror)(res) ? (-1) : 0;
}

/* Nb: we do not allow the Linux extension which malloc()s memory for the
   buffer if buf==NULL, because we don't want Linux calling malloc() */
Char* VG_(getcwd) ( Char* buf, SizeT size )
{
   Word res;
   vg_assert(buf != NULL);
   res = VG_(do_syscall2)(__NR_getcwd, (UWord)buf, size);
   return VG_(is_kerror)(res) ? ((Char*)NULL) : (Char*)res;
}

/* Alternative version that does allocate the memory.  Easier to use. */
Bool VG_(getcwd_alloc) ( Char** out )
{
   SizeT size = 4;

   *out = NULL;
   while (True) {
      *out = VG_(malloc)(size);
      if (NULL == VG_(getcwd)(*out, size)) {
         VG_(free)(*out);
         if (size > 65535)
            return False;
         size *= 2;
      } else {
         return True;
      }
   }
}


/* ---------------------------------------------------------------------
   Misc functions looking for a proper home.
   ------------------------------------------------------------------ */

/* clone the environment */
static Char **env_clone ( Char **oldenv )
{
   Char **oldenvp;
   Char **newenvp;
   Char **newenv;
   Int  envlen;

   for (oldenvp = oldenv; oldenvp && *oldenvp; oldenvp++);

   envlen = oldenvp - oldenv + 1;
   
   newenv = VG_(arena_malloc)(VG_AR_CORE, envlen * sizeof(Char **));

   oldenvp = oldenv;
   newenvp = newenv;
   
   while (oldenvp && *oldenvp) {
      *newenvp++ = *oldenvp++;
   }
   
   *newenvp = *oldenvp;

   return newenv;
}

void  VG_(env_unsetenv) ( Char **env, const Char *varname )
{
   Char **from;
   Char **to = NULL;
   Int len = VG_(strlen)(varname);

   for(from = to = env; from && *from; from++) {
      if (!(VG_(strncmp)(varname, *from, len) == 0 && (*from)[len] == '=')) {
	 *to = *from;
	 to++;
      }
   }
   *to = *from;
}

/* set the environment; returns the old env if a new one was allocated */
Char **VG_(env_setenv) ( Char ***envp, const Char* varname, const Char *val )
{
   Char **env = (*envp);
   Char **cpp;
   Int len = VG_(strlen)(varname);
   Char *valstr = VG_(arena_malloc)(VG_AR_CORE, len + VG_(strlen)(val) + 2);
   Char **oldenv = NULL;

   VG_(sprintf)(valstr, "%s=%s", varname, val);

   for(cpp = env; cpp && *cpp; cpp++) {
      if (VG_(strncmp)(varname, *cpp, len) == 0 && (*cpp)[len] == '=') {
	 *cpp = valstr;
	 return oldenv;
      }
   }

   if (env == NULL) {
      env = VG_(arena_malloc)(VG_AR_CORE, sizeof(Char **) * 2);
      env[0] = valstr;
      env[1] = NULL;

      *envp = env;

   }  else {
      Int envlen = (cpp-env) + 2;
      Char **newenv = VG_(arena_malloc)(VG_AR_CORE, envlen * sizeof(Char **));

      for(cpp = newenv; *env; )
	 *cpp++ = *env++;
      *cpp++ = valstr;
      *cpp++ = NULL;

      oldenv = *envp;

      *envp = newenv;
   }

   return oldenv;
}

/* We do getenv without libc's help by snooping around in
   VG_(client_envp) as determined at startup time. */
Char *VG_(getenv)(Char *varname)
{
   Int i, n;
   n = VG_(strlen)(varname);
   for (i = 0; VG_(client_envp)[i] != NULL; i++) {
      Char* s = VG_(client_envp)[i];
      if (VG_(strncmp)(varname, s, n) == 0 && s[n] == '=') {
         return & s[n+1];
      }
   }
   return NULL;
}

/* Support for getrlimit. */
Int VG_(getrlimit) (Int resource, struct vki_rlimit *rlim)
{
   Int res = -VKI_ENOSYS;
   /* res = getrlimit( resource, rlim ); */
#  ifdef __NR_ugetrlimit
   res = VG_(do_syscall2)(__NR_ugetrlimit, resource, (UWord)rlim);
#  endif
   if (res == -VKI_ENOSYS)
      res = VG_(do_syscall2)(__NR_getrlimit, resource, (UWord)rlim);
   if (VG_(is_kerror)(res)) res = -1;
   return res;
}


/* Support for setrlimit. */
Int VG_(setrlimit) (Int resource, const struct vki_rlimit *rlim)
{
   Int res;
   /* res = setrlimit( resource, rlim ); */
   res = VG_(do_syscall2)(__NR_setrlimit, resource, (UWord)rlim);
   if (VG_(is_kerror)(res)) res = -1;
   return res;
}


/* Support for getdents. */
Int VG_(getdents) (UInt fd, struct vki_dirent *dirp, UInt count)
{
   Int res;
   /* res = getdents( fd, dirp, count ); */
   res = VG_(do_syscall3)(__NR_getdents, fd, (UWord)dirp, count);
   if (VG_(is_kerror)(res)) res = -1;
   return res;
}

/* Support for a readlink.  */
Int VG_(readlink) (Char* path, Char* buf, UInt bufsiz)
{
   Int res;
   /* res = readlink( path, buf, bufsiz ); */
   res = VG_(do_syscall3)(__NR_readlink, (UWord)path, (UWord)buf, bufsiz);
   if (VG_(is_kerror)(res)) res = -1;
   return res;
}

/* You'd be amazed how many places need to know the current pid. */
Int VG_(getpid) ( void )
{
   Int res;
   /* res = getpid(); */
   res = VG_(do_syscall0)(__NR_getpid);
   return res;
}

Int VG_(getpgrp) ( void )
{
   Int res;
   /* res = getpgid(); */
   res = VG_(do_syscall0)(__NR_getpgrp);
   return res;
}

Int VG_(getppid) ( void )
{
   Int res;
   res = VG_(do_syscall0)(__NR_getppid);
   return res;
}

Int VG_(setpgid) ( Int pid, Int pgrp )
{
   return VG_(do_syscall2)(__NR_setpgid, pid, pgrp);
}

/* Walk through a colon-separated environment variable, and remove the
   entries which match remove_pattern.  It slides everything down over
   the removed entries, and pads the remaining space with '\0'.  It
   modifies the entries in place (in the client address space), but it
   shouldn't matter too much, since we only do this just before an
   execve().

   This is also careful to mop up any excess ':'s, since empty strings
   delimited by ':' are considered to be '.' in a path.
*/
static void mash_colon_env(Char *varp, const Char *remove_pattern)
{
   Char *const start = varp;
   Char *entry_start = varp;
   Char *output = varp;

   if (varp == NULL)
      return;

   while(*varp) {
      if (*varp == ':') {
	 Char prev;
	 Bool match;

	 /* This is a bit subtle: we want to match against the entry
	    we just copied, because it may have overlapped with
	    itself, junking the original. */

	 prev = *output;
	 *output = '\0';

	 match = VG_(string_match)(remove_pattern, entry_start);

	 *output = prev;
	 
	 if (match) {
	    output = entry_start;
	    varp++;			/* skip ':' after removed entry */
	 } else
	    entry_start = output+1;	/* entry starts after ':' */
      }

      *output++ = *varp++;
   }

   /* match against the last entry */
   if (VG_(string_match)(remove_pattern, entry_start)) {
      output = entry_start;
      if (output > start) {
	 /* remove trailing ':' */
	 output--;
	 vg_assert(*output == ':');
      }
   }	 

   /* pad out the left-overs with '\0' */
   while(output < varp)
      *output++ = '\0';
}


// Removes all the Valgrind-added stuff from the passed environment.  Used
// when starting child processes, so they don't see that added stuff.
void VG_(env_remove_valgrind_env_stuff)(Char** envp)
{
   Int i;
   Char* ld_preload_str = NULL;
   Char* ld_library_path_str = NULL;
   Char* buf;

   // Find LD_* variables
   for (i = 0; envp[i] != NULL; i++) {
      if (VG_(strncmp)(envp[i], "LD_PRELOAD=", 11) == 0)
         ld_preload_str = &envp[i][11];
      if (VG_(strncmp)(envp[i], "LD_LIBRARY_PATH=", 16) == 0)
         ld_library_path_str = &envp[i][16];
   }

   buf = VG_(arena_malloc)(VG_AR_CORE, VG_(strlen)(VG_(libdir)) + 20);

   // Remove Valgrind-specific entries from LD_*.
   VG_(sprintf)(buf, "%s*/vg_inject.so", VG_(libdir));
   mash_colon_env(ld_preload_str, buf);
   VG_(sprintf)(buf, "%s*/vgpreload_*.so", VG_(libdir));
   mash_colon_env(ld_preload_str, buf);
   VG_(sprintf)(buf, "%s*", VG_(libdir));
   mash_colon_env(ld_library_path_str, buf);

   // Remove VALGRIND_CLO variable.
   VG_(env_unsetenv)(envp, VALGRINDCLO);

   // XXX if variable becomes empty, remove it completely?

   VG_(arena_free)(VG_AR_CORE, buf);
}

/* Return -1 if error, else 0.  NOTE does not indicate return code of
   child! */
Int VG_(system) ( Char* cmd )
{
   Int pid, res;
   if (cmd == NULL)
      return 1;
   pid = VG_(do_syscall0)(__NR_fork);
   if (VG_(is_kerror)(pid))
      return -1;
   if (pid == 0) {
      /* child */
      static Char** envp = NULL;
      Char* argv[4];

      /* restore the DATA rlimit for the child */
      VG_(setrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));

      envp = env_clone(VG_(client_envp));
      VG_(env_remove_valgrind_env_stuff)( envp ); 

      argv[0] = "/bin/sh";
      argv[1] = "-c";
      argv[2] = cmd;
      argv[3] = 0;

      (void)VG_(do_syscall3)(__NR_execve, 
                             (UWord)"/bin/sh", (UWord)argv, (UWord)envp);

      /* If we're still alive here, execve failed. */
      VG_(exit)(1);
   } else {
      /* parent */
      res = VG_(waitpid)(pid, NULL, 0);
      if (VG_(is_kerror)(res)) {
         return -1;
      } else {
	 return 0;
      }
   }
}


/* ---------------------------------------------------------------------
   Support for a millisecond-granularity timer.
   ------------------------------------------------------------------ */

UInt VG_(read_millisecond_timer) ( void )
{
   static ULong base = 0;
   struct vki_timeval tv_now;
   ULong now;
   Int res;

   res = VG_(do_syscall2)(__NR_gettimeofday, (UWord)&tv_now, (UWord)NULL);
   
   now = tv_now.tv_sec * 1000000ULL + tv_now.tv_usec;
   
   if (base == 0)
      base = now;

   return (now - base) / 1000;
}


void VG_(nanosleep)(struct vki_timespec *ts)
{
   VG_(do_syscall2)(__NR_nanosleep, (UWord)ts, (UWord)NULL);
}

/* ---------------------------------------------------------------------
   Primitive support for bagging memory via mmap.
   ------------------------------------------------------------------ */

void* VG_(get_memory_from_mmap) ( SizeT nBytes, Char* who )
{
   static SizeT tot_alloc = 0;
   void* p;
   p = VG_(mmap)(0, nBytes,
                 VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC,
                 VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 0, -1, 0);

   if (p != ((void*)(-1))) {
      vg_assert((void*)VG_(valgrind_base) <= p && p <= (void*)VG_(valgrind_last));
      tot_alloc += nBytes;
      if (0)
         VG_(printf)(
            "get_memory_from_mmap: %llu tot, %llu req = %p .. %p, caller %s\n",
            (ULong)tot_alloc, (ULong)nBytes, p, ((char*)p) + nBytes - 1, who );
      return p;
   }

   VG_(printf)("\n");
   VG_(printf)("VG_(get_memory_from_mmap): %s's request for %llu bytes failed.\n",
               who, (ULong)nBytes);
   VG_(printf)("VG_(get_memory_from_mmap): %llu bytes already allocated.\n", 
               (ULong)tot_alloc);
   VG_(printf)("\n");
   VG_(printf)("Sorry.  You could try using a tool that uses less memory;\n");
   VG_(printf)("eg. addrcheck instead of memcheck.\n");
   VG_(printf)("\n");
   VG_(exit)(1);
}

/* ---------------------------------------------------------------------
   Generally useful...
   ------------------------------------------------------------------ */

Int VG_(log2) ( Int x ) 
{
   Int i;
   /* Any more than 32 and we overflow anyway... */
   for (i = 0; i < 32; i++) {
      if (1 << i == x) return i;
   }
   return -1;
}


// Generic shell sort.  Like stdlib.h's qsort().
void VG_(ssort)( void* base, SizeT nmemb, SizeT size,
                 Int (*compar)(void*, void*) )
{
   Int   incs[14] = { 1, 4, 13, 40, 121, 364, 1093, 3280,
                      9841, 29524, 88573, 265720,
                      797161, 2391484 };
   Int   lo = 0;
   Int   hi = nmemb-1;
   Int   i, j, h, bigN, hp;

   bigN = hi - lo + 1; if (bigN < 2) return;
   hp = 0; while (hp < 14 && incs[hp] < bigN) hp++; hp--;
   vg_assert(0 <= hp && hp < 14);

   #define SORT \
   for ( ; hp >= 0; hp--) { \
      h = incs[hp]; \
      for (i = lo + h; i <= hi; i++) { \
         ASSIGN(v,0, a,i); \
         j = i; \
         while (COMPAR(a,(j-h), v,0) > 0) { \
            ASSIGN(a,j, a,(j-h)); \
            j = j - h; \
            if (j <= (lo + h - 1)) break; \
         } \
         ASSIGN(a,j, v,0); \
      } \
   }

   // Specialised cases
   if (sizeof(ULong) == size) {

      #define ASSIGN(dst, dsti, src, srci) \
      (dst)[(dsti)] = (src)[(srci)];      
      
      #define COMPAR(dst, dsti, src, srci) \
      compar( (void*)(& (dst)[(dsti)]), (void*)(& (src)[(srci)]) )

      ULong* a = (ULong*)base;
      ULong  v[1];

      SORT;

   } else if (sizeof(UInt) == size) {

      UInt* a = (UInt*)base;
      UInt  v[1];

      SORT;

   } else if (sizeof(UShort) == size) {
      UShort* a = (UShort*)base;
      UShort  v[1];

      SORT;

   } else if (sizeof(UChar) == size) {
      UChar* a = (UChar*)base;
      UChar  v[1];

      SORT;

      #undef ASSIGN
      #undef COMPAR

   // General case
   } else {
      char* a = base;
      char  v[size];      // will be at least 'size' bytes

      #define ASSIGN(dst, dsti, src, srci) \
      VG_(memcpy)( &dst[size*(dsti)], &src[size*(srci)], size );

      #define COMPAR(dst, dsti, src, srci) \
      compar( &dst[size*(dsti)], &src[size*(srci)] )

      SORT;

      #undef ASSIGN
      #undef COMPAR
   }
   #undef SORT
}

/* ---------------------------------------------------------------------
   Gruesome hackery for connecting to a logging server over the network.
   This is all very Linux-kernel specific.
   ------------------------------------------------------------------ */

static
Int parse_inet_addr_and_port ( UChar* str, UInt* ip_addr, UShort* port );

static
Int my_socket ( Int domain, Int type, Int protocol );

static
Int my_connect ( Int sockfd, struct vki_sockaddr_in* serv_addr, 
                 Int addrlen );

static 
UInt my_htonl ( UInt x )
{
   return
      (((x >> 24) & 0xFF) << 0) | (((x >> 16) & 0xFF) << 8)
      | (((x >> 8) & 0xFF) << 16) | (((x >> 0) & 0xFF) << 24);
}

static
UShort my_htons ( UShort x )
{
   return
      (((x >> 8) & 0xFF) << 0) | (((x >> 0) & 0xFF) << 8);
}


/* The main function. 

   Supplied string contains either an ip address "192.168.0.1" or
   an ip address and port pair, "192.168.0.1:1500".  Parse these,
   and return:
     -1 if there is a parse error
     -2 if no parse error, but specified host:port cannot be opened
     the relevant file (socket) descriptor, otherwise.
 is used.
*/
Int VG_(connect_via_socket)( UChar* str )
{
   Int sd, res;
   struct vki_sockaddr_in servAddr;
   UInt   ip   = 0;
   UShort port = VG_CLO_DEFAULT_LOGPORT;
   Bool   ok   = parse_inet_addr_and_port(str, &ip, &port);
   if (!ok) 
      return -1;

   if (0)
      VG_(printf)("ip = %d.%d.%d.%d, port %d\n",
                  (ip >> 24) & 0xFF, (ip >> 16) & 0xFF, 
                  (ip >> 8) & 0xFF, ip & 0xFF, 
	          (UInt)port );

   servAddr.sin_family = VKI_AF_INET;
   servAddr.sin_addr.s_addr = my_htonl(ip);
   servAddr.sin_port = my_htons(port);

   /* create socket */
   sd = my_socket(VKI_AF_INET, VKI_SOCK_STREAM, 0 /* IPPROTO_IP ? */);
   if (sd < 0) {
     /* this shouldn't happen ... nevertheless */
     return -2;
   }
			
   /* connect to server */
   res = my_connect(sd, (struct vki_sockaddr_in *) &servAddr, 
                        sizeof(servAddr));
   if (res < 0) {
     /* connection failed */
     return -2;
   }

   return sd;
}


/* Let d = one or more digits.  Accept either:
   d.d.d.d  or  d.d.d.d:d
*/
Int parse_inet_addr_and_port ( UChar* str, UInt* ip_addr, UShort* port )
{
#  define GET_CH ((*str) ? (*str++) : 0)
   UInt ipa, i, j, c, any;
   ipa = 0;
   for (i = 0; i < 4; i++) {
      j = 0;
      any = 0;
      while (1) {
         c = GET_CH; 
         if (c < '0' || c > '9') break;
         j = 10 * j + (int)(c - '0');
         any = 1;
      }
      if (any == 0 || j > 255) goto syntaxerr;
      ipa = (ipa << 8) + j;
      if (i <= 2 && c != '.') goto syntaxerr;
   }
   if (c == 0 || c == ':') 
      *ip_addr = ipa;
   if (c == 0) goto ok;
   if (c != ':') goto syntaxerr;
   j = 0;
   any = 0;
   while (1) {
      c = GET_CH; 
      if (c < '0' || c > '9') break;
      j = j * 10 + (int)(c - '0');
      any = 1;
      if (j > 65535) goto syntaxerr;
   }
   if (any == 0 || c != 0) goto syntaxerr;
   if (j < 1024) goto syntaxerr;
   *port = (UShort)j;
 ok:
   return 1;
 syntaxerr:
   return 0;
#  undef GET_CH
}


static
Int my_socket ( Int domain, Int type, Int protocol )
{
// AMD64/Linux doesn't define __NR_socketcall... see comment above
// VG_(sigpending)() for more details.
#ifdef __amd64__
   I_die_here;
#else
   Int res;
   UWord args[3];
   args[0] = domain;
   args[1] = type;
   args[2] = protocol;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_SOCKET, (UWord)&args);
   if (VG_(is_kerror)(res)) 
      res = -1;
   return res;
#endif
}

static
Int my_connect ( Int sockfd, struct vki_sockaddr_in* serv_addr, 
                 Int addrlen )
{
// AMD64/Linux doesn't define __NR_socketcall... see comment above
// VG_(sigpending)() for more details.
#ifdef __amd64__
   I_die_here;
#else
   Int res;
   UWord args[3];
   args[0] = sockfd;
   args[1] = (UWord)serv_addr;
   args[2] = addrlen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_CONNECT, (UWord)&args);
   if (VG_(is_kerror)(res)) 
      res = -1;
   return res;
#endif
}

Int VG_(write_socket)( Int sd, void *msg, Int count )
{
// AMD64/Linux doesn't define __NR_socketcall... see comment above
// VG_(sigpending)() for more details.
#ifdef __amd64__
   I_die_here;
#else
   /* This is actually send(). */

   /* Requests not to send SIGPIPE on errors on stream oriented
      sockets when the other end breaks the connection. The EPIPE
      error is still returned. */
   Int flags = VKI_MSG_NOSIGNAL;

   Int res;
   UWord args[4];
   args[0] = sd;
   args[1] = (UWord)msg;
   args[2] = count;
   args[3] = flags;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_SEND, (UWord)&args);
   if (VG_(is_kerror)(res)) 
      res = -1;
   return res;
#endif
}

Int VG_(getsockname) ( Int sd, struct vki_sockaddr *name, Int *namelen)
{
// AMD64/Linux doesn't define __NR_socketcall... see comment above
// VG_(sigpending)() for more details.
#ifdef __amd64__
   I_die_here;
#else
   Int res;
   UWord args[3];
   args[0] = sd;
   args[1] = (UWord)name;
   args[2] = (UWord)namelen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETSOCKNAME, (UWord)&args);
   if(VG_(is_kerror)(res))
      res = -1;
   return res;
#endif
}

Int VG_(getpeername) ( Int sd, struct vki_sockaddr *name, Int *namelen)
{
// AMD64/Linux doesn't define __NR_socketcall... see comment above
// VG_(sigpending)() for more details.
#ifdef __amd64__
   I_die_here;
#else
   Int res;
   UWord args[3];
   args[0] = sd;
   args[1] = (UWord)name;
   args[2] = (UWord)namelen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETPEERNAME, (UWord)&args);
   if(VG_(is_kerror)(res))
      res = -1;
   return res;
#endif
}

Int VG_(getsockopt) ( Int sd, Int level, Int optname, void *optval,
                      Int *optlen)
{
// AMD64/Linux doesn't define __NR_socketcall... see comment above
// VG_(sigpending)() for more details.
#ifdef __amd64__
   I_die_here;
#else
   Int res;
   UWord args[5];
   args[0] = sd;
   args[1] = level;
   args[2] = optname;
   args[3] = (UWord)optval;
   args[4] = (UWord)optlen;
   res = VG_(do_syscall2)(__NR_socketcall, VKI_SYS_GETSOCKOPT, (UWord)&args);
   if(VG_(is_kerror)(res))
      res = -1;
   return res;
#endif
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
