
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- A library of wrappers for MPI 2 functions.              ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/* ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (mpiwrap.c) only.  The rest of Valgrind is licensed under the
   terms of the GNU General Public License, version 2, unless
   otherwise indicated.  See the COPYING file in the source
   distribution for details.

   ----------------------------------------------------------------

   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2013 OpenWorks LLP.  All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. The origin of this software must not be misrepresented; you must
      not claim that you wrote the original software.  If you use this
      software in a product, an acknowledgment in the product
      documentation would be appreciated but is not required.

   3. Altered source versions must be plainly marked as such, and must
      not be misrepresented as being the original software.

   4. The name of the author may not be used to endorse or promote
      products derived from this software without specific prior written
      permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* Handling of MPI_STATUS{ES}_IGNORE for MPI_Status* arguments.

   The MPI-2 spec allows many functions which have MPI_Status* purely
   as an out parameter, to accept the constants MPI_STATUS_IGNORE or
   MPI_STATUSES_IGNORE there instead, if the caller does not care
   about the status.  See the MPI-2 spec sec 4.5.1 ("Passing
   MPI_STATUS_IGNORE for Status").  (mpi2-report.pdf, 1615898 bytes,
   md5=694a5efe2fd291eecf7e8c9875b5f43f).

   This library handles such cases by allocating a fake MPI_Status
   object (on the stack) or an array thereof (on the heap), and
   passing that onwards instead.  From the outside the caller sees no
   difference.  Unfortunately the simpler approach of merely detecting
   and handling these special cases at a lower level does not work,
   because we need to use information returned in MPI_Status*
   arguments to paint result buffers, even if the caller doesn't
   supply a real MPI_Status object.

   Eg, MPI_Recv.  We can't paint the result buffer without knowing how
   many items arrived; but we can't find that out without passing a
   real MPI_Status object to the (real) MPI_Recv call.  Hence, if the
   caller did not supply one, we have no option but to use a temporary
   stack allocated one for the inner call.  Ditto, more indirectly
   (via maybe_complete) for nonblocking receives and the various
   associated wait/test calls. */


/*------------------------------------------------------------*/
/*--- includes                                             ---*/
/*------------------------------------------------------------*/

#include <stdio.h>
#include <assert.h>
#include <unistd.h>     /* getpid */
#include <stdlib.h>     /* exit */
#include <string.h>     /* strstr */
#include <pthread.h>    /* pthread_mutex_{lock,unlock} */

/* Include Valgrind magic macros for writing wrappers. */
#include "../memcheck/memcheck.h"

/* Include macros for VALGRIND_{DIS,EN}ABLE_ERROR_REPORTING.
   This is somewhat experimental and hence disable-able, by
   setting cONFIG_DER to zero. */
#include "../include/valgrind.h"

#define cONFIG_DER  1   /* set to 0 to disable */


/*------------------------------------------------------------*/
/*--- Connect to MPI library                               ---*/
/*------------------------------------------------------------*/

/* Include headers for whatever MPI implementation the wrappers are to
   be used with.  The configure system will tell us what the path to
   the chosen MPI implementation is, via -I.. to the compiler. */
#include "mpi.h"

/* Where are API symbols?
   Open MPI      lib/libmpi.so,   soname = libmpi.so.0
   Quadrics MPI  lib/libmpi.so,   soname = libmpi.so.0
   MPICH         libmpich.so.1.0, soname = libmpich.so.1.0

   A suitable soname to match with is therefore "libmpi*.so*".
*/
#define I_WRAP_FNNAME_U(_name) \
        I_WRAP_SONAME_FNNAME_ZU(libmpiZaZdsoZa,_name)


/* Define HAVE_MPI_STATUS_IGNORE iff we have to deal with
   MPI_STATUS{ES}_IGNORE. */
#if MPI_VERSION >= 2 \
    || (defined(MPI_STATUS_IGNORE) && defined(MPI_STATUSES_IGNORE))
#  undef HAVE_MPI_STATUS_IGNORE
#  define HAVE_MPI_STATUS_IGNORE 1
#else
#  undef HAVE_MPI_STATUS_IGNORE
#endif


/*------------------------------------------------------------*/
/*--- Decls                                                ---*/
/*------------------------------------------------------------*/

typedef  unsigned char  Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

/* Word, UWord are machine words - same size as a pointer.  This is
   checked at startup.  The wrappers below use 'long' to mean a
   machine word - this too is tested at startup. */
typedef    signed long  Word;
typedef  unsigned long  UWord;

#if !defined(offsetof)
#  define offsetof(type,memb) ((int)&((type*)0)->memb)
#endif

/* Find the size of long double image (not 'sizeof(long double)').
   See comments in sizeofOneNamedTy. */
static long sizeof_long_double_image ( void );


/*------------------------------------------------------------*/
/*--- Simple helpers                                       ---*/
/*------------------------------------------------------------*/

/* ------ Helpers for debug printing ------ */

/* constant */
static const char* preamble = "valgrind MPI wrappers";

/* established at startup */
static pid_t my_pid         = -1;
static char* options_str    = NULL;
static int   opt_verbosity  = 1;
static Bool  opt_missing    = 0; /* 0:silent; 1:warn; 2:abort */
static Bool  opt_help       = False;
static Bool  opt_initkludge = False;

static void before ( char* fnname )
{
   /* This isn't thread-safe wrt 'done' (no locking).  It's not
      critical. */
   static int done = 0;
   if (done == 0) {
      done = 1;
      my_pid = getpid();
      options_str = getenv("MPIWRAP_DEBUG");
      if (options_str) {
         if (NULL != strstr(options_str, "warn"))
            opt_missing = 1;
         if (NULL != strstr(options_str, "strict"))
            opt_missing = 2;
         if (NULL != strstr(options_str, "verbose"))
            opt_verbosity++;
         if (NULL != strstr(options_str, "quiet"))
            opt_verbosity--;
         if (NULL != strstr(options_str, "help"))
            opt_help = True;
         if (NULL != strstr(options_str, "initkludge"))
            opt_initkludge = True;
      }
      if (opt_verbosity > 0)
         fprintf(stderr, "%s %5d: Active for pid %d\n", 
                         preamble, my_pid, my_pid);
      /* Sanity check - that Word/UWord really are machine words. */
      assert(sizeof(Word)  == sizeof(void*));
      assert(sizeof(UWord) == sizeof(void*));
      /* Sanity check - char is byte-sized (else address calculations
         in walk_type don't work. */
      assert(sizeof(char) == 1);
      if (opt_help) {
         fprintf(stderr, "\n");
         fprintf(stderr, "Valid options for the MPIWRAP_DEBUG environment"
                         " variable are:\n");
         fprintf(stderr, "\n");
         fprintf(stderr, "   quiet       be silent except for errors\n");
         fprintf(stderr, "   verbose     show wrapper entries/exits\n");
         fprintf(stderr, "   strict      abort the program if a function"
                         " with no wrapper is used\n");
         fprintf(stderr, "   warn        give a warning if a function"
                         " with no wrapper is used\n");
         fprintf(stderr, "   help        display this message, then exit\n");
         fprintf(stderr, "   initkludge  debugging hack; do not use\n");
         fprintf(stderr, "\n");
         fprintf(stderr, "Multiple options are allowed, eg"
                         " MPIWRAP_DEBUG=strict,verbose\n");
         fprintf(stderr, "Note: 'warn' generates output even if 'quiet'"
                         " is also specified\n");
         fprintf(stderr, "\n");
         fprintf(stderr, "%s %5d: exiting now\n", preamble, my_pid );
         exit(1);
      }
      if (opt_verbosity > 0)
         fprintf(stderr, 
                 "%s %5d: Try MPIWRAP_DEBUG=help for possible options\n", 
                 preamble, my_pid);

   }
   if (opt_verbosity > 1)
      fprintf(stderr, "%s %5d: enter PMPI_%s\n", preamble,  my_pid, fnname );
}

static __inline__ void after ( char* fnname, int err )
{
   if (opt_verbosity > 1)
      fprintf(stderr, "%s %5d:  exit PMPI_%s (err = %d)\n", 
                      preamble, my_pid, fnname, err );
}

static void barf ( char* msg )
{
   fprintf(stderr, "%s %5d: fatal: %s\n",   preamble, my_pid, msg);
   fprintf(stderr, "%s %5d: exiting now\n", preamble, my_pid );
   exit(1);
}

/* Half-hearted type-showing function (for debugging). */
static void showTy ( FILE* f, MPI_Datatype ty )
{
        if (ty == MPI_DATATYPE_NULL)  fprintf(f,"DATATYPE_NULL");
   else if (ty == MPI_BYTE)           fprintf(f,"BYTE");
   else if (ty == MPI_PACKED)         fprintf(f,"PACKED");
   else if (ty == MPI_CHAR)           fprintf(f,"CHAR");
   else if (ty == MPI_SHORT)          fprintf(f,"SHORT");
   else if (ty == MPI_INT)            fprintf(f,"INT");
   else if (ty == MPI_LONG)           fprintf(f,"LONG");
   else if (ty == MPI_FLOAT)          fprintf(f,"FLOAT");
   else if (ty == MPI_DOUBLE)         fprintf(f,"DOUBLE");
   else if (ty == MPI_LONG_DOUBLE)    fprintf(f,"LONG_DOUBLE");
   else if (ty == MPI_UNSIGNED_CHAR)  fprintf(f,"UNSIGNED_CHAR");
   else if (ty == MPI_UNSIGNED_SHORT) fprintf(f,"UNSIGNED_SHORT");
   else if (ty == MPI_UNSIGNED_LONG)  fprintf(f,"UNSIGNED_LONG");
   else if (ty == MPI_UNSIGNED)       fprintf(f,"UNSIGNED");
   else if (ty == MPI_FLOAT_INT)      fprintf(f,"FLOAT_INT");
   else if (ty == MPI_DOUBLE_INT)     fprintf(f,"DOUBLE_INT");
   else if (ty == MPI_LONG_DOUBLE_INT) fprintf(f,"LONG_DOUBLE_INT");
   else if (ty == MPI_LONG_INT)       fprintf(f,"LONG_INT");
   else if (ty == MPI_SHORT_INT)      fprintf(f,"SHORT_INT");
   else if (ty == MPI_2INT)           fprintf(f,"2INT");
   else if (ty == MPI_UB)             fprintf(f,"UB");
   else if (ty == MPI_LB)             fprintf(f,"LB");
#  if defined(MPI_WCHAR)
   else if (ty == MPI_WCHAR)          fprintf(f,"WCHAR");
#  endif
   else if (ty == MPI_LONG_LONG_INT)  fprintf(f,"LONG_LONG_INT");
#  if defined(MPI_LONG_LONG)
   else if (ty == MPI_LONG_LONG)      fprintf(f,"LONG_LONG");
#  endif
#  if defined(MPI_UNSIGNED_LONG_LONG)
   else if (ty == MPI_UNSIGNED_LONG_LONG) fprintf(f,"UNSIGNED_LONG_LONG");
#  endif
#  if defined(MPI_REAL8)
   else if (ty == MPI_REAL8)          fprintf(f, "REAL8");
#  endif
#  if defined(MPI_REAL4)
   else if (ty == MPI_REAL4)          fprintf(f, "REAL4");
#  endif
#  if defined(MPI_REAL)
   else if (ty == MPI_REAL)           fprintf(f, "REAL");
#  endif
#  if defined(MPI_INTEGER8)
   else if (ty == MPI_INTEGER8)       fprintf(f, "INTEGER8");
#  endif
#  if defined(MPI_INTEGER4)
   else if (ty == MPI_INTEGER4)       fprintf(f, "INTEGER4");
#  endif
#  if defined(MPI_INTEGER)
   else if (ty == MPI_INTEGER)        fprintf(f, "INTEGER");
#  endif
#  if defined(MPI_DOUBLE_PRECISION)
   else if (ty == MPI_DOUBLE_PRECISION) fprintf(f, "DOUBLE_PRECISION");
#  endif
#  if defined(MPI_COMPLEX)
   else if (ty == MPI_COMPLEX)          fprintf(f, "COMPLEX");
#  endif
#  if defined(MPI_DOUBLE_COMPLEX)
   else if (ty == MPI_DOUBLE_COMPLEX)   fprintf(f, "DOUBLE_COMPLEX");
#  endif
#  if defined(MPI_LOGICAL)
   else if (ty == MPI_LOGICAL)          fprintf(f, "LOGICAL");
#  endif
#  if defined(MPI_2INTEGER)
   else if (ty == MPI_2INTEGER)         fprintf(f, "2INTEGER");
#  endif
#  if defined(MPI_2COMPLEX)
   else if (ty == MPI_2COMPLEX)         fprintf(f, "2COMPLEX");
#  endif
#  if defined(MPI_2DOUBLE_COMPLEX)
   else if (ty == MPI_2DOUBLE_COMPLEX)  fprintf(f, "2DOUBLE_COMPLEX");
#  endif
#  if defined(MPI_2REAL)
   else if (ty == MPI_2REAL)            fprintf(f, "2REAL");
#  endif
#  if defined(MPI_2DOUBLE_PRECISION)
   else if (ty == MPI_2DOUBLE_PRECISION) fprintf(f, "2DOUBLE_PRECISION");
#  endif
#  if defined(MPI_CHARACTER)
   else if (ty == MPI_CHARACTER)         fprintf(f, "CHARACTER");
#  endif
   else fprintf(f,"showTy:???");
}

static void showCombiner ( FILE* f, int combiner )
{
   switch (combiner) {
      case MPI_COMBINER_NAMED:       fprintf(f, "NAMED"); break;
#if   defined(MPI_COMBINER_DUP)
      case MPI_COMBINER_DUP:         fprintf(f, "DUP"); break;
#     endif
      case MPI_COMBINER_CONTIGUOUS:  fprintf(f, "CONTIGUOUS"); break;
      case MPI_COMBINER_VECTOR:      fprintf(f, "VECTOR"); break;
#if   defined(MPI_COMBINER_HVECTOR_INTEGER)
      case MPI_COMBINER_HVECTOR_INTEGER: fprintf(f, "HVECTOR_INTEGER"); break;
#     endif
      case MPI_COMBINER_HVECTOR:     fprintf(f, "HVECTOR"); break;
      case MPI_COMBINER_INDEXED:     fprintf(f, "INDEXED"); break;
#if   defined(MPI_COMBINER_HINDEXED_INTEGER)
      case MPI_COMBINER_HINDEXED_INTEGER: fprintf(f, "HINDEXED_INTEGER"); break;
#     endif
      case MPI_COMBINER_HINDEXED:    fprintf(f, "HINDEXED"); break;
#if   defined(MPI_COMBINER_INDEXED_BLOCK)
      case MPI_COMBINER_INDEXED_BLOCK: fprintf(f, "INDEXED_BLOCK"); break;
#     endif
#if   defined(MPI_COMBINER_STRUCT_INTEGER)
      case MPI_COMBINER_STRUCT_INTEGER: fprintf(f, "STRUCT_INTEGER"); break;
#     endif
      case MPI_COMBINER_STRUCT:      fprintf(f, "STRUCT"); break;
#if   defined(MPI_COMBINER_SUBARRAY)
      case MPI_COMBINER_SUBARRAY:    fprintf(f, "SUBARRAY"); break;
#     endif
#if   defined(MPI_COMBINER_DARRAY)
      case MPI_COMBINER_DARRAY:      fprintf(f, "DARRAY"); break;
#     endif
#if   defined(MPI_COMBINER_F90_REAL)
      case MPI_COMBINER_F90_REAL:    fprintf(f, "F90_REAL"); break;
#     endif
#if   defined(MPI_COMBINER_F90_COMPLEX)
      case MPI_COMBINER_F90_COMPLEX: fprintf(f, "F90_COMPLEX"); break;
#     endif
#if   defined(MPI_COMBINER_F90_INTEGER)
      case MPI_COMBINER_F90_INTEGER: fprintf(f, "F90_INTEGER"); break;
#     endif
#if   defined(MPI_COMBINER_RESIZED)
      case MPI_COMBINER_RESIZED:     fprintf(f, "RESIZED"); break;
#     endif
      default: fprintf(f, "showCombiner:??"); break;
   }
}


/* ------ Get useful bits of info ------ */

/* Note, PMPI_Comm_rank/size are themselves wrapped.  Should work
   fine. */

static __inline__ int comm_rank ( MPI_Comm comm ) 
{
   int err, r;
   err = PMPI_Comm_rank(comm, &r);
   return err ? 0/*arbitrary*/ : r;
}

static __inline__ int comm_size ( MPI_Comm comm ) 
{
   int err, r;
   err = PMPI_Comm_size(comm, &r);
   return err ? 0/*arbitrary*/ : r;
}

static __inline__ Bool count_from_Status( /*OUT*/int* recv_count, 
                                      MPI_Datatype datatype, 
                                      MPI_Status* status)
{
   int n;
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   int err = PMPI_Get_count(status, datatype, &n);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS) {
      *recv_count = n;
      return True;
   } else {
      return False;
   }
}

/* It's critical that we can do equality on MPI_Requests.
   Unfortunately these are opaque objects to us (handles, in the
   parlance of the MPI 1.1 spec).  Fortunately Sec 2.4.1 ("Opaque
   Objects") specifies that "In C, [...] These [handles] should be
   types that support assignment and equality operations."  Hence the
   following function should compile for any compliant definition of
   MPI_Request. */
static __inline__ 
Bool eq_MPI_Request ( MPI_Request r1, MPI_Request r2 )
{
   return r1 == r2;
}

/* Return True if status is MPI_STATUS_IGNORE or MPI_STATUSES_IGNORE.
   On MPI-1.x platforms which don't have these symbols (and they would
   only have them if they've been backported from 2.x) always return
   False. */
static __inline__
Bool isMSI ( MPI_Status* status )
{
#  if defined(HAVE_MPI_STATUS_IGNORE)
   return status == MPI_STATUSES_IGNORE || status == MPI_STATUS_IGNORE;
#  else
   return False;
#  endif
}

/* Get the 'extent' of a type.  Note, as per the MPI spec this
   includes whatever padding would be required when using 'ty' in an
   array. */
static long extentOfTy ( MPI_Datatype ty )
{
   int      r;
   MPI_Aint n;
   r = PMPI_Type_extent(ty, &n);
   assert(r == MPI_SUCCESS);
   return (long)n;
}

/* Free up *ty, if it is safe to do so */
static void maybeFreeTy ( MPI_Datatype* ty )
{
   int r, n_ints, n_addrs, n_dtys, tycon;

   r = PMPI_Type_get_envelope( *ty, &n_ints, &n_addrs, &n_dtys, &tycon );
   assert(r == MPI_SUCCESS);

   /* can't free named types */
   if (tycon == MPI_COMBINER_NAMED)
      return;

   /* some kinds of structs are predefined so we can't free them
      either. */
   if (*ty == MPI_FLOAT_INT || *ty == MPI_DOUBLE_INT 
       || *ty == MPI_LONG_INT || *ty == MPI_2INT 
       || *ty == MPI_SHORT_INT || *ty == MPI_LONG_DOUBLE_INT)
      return;

   /* Looks OK - free it. */
   if (0) {
      /* show me what you're about to free .. */
      fprintf(stderr, "freeing combiner ");
      showCombiner(stderr,tycon);
      fprintf(stderr, " ty= ");
      showTy(stderr,*ty);
      fprintf(stderr,"\n");
   }
   r = PMPI_Type_free(ty);
   assert(r == MPI_SUCCESS);
}

/* How big is a "named" (base) type?  Returns 0 if not known.  Note.
   There is a subtlety, which is that this is required to return the
   exact size of one item of the type, NOT the size of it when padded
   suitably to make an array of them.  In particular that's why the
   size of LONG_DOUBLE is computed by looking at the result of doing a
   long double store, rather than just asking what is the sizeof(long
   double).

   For LONG_DOUBLE on x86-linux and amd64-linux my impression is that
   the right answer is 10 even though sizeof(long double) says 12 and
   16 respectively.  On ppc32-linux it appears to be 16.

   Ref: MPI 1.1 doc p18 */
static long sizeofOneNamedTy ( MPI_Datatype ty )
{
   if (ty == MPI_CHAR)           return sizeof(signed char);
   if (ty == MPI_SHORT)          return sizeof(signed short int);
   if (ty == MPI_INT)            return sizeof(signed int);
   if (ty == MPI_LONG)           return sizeof(signed long int);
   if (ty == MPI_UNSIGNED_CHAR)  return sizeof(unsigned char);
   if (ty == MPI_UNSIGNED_SHORT) return sizeof(unsigned short int);
   if (ty == MPI_UNSIGNED)       return sizeof(unsigned int);
   if (ty == MPI_UNSIGNED_LONG)  return sizeof(unsigned long int);
   if (ty == MPI_FLOAT)          return sizeof(float);
   if (ty == MPI_DOUBLE)         return sizeof(double);
   if (ty == MPI_BYTE)           return 1;
   if (ty == MPI_LONG_DOUBLE)    return sizeof_long_double_image();
   if (ty == MPI_PACKED)         return 1;
   if (ty == MPI_LONG_LONG_INT)  return sizeof(signed long long int);

#  if defined(MPI_REAL8)
   if (ty == MPI_REAL8)          return 8; /* MPI2 spec */;
#  endif
#  if defined(MPI_REAL4)
   if (ty == MPI_REAL4)          return 4; /* MPI2 spec */;
#  endif
#  if defined(MPI_REAL)
   if (ty == MPI_REAL)           return 4; /* MPI2 spec */;
#  endif
#  if defined(MPI_INTEGER8)
   if (ty == MPI_INTEGER8)       return 8; /* MPI2 spec */;
#  endif
#  if defined(MPI_INTEGER4)
   if (ty == MPI_INTEGER4)       return 4; /* MPI2 spec */;
#  endif
#  if defined(MPI_INTEGER)
   if (ty == MPI_INTEGER)        return 4; /* MPI2 spec */;
#  endif
#  if defined(MPI_DOUBLE_PRECISION)
   if (ty == MPI_DOUBLE_PRECISION) return 8; /* MPI2 spec */;
#  endif

   /* new in MPI2: */
#  if defined(MPI_WCHAR)
   if (ty == MPI_WCHAR)              return 2; /* MPI2 spec */;
#  endif
#  if defined(MPI_SIGNED_CHAR)
   if (ty == MPI_SIGNED_CHAR)        return 1; /* MPI2 spec */;
#  endif
#  if defined(MPI_UNSIGNED_LONG_LONG)
   if (ty == MPI_UNSIGNED_LONG_LONG) return 8; /* MPI2 spec */;
#  endif
#  if defined(MPI_COMPLEX)
   if (ty == MPI_COMPLEX)            return 2 * 4; /* MPI2 spec */
#  endif
#  if defined(MPI_DOUBLE_COMPLEX)
   if (ty == MPI_DOUBLE_COMPLEX)     return 2 * 8; /* MPI2 spec */
#  endif
#  if defined(MPI_LOGICAL)
   if (ty == MPI_LOGICAL)            return 4; /* MPI2 spec */
#  endif
#  if defined(MPI_2INTEGER)
   if (ty == MPI_2INTEGER)      return 2 * 4; /* undocumented in MPI2 */
#  endif
#  if defined(MPI_2COMPLEX)
   if (ty == MPI_2COMPLEX)      return 2 * 8; /* undocumented in MPI2 */
#  endif
#  if defined(MPI_2DOUBLE_COMPLEX)
   /* 32: this is how openmpi-1.2.2 behaves on x86-linux, but I have
      really no idea if this is right. */
   if (ty == MPI_2DOUBLE_COMPLEX)   return 32; /* undocumented in MPI2 */
#  endif
#  if defined(MPI_2REAL)
   if (ty == MPI_2REAL)              return 2 * 4; /* undocumented in MPI2 */
#  endif
#  if defined(MPI_2DOUBLE_PRECISION)
   if (ty == MPI_2DOUBLE_PRECISION)  return 2 * 8; /* undocumented in MPI2 */
#  endif
#  if defined(MPI_CHARACTER)
   if (ty == MPI_CHARACTER)          return 1; /* MPI2 spec */
#  endif

   /* Note: the following are named structs, not named basic types,
      and so are not handled here:
         FLOAT_INT DOUBLE_INT LONG_INT 2INT SHORT_INT LONG_DOUBLE_INT
      My guess is they are probably for doing max-w-index style
      reductions, the INT carrying the index of the max/min and the
      other type its actual value.
   */
   return 0;
}


/* Find the size of long double image (not 'sizeof(long double)').
   See comments in sizeofOneNamedTy. 
*/
static long sizeof_long_double_image ( void )
{
   long i;
   unsigned char* p;
   static long cached_result = 0;

   /* Hopefully we have it already. */
   if (cached_result != 0) {
      assert(cached_result == 10 || cached_result == 16 || cached_result == 8);
      return cached_result;
   }

   /* No?  Then we'll have to compute it.  This isn't thread-safe but
      it doesn't really matter since all races to compute it should
      produce the same answer. */
   p = malloc(64);
   assert(p);
   for (i = 0; i < 64; i++)
      p[i] = 0x55;

   /* Write a value which isn't known at compile time and therefore
      must come out of a register.  If we just store a constant here,
      some compilers write more data than a store from a machine
      register would.  Therefore we have to force a store from a
      machine register by storing a value which isn't known at compile
      time.  Since getpid() will return a value < 1 million, turn it
      into a zero by dividing by 1e+30. */
   *(long double*)(&p[16]) = (long double)(1.0e-30 * (double)getpid());

   for (i = 0; i < 16; i++) {
      assert(p[i] == 0x55);
      assert(p[i+48] == 0x55);
   }
   for (i = 16; i <= 48; i++) {
      if (p[i] == 0x55)
         break;
   }

   assert(i < 48);
   assert(i > 16);
   free(p);
   cached_result = i - 16;

   if (0) 
      printf("sizeof_long_double_image: computed %d\n", (int)cached_result);

   assert(cached_result == 10 || cached_result == 16 || cached_result == 8);
   return cached_result;
}


/*------------------------------------------------------------*/
/*--- Unpicking datatypes                                  ---*/
/*------------------------------------------------------------*/

static __inline__
void walk_type_array ( void(*f)(void*,long), char* base, 
                       MPI_Datatype ty, long count );


/* Walk over all fragments of the object of type 'ty' with base
   address 'base', and apply 'f' to the start/length of each
   contiguous fragment. */
static 
void walk_type ( void(*f)(void*,long), char* base, MPI_Datatype ty )
{
   int  r, n_ints, n_addrs, n_dtys, tycon;
   long ex, i;
   int*          ints  = NULL;
   MPI_Aint*     addrs = NULL;
   MPI_Datatype* dtys  = NULL;

   /* Stuff for limiting how much complaining text it spews out */
   static int complaints = 3;
   static int last_complained_about_tycon = -987654321; /* presumably bogus */

   if (0)
      printf("walk_type %p\n", (void*)(unsigned long)ty);

   r = PMPI_Type_get_envelope( ty, &n_ints, &n_addrs, &n_dtys, &tycon );
   assert(r == MPI_SUCCESS);

   /* Handle the base cases fast(er/ish). */
   if (tycon == MPI_COMBINER_NAMED) {
      long sz = sizeofOneNamedTy(ty);
      if (sz > 0) {
         f(base, sz);
         return;
      }
      /* Hmm.  Perhaps it's a named struct?  Unfortunately we can't
         take them to bits so we have to do a really ugly hack, which
         makes assumptions about how the MPI implementation has laid
         out these types.  At least Open MPI 1.0.1 appears to put
         the 'val' field first.  MPICH2 agrees.
      */
      if (ty == MPI_2INT) {
         typedef struct { int val; int loc; } Ty;
         f(base + offsetof(Ty,val), sizeof(int));
         f(base + offsetof(Ty,loc), sizeof(int));
         return;
      }
      if (ty == MPI_LONG_INT) {
         typedef struct { long val; int loc; } Ty;
         f(base + offsetof(Ty,val), sizeof(long));
         f(base + offsetof(Ty,loc), sizeof(int));
         return;
      }
      if (ty == MPI_DOUBLE_INT) {
         typedef struct { double val; int loc; } Ty;
         f(base + offsetof(Ty,val), sizeof(double));
         f(base + offsetof(Ty,loc), sizeof(int));
         return;
      }
      if (ty == MPI_SHORT_INT) {
         typedef struct { short val; int loc; } Ty;
         f(base + offsetof(Ty,val), sizeof(short));
         f(base + offsetof(Ty,loc), sizeof(int));
         return;
      }
      if (ty == MPI_FLOAT_INT) {
         typedef struct { float val; int loc; } Ty;
         f(base + offsetof(Ty,val), sizeof(float));
         f(base + offsetof(Ty,loc), sizeof(int));
         return;
      }
      if (ty == MPI_LONG_DOUBLE_INT) {
         typedef struct { long double val; int loc; } Ty;
         f(base + offsetof(Ty,val), sizeof_long_double_image());
         f(base + offsetof(Ty,loc), sizeof(int));
         return;
      }
      if (ty == MPI_LB || ty == MPI_UB)
         return; /* have zero size, so nothing needs to be done */
      goto unhandled;
      /*NOTREACHED*/
   }

   if (0) {
      ex = extentOfTy(ty);
      printf("tycon 0x%llx %d %d %d (ext %d)\n",
             (unsigned long long int)tycon, 
             n_ints, n_addrs, n_dtys, (int)ex );
   }

   /* Now safe to do MPI_Type_get_contents */
   assert(n_ints  >= 0);
   assert(n_addrs >= 0);
   assert(n_dtys  >= 0);

   if (n_ints  > 0) {
      ints = malloc(n_ints * sizeof(int));
      assert(ints);
   }
   if (n_addrs > 0) {
      addrs = malloc(n_addrs * sizeof(MPI_Aint));
      assert(addrs);
   }
   if (n_dtys  > 0) {
      dtys = malloc(n_dtys * sizeof(MPI_Datatype));
      assert(dtys);
   }

   r = PMPI_Type_get_contents( ty, n_ints, n_addrs, n_dtys,
                                   ints, addrs, dtys );
   assert(r == MPI_SUCCESS);

   switch (tycon) {

      case MPI_COMBINER_CONTIGUOUS:
         assert(n_ints == 1 && n_addrs == 0 && n_dtys == 1);
	 walk_type_array( f, base, dtys[0], ints[0] );
         maybeFreeTy( &dtys[0] );
         break;

      case MPI_COMBINER_VECTOR:
         assert(n_ints == 3 && n_addrs == 0 && n_dtys == 1);
         ex = extentOfTy(dtys[0]);
         if (0)
         printf("vector count %d x (bl %d stride %d)\n", 
                (int)ints[0], (int)ints[1], (int)ints[2]);
         for (i = 0; i < ints[0]; i++) {
            walk_type_array( f, base + i * ints[2]/*stride*/ * ex,
                                dtys[0], ints[1]/*blocklength*/ );
         }
         maybeFreeTy( &dtys[0] );
         break;

      case MPI_COMBINER_HVECTOR:
         assert(n_ints == 2 && n_addrs == 1 && n_dtys == 1);
         ex = extentOfTy(dtys[0]);
         if (0)
         printf("hvector count %d x (bl %d hstride %d)\n", 
                (int)ints[0], (int)ints[1], (int)addrs[0]);
         for (i = 0; i < ints[0]; i++) {
            walk_type_array( f, base + i * addrs[0]/*hstride*/,
                                dtys[0], ints[1]/*blocklength*/ );
         }
         maybeFreeTy( &dtys[0] );
         break;

      case MPI_COMBINER_INDEXED:
         assert(n_addrs == 0 && n_dtys == 1);
         assert(n_ints > 0);
         assert(n_ints == 2 * ints[0] + 1);
         ex = extentOfTy(dtys[0]);
         for (i = 0; i < ints[0]; i++) {
            if (0) 
            printf("indexed (elem %d) off %d copies %d\n",
                   (int)i, ints[i+1+ints[0]], ints[i+1] );
            walk_type_array( f, base + ex * ints[i+1+ints[0]], 
                                dtys[0], ints[i+1] );
         }
         maybeFreeTy( &dtys[0] );
         break;

      case MPI_COMBINER_HINDEXED:
         assert(n_ints > 0);
         assert(n_ints == ints[0] + 1);
         assert(n_addrs == ints[0] && n_dtys == 1);
         ex = extentOfTy(dtys[0]);
         for (i = 0; i < ints[0]; i++) {
            if (0) 
            printf("hindexed (elem %d) hoff %d copies %d\n",
                   (int)i, (int)addrs[i], ints[i+1] );
            walk_type_array( f, base + addrs[i], 
                                dtys[0], ints[i+1] );
         }
         maybeFreeTy( &dtys[0] );
         break;

      case MPI_COMBINER_STRUCT:
         assert(n_addrs == n_ints-1);
         assert(n_dtys  == n_ints-1);
         assert(n_ints > 0);
         assert(n_ints == ints[0] + 1);
	 for (i = 0; i < ints[0]; i++) {
            if (0)
            printf("struct (elem %d limit %d) hoff %d copies %d\n", 
                   (int)i, (int)ints[0], (int)addrs[i], (int)ints[i+1]);
            walk_type_array( f, base + addrs[i], dtys[i], (long)ints[i+1] );
            maybeFreeTy( &dtys[i] );
	 }
         break;

      default:
         goto unhandled;

   }

   /* normal exit */
   if (ints)  free(ints);
   if (addrs) free(addrs);
   if (dtys)  free(dtys);
   return;

  unhandled:
   /* Complain, but limit the amount of complaining that can happen to
      the first 3 different unhandled tycons that show up, so as to
      avoid swamping users with thousands of duplicate messages. */
   if (complaints > 0 && tycon != last_complained_about_tycon) {
      complaints--;
      last_complained_about_tycon = tycon;
      if (tycon == MPI_COMBINER_NAMED) {
         fprintf(stderr, "%s %5d: walk_type: unhandled base type 0x%lx ",
                         preamble, my_pid, (long)ty);
         showTy(stderr, ty);
         fprintf(stderr, "\n");
      } else {
         fprintf(stderr, "%s %5d: walk_type: unhandled combiner 0x%lx\n",
                         preamble, my_pid, (long)tycon);
      }
   }
   if (ints)  free(ints);
   if (addrs) free(addrs);
   if (dtys)  free(dtys);
   if (opt_missing >= 2)
      barf("walk_type: unhandled combiner, strict checking selected");
}


/* Same as walk_type but apply 'f' to every element in an array of
   'count' items starting at 'base'.  The only purpose of pushing this
   into a different routine is so it can attempt to optimise the case
   where the array elements are contiguous and packed together without
   holes. */
static __inline__
void walk_type_array ( void(*f)(void*,long), char* base, 
                       MPI_Datatype elemTy, long count )
{
   long i, ex;

   assert(sizeof(unsigned long) == sizeof(char*));

   /* First see if we can do this the fast way. */
   ex = sizeofOneNamedTy(elemTy);

   if ( /* ty is a primitive type with power-of-2 size */
        (ex == 8 || ex == 4 || ex == 2 || ex == 1)
        && /* base is suitably aligned for ty */
           ( ((unsigned long)base) & (ex-1)) == 0)  {

      /* We're sure it's contiguous, so just paint/check it in one
         go. */
     if (0) printf("walk_type_array fast %ld of size %ld\n", count, ex );
     f ( base, count * ex );

   } else {

      /* Bad news.  We have to futz with each element individually.
         This could be very expensive.

         Note: subtle.  If ty is LONG_DOUBLE then the extent will be
         12, so the following loop will jump along in steps of 12, but
         the size painted by walk_type will be 10 since it uses
         sizeofOneNamedTy to establish the size of base types.  Which
         is what we need to happen. */
      ex = extentOfTy(elemTy);
      if (0) printf("walk_type_array SLOW %ld of size %ld\n", count, ex );
      for (i = 0; i < count; i++)
         walk_type( f, base + i * ex, elemTy );

   }
}


/* Hook so it's visible from outside (can be handy to dlopen/dlsym
   it) */
void mpiwrap_walk_type_EXTERNALLY_VISIBLE
    ( void(*f)(void*,long), char* base, MPI_Datatype ty )
{
   walk_type(f, base, ty);
}


/*------------------------------------------------------------*/
/*--- Address-range helpers                                ---*/
/*------------------------------------------------------------*/

/* ----------------
   Do corresponding checks on memory areas defined using a 
   straightforward (start, length) description.
   ----------------
*/

static __inline__
void check_mem_is_defined_untyped ( void* buffer, long nbytes )
{
   if (nbytes > 0) {
      VALGRIND_CHECK_MEM_IS_DEFINED(buffer, nbytes);
   }
}

static __inline__
void check_mem_is_addressable_untyped ( void* buffer, long nbytes )
{
   if (nbytes > 0) {
      VALGRIND_CHECK_MEM_IS_ADDRESSABLE(buffer, nbytes);
   }
}

static __inline__
void make_mem_defined_if_addressable_untyped ( void* buffer, long nbytes )
{
   if (nbytes > 0) {
      VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(buffer, nbytes);
   }
}

static __inline__
void make_mem_defined_if_addressable_if_success_untyped ( int err, 
                                       void* buffer, long nbytes )
{
   if (err == MPI_SUCCESS && nbytes > 0) {
      VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(buffer, nbytes);
   }
}


/* ----------------
   Do checks on memory areas defined using the MPI (buffer, count,
   type) convention.
   ----------------
*/

/* Check that the specified area is both addressible and contains
   initialised data, and cause V to complain if not. */

static __inline__
void check_mem_is_defined ( char* buffer, long count, MPI_Datatype datatype )
{
   walk_type_array( check_mem_is_defined_untyped, buffer, datatype, count );
}


/* Check that the specified area is addressible, and cause V to
   complain if not. Doesn't matter whether the data there is
   initialised or not. */

static __inline__
void check_mem_is_addressable ( void *buffer, long count, MPI_Datatype datatype )
{
   walk_type_array( check_mem_is_addressable_untyped, buffer, datatype, count );
}


/* Set the specified area to 'defined for each byte which is
   addressible' state. */

static __inline__
void make_mem_defined_if_addressable ( void *buffer, int count, MPI_Datatype datatype )
{
   walk_type_array( make_mem_defined_if_addressable_untyped,
                    buffer, datatype, count );
}

static __inline__
void 
make_mem_defined_if_addressable_if_success ( int err, void *buffer, int count, 
                                             MPI_Datatype datatype )
{
   if (err == MPI_SUCCESS)
      make_mem_defined_if_addressable(buffer, count, datatype);
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The wrappers proper.   They are listed in the order  ---*/
/*--- in which they appear in "MPI: A Message-Passing      ---*/
/*--- Interface Standard, MPIF, Nov 15 2003" (the MPI 1.1  ---*/
/*--- spec.  All unimplemented wrappers are listed at the  ---*/
/*--- end of the file.  The list of function names is      ---*/
/*--- taken from the headers of Open MPI svn r9191.        ---*/
/*--- Hopefully it is a complete list of all the MPI 2     ---*/
/*--- functions.                                           ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Handy abbreviation */
#define WRAPPER_FOR(name) I_WRAP_FNNAME_U(name)

/* Generates (conceptually) a wrapper which does nothing.  In
   fact just generate no wrapper at all. */
#define HAS_NO_WRAPPER(basename) /* */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 3.2, Blocking Send and Receive Operations        ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- {,B,S,R}Send --- */
/* pre: rd: (buf,count,datatype) */
static
int generic_Send(void *buf, int count, MPI_Datatype datatype, 
                            int dest, int tag, MPI_Comm comm)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("{,B,S,R}Send");
   check_mem_is_defined(buf, count, datatype);
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_6W(err, fn, buf,count,datatype,dest,tag,comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("{,B,S,R}Send", err);
   return err;
}
int WRAPPER_FOR(PMPI_Send)(void *buf, int count, MPI_Datatype datatype, 
                           int dest, int tag, MPI_Comm comm) {
   return generic_Send(buf,count,datatype, dest,tag,comm);
}
int WRAPPER_FOR(PMPI_Bsend)(void *buf, int count, MPI_Datatype datatype, 
                            int dest, int tag, MPI_Comm comm) {
   return generic_Send(buf,count,datatype, dest,tag,comm);
}
int WRAPPER_FOR(PMPI_Ssend)(void *buf, int count, MPI_Datatype datatype, 
                            int dest, int tag, MPI_Comm comm) {
   return generic_Send(buf,count,datatype, dest,tag,comm);
}
int WRAPPER_FOR(PMPI_Rsend)(void *buf, int count, MPI_Datatype datatype, 
                            int dest, int tag, MPI_Comm comm) {
   return generic_Send(buf,count,datatype, dest,tag,comm);
}

/* --- Recv --- */
/* pre:  must be writable: (buf,count,datatype)
         must be writable: status
   post: make readable: (buf,recv_count,datatype)
         where recv_count is determined from *status
*/
int WRAPPER_FOR(PMPI_Recv)(void *buf, int count, MPI_Datatype datatype, 
                           int source, int tag, 
                           MPI_Comm comm, MPI_Status *status)
{
   OrigFn     fn;
   int        err, recv_count = 0;
   MPI_Status fake_status;
   VALGRIND_GET_ORIG_FN(fn);
   before("Recv");
   if (isMSI(status))
      status = &fake_status;
   check_mem_is_addressable(buf, count, datatype);
   check_mem_is_addressable_untyped(status, sizeof(*status));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_7W(err, fn, buf,count,datatype,source,tag,comm,status);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, status, sizeof(*status));
   if (err == MPI_SUCCESS && count_from_Status(&recv_count,datatype,status)) {
      make_mem_defined_if_addressable(buf, recv_count, datatype);
   }
   after("Recv", err);
   return err;
}

/* --- Get_count --- */
/* pre:  must be readable: *status
   post: make defined: *count -- don't bother, libmpi will surely do this
*/
int WRAPPER_FOR(PMPI_Get_count)(MPI_Status* status, 
                                MPI_Datatype ty, int* count )
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Get_count");
   check_mem_is_defined_untyped(status, sizeof(*status));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWW(err, fn, status,ty,count);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Get_count", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 3.7, Nonblocking communication                   ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Maintain a table that makes it possible for the wrappers to
   complete MPI_Irecv successfully.

   The issue is that MPI_Irecv states the recv buffer and returns
   immediately, giving a handle (MPI_Request) for the transaction.
   Later the user will have to poll for completion with MPI_Wait etc,
   and at that point these wrappers have to paint the recv buffer.
   But the recv buffer details are not presented to MPI_Wait - only
   the handle is.  We therefore have to use a shadow table
   (sReqs{,_size,_used,_lock}) which associates uncompleted
   MPI_Requests with the corresponding buffer address/count/type.

   Only read requests are placed in the table, since there is no need
   to do any buffer painting following completion of an Isend - all
   the checks for that are done at the time Isend is called.

   Care has to be take to remove completed requests from the table.

   Access to the table is guarded by sReqs_lock so as to make it
   thread-safe.
*/

typedef
   struct {
      Bool         inUse;
      MPI_Request  key;
      void*        buf;
      int          count;
      MPI_Datatype datatype;
   }
   ShadowRequest;

static ShadowRequest*  sReqs      = NULL;
static int             sReqs_size = 0;
static int             sReqs_used = 0;
static pthread_mutex_t sReqs_lock = PTHREAD_MUTEX_INITIALIZER;

#define LOCK_SREQS                                  \
  do { int pr = pthread_mutex_lock(&sReqs_lock);    \
       assert(pr == 0);                             \
  } while (0)

#define UNLOCK_SREQS                                \
  do { int pr = pthread_mutex_unlock(&sReqs_lock);  \
       assert(pr == 0);                             \
  } while (0)


/* Ensure the sReqs expandable array has at least one free slot, by
   copying it into a larger one if necessary.  NOTE: sReqs_lock is
   held throughout this procedure.*/
static void ensure_sReq_space ( void )
{
   int            i;
   ShadowRequest* sReqs2;
   if (sReqs_used == sReqs_size) {
      sReqs_size = sReqs_size==0 ? 2 : 2*sReqs_size;
      sReqs2 = malloc( sReqs_size * sizeof(ShadowRequest) );
      if (sReqs2 == NULL) {
         UNLOCK_SREQS;
         barf("add_shadow_Request: malloc failed.\n");
      }
      for (i = 0; i < sReqs_used; i++)
         sReqs2[i] = sReqs[i];
      if (sReqs)
         free(sReqs);
      sReqs = sReqs2;
   }
   assert(sReqs_used < sReqs_size);
}


/* Find shadow info for 'request', or NULL if none. */

static 
ShadowRequest* find_shadow_Request ( MPI_Request request )
{
   ShadowRequest* ret = NULL;
   int i;
   LOCK_SREQS;
   for (i = 0; i < sReqs_used; i++) {
      if (sReqs[i].inUse && eq_MPI_Request(sReqs[i].key,request)) {
         ret = &sReqs[i];
         break;
      }
   }
   UNLOCK_SREQS;
   return ret;
}


/* Delete shadow info for 'request', if any. */

static void delete_shadow_Request ( MPI_Request request )
{
   int i;
   LOCK_SREQS;
   for (i = 0; i < sReqs_used; i++) {
      if (sReqs[i].inUse && eq_MPI_Request(sReqs[i].key,request)) {
         sReqs[i].inUse = False;
         break;
      }
   }
   UNLOCK_SREQS;
}


/* Add a shadow for 'request', overwriting any old binding for it. */

static 
void add_shadow_Request( MPI_Request request, 
                         void* buf, int count, 
                         MPI_Datatype datatype )
{
   int i, ix = -1;
   LOCK_SREQS;
   assert(sReqs_used >= 0);
   assert(sReqs_size >= 0);
   assert(sReqs_used <= sReqs_size);
   if (sReqs == NULL) assert(sReqs_size == 0);

   /* First of all see if we already have a binding for this key; if
      so just replace it, and have done. */
   for (i = 0; i < sReqs_used; i++) {
      if (sReqs[i].inUse && eq_MPI_Request(sReqs[i].key,request)) {
         ix = i;
         break;
      }
   }

   if (ix < 0) {
      /* Ok, we don't have it, so will have to add it.  First search
         to see if there is an existing empty slot. */
      for (i = 0; i < sReqs_used; i++) {
         if (!sReqs[i].inUse) {
            ix = i;
            break;
         }
      }
   }

   /* No empty slots.  Allocate a new one. */
   if (ix < 0) {
      ensure_sReq_space();
      assert(sReqs_used < sReqs_size);
      ix = sReqs_used;
      sReqs_used++;
   }

   assert(ix >= 0 && ix < sReqs_used);
   assert(sReqs_used <= sReqs_size);

   sReqs[ix].inUse    = True;
   sReqs[ix].key      = request;
   sReqs[ix].buf      = buf;
   sReqs[ix].count    = count;
   sReqs[ix].datatype = datatype;

   UNLOCK_SREQS;
   if (opt_verbosity > 1)
      fprintf(stderr, "%s %5d: sReq+ 0x%lx -> b/c/d %p/%d/0x%lx [slot %d]\n",
                      preamble, my_pid, (unsigned long)request, 
                                buf, count, (long)datatype, ix);
}

static 
MPI_Request* clone_Request_array ( int count, MPI_Request* orig )
{
   MPI_Request* copy;
   int i;
   LOCK_SREQS;
   if (count < 0) 
      count = 0; /* Hmm.  Call Mulder and Scully. */
   copy = malloc( count * sizeof(MPI_Request) );
   if (copy == NULL && count > 0) {
      UNLOCK_SREQS;
      barf("clone_Request_array: malloc failed");
   }
   for (i = 0; i < count; i++)
      copy[i] = orig[i];
   UNLOCK_SREQS;
   return copy;
}

#undef LOCK_SREQS
#undef UNLOCK_SREQS


static void maybe_complete ( Bool         error_in_status,
                             MPI_Request  request_before,
                             MPI_Request  request_after,
                             MPI_Status*  status )
{
   int recv_count = 0;
   ShadowRequest* shadow;
   /* How do we know if this is an Irecv request that has now
      finished successfully? 
      
      request_before isn't MPI_REQUEST_NULL
      and request_before is found in the shadow table
      and request_after *is* MPI_REQUEST_NULL
      and (if error_in_status then status.MPI_ERROR is MPI_SUCCESS)

      (when error_in_status == False, then we expect not to get
      called at all if there was an error.)
   */
   if (request_before != MPI_REQUEST_NULL
       && request_after == MPI_REQUEST_NULL
       && (error_in_status ? status->MPI_ERROR == MPI_SUCCESS : True)
       && ( (shadow=find_shadow_Request(request_before)) != NULL) ) {
      /* The Irecv detailed in 'shadow' completed.  Paint the result
         buffer, and delete the entry. */
      if (count_from_Status(&recv_count, shadow->datatype, status)) {
         make_mem_defined_if_addressable(shadow->buf, recv_count, shadow->datatype);
         if (opt_verbosity > 1)
            fprintf(stderr, "%s %5d: sReq- %p (completed)\n", 
                            preamble, my_pid, request_before);
      }
      delete_shadow_Request(request_before);
   }
}


/* --- Isend --- */
/* rd: (buf,count,datatype) */
/* wr: *request */
static __inline__
int generic_Isend(void *buf, int count, MPI_Datatype datatype, 
                             int dest, int tag, MPI_Comm comm, 
                             MPI_Request* request)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("{,B,S,R}Isend");
   check_mem_is_defined(buf, count, datatype);
   check_mem_is_addressable_untyped(request, sizeof(*request));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_7W(err, fn, buf,count,datatype,dest,tag,comm,request);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, request, sizeof(*request));
   after("{,B,S,R}Isend", err);
   return err;
}
int WRAPPER_FOR(PMPI_Isend)(void *buf, int count, MPI_Datatype datatype, 
                            int dest, int tag, MPI_Comm comm, 
                            MPI_Request* request) {
   return generic_Isend(buf,count,datatype, dest,tag,comm, request);
}
int WRAPPER_FOR(PMPI_Ibsend)(void *buf, int count, MPI_Datatype datatype, 
                             int dest, int tag, MPI_Comm comm, 
                             MPI_Request* request) {
   return generic_Isend(buf,count,datatype, dest,tag,comm, request);
}
int WRAPPER_FOR(PMPI_Issend)(void *buf, int count, MPI_Datatype datatype, 
                             int dest, int tag, MPI_Comm comm, 
                             MPI_Request* request) {
   return generic_Isend(buf,count,datatype, dest,tag,comm, request);
}
int WRAPPER_FOR(PMPI_Irsend)(void *buf, int count, MPI_Datatype datatype, 
                             int dest, int tag, MPI_Comm comm, 
                             MPI_Request* request) {
   return generic_Isend(buf,count,datatype, dest,tag,comm, request);
}


/* --- Irecv --- */
/* pre:  must be writable: (buf,count,datatype), *request
   post: make readable *request
         add a request->(buf,count,ty) binding to the 
         shadow request table. 
*/
int WRAPPER_FOR(PMPI_Irecv)( void* buf, int count, MPI_Datatype datatype, 
                             int source, int tag, MPI_Comm comm, 
                             MPI_Request* request )
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Irecv");
   check_mem_is_addressable(buf, count, datatype);
   check_mem_is_addressable_untyped(request, sizeof(*request));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_7W(err, fn, buf,count,datatype,source,tag,comm,request);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS) {
      make_mem_defined_if_addressable_untyped(request, sizeof(*request));
      add_shadow_Request( *request, buf,count,datatype );
   }
   after("Irecv", err);
   return err;
}

/* --- Wait --- */
/* The MPI1 spec (imprecisely) defines 3 request states:
   - "null"     if the request is MPI_REQUEST_NULL
   - "inactive" if not "null" and not associated with ongoing comms
   - "active"   if not "null" and is associated with ongoing comms
*/
int WRAPPER_FOR(PMPI_Wait)( MPI_Request* request,
                            MPI_Status* status )
{
   MPI_Request  request_before;
   MPI_Status   fake_status;
   OrigFn       fn;
   int          err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Wait");
   if (isMSI(status))
      status = &fake_status;
   check_mem_is_addressable_untyped(status, sizeof(MPI_Status));
   check_mem_is_defined_untyped(request, sizeof(MPI_Request));
   request_before = *request;
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WW(err, fn, request,status);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS) {
      maybe_complete(False/*err in status?*/, 
                     request_before, *request, status);
      make_mem_defined_if_addressable_untyped(status, sizeof(MPI_Status));
   }
   after("Wait", err);
   return err;
}

/* --- Waitany --- */
int WRAPPER_FOR(PMPI_Waitany)( int count,
                               MPI_Request* requests,
                               int* index,
                               MPI_Status* status )
{
   MPI_Request* requests_before = NULL;
   MPI_Status   fake_status;
   OrigFn       fn;
   int          err, i;
   VALGRIND_GET_ORIG_FN(fn);
   before("Waitany");
   if (isMSI(status))
      status = &fake_status;
   if (0) fprintf(stderr, "Waitany: %d\n", count);
   check_mem_is_addressable_untyped(index, sizeof(int));
   check_mem_is_addressable_untyped(status, sizeof(MPI_Status));
   for (i = 0; i < count; i++) {
      check_mem_is_defined_untyped(&requests[i], sizeof(MPI_Request));
   }
   requests_before = clone_Request_array( count, requests );
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWWW(err, fn, count,requests,index,status);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS && *index >= 0 && *index < count) {
      maybe_complete(False/*err in status?*/, 
                     requests_before[*index], requests[*index], status);
      make_mem_defined_if_addressable_untyped(status, sizeof(MPI_Status));
   }
   if (requests_before)
      free(requests_before);
   after("Waitany", err);
   return err;
}

/* --- Waitall --- */
int WRAPPER_FOR(PMPI_Waitall)( int count, 
                               MPI_Request* requests,
                               MPI_Status* statuses )
{
   MPI_Request* requests_before = NULL;
   OrigFn       fn;
   int          err, i;
   Bool         free_sta = False;
   VALGRIND_GET_ORIG_FN(fn);
   before("Waitall");
   if (0) fprintf(stderr, "Waitall: %d\n", count);
   if (isMSI(statuses)) {
      free_sta = True;
      statuses = malloc( (count < 0 ? 0 : count) * sizeof(MPI_Status) );
   }
   for (i = 0; i < count; i++) {
      check_mem_is_addressable_untyped(&statuses[i], sizeof(MPI_Status));
      check_mem_is_defined_untyped(&requests[i], sizeof(MPI_Request));
   }
   requests_before = clone_Request_array( count, requests );
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWW(err, fn, count,requests,statuses);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS /*complete success*/
       || err == MPI_ERR_IN_STATUS /* partial success */) {
      Bool e_i_s = err == MPI_ERR_IN_STATUS;
      for (i = 0; i < count; i++) {
         maybe_complete(e_i_s, requests_before[i], requests[i], 
                               &statuses[i]);
         make_mem_defined_if_addressable_untyped(&statuses[i],
                                                 sizeof(MPI_Status));
      }
   }
   if (requests_before)
      free(requests_before);
   if (free_sta)
      free(statuses);
   after("Waitall", err);
   return err;
}

/* --- Test --- */
/* nonblocking version of Wait */
int WRAPPER_FOR(PMPI_Test)( MPI_Request* request, int* flag, 
                            MPI_Status* status )
{
   MPI_Request  request_before;
   MPI_Status   fake_status;
   OrigFn       fn;
   int          err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Test");
   if (isMSI(status))
      status = &fake_status;
   check_mem_is_addressable_untyped(status, sizeof(MPI_Status));
   check_mem_is_addressable_untyped(flag, sizeof(int));
   check_mem_is_defined_untyped(request, sizeof(MPI_Request));
   request_before = *request;
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWW(err, fn, request,flag,status);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS && *flag) {
      maybe_complete(False/*err in status?*/, 
                     request_before, *request, status);
      make_mem_defined_if_addressable_untyped(status, sizeof(MPI_Status));
   }
   after("Test", err);
   return err;
}

/* --- Testall --- */
/* nonblocking version of Waitall */
int WRAPPER_FOR(PMPI_Testall)( int count, MPI_Request* requests,
                               int* flag, MPI_Status* statuses )
{
   MPI_Request* requests_before = NULL;
   OrigFn       fn;
   int          err, i;
   Bool         free_sta = False;
   VALGRIND_GET_ORIG_FN(fn);
   before("Testall");
   if (0) fprintf(stderr, "Testall: %d\n", count);
   if (isMSI(statuses)) {
      free_sta = True;
      statuses = malloc( (count < 0 ? 0 : count) * sizeof(MPI_Status) );
   }
   check_mem_is_addressable_untyped(flag, sizeof(int));
   for (i = 0; i < count; i++) {
      check_mem_is_addressable_untyped(&statuses[i], sizeof(MPI_Status));
      check_mem_is_defined_untyped(&requests[i], sizeof(MPI_Request));
   }
   requests_before = clone_Request_array( count, requests );
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWWW(err, fn, count,requests,flag,statuses);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   /* Urk.  Is the following "if (...)" really right?  I don't know. */
   if (*flag
       && (err == MPI_SUCCESS /*complete success*/
           || err == MPI_ERR_IN_STATUS /* partial success */)) {
      Bool e_i_s = err == MPI_ERR_IN_STATUS;
      for (i = 0; i < count; i++) {
         maybe_complete(e_i_s, requests_before[i], requests[i], 
                               &statuses[i]);
         make_mem_defined_if_addressable_untyped(&statuses[i],
                                                 sizeof(MPI_Status));
      }
   }
   if (requests_before)
      free(requests_before);
   if (free_sta)
      free(statuses);
   after("Testall", err);
   return err;
}

/* --- Iprobe --- */
/* pre:  must-be-writable: *flag, *status */
/* post: make-readable *flag
         if *flag==True  make-defined *status */
int WRAPPER_FOR(PMPI_Iprobe)(int source, int tag, 
                             MPI_Comm comm, 
                             int* flag, MPI_Status* status)
{
   MPI_Status fake_status;
   OrigFn     fn;
   int        err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Iprobe");
   if (isMSI(status))
      status = &fake_status;
   check_mem_is_addressable_untyped(flag, sizeof(*flag));
   check_mem_is_addressable_untyped(status, sizeof(*status));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_5W(err, fn, source,tag,comm,flag,status);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS) {
      make_mem_defined_if_addressable_untyped(flag, sizeof(*flag));
      if (*flag)
         make_mem_defined_if_addressable_untyped(status, sizeof(*status));
   }
   after("Iprobe", err);
   return err;
}

/* --- Probe --- */
/* pre:  must-be-writable *status */
/* post: make-defined *status */
int WRAPPER_FOR(PMPI_Probe)(int source, int tag,
                            MPI_Comm comm, MPI_Status* status)
{
   MPI_Status fake_status;
   OrigFn     fn;
   int        err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Probe");
   if (isMSI(status))
      status = &fake_status;
   check_mem_is_addressable_untyped(status, sizeof(*status));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWWW(err, fn, source,tag,comm,status);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, status, sizeof(*status));
   after("Probe", err);
   return err;
}

/* --- Cancel --- */
/* Wrapping PMPI_Cancel is interesting only to the extent that we need
   to be able to detect when a request should be removed from our
   shadow table due to cancellation. */
int WRAPPER_FOR(PMPI_Cancel)(MPI_Request* request)
{
   OrigFn      fn;
   int         err;
   MPI_Request tmp;
   VALGRIND_GET_ORIG_FN(fn);
   before("Cancel");
   check_mem_is_addressable_untyped(request, sizeof(*request));
   tmp = *request;
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_W(err, fn, request);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (err == MPI_SUCCESS)
      delete_shadow_Request(tmp);
   after("Cancel", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 3.10, Send-receive                               ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Sendrecv --- */
/* pre: must be readable: (sendbuf,sendcount,sendtype)
        must be writable: (recvbuf,recvcount,recvtype)
   post: make readable: (recvbuf,recvcount_actual,datatype)
         where recvcount_actual is determined from *status
*/
int WRAPPER_FOR(PMPI_Sendrecv)(
       void *sendbuf, int sendcount, MPI_Datatype sendtype,
       int dest, int sendtag, 
       void *recvbuf, int recvcount, MPI_Datatype recvtype, 
       int source, int recvtag,
       MPI_Comm comm,  MPI_Status *status)
{
   MPI_Status fake_status;
   OrigFn     fn;
   int        err, recvcount_actual = 0;
   VALGRIND_GET_ORIG_FN(fn);
   before("Sendrecv");
   if (isMSI(status))
      status = &fake_status;
   check_mem_is_defined(sendbuf, sendcount, sendtype);
   check_mem_is_addressable(recvbuf, recvcount, recvtype);
   check_mem_is_addressable_untyped(status, sizeof(*status));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_12W(err, fn, sendbuf,sendcount,sendtype,dest,sendtag,
                          recvbuf,recvcount,recvtype,source,recvtag,
                          comm,status);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, status, sizeof(*status));
   if (err == MPI_SUCCESS 
       && count_from_Status(&recvcount_actual,recvtype,status)) {
      make_mem_defined_if_addressable(recvbuf, recvcount_actual, recvtype);
   }
   after("Sendrecv", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 3.12, Derived datatypes                          ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Address --- */
/* Does this have anything worth checking? */
HAS_NO_WRAPPER(Address)

/* --- MPI 2 stuff --- */
/* Type_extent, Type_get_contents and Type_get_envelope sometimes get
   used intensively by the type walker (walk_type).  There's no reason
   why they couldn't be properly wrapped if needed, but doing so slows
   everything down, so don't bother until needed. */
HAS_NO_WRAPPER(Type_extent)
HAS_NO_WRAPPER(Type_get_contents)
HAS_NO_WRAPPER(Type_get_envelope)

/* --- Type_commit --- */
int WRAPPER_FOR(PMPI_Type_commit)( MPI_Datatype* ty )
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Type_commit");
   check_mem_is_defined_untyped(ty, sizeof(*ty));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_W(err, fn, ty);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Type_commit", err);
   return err;
}

/* --- Type_free --- */
int WRAPPER_FOR(PMPI_Type_free)( MPI_Datatype* ty )
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Type_free");
   check_mem_is_defined_untyped(ty, sizeof(*ty));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_W(err, fn, ty);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Type_free", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 3.13, Pack and unpack                            ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Pack --- */
/* pre: must be readable: position
        must be readable: (inbuf,incount,datatype)
        must be writable: outbuf[0 .. outsize-1]
        must be writable: outbuf[*position .. 
                                 *position - 1 
                                 + however much space PMPI_Pack_size 
                                   says we will need]
   post: make readable: outbuf[old *position .. new *position]
*/
int WRAPPER_FOR(PMPI_Pack)( void* inbuf, int incount, MPI_Datatype datatype, 
                            void* outbuf, int outsize, 
                            int* position, MPI_Comm comm ) 
{
   OrigFn fn;
   int    err, szB = 0;
   int    position_ORIG = *position;
   VALGRIND_GET_ORIG_FN(fn);
   before("Pack");
   /* stay sane */
   check_mem_is_defined_untyped(position, sizeof(*position));
   /* check input */
   check_mem_is_defined(inbuf, incount, datatype);
   /* check output area's stated bounds make sense */
   check_mem_is_addressable_untyped(outbuf, outsize);
   /* check output area's actual used size properly */
   err = PMPI_Pack_size( incount, datatype, comm, &szB );
   if (err == MPI_SUCCESS && szB > 0) {
      check_mem_is_addressable_untyped( 
         ((char*)outbuf) + position_ORIG, szB
      );
   }

   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_7W(err, fn, inbuf,incount,datatype, outbuf,outsize,position, comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;

   if (err == MPI_SUCCESS && (*position) > position_ORIG) {
      /* paint output */
      make_mem_defined_if_addressable_untyped( 
         ((char*)outbuf) + position_ORIG, *position - position_ORIG
      );
   }
   after("Pack", err);
   return err;
}

/* --- Unpack --- */
/* pre: must be readable: position
        must be writable: (outbuf,outcount,datatype)
        must be writable: outbuf[0 .. outsize-1]
        must be writable: outbuf[*position .. 
                                 *position - 1 
                                 + however much space PMPI_Pack_size 
                                   says we will need]
   post: make readable: (outbuf,outcount,datatype)
         and also do a readability check of
         inbuf[old *position .. new *position]
*/
int WRAPPER_FOR(PMPI_Unpack)( void* inbuf, int insize, int* position,
                              void* outbuf, int outcount, MPI_Datatype datatype, 
                              MPI_Comm comm )
{
   OrigFn fn;
   int    err, szB = 0;
   int    position_ORIG = *position;
   VALGRIND_GET_ORIG_FN(fn);
   before("Unpack");
   /* stay sane */
   check_mem_is_defined_untyped(position, sizeof(*position));
   /* check output area is accessible */
   check_mem_is_addressable(outbuf, outcount, datatype);
   /* check input area's stated bounds make sense */
   check_mem_is_addressable_untyped(inbuf, insize);
   /* check input area's actual used size properly */
   err = PMPI_Pack_size( outcount, datatype, comm, &szB );
   if (err == MPI_SUCCESS && szB > 0) {
      check_mem_is_addressable_untyped( 
         ((char*)inbuf) + position_ORIG, szB
      );
   }

   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_7W(err, fn, inbuf,insize,position, outbuf,outcount,datatype, comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;

   if (err == MPI_SUCCESS && (*position) > position_ORIG) {
      /* recheck input more carefully */
      check_mem_is_defined_untyped( 
         ((char*)inbuf) + position_ORIG, *position - position_ORIG
      );
      /* paint output */
      make_mem_defined_if_addressable( outbuf, outcount, datatype );
   }
   after("Unpack", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 4.4, Broadcast                                   ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Bcast --- */
/* pre:  must-be-readable (buffer,count,datatype) for rank==root
         must-be-writable (buffer,count,datatype) for rank!=root
   post: make-readable (buffer,count,datatype) for all

   Resulting behaviour is: if root sends uninitialised stuff, then
   V complains, but then all ranks, including itself, see the buffer
   as initialised after that.
*/
int WRAPPER_FOR(PMPI_Bcast)(void *buffer, int count, 
                            MPI_Datatype datatype,
                            int root, MPI_Comm comm)
{
   OrigFn fn;
   int    err;
   Bool  i_am_sender;
   VALGRIND_GET_ORIG_FN(fn);
   before("Bcast");
   i_am_sender = root == comm_rank(comm);
   if (i_am_sender) {
      check_mem_is_defined(buffer, count, datatype);
   } else {
      check_mem_is_addressable(buffer, count, datatype);
   }
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_5W(err, fn, buffer,count,datatype,root,comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success(err, buffer, count, datatype);
   after("Bcast", err);
   return err; 
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 4.5, Gather                                      ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Gather --- */
/* JRS 20060217: I don't really understand this.  Each process is
   going to send sendcount items of type sendtype to the root.  So
   the root is going to receive comm_size*sendcount items of type
   sendtype (right?)  So why specify recvcount and recvtype?

   Anyway, assuming the MPI Spec is correct (seems likely :-) we have:

   pre:  (all)        must be readable: (sendbuf,sendcount,sendtype)
         (root only): must be writable: (recvbuf,recvcount * comm_size,recvtype)
   post: (root only): make readable: (recvbuf,recvcount * comm_size,recvtype)
*/
int WRAPPER_FOR(PMPI_Gather)(
       void *sendbuf, int sendcount, MPI_Datatype sendtype,
       void *recvbuf, int recvcount, MPI_Datatype recvtype,
       int root, MPI_Comm comm)
{
   OrigFn fn;
   int    err, me, sz;
   VALGRIND_GET_ORIG_FN(fn);
   before("Gather");
   me = comm_rank(comm);
   sz = comm_size(comm);
   check_mem_is_defined(sendbuf, sendcount, sendtype);
   if (me == root)
      check_mem_is_addressable(recvbuf, recvcount * sz, recvtype);
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_8W(err, fn, sendbuf,sendcount,sendtype,
                         recvbuf,recvcount,recvtype,
                         root,comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (me == root)
      make_mem_defined_if_addressable_if_success(err, recvbuf, recvcount * sz, recvtype);
   after("Gather", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 4.6, Scatter                                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* pre:  (root only): must be readable: (sendbuf,sendcount * comm_size,sendtype)
         (all):       must be writable: (recvbuf,recvbuf,recvtype)
   post: (all):       make defined: (recvbuf,recvbuf,recvtype)
*/
int WRAPPER_FOR(PMPI_Scatter)(
       void* sendbuf, int sendcount, MPI_Datatype sendtype,
       void* recvbuf, int recvcount, MPI_Datatype recvtype,
       int root, MPI_Comm comm)
{
   OrigFn fn;
   int    err, me, sz;
   VALGRIND_GET_ORIG_FN(fn);
   before("Scatter");
   me = comm_rank(comm);
   sz = comm_size(comm);
   check_mem_is_addressable(recvbuf, recvcount, recvtype);
   if (me == root)
      check_mem_is_defined(sendbuf, sendcount * sz, sendtype);
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_8W(err, fn, sendbuf,sendcount,sendtype,
                         recvbuf,recvcount,recvtype,
                         root,comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success(err, recvbuf, recvcount, recvtype);
   after("Scatter", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 4.8, All-to-All Scatter/Gather                   ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* pre:  (all) must be readable: (sendbuf,sendcount * comm_size,sendtype)
         (all) must be writable: (recvbuf,recvcount * comm_size,recvtype)
   post: (all) make defined:     (recvbuf,recvcount * comm_size,recvtype)
*/
int WRAPPER_FOR(PMPI_Alltoall)(
       void* sendbuf, int sendcount, MPI_Datatype sendtype,
       void* recvbuf, int recvcount, MPI_Datatype recvtype,
       MPI_Comm comm)
{
   OrigFn fn;
   int    err, sz;
   VALGRIND_GET_ORIG_FN(fn);
   before("Alltoall");
   sz = comm_size(comm);
   check_mem_is_defined(sendbuf, sendcount * sz, sendtype);
   check_mem_is_addressable(recvbuf, recvcount * sz, recvtype);
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_7W(err, fn, sendbuf,sendcount,sendtype,
                         recvbuf,recvcount,recvtype,
                         comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success(err, recvbuf, recvcount * sz, recvtype);
   after("Alltoall", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 4.9, Global Reduction Operations                 ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Reduce --- */
/* rd: (sendbuf,count,datatype) for all
   wr: (recvbuf,count,datatype) but only for rank == root
*/
int WRAPPER_FOR(PMPI_Reduce)(void *sendbuf, void *recvbuf, 
                             int count,
                             MPI_Datatype datatype, MPI_Op op, 
                             int root, MPI_Comm comm)
{
   OrigFn fn;
   int    err;
   Bool  i_am_root;
   VALGRIND_GET_ORIG_FN(fn);
   before("Reduce");
   i_am_root = root == comm_rank(comm);
   check_mem_is_defined(sendbuf, count, datatype);
   if (i_am_root)
      check_mem_is_addressable(recvbuf, count, datatype);
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_7W(err, fn, sendbuf,recvbuf,count,datatype,op,root,comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   if (i_am_root)
      make_mem_defined_if_addressable_if_success(err, recvbuf, count, datatype);
   after("Reduce", err);
   return err;
}


/* --- Allreduce --- */
/* rd: (sendbuf,count,datatype) for all
   wr: (recvbuf,count,datatype) for all
*/
int WRAPPER_FOR(PMPI_Allreduce)(void *sendbuf, void *recvbuf, 
                                int count,
                                MPI_Datatype datatype, MPI_Op op, 
                                MPI_Comm comm)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Allreduce");
   check_mem_is_defined(sendbuf, count, datatype);
   check_mem_is_addressable(recvbuf, count, datatype);
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_6W(err, fn, sendbuf,recvbuf,count,datatype,op,comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success(err, recvbuf, count, datatype);
   after("Allreduce", err);
   return err;
}


/* --- Op_create --- */
/* This is a bit dubious.  I suppose it takes 'function' and 
   writes something at *op, but who knows what an MPI_Op is? 
   Can we safely do 'sizeof' on it? */
int WRAPPER_FOR(PMPI_Op_create)( MPI_User_function* function,
                                 int commute, 
                                 MPI_Op* op )
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Op_create");
   check_mem_is_addressable_untyped(op, sizeof(*op));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWW(err, fn, function,commute,op);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, op, sizeof(*op));
   after("Op_create", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 5.4, Communicator management                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Hardly seems worth wrapping Comm_rank and Comm_size, but
   since it's done now .. */

/* --- Comm_create --- */
/* Let normal memcheck tracking handle this. */
int WRAPPER_FOR(PMPI_Comm_create)(MPI_Comm comm, MPI_Group group,
                                  MPI_Comm* newcomm)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Comm_create");
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWW(err, fn, comm,group,newcomm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Comm_create", err);
   return err;
}

/* --- Comm_dup --- */
/* Let normal memcheck tracking handle this. */
int WRAPPER_FOR(PMPI_Comm_dup)(MPI_Comm comm, MPI_Comm* newcomm)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Comm_dup");
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WW(err, fn, comm,newcomm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Comm_dup", err);
   return err;
}

/* --- Comm_free --- */
/* Let normal memcheck tracking handle this. */
int WRAPPER_FOR(PMPI_Comm_free)(MPI_Comm* comm)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Comm_free");
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_W(err, fn, comm);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Comm_free", err);
   return err;
}

/* --- Comm_rank --- */
/* wr: (rank, sizeof(*rank)) */
int WRAPPER_FOR(PMPI_Comm_rank)(MPI_Comm comm, int *rank)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Comm_rank");
   check_mem_is_addressable_untyped(rank, sizeof(*rank));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WW(err, fn, comm,rank);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, rank, sizeof(*rank));
   after("Comm_rank", err);
   return err;
}

/* --- Comm_size --- */
/* wr: (size, sizeof(*size)) */
int WRAPPER_FOR(PMPI_Comm_size)(MPI_Comm comm, int *size)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Comm_size");
   check_mem_is_addressable_untyped(size, sizeof(*size));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WW(err, fn, comm,size);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, size, sizeof(*size));
   after("Comm_size", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 5.7, Caching                                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 7.3, Error codes and classes                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Error_string --- */
int WRAPPER_FOR(PMPI_Error_string)( int errorcode, char* string, 
                                    int* resultlen )
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Error_string");
   check_mem_is_addressable_untyped(resultlen, sizeof(int));
   check_mem_is_addressable_untyped(string, MPI_MAX_ERROR_STRING);
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WWW(err, fn, errorcode,string,resultlen);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   /* Don't bother to paint the result; we assume the real function
      will have filled it with defined characters :-) */
   after("Error_string", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Sec 7.5, Startup                                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Init --- */
/* rd: *argc, *argv[0 .. *argc-1] */
long WRAPPER_FOR(PMPI_Init)(int *argc, char ***argv)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Init");
   if (argc) {
      check_mem_is_defined_untyped(argc, sizeof(int));
   }
   if (argc && argv) {
      check_mem_is_defined_untyped(*argv, *argc * sizeof(char**));
   }
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_WW(err, fn, argc,argv);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Init", err);
   if (opt_initkludge)
      return (long)(void*)&mpiwrap_walk_type_EXTERNALLY_VISIBLE;
   else
      return (long)err;
}

/* --- Initialized --- */
int WRAPPER_FOR(PMPI_Initialized)(int* flag)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Initialized");
   check_mem_is_addressable_untyped(flag, sizeof(int));
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_W(err, fn, flag);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   make_mem_defined_if_addressable_if_success_untyped(err, flag, sizeof(int));
   after("Initialized", err);
   return err;
}

/* --- Finalize --- */
int WRAPPER_FOR(PMPI_Finalize)(void)
{
   OrigFn fn;
   int    err;
   VALGRIND_GET_ORIG_FN(fn);
   before("Finalize");
   if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;
   CALL_FN_W_v(err, fn);
   if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;
   after("Finalize", err);
   return err;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Default wrappers for all remaining functions         ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Boilerplate for default wrappers. */
#define DEFAULT_WRAPPER_PREAMBLE(basename)                        \
      OrigFn fn;                                                  \
      UWord  res;                                                 \
      static int complaints = 1;                                  \
      VALGRIND_GET_ORIG_FN(fn);                                   \
      before(#basename);                                          \
      if (opt_missing >= 2) {                                     \
         barf("no wrapper for PMPI_" #basename                    \
              ",\n\t\t\t     and you have "                       \
              "requested strict checking");                       \
      }                                                           \
      if (opt_missing == 1 && complaints > 0) {                   \
         fprintf(stderr, "%s %5d: warning: no wrapper "           \
                         "for PMPI_" #basename "\n",              \
                 preamble, my_pid);                               \
         complaints--;                                            \
      }                                                           \

#define DEFAULT_WRAPPER_W_0W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)( void )                     \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_v(res, fn);                                       \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_1W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)( UWord a1 )                 \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_W(res, fn, a1);                                   \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_2W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)( UWord a1, UWord a2 )       \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_WW(res, fn, a1,a2);                               \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_3W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3 )                            \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_WWW(res, fn, a1,a2,a3);                           \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_4W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4 )                  \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_WWWW(res, fn, a1,a2,a3,a4);                       \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_5W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4, UWord a5 )        \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_5W(res, fn, a1,a2,a3,a4,a5);                      \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_6W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4, UWord a5,         \
        UWord a6 )                                                \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_6W(res, fn, a1,a2,a3,a4,a5,a6);                   \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_7W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4, UWord a5,         \
        UWord a6, UWord a7 )                                      \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_7W(res, fn, a1,a2,a3,a4,a5,a6,a7);                \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_8W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4, UWord a5,         \
        UWord a6, UWord a7, UWord a8 )                            \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_8W(res, fn, a1,a2,a3,a4,a5,a6,a7,a8);             \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_9W(basename)                            \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4, UWord a5,         \
        UWord a6, UWord a7, UWord a8, UWord a9 )                  \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_9W(res, fn, a1,a2,a3,a4,a5,a6,a7,a8,a9);          \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_10W(basename)                           \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4, UWord a5,         \
        UWord a6, UWord a7, UWord a8, UWord a9, UWord a10 )       \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_10W(res, fn, a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);     \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }

#define DEFAULT_WRAPPER_W_12W(basename)                           \
   UWord WRAPPER_FOR(PMPI_##basename)                             \
      ( UWord a1, UWord a2, UWord a3, UWord a4, UWord a5,         \
        UWord a6, UWord a7, UWord a8, UWord a9, UWord a10,        \
        UWord a11, UWord a12 )                                    \
   {                                                              \
      DEFAULT_WRAPPER_PREAMBLE(basename)                          \
      if (cONFIG_DER) VALGRIND_DISABLE_ERROR_REPORTING;           \
      CALL_FN_W_12W(res, fn, a1,a2,a3,a4,a5,a6,                   \
                             a7,a8,a9,a10,a11,a12);               \
      if (cONFIG_DER) VALGRIND_ENABLE_ERROR_REPORTING;            \
      return res;                                                 \
   }


/* Canned summary of MPI-1.1/MPI-2 entry points, as derived from mpi.h
   from Open MPI svn rev 9191 (somewhere between Open MPI versions
   1.0.1 and 1.1.0). */

/* If a function is commented out in this list, it's because it has a
   proper wrapper written elsewhere (above here). */

DEFAULT_WRAPPER_W_2W(Abort)
DEFAULT_WRAPPER_W_9W(Accumulate)
DEFAULT_WRAPPER_W_1W(Add_error_class)
DEFAULT_WRAPPER_W_2W(Add_error_code)
DEFAULT_WRAPPER_W_2W(Add_error_string)
/* DEFAULT_WRAPPER_W_2W(Address) */
DEFAULT_WRAPPER_W_7W(Allgather)
DEFAULT_WRAPPER_W_8W(Allgatherv)
DEFAULT_WRAPPER_W_3W(Alloc_mem)
/* DEFAULT_WRAPPER_W_6W(Allreduce) */
/* DEFAULT_WRAPPER_W_7W(Alltoall) */
DEFAULT_WRAPPER_W_9W(Alltoallv)
DEFAULT_WRAPPER_W_9W(Alltoallw)
DEFAULT_WRAPPER_W_2W(Attr_delete)
DEFAULT_WRAPPER_W_4W(Attr_get)
DEFAULT_WRAPPER_W_3W(Attr_put)
DEFAULT_WRAPPER_W_1W(Barrier)
/* DEFAULT_WRAPPER_W_5W(Bcast) */
/* DEFAULT_WRAPPER_W_6W(Bsend) */
DEFAULT_WRAPPER_W_7W(Bsend_init)
DEFAULT_WRAPPER_W_2W(Buffer_attach)
DEFAULT_WRAPPER_W_2W(Buffer_detach)
/* DEFAULT_WRAPPER_W_1W(Cancel) */
DEFAULT_WRAPPER_W_4W(Cart_coords)
DEFAULT_WRAPPER_W_6W(Cart_create)
DEFAULT_WRAPPER_W_5W(Cart_get)
DEFAULT_WRAPPER_W_5W(Cart_map)
DEFAULT_WRAPPER_W_3W(Cart_rank)
DEFAULT_WRAPPER_W_5W(Cart_shift)
DEFAULT_WRAPPER_W_3W(Cart_sub)
DEFAULT_WRAPPER_W_2W(Cartdim_get)
DEFAULT_WRAPPER_W_1W(Close_port)
DEFAULT_WRAPPER_W_5W(Comm_accept)
DEFAULT_WRAPPER_W_1W(Comm_c2f)
DEFAULT_WRAPPER_W_2W(Comm_call_errhandler)
DEFAULT_WRAPPER_W_3W(Comm_compare)
DEFAULT_WRAPPER_W_5W(Comm_connect)
DEFAULT_WRAPPER_W_2W(Comm_create_errhandler)
DEFAULT_WRAPPER_W_4W(Comm_create_keyval)
/* DEFAULT_WRAPPER_W_3W(Comm_create) */
DEFAULT_WRAPPER_W_2W(Comm_delete_attr)
DEFAULT_WRAPPER_W_1W(Comm_disconnect)
/* DEFAULT_WRAPPER_W_2W(Comm_dup) */
DEFAULT_WRAPPER_W_1W(Comm_f2c)
DEFAULT_WRAPPER_W_1W(Comm_free_keyval)
/* DEFAULT_WRAPPER_W_1W(Comm_free) */
DEFAULT_WRAPPER_W_4W(Comm_get_attr)
DEFAULT_WRAPPER_W_2W(Comm_get_errhandler)
DEFAULT_WRAPPER_W_3W(Comm_get_name)
DEFAULT_WRAPPER_W_1W(Comm_get_parent)
DEFAULT_WRAPPER_W_2W(Comm_group)
DEFAULT_WRAPPER_W_2W(Comm_join)
/* DEFAULT_WRAPPER_W_2W(Comm_rank) */
DEFAULT_WRAPPER_W_2W(Comm_remote_group)
DEFAULT_WRAPPER_W_2W(Comm_remote_size)
DEFAULT_WRAPPER_W_3W(Comm_set_attr)
DEFAULT_WRAPPER_W_2W(Comm_set_errhandler)
DEFAULT_WRAPPER_W_2W(Comm_set_name)
/* DEFAULT_WRAPPER_W_2W(Comm_size) */
DEFAULT_WRAPPER_W_8W(Comm_spawn)
DEFAULT_WRAPPER_W_9W(Comm_spawn_multiple)
DEFAULT_WRAPPER_W_4W(Comm_split)
DEFAULT_WRAPPER_W_2W(Comm_test_inter)
DEFAULT_WRAPPER_W_3W(Dims_create)
DEFAULT_WRAPPER_W_1W(Errhandler_c2f)
DEFAULT_WRAPPER_W_2W(Errhandler_create)
DEFAULT_WRAPPER_W_1W(Errhandler_f2c)
DEFAULT_WRAPPER_W_1W(Errhandler_free)
DEFAULT_WRAPPER_W_2W(Errhandler_get)
DEFAULT_WRAPPER_W_2W(Errhandler_set)
DEFAULT_WRAPPER_W_2W(Error_class)
/* DEFAULT_WRAPPER_W_3W(Error_string) */
DEFAULT_WRAPPER_W_6W(Exscan)
DEFAULT_WRAPPER_W_1W(File_c2f)
DEFAULT_WRAPPER_W_1W(File_f2c)
DEFAULT_WRAPPER_W_2W(File_call_errhandler)
DEFAULT_WRAPPER_W_2W(File_create_errhandler)
DEFAULT_WRAPPER_W_2W(File_set_errhandler)
DEFAULT_WRAPPER_W_2W(File_get_errhandler)
DEFAULT_WRAPPER_W_5W(File_open)
DEFAULT_WRAPPER_W_1W(File_close)
DEFAULT_WRAPPER_W_2W(File_delete)
DEFAULT_WRAPPER_W_2W(File_set_size)
DEFAULT_WRAPPER_W_2W(File_preallocate)
DEFAULT_WRAPPER_W_2W(File_get_size)
DEFAULT_WRAPPER_W_2W(File_get_group)
DEFAULT_WRAPPER_W_2W(File_get_amode)
DEFAULT_WRAPPER_W_2W(File_set_info)
DEFAULT_WRAPPER_W_2W(File_get_info)
DEFAULT_WRAPPER_W_6W(File_set_view)
DEFAULT_WRAPPER_W_5W(File_get_view)
DEFAULT_WRAPPER_W_6W(File_read_at)
DEFAULT_WRAPPER_W_6W(File_read_at_all)
DEFAULT_WRAPPER_W_6W(File_write_at)
DEFAULT_WRAPPER_W_6W(File_write_at_all)
DEFAULT_WRAPPER_W_6W(File_iread_at)
DEFAULT_WRAPPER_W_6W(File_iwrite_at)
DEFAULT_WRAPPER_W_5W(File_read)
DEFAULT_WRAPPER_W_5W(File_read_all)
DEFAULT_WRAPPER_W_5W(File_write)
DEFAULT_WRAPPER_W_5W(File_write_all)
DEFAULT_WRAPPER_W_5W(File_iread)
DEFAULT_WRAPPER_W_5W(File_iwrite)
DEFAULT_WRAPPER_W_3W(File_seek)
DEFAULT_WRAPPER_W_2W(File_get_position)
DEFAULT_WRAPPER_W_3W(File_get_byte_offset)
DEFAULT_WRAPPER_W_5W(File_read_shared)
DEFAULT_WRAPPER_W_5W(File_write_shared)
DEFAULT_WRAPPER_W_5W(File_iread_shared)
DEFAULT_WRAPPER_W_5W(File_iwrite_shared)
DEFAULT_WRAPPER_W_5W(File_read_ordered)
DEFAULT_WRAPPER_W_5W(File_write_ordered)
DEFAULT_WRAPPER_W_3W(File_seek_shared)
DEFAULT_WRAPPER_W_2W(File_get_position_shared)
DEFAULT_WRAPPER_W_5W(File_read_at_all_begin)
DEFAULT_WRAPPER_W_3W(File_read_at_all_end)
DEFAULT_WRAPPER_W_5W(File_write_at_all_begin)
DEFAULT_WRAPPER_W_3W(File_write_at_all_end)
DEFAULT_WRAPPER_W_4W(File_read_all_begin)
DEFAULT_WRAPPER_W_3W(File_read_all_end)
DEFAULT_WRAPPER_W_4W(File_write_all_begin)
DEFAULT_WRAPPER_W_3W(File_write_all_end)
DEFAULT_WRAPPER_W_4W(File_read_ordered_begin)
DEFAULT_WRAPPER_W_3W(File_read_ordered_end)
DEFAULT_WRAPPER_W_4W(File_write_ordered_begin)
DEFAULT_WRAPPER_W_3W(File_write_ordered_end)
DEFAULT_WRAPPER_W_3W(File_get_type_extent)
DEFAULT_WRAPPER_W_2W(File_set_atomicity)
DEFAULT_WRAPPER_W_2W(File_get_atomicity)
DEFAULT_WRAPPER_W_1W(File_sync)
/* DEFAULT_WRAPPER_W_0W(Finalize) */
DEFAULT_WRAPPER_W_1W(Finalized)
DEFAULT_WRAPPER_W_1W(Free_mem)
/* DEFAULT_WRAPPER_W_8W(Gather) */
DEFAULT_WRAPPER_W_9W(Gatherv)
DEFAULT_WRAPPER_W_2W(Get_address)
/* DEFAULT_WRAPPER_W_3W(Get_count) */
DEFAULT_WRAPPER_W_3W(Get_elements)
DEFAULT_WRAPPER_W_8W(Get)
DEFAULT_WRAPPER_W_2W(Get_processor_name)
DEFAULT_WRAPPER_W_2W(Get_version)
DEFAULT_WRAPPER_W_6W(Graph_create)
DEFAULT_WRAPPER_W_5W(Graph_get)
DEFAULT_WRAPPER_W_5W(Graph_map)
DEFAULT_WRAPPER_W_3W(Graph_neighbors_count)
DEFAULT_WRAPPER_W_4W(Graph_neighbors)
DEFAULT_WRAPPER_W_3W(Graphdims_get)
DEFAULT_WRAPPER_W_1W(Grequest_complete)
DEFAULT_WRAPPER_W_5W(Grequest_start)
DEFAULT_WRAPPER_W_1W(Group_c2f)
DEFAULT_WRAPPER_W_3W(Group_compare)
DEFAULT_WRAPPER_W_3W(Group_difference)
DEFAULT_WRAPPER_W_4W(Group_excl)
DEFAULT_WRAPPER_W_1W(Group_f2c)
DEFAULT_WRAPPER_W_1W(Group_free)
DEFAULT_WRAPPER_W_4W(Group_incl)
DEFAULT_WRAPPER_W_3W(Group_intersection)
DEFAULT_WRAPPER_W_4W(Group_range_excl)
DEFAULT_WRAPPER_W_4W(Group_range_incl)
DEFAULT_WRAPPER_W_2W(Group_rank)
DEFAULT_WRAPPER_W_2W(Group_size)
DEFAULT_WRAPPER_W_5W(Group_translate_ranks)
DEFAULT_WRAPPER_W_3W(Group_union)
/* DEFAULT_WRAPPER_W_7W(Ibsend) */
DEFAULT_WRAPPER_W_1W(Info_c2f)
DEFAULT_WRAPPER_W_1W(Info_create)
DEFAULT_WRAPPER_W_2W(Info_delete)
DEFAULT_WRAPPER_W_2W(Info_dup)
DEFAULT_WRAPPER_W_1W(Info_f2c)
DEFAULT_WRAPPER_W_1W(Info_free)
DEFAULT_WRAPPER_W_5W(Info_get)
DEFAULT_WRAPPER_W_2W(Info_get_nkeys)
DEFAULT_WRAPPER_W_3W(Info_get_nthkey)
DEFAULT_WRAPPER_W_4W(Info_get_valuelen)
DEFAULT_WRAPPER_W_3W(Info_set)
/* DEFAULT_WRAPPER_W_2W(Init) */
/* DEFAULT_WRAPPER_W_1W(Initialized) */
DEFAULT_WRAPPER_W_4W(Init_thread)
DEFAULT_WRAPPER_W_6W(Intercomm_create)
DEFAULT_WRAPPER_W_3W(Intercomm_merge)
/* DEFAULT_WRAPPER_W_5W(Iprobe) */
/* DEFAULT_WRAPPER_W_7W(Irecv) */
/* DEFAULT_WRAPPER_W_7W(Irsend) */
/* DEFAULT_WRAPPER_W_7W(Isend) */
/* DEFAULT_WRAPPER_W_7W(Issend) */
DEFAULT_WRAPPER_W_1W(Is_thread_main)
DEFAULT_WRAPPER_W_4W(Keyval_create)
DEFAULT_WRAPPER_W_1W(Keyval_free)
DEFAULT_WRAPPER_W_3W(Lookup_name)
DEFAULT_WRAPPER_W_1W(Op_c2f)
/* DEFAULT_WRAPPER_W_3W(Op_create) */
DEFAULT_WRAPPER_W_2W(Open_port)
DEFAULT_WRAPPER_W_1W(Op_f2c)
DEFAULT_WRAPPER_W_1W(Op_free)
DEFAULT_WRAPPER_W_7W(Pack_external)
DEFAULT_WRAPPER_W_4W(Pack_external_size)
/* DEFAULT_WRAPPER_W_7W(Pack) */
DEFAULT_WRAPPER_W_4W(Pack_size)
/* int MPI_Pcontrol(const int level, ...) */
/* DEFAULT_WRAPPER_W_4W(Probe) */
DEFAULT_WRAPPER_W_3W(Publish_name)
DEFAULT_WRAPPER_W_8W(Put)
DEFAULT_WRAPPER_W_1W(Query_thread)
DEFAULT_WRAPPER_W_7W(Recv_init)
/* DEFAULT_WRAPPER_W_7W(Recv) */
/* DEFAULT_WRAPPER_W_7W(Reduce) */
DEFAULT_WRAPPER_W_6W(Reduce_scatter)
DEFAULT_WRAPPER_W_5W(Register_datarep)
DEFAULT_WRAPPER_W_1W(Request_c2f)
DEFAULT_WRAPPER_W_1W(Request_f2c)
DEFAULT_WRAPPER_W_1W(Request_free)
DEFAULT_WRAPPER_W_3W(Request_get_status)
/* DEFAULT_WRAPPER_W_6W(Rsend) */
DEFAULT_WRAPPER_W_7W(Rsend_init)
DEFAULT_WRAPPER_W_6W(Scan)
/* DEFAULT_WRAPPER_W_8W(Scatter) */
DEFAULT_WRAPPER_W_9W(Scatterv)
DEFAULT_WRAPPER_W_7W(Send_init)
/* DEFAULT_WRAPPER_W_6W(Send) */
/* DEFAULT_WRAPPER_W_12W(Sendrecv) */
DEFAULT_WRAPPER_W_9W(Sendrecv_replace)
DEFAULT_WRAPPER_W_7W(Ssend_init)
/* DEFAULT_WRAPPER_W_6W(Ssend) */
DEFAULT_WRAPPER_W_1W(Start)
DEFAULT_WRAPPER_W_2W(Startall)
DEFAULT_WRAPPER_W_2W(Status_c2f)
DEFAULT_WRAPPER_W_2W(Status_f2c)
DEFAULT_WRAPPER_W_2W(Status_set_cancelled)
DEFAULT_WRAPPER_W_3W(Status_set_elements)
/* DEFAULT_WRAPPER_W_4W(Testall) */
DEFAULT_WRAPPER_W_5W(Testany)
/* DEFAULT_WRAPPER_W_3W(Test) */
DEFAULT_WRAPPER_W_2W(Test_cancelled)
DEFAULT_WRAPPER_W_5W(Testsome)
DEFAULT_WRAPPER_W_2W(Topo_test)
DEFAULT_WRAPPER_W_1W(Type_c2f)
/* DEFAULT_WRAPPER_W_1W(Type_commit) */
DEFAULT_WRAPPER_W_3W(Type_contiguous)
DEFAULT_WRAPPER_W_10W(Type_create_darray)
DEFAULT_WRAPPER_W_3W(Type_create_f90_complex)
DEFAULT_WRAPPER_W_2W(Type_create_f90_integer)
DEFAULT_WRAPPER_W_3W(Type_create_f90_real)
DEFAULT_WRAPPER_W_5W(Type_create_hindexed)
DEFAULT_WRAPPER_W_5W(Type_create_hvector)
DEFAULT_WRAPPER_W_4W(Type_create_keyval)
DEFAULT_WRAPPER_W_5W(Type_create_indexed_block)
DEFAULT_WRAPPER_W_5W(Type_create_struct)
DEFAULT_WRAPPER_W_7W(Type_create_subarray)
DEFAULT_WRAPPER_W_4W(Type_create_resized)
DEFAULT_WRAPPER_W_2W(Type_delete_attr)
DEFAULT_WRAPPER_W_2W(Type_dup)
/* DEFAULT_WRAPPER_W_2W(Type_extent) */
/* DEFAULT_WRAPPER_W_1W(Type_free) */
DEFAULT_WRAPPER_W_1W(Type_free_keyval)
DEFAULT_WRAPPER_W_1W(Type_f2c)
DEFAULT_WRAPPER_W_4W(Type_get_attr)
/* DEFAULT_WRAPPER_W_7W(Type_get_contents) */
/* DEFAULT_WRAPPER_W_5W(Type_get_envelope) */
DEFAULT_WRAPPER_W_3W(Type_get_extent)
DEFAULT_WRAPPER_W_3W(Type_get_name)
DEFAULT_WRAPPER_W_3W(Type_get_true_extent)
DEFAULT_WRAPPER_W_5W(Type_hindexed)
DEFAULT_WRAPPER_W_5W(Type_hvector)
DEFAULT_WRAPPER_W_5W(Type_indexed)
DEFAULT_WRAPPER_W_2W(Type_lb)
DEFAULT_WRAPPER_W_3W(Type_match_size)
DEFAULT_WRAPPER_W_3W(Type_set_attr)
DEFAULT_WRAPPER_W_2W(Type_set_name)
DEFAULT_WRAPPER_W_2W(Type_size)
DEFAULT_WRAPPER_W_5W(Type_struct)
DEFAULT_WRAPPER_W_2W(Type_ub)
DEFAULT_WRAPPER_W_5W(Type_vector)
/* DEFAULT_WRAPPER_W_7W(Unpack) */
DEFAULT_WRAPPER_W_3W(Unpublish_name)
DEFAULT_WRAPPER_W_7W(Unpack_external)
/* DEFAULT_WRAPPER_W_3W(Waitall) */
/* DEFAULT_WRAPPER_W_4W(Waitany) */
/* DEFAULT_WRAPPER_W_2W(Wait) */
DEFAULT_WRAPPER_W_5W(Waitsome)
DEFAULT_WRAPPER_W_1W(Win_c2f)
DEFAULT_WRAPPER_W_2W(Win_call_errhandler)
DEFAULT_WRAPPER_W_1W(Win_complete)
DEFAULT_WRAPPER_W_6W(Win_create)
DEFAULT_WRAPPER_W_2W(Win_create_errhandler)
DEFAULT_WRAPPER_W_4W(Win_create_keyval)
DEFAULT_WRAPPER_W_2W(Win_delete_attr)
DEFAULT_WRAPPER_W_1W(Win_f2c)
DEFAULT_WRAPPER_W_2W(Win_fence)
DEFAULT_WRAPPER_W_1W(Win_free)
DEFAULT_WRAPPER_W_1W(Win_free_keyval)
DEFAULT_WRAPPER_W_4W(Win_get_attr)
DEFAULT_WRAPPER_W_2W(Win_get_errhandler)
DEFAULT_WRAPPER_W_2W(Win_get_group)
DEFAULT_WRAPPER_W_3W(Win_get_name)
DEFAULT_WRAPPER_W_4W(Win_lock)
DEFAULT_WRAPPER_W_3W(Win_post)
DEFAULT_WRAPPER_W_3W(Win_set_attr)
DEFAULT_WRAPPER_W_2W(Win_set_errhandler)
DEFAULT_WRAPPER_W_2W(Win_set_name)
DEFAULT_WRAPPER_W_3W(Win_start)
DEFAULT_WRAPPER_W_2W(Win_test)
DEFAULT_WRAPPER_W_2W(Win_unlock)
DEFAULT_WRAPPER_W_1W(Win_wait)
/* double MPI_Wtick(void) */
/* double MPI_Wtime(void) */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*---                                                      ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*---------------------------------------------------------------*/
/*--- end                                           mpiwrap.c ---*/
/*---------------------------------------------------------------*/
