
/* A test program to check whether the type-traversal functions in
   mpiwrap.c (walk_type, walk_type_array) are correct.  It does this
   by sending a message to itself, thereby discovering what areas of
   memory the MPI implementation itself believe constitute the type.
   It then gets walk_type to enumerate the type, and compares the
   results. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "mpi.h"
#include "../memcheck/memcheck.h"

typedef MPI_Datatype Ty;

typedef  unsigned char  Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

void* walk_type_fn = NULL;

static Ty tycon_Contiguous ( int count, Ty t )
{
   Ty t2;  
   int r = MPI_Type_contiguous( count, t, &t2 );
   assert(r == MPI_SUCCESS);
   return t2;
}

static Ty tycon_Struct2 ( int d1, int copies1, Ty t1,
                          int d2, int copies2, Ty t2 )
{
   int blocklens[2];
   MPI_Aint disps[2];
   Ty tys[2];
   Ty tres;
   int r;
   blocklens[0] = copies1;
   blocklens[1] = copies2;
   disps[0] = d1;
   disps[1] = d2;
   tys[0] = t1;
   tys[1] = t2;
   r = MPI_Type_struct( 2, blocklens, disps, tys, &tres );
   assert(r == MPI_SUCCESS);
   return tres;
}

static Ty tycon_Vector ( int count, int blocklen, int stride, Ty t )
{
   Ty tres;
   int r;
   r = MPI_Type_vector( count, blocklen, stride, t, &tres );
   assert(r == MPI_SUCCESS);
   return tres;
}

static Ty tycon_HVector ( int count, int blocklen, MPI_Aint stride, Ty t )
{
   Ty tres;
   int r;
   r = MPI_Type_hvector( count, blocklen, stride, t, &tres );
   assert(r == MPI_SUCCESS);
   return tres;
}

static Ty tycon_Indexed2 ( int d1, int copies1, 
                           int d2, int copies2, Ty t )
{
   int blocklens[2];
   int disps[2];
   Ty tres;
   int r;
   blocklens[0] = copies1;
   blocklens[1] = copies2;
   disps[0] = d1;
   disps[1] = d2;
   r = MPI_Type_indexed( 2, blocklens, disps, t, &tres );
   assert(r == MPI_SUCCESS);
   return tres;
}

static Ty tycon_HIndexed2 ( MPI_Aint d1, int copies1, 
                            MPI_Aint d2, int copies2, Ty t )
{
   int blocklens[2];
   MPI_Aint disps[2];
   Ty tres;
   int r;
   blocklens[0] = copies1;
   blocklens[1] = copies2;
   disps[0] = d1;
   disps[1] = d2;
   r = MPI_Type_hindexed( 2, blocklens, disps, t, &tres );
   assert(r == MPI_SUCCESS);
   return tres;
}

/* ------------------------------ */

char characterise ( unsigned char b )
{
   if (b == 0x00) return 'D';
   if (b == 0xFF) return '.';
   return '?';
}

void sendToMyself_callback( void* v, long n )
{
   long i;
   unsigned char* p = (unsigned char*)v;
   if (0) printf("callback: %p %ld\n", v, n);
   for (i = 0; i < n; i++)
      p[i] = 0x00;
}

void sendToMyself ( Bool commit_free, Ty* tyP, char* name )
{
   int i;
   MPI_Aint lb, ub, ex;
   MPI_Request req;
   MPI_Status status;
   char* sbuf;
   char* rbuf;
   char* rbuf_walk;
   int r;

   /* C: what a fabulous functional programming language :-) */
   void(*dl_walk_type)(void(*)(void*,long),char*,MPI_Datatype) 
     = (void(*)(void(*)(void*,long),char*,MPI_Datatype))
       walk_type_fn;
  
   if (!dl_walk_type) {
      printf("sendToMyself: can't establish type walker fn\n");
      return;
   }

   printf("\nsendToMyself: trying %s\n", name);

   if (commit_free) {
      r = MPI_Type_commit( tyP );
      assert(r == MPI_SUCCESS);
   }

   r = MPI_Type_lb( *tyP, &lb );
   assert(r == MPI_SUCCESS);
   r = MPI_Type_ub( *tyP, &ub );
   assert(r == MPI_SUCCESS);
   r = MPI_Type_extent( *tyP, &ex );
   assert(r == MPI_SUCCESS);
   printf("sendToMyself: ex=%d (%d,%d)\n", (int)ex, (int)lb, (int)ub);
   assert(lb >= 0);

   /* Fill send buffer with zeroes */
   sbuf = malloc(ub);
   assert(sbuf);
   for (i = 0; i < ub; i++)
      sbuf[i] = 0;

   r = MPI_Isend( sbuf,1,*tyP, 0,99,MPI_COMM_WORLD, &req);
   assert(r == MPI_SUCCESS);

   /* Fill recv buffer with 0xFFs */
   rbuf = malloc(ub);
   assert(rbuf);
   for (i = 0; i < ub; i++)
      rbuf[i] = 0xFF;

   r = MPI_Recv( rbuf,1,*tyP, 0,99,MPI_COMM_WORLD, &status);
   assert(r == MPI_SUCCESS);

   /* Now: rbuf should contain 0x00s where data was transferred and
      undefined 0xFFs where data was not transferred.  Get
      libmpiwrap.so to walk the transferred type, using the callback
      to set to 0x00 all parts of rbuf_walk it considers part of the
      type. */

   rbuf_walk = malloc(ub);
   assert(rbuf_walk);
   for (i = 0; i < ub; i++)
      rbuf_walk[i] = 0xFF;

   dl_walk_type( sendToMyself_callback, rbuf_walk, *tyP );

   if (commit_free) {
      r = MPI_Type_free( tyP );
      assert(r == MPI_SUCCESS);
   }

   for (i = 0; i < ub; i++) {
      if (rbuf_walk[i] == rbuf[i])
         continue; /* ok */
      else
         break; /* discrepancy */
   }

   if (i == ub)
      printf("SUCCESS\n");
   else
      printf("FAILED\n");

   printf(" libmpiwrap=");
   for (i = 0; i < ub; i++)
      printf("%c", characterise(rbuf_walk[i]));
   printf("\n");

   printf("MPI library=");
   for (i = 0; i < ub; i++)
      printf("%c", characterise(rbuf[i]));
   printf("\n");

   free(sbuf);
   free(rbuf);
   free(rbuf_walk);
}


typedef  char*  Nm;

int main ( int argc, char** argv )
{
    int rank, size;
    char* opts;

    if (!RUNNING_ON_VALGRIND) {
       printf("error: this program must be run on valgrind\n");
       return 1;
    }
    opts = getenv("MPIWRAP_DEBUG");
    if ((!opts) || NULL==strstr(opts, "initkludge")) {
       printf("error: program requires MPIWRAP_DEBUG=initkludge\n");
       return 1;
    }

    /* Note: this trick doesn't work on 64-bit platforms, 
       since MPI_Init returns int. */
    walk_type_fn = (void*)(long) MPI_Init( &argc, &argv );
    printf("mpiwrap_type_test: walk_type_fn = %p\n", walk_type_fn);
    assert(walk_type_fn);

    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    if (rank == 0) {

#define TRY(_commit_free,_type,_name)              \
       do { Ty ty = (_type);                       \
            Nm nm = (_name);                       \
            sendToMyself((_commit_free), &ty, nm); \
       } while (0)

    TRY(True, tycon_Contiguous(3, MPI_INT),
              "Contig{3xINT}");

    TRY(True, tycon_Struct2(3,2,MPI_CHAR, 8,1,MPI_DOUBLE),
              "Struct{h3:2xCHAR, h8:1xDOUBLE}");

    TRY(True, tycon_Struct2(0,1,MPI_CHAR, 8,1,tycon_Contiguous(4, MPI_DOUBLE)),
              "Struct{h0:1xCHAR, h8:1xContig{4xDOUBLE}}");

    TRY(True, tycon_Contiguous(10, tycon_Struct2(1,1,MPI_CHAR, 4,1,MPI_FLOAT)),
              "Contig{10xStruct{h1:1xCHAR, h4:1xFLOAT}}");

    TRY(True, tycon_Vector(5, 2,3,MPI_DOUBLE),
              "Vector{5x(2,3)xDOUBLE}");

    TRY(True, tycon_Vector(3, 1,2,MPI_LONG_DOUBLE),
              "Vector{3x(1,2)xLONG_DOUBLE}");

    TRY(True, tycon_HVector(4, 1,3,MPI_SHORT),
              "HVector{4x(1,h3)xSHORT}");

    TRY(True, tycon_Indexed2(1,3, 5,2, MPI_UNSIGNED_CHAR),
              "Indexed{1:3x,5:2x,UNSIGNED_CHAR}");

    TRY(True,  tycon_HIndexed2(1,2, 6,3, MPI_UNSIGNED_SHORT),
              "HIndexed{h1:2x,h6:3x,UNSIGNED_SHORT}");

    TRY(False, MPI_FLOAT_INT,        "FLOAT_INT");
    TRY(False, MPI_DOUBLE_INT,       "DOUBLE_INT");
    TRY(False, MPI_LONG_INT,         "LONG_INT");
    TRY(False, MPI_SHORT_INT,        "SHORT_INT");
    TRY(False, MPI_2INT,             "2INT");
    TRY(False, MPI_LONG_DOUBLE_INT,  "LONG_DOUBLE_INT");

    /* The next 4 don't seem to exist on openmpi-1.2.2. */

#if defined(MPI_REAL8)
    TRY(False,  MPI_REAL8,            "REAL8");
#endif
#if defined(MPI_REAL4)
    TRY(False,  MPI_REAL4,            "REAL4");
#endif
#if defined(MPI_INTEGER8)
    TRY(False,  MPI_INTEGER8,         "INTEGER8");
#endif
#if defined(MPI_INTEGER4)
    TRY(False,  MPI_INTEGER4,         "INTEGER4");
#endif

    TRY(False, MPI_COMPLEX,           "COMPLEX");
    TRY(False, MPI_DOUBLE_COMPLEX,    "DOUBLE_COMPLEX");

    // On openmpi-1.2.2 on x86-linux, sendToMyself bombs openmpi,
    // for some reason (openmpi thinks these all have zero size/extent
    // and therefore can't be MPI_Send-ed, AIUI).
    // TRY(False, MPI_LOGICAL,           "LOGICAL");
    // TRY(False, MPI_REAL,              "REAL");
    // TRY(False, MPI_DOUBLE_PRECISION,  "DOUBLE_PRECISION");
    // TRY(False, MPI_INTEGER,           "INTEGER");
    TRY(False, MPI_2INTEGER,          "2INTEGER");
    TRY(False, MPI_2COMPLEX,          "2COMPLEX");
    TRY(False, MPI_2DOUBLE_COMPLEX,   "2DOUBLE_COMPLEX");
    TRY(False, MPI_2REAL,             "2REAL");
    TRY(False, MPI_2DOUBLE_PRECISION, "2DOUBLE_PRECISION");
    TRY(False, MPI_CHARACTER,         "CHARACTER");

    /* The following from a table in chapter 9 of the MPI2 spec
       date Nov 15, 2003, page 247. */
    TRY(False, MPI_PACKED, "PACKED");
    TRY(False, MPI_BYTE, "BYTE");
    TRY(False, MPI_CHAR, "CHAR");
    TRY(False, MPI_UNSIGNED_CHAR, "UNSIGNED_CHAR");
    TRY(False, MPI_SIGNED_CHAR, "SIGNED_CHAR");
    TRY(False, MPI_WCHAR, "WCHAR");
    TRY(False, MPI_SHORT, "SHORT");
    TRY(False, MPI_UNSIGNED_SHORT, "UNSIGNED_SHORT");
    TRY(False, MPI_INT, "INT");
    TRY(False, MPI_UNSIGNED, "UNSIGNED");
    TRY(False, MPI_LONG, "LONG");
    TRY(False, MPI_UNSIGNED_LONG, "UNSIGNED_LONG");
    TRY(False, MPI_FLOAT, "FLOAT");
    TRY(False, MPI_DOUBLE, "DOUBLE");
    TRY(False, MPI_LONG_DOUBLE, "LONG_DOUBLE");
    TRY(False, MPI_CHARACTER, "CHARACTER");

    // Same deal as above
    // TRY(False, MPI_LOGICAL, "LOGICAL");
    // TRY(False, MPI_INTEGER, "INTEGER");
    // TRY(False, MPI_REAL, "REAL");
    // TRY(False, MPI_DOUBLE_PRECISION, "DOUBLE_PRECISION");

    TRY(False, MPI_COMPLEX, "COMPLEX");
    TRY(False, MPI_DOUBLE_COMPLEX, "DOUBLE_COMPLEX");
#if defined(MPI_INTEGER1)
    TRY(False, MPI_INTEGER1, "INTEGER1");
#endif
#if defined(MPI_INTEGER2)
    TRY(False, MPI_INTEGER2, "INTEGER2");
#endif
#if defined(MPI_INTEGER4)
    TRY(False, MPI_INTEGER4, "INTEGER4");
#endif
#if defined(MPI_INTEGER8)
    TRY(False, MPI_INTEGER8, "INTEGER8");
#endif
    TRY(False, MPI_LONG_LONG, "LONG_LONG");
    TRY(False, MPI_UNSIGNED_LONG_LONG, "UNSIGNED_LONG_LONG");
#if defined(MPI_REAL4)
    TRY(False, MPI_REAL4, "REAL4");
#endif
#if defined(MPI_REAL8)
    TRY(False, MPI_REAL8, "REAL8");
#endif
#if defined(MPI_REAL16)
    TRY(False, MPI_REAL16, "REAL16");
#endif

#undef TRY

    }

    MPI_Finalize();
    return 0;
}
