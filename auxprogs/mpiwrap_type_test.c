
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

   for (i = 0; i < ub; i++)
      printf("%c", characterise(rbuf_walk[i]));
   printf("\n");

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

    Ty t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18;
    Nm n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, n18;

    t2 = tycon_Contiguous(3, MPI_INT);
    n2 = "Contig{3xINT}";

    t3 = tycon_Struct2(3,2,MPI_CHAR, 8,1,MPI_DOUBLE);
    n3 = "Struct{h3:2xCHAR, h8:1xDOUBLE}";

    t4 = tycon_Struct2(0,1,MPI_CHAR, 8,1,tycon_Contiguous(4, MPI_DOUBLE));
    n4 = "Struct{h0:1xCHAR, h8:1xContig{4xDOUBLE}}";

    t5 = tycon_Contiguous(10, tycon_Struct2(1,1,MPI_CHAR, 4,1,MPI_FLOAT));
    n5 = "Contig{10xStruct{h1:1xCHAR, h4:1xFLOAT}}";

    t6 = tycon_Vector(5, 2,3,MPI_DOUBLE);
    n6 = "Vector{5x(2,3)xDOUBLE}";

    t7 = tycon_Vector(3, 1,2,MPI_LONG_DOUBLE);
    n7 = "Vector{3x(1,2)xLONG_DOUBLE}";

    t8 = tycon_HVector(4, 1,3,MPI_SHORT);
    n8 = "HVector{4x(1,h3)xSHORT}";

    t9 = tycon_Indexed2(1,3, 5,2, MPI_UNSIGNED_CHAR);
    n9 = "Indexed{1:3x,5:2x,UNSIGNED_CHAR}";

    t10 = tycon_HIndexed2(1,2, 6,3, MPI_UNSIGNED_SHORT);
    n10 = "HIndexed{h1:2x,h6:3x,UNSIGNED_SHORT}";

    t11 = MPI_LONG_INT;
    n11 = "LONG_INT";

    t12 = MPI_DOUBLE_INT;
    n12 = "DOUBLE_INT";

    t13 = MPI_SHORT_INT;
    n13 = "SHORT_INT";

    t14 = MPI_REAL8;
    n14 = "REAL8";

    t15 = MPI_REAL4;
    n15 = "REAL4";

    t16 = MPI_INTEGER8;
    n16 = "INTEGER8";

    t17 = MPI_INTEGER4;
    n17 = "INTEGER4";

    t18 = MPI_2INT;
    n18 = "2INT";

    sendToMyself(True,  &t2,  n2);
    sendToMyself(True,  &t3,  n3);
    sendToMyself(True,  &t4,  n4);
    sendToMyself(True,  &t5,  n5);
    sendToMyself(True,  &t6,  n6);
    sendToMyself(True,  &t7,  n7);
    sendToMyself(True,  &t8,  n8);
    sendToMyself(True,  &t9,  n9);
    sendToMyself(True,  &t10, n10);
    sendToMyself(False, &t11, n11);
    sendToMyself(False, &t12, n12);
    sendToMyself(False, &t13, n13);
    sendToMyself(False, &t14, n14);
    sendToMyself(False, &t15, n15);
    sendToMyself(False, &t16, n16);
    sendToMyself(False, &t17, n17);
    sendToMyself(False, &t18, n18);
    }

    MPI_Finalize();
    return 0;
}
