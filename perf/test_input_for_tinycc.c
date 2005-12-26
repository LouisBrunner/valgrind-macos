
typedef unsigned int size_t;

typedef long int wchar_t;


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;




extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__));




extern double atof (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));

extern int atoi (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));

extern long int atol (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





__extension__ extern long long int atoll (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern double strtod (__const char *__restrict __nptr,
        char **__restrict __endptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));




extern long int strtol (__const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

extern unsigned long int strtoul (__const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));




__extension__
extern long long int strtoq (__const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtouq (__const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





__extension__
extern long long int strtoll (__const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtoull (__const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern double __strtod_internal (__const char *__restrict __nptr,
     char **__restrict __endptr, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern float __strtof_internal (__const char *__restrict __nptr,
    char **__restrict __endptr, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern long double __strtold_internal (__const char *__restrict __nptr,
           char **__restrict __endptr,
           int __group) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

extern long int __strtol_internal (__const char *__restrict __nptr,
       char **__restrict __endptr,
       int __base, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern unsigned long int __strtoul_internal (__const char *__restrict __nptr,
          char **__restrict __endptr,
          int __base, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));




__extension__
extern long long int __strtoll_internal (__const char *__restrict __nptr,
      char **__restrict __endptr,
      int __base, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



__extension__
extern unsigned long long int __strtoull_internal (__const char *
         __restrict __nptr,
         char **__restrict __endptr,
         int __base, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

extern char *l64a (long int __n) __attribute__ ((__nothrow__));


extern long int a64l (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));






typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;




__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;







__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;






__extension__ typedef __u_quad_t __dev_t;
__extension__ typedef unsigned int __uid_t;
__extension__ typedef unsigned int __gid_t;
__extension__ typedef unsigned long int __ino_t;
__extension__ typedef __u_quad_t __ino64_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef unsigned int __nlink_t;
__extension__ typedef long int __off_t;
__extension__ typedef __quad_t __off64_t;
__extension__ typedef int __pid_t;
__extension__ typedef struct { int __val[2]; } __fsid_t;
__extension__ typedef long int __clock_t;
__extension__ typedef unsigned long int __rlim_t;
__extension__ typedef __u_quad_t __rlim64_t;
__extension__ typedef unsigned int __id_t;
__extension__ typedef long int __time_t;
__extension__ typedef unsigned int __useconds_t;
__extension__ typedef long int __suseconds_t;

__extension__ typedef int __daddr_t;
__extension__ typedef long int __swblk_t;
__extension__ typedef int __key_t;


__extension__ typedef int __clockid_t;


__extension__ typedef int __timer_t;


__extension__ typedef long int __blksize_t;




__extension__ typedef long int __blkcnt_t;
__extension__ typedef __quad_t __blkcnt64_t;


__extension__ typedef unsigned long int __fsblkcnt_t;
__extension__ typedef __u_quad_t __fsblkcnt64_t;


__extension__ typedef unsigned long int __fsfilcnt_t;
__extension__ typedef __u_quad_t __fsfilcnt64_t;

__extension__ typedef int __ssize_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


__extension__ typedef int __intptr_t;


__extension__ typedef unsigned int __socklen_t;




typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;




typedef __loff_t loff_t;



typedef __ino_t ino_t;

typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;





typedef __off_t off_t;

typedef __pid_t pid_t;




typedef __id_t id_t;




typedef __ssize_t ssize_t;





typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;


typedef __time_t time_t;




typedef __clockid_t clockid_t;

typedef __timer_t timer_t;




typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;

typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));


typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

typedef int register_t __attribute__ ((__mode__ (__word__)));

typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;




typedef __sigset_t sigset_t;





struct timespec
  {
    __time_t tv_sec;
    long int tv_nsec;
  };

struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };



typedef __suseconds_t suseconds_t;





typedef long int __fd_mask;

typedef struct
  {






    __fd_mask __fds_bits[1024 / (8 * sizeof (__fd_mask))];


  } fd_set;






typedef __fd_mask fd_mask;



extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);

__extension__
extern __inline unsigned int gnu_dev_major (unsigned long long int __dev)
     __attribute__ ((__nothrow__));
__extension__
extern __inline unsigned int gnu_dev_minor (unsigned long long int __dev)
     __attribute__ ((__nothrow__));
__extension__
extern __inline unsigned long long int gnu_dev_makedev (unsigned int __major,
       unsigned int __minor)
     __attribute__ ((__nothrow__));


__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_major (unsigned long long int __dev)
{
  return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
}

__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_minor (unsigned long long int __dev)
{
  return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
}

__extension__ extern __inline unsigned long long int
__attribute__ ((__nothrow__)) gnu_dev_makedev (unsigned int __major, unsigned int __minor)
{
  return ((__minor & 0xff) | ((__major & 0xfff) << 8)
   | (((unsigned long long int) (__minor & ~0xff)) << 12)
   | (((unsigned long long int) (__major & ~0xfff)) << 32));
}

typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;

struct __sched_param
  {
    int __sched_priority;
  };



struct _pthread_fastlock
{
  long int __status;
  int __spinlock;

};



typedef struct _pthread_descr_struct *_pthread_descr;





typedef struct __pthread_attr_s
{
  int __detachstate;
  int __schedpolicy;
  struct __sched_param __schedparam;
  int __inheritsched;
  int __scope;
  size_t __guardsize;
  int __stackaddr_set;
  void *__stackaddr;
  size_t __stacksize;
} pthread_attr_t;





__extension__ typedef long long __pthread_cond_align_t;




typedef struct
{
  struct _pthread_fastlock __c_lock;
  _pthread_descr __c_waiting;
  char __padding[48 - sizeof (struct _pthread_fastlock)
   - sizeof (_pthread_descr) - sizeof (__pthread_cond_align_t)];
  __pthread_cond_align_t __align;
} pthread_cond_t;



typedef struct
{
  int __dummy;
} pthread_condattr_t;


typedef unsigned int pthread_key_t;





typedef struct
{
  int __m_reserved;
  int __m_count;
  _pthread_descr __m_owner;
  int __m_kind;
  struct _pthread_fastlock __m_lock;
} pthread_mutex_t;



typedef struct
{
  int __mutexkind;
} pthread_mutexattr_t;



typedef int pthread_once_t;

typedef unsigned long int pthread_t;







extern long int random (void) __attribute__ ((__nothrow__));


extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__));





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));



extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4)));

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));






extern int rand (void) __attribute__ ((__nothrow__));

extern void srand (unsigned int __seed) __attribute__ ((__nothrow__));




extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__));







extern double drand48 (void) __attribute__ ((__nothrow__));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern long int lrand48 (void) __attribute__ ((__nothrow__));
extern long int nrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern long int mrand48 (void) __attribute__ ((__nothrow__));
extern long int jrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern void srand48 (long int __seedval) __attribute__ ((__nothrow__));
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    unsigned long long int __a;
  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));









extern void *malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));

extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));







extern void *realloc (void *__ptr, size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));

extern void free (void *__ptr) __attribute__ ((__nothrow__));




extern void cfree (void *__ptr) __attribute__ ((__nothrow__));




extern void *alloca (size_t __size) __attribute__ ((__nothrow__));






extern void *valloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));

extern void abort (void) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern void exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));

extern char *getenv (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));




extern char *__secure_getenv (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int putenv (char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int setenv (__const char *__name, __const char *__value, int __replace)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));


extern int unsetenv (__const char *__name) __attribute__ ((__nothrow__));






extern int clearenv (void) __attribute__ ((__nothrow__));

extern char *mktemp (char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

extern int mkstemp (char *__template) __attribute__ ((__nonnull__ (1)));

extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));








extern int system (__const char *__command);

extern char *realpath (__const char *__restrict __name,
         char *__restrict __resolved) __attribute__ ((__nothrow__));






typedef int (*__compar_fn_t) (__const void *, __const void *);









extern void *bsearch (__const void *__key, __const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5)));



extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));



extern int abs (int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long int labs (long int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));












extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));

extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4)));




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4)));




extern char *gcvt (double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3)));




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4)));
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4)));
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3)));




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));







extern int mblen (__const char *__s, size_t __n) __attribute__ ((__nothrow__));


extern int mbtowc (wchar_t *__restrict __pwc,
     __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));


extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__));



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));

extern size_t wcstombs (char *__restrict __s,
   __const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__));








extern int rpmatch (__const char *__response) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int getloadavg (double __loadavg[], int __nelem)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));







typedef
   struct {
      char *next_in;
      unsigned int avail_in;
      unsigned int total_in_lo32;
      unsigned int total_in_hi32;

      char *next_out;
      unsigned int avail_out;
      unsigned int total_out_lo32;
      unsigned int total_out_hi32;

      void *state;

      void *(*bzalloc)(void *,int,int);
      void (*bzfree)(void *,void *);
      void *opaque;
   }
   bz_stream;

extern int BZ2_bzCompressInit (
      bz_stream* strm,
      int blockSize100k,
      int verbosity,
      int workFactor
   );

extern int BZ2_bzCompress (
      bz_stream* strm,
      int action
   );

extern int BZ2_bzCompressEnd (
      bz_stream* strm
   );

extern int BZ2_bzDecompressInit (
      bz_stream *strm,
      int verbosity,
      int small
   );

extern int BZ2_bzDecompress (
      bz_stream* strm
   );

extern int BZ2_bzDecompressEnd (
      bz_stream *strm
   );

extern int BZ2_bzBuffToBuffCompress (
      char* dest,
      unsigned int* destLen,
      char* source,
      unsigned int sourceLen,
      int blockSize100k,
      int verbosity,
      int workFactor
   );

extern int BZ2_bzBuffToBuffDecompress (
      char* dest,
      unsigned int* destLen,
      char* source,
      unsigned int sourceLen,
      int small,
      int verbosity
   );

extern const char * BZ2_bzlibVersion (
      void
   );

typedef char Char;
typedef unsigned char Bool;
typedef unsigned char UChar;
typedef int Int32;
typedef unsigned int UInt32;
typedef short Int16;
typedef unsigned short UInt16;

extern void bz_internal_error ( int errcode );

extern Int32 BZ2_rNums[512];

extern UInt32 BZ2_crc32Table[256];

typedef
   struct {

      bz_stream* strm;



      Int32 mode;
      Int32 state;


      UInt32 avail_in_expect;


      UInt32* arr1;
      UInt32* arr2;
      UInt32* ftab;
      Int32 origPtr;


      UInt32* ptr;
      UChar* block;
      UInt16* mtfv;
      UChar* zbits;


      Int32 workFactor;


      UInt32 state_in_ch;
      Int32 state_in_len;
      Int32 rNToGo; Int32 rTPos;


      Int32 nblock;
      Int32 nblockMAX;
      Int32 numZ;
      Int32 state_out_pos;


      Int32 nInUse;
      Bool inUse[256];
      UChar unseqToSeq[256];


      UInt32 bsBuff;
      Int32 bsLive;


      UInt32 blockCRC;
      UInt32 combinedCRC;


      Int32 verbosity;
      Int32 blockNo;
      Int32 blockSize100k;


      Int32 nMTF;
      Int32 mtfFreq [258];
      UChar selector [(2 + (900000 / 50))];
      UChar selectorMtf[(2 + (900000 / 50))];

      UChar len [6][258];
      Int32 code [6][258];
      Int32 rfreq [6][258];

      UInt32 len_pack[258][4];

   }
   EState;





extern void
BZ2_blockSort ( EState* );

extern void
BZ2_compressBlock ( EState*, Bool );

extern void
BZ2_bsInitWrite ( EState* );

extern void
BZ2_hbAssignCodes ( Int32*, UChar*, Int32, Int32, Int32 );

extern void
BZ2_hbMakeCodeLengths ( UChar*, Int32*, Int32, Int32 );

typedef
   struct {

      bz_stream* strm;


      Int32 state;


      UChar state_out_ch;
      Int32 state_out_len;
      Bool blockRandomised;
      Int32 rNToGo; Int32 rTPos;


      UInt32 bsBuff;
      Int32 bsLive;


      Int32 blockSize100k;
      Bool smallDecompress;
      Int32 currBlockNo;
      Int32 verbosity;


      Int32 origPtr;
      UInt32 tPos;
      Int32 k0;
      Int32 unzftab[256];
      Int32 nblock_used;
      Int32 cftab[257];
      Int32 cftabCopy[257];


      UInt32 *tt;


      UInt16 *ll16;
      UChar *ll4;


      UInt32 storedBlockCRC;
      UInt32 storedCombinedCRC;
      UInt32 calculatedBlockCRC;
      UInt32 calculatedCombinedCRC;


      Int32 nInUse;
      Bool inUse[256];
      Bool inUse16[16];
      UChar seqToUnseq[256];


      UChar mtfa [4096];
      Int32 mtfbase[256 / 16];
      UChar selector [(2 + (900000 / 50))];
      UChar selectorMtf[(2 + (900000 / 50))];
      UChar len [6][258];

      Int32 limit [6][258];
      Int32 base [6][258];
      Int32 perm [6][258];
      Int32 minLens[6];


      Int32 save_i;
      Int32 save_j;
      Int32 save_t;
      Int32 save_alphaSize;
      Int32 save_nGroups;
      Int32 save_nSelectors;
      Int32 save_EOB;
      Int32 save_groupNo;
      Int32 save_groupPos;
      Int32 save_nextSym;
      Int32 save_nblockMAX;
      Int32 save_nblock;
      Int32 save_es;
      Int32 save_N;
      Int32 save_curr;
      Int32 save_zt;
      Int32 save_zn;
      Int32 save_zvec;
      Int32 save_zj;
      Int32 save_gSel;
      Int32 save_gMinlen;
      Int32* save_gLimit;
      Int32* save_gBase;
      Int32* save_gPerm;

   }
   DState;

extern Int32
BZ2_indexIntoF ( Int32, Int32* );

extern Int32
BZ2_decompress ( DState* );

extern void
BZ2_hbCreateDecodeTables ( Int32*, Int32*, Int32*, UChar*,
                           Int32, Int32, Int32 );

typedef unsigned long HWord;
typedef char HChar;
typedef signed int Int;
typedef unsigned int UInt;

typedef signed long long int Long;
typedef unsigned long long int ULong;





static HWord (*serviceFn)(HWord,HWord) = 0;

char* my_strcat ( char* dest, const char* src )
{
   char* dest_orig = dest;
   while (*dest) dest++;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}




static void vex_log_bytes ( char* p, int n )
{
   int i;
   for (i = 0; i < n; i++)
      (*serviceFn)( 1, (int)p[i] );
}




static HChar vex_toupper ( HChar c )
{
   if (c >= 'a' && c <= 'z')
      return c + ('A' - 'a');
   else
      return c;
}

static Int vex_strlen ( const HChar* str )
{
   Int i = 0;
   while (str[i] != 0) i++;
   return i;
}

Bool vex_streq ( const HChar* s1, const HChar* s2 )
{
   while (((Bool)1)) {
      if (*s1 == 0 && *s2 == 0)
         return ((Bool)1);
      if (*s1 != *s2)
         return ((Bool)0);
      s1++;
      s2++;
   }
}

static UInt
myvprintf_str ( void(*send)(HChar), Int flags, Int width, HChar* str,
                Bool capitalise )
{

   UInt ret = 0;
   Int i, extra;
   Int len = vex_strlen(str);

   if (width == 0) {
      ret += len;
      for (i = 0; i < len; i++)
         send((capitalise ? vex_toupper(str[i]) : (str[i])));
      return ret;
   }

   if (len > width) {
      ret += width;
      for (i = 0; i < width; i++)
         send((capitalise ? vex_toupper(str[i]) : (str[i])));
      return ret;
   }

   extra = width - len;
   if (flags & 4) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ');
   }
   ret += len;
   for (i = 0; i < len; i++)
      send((capitalise ? vex_toupper(str[i]) : (str[i])));
   if (!(flags & 4)) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ');
   }



   return ret;
}







static UInt
myvprintf_int64 ( void(*send)(HChar), Int flags, Int base, Int width, ULong pL)
{
   HChar buf[40];
   Int ind = 0;
   Int i, nc = 0;
   Bool neg = ((Bool)0);
   HChar *digits = "0123456789ABCDEF";
   UInt ret = 0;
   UInt p = (UInt)pL;

   if (base < 2 || base > 16)
      return ret;

   if ((flags & 1) && (Int)p < 0) {
      p = - (Int)p;
      neg = ((Bool)1);
   }

   if (p == 0)
      buf[ind++] = '0';
   else {
      while (p > 0) {
         if ((flags & 16) && 10 == base &&
             0 == (ind-nc) % 3 && 0 != ind)
         {
            buf[ind++] = ',';
            nc++;
         }
         buf[ind++] = digits[p % base];
         p /= base;
      }
   }

   if (neg)
      buf[ind++] = '-';

   if (width > 0 && !(flags & 4)) {
      for(; ind < width; ind++) {

         buf[ind] = ((flags & 2) ? '0': ' ');
      }
   }


   ret += ind;
   for (i = ind -1; i >= 0; i--) {
      send(buf[i]);
   }
   if (width > 0 && (flags & 4)) {
      for(; ind < width; ind++) {
  ret++;
         send(' ');
      }
   }
   return ret;
}



static
UInt vprintf_wrk ( )
{
  return 0;
}






static HChar myprintf_buf[1000];
static Int n_myprintf_buf;

static void add_to_myprintf_buf ( HChar c )
{
   if (c == '\n' || n_myprintf_buf >= 1000-10 ) {
      vex_log_bytes( myprintf_buf, vex_strlen(myprintf_buf) );
      n_myprintf_buf = 0;
      myprintf_buf[n_myprintf_buf] = 0;
   }
   myprintf_buf[n_myprintf_buf++] = c;
   myprintf_buf[n_myprintf_buf] = 0;
}

static UInt vex_printf ( const char *format, ... )
{
  return 0;
}

static
void makeMaps_d ( DState* s )
{
   Int32 i;
   s->nInUse = 0;
   for (i = 0; i < 256; i++)
      if (s->inUse[i]) {
         s->seqToUnseq[s->nInUse] = i;
         s->nInUse++;
      }
}

__inline__ Int32 BZ2_indexIntoF ( Int32 indx, Int32 *cftab )
{
   Int32 nb, na, mid;
   nb = 0;
   na = 256;
   do {
      mid = (nb + na) >> 1;
      if (indx >= cftab[mid]) nb = mid; else na = mid;
   }
   while (na - nb != 1);
   return nb;
}


Int32 BZ2_decompress ( DState* s )
{
   UChar uc;
   Int32 retVal;
   Int32 minLen, maxLen;
   bz_stream* strm = s->strm;


   Int32 i;
   Int32 j;
   Int32 t;
   Int32 alphaSize;
   Int32 nGroups;
   Int32 nSelectors;
   Int32 EOB;
   Int32 groupNo;
   Int32 groupPos;
   Int32 nextSym;
   Int32 nblockMAX;
   Int32 nblock;
   Int32 es;
   Int32 N;
   Int32 curr;
   Int32 zt;
   Int32 zn;
   Int32 zvec;
   Int32 zj;
   Int32 gSel;
   Int32 gMinlen;
   Int32* gLimit;
   Int32* gBase;
   Int32* gPerm;

   if (s->state == 10) {

      s->save_i = 0;
      s->save_j = 0;
      s->save_t = 0;
      s->save_alphaSize = 0;
      s->save_nGroups = 0;
      s->save_nSelectors = 0;
      s->save_EOB = 0;
      s->save_groupNo = 0;
      s->save_groupPos = 0;
      s->save_nextSym = 0;
      s->save_nblockMAX = 0;
      s->save_nblock = 0;
      s->save_es = 0;
      s->save_N = 0;
      s->save_curr = 0;
      s->save_zt = 0;
      s->save_zn = 0;
      s->save_zvec = 0;
      s->save_zj = 0;
      s->save_gSel = 0;
      s->save_gMinlen = 0;
      s->save_gLimit = ((void *)0);
      s->save_gBase = ((void *)0);
      s->save_gPerm = ((void *)0);
   }


   i = s->save_i;
   j = s->save_j;
   t = s->save_t;
   alphaSize = s->save_alphaSize;
   nGroups = s->save_nGroups;
   nSelectors = s->save_nSelectors;
   EOB = s->save_EOB;
   groupNo = s->save_groupNo;
   groupPos = s->save_groupPos;
   nextSym = s->save_nextSym;
   nblockMAX = s->save_nblockMAX;
   nblock = s->save_nblock;
   es = s->save_es;
   N = s->save_N;
   curr = s->save_curr;
   zt = s->save_zt;
   zn = s->save_zn;
   zvec = s->save_zvec;
   zj = s->save_zj;
   gSel = s->save_gSel;
   gMinlen = s->save_gMinlen;
   gLimit = s->save_gLimit;
   gBase = s->save_gBase;
   gPerm = s->save_gPerm;

   retVal = 0;

   switch (s->state) {

      case 10: s->state = 10; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x42) { retVal = (-5); goto save_state_and_return; };;

      case 11: s->state = 11; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x5a) { retVal = (-5); goto save_state_and_return; };;

      case 12: s->state = 12; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }
      if (uc != 0x68) { retVal = (-5); goto save_state_and_return; };;

      case 13: s->state = 13; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; s->blockSize100k = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }
      if (s->blockSize100k < (0x30 + 1) ||
          s->blockSize100k > (0x30 + 9)) { retVal = (-5); goto save_state_and_return; };;
      s->blockSize100k -= 0x30;

      if (s->smallDecompress) {
         s->ll16 = (strm->bzalloc)(strm->opaque,(s->blockSize100k * 100000 * sizeof(UInt16)),1);
         s->ll4 = (strm->bzalloc)(strm->opaque,(((1 + s->blockSize100k * 100000) >> 1) * sizeof(UChar)),1);


         if (s->ll16 == ((void *)0) || s->ll4 == ((void *)0)) { retVal = (-3); goto save_state_and_return; };;
      } else {
         s->tt = (strm->bzalloc)(strm->opaque,(s->blockSize100k * 100000 * sizeof(Int32)),1);
         if (s->tt == ((void *)0)) { retVal = (-3); goto save_state_and_return; };;
      }

      case 14: s->state = 14; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };

      if (uc == 0x17) goto endhdr_2;
      if (uc != 0x31) { retVal = (-4); goto save_state_and_return; };;
      case 15: s->state = 15; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x41) { retVal = (-4); goto save_state_and_return; };;
      case 16: s->state = 16; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x59) { retVal = (-4); goto save_state_and_return; };;
      case 17: s->state = 17; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x26) { retVal = (-4); goto save_state_and_return; };;
      case 18: s->state = 18; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x53) { retVal = (-4); goto save_state_and_return; };;
      case 19: s->state = 19; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x59) { retVal = (-4); goto save_state_and_return; };;

      s->currBlockNo++;
      if (s->verbosity >= 2)
         vex_printf("\n    [%d: huff+mtf ",s->currBlockNo);

      s->storedBlockCRC = 0;
      case 20: s->state = 20; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);
      case 21: s->state = 21; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);
      case 22: s->state = 22; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);
      case 23: s->state = 23; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);

      case 24: s->state = 24; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; s->blockRandomised = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };

      s->origPtr = 0;
      case 25: s->state = 25; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);
      case 26: s->state = 26; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);
      case 27: s->state = 27; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);

      if (s->origPtr < 0)
         { retVal = (-4); goto save_state_and_return; };;
      if (s->origPtr > 10 + 100000*s->blockSize100k)
         { retVal = (-4); goto save_state_and_return; };;


      for (i = 0; i < 16; i++) {
         case 28: s->state = 28; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
         if (uc == 1)
            s->inUse16[i] = ((Bool)1); else
            s->inUse16[i] = ((Bool)0);
      }

      for (i = 0; i < 256; i++) s->inUse[i] = ((Bool)0);

      for (i = 0; i < 16; i++)
         if (s->inUse16[i])
            for (j = 0; j < 16; j++) {
               case 29: s->state = 29; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
               if (uc == 1) s->inUse[i * 16 + j] = ((Bool)1);
            }
      makeMaps_d ( s );
      if (s->nInUse == 0) { retVal = (-4); goto save_state_and_return; };;
      alphaSize = s->nInUse+2;


      case 30: s->state = 30; while (((Bool)1)) { if (s->bsLive >= 3) { UInt32 v; v = (s->bsBuff >> (s->bsLive-3)) & ((1 << 3)-1); s->bsLive -= 3; nGroups = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (nGroups < 2 || nGroups > 6) { retVal = (-4); goto save_state_and_return; };;
      case 31: s->state = 31; while (((Bool)1)) { if (s->bsLive >= 15) { UInt32 v; v = (s->bsBuff >> (s->bsLive-15)) & ((1 << 15)-1); s->bsLive -= 15; nSelectors = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (nSelectors < 1) { retVal = (-4); goto save_state_and_return; };;
      for (i = 0; i < nSelectors; i++) {
         j = 0;
         while (((Bool)1)) {
            case 32: s->state = 32; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
            if (uc == 0) break;
            j++;
            if (j >= nGroups) { retVal = (-4); goto save_state_and_return; };;
         }
         s->selectorMtf[i] = j;
      }


      {
         UChar pos[6], tmp, v;
         for (v = 0; v < nGroups; v++) pos[v] = v;

         for (i = 0; i < nSelectors; i++) {
            v = s->selectorMtf[i];
            tmp = pos[v];
            while (v > 0) { pos[v] = pos[v-1]; v--; }
            pos[0] = tmp;
            s->selector[i] = tmp;
         }
      }


      for (t = 0; t < nGroups; t++) {
         case 33: s->state = 33; while (((Bool)1)) { if (s->bsLive >= 5) { UInt32 v; v = (s->bsBuff >> (s->bsLive-5)) & ((1 << 5)-1); s->bsLive -= 5; curr = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
         for (i = 0; i < alphaSize; i++) {
            while (((Bool)1)) {
               if (curr < 1 || curr > 20) { retVal = (-4); goto save_state_and_return; };;
               case 34: s->state = 34; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
               if (uc == 0) break;
               case 35: s->state = 35; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
               if (uc == 0) curr++; else curr--;
            }
            s->len[t][i] = curr;
         }
      }


      for (t = 0; t < nGroups; t++) {
         minLen = 32;
         maxLen = 0;
         for (i = 0; i < alphaSize; i++) {
            if (s->len[t][i] > maxLen) maxLen = s->len[t][i];
            if (s->len[t][i] < minLen) minLen = s->len[t][i];
         }
         BZ2_hbCreateDecodeTables (
            &(s->limit[t][0]),
            &(s->base[t][0]),
            &(s->perm[t][0]),
            &(s->len[t][0]),
            minLen, maxLen, alphaSize
         );
         s->minLens[t] = minLen;
      }



      EOB = s->nInUse+1;
      nblockMAX = 100000 * s->blockSize100k;
      groupNo = -1;
      groupPos = 0;

      for (i = 0; i <= 255; i++) s->unzftab[i] = 0;


      {
         Int32 ii, jj, kk;
         kk = 4096 -1;
         for (ii = 256 / 16 - 1; ii >= 0; ii--) {
            for (jj = 16 -1; jj >= 0; jj--) {
               s->mtfa[kk] = (UChar)(ii * 16 + jj);
               kk--;
            }
            s->mtfbase[ii] = kk + 1;
         }
      }


      nblock = 0;
      { if (groupPos == 0) { groupNo++; if (groupNo >= nSelectors) { retVal = (-4); goto save_state_and_return; };; groupPos = 50; gSel = s->selector[groupNo]; gMinlen = s->minLens[gSel]; gLimit = &(s->limit[gSel][0]); gPerm = &(s->perm[gSel][0]); gBase = &(s->base[gSel][0]); } groupPos--; zn = gMinlen; case 36: s->state = 36; while (((Bool)1)) { if (s->bsLive >= zn) { UInt32 v; v = (s->bsBuff >> (s->bsLive-zn)) & ((1 << zn)-1); s->bsLive -= zn; zvec = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }; while (1) { if (zn > 20 ) { retVal = (-4); goto save_state_and_return; };; if (zvec <= gLimit[zn]) break; zn++; case 37: s->state = 37; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; zj = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }; zvec = (zvec << 1) | zj; }; if (zvec - gBase[zn] < 0 || zvec - gBase[zn] >= 258) { retVal = (-4); goto save_state_and_return; };; nextSym = gPerm[zvec - gBase[zn]]; };

      while (((Bool)1)) {

         if (nextSym == EOB) break;

         if (nextSym == 0 || nextSym == 1) {

            es = -1;
            N = 1;
            do {
               if (nextSym == 0) es = es + (0+1) * N; else
               if (nextSym == 1) es = es + (1+1) * N;
               N = N * 2;
               { if (groupPos == 0) { groupNo++; if (groupNo >= nSelectors) { retVal = (-4); goto save_state_and_return; };; groupPos = 50; gSel = s->selector[groupNo]; gMinlen = s->minLens[gSel]; gLimit = &(s->limit[gSel][0]); gPerm = &(s->perm[gSel][0]); gBase = &(s->base[gSel][0]); } groupPos--; zn = gMinlen; case 38: s->state = 38; while (((Bool)1)) { if (s->bsLive >= zn) { UInt32 v; v = (s->bsBuff >> (s->bsLive-zn)) & ((1 << zn)-1); s->bsLive -= zn; zvec = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }; while (1) { if (zn > 20 ) { retVal = (-4); goto save_state_and_return; };; if (zvec <= gLimit[zn]) break; zn++; case 39: s->state = 39; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; zj = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }; zvec = (zvec << 1) | zj; }; if (zvec - gBase[zn] < 0 || zvec - gBase[zn] >= 258) { retVal = (-4); goto save_state_and_return; };; nextSym = gPerm[zvec - gBase[zn]]; };
            }
               while (nextSym == 0 || nextSym == 1);

            es++;
            uc = s->seqToUnseq[ s->mtfa[s->mtfbase[0]] ];
            s->unzftab[uc] += es;

            if (s->smallDecompress)
               while (es > 0) {
                  if (nblock >= nblockMAX) { retVal = (-4); goto save_state_and_return; };;
                  s->ll16[nblock] = (UInt16)uc;
                  nblock++;
                  es--;
               }
            else
               while (es > 0) {
                  if (nblock >= nblockMAX) { retVal = (-4); goto save_state_and_return; };;
                  s->tt[nblock] = (UInt32)uc;
                  nblock++;
                  es--;
               };

            continue;

         } else {

            if (nblock >= nblockMAX) { retVal = (-4); goto save_state_and_return; };;


            {
               Int32 ii, jj, kk, pp, lno, off;
               UInt32 nn;
               nn = (UInt32)(nextSym - 1);

               if (nn < 16) {

                  pp = s->mtfbase[0];
                  uc = s->mtfa[pp+nn];
                  while (nn > 3) {
                     Int32 z = pp+nn;
                     s->mtfa[(z) ] = s->mtfa[(z)-1];
                     s->mtfa[(z)-1] = s->mtfa[(z)-2];
                     s->mtfa[(z)-2] = s->mtfa[(z)-3];
                     s->mtfa[(z)-3] = s->mtfa[(z)-4];
                     nn -= 4;
                  }
                  while (nn > 0) {
                     s->mtfa[(pp+nn)] = s->mtfa[(pp+nn)-1]; nn--;
                  };
                  s->mtfa[pp] = uc;
               } else {

                  lno = nn / 16;
                  off = nn % 16;
                  pp = s->mtfbase[lno] + off;
                  uc = s->mtfa[pp];
                  while (pp > s->mtfbase[lno]) {
                     s->mtfa[pp] = s->mtfa[pp-1]; pp--;
                  };
                  s->mtfbase[lno]++;
                  while (lno > 0) {
                     s->mtfbase[lno]--;
                     s->mtfa[s->mtfbase[lno]]
                        = s->mtfa[s->mtfbase[lno-1] + 16 - 1];
                     lno--;
                  }
                  s->mtfbase[0]--;
                  s->mtfa[s->mtfbase[0]] = uc;
                  if (s->mtfbase[0] == 0) {
                     kk = 4096 -1;
                     for (ii = 256 / 16 -1; ii >= 0; ii--) {
                        for (jj = 16 -1; jj >= 0; jj--) {
                           s->mtfa[kk] = s->mtfa[s->mtfbase[ii] + jj];
                           kk--;
                        }
                        s->mtfbase[ii] = kk + 1;
                     }
                  }
               }
            }


            s->unzftab[s->seqToUnseq[uc]]++;
            if (s->smallDecompress)
               s->ll16[nblock] = (UInt16)(s->seqToUnseq[uc]); else
               s->tt[nblock] = (UInt32)(s->seqToUnseq[uc]);
            nblock++;

            { if (groupPos == 0) { groupNo++; if (groupNo >= nSelectors) { retVal = (-4); goto save_state_and_return; };; groupPos = 50; gSel = s->selector[groupNo]; gMinlen = s->minLens[gSel]; gLimit = &(s->limit[gSel][0]); gPerm = &(s->perm[gSel][0]); gBase = &(s->base[gSel][0]); } groupPos--; zn = gMinlen; case 40: s->state = 40; while (((Bool)1)) { if (s->bsLive >= zn) { UInt32 v; v = (s->bsBuff >> (s->bsLive-zn)) & ((1 << zn)-1); s->bsLive -= zn; zvec = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }; while (1) { if (zn > 20 ) { retVal = (-4); goto save_state_and_return; };; if (zvec <= gLimit[zn]) break; zn++; case 41: s->state = 41; while (((Bool)1)) { if (s->bsLive >= 1) { UInt32 v; v = (s->bsBuff >> (s->bsLive-1)) & ((1 << 1)-1); s->bsLive -= 1; zj = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; }; zvec = (zvec << 1) | zj; }; if (zvec - gBase[zn] < 0 || zvec - gBase[zn] >= 258) { retVal = (-4); goto save_state_and_return; };; nextSym = gPerm[zvec - gBase[zn]]; };
            continue;
         }
      }




      if (s->origPtr < 0 || s->origPtr >= nblock)
         { retVal = (-4); goto save_state_and_return; };;


      s->cftab[0] = 0;
      for (i = 1; i <= 256; i++) s->cftab[i] = s->unzftab[i-1];
      for (i = 1; i <= 256; i++) s->cftab[i] += s->cftab[i-1];
      for (i = 0; i <= 256; i++) {
         if (s->cftab[i] < 0 || s->cftab[i] > nblock) {

            { retVal = (-4); goto save_state_and_return; };;
         }
      }

      s->state_out_len = 0;
      s->state_out_ch = 0;
      { s->calculatedBlockCRC = 0xffffffffL; };
      s->state = 2;
      if (s->verbosity >= 2) vex_printf("rt+rld");

      if (s->smallDecompress) {


         for (i = 0; i <= 256; i++) s->cftabCopy[i] = s->cftab[i];


         for (i = 0; i < nblock; i++) {
            uc = (UChar)(s->ll16[i]);
            { s->ll16[i] = (UInt16)(s->cftabCopy[uc] & 0x0000ffff); { if (((i) & 0x1) == 0) s->ll4[(i) >> 1] = (s->ll4[(i) >> 1] & 0xf0) | (s->cftabCopy[uc] >> 16); else s->ll4[(i) >> 1] = (s->ll4[(i) >> 1] & 0x0f) | ((s->cftabCopy[uc] >> 16) << 4); }; };
            s->cftabCopy[uc]++;
         }


         i = s->origPtr;
         j = (((UInt32)s->ll16[i]) | (((((UInt32)(s->ll4[(i) >> 1])) >> (((i) << 2) & 0x4)) & 0xF) << 16));
         do {
            Int32 tmp = (((UInt32)s->ll16[j]) | (((((UInt32)(s->ll4[(j) >> 1])) >> (((j) << 2) & 0x4)) & 0xF) << 16));
            { s->ll16[j] = (UInt16)(i & 0x0000ffff); { if (((j) & 0x1) == 0) s->ll4[(j) >> 1] = (s->ll4[(j) >> 1] & 0xf0) | (i >> 16); else s->ll4[(j) >> 1] = (s->ll4[(j) >> 1] & 0x0f) | ((i >> 16) << 4); }; };
            i = j;
            j = tmp;
         }
            while (i != s->origPtr);

         s->tPos = s->origPtr;
         s->nblock_used = 0;
         if (s->blockRandomised) {
            s->rNToGo = 0; s->rTPos = 0;
            s->k0 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; s->nblock_used++;
            if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;; s->k0 ^= ((s->rNToGo == 1) ? 1 : 0);
         } else {
            s->k0 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; s->nblock_used++;
         }

      } else {


         for (i = 0; i < nblock; i++) {
            uc = (UChar)(s->tt[i] & 0xff);
            s->tt[s->cftab[uc]] |= (i << 8);
            s->cftab[uc]++;
         }

         s->tPos = s->tt[s->origPtr] >> 8;
         s->nblock_used = 0;
         if (s->blockRandomised) {
            s->rNToGo = 0; s->rTPos = 0;
            s->tPos = s->tt[s->tPos]; s->k0 = (UChar)(s->tPos & 0xff); s->tPos >>= 8;; s->nblock_used++;
            if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;; s->k0 ^= ((s->rNToGo == 1) ? 1 : 0);
         } else {
            s->tPos = s->tt[s->tPos]; s->k0 = (UChar)(s->tPos & 0xff); s->tPos >>= 8;; s->nblock_used++;
         }

      }

      { retVal = 0; goto save_state_and_return; };;



    endhdr_2:

      case 42: s->state = 42; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x72) { retVal = (-4); goto save_state_and_return; };;
      case 43: s->state = 43; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x45) { retVal = (-4); goto save_state_and_return; };;
      case 44: s->state = 44; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x38) { retVal = (-4); goto save_state_and_return; };;
      case 45: s->state = 45; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x50) { retVal = (-4); goto save_state_and_return; };;
      case 46: s->state = 46; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      if (uc != 0x90) { retVal = (-4); goto save_state_and_return; };;

      s->storedCombinedCRC = 0;
      case 47: s->state = 47; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);
      case 48: s->state = 48; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);
      case 49: s->state = 49; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);
      case 50: s->state = 50; while (((Bool)1)) { if (s->bsLive >= 8) { UInt32 v; v = (s->bsBuff >> (s->bsLive-8)) & ((1 << 8)-1); s->bsLive -= 8; uc = v; break; } if (s->strm->avail_in == 0) { retVal = 0; goto save_state_and_return; };; s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->strm->next_in)))); s->bsLive += 8; s->strm->next_in++; s->strm->avail_in--; s->strm->total_in_lo32++; if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++; };
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);

      s->state = 1;
      { retVal = 4; goto save_state_and_return; };;

      default: { if (!(((Bool)0))) bz_internal_error ( 4001 ); };
   }

   { if (!(((Bool)0))) bz_internal_error ( 4002 ); };

   save_state_and_return:

   s->save_i = i;
   s->save_j = j;
   s->save_t = t;
   s->save_alphaSize = alphaSize;
   s->save_nGroups = nGroups;
   s->save_nSelectors = nSelectors;
   s->save_EOB = EOB;
   s->save_groupNo = groupNo;
   s->save_groupPos = groupPos;
   s->save_nextSym = nextSym;
   s->save_nblockMAX = nblockMAX;
   s->save_nblock = nblock;
   s->save_es = es;
   s->save_N = N;
   s->save_curr = curr;
   s->save_zt = zt;
   s->save_zn = zn;
   s->save_zvec = zvec;
   s->save_zj = zj;
   s->save_gSel = gSel;
   s->save_gMinlen = gMinlen;
   s->save_gLimit = gLimit;
   s->save_gBase = gBase;
   s->save_gPerm = gPerm;

   return retVal;
}

static
__inline__
void fallbackSimpleSort ( UInt32* fmap,
                          UInt32* eclass,
                          Int32 lo,
                          Int32 hi )
{
   Int32 i, j, tmp;
   UInt32 ec_tmp;

   if (lo == hi) return;

   if (hi - lo > 3) {
      for ( i = hi-4; i >= lo; i-- ) {
         tmp = fmap[i];
         ec_tmp = eclass[tmp];
         for ( j = i+4; j <= hi && ec_tmp > eclass[fmap[j]]; j += 4 )
            fmap[j-4] = fmap[j];
         fmap[j-4] = tmp;
      }
   }

   for ( i = hi-1; i >= lo; i-- ) {
      tmp = fmap[i];
      ec_tmp = eclass[tmp];
      for ( j = i+1; j <= hi && ec_tmp > eclass[fmap[j]]; j++ )
         fmap[j-1] = fmap[j];
      fmap[j-1] = tmp;
   }
}

static
void fallbackQSort3 ( UInt32* fmap,
                      UInt32* eclass,
                      Int32 loSt,
                      Int32 hiSt )
{
   Int32 unLo, unHi, ltLo, gtHi, n, m;
   Int32 sp, lo, hi;
   UInt32 med, r, r3;
   Int32 stackLo[100];
   Int32 stackHi[100];

   r = 0;

   sp = 0;
   { stackLo[sp] = loSt; stackHi[sp] = hiSt; sp++; };

   while (sp > 0) {

      { if (!(sp < 100)) bz_internal_error ( 1004 ); };

      { sp--; lo = stackLo[sp]; hi = stackHi[sp]; };
      if (hi - lo < 10) {
         fallbackSimpleSort ( fmap, eclass, lo, hi );
         continue;
      }

      r = ((r * 7621) + 1) % 32768;
      r3 = r % 3;
      if (r3 == 0) med = eclass[fmap[lo]]; else
      if (r3 == 1) med = eclass[fmap[(lo+hi)>>1]]; else
                   med = eclass[fmap[hi]];

      unLo = ltLo = lo;
      unHi = gtHi = hi;

      while (1) {
         while (1) {
            if (unLo > unHi) break;
            n = (Int32)eclass[fmap[unLo]] - (Int32)med;
            if (n == 0) {
               { Int32 zztmp = fmap[unLo]; fmap[unLo] = fmap[ltLo]; fmap[ltLo] = zztmp; };
               ltLo++; unLo++;
               continue;
            };
            if (n > 0) break;
            unLo++;
         }
         while (1) {
            if (unLo > unHi) break;
            n = (Int32)eclass[fmap[unHi]] - (Int32)med;
            if (n == 0) {
               { Int32 zztmp = fmap[unHi]; fmap[unHi] = fmap[gtHi]; fmap[gtHi] = zztmp; };
               gtHi--; unHi--;
               continue;
            };
            if (n < 0) break;
            unHi--;
         }
         if (unLo > unHi) break;
         { Int32 zztmp = fmap[unLo]; fmap[unLo] = fmap[unHi]; fmap[unHi] = zztmp; }; unLo++; unHi--;
      }

      ;

      if (gtHi < ltLo) continue;

      n = ((ltLo-lo) < (unLo-ltLo)) ? (ltLo-lo) : (unLo-ltLo); { Int32 yyp1 = (lo); Int32 yyp2 = (unLo-n); Int32 yyn = (n); while (yyn > 0) { { Int32 zztmp = fmap[yyp1]; fmap[yyp1] = fmap[yyp2]; fmap[yyp2] = zztmp; }; yyp1++; yyp2++; yyn--; } };
      m = ((hi-gtHi) < (gtHi-unHi)) ? (hi-gtHi) : (gtHi-unHi); { Int32 yyp1 = (unLo); Int32 yyp2 = (hi-m+1); Int32 yyn = (m); while (yyn > 0) { { Int32 zztmp = fmap[yyp1]; fmap[yyp1] = fmap[yyp2]; fmap[yyp2] = zztmp; }; yyp1++; yyp2++; yyn--; } };

      n = lo + unLo - ltLo - 1;
      m = hi - (gtHi - unHi) + 1;

      if (n - lo > hi - m) {
         { stackLo[sp] = lo; stackHi[sp] = n; sp++; };
         { stackLo[sp] = m; stackHi[sp] = hi; sp++; };
      } else {
         { stackLo[sp] = m; stackHi[sp] = hi; sp++; };
         { stackLo[sp] = lo; stackHi[sp] = n; sp++; };
      }
   }
}

static
void fallbackSort ( UInt32* fmap,
                    UInt32* eclass,
                    UInt32* bhtab,
                    Int32 nblock,
                    Int32 verb )
{
   Int32 ftab[257];
   Int32 ftabCopy[256];
   Int32 H, i, j, k, l, r, cc, cc1;
   Int32 nNotDone;
   Int32 nBhtab;
   UChar* eclass8 = (UChar*)eclass;





   if (verb >= 4)
      vex_printf("        bucket sorting ...\n");
   for (i = 0; i < 257; i++) ftab[i] = 0;
   for (i = 0; i < nblock; i++) ftab[eclass8[i]]++;
   for (i = 0; i < 256; i++) ftabCopy[i] = ftab[i];
   for (i = 1; i < 257; i++) ftab[i] += ftab[i-1];

   for (i = 0; i < nblock; i++) {
      j = eclass8[i];
      k = ftab[j] - 1;
      ftab[j] = k;
      fmap[k] = i;
   }

   nBhtab = 2 + (nblock / 32);
   for (i = 0; i < nBhtab; i++) bhtab[i] = 0;
   for (i = 0; i < 256; i++) bhtab[(ftab[i]) >> 5] |= (1 << ((ftab[i]) & 31));

   for (i = 0; i < 32; i++) {
      bhtab[(nblock + 2*i) >> 5] |= (1 << ((nblock + 2*i) & 31));
      bhtab[(nblock + 2*i + 1) >> 5] &= ~(1 << ((nblock + 2*i + 1) & 31));
   }


   H = 1;
   while (1) {

      if (verb >= 4)
         vex_printf("        depth %6d has ",H);

      j = 0;
      for (i = 0; i < nblock; i++) {
         if ((bhtab[(i) >> 5] & (1 << ((i) & 31)))) j = i;
         k = fmap[i] - H; if (k < 0) k += nblock;
         eclass[k] = j;
      }

      nNotDone = 0;
      r = -1;
      while (1) {


         k = r + 1;
         while ((bhtab[(k) >> 5] & (1 << ((k) & 31))) && ((k) & 0x01f)) k++;
         if ((bhtab[(k) >> 5] & (1 << ((k) & 31)))) {
            while (bhtab[(k) >> 5] == 0xffffffff) k += 32;
            while ((bhtab[(k) >> 5] & (1 << ((k) & 31)))) k++;
         }
         l = k - 1;
         if (l >= nblock) break;
         while (!(bhtab[(k) >> 5] & (1 << ((k) & 31))) && ((k) & 0x01f)) k++;
         if (!(bhtab[(k) >> 5] & (1 << ((k) & 31)))) {
            while (bhtab[(k) >> 5] == 0x00000000) k += 32;
            while (!(bhtab[(k) >> 5] & (1 << ((k) & 31)))) k++;
         }
         r = k - 1;
         if (r >= nblock) break;


         if (r > l) {
            nNotDone += (r - l + 1);
            fallbackQSort3 ( fmap, eclass, l, r );


            cc = -1;
            for (i = l; i <= r; i++) {
               cc1 = eclass[fmap[i]];
               if (cc != cc1) { bhtab[(i) >> 5] |= (1 << ((i) & 31)); cc = cc1; };
            }
         }
      }

      if (verb >= 4)
         vex_printf("%6d unresolved strings\n",nNotDone);

      H *= 2;
      if (H > nblock || nNotDone == 0) break;
   }






   if (verb >= 4)
      vex_printf("        reconstructing block ...\n");
   j = 0;
   for (i = 0; i < nblock; i++) {
      while (ftabCopy[j] == 0) j++;
      ftabCopy[j]--;
      eclass8[fmap[i]] = (UChar)j;
   }
   { if (!(j < 256)) bz_internal_error ( 1005 ); };
}

static
__inline__
Bool mainGtU ( UInt32 i1,
               UInt32 i2,
               UChar* block,
               UInt16* quadrant,
               UInt32 nblock,
               Int32* budget )
{
   Int32 k;
   UChar c1, c2;
   UInt16 s1, s2;

   ;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   c1 = block[i1]; c2 = block[i2];
   if (c1 != c2) return (c1 > c2);
   i1++; i2++;

   k = nblock + 8;

   do {

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      c1 = block[i1]; c2 = block[i2];
      if (c1 != c2) return (c1 > c2);
      s1 = quadrant[i1]; s2 = quadrant[i2];
      if (s1 != s2) return (s1 > s2);
      i1++; i2++;

      if (i1 >= nblock) i1 -= nblock;
      if (i2 >= nblock) i2 -= nblock;

      k -= 8;
      (*budget)--;
   }
      while (k >= 0);

   return ((Bool)0);
}

static
Int32 incs[14] = { 1, 4, 13, 40, 121, 364, 1093, 3280,
                   9841, 29524, 88573, 265720,
                   797161, 2391484 };

static
void mainSimpleSort ( UInt32* ptr,
                      UChar* block,
                      UInt16* quadrant,
                      Int32 nblock,
                      Int32 lo,
                      Int32 hi,
                      Int32 d,
                      Int32* budget )
{
   Int32 i, j, h, bigN, hp;
   UInt32 v;

   bigN = hi - lo + 1;
   if (bigN < 2) return;

   hp = 0;
   while (incs[hp] < bigN) hp++;
   hp--;

   for (; hp >= 0; hp--) {
      h = incs[hp];

      i = lo + h;
      while (((Bool)1)) {


         if (i > hi) break;
         v = ptr[i];
         j = i;
         while ( mainGtU (
                    ptr[j-h]+d, v+d, block, quadrant, nblock, budget
                 ) ) {
            ptr[j] = ptr[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         ptr[j] = v;
         i++;


         if (i > hi) break;
         v = ptr[i];
         j = i;
         while ( mainGtU (
                    ptr[j-h]+d, v+d, block, quadrant, nblock, budget
                 ) ) {
            ptr[j] = ptr[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         ptr[j] = v;
         i++;


         if (i > hi) break;
         v = ptr[i];
         j = i;
         while ( mainGtU (
                    ptr[j-h]+d, v+d, block, quadrant, nblock, budget
                 ) ) {
            ptr[j] = ptr[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         ptr[j] = v;
         i++;

         if (*budget < 0) return;
      }
   }
}

static
__inline__
UChar mmed3 ( UChar a, UChar b, UChar c )
{
   UChar t;
   if (a > b) { t = a; a = b; b = t; };
   if (b > c) {
      b = c;
      if (a > b) b = a;
   }
   return b;
}

static
void mainQSort3 ( UInt32* ptr,
                  UChar* block,
                  UInt16* quadrant,
                  Int32 nblock,
                  Int32 loSt,
                  Int32 hiSt,
                  Int32 dSt,
                  Int32* budget )
{
   Int32 unLo, unHi, ltLo, gtHi, n, m, med;
   Int32 sp, lo, hi, d;

   Int32 stackLo[100];
   Int32 stackHi[100];
   Int32 stackD [100];

   Int32 nextLo[3];
   Int32 nextHi[3];
   Int32 nextD [3];

   sp = 0;
   { stackLo[sp] = loSt; stackHi[sp] = hiSt; stackD [sp] = dSt; sp++; };

   while (sp > 0) {

      { if (!(sp < 100)) bz_internal_error ( 1001 ); };

      { sp--; lo = stackLo[sp]; hi = stackHi[sp]; d = stackD [sp]; };
      if (hi - lo < 20 ||
          d > (2 + 12)) {
         mainSimpleSort ( ptr, block, quadrant, nblock, lo, hi, d, budget );
         if (*budget < 0) return;
         continue;
      }

      med = (Int32)
            mmed3 ( block[ptr[ lo ]+d],
                    block[ptr[ hi ]+d],
                    block[ptr[ (lo+hi)>>1 ]+d] );

      unLo = ltLo = lo;
      unHi = gtHi = hi;

      while (((Bool)1)) {
         while (((Bool)1)) {
            if (unLo > unHi) break;
            n = ((Int32)block[ptr[unLo]+d]) - med;
            if (n == 0) {
               { Int32 zztmp = ptr[unLo]; ptr[unLo] = ptr[ltLo]; ptr[ltLo] = zztmp; };
               ltLo++; unLo++; continue;
            };
            if (n > 0) break;
            unLo++;
         }
         while (((Bool)1)) {
            if (unLo > unHi) break;
            n = ((Int32)block[ptr[unHi]+d]) - med;
            if (n == 0) {
               { Int32 zztmp = ptr[unHi]; ptr[unHi] = ptr[gtHi]; ptr[gtHi] = zztmp; };
               gtHi--; unHi--; continue;
            };
            if (n < 0) break;
            unHi--;
         }
         if (unLo > unHi) break;
         { Int32 zztmp = ptr[unLo]; ptr[unLo] = ptr[unHi]; ptr[unHi] = zztmp; }; unLo++; unHi--;
      }

      ;

      if (gtHi < ltLo) {
         { stackLo[sp] = lo; stackHi[sp] = hi; stackD [sp] = d+1; sp++; };
         continue;
      }

      n = ((ltLo-lo) < (unLo-ltLo)) ? (ltLo-lo) : (unLo-ltLo); { Int32 yyp1 = (lo); Int32 yyp2 = (unLo-n); Int32 yyn = (n); while (yyn > 0) { { Int32 zztmp = ptr[yyp1]; ptr[yyp1] = ptr[yyp2]; ptr[yyp2] = zztmp; }; yyp1++; yyp2++; yyn--; } };
      m = ((hi-gtHi) < (gtHi-unHi)) ? (hi-gtHi) : (gtHi-unHi); { Int32 yyp1 = (unLo); Int32 yyp2 = (hi-m+1); Int32 yyn = (m); while (yyn > 0) { { Int32 zztmp = ptr[yyp1]; ptr[yyp1] = ptr[yyp2]; ptr[yyp2] = zztmp; }; yyp1++; yyp2++; yyn--; } };

      n = lo + unLo - ltLo - 1;
      m = hi - (gtHi - unHi) + 1;

      nextLo[0] = lo; nextHi[0] = n; nextD[0] = d;
      nextLo[1] = m; nextHi[1] = hi; nextD[1] = d;
      nextLo[2] = n+1; nextHi[2] = m-1; nextD[2] = d+1;

      if ((nextHi[0]-nextLo[0]) < (nextHi[1]-nextLo[1])) { Int32 tz; tz = nextLo[0]; nextLo[0] = nextLo[1]; nextLo[1] = tz; tz = nextHi[0]; nextHi[0] = nextHi[1]; nextHi[1] = tz; tz = nextD [0]; nextD [0] = nextD [1]; nextD [1] = tz; };
      if ((nextHi[1]-nextLo[1]) < (nextHi[2]-nextLo[2])) { Int32 tz; tz = nextLo[1]; nextLo[1] = nextLo[2]; nextLo[2] = tz; tz = nextHi[1]; nextHi[1] = nextHi[2]; nextHi[2] = tz; tz = nextD [1]; nextD [1] = nextD [2]; nextD [2] = tz; };
      if ((nextHi[0]-nextLo[0]) < (nextHi[1]-nextLo[1])) { Int32 tz; tz = nextLo[0]; nextLo[0] = nextLo[1]; nextLo[1] = tz; tz = nextHi[0]; nextHi[0] = nextHi[1]; nextHi[1] = tz; tz = nextD [0]; nextD [0] = nextD [1]; nextD [1] = tz; };

      ;
      ;

      { stackLo[sp] = nextLo[0]; stackHi[sp] = nextHi[0]; stackD [sp] = nextD[0]; sp++; };
      { stackLo[sp] = nextLo[1]; stackHi[sp] = nextHi[1]; stackD [sp] = nextD[1]; sp++; };
      { stackLo[sp] = nextLo[2]; stackHi[sp] = nextHi[2]; stackD [sp] = nextD[2]; sp++; };
   }
}

static
void mainSort ( UInt32* ptr,
                UChar* block,
                UInt16* quadrant,
                UInt32* ftab,
                Int32 nblock,
                Int32 verb,
                Int32* budget )
{
   Int32 i, j, k, ss, sb;
   Int32 runningOrder[256];
   Bool bigDone[256];
   Int32 copyStart[256];
   Int32 copyEnd [256];
   UChar c1;
   Int32 numQSorted;
   UInt16 s;
   if (verb >= 4) vex_printf("        main sort initialise ...\n");


   for (i = 65536; i >= 0; i--) ftab[i] = 0;

   j = block[0] << 8;
   i = nblock-1;
   for (; i >= 3; i -= 4) {
      quadrant[i] = 0;
      j = (j >> 8) | ( ((UInt16)block[i]) << 8);
      ftab[j]++;
      quadrant[i-1] = 0;
      j = (j >> 8) | ( ((UInt16)block[i-1]) << 8);
      ftab[j]++;
      quadrant[i-2] = 0;
      j = (j >> 8) | ( ((UInt16)block[i-2]) << 8);
      ftab[j]++;
      quadrant[i-3] = 0;
      j = (j >> 8) | ( ((UInt16)block[i-3]) << 8);
      ftab[j]++;
   }
   for (; i >= 0; i--) {
      quadrant[i] = 0;
      j = (j >> 8) | ( ((UInt16)block[i]) << 8);
      ftab[j]++;
   }


   for (i = 0; i < (2 + 12 + 18 + 2); i++) {
      block [nblock+i] = block[i];
      quadrant[nblock+i] = 0;
   }

   if (verb >= 4) vex_printf("        bucket sorting ...\n");


   for (i = 1; i <= 65536; i++) ftab[i] += ftab[i-1];

   s = block[0] << 8;
   i = nblock-1;
   for (; i >= 3; i -= 4) {
      s = (s >> 8) | (block[i] << 8);
      j = ftab[s] -1;
      ftab[s] = j;
      ptr[j] = i;
      s = (s >> 8) | (block[i-1] << 8);
      j = ftab[s] -1;
      ftab[s] = j;
      ptr[j] = i-1;
      s = (s >> 8) | (block[i-2] << 8);
      j = ftab[s] -1;
      ftab[s] = j;
      ptr[j] = i-2;
      s = (s >> 8) | (block[i-3] << 8);
      j = ftab[s] -1;
      ftab[s] = j;
      ptr[j] = i-3;
   }
   for (; i >= 0; i--) {
      s = (s >> 8) | (block[i] << 8);
      j = ftab[s] -1;
      ftab[s] = j;
      ptr[j] = i;
   }






   for (i = 0; i <= 255; i++) {
      bigDone [i] = ((Bool)0);
      runningOrder[i] = i;
   }

   {
      Int32 vv;
      Int32 h = 1;
      do h = 3 * h + 1; while (h <= 256);
      do {
         h = h / 3;
         for (i = h; i <= 255; i++) {
            vv = runningOrder[i];
            j = i;
            while ( (ftab[((runningOrder[j-h])+1) << 8] - ftab[(runningOrder[j-h]) << 8]) > (ftab[((vv)+1) << 8] - ftab[(vv) << 8]) ) {
               runningOrder[j] = runningOrder[j-h];
               j = j - h;
               if (j <= (h - 1)) goto zero;
            }
            zero:
            runningOrder[j] = vv;
         }
      } while (h != 1);
   }





   numQSorted = 0;

   for (i = 0; i <= 255; i++) {







      ss = runningOrder[i];

      for (j = 0; j <= 255; j++) {
         if (j != ss) {
            sb = (ss << 8) + j;
            if ( ! (ftab[sb] & (1 << 21)) ) {
               Int32 lo = ftab[sb] & (~((1 << 21)));
               Int32 hi = (ftab[sb+1] & (~((1 << 21)))) - 1;
               if (hi > lo) {
                  if (verb >= 4)
                     vex_printf("        qsort [0x%x, 0x%x]   " "done %d   this %d\n",ss,j,numQSorted,hi - lo + 1);


                  mainQSort3 (
                     ptr, block, quadrant, nblock,
                     lo, hi, 2, budget
                  );
                  numQSorted += (hi - lo + 1);
                  if (*budget < 0) return;
               }
            }
            ftab[sb] |= (1 << 21);
         }
      }

      { if (!(!bigDone[ss])) bz_internal_error ( 1006 ); };

      {
         for (j = 0; j <= 255; j++) {
            copyStart[j] = ftab[(j << 8) + ss] & (~((1 << 21)));
            copyEnd [j] = (ftab[(j << 8) + ss + 1] & (~((1 << 21)))) - 1;
         }
         for (j = ftab[ss << 8] & (~((1 << 21))); j < copyStart[ss]; j++) {
            k = ptr[j]-1; if (k < 0) k += nblock;
            c1 = block[k];
            if (!bigDone[c1])
               ptr[ copyStart[c1]++ ] = k;
         }
         for (j = (ftab[(ss+1) << 8] & (~((1 << 21)))) - 1; j > copyEnd[ss]; j--) {
            k = ptr[j]-1; if (k < 0) k += nblock;
            c1 = block[k];
            if (!bigDone[c1])
               ptr[ copyEnd[c1]-- ] = k;
         }
      }

      { if (!((copyStart[ss]-1 == copyEnd[ss]) || (copyStart[ss] == 0 && copyEnd[ss] == nblock-1))) bz_internal_error ( 1007 ); }

      for (j = 0; j <= 255; j++) ftab[(j << 8) + ss] |= (1 << 21);

      bigDone[ss] = ((Bool)1);

      if (i < 255) {
         Int32 bbStart = ftab[ss << 8] & (~((1 << 21)));
         Int32 bbSize = (ftab[(ss+1) << 8] & (~((1 << 21)))) - bbStart;
         Int32 shifts = 0;

         while ((bbSize >> shifts) > 65534) shifts++;

         for (j = bbSize-1; j >= 0; j--) {
            Int32 a2update = ptr[bbStart + j];
            UInt16 qVal = (UInt16)(j >> shifts);
            quadrant[a2update] = qVal;
            if (a2update < (2 + 12 + 18 + 2))
               quadrant[a2update + nblock] = qVal;
         }
         { if (!(((bbSize-1) >> shifts) <= 65535)) bz_internal_error ( 1002 ); };
      }

   }

   if (verb >= 4)
      vex_printf("        %d pointers, %d sorted, %d scanned\n",nblock,numQSorted,nblock - numQSorted);

}

void BZ2_blockSort ( EState* s )
{
   UInt32* ptr = s->ptr;
   UChar* block = s->block;
   UInt32* ftab = s->ftab;
   Int32 nblock = s->nblock;
   Int32 verb = s->verbosity;
   Int32 wfact = s->workFactor;
   UInt16* quadrant;
   Int32 budget;
   Int32 budgetInit;
   Int32 i;

   if (nblock < 1000 ) {
      fallbackSort ( s->arr1, s->arr2, ftab, nblock, verb );
   } else {





      i = nblock+(2 + 12 + 18 + 2);
      if (i & 1) i++;
      quadrant = (UInt16*)(&(block[i]));

      if (wfact < 1 ) wfact = 1;
      if (wfact > 100) wfact = 100;
      budgetInit = nblock * ((wfact-1) / 3);
      budget = budgetInit;

      mainSort ( ptr, block, quadrant, ftab, nblock, verb, &budget );
      if (0 && verb >= 3)
         vex_printf("      %d work, %d block, ratio %5.2f\n",budgetInit - budget,nblock,(float)(budgetInit - budget) / (float)(nblock==0 ? 1 : nblock));




      if (budget < 0) {
         if (verb >= 2)
            vex_printf("    too repetitive; using fallback" " sorting algorithm\n");

         fallbackSort ( s->arr1, s->arr2, ftab, nblock, verb );
      }
   }

   s->origPtr = -1;
   for (i = 0; i < s->nblock; i++)
      if (ptr[i] == 0)
         { s->origPtr = i; break; };

   { if (!(s->origPtr != -1)) bz_internal_error ( 1003 ); };
}

void BZ2_hbMakeCodeLengths ( UChar *len,
                             Int32 *freq,
                             Int32 alphaSize,
                             Int32 maxLen )
{




   Int32 nNodes, nHeap, n1, n2, i, j, k;
   Bool tooLong;

   Int32 heap [ 258 + 2 ];
   Int32 weight [ 258 * 2 ];
   Int32 parent [ 258 * 2 ];

   for (i = 0; i < alphaSize; i++)
      weight[i+1] = (freq[i] == 0 ? 1 : freq[i]) << 8;

   while (((Bool)1)) {

      nNodes = alphaSize;
      nHeap = 0;

      heap[0] = 0;
      weight[0] = 0;
      parent[0] = -2;

      for (i = 1; i <= alphaSize; i++) {
         parent[i] = -1;
         nHeap++;
         heap[nHeap] = i;
         { Int32 zz, tmp; zz = nHeap; tmp = heap[zz]; while (weight[tmp] < weight[heap[zz >> 1]]) { heap[zz] = heap[zz >> 1]; zz >>= 1; } heap[zz] = tmp; };
      }

      { if (!(nHeap < (258 +2))) bz_internal_error ( 2001 ); };

      while (nHeap > 1) {
         n1 = heap[1]; heap[1] = heap[nHeap]; nHeap--; { Int32 zz, yy, tmp; zz = 1; tmp = heap[zz]; while (((Bool)1)) { yy = zz << 1; if (yy > nHeap) break; if (yy < nHeap && weight[heap[yy+1]] < weight[heap[yy]]) yy++; if (weight[tmp] < weight[heap[yy]]) break; heap[zz] = heap[yy]; zz = yy; } heap[zz] = tmp; };
         n2 = heap[1]; heap[1] = heap[nHeap]; nHeap--; { Int32 zz, yy, tmp; zz = 1; tmp = heap[zz]; while (((Bool)1)) { yy = zz << 1; if (yy > nHeap) break; if (yy < nHeap && weight[heap[yy+1]] < weight[heap[yy]]) yy++; if (weight[tmp] < weight[heap[yy]]) break; heap[zz] = heap[yy]; zz = yy; } heap[zz] = tmp; };
         nNodes++;
         parent[n1] = parent[n2] = nNodes;
         weight[nNodes] = (((weight[n1]) & 0xffffff00)+((weight[n2]) & 0xffffff00)) | (1 + ((((weight[n1]) & 0x000000ff)) > (((weight[n2]) & 0x000000ff)) ? (((weight[n1]) & 0x000000ff)) : (((weight[n2]) & 0x000000ff))));
         parent[nNodes] = -1;
         nHeap++;
         heap[nHeap] = nNodes;
         { Int32 zz, tmp; zz = nHeap; tmp = heap[zz]; while (weight[tmp] < weight[heap[zz >> 1]]) { heap[zz] = heap[zz >> 1]; zz >>= 1; } heap[zz] = tmp; };
      }

      { if (!(nNodes < (258 * 2))) bz_internal_error ( 2002 ); };

      tooLong = ((Bool)0);
      for (i = 1; i <= alphaSize; i++) {
         j = 0;
         k = i;
         while (parent[k] >= 0) { k = parent[k]; j++; }
         len[i-1] = j;
         if (j > maxLen) tooLong = ((Bool)1);
      }

      if (! tooLong) break;

      for (i = 1; i <= alphaSize; i++) {
         j = weight[i] >> 8;
         j = 1 + (j / 2);
         weight[i] = j << 8;
      }
   }
}



void BZ2_hbAssignCodes ( Int32 *code,
                         UChar *length,
                         Int32 minLen,
                         Int32 maxLen,
                         Int32 alphaSize )
{
   Int32 n, vec, i;

   vec = 0;
   for (n = minLen; n <= maxLen; n++) {
      for (i = 0; i < alphaSize; i++)
         if (length[i] == n) { code[i] = vec; vec++; };
      vec <<= 1;
   }
}



void BZ2_hbCreateDecodeTables ( Int32 *limit,
                                Int32 *base,
                                Int32 *perm,
                                UChar *length,
                                Int32 minLen,
                                Int32 maxLen,
                                Int32 alphaSize )
{
   Int32 pp, i, j, vec;

   pp = 0;
   for (i = minLen; i <= maxLen; i++)
      for (j = 0; j < alphaSize; j++)
         if (length[j] == i) { perm[pp] = j; pp++; };

   for (i = 0; i < 23; i++) base[i] = 0;
   for (i = 0; i < alphaSize; i++) base[length[i]+1]++;

   for (i = 1; i < 23; i++) base[i] += base[i-1];

   for (i = 0; i < 23; i++) limit[i] = 0;
   vec = 0;

   for (i = minLen; i <= maxLen; i++) {
      vec += (base[i+1] - base[i]);
      limit[i] = vec-1;
      vec <<= 1;
   }
   for (i = minLen + 1; i <= maxLen; i++)
      base[i] = ((limit[i-1] + 1) << 1) - base[i];
}

void BZ2_bsInitWrite ( EState* s )
{
   s->bsLive = 0;
   s->bsBuff = 0;
}



static
void bsFinishWrite ( EState* s )
{
   while (s->bsLive > 0) {
      s->zbits[s->numZ] = (UChar)(s->bsBuff >> 24);
      s->numZ++;
      s->bsBuff <<= 8;
      s->bsLive -= 8;
   }
}

static
__inline__
void bsW ( EState* s, Int32 n, UInt32 v )
{
   { while (s->bsLive >= 8) { s->zbits[s->numZ] = (UChar)(s->bsBuff >> 24); s->numZ++; s->bsBuff <<= 8; s->bsLive -= 8; } };
   s->bsBuff |= (v << (32 - s->bsLive - n));
   s->bsLive += n;
}



static
void bsPutUInt32 ( EState* s, UInt32 u )
{
   bsW ( s, 8, (u >> 24) & 0xffL );
   bsW ( s, 8, (u >> 16) & 0xffL );
   bsW ( s, 8, (u >> 8) & 0xffL );
   bsW ( s, 8, u & 0xffL );
}



static
void bsPutUChar ( EState* s, UChar c )
{
   bsW( s, 8, (UInt32)c );
}







static
void makeMaps_e ( EState* s )
{
   Int32 i;
   s->nInUse = 0;
   for (i = 0; i < 256; i++)
      if (s->inUse[i]) {
         s->unseqToSeq[i] = s->nInUse;
         s->nInUse++;
      }
}



static
void generateMTFValues ( EState* s )
{
   UChar yy[256];
   Int32 i, j;
   Int32 zPend;
   Int32 wr;
   Int32 EOB;

   UInt32* ptr = s->ptr;
   UChar* block = s->block;
   UInt16* mtfv = s->mtfv;

   makeMaps_e ( s );
   EOB = s->nInUse+1;

   for (i = 0; i <= EOB; i++) s->mtfFreq[i] = 0;

   wr = 0;
   zPend = 0;
   for (i = 0; i < s->nInUse; i++) yy[i] = (UChar) i;

   for (i = 0; i < s->nblock; i++) {
      UChar ll_i;
      ;
      j = ptr[i]-1; if (j < 0) j += s->nblock;
      ll_i = s->unseqToSeq[block[j]];
      ;

      if (yy[0] == ll_i) {
         zPend++;
      } else {

         if (zPend > 0) {
            zPend--;
            while (((Bool)1)) {
               if (zPend & 1) {
                  mtfv[wr] = 1; wr++;
                  s->mtfFreq[1]++;
               } else {
                  mtfv[wr] = 0; wr++;
                  s->mtfFreq[0]++;
               }
               if (zPend < 2) break;
               zPend = (zPend - 2) / 2;
            };
            zPend = 0;
         }
         {
            register UChar rtmp;
            register UChar* ryy_j;
            register UChar rll_i;
            rtmp = yy[1];
            yy[1] = yy[0];
            ryy_j = &(yy[1]);
            rll_i = ll_i;
            while ( rll_i != rtmp ) {
               register UChar rtmp2;
               ryy_j++;
               rtmp2 = rtmp;
               rtmp = *ryy_j;
               *ryy_j = rtmp2;
            };
            yy[0] = rtmp;
            j = ryy_j - &(yy[0]);
            mtfv[wr] = j+1; wr++; s->mtfFreq[j+1]++;
         }

      }
   }

   if (zPend > 0) {
      zPend--;
      while (((Bool)1)) {
         if (zPend & 1) {
            mtfv[wr] = 1; wr++;
            s->mtfFreq[1]++;
         } else {
            mtfv[wr] = 0; wr++;
            s->mtfFreq[0]++;
         }
         if (zPend < 2) break;
         zPend = (zPend - 2) / 2;
      };
      zPend = 0;
   }

   mtfv[wr] = EOB; wr++; s->mtfFreq[EOB]++;

   s->nMTF = wr;
}






static
void sendMTFValues ( EState* s )
{
   Int32 v, t, i, j, gs, ge, totc, bt, bc, iter;
   Int32 nSelectors, alphaSize, minLen, maxLen, selCtr;
   Int32 nGroups, nBytes;

   UInt16 cost[6];
   Int32 fave[6];

   UInt16* mtfv = s->mtfv;

   if (s->verbosity >= 3)
      vex_printf("      %d in block, %d after MTF & 1-2 coding, " "%d+2 syms in use\n",s->nblock,s->nMTF,s->nInUse);



   alphaSize = s->nInUse+2;
   for (t = 0; t < 6; t++)
      for (v = 0; v < alphaSize; v++)
         s->len[t][v] = 15;


   { if (!(s->nMTF > 0)) bz_internal_error ( 3001 ); };
   if (s->nMTF < 200) nGroups = 2; else
   if (s->nMTF < 600) nGroups = 3; else
   if (s->nMTF < 1200) nGroups = 4; else
   if (s->nMTF < 2400) nGroups = 5; else
                       nGroups = 6;


   {
      Int32 nPart, remF, tFreq, aFreq;

      nPart = nGroups;
      remF = s->nMTF;
      gs = 0;
      while (nPart > 0) {
         tFreq = remF / nPart;
         ge = gs-1;
         aFreq = 0;
         while (aFreq < tFreq && ge < alphaSize-1) {
            ge++;
            aFreq += s->mtfFreq[ge];
         }

         if (ge > gs
             && nPart != nGroups && nPart != 1
             && ((nGroups-nPart) % 2 == 1)) {
            aFreq -= s->mtfFreq[ge];
            ge--;
         }

         if (0 && s->verbosity >= 3)
            vex_printf("      initial group %d, [%d .. %d], " "has %d syms (%4.1f%%)\n",nPart,gs,ge,aFreq,(100.0 * (float)aFreq) / (float)(s->nMTF));




         for (v = 0; v < alphaSize; v++)
            if (v >= gs && v <= ge)
               s->len[nPart-1][v] = 0; else
               s->len[nPart-1][v] = 15;

         nPart--;
         gs = ge+1;
         remF -= aFreq;
      }
   }




   for (iter = 0; iter < 4; iter++) {

      for (t = 0; t < nGroups; t++) fave[t] = 0;

      for (t = 0; t < nGroups; t++)
         for (v = 0; v < alphaSize; v++)
            s->rfreq[t][v] = 0;





      if (nGroups == 6) {
         for (v = 0; v < alphaSize; v++) {
            s->len_pack[v][0] = (s->len[1][v] << 16) | s->len[0][v];
            s->len_pack[v][1] = (s->len[3][v] << 16) | s->len[2][v];
            s->len_pack[v][2] = (s->len[5][v] << 16) | s->len[4][v];
  }
      }

      nSelectors = 0;
      totc = 0;
      gs = 0;
      while (((Bool)1)) {


         if (gs >= s->nMTF) break;
         ge = gs + 50 - 1;
         if (ge >= s->nMTF) ge = s->nMTF-1;





         for (t = 0; t < nGroups; t++) cost[t] = 0;

         if (nGroups == 6 && 50 == ge-gs+1) {

            register UInt32 cost01, cost23, cost45;
            register UInt16 icv;
            cost01 = cost23 = cost45 = 0;







            icv = mtfv[gs+(0)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(1)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(2)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(3)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(4)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(5)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(6)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(7)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(8)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(9)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(10)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(11)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(12)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(13)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(14)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(15)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(16)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(17)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(18)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(19)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(20)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(21)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(22)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(23)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(24)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(25)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(26)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(27)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(28)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(29)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(30)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(31)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(32)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(33)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(34)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(35)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(36)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(37)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(38)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(39)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(40)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(41)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(42)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(43)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(44)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;
            icv = mtfv[gs+(45)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(46)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(47)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(48)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];; icv = mtfv[gs+(49)]; cost01 += s->len_pack[icv][0]; cost23 += s->len_pack[icv][1]; cost45 += s->len_pack[icv][2];;



            cost[0] = cost01 & 0xffff; cost[1] = cost01 >> 16;
            cost[2] = cost23 & 0xffff; cost[3] = cost23 >> 16;
            cost[4] = cost45 & 0xffff; cost[5] = cost45 >> 16;

         } else {

            for (i = gs; i <= ge; i++) {
               UInt16 icv = mtfv[i];
               for (t = 0; t < nGroups; t++) cost[t] += s->len[t][icv];
            }
         }





         bc = 999999999; bt = -1;
         for (t = 0; t < nGroups; t++)
            if (cost[t] < bc) { bc = cost[t]; bt = t; };
         totc += bc;
         fave[bt]++;
         s->selector[nSelectors] = bt;
         nSelectors++;




         if (nGroups == 6 && 50 == ge-gs+1) {




            s->rfreq[bt][ mtfv[gs+(0)] ]++; s->rfreq[bt][ mtfv[gs+(1)] ]++; s->rfreq[bt][ mtfv[gs+(2)] ]++; s->rfreq[bt][ mtfv[gs+(3)] ]++; s->rfreq[bt][ mtfv[gs+(4)] ]++;
            s->rfreq[bt][ mtfv[gs+(5)] ]++; s->rfreq[bt][ mtfv[gs+(6)] ]++; s->rfreq[bt][ mtfv[gs+(7)] ]++; s->rfreq[bt][ mtfv[gs+(8)] ]++; s->rfreq[bt][ mtfv[gs+(9)] ]++;
            s->rfreq[bt][ mtfv[gs+(10)] ]++; s->rfreq[bt][ mtfv[gs+(11)] ]++; s->rfreq[bt][ mtfv[gs+(12)] ]++; s->rfreq[bt][ mtfv[gs+(13)] ]++; s->rfreq[bt][ mtfv[gs+(14)] ]++;
            s->rfreq[bt][ mtfv[gs+(15)] ]++; s->rfreq[bt][ mtfv[gs+(16)] ]++; s->rfreq[bt][ mtfv[gs+(17)] ]++; s->rfreq[bt][ mtfv[gs+(18)] ]++; s->rfreq[bt][ mtfv[gs+(19)] ]++;
            s->rfreq[bt][ mtfv[gs+(20)] ]++; s->rfreq[bt][ mtfv[gs+(21)] ]++; s->rfreq[bt][ mtfv[gs+(22)] ]++; s->rfreq[bt][ mtfv[gs+(23)] ]++; s->rfreq[bt][ mtfv[gs+(24)] ]++;
            s->rfreq[bt][ mtfv[gs+(25)] ]++; s->rfreq[bt][ mtfv[gs+(26)] ]++; s->rfreq[bt][ mtfv[gs+(27)] ]++; s->rfreq[bt][ mtfv[gs+(28)] ]++; s->rfreq[bt][ mtfv[gs+(29)] ]++;
            s->rfreq[bt][ mtfv[gs+(30)] ]++; s->rfreq[bt][ mtfv[gs+(31)] ]++; s->rfreq[bt][ mtfv[gs+(32)] ]++; s->rfreq[bt][ mtfv[gs+(33)] ]++; s->rfreq[bt][ mtfv[gs+(34)] ]++;
            s->rfreq[bt][ mtfv[gs+(35)] ]++; s->rfreq[bt][ mtfv[gs+(36)] ]++; s->rfreq[bt][ mtfv[gs+(37)] ]++; s->rfreq[bt][ mtfv[gs+(38)] ]++; s->rfreq[bt][ mtfv[gs+(39)] ]++;
            s->rfreq[bt][ mtfv[gs+(40)] ]++; s->rfreq[bt][ mtfv[gs+(41)] ]++; s->rfreq[bt][ mtfv[gs+(42)] ]++; s->rfreq[bt][ mtfv[gs+(43)] ]++; s->rfreq[bt][ mtfv[gs+(44)] ]++;
            s->rfreq[bt][ mtfv[gs+(45)] ]++; s->rfreq[bt][ mtfv[gs+(46)] ]++; s->rfreq[bt][ mtfv[gs+(47)] ]++; s->rfreq[bt][ mtfv[gs+(48)] ]++; s->rfreq[bt][ mtfv[gs+(49)] ]++;



         } else {

            for (i = gs; i <= ge; i++)
               s->rfreq[bt][ mtfv[i] ]++;
         }

         gs = ge+1;
      }
      if (s->verbosity >= 3) {
         vex_printf("      pass %d: size is %d, grp uses are ",iter+1,totc/8);

         for (t = 0; t < nGroups; t++)
            vex_printf("%d ",fave[t]);
         vex_printf("\n");
      }






      for (t = 0; t < nGroups; t++)
         BZ2_hbMakeCodeLengths ( &(s->len[t][0]), &(s->rfreq[t][0]),
                                 alphaSize, 17 );
   }


   { if (!(nGroups < 8)) bz_internal_error ( 3002 ); };
   { if (!(nSelectors < 32768 && nSelectors <= (2 + (900000 / 50)))) bz_internal_error ( 3003 ); };





   {
      UChar pos[6], ll_i, tmp2, tmp;
      for (i = 0; i < nGroups; i++) pos[i] = i;
      for (i = 0; i < nSelectors; i++) {
         ll_i = s->selector[i];
         j = 0;
         tmp = pos[j];
         while ( ll_i != tmp ) {
            j++;
            tmp2 = tmp;
            tmp = pos[j];
            pos[j] = tmp2;
         };
         pos[0] = tmp;
         s->selectorMtf[i] = j;
      }
   };


   for (t = 0; t < nGroups; t++) {
      minLen = 32;
      maxLen = 0;
      for (i = 0; i < alphaSize; i++) {
         if (s->len[t][i] > maxLen) maxLen = s->len[t][i];
         if (s->len[t][i] < minLen) minLen = s->len[t][i];
      }
      { if (!(!(maxLen > 17 ))) bz_internal_error ( 3004 ); };
      { if (!(!(minLen < 1))) bz_internal_error ( 3005 ); };
      BZ2_hbAssignCodes ( &(s->code[t][0]), &(s->len[t][0]),
                          minLen, maxLen, alphaSize );
   }


   {
      Bool inUse16[16];
      for (i = 0; i < 16; i++) {
          inUse16[i] = ((Bool)0);
          for (j = 0; j < 16; j++)
             if (s->inUse[i * 16 + j]) inUse16[i] = ((Bool)1);
      }

      nBytes = s->numZ;
      for (i = 0; i < 16; i++)
         if (inUse16[i]) bsW(s,1,1); else bsW(s,1,0);

      for (i = 0; i < 16; i++)
         if (inUse16[i])
            for (j = 0; j < 16; j++) {
               if (s->inUse[i * 16 + j]) bsW(s,1,1); else bsW(s,1,0);
            }

      if (s->verbosity >= 3)
         vex_printf("      bytes: mapping %d, ",s->numZ-nBytes);
   }


   nBytes = s->numZ;
   bsW ( s, 3, nGroups );
   bsW ( s, 15, nSelectors );
   for (i = 0; i < nSelectors; i++) {
      for (j = 0; j < s->selectorMtf[i]; j++) bsW(s,1,1);
      bsW(s,1,0);
   }
   if (s->verbosity >= 3)
      vex_printf("selectors %d, ",s->numZ-nBytes);


   nBytes = s->numZ;

   for (t = 0; t < nGroups; t++) {
      Int32 curr = s->len[t][0];
      bsW ( s, 5, curr );
      for (i = 0; i < alphaSize; i++) {
         while (curr < s->len[t][i]) { bsW(s,2,2); curr++; };
         while (curr > s->len[t][i]) { bsW(s,2,3); curr--; };
         bsW ( s, 1, 0 );
      }
   }

   if (s->verbosity >= 3)
      vex_printf("code lengths %d, ",s->numZ-nBytes);


   nBytes = s->numZ;
   selCtr = 0;
   gs = 0;
   while (((Bool)1)) {
      if (gs >= s->nMTF) break;
      ge = gs + 50 - 1;
      if (ge >= s->nMTF) ge = s->nMTF-1;
      { if (!(s->selector[selCtr] < nGroups)) bz_internal_error ( 3006 ); };

      if (nGroups == 6 && 50 == ge-gs+1) {

            UInt16 mtfv_i;
            UChar* s_len_sel_selCtr
               = &(s->len[s->selector[selCtr]][0]);
            Int32* s_code_sel_selCtr
               = &(s->code[s->selector[selCtr]][0]);







            mtfv_i = mtfv[gs+(0)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(1)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(2)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(3)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(4)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(5)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(6)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(7)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(8)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(9)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(10)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(11)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(12)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(13)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(14)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(15)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(16)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(17)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(18)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(19)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(20)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(21)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(22)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(23)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(24)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(25)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(26)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(27)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(28)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(29)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(30)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(31)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(32)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(33)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(34)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(35)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(36)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(37)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(38)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(39)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(40)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(41)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(42)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(43)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(44)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );
            mtfv_i = mtfv[gs+(45)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(46)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(47)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(48)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] ); mtfv_i = mtfv[gs+(49)]; bsW ( s, s_len_sel_selCtr[mtfv_i], s_code_sel_selCtr[mtfv_i] );



      } else {

         for (i = gs; i <= ge; i++) {
            bsW ( s,
                  s->len [s->selector[selCtr]] [mtfv[i]],
                  s->code [s->selector[selCtr]] [mtfv[i]] );
         }
      }


      gs = ge+1;
      selCtr++;
   }
   { if (!(selCtr == nSelectors)) bz_internal_error ( 3007 ); };

   if (s->verbosity >= 3)
      vex_printf("codes %d\n",s->numZ-nBytes);
}



void BZ2_compressBlock ( EState* s, Bool is_last_block )
{
   if (s->nblock > 0) {

      { s->blockCRC = ~(s->blockCRC); };
      s->combinedCRC = (s->combinedCRC << 1) | (s->combinedCRC >> 31);
      s->combinedCRC ^= s->blockCRC;
      if (s->blockNo > 1) s->numZ = 0;

      if (s->verbosity >= 2)
         vex_printf("    block %d: crc = 0x%08x, " "combined CRC = 0x%08x, size = %d\n",s->blockNo,s->blockCRC,s->combinedCRC,s->nblock);



      BZ2_blockSort ( s );
   }

   s->zbits = (UChar*) (&((UChar*)s->arr2)[s->nblock]);


   if (s->blockNo == 1) {
      BZ2_bsInitWrite ( s );
      bsPutUChar ( s, 0x42 );
      bsPutUChar ( s, 0x5a );
      bsPutUChar ( s, 0x68 );
      bsPutUChar ( s, (UChar)(0x30 + s->blockSize100k) );
   }

   if (s->nblock > 0) {

      bsPutUChar ( s, 0x31 ); bsPutUChar ( s, 0x41 );
      bsPutUChar ( s, 0x59 ); bsPutUChar ( s, 0x26 );
      bsPutUChar ( s, 0x53 ); bsPutUChar ( s, 0x59 );


      bsPutUInt32 ( s, s->blockCRC );

      bsW(s,1,0);

      bsW ( s, 24, s->origPtr );
      generateMTFValues ( s );
      sendMTFValues ( s );
   }



   if (is_last_block) {

      bsPutUChar ( s, 0x17 ); bsPutUChar ( s, 0x72 );
      bsPutUChar ( s, 0x45 ); bsPutUChar ( s, 0x38 );
      bsPutUChar ( s, 0x50 ); bsPutUChar ( s, 0x90 );
      bsPutUInt32 ( s, s->combinedCRC );
      if (s->verbosity >= 2)
         vex_printf("    final combined CRC = 0x%08x\n   ",s->combinedCRC);
      bsFinishWrite ( s );
   }
}

Int32 BZ2_rNums[512] = {
   619, 720, 127, 481, 931, 816, 813, 233, 566, 247,
   985, 724, 205, 454, 863, 491, 741, 242, 949, 214,
   733, 859, 335, 708, 621, 574, 73, 654, 730, 472,
   419, 436, 278, 496, 867, 210, 399, 680, 480, 51,
   878, 465, 811, 169, 869, 675, 611, 697, 867, 561,
   862, 687, 507, 283, 482, 129, 807, 591, 733, 623,
   150, 238, 59, 379, 684, 877, 625, 169, 643, 105,
   170, 607, 520, 932, 727, 476, 693, 425, 174, 647,
   73, 122, 335, 530, 442, 853, 695, 249, 445, 515,
   909, 545, 703, 919, 874, 474, 882, 500, 594, 612,
   641, 801, 220, 162, 819, 984, 589, 513, 495, 799,
   161, 604, 958, 533, 221, 400, 386, 867, 600, 782,
   382, 596, 414, 171, 516, 375, 682, 485, 911, 276,
   98, 553, 163, 354, 666, 933, 424, 341, 533, 870,
   227, 730, 475, 186, 263, 647, 537, 686, 600, 224,
   469, 68, 770, 919, 190, 373, 294, 822, 808, 206,
   184, 943, 795, 384, 383, 461, 404, 758, 839, 887,
   715, 67, 618, 276, 204, 918, 873, 777, 604, 560,
   951, 160, 578, 722, 79, 804, 96, 409, 713, 940,
   652, 934, 970, 447, 318, 353, 859, 672, 112, 785,
   645, 863, 803, 350, 139, 93, 354, 99, 820, 908,
   609, 772, 154, 274, 580, 184, 79, 626, 630, 742,
   653, 282, 762, 623, 680, 81, 927, 626, 789, 125,
   411, 521, 938, 300, 821, 78, 343, 175, 128, 250,
   170, 774, 972, 275, 999, 639, 495, 78, 352, 126,
   857, 956, 358, 619, 580, 124, 737, 594, 701, 612,
   669, 112, 134, 694, 363, 992, 809, 743, 168, 974,
   944, 375, 748, 52, 600, 747, 642, 182, 862, 81,
   344, 805, 988, 739, 511, 655, 814, 334, 249, 515,
   897, 955, 664, 981, 649, 113, 974, 459, 893, 228,
   433, 837, 553, 268, 926, 240, 102, 654, 459, 51,
   686, 754, 806, 760, 493, 403, 415, 394, 687, 700,
   946, 670, 656, 610, 738, 392, 760, 799, 887, 653,
   978, 321, 576, 617, 626, 502, 894, 679, 243, 440,
   680, 879, 194, 572, 640, 724, 926, 56, 204, 700,
   707, 151, 457, 449, 797, 195, 791, 558, 945, 679,
   297, 59, 87, 824, 713, 663, 412, 693, 342, 606,
   134, 108, 571, 364, 631, 212, 174, 643, 304, 329,
   343, 97, 430, 751, 497, 314, 983, 374, 822, 928,
   140, 206, 73, 263, 980, 736, 876, 478, 430, 305,
   170, 514, 364, 692, 829, 82, 855, 953, 676, 246,
   369, 970, 294, 750, 807, 827, 150, 790, 288, 923,
   804, 378, 215, 828, 592, 281, 565, 555, 710, 82,
   896, 831, 547, 261, 524, 462, 293, 465, 502, 56,
   661, 821, 976, 991, 658, 869, 905, 758, 745, 193,
   768, 550, 608, 933, 378, 286, 215, 979, 792, 961,
   61, 688, 793, 644, 986, 403, 106, 366, 905, 644,
   372, 567, 466, 434, 645, 210, 389, 550, 919, 135,
   780, 773, 635, 389, 707, 100, 626, 958, 165, 504,
   920, 176, 193, 713, 857, 265, 203, 50, 668, 108,
   645, 990, 626, 197, 510, 357, 358, 850, 858, 364,
   936, 638
};

UInt32 BZ2_crc32Table[256] = {



   0x00000000L, 0x04c11db7L, 0x09823b6eL, 0x0d4326d9L,
   0x130476dcL, 0x17c56b6bL, 0x1a864db2L, 0x1e475005L,
   0x2608edb8L, 0x22c9f00fL, 0x2f8ad6d6L, 0x2b4bcb61L,
   0x350c9b64L, 0x31cd86d3L, 0x3c8ea00aL, 0x384fbdbdL,
   0x4c11db70L, 0x48d0c6c7L, 0x4593e01eL, 0x4152fda9L,
   0x5f15adacL, 0x5bd4b01bL, 0x569796c2L, 0x52568b75L,
   0x6a1936c8L, 0x6ed82b7fL, 0x639b0da6L, 0x675a1011L,
   0x791d4014L, 0x7ddc5da3L, 0x709f7b7aL, 0x745e66cdL,
   0x9823b6e0L, 0x9ce2ab57L, 0x91a18d8eL, 0x95609039L,
   0x8b27c03cL, 0x8fe6dd8bL, 0x82a5fb52L, 0x8664e6e5L,
   0xbe2b5b58L, 0xbaea46efL, 0xb7a96036L, 0xb3687d81L,
   0xad2f2d84L, 0xa9ee3033L, 0xa4ad16eaL, 0xa06c0b5dL,
   0xd4326d90L, 0xd0f37027L, 0xddb056feL, 0xd9714b49L,
   0xc7361b4cL, 0xc3f706fbL, 0xceb42022L, 0xca753d95L,
   0xf23a8028L, 0xf6fb9d9fL, 0xfbb8bb46L, 0xff79a6f1L,
   0xe13ef6f4L, 0xe5ffeb43L, 0xe8bccd9aL, 0xec7dd02dL,
   0x34867077L, 0x30476dc0L, 0x3d044b19L, 0x39c556aeL,
   0x278206abL, 0x23431b1cL, 0x2e003dc5L, 0x2ac12072L,
   0x128e9dcfL, 0x164f8078L, 0x1b0ca6a1L, 0x1fcdbb16L,
   0x018aeb13L, 0x054bf6a4L, 0x0808d07dL, 0x0cc9cdcaL,
   0x7897ab07L, 0x7c56b6b0L, 0x71159069L, 0x75d48ddeL,
   0x6b93dddbL, 0x6f52c06cL, 0x6211e6b5L, 0x66d0fb02L,
   0x5e9f46bfL, 0x5a5e5b08L, 0x571d7dd1L, 0x53dc6066L,
   0x4d9b3063L, 0x495a2dd4L, 0x44190b0dL, 0x40d816baL,
   0xaca5c697L, 0xa864db20L, 0xa527fdf9L, 0xa1e6e04eL,
   0xbfa1b04bL, 0xbb60adfcL, 0xb6238b25L, 0xb2e29692L,
   0x8aad2b2fL, 0x8e6c3698L, 0x832f1041L, 0x87ee0df6L,
   0x99a95df3L, 0x9d684044L, 0x902b669dL, 0x94ea7b2aL,
   0xe0b41de7L, 0xe4750050L, 0xe9362689L, 0xedf73b3eL,
   0xf3b06b3bL, 0xf771768cL, 0xfa325055L, 0xfef34de2L,
   0xc6bcf05fL, 0xc27dede8L, 0xcf3ecb31L, 0xcbffd686L,
   0xd5b88683L, 0xd1799b34L, 0xdc3abdedL, 0xd8fba05aL,
   0x690ce0eeL, 0x6dcdfd59L, 0x608edb80L, 0x644fc637L,
   0x7a089632L, 0x7ec98b85L, 0x738aad5cL, 0x774bb0ebL,
   0x4f040d56L, 0x4bc510e1L, 0x46863638L, 0x42472b8fL,
   0x5c007b8aL, 0x58c1663dL, 0x558240e4L, 0x51435d53L,
   0x251d3b9eL, 0x21dc2629L, 0x2c9f00f0L, 0x285e1d47L,
   0x36194d42L, 0x32d850f5L, 0x3f9b762cL, 0x3b5a6b9bL,
   0x0315d626L, 0x07d4cb91L, 0x0a97ed48L, 0x0e56f0ffL,
   0x1011a0faL, 0x14d0bd4dL, 0x19939b94L, 0x1d528623L,
   0xf12f560eL, 0xf5ee4bb9L, 0xf8ad6d60L, 0xfc6c70d7L,
   0xe22b20d2L, 0xe6ea3d65L, 0xeba91bbcL, 0xef68060bL,
   0xd727bbb6L, 0xd3e6a601L, 0xdea580d8L, 0xda649d6fL,
   0xc423cd6aL, 0xc0e2d0ddL, 0xcda1f604L, 0xc960ebb3L,
   0xbd3e8d7eL, 0xb9ff90c9L, 0xb4bcb610L, 0xb07daba7L,
   0xae3afba2L, 0xaafbe615L, 0xa7b8c0ccL, 0xa379dd7bL,
   0x9b3660c6L, 0x9ff77d71L, 0x92b45ba8L, 0x9675461fL,
   0x8832161aL, 0x8cf30badL, 0x81b02d74L, 0x857130c3L,
   0x5d8a9099L, 0x594b8d2eL, 0x5408abf7L, 0x50c9b640L,
   0x4e8ee645L, 0x4a4ffbf2L, 0x470cdd2bL, 0x43cdc09cL,
   0x7b827d21L, 0x7f436096L, 0x7200464fL, 0x76c15bf8L,
   0x68860bfdL, 0x6c47164aL, 0x61043093L, 0x65c52d24L,
   0x119b4be9L, 0x155a565eL, 0x18197087L, 0x1cd86d30L,
   0x029f3d35L, 0x065e2082L, 0x0b1d065bL, 0x0fdc1becL,
   0x3793a651L, 0x3352bbe6L, 0x3e119d3fL, 0x3ad08088L,
   0x2497d08dL, 0x2056cd3aL, 0x2d15ebe3L, 0x29d4f654L,
   0xc5a92679L, 0xc1683bceL, 0xcc2b1d17L, 0xc8ea00a0L,
   0xd6ad50a5L, 0xd26c4d12L, 0xdf2f6bcbL, 0xdbee767cL,
   0xe3a1cbc1L, 0xe760d676L, 0xea23f0afL, 0xeee2ed18L,
   0xf0a5bd1dL, 0xf464a0aaL, 0xf9278673L, 0xfde69bc4L,
   0x89b8fd09L, 0x8d79e0beL, 0x803ac667L, 0x84fbdbd0L,
   0x9abc8bd5L, 0x9e7d9662L, 0x933eb0bbL, 0x97ffad0cL,
   0xafb010b1L, 0xab710d06L, 0xa6322bdfL, 0xa2f33668L,
   0xbcb4666dL, 0xb8757bdaL, 0xb5365d03L, 0xb1f740b4L
};

void BZ2_bz__AssertH__fail ( int errcode )
{
   vex_printf("BZ2_bz__AssertH__fail(%d) called, exiting\n", errcode);
   (*serviceFn)(0,0);
}

void bz_internal_error ( int errcode )
{
   vex_printf("bz_internal_error called, exiting\n", errcode);
   (*serviceFn)(0,0);
}


static
int bz_config_ok ( void )
{
   if (sizeof(int) != 4) return 0;
   if (sizeof(short) != 2) return 0;
   if (sizeof(char) != 1) return 0;
   return 1;
}



static
void* default_bzalloc ( void* opaque, Int32 items, Int32 size )
{
   void* v = (void*) (*serviceFn)(2, items * size );
   return v;
}

static
void default_bzfree ( void* opaque, void* addr )
{
   if (addr != ((void *)0)) (*serviceFn)( 3, (HWord)addr );
}



static
void prepare_new_block ( EState* s )
{
   Int32 i;
   s->nblock = 0;
   s->numZ = 0;
   s->state_out_pos = 0;
   { s->blockCRC = 0xffffffffL; };
   for (i = 0; i < 256; i++) s->inUse[i] = ((Bool)0);
   s->blockNo++;
}



static
void init_RL ( EState* s )
{
   s->state_in_ch = 256;
   s->state_in_len = 0;
}


static
Bool isempty_RL ( EState* s )
{
   if (s->state_in_ch < 256 && s->state_in_len > 0)
      return ((Bool)0); else
      return ((Bool)1);
}



int BZ2_bzCompressInit
                    ( bz_stream* strm,
                     int blockSize100k,
                     int verbosity,
                     int workFactor )
{
   Int32 n;
   EState* s;

   if (!bz_config_ok()) return (-9);

   if (strm == ((void *)0) ||
       blockSize100k < 1 || blockSize100k > 9 ||
       workFactor < 0 || workFactor > 250)
     return (-2);

   if (workFactor == 0) workFactor = 30;
   if (strm->bzalloc == ((void *)0)) strm->bzalloc = default_bzalloc;
   if (strm->bzfree == ((void *)0)) strm->bzfree = default_bzfree;

   s = (strm->bzalloc)(strm->opaque,(sizeof(EState)),1);
   if (s == ((void *)0)) return (-3);
   s->strm = strm;

   s->arr1 = ((void *)0);
   s->arr2 = ((void *)0);
   s->ftab = ((void *)0);

   n = 100000 * blockSize100k;
   s->arr1 = (strm->bzalloc)(strm->opaque,(n * sizeof(UInt32)),1);
   s->arr2 = (strm->bzalloc)(strm->opaque,((n+(2 + 12 + 18 + 2)) * sizeof(UInt32)),1);
   s->ftab = (strm->bzalloc)(strm->opaque,(65537 * sizeof(UInt32)),1);

   if (s->arr1 == ((void *)0) || s->arr2 == ((void *)0) || s->ftab == ((void *)0)) {
      if (s->arr1 != ((void *)0)) (strm->bzfree)(strm->opaque,(s->arr1));
      if (s->arr2 != ((void *)0)) (strm->bzfree)(strm->opaque,(s->arr2));
      if (s->ftab != ((void *)0)) (strm->bzfree)(strm->opaque,(s->ftab));
      if (s != ((void *)0)) (strm->bzfree)(strm->opaque,(s));
      return (-3);
   }

   s->blockNo = 0;
   s->state = 2;
   s->mode = 2;
   s->combinedCRC = 0;
   s->blockSize100k = blockSize100k;
   s->nblockMAX = 100000 * blockSize100k - 19;
   s->verbosity = verbosity;
   s->workFactor = workFactor;

   s->block = (UChar*)s->arr2;
   s->mtfv = (UInt16*)s->arr1;
   s->zbits = ((void *)0);
   s->ptr = (UInt32*)s->arr1;

   strm->state = s;
   strm->total_in_lo32 = 0;
   strm->total_in_hi32 = 0;
   strm->total_out_lo32 = 0;
   strm->total_out_hi32 = 0;
   init_RL ( s );
   prepare_new_block ( s );
   return 0;
}



static
void add_pair_to_block ( EState* s )
{
   Int32 i;
   UChar ch = (UChar)(s->state_in_ch);
   for (i = 0; i < s->state_in_len; i++) {
      { s->blockCRC = (s->blockCRC << 8) ^ BZ2_crc32Table[(s->blockCRC >> 24) ^ ((UChar)ch)]; };
   }
   s->inUse[s->state_in_ch] = ((Bool)1);
   switch (s->state_in_len) {
      case 1:
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         break;
      case 2:
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         break;
      case 3:
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         break;
      default:
         s->inUse[s->state_in_len-4] = ((Bool)1);
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = ((UChar)(s->state_in_len-4));
         s->nblock++;
         break;
   }
}



static
void flush_RL ( EState* s )
{
   if (s->state_in_ch < 256) add_pair_to_block ( s );
   init_RL ( s );
}

static
Bool copy_input_until_stop ( EState* s )
{
   Bool progress_in = ((Bool)0);

   if (s->mode == 2) {


      while (((Bool)1)) {

         if (s->nblock >= s->nblockMAX) break;

         if (s->strm->avail_in == 0) break;
         progress_in = ((Bool)1);
         { UInt32 zchh = (UInt32)((UInt32)(*((UChar*)(s->strm->next_in)))); if (zchh != s->state_in_ch && s->state_in_len == 1) { UChar ch = (UChar)(s->state_in_ch); { s->blockCRC = (s->blockCRC << 8) ^ BZ2_crc32Table[(s->blockCRC >> 24) ^ ((UChar)ch)]; }; s->inUse[s->state_in_ch] = ((Bool)1); s->block[s->nblock] = (UChar)ch; s->nblock++; s->state_in_ch = zchh; } else if (zchh != s->state_in_ch || s->state_in_len == 255) { if (s->state_in_ch < 256) add_pair_to_block ( s ); s->state_in_ch = zchh; s->state_in_len = 1; } else { s->state_in_len++; } };
         s->strm->next_in++;
         s->strm->avail_in--;
         s->strm->total_in_lo32++;
         if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++;
      }

   } else {


      while (((Bool)1)) {

         if (s->nblock >= s->nblockMAX) break;

         if (s->strm->avail_in == 0) break;

         if (s->avail_in_expect == 0) break;
         progress_in = ((Bool)1);
         { UInt32 zchh = (UInt32)((UInt32)(*((UChar*)(s->strm->next_in)))); if (zchh != s->state_in_ch && s->state_in_len == 1) { UChar ch = (UChar)(s->state_in_ch); { s->blockCRC = (s->blockCRC << 8) ^ BZ2_crc32Table[(s->blockCRC >> 24) ^ ((UChar)ch)]; }; s->inUse[s->state_in_ch] = ((Bool)1); s->block[s->nblock] = (UChar)ch; s->nblock++; s->state_in_ch = zchh; } else if (zchh != s->state_in_ch || s->state_in_len == 255) { if (s->state_in_ch < 256) add_pair_to_block ( s ); s->state_in_ch = zchh; s->state_in_len = 1; } else { s->state_in_len++; } };
         s->strm->next_in++;
         s->strm->avail_in--;
         s->strm->total_in_lo32++;
         if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++;
         s->avail_in_expect--;
      }
   }
   return progress_in;
}



static
Bool copy_output_until_stop ( EState* s )
{
   Bool progress_out = ((Bool)0);

   while (((Bool)1)) {


      if (s->strm->avail_out == 0) break;


      if (s->state_out_pos >= s->numZ) break;

      progress_out = ((Bool)1);
      *(s->strm->next_out) = s->zbits[s->state_out_pos];
      s->state_out_pos++;
      s->strm->avail_out--;
      s->strm->next_out++;
      s->strm->total_out_lo32++;
      if (s->strm->total_out_lo32 == 0) s->strm->total_out_hi32++;
   }

   return progress_out;
}



static
Bool handle_compress ( bz_stream* strm )
{
   Bool progress_in = ((Bool)0);
   Bool progress_out = ((Bool)0);
   EState* s = strm->state;

   while (((Bool)1)) {

      if (s->state == 1) {
         progress_out |= copy_output_until_stop ( s );
         if (s->state_out_pos < s->numZ) break;
         if (s->mode == 4 &&
             s->avail_in_expect == 0 &&
             isempty_RL(s)) break;
         prepare_new_block ( s );
         s->state = 2;
         if (s->mode == 3 &&
             s->avail_in_expect == 0 &&
             isempty_RL(s)) break;
      }

      if (s->state == 2) {
         progress_in |= copy_input_until_stop ( s );
         if (s->mode != 2 && s->avail_in_expect == 0) {
            flush_RL ( s );
            BZ2_compressBlock ( s, (Bool)(s->mode == 4) );
            s->state = 1;
         }
         else
         if (s->nblock >= s->nblockMAX) {
            BZ2_compressBlock ( s, ((Bool)0) );
            s->state = 1;
         }
         else
         if (s->strm->avail_in == 0) {
            break;
         }
      }

   }

   return progress_in || progress_out;
}



int BZ2_bzCompress ( bz_stream *strm, int action )
{
   Bool progress;
   EState* s;
   if (strm == ((void *)0)) return (-2);
   s = strm->state;
   if (s == ((void *)0)) return (-2);
   if (s->strm != strm) return (-2);

   preswitch:
   switch (s->mode) {

      case 1:
         return (-1);

      case 2:
         if (action == 0) {
            progress = handle_compress ( strm );
            return progress ? 1 : (-2);
         }
         else
  if (action == 1) {
            s->avail_in_expect = strm->avail_in;
            s->mode = 3;
            goto preswitch;
         }
         else
         if (action == 2) {
            s->avail_in_expect = strm->avail_in;
            s->mode = 4;
            goto preswitch;
         }
         else
            return (-2);

      case 3:
         if (action != 1) return (-1);
         if (s->avail_in_expect != s->strm->avail_in)
            return (-1);
         progress = handle_compress ( strm );
         if (s->avail_in_expect > 0 || !isempty_RL(s) ||
             s->state_out_pos < s->numZ) return 2;
         s->mode = 2;
         return 1;

      case 4:
         if (action != 2) return (-1);
         if (s->avail_in_expect != s->strm->avail_in)
            return (-1);
         progress = handle_compress ( strm );
         if (!progress) return (-1);
         if (s->avail_in_expect > 0 || !isempty_RL(s) ||
             s->state_out_pos < s->numZ) return 3;
         s->mode = 1;
         return 4;
   }
   return 0;
}



int BZ2_bzCompressEnd ( bz_stream *strm )
{
   EState* s;
   if (strm == ((void *)0)) return (-2);
   s = strm->state;
   if (s == ((void *)0)) return (-2);
   if (s->strm != strm) return (-2);

   if (s->arr1 != ((void *)0)) (strm->bzfree)(strm->opaque,(s->arr1));
   if (s->arr2 != ((void *)0)) (strm->bzfree)(strm->opaque,(s->arr2));
   if (s->ftab != ((void *)0)) (strm->bzfree)(strm->opaque,(s->ftab));
   (strm->bzfree)(strm->opaque,(strm->state));

   strm->state = ((void *)0);

   return 0;
}







int BZ2_bzDecompressInit
                     ( bz_stream* strm,
                       int verbosity,
                       int small )
{
   DState* s;

   if (!bz_config_ok()) return (-9);

   if (strm == ((void *)0)) return (-2);
   if (small != 0 && small != 1) return (-2);
   if (verbosity < 0 || verbosity > 4) return (-2);

   if (strm->bzalloc == ((void *)0)) strm->bzalloc = default_bzalloc;
   if (strm->bzfree == ((void *)0)) strm->bzfree = default_bzfree;

   s = (strm->bzalloc)(strm->opaque,(sizeof(DState)),1);
   if (s == ((void *)0)) return (-3);
   s->strm = strm;
   strm->state = s;
   s->state = 10;
   s->bsLive = 0;
   s->bsBuff = 0;
   s->calculatedCombinedCRC = 0;
   strm->total_in_lo32 = 0;
   strm->total_in_hi32 = 0;
   strm->total_out_lo32 = 0;
   strm->total_out_hi32 = 0;
   s->smallDecompress = (Bool)small;
   s->ll4 = ((void *)0);
   s->ll16 = ((void *)0);
   s->tt = ((void *)0);
   s->currBlockNo = 0;
   s->verbosity = verbosity;

   return 0;
}






static
Bool unRLE_obuf_to_output_FAST ( DState* s )
{
   UChar k1;

   if (s->blockRandomised) {

      while (((Bool)1)) {

         while (((Bool)1)) {
            if (s->strm->avail_out == 0) return ((Bool)0);
            if (s->state_out_len == 0) break;
            *( (UChar*)(s->strm->next_out) ) = s->state_out_ch;
            { s->calculatedBlockCRC = (s->calculatedBlockCRC << 8) ^ BZ2_crc32Table[(s->calculatedBlockCRC >> 24) ^ ((UChar)s->state_out_ch)]; };
            s->state_out_len--;
            s->strm->next_out++;
            s->strm->avail_out--;
            s->strm->total_out_lo32++;
            if (s->strm->total_out_lo32 == 0) s->strm->total_out_hi32++;
         }


         if (s->nblock_used == s->save_nblock+1) return ((Bool)0);


         if (s->nblock_used > s->save_nblock+1)
            return ((Bool)1);

         s->state_out_len = 1;
         s->state_out_ch = s->k0;
         s->tPos = s->tt[s->tPos]; k1 = (UChar)(s->tPos & 0xff); s->tPos >>= 8;; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         s->state_out_len = 2;
         s->tPos = s->tt[s->tPos]; k1 = (UChar)(s->tPos & 0xff); s->tPos >>= 8;; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         s->state_out_len = 3;
         s->tPos = s->tt[s->tPos]; k1 = (UChar)(s->tPos & 0xff); s->tPos >>= 8;; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         s->tPos = s->tt[s->tPos]; k1 = (UChar)(s->tPos & 0xff); s->tPos >>= 8;; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         s->state_out_len = ((Int32)k1) + 4;
         s->tPos = s->tt[s->tPos]; s->k0 = (UChar)(s->tPos & 0xff); s->tPos >>= 8;; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         s->k0 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
      }

   } else {


      UInt32 c_calculatedBlockCRC = s->calculatedBlockCRC;
      UChar c_state_out_ch = s->state_out_ch;
      Int32 c_state_out_len = s->state_out_len;
      Int32 c_nblock_used = s->nblock_used;
      Int32 c_k0 = s->k0;
      UInt32* c_tt = s->tt;
      UInt32 c_tPos = s->tPos;
      char* cs_next_out = s->strm->next_out;
      unsigned int cs_avail_out = s->strm->avail_out;


      UInt32 avail_out_INIT = cs_avail_out;
      Int32 s_save_nblockPP = s->save_nblock+1;
      unsigned int total_out_lo32_old;

      while (((Bool)1)) {


         if (c_state_out_len > 0) {
            while (((Bool)1)) {
               if (cs_avail_out == 0) goto return_notr;
               if (c_state_out_len == 1) break;
               *( (UChar*)(cs_next_out) ) = c_state_out_ch;
               { c_calculatedBlockCRC = (c_calculatedBlockCRC << 8) ^ BZ2_crc32Table[(c_calculatedBlockCRC >> 24) ^ ((UChar)c_state_out_ch)]; };
               c_state_out_len--;
               cs_next_out++;
               cs_avail_out--;
            }
            s_state_out_len_eq_one:
            {
               if (cs_avail_out == 0) {
                  c_state_out_len = 1; goto return_notr;
               };
               *( (UChar*)(cs_next_out) ) = c_state_out_ch;
               { c_calculatedBlockCRC = (c_calculatedBlockCRC << 8) ^ BZ2_crc32Table[(c_calculatedBlockCRC >> 24) ^ ((UChar)c_state_out_ch)]; };
               cs_next_out++;
               cs_avail_out--;
            }
         }

         if (c_nblock_used > s_save_nblockPP)
            return ((Bool)1);


         if (c_nblock_used == s_save_nblockPP) {
            c_state_out_len = 0; goto return_notr;
         };
         c_state_out_ch = c_k0;
         c_tPos = c_tt[c_tPos]; k1 = (UChar)(c_tPos & 0xff); c_tPos >>= 8;; c_nblock_used++;
         if (k1 != c_k0) {
            c_k0 = k1; goto s_state_out_len_eq_one;
         };
         if (c_nblock_used == s_save_nblockPP)
            goto s_state_out_len_eq_one;

         c_state_out_len = 2;
         c_tPos = c_tt[c_tPos]; k1 = (UChar)(c_tPos & 0xff); c_tPos >>= 8;; c_nblock_used++;
         if (c_nblock_used == s_save_nblockPP) continue;
         if (k1 != c_k0) { c_k0 = k1; continue; };

         c_state_out_len = 3;
         c_tPos = c_tt[c_tPos]; k1 = (UChar)(c_tPos & 0xff); c_tPos >>= 8;; c_nblock_used++;
         if (c_nblock_used == s_save_nblockPP) continue;
         if (k1 != c_k0) { c_k0 = k1; continue; };

         c_tPos = c_tt[c_tPos]; k1 = (UChar)(c_tPos & 0xff); c_tPos >>= 8;; c_nblock_used++;
         c_state_out_len = ((Int32)k1) + 4;
         c_tPos = c_tt[c_tPos]; c_k0 = (UChar)(c_tPos & 0xff); c_tPos >>= 8;; c_nblock_used++;
      }

      return_notr:
      total_out_lo32_old = s->strm->total_out_lo32;
      s->strm->total_out_lo32 += (avail_out_INIT - cs_avail_out);
      if (s->strm->total_out_lo32 < total_out_lo32_old)
         s->strm->total_out_hi32++;


      s->calculatedBlockCRC = c_calculatedBlockCRC;
      s->state_out_ch = c_state_out_ch;
      s->state_out_len = c_state_out_len;
      s->nblock_used = c_nblock_used;
      s->k0 = c_k0;
      s->tt = c_tt;
      s->tPos = c_tPos;
      s->strm->next_out = cs_next_out;
      s->strm->avail_out = cs_avail_out;

   }
   return ((Bool)0);
}







static
Bool unRLE_obuf_to_output_SMALL ( DState* s )
{
   UChar k1;

   if (s->blockRandomised) {

      while (((Bool)1)) {

         while (((Bool)1)) {
            if (s->strm->avail_out == 0) return ((Bool)0);
            if (s->state_out_len == 0) break;
            *( (UChar*)(s->strm->next_out) ) = s->state_out_ch;
            { s->calculatedBlockCRC = (s->calculatedBlockCRC << 8) ^ BZ2_crc32Table[(s->calculatedBlockCRC >> 24) ^ ((UChar)s->state_out_ch)]; };
            s->state_out_len--;
            s->strm->next_out++;
            s->strm->avail_out--;
            s->strm->total_out_lo32++;
            if (s->strm->total_out_lo32 == 0) s->strm->total_out_hi32++;
         }


         if (s->nblock_used == s->save_nblock+1) return ((Bool)0);


         if (s->nblock_used > s->save_nblock+1)
            return ((Bool)1);

         s->state_out_len = 1;
         s->state_out_ch = s->k0;
         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         s->state_out_len = 2;
         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         s->state_out_len = 3;
         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         k1 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
         s->state_out_len = ((Int32)k1) + 4;
         s->k0 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; if (s->rNToGo == 0) { s->rNToGo = BZ2_rNums[s->rTPos]; s->rTPos++; if (s->rTPos == 512) s->rTPos = 0; } s->rNToGo--;;
         s->k0 ^= ((s->rNToGo == 1) ? 1 : 0); s->nblock_used++;
      }

   } else {

      while (((Bool)1)) {

         while (((Bool)1)) {
            if (s->strm->avail_out == 0) return ((Bool)0);
            if (s->state_out_len == 0) break;
            *( (UChar*)(s->strm->next_out) ) = s->state_out_ch;
            { s->calculatedBlockCRC = (s->calculatedBlockCRC << 8) ^ BZ2_crc32Table[(s->calculatedBlockCRC >> 24) ^ ((UChar)s->state_out_ch)]; };
            s->state_out_len--;
            s->strm->next_out++;
            s->strm->avail_out--;
            s->strm->total_out_lo32++;
            if (s->strm->total_out_lo32 == 0) s->strm->total_out_hi32++;
         }


         if (s->nblock_used == s->save_nblock+1) return ((Bool)0);


         if (s->nblock_used > s->save_nblock+1)
            return ((Bool)1);

         s->state_out_len = 1;
         s->state_out_ch = s->k0;
         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         s->state_out_len = 2;
         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         s->state_out_len = 3;
         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };

         k1 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; s->nblock_used++;
         s->state_out_len = ((Int32)k1) + 4;
         s->k0 = BZ2_indexIntoF ( s->tPos, s->cftab ); s->tPos = (((UInt32)s->ll16[s->tPos]) | (((((UInt32)(s->ll4[(s->tPos) >> 1])) >> (((s->tPos) << 2) & 0x4)) & 0xF) << 16));; s->nblock_used++;
      }

   }
}



int BZ2_bzDecompress ( bz_stream *strm )
{
   Bool corrupt;
   DState* s;
   if (strm == ((void *)0)) return (-2);
   s = strm->state;
   if (s == ((void *)0)) return (-2);
   if (s->strm != strm) return (-2);

   while (((Bool)1)) {
      if (s->state == 1) return (-1);
      if (s->state == 2) {
         if (s->smallDecompress)
            corrupt = unRLE_obuf_to_output_SMALL ( s ); else
            corrupt = unRLE_obuf_to_output_FAST ( s );
         if (corrupt) return (-4);
         if (s->nblock_used == s->save_nblock+1 && s->state_out_len == 0) {
            { s->calculatedBlockCRC = ~(s->calculatedBlockCRC); };
            if (s->verbosity >= 3)
               vex_printf(" {0x%08x, 0x%08x}",s->storedBlockCRC,s->calculatedBlockCRC);

            if (s->verbosity >= 2) vex_printf("]");
            if (s->calculatedBlockCRC != s->storedBlockCRC)
               return (-4);
            s->calculatedCombinedCRC
               = (s->calculatedCombinedCRC << 1) |
                    (s->calculatedCombinedCRC >> 31);
            s->calculatedCombinedCRC ^= s->calculatedBlockCRC;
            s->state = 14;
         } else {
            return 0;
         }
      }
      if (s->state >= 10) {
         Int32 r = BZ2_decompress ( s );
         if (r == 4) {
            if (s->verbosity >= 3)
               vex_printf("\n    combined CRCs: stored = 0x%08x, computed = 0x%08x",s->storedCombinedCRC,s->calculatedCombinedCRC);

            if (s->calculatedCombinedCRC != s->storedCombinedCRC)
               return (-4);
            return r;
         }
         if (s->state != 2) return r;
      }
   }

   { if (!(0)) bz_internal_error ( 6001 ); };

   return 0;
}



int BZ2_bzDecompressEnd ( bz_stream *strm )
{
   DState* s;
   if (strm == ((void *)0)) return (-2);
   s = strm->state;
   if (s == ((void *)0)) return (-2);
   if (s->strm != strm) return (-2);

   if (s->tt != ((void *)0)) (strm->bzfree)(strm->opaque,(s->tt));
   if (s->ll16 != ((void *)0)) (strm->bzfree)(strm->opaque,(s->ll16));
   if (s->ll4 != ((void *)0)) (strm->bzfree)(strm->opaque,(s->ll4));

   (strm->bzfree)(strm->opaque,(strm->state));
   strm->state = ((void *)0);

   return 0;
}

int BZ2_bzBuffToBuffCompress
                         ( char* dest,
                           unsigned int* destLen,
                           char* source,
                           unsigned int sourceLen,
                           int blockSize100k,
                           int verbosity,
                           int workFactor )
{
   bz_stream strm;
   int ret;

   if (dest == ((void *)0) || destLen == ((void *)0) ||
       source == ((void *)0) ||
       blockSize100k < 1 || blockSize100k > 9 ||
       verbosity < 0 || verbosity > 4 ||
       workFactor < 0 || workFactor > 250)
      return (-2);

   if (workFactor == 0) workFactor = 30;
   strm.bzalloc = ((void *)0);
   strm.bzfree = ((void *)0);
   strm.opaque = ((void *)0);
   ret = BZ2_bzCompressInit ( &strm, blockSize100k,
                              verbosity, workFactor );
   if (ret != 0) return ret;

   strm.next_in = source;
   strm.next_out = dest;
   strm.avail_in = sourceLen;
   strm.avail_out = *destLen;

   ret = BZ2_bzCompress ( &strm, 2 );
   if (ret == 3) goto output_overflow;
   if (ret != 4) goto errhandler;


   *destLen -= strm.avail_out;
   BZ2_bzCompressEnd ( &strm );
   return 0;

   output_overflow:
   BZ2_bzCompressEnd ( &strm );
   return (-8);

   errhandler:
   BZ2_bzCompressEnd ( &strm );
   return ret;
}



int BZ2_bzBuffToBuffDecompress
                           ( char* dest,
                             unsigned int* destLen,
                             char* source,
                             unsigned int sourceLen,
                             int small,
                             int verbosity )
{
   bz_stream strm;
   int ret;

   if (dest == ((void *)0) || destLen == ((void *)0) ||
       source == ((void *)0) ||
       (small != 0 && small != 1) ||
       verbosity < 0 || verbosity > 4)
          return (-2);

   strm.bzalloc = ((void *)0);
   strm.bzfree = ((void *)0);
   strm.opaque = ((void *)0);
   ret = BZ2_bzDecompressInit ( &strm, verbosity, small );
   if (ret != 0) return ret;

   strm.next_in = source;
   strm.next_out = dest;
   strm.avail_in = sourceLen;
   strm.avail_out = *destLen;

   ret = BZ2_bzDecompress ( &strm );
   if (ret == 0) goto output_overflow_or_eof;
   if (ret != 4) goto errhandler;


   *destLen -= strm.avail_out;
   BZ2_bzDecompressEnd ( &strm );
   return 0;

   output_overflow_or_eof:
   if (strm.avail_out > 0) {
      BZ2_bzDecompressEnd ( &strm );
      return (-7);
   } else {
      BZ2_bzDecompressEnd ( &strm );
      return (-8);
   };

   errhandler:
   BZ2_bzDecompressEnd ( &strm );
   return ret;
}

const char * BZ2_bzlibVersion(void)
{
   return "1.0.3, 17-Oct-2004";
}

typedef unsigned char uchar;


 char inbuf[1000000];
 char outbuf[(1000000 + 1000000)];
 char zbuf[1000000 + 600 + (1000000 / 100)];

int nIn;
unsigned int nOut;
unsigned int nZ;

void flip_bit ( int bit )
{
   int byteno = bit / 8;
   int bitno = bit % 8;
   uchar mask = 1 << bitno;


   zbuf[byteno] ^= mask;
}

void set_inbuf ( void )
{
  inbuf[0] = 0;
  my_strcat(inbuf, "At her sixtieth birthday party, Margaret Thatcher ");
  my_strcat(inbuf, "blew on the cake to light the candles.\n");
  my_strcat(inbuf, "This program, bzip2, the associated library libbzip2, and all\n");
  my_strcat(inbuf, "documentation, are copyright (C) 1996-2004 Julian R Seward.  All\n");
  my_strcat(inbuf, "rights reserved.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "Redistribution and use in source and binary forms, with or without\n");
  my_strcat(inbuf, "modification, are permitted provided that the following conditions\n");
  my_strcat(inbuf, "are met:\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "1. Redistributions of source code must retain the above copyright\n");
  my_strcat(inbuf, "   notice, this list of conditions and the following disclaimer.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "2. The origin of this software must not be misrepresented; you must\n");
  my_strcat(inbuf, "   not claim that you wrote the original software.  If you use this\n");
  my_strcat(inbuf, "   software in a product, an acknowledgment in the product\n");
  my_strcat(inbuf, "   documentation would be appreciated but is not required.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "3. Altered source versions must be plainly marked as such, and must\n");
  my_strcat(inbuf, "   not be misrepresented as being the original software.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "4. The name of the author may not be used to endorse or promote\n");
  my_strcat(inbuf, "   products derived from this software without specific prior written\n");
  my_strcat(inbuf, "   permission.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS\n");
  my_strcat(inbuf, "OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n");
  my_strcat(inbuf, "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n");
  my_strcat(inbuf, "ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY\n");
  my_strcat(inbuf, "DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n");
  my_strcat(inbuf, "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE\n");
  my_strcat(inbuf, "GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n");
  my_strcat(inbuf, "INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,\n");
  my_strcat(inbuf, "WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING\n");
  my_strcat(inbuf, "NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n");
  my_strcat(inbuf, "SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "ababababababababababababababababababababababababababababababab");
  my_strcat(inbuf, "		    GNU GENERAL PUBLIC LICENSE\n");
  my_strcat(inbuf, "		       Version 2, June 1991\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, " Copyright (C) 1989, 1991 Free Software Foundation, Inc.\n");
  my_strcat(inbuf, "     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA\n");
  my_strcat(inbuf, " Everyone is permitted to copy and distribute verbatim copies\n");
  my_strcat(inbuf, " of this license document, but changing it is not allowed.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "			    Preamble\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  The licenses for most software are designed to take away your\n");
  my_strcat(inbuf, "freedom to share and change it.  By contrast, the GNU General Public\n");
  my_strcat(inbuf, "License is intended to guarantee your freedom to share and change free\n");
  my_strcat(inbuf, "software--to make sure the software is free for all its users.  This\n");
  my_strcat(inbuf, "General Public License applies to most of the Free Software\n");
  my_strcat(inbuf, "Foundation's software and to any other program whose authors commit to\n");
  my_strcat(inbuf, "using it.  (Some other Free Software Foundation software is covered by\n");
  my_strcat(inbuf, "the GNU Library General Public License instead.)  You can apply it to\n");
  my_strcat(inbuf, "your programs, too.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  When we speak of free software, we are referring to freedom, not\n");
  my_strcat(inbuf, "price.  Our General Public Licenses are designed to make sure that you\n");
  my_strcat(inbuf, "have the freedom to distribute copies of free software (and charge for\n");
  my_strcat(inbuf, "this service if you wish), that you receive source code or can get it\n");
  my_strcat(inbuf, "if you want it, that you can change the software or use pieces of it\n");
  my_strcat(inbuf, "in new free programs; and that you know you can do these things.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  To protect your rights, we need to make restrictions that forbid\n");
  my_strcat(inbuf, "anyone to deny you these rights or to ask you to surrender the rights.\n");
  my_strcat(inbuf, "These restrictions translate to certain responsibilities for you if you\n");
  my_strcat(inbuf, "distribute copies of the software, or if you modify it.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  For example, if you distribute copies of such a program, whether\n");
  my_strcat(inbuf, "gratis or for a fee, you must give the recipients all the rights that\n");
  my_strcat(inbuf, "you have.  You must make sure that they, too, receive or can get the\n");
  my_strcat(inbuf, "source code.  And you must show them these terms so they know their\n");
  my_strcat(inbuf, "rights.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  We protect your rights with two steps: (1) copyright the software, and\n");
  my_strcat(inbuf, "(2) offer you this license which gives you legal permission to copy,\n");
  my_strcat(inbuf, "distribute and/or modify the software.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  Also, for each author's protection and ours, we want to make certain\n");
  my_strcat(inbuf, "that everyone understands that there is no warranty for this free\n");
  my_strcat(inbuf, "software.  If the software is modified by someone else and passed on, we\n");
  my_strcat(inbuf, "want its recipients to know that what they have is not the original, so\n");
  my_strcat(inbuf, "that any problems introduced by others will not reflect on the original\n");
  my_strcat(inbuf, "authors' reputations.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  Finally, any free program is threatened constantly by software\n");
  my_strcat(inbuf, "patents.  We wish to avoid the danger that redistributors of a free\n");
  my_strcat(inbuf, "program will individually obtain patent licenses, in effect making the\n");
  my_strcat(inbuf, "program proprietary.  To prevent this, we have made it clear that any\n");
  my_strcat(inbuf, "patent must be licensed for everyone's free use or not licensed at all.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  The precise terms and conditions for copying, distribution and\n");
  my_strcat(inbuf, "modification follow.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "		    GNU GENERAL PUBLIC LICENSE\n");
  my_strcat(inbuf, "   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  0. This License applies to any program or other work which contains\n");
  my_strcat(inbuf, "a notice placed by the copyright holder saying it may be distributed\n");
  my_strcat(inbuf, "under the terms of this General Public License.  The Program, below,\n");
  my_strcat(inbuf, "refers to any such program or work, and a work based on the Program\n");
  my_strcat(inbuf, "means either the Program or any derivative work under copyright law:\n");
  my_strcat(inbuf, "that is to say, a work containing the Program or a portion of it,\n");
  my_strcat(inbuf, "either verbatim or with modifications and/or translated into another\n");
  my_strcat(inbuf, "language.  (Hereinafter, translation is included without limitation in\n");
  my_strcat(inbuf, "the term modification.)  Each licensee is addressed as you.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "Activities other than copying, distribution and modification are not\n");
  my_strcat(inbuf, "covered by this License; they are outside its scope.  The act of\n");
  my_strcat(inbuf, "running the Program is not restricted, and the output from the Program\n");
  my_strcat(inbuf, "is covered only if its contents constitute a work based on the\n");
  my_strcat(inbuf, "Program (independent of having been made by running the Program).\n");
  my_strcat(inbuf, "Whether that is true depends on what the Program does.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  1. You may copy and distribute verbatim copies of the Program's\n");
  my_strcat(inbuf, "source code as you receive it, in any medium, provided that you\n");
  my_strcat(inbuf, "conspicuously and appropriately publish on each copy an appropriate\n");
  my_strcat(inbuf, "copyright notice and disclaimer of warranty; keep intact all the\n");
  my_strcat(inbuf, "notices that refer to this License and to the absence of any warranty;\n");
  my_strcat(inbuf, "and give any other recipients of the Program a copy of this License\n");
  my_strcat(inbuf, "along with the Program.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "You may charge a fee for the physical act of transferring a copy, and\n");
  my_strcat(inbuf, "you may at your option offer warranty protection in exchange for a fee.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  2. You may modify your copy or copies of the Program or any portion\n");
  my_strcat(inbuf, "of it, thus forming a work based on the Program, and copy and\n");
  my_strcat(inbuf, "distribute such modifications or work under the terms of Section 1\n");
  my_strcat(inbuf, "above, provided that you also meet all of these conditions:\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    a) You must cause the modified files to carry prominent notices\n");
  my_strcat(inbuf, "    stating that you changed the files and the date of any change.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    b) You must cause any work that you distribute or publish, that in\n");
  my_strcat(inbuf, "    whole or in part contains or is derived from the Program or any\n");
  my_strcat(inbuf, "    part thereof, to be licensed as a whole at no charge to all third\n");
  my_strcat(inbuf, "    parties under the terms of this License.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    c) If the modified program normally reads commands interactively\n");
  my_strcat(inbuf, "    when run, you must cause it, when started running for such\n");
  my_strcat(inbuf, "    interactive use in the most ordinary way, to print or display an\n");
  my_strcat(inbuf, "    announcement including an appropriate copyright notice and a\n");
  my_strcat(inbuf, "    notice that there is no warranty (or else, saying that you provide\n");
  my_strcat(inbuf, "    a warranty) and that users may redistribute the program under\n");
  my_strcat(inbuf, "    these conditions, and telling the user how to view a copy of this\n");
  my_strcat(inbuf, "    License.  (Exception: if the Program itself is interactive but\n");
  my_strcat(inbuf, "    does not normally print such an announcement, your work based on\n");
  my_strcat(inbuf, "    the Program is not required to print an announcement.)\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "These requirements apply to the modified work as a whole.  If\n");
  my_strcat(inbuf, "identifiable sections of that work are not derived from the Program,\n");
  my_strcat(inbuf, "and can be reasonably considered independent and separate works in\n");
  my_strcat(inbuf, "themselves, then this License, and its terms, do not apply to those\n");
  my_strcat(inbuf, "sections when you distribute them as separate works.  But when you\n");
  my_strcat(inbuf, "distribute the same sections as part of a whole which is a work based\n");
  my_strcat(inbuf, "on the Program, the distribution of the whole must be on the terms of\n");
  my_strcat(inbuf, "this License, whose permissions for other licensees extend to the\n");
  my_strcat(inbuf, "entire whole, and thus to each and every part regardless of who wrote it.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "Thus, it is not the intent of this section to claim rights or contest\n");
  my_strcat(inbuf, "your rights to work written entirely by you; rather, the intent is to\n");
  my_strcat(inbuf, "exercise the right to control the distribution of derivative or\n");
  my_strcat(inbuf, "collective works based on the Program.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "In addition, mere aggregation of another work not based on the Program\n");
  my_strcat(inbuf, "with the Program (or with a work based on the Program) on a volume of\n");
  my_strcat(inbuf, "a storage or distribution medium does not bring the other work under\n");
  my_strcat(inbuf, "the scope of this License.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  3. You may copy and distribute the Program (or a work based on it,\n");
  my_strcat(inbuf, "under Section 2) in object code or executable form under the terms of\n");
  my_strcat(inbuf, "Sections 1 and 2 above provided that you also do one of the following:\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    a) Accompany it with the complete corresponding machine-readable\n");
  my_strcat(inbuf, "    source code, which must be distributed under the terms of Sections\n");
  my_strcat(inbuf, "    1 and 2 above on a medium customarily used for software interchange; or,\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    b) Accompany it with a written offer, valid for at least three\n");
  my_strcat(inbuf, "    years, to give any third party, for a charge no more than your\n");
  my_strcat(inbuf, "    cost of physically performing source distribution, a complete\n");
  my_strcat(inbuf, "    machine-readable copy of the corresponding source code, to be\n");
  my_strcat(inbuf, "    distributed under the terms of Sections 1 and 2 above on a medium\n");
  my_strcat(inbuf, "    customarily used for software interchange; or,\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    c) Accompany it with the information you received as to the offer\n");
  my_strcat(inbuf, "    to distribute corresponding source code.  (This alternative is\n");
  my_strcat(inbuf, "    allowed only for noncommercial distribution and only if you\n");
  my_strcat(inbuf, "    received the program in object code or executable form with such\n");
  my_strcat(inbuf, "    an offer, in accord with Subsection b above.)\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "The source code for a work means the preferred form of the work for\n");
  my_strcat(inbuf, "making modifications to it.  For an executable work, complete source\n");
  my_strcat(inbuf, "code means all the source code for all modules it contains, plus any\n");
  my_strcat(inbuf, "associated interface definition files, plus the scripts used to\n");
  my_strcat(inbuf, "control compilation and installation of the executable.  However, as a\n");
  my_strcat(inbuf, "special exception, the source code distributed need not include\n");
  my_strcat(inbuf, "anything that is normally distributed (in either source or binary\n");
  my_strcat(inbuf, "form) with the major components (compiler, kernel, and so on) of the\n");
  my_strcat(inbuf, "operating system on which the executable runs, unless that component\n");
  my_strcat(inbuf, "itself accompanies the executable.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "If distribution of executable or object code is made by offering\n");
  my_strcat(inbuf, "access to copy from a designated place, then offering equivalent\n");
  my_strcat(inbuf, "access to copy the source code from the same place counts as\n");
  my_strcat(inbuf, "distribution of the source code, even though third parties are not\n");
  my_strcat(inbuf, "compelled to copy the source along with the object code.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  4. You may not copy, modify, sublicense, or distribute the Program\n");
  my_strcat(inbuf, "except as expressly provided under this License.  Any attempt\n");
  my_strcat(inbuf, "otherwise to copy, modify, sublicense or distribute the Program is\n");
  my_strcat(inbuf, "void, and will automatically terminate your rights under this License.\n");
  my_strcat(inbuf, "However, parties who have received copies, or rights, from you under\n");
  my_strcat(inbuf, "this License will not have their licenses terminated so long as such\n");
  my_strcat(inbuf, "parties remain in full compliance.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  5. You are not required to accept this License, since you have not\n");
  my_strcat(inbuf, "signed it.  However, nothing else grants you permission to modify or\n");
  my_strcat(inbuf, "distribute the Program or its derivative works.  These actions are\n");
  my_strcat(inbuf, "prohibited by law if you do not accept this License.  Therefore, by\n");
  my_strcat(inbuf, "modifying or distributing the Program (or any work based on the\n");
  my_strcat(inbuf, "Program), you indicate your acceptance of this License to do so, and\n");
  my_strcat(inbuf, "all its terms and conditions for copying, distributing or modifying\n");
  my_strcat(inbuf, "the Program or works based on it.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  6. Each time you redistribute the Program (or any work based on the\n");
  my_strcat(inbuf, "Program), the recipient automatically receives a license from the\n");
  my_strcat(inbuf, "original licensor to copy, distribute or modify the Program subject to\n");
  my_strcat(inbuf, "these terms and conditions.  You may not impose any further\n");
  my_strcat(inbuf, "restrictions on the recipients' exercise of the rights granted herein.\n");
  my_strcat(inbuf, "You are not responsible for enforcing compliance by third parties to\n");
  my_strcat(inbuf, "this License.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  7. If, as a consequence of a court judgment or allegation of patent\n");
  my_strcat(inbuf, "infringement or for any other reason (not limited to patent issues),\n");
  my_strcat(inbuf, "conditions are imposed on you (whether by court order, agreement or\n");
  my_strcat(inbuf, "otherwise) that contradict the conditions of this License, they do not\n");
  my_strcat(inbuf, "excuse you from the conditions of this License.  If you cannot\n");
  my_strcat(inbuf, "distribute so as to satisfy simultaneously your obligations under this\n");
  my_strcat(inbuf, "License and any other pertinent obligations, then as a consequence you\n");
  my_strcat(inbuf, "may not distribute the Program at all.  For example, if a patent\n");
  my_strcat(inbuf, "license would not permit royalty-free redistribution of the Program by\n");
  my_strcat(inbuf, "all those who receive copies directly or indirectly through you, then\n");
  my_strcat(inbuf, "the only way you could satisfy both it and this License would be to\n");
  my_strcat(inbuf, "refrain entirely from distribution of the Program.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "If any portion of this section is held invalid or unenforceable under\n");
  my_strcat(inbuf, "any particular circumstance, the balance of the section is intended to\n");
  my_strcat(inbuf, "apply and the section as a whole is intended to apply in other\n");
  my_strcat(inbuf, "circumstances.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "It is not the purpose of this section to induce you to infringe any\n");
  my_strcat(inbuf, "patents or other property right claims or to contest validity of any\n");
  my_strcat(inbuf, "such claims; this section has the sole purpose of protecting the\n");
  my_strcat(inbuf, "integrity of the free software distribution system, which is\n");
  my_strcat(inbuf, "implemented by public license practices.  Many people have made\n");
  my_strcat(inbuf, "generous contributions to the wide range of software distributed\n");
  my_strcat(inbuf, "through that system in reliance on consistent application of that\n");
  my_strcat(inbuf, "system; it is up to the author/donor to decide if he or she is willing\n");
  my_strcat(inbuf, "to distribute software through any other system and a licensee cannot\n");
  my_strcat(inbuf, "impose that choice.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "This section is intended to make thoroughly clear what is believed to\n");
  my_strcat(inbuf, "be a consequence of the rest of this License.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  8. If the distribution and/or use of the Program is restricted in\n");
  my_strcat(inbuf, "certain countries either by patents or by copyrighted interfaces, the\n");
  my_strcat(inbuf, "original copyright holder who places the Program under this License\n");
  my_strcat(inbuf, "may add an explicit geographical distribution limitation excluding\n");
  my_strcat(inbuf, "those countries, so that distribution is permitted only in or among\n");
  my_strcat(inbuf, "countries not thus excluded.  In such case, this License incorporates\n");
  my_strcat(inbuf, "the limitation as if written in the body of this License.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  9. The Free Software Foundation may publish revised and/or new versions\n");
  my_strcat(inbuf, "of the General Public License from time to time.  Such new versions will\n");
  my_strcat(inbuf, "be similar in spirit to the present version, but may differ in detail to\n");
  my_strcat(inbuf, "address new problems or concerns.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "Each version is given a distinguishing version number.  If the Program\n");
  my_strcat(inbuf, "specifies a version number of this License which applies to it and any\n");
  my_strcat(inbuf, "later version, you have the option of following the terms and conditions\n");
  my_strcat(inbuf, "either of that version or of any later version published by the Free\n");
  my_strcat(inbuf, "Software Foundation.  If the Program does not specify a version number of\n");
  my_strcat(inbuf, "this License, you may choose any version ever published by the Free Software\n");
  my_strcat(inbuf, "Foundation.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  10. If you wish to incorporate parts of the Program into other free\n");
  my_strcat(inbuf, "programs whose distribution conditions are different, write to the author\n");
  my_strcat(inbuf, "to ask for permission.  For software which is copyrighted by the Free\n");
  my_strcat(inbuf, "Software Foundation, write to the Free Software Foundation; we sometimes\n");
  my_strcat(inbuf, "make exceptions for this.  Our decision will be guided by the two goals\n");
  my_strcat(inbuf, "of preserving the free status of all derivatives of our free software and\n");
  my_strcat(inbuf, "of promoting the sharing and reuse of software generally.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "			    NO WARRANTY\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY\n");
  my_strcat(inbuf, "FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN\n");
  my_strcat(inbuf, "OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES\n");
  my_strcat(inbuf, "PROVIDE THE PROGRAM AS IS WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED\n");
  my_strcat(inbuf, "OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n");
  my_strcat(inbuf, "MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS\n");
  my_strcat(inbuf, "TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE\n");
  my_strcat(inbuf, "PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,\n");
  my_strcat(inbuf, "REPAIR OR CORRECTION.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n");
  my_strcat(inbuf, "WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR\n");
  my_strcat(inbuf, "REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,\n");
  my_strcat(inbuf, "INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING\n");
  my_strcat(inbuf, "OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED\n");
  my_strcat(inbuf, "TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY\n");
  my_strcat(inbuf, "YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER\n");
  my_strcat(inbuf, "PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE\n");
  my_strcat(inbuf, "POSSIBILITY OF SUCH DAMAGES.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "		     END OF TERMS AND CONDITIONS\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "	    How to Apply These Terms to Your New Programs\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  If you develop a new program, and you want it to be of the greatest\n");
  my_strcat(inbuf, "possible use to the public, the best way to achieve this is to make it\n");
  my_strcat(inbuf, "free software which everyone can redistribute and change under these terms.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  To do so, attach the following notices to the program.  It is safest\n");
  my_strcat(inbuf, "to attach them to the start of each source file to most effectively\n");
  my_strcat(inbuf, "convey the exclusion of warranty; and each file should have at least\n");
  my_strcat(inbuf, "the copyright line and a pointer to where the full notice is found.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    <one line to give the program's name and a brief idea of what it does.>\n");
  my_strcat(inbuf, "    Copyright (C) <year>  <name of author>\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    This program is free software; you can redistribute it and/or modify\n");
  my_strcat(inbuf, "    it under the terms of the GNU General Public License as published by\n");
  my_strcat(inbuf, "    the Free Software Foundation; either version 2 of the License, or\n");
  my_strcat(inbuf, "    (at your option) any later version.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    This program is distributed in the hope that it will be useful,\n");
  my_strcat(inbuf, "    but WITHOUT ANY WARRANTY; without even the implied warranty of\n");
  my_strcat(inbuf, "    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n");
  my_strcat(inbuf, "    GNU General Public License for more details.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    You should have received a copy of the GNU General Public License\n");
  my_strcat(inbuf, "    along with this program; if not, write to the Free Software\n");
  my_strcat(inbuf, "    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "Also add information on how to contact you by electronic and paper mail.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "If the program is interactive, make it output a short notice like this\n");
  my_strcat(inbuf, "when it starts in an interactive mode:\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "    Gnomovision version 69, Copyright (C) year  name of author\n");
  my_strcat(inbuf, "    Gnomovision comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n");
  my_strcat(inbuf, "    This is free software, and you are welcome to redistribute it\n");
  my_strcat(inbuf, "    under certain conditions; type `show c' for details.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "The hypothetical commands `show w' and `show c' should show the appropriate\n");
  my_strcat(inbuf, "parts of the General Public License.  Of course, the commands you use may\n");
  my_strcat(inbuf, "be called something other than `show w' and `show c'; they could even be\n");
  my_strcat(inbuf, "mouse-clicks or menu items--whatever suits your program.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "You should also get your employer (if you work as a programmer) or your\n");
  my_strcat(inbuf, "school, if any, to sign a copyright disclaimer for the program, if\n");
  my_strcat(inbuf, "necessary.  Here is a sample; alter the names:\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  Yoyodyne, Inc., hereby disclaims all copyright interest in the program\n");
  my_strcat(inbuf, "  `Gnomovision' (which makes passes at compilers) written by James Hacker.\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "  <signature of Ty Coon>, 1 April 1989\n");
  my_strcat(inbuf, "  Ty Coon, President of Vice\n");
  my_strcat(inbuf, "\n");
  my_strcat(inbuf, "This General Public License does not permit incorporating your program into\n");
  my_strcat(inbuf, "proprietary programs.  If your program is a subroutine library, you may\n");
  my_strcat(inbuf, "consider it more useful to permit linking proprietary applications with the\n");
  my_strcat(inbuf, "library.  If this is what you want to do, use the GNU Library General\n");
  my_strcat(inbuf, "Public License instead of this License.\n");

  my_strcat(inbuf, "\n");
}


typedef struct _IO_FILE FILE;



typedef struct _IO_FILE __FILE;
typedef unsigned int wint_t;

typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;


typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;


enum
{
  __GCONV_OK = 0,
  __GCONV_NOCONV,
  __GCONV_NODB,
  __GCONV_NOMEM,

  __GCONV_EMPTY_INPUT,
  __GCONV_FULL_OUTPUT,
  __GCONV_ILLEGAL_INPUT,
  __GCONV_INCOMPLETE_INPUT,

  __GCONV_ILLEGAL_DESCRIPTOR,
  __GCONV_INTERNAL_ERROR
};



enum
{
  __GCONV_IS_LAST = 0x0001,
  __GCONV_IGNORE_ERRORS = 0x0002
};



struct __gconv_step;
struct __gconv_step_data;
struct __gconv_loaded_object;
struct __gconv_trans_data;



typedef int (*__gconv_fct) (struct __gconv_step *, struct __gconv_step_data *,
       __const unsigned char **, __const unsigned char *,
       unsigned char **, size_t *, int, int);


typedef wint_t (*__gconv_btowc_fct) (struct __gconv_step *, unsigned char);


typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);



typedef int (*__gconv_trans_fct) (struct __gconv_step *,
      struct __gconv_step_data *, void *,
      __const unsigned char *,
      __const unsigned char **,
      __const unsigned char *, unsigned char **,
      size_t *);


typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
       __const unsigned char *,
       unsigned char *, unsigned char *);


typedef int (*__gconv_trans_query_fct) (__const char *, __const char ***,
     size_t *);


typedef int (*__gconv_trans_init_fct) (void **, const char *);
typedef void (*__gconv_trans_end_fct) (void *);

struct __gconv_trans_data
{

  __gconv_trans_fct __trans_fct;
  __gconv_trans_context_fct __trans_context_fct;
  __gconv_trans_end_fct __trans_end_fct;
  void *__data;
  struct __gconv_trans_data *__next;
};



struct __gconv_step
{
  struct __gconv_loaded_object *__shlib_handle;
  __const char *__modname;

  int __counter;

  char *__from_name;
  char *__to_name;

  __gconv_fct __fct;
  __gconv_btowc_fct __btowc_fct;
  __gconv_init_fct __init_fct;
  __gconv_end_fct __end_fct;



  int __min_needed_from;
  int __max_needed_from;
  int __min_needed_to;
  int __max_needed_to;


  int __stateful;

  void *__data;
};



struct __gconv_step_data
{
  unsigned char *__outbuf;
  unsigned char *__outbufend;



  int __flags;



  int __invocation_counter;



  int __internal_use;

  __mbstate_t *__statep;
  __mbstate_t __state;



  struct __gconv_trans_data *__trans;
};



typedef struct __gconv_info
{
  size_t __nsteps;
  struct __gconv_step *__steps;
  __extension__ struct __gconv_step_data __data [];
} *__gconv_t;

typedef union
{
  struct __gconv_info __cd;
  struct
  {
    struct __gconv_info __cd;
    struct __gconv_step_data __data;
  } __combined;
} _G_iconv_t;

typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));

struct _IO_jump_t; struct _IO_FILE;

typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;

};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};

struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;



  int _flags2;

  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;

  __off64_t _offset;





  void *__pad1;
  void *__pad2;

  int _mode;

  char _unused2[15 * sizeof (int) - 2 * sizeof (void *)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;

typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
     size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);

extern int __underflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern int __uflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern int __overflow (_IO_FILE *, int) __attribute__ ((__nothrow__));
extern wint_t __wunderflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern wint_t __wuflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern wint_t __woverflow (_IO_FILE *, wint_t) __attribute__ ((__nothrow__));

extern int _IO_getc (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_putc (int __c, _IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__));

extern int _IO_peekc_locked (_IO_FILE *__fp) __attribute__ ((__nothrow__));





extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__));

extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) __attribute__ ((__nothrow__));
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) __attribute__ ((__nothrow__));

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) __attribute__ ((__nothrow__));
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) __attribute__ ((__nothrow__));

extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__));


typedef _G_fpos_t fpos_t;








extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;









extern int remove (__const char *__filename) __attribute__ ((__nothrow__));

extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));









extern FILE *tmpfile (void);

extern char *tmpnam (char *__s) __attribute__ ((__nothrow__));





extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__));

extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);


extern int fflush_unlocked (FILE *__stream);







extern FILE *fopen (__const char *__restrict __filename,
      __const char *__restrict __modes);




extern FILE *freopen (__const char *__restrict __filename,
        __const char *__restrict __modes,
        FILE *__restrict __stream);



extern FILE *fdopen (int __fd, __const char *__modes) __attribute__ ((__nothrow__));




extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__));





extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__));








extern int fprintf (FILE *__restrict __stream,
      __const char *__restrict __format, ...);




extern int printf (__const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      __const char *__restrict __format, ...) __attribute__ ((__nothrow__));
















extern int snprintf (char *__restrict __s, size_t __maxlen,
       __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));








extern int fscanf (FILE *__restrict __stream,
     __const char *__restrict __format, ...);




extern int scanf (__const char *__restrict __format, ...);

extern int sscanf (__const char *__restrict __s,
     __const char *__restrict __format, ...) __attribute__ ((__nothrow__));







extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);


extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);

extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);


extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);






extern char *gets (char *__s);







extern int fputs (__const char *__restrict __s, FILE *__restrict __stream);





extern int puts (__const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream);




extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s);


extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream);
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream);




extern void rewind (FILE *__stream);








extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, __const fpos_t *__pos);



extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__));

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__));




extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__));








extern void perror (__const char *__s);






extern int sys_nerr;
extern __const char *__const sys_errlist[];





extern int fileno (FILE *__stream) __attribute__ ((__nothrow__));




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__));

extern FILE *popen (__const char *__command, __const char *__modes);





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) __attribute__ ((__nothrow__));

extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__));


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__));

typedef int ptrdiff_t;

extern void * malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));


extern void * calloc (size_t __nmemb, size_t __size) __attribute__ ((__nothrow__))
       __attribute__ ((__malloc__));



extern void * realloc (void * __ptr, size_t __size) __attribute__ ((__nothrow__))

       __attribute__ ((__malloc__));


extern void free (void * __ptr) __attribute__ ((__nothrow__));


extern void cfree (void * __ptr) __attribute__ ((__nothrow__));


extern void * memalign (size_t __alignment, size_t __size) __attribute__ ((__nothrow__));


extern void * valloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));



extern void * pvalloc (size_t __size) __attribute__ ((__nothrow__))
       __attribute__ ((__malloc__));



extern void * (*__morecore) (ptrdiff_t __size);


extern void * __default_morecore (ptrdiff_t __size) __attribute__ ((__nothrow__))
       __attribute__ ((__malloc__));



struct mallinfo {
  int arena;
  int ordblks;
  int smblks;
  int hblks;
  int hblkhd;
  int usmblks;
  int fsmblks;
  int uordblks;
  int fordblks;
  int keepcost;
};


extern struct mallinfo mallinfo (void) __attribute__ ((__nothrow__));

extern int mallopt (int __param, int __val) __attribute__ ((__nothrow__));



extern int malloc_trim (size_t __pad) __attribute__ ((__nothrow__));



extern size_t malloc_usable_size (void * __ptr) __attribute__ ((__nothrow__));


extern void malloc_stats (void) __attribute__ ((__nothrow__));


extern void * malloc_get_state (void) __attribute__ ((__nothrow__));



extern int malloc_set_state (void * __ptr) __attribute__ ((__nothrow__));




extern void (*__malloc_initialize_hook) (void);

extern void (*__free_hook) (void * __ptr, __const void *);

extern void * (*__malloc_hook) (size_t __size, __const void *);

extern void * (*__realloc_hook) (void * __ptr, size_t __size, __const void *);


extern void * (*__memalign_hook) (size_t __alignment, size_t __size, __const void *);


extern void (*__after_morecore_hook) (void);


extern void __malloc_check_init (void) __attribute__ ((__nothrow__));



extern void __assert_fail (__const char *__assertion, __const char *__file,
      unsigned int __line, __const char *__function)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));


extern void __assert_perror_fail (int __errnum, __const char *__file,
      unsigned int __line,
      __const char *__function)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));




extern void __assert (const char *__assertion, const char *__file, int __line)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));






static HWord g_serviceFn ( HWord arg1, HWord arg2 )
{
   switch (arg1) {
      case 0:
         exit(0);
      case 1:
         putchar(arg2);
         return 0;
      case 2:
         return (HWord)malloc(arg2);
      case 3:
         free((void*)arg2);
         return 0;
      default:
         ((void) ((0) ? 0 : (__assert_fail ("0", "./bz2.c", 6428, "__PRETTY_FUNCTION__"), 0)));
   }
}


int main ( void )
{
   int r;
   int bit;
   int i;

   serviceFn = g_serviceFn;

   set_inbuf();
   nIn = vex_strlen(inbuf)+1;
   vex_printf( "%d bytes read\n", nIn );

   nZ = 1000000;
   r = BZ2_bzBuffToBuffCompress (
          zbuf, &nZ, inbuf, nIn, 9, 3 , 30 );

   if (r != 0) {
     vex_printf("initial compress failed!\n");
     (*serviceFn)(0,0);
   }
   vex_printf( "%d after compression\n", nZ );

   for (bit = 0; bit < nZ*8; bit += (bit < 35 ? 1 : 137)) {

      flip_bit ( bit );
      nOut = (1000000 + 1000000);
      r = BZ2_bzBuffToBuffDecompress (
             outbuf, &nOut, zbuf, nZ, 1 , 0 );


      if (r != 0) {

      } else {
         if (nOut != nIn) {
           vex_printf( "nIn/nOut mismatch %d %d\n", nIn, nOut );
           (*serviceFn)(0,0);
         } else {
           for (i = 0; i < nOut; i++)
             if (inbuf[i] != outbuf[i]) {
                vex_printf( "mismatch at %d\n", i );
                (*serviceFn)(0,0);
           }
           if (i == nOut) vex_printf( "really ok!\n" );
         }
      }

      flip_bit ( bit );
   }

   vex_printf( "all ok\n" );
   (*serviceFn)(0,0);

   return 0;
}
