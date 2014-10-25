/*
** emfloat.c
** Source for emulated floating-point routines.
** BYTEmark (tm)
** BYTE's Native Mode Benchmarks
** Rick Grehan, BYTE Magazine.
**
** Created:
** Last update: 3/95
**
** DISCLAIMER
** The source, executable, and documentation files that comprise
** the BYTEmark benchmarks are made available on an "as is" basis.
** This means that we at BYTE Magazine have made every reasonable
** effort to verify that the there are no errors in the source and
** executable code.  We cannot, however, guarantee that the programs
** are error-free.  Consequently, McGraw-HIll and BYTE Magazine make
** no claims in regard to the fitness of the source code, executable
** code, and documentation of the BYTEmark.
**  Furthermore, BYTE Magazine, McGraw-Hill, and all employees
** of McGraw-Hill cannot be held responsible for any damages resulting
** from the use of this code or the results obtained from using
** this code.
*/

#include "../pub/libvex_basictypes.h"

static HWord (*serviceFn)(HWord,HWord) = 0;


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////

static char* my_strcpy ( char* dest, const char* src )
{
   char* dest_orig = dest;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

static void* my_memcpy ( void *dest, const void *src, int sz )
{
   const char *s = (const char *)src;
   char *d = (char *)dest;

   while (sz--)
      *d++ = *s++;

   return dest;
}

static void* my_memmove( void *dst, const void *src, unsigned int len )
{
    register char *d;
    register char *s;
    if ( dst > src ) {
        d = (char *)dst + len - 1;
        s = (char *)src + len - 1;
        while ( len >= 4 ) {
            *d-- = *s--;
            *d-- = *s--;
            *d-- = *s--;
            *d-- = *s--;
            len -= 4;
        }
        while ( len-- ) {
            *d-- = *s--;
        }
    } else if ( dst < src ) {
        d = (char *)dst;
        s = (char *)src;
        while ( len >= 4 ) {
            *d++ = *s++;
            *d++ = *s++;
            *d++ = *s++;
            *d++ = *s++;
            len -= 4;
        }
        while ( len-- ) {
            *d++ = *s++;
        }
    }
    return dst;
}

/////////////////////////////////////////////////////////////////////

static void vexxx_log_bytes ( char* p, int n )
{
   int i;
   for (i = 0; i < n; i++)
      (*serviceFn)( 1, (int)p[i] );
}

/*---------------------------------------------------------*/
/*--- vexxx_printf                                        ---*/
/*---------------------------------------------------------*/

/* This should be the only <...> include in the entire VEXXX library.
   New code for vexxx_util.c should go above this point. */
#include <stdarg.h>

static HChar vexxx_toupper ( HChar c )
{
   if (c >= 'a' && c <= 'z')
      return toHChar(c + ('A' - 'a'));
   else
      return c;
}

static Int vexxx_strlen ( const HChar* str )
{
   Int i = 0;
   while (str[i] != 0) i++;
   return i;
}

Bool vexxx_streq ( const HChar* s1, const HChar* s2 )
{
   while (True) {
      if (*s1 == 0 && *s2 == 0)
         return True;
      if (*s1 != *s2)
         return False;
      s1++;
      s2++;
   }
}

/* Some flags.  */
#define VG_MSG_SIGNED    1 /* The value is signed. */
#define VG_MSG_ZJUSTIFY  2 /* Must justify with '0'. */
#define VG_MSG_LJUSTIFY  4 /* Must justify on the left. */
#define VG_MSG_PAREN     8 /* Parenthesize if present (for %y) */
#define VG_MSG_COMMA    16 /* Add commas to numbers (for %d, %u) */

/* Copy a string into the buffer. */
static UInt
myvprintf_str ( void(*send)(HChar), Int flags, Int width, HChar* str, 
                Bool capitalise )
{
#  define MAYBE_TOUPPER(ch) toHChar(capitalise ? vexxx_toupper(ch) : (ch))
   UInt ret = 0;
   Int i, extra;
   Int len = vexxx_strlen(str);

   if (width == 0) {
      ret += len;
      for (i = 0; i < len; i++)
         send(MAYBE_TOUPPER(str[i]));
      return ret;
   }

   if (len > width) {
      ret += width;
      for (i = 0; i < width; i++)
         send(MAYBE_TOUPPER(str[i]));
      return ret;
   }

   extra = width - len;
   if (flags & VG_MSG_LJUSTIFY) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ');
   }
   ret += len;
   for (i = 0; i < len; i++)
      send(MAYBE_TOUPPER(str[i]));
   if (!(flags & VG_MSG_LJUSTIFY)) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ');
   }

#  undef MAYBE_TOUPPER

   return ret;
}

/* Write P into the buffer according to these args:
 *  If SIGN is true, p is a signed.
 *  BASE is the base.
 *  If WITH_ZERO is true, '0' must be added.
 *  WIDTH is the width of the field.
 */
static UInt
myvprintf_int64 ( void(*send)(HChar), Int flags, Int base, Int width, ULong pL)
{
   HChar buf[40];
   Int   ind = 0;
   Int   i, nc = 0;
   Bool  neg = False;
   HChar *digits = "0123456789ABCDEF";
   UInt  ret = 0;
   UInt  p = (UInt)pL;

   if (base < 2 || base > 16)
      return ret;
 
   if ((flags & VG_MSG_SIGNED) && (Int)p < 0) {
      p   = - (Int)p;
      neg = True;
   }

   if (p == 0)
      buf[ind++] = '0';
   else {
      while (p > 0) {
         if ((flags & VG_MSG_COMMA) && 10 == base &&
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

   if (width > 0 && !(flags & VG_MSG_LJUSTIFY)) {
      for(; ind < width; ind++) {
	//vassert(ind < 39);
         buf[ind] = toHChar((flags & VG_MSG_ZJUSTIFY) ? '0': ' ');
      }
   }

   /* Reverse copy to buffer.  */
   ret += ind;
   for (i = ind -1; i >= 0; i--) {
      send(buf[i]);
   }
   if (width > 0 && (flags & VG_MSG_LJUSTIFY)) {
      for(; ind < width; ind++) {
	 ret++;
         send(' ');  // Never pad with zeroes on RHS -- changes the value!
      }
   }
   return ret;
}


/* A simple vprintf().  */
static 
UInt vprintf_wrk ( void(*send)(HChar), const HChar *format, va_list vargs )
{
   UInt ret = 0;
   int i;
   int flags;
   int width;
   Bool is_long;

   /* We assume that vargs has already been initialised by the 
      caller, using va_start, and that the caller will similarly
      clean up with va_end.
   */

   for (i = 0; format[i] != 0; i++) {
      if (format[i] != '%') {
         send(format[i]);
	 ret++;
         continue;
      }
      i++;
      /* A '%' has been found.  Ignore a trailing %. */
      if (format[i] == 0)
         break;
      if (format[i] == '%') {
         /* `%%' is replaced by `%'. */
         send('%');
	 ret++;
         continue;
      }
      flags = 0;
      is_long = False;
      width = 0; /* length of the field. */
      if (format[i] == '(') {
	 flags |= VG_MSG_PAREN;
	 i++;
      }
      /* If ',' follows '%', commas will be inserted. */
      if (format[i] == ',') {
         flags |= VG_MSG_COMMA;
         i++;
      }
      /* If '-' follows '%', justify on the left. */
      if (format[i] == '-') {
         flags |= VG_MSG_LJUSTIFY;
         i++;
      }
      /* If '0' follows '%', pads will be inserted. */
      if (format[i] == '0') {
         flags |= VG_MSG_ZJUSTIFY;
         i++;
      }
      /* Compute the field length. */
      while (format[i] >= '0' && format[i] <= '9') {
         width *= 10;
         width += format[i++] - '0';
      }
      while (format[i] == 'l') {
         i++;
         is_long = True;
      }

      switch (format[i]) {
         case 'd': /* %d */
            flags |= VG_MSG_SIGNED;
            if (is_long)
               ret += myvprintf_int64(send, flags, 10, width, 
				      (ULong)(va_arg (vargs, Long)));
            else
               ret += myvprintf_int64(send, flags, 10, width, 
				      (ULong)(va_arg (vargs, Int)));
            break;
         case 'u': /* %u */
            if (is_long)
               ret += myvprintf_int64(send, flags, 10, width, 
				      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, flags, 10, width, 
				      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'p': /* %p */
	    ret += 2;
            send('0');
            send('x');
            ret += myvprintf_int64(send, flags, 16, width, 
				   (ULong)((HWord)va_arg (vargs, void *)));
            break;
         case 'x': /* %x */
            if (is_long)
               ret += myvprintf_int64(send, flags, 16, width, 
				      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, flags, 16, width, 
				      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'c': /* %c */
	    ret++;
            send(toHChar(va_arg (vargs, int)));
            break;
         case 's': case 'S': { /* %s */
            char *str = va_arg (vargs, char *);
            if (str == (char*) 0) str = "(null)";
            ret += myvprintf_str(send, flags, width, str, 
                                 toBool(format[i]=='S'));
            break;
	 }
#        if 0
	 case 'y': { /* %y - print symbol */
	    Addr a = va_arg(vargs, Addr);

            HChar *name;
	    if (VG_(get_fnname_w_offset)(a, &name)) {
               HChar buf[1 + VG_strlen(name) + 1 + 1];
	       if (flags & VG_MSG_PAREN) {
                  VG_(sprintf)(str, "(%s)", name):
	       } else {
                  VG_(sprintf)(str, "%s", name):
               }
	       ret += myvprintf_str(send, flags, width, buf, 0);
	    }
	    break;
	 }
#        endif
         default:
            break;
      }
   }
   return ret;
}


/* A general replacement for printf().  Note that only low-level 
   debugging info should be sent via here.  The official route is to
   to use vg_message().  This interface is deprecated.
*/
static HChar myprintf_buf[1000];
static Int   n_myprintf_buf;

static void add_to_myprintf_buf ( HChar c )
{
   if (c == '\n' || n_myprintf_buf >= 1000-10 /*paranoia*/ ) {
      (*vexxx_log_bytes)( myprintf_buf, vexxx_strlen(myprintf_buf) );
      n_myprintf_buf = 0;
      myprintf_buf[n_myprintf_buf] = 0;      
   }
   myprintf_buf[n_myprintf_buf++] = c;
   myprintf_buf[n_myprintf_buf] = 0;
}

static UInt vexxx_printf ( const char *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs,format);
   
   n_myprintf_buf = 0;
   myprintf_buf[n_myprintf_buf] = 0;      
   ret = vprintf_wrk ( add_to_myprintf_buf, format, vargs );

   if (n_myprintf_buf > 0) {
      (*vexxx_log_bytes)( myprintf_buf, n_myprintf_buf );
   }

   va_end(vargs);

   return ret;
}

/*---------------------------------------------------------------*/
/*--- end                                          vexxx_util.c ---*/
/*---------------------------------------------------------------*/


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////

//#include <stdio.h>
//#include <string.h>
//#include <malloc.h>

typedef unsigned char uchar;
typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned long ulong;
typedef int int32;              /* Signed 32 bit integer */

#define INTERNAL_FPF_PRECISION 4
#define CPUEMFLOATLOOPMAX 500000L
#define EMFARRAYSIZE 3000L

typedef struct {
        int adjust;             /* Set adjust code */
        ulong request_secs;     /* # of seconds requested */
        ulong arraysize;        /* Size of array */
        ulong loops;            /* Loops per iterations */
        double emflops;         /* Results */
} EmFloatStruct;



/* Is this a 64 bit architecture? If so, this will define LONG64 */
/* Uwe F. Mayer 15 November 1997                                 */
// #include "pointer.h"

#define u8 unsigned char
#define u16 unsigned short
#ifdef LONG64
#define u32 unsigned int
#else
#define u32 unsigned long
#endif
#define uchar unsigned char
#define ulong unsigned long

#define MAX_EXP 32767L
#define MIN_EXP (-32767L)

#define IFPF_IS_ZERO 0
#define IFPF_IS_SUBNORMAL 1
#define IFPF_IS_NORMAL 2
#define IFPF_IS_INFINITY 3
#define IFPF_IS_NAN 4
#define IFPF_TYPE_COUNT 5

#define ZERO_ZERO                       0
#define ZERO_SUBNORMAL                  1
#define ZERO_NORMAL                     2
#define ZERO_INFINITY                   3
#define ZERO_NAN                        4

#define SUBNORMAL_ZERO                  5
#define SUBNORMAL_SUBNORMAL             6
#define SUBNORMAL_NORMAL                7
#define SUBNORMAL_INFINITY              8
#define SUBNORMAL_NAN                   9

#define NORMAL_ZERO                     10
#define NORMAL_SUBNORMAL                11
#define NORMAL_NORMAL                   12
#define NORMAL_INFINITY                 13
#define NORMAL_NAN                      14

#define INFINITY_ZERO                   15
#define INFINITY_SUBNORMAL              16
#define INFINITY_NORMAL                 17
#define INFINITY_INFINITY               18
#define INFINITY_NAN                    19

#define NAN_ZERO                        20
#define NAN_SUBNORMAL                   21
#define NAN_NORMAL                      22
#define NAN_INFINITY                    23
#define NAN_NAN                         24
#define OPERAND_ZERO                    0
#define OPERAND_SUBNORMAL               1
#define OPERAND_NORMAL                  2
#define OPERAND_INFINITY                3
#define OPERAND_NAN                     4

typedef struct
{
        u8 type;        /* Indicates, NORMAL, SUBNORMAL, etc. */
        u8 sign;        /* Mantissa sign */
        short exp;      /* Signed exponent...no bias */
        u16 mantissa[INTERNAL_FPF_PRECISION];
} InternalFPF;

static
void SetupCPUEmFloatArrays(InternalFPF *abase,
        InternalFPF *bbase, InternalFPF *cbase, ulong arraysize);
static
ulong DoEmFloatIteration(InternalFPF *abase,
        InternalFPF *bbase, InternalFPF *cbase,
        ulong arraysize, ulong loops);

static void SetInternalFPFZero(InternalFPF *dest,
                        uchar sign);
static void SetInternalFPFInfinity(InternalFPF *dest,
                        uchar sign);
static void SetInternalFPFNaN(InternalFPF *dest);
static int IsMantissaZero(u16 *mant);
static void Add16Bits(u16 *carry,u16 *a,u16 b,u16 c);
static void Sub16Bits(u16 *borrow,u16 *a,u16 b,u16 c);
static void ShiftMantLeft1(u16 *carry,u16 *mantissa);
static void ShiftMantRight1(u16 *carry,u16 *mantissa);
static void StickyShiftRightMant(InternalFPF *ptr,int amount);
static void normalize(InternalFPF *ptr);
static void denormalize(InternalFPF *ptr,int minimum_exponent);
static void RoundInternalFPF(InternalFPF *ptr);
static void choose_nan(InternalFPF *x,InternalFPF *y,InternalFPF *z,
                int intel_flag);
static void AddSubInternalFPF(uchar operation,InternalFPF *x,
                InternalFPF *y,InternalFPF *z);
static void MultiplyInternalFPF(InternalFPF *x,InternalFPF *y,
                        InternalFPF *z);
static void DivideInternalFPF(InternalFPF *x,InternalFPF *y, 
                        InternalFPF *z);

static void Int32ToInternalFPF(int32 mylong,
                InternalFPF *dest);
static int InternalFPFToString(char *dest,
                InternalFPF *src);

static int32 randnum(int32 lngval);

static int32 randwc(int32 num)
{
	return(randnum((int32)0)%num);
}

static int32 randw[2] = { (int32)13 , (int32)117 };
static int32 randnum(int32 lngval)
{
	register int32 interm;

	if (lngval!=(int32)0)
	{	randw[0]=(int32)13; randw[1]=(int32)117; }

	interm=(randw[0]*(int32)254754+randw[1]*(int32)529562)%(int32)999563;
	randw[1]=randw[0];
	randw[0]=interm;
	return(interm);
}


static 
void SetupCPUEmFloatArrays(InternalFPF *abase,
                InternalFPF *bbase,
                InternalFPF *cbase,
                ulong arraysize)
{
ulong i;
InternalFPF locFPF1,locFPF2;

randnum((int32)13);

for(i=0;i<arraysize;i++)
{/*       LongToInternalFPF(randwc(50000L),&locFPF1); */
        Int32ToInternalFPF(randwc((int32)50000),&locFPF1);
 /*       LongToInternalFPF(randwc(50000L)+1L,&locFPF2); */
        Int32ToInternalFPF(randwc((int32)50000)+(int32)1,&locFPF2);
        DivideInternalFPF(&locFPF1,&locFPF2,abase+i);
 /*       LongToInternalFPF(randwc(50000L)+1L,&locFPF2); */
        Int32ToInternalFPF(randwc((int32)50000)+(int32)1,&locFPF2);
        DivideInternalFPF(&locFPF1,&locFPF2,bbase+i);
}
return;
}


static char* str1 = "loops %d\n";
static 
ulong DoEmFloatIteration(InternalFPF *abase,
                InternalFPF *bbase,
                InternalFPF *cbase,
                ulong arraysize, ulong loops)
{
static uchar jtable[16] = {0,0,0,0,1,1,1,1,2,2,2,2,2,3,3,3};
ulong i;
int number_of_loops;
 loops = 100;
number_of_loops=loops-1; /* the index of the first loop we run */

vexxx_printf(str1, (int)loops);

/*
** Each pass through the array performs operations in
** the followingratios:
**   4 adds, 4 subtracts, 5 multiplies, 3 divides
** (adds and subtracts being nearly the same operation)
*/

{
        for(i=0;i<arraysize;i++)
                switch(jtable[i % 16])
                {
                        case 0: /* Add */
                                AddSubInternalFPF(0,abase+i,
                                  bbase+i,
                                  cbase+i);
                                break;
                        case 1: /* Subtract */
                                AddSubInternalFPF(1,abase+i,
                                  bbase+i,
                                  cbase+i);
                                break;
                        case 2: /* Multiply */
                                MultiplyInternalFPF(abase+i,
                                  bbase+i,
                                  cbase+i);
                                break;
                        case 3: /* Divide */
                                DivideInternalFPF(abase+i,
                                  bbase+i,
                                  cbase+i);
                                break;
                }
{
  ulong j[8];   /* we test 8 entries */
  int k;
  ulong i;
  char buffer[1024];
  if (100==loops) /* the first loop */
    {
      j[0]=(ulong)2;
      j[1]=(ulong)6;
      j[2]=(ulong)10;
      j[3]=(ulong)14;
      j[4]=(ulong)(arraysize-14);
      j[5]=(ulong)(arraysize-10);
      j[6]=(ulong)(arraysize-6);
      j[7]=(ulong)(arraysize-2);
      for(k=0;k<8;k++){
	i=j[k];
	InternalFPFToString(buffer,abase+i);
	vexxx_printf("%6d: (%s) ",i,buffer);
	switch(jtable[i % 16])
	  {
	  case 0: my_strcpy(buffer,"+"); break;
	  case 1: my_strcpy(buffer,"-"); break;
	  case 2: my_strcpy(buffer,"*"); break;
	  case 3: my_strcpy(buffer,"/"); break;
	  }
	vexxx_printf("%s ",buffer);
	InternalFPFToString(buffer,bbase+i);
	vexxx_printf("(%s) = ",buffer);
	InternalFPFToString(buffer,cbase+i);
	vexxx_printf("%s\n",buffer);
      }
return 0;
    }
}
}
return 0;
}

/***********************
** SetInternalFPFZero **
************************
** Set an internal floating-point-format number to zero.
** sign determines the sign of the zero.
*/
static void SetInternalFPFZero(InternalFPF *dest,
                        uchar sign)
{
int i;          /* Index */

dest->type=IFPF_IS_ZERO;
dest->sign=sign;
dest->exp=MIN_EXP;
for(i=0;i<INTERNAL_FPF_PRECISION;i++)
        dest->mantissa[i]=0;
return;
}

/***************************
** SetInternalFPFInfinity **
****************************
** Set an internal floating-point-format number to infinity.
** This can happen if the exponent exceeds MAX_EXP.
** As above, sign picks the sign of infinity.
*/
static void SetInternalFPFInfinity(InternalFPF *dest,
                        uchar sign)
{
int i;          /* Index */

dest->type=IFPF_IS_INFINITY;
dest->sign=sign;
dest->exp=MIN_EXP;
for(i=0;i<INTERNAL_FPF_PRECISION;i++)
        dest->mantissa[i]=0;
return;
}

/**********************
** SetInternalFPFNaN **
***********************
** Set an internal floating-point-format number to Nan
** (not a number).  Note that we "emulate" an 80x87 as far
** as the mantissa bits go.
*/
static void SetInternalFPFNaN(InternalFPF *dest)
{
int i;          /* Index */

dest->type=IFPF_IS_NAN;
dest->exp=MAX_EXP;
dest->sign=1;
dest->mantissa[0]=0x4000;
for(i=1;i<INTERNAL_FPF_PRECISION;i++)
        dest->mantissa[i]=0;

return;
}

/*******************
** IsMantissaZero **
********************
** Pass this routine a pointer to an internal floating point format
** number's mantissa.  It checks for an all-zero mantissa.
** Returns 0 if it is NOT all zeros, !=0 otherwise.
*/
static int IsMantissaZero(u16 *mant)
{
int i;          /* Index */
int n;          /* Return value */

n=0;
for(i=0;i<INTERNAL_FPF_PRECISION;i++)
        n|=mant[i];

return(!n);
}

/**************
** Add16Bits **
***************
** Add b, c, and carry.  Retult in a.  New carry in carry.
*/
static void Add16Bits(u16 *carry,
                u16 *a,
                u16 b,
                u16 c)
{
u32 accum;              /* Accumulator */

/*
** Do the work in the 32-bit accumulator so we can return
** the carry.
*/
accum=(u32)b;
accum+=(u32)c;
accum+=(u32)*carry;
*carry=(u16)((accum & 0x00010000) ? 1 : 0);     /* New carry */
*a=(u16)(accum & 0xFFFF);       /* Result is lo 16 bits */
return;
}

/**************
** Sub16Bits **
***************
** Additive inverse of above.
*/
static void Sub16Bits(u16 *borrow,
                u16 *a,
                u16 b,
                u16 c)
{
u32 accum;              /* Accumulator */

accum=(u32)b;
accum-=(u32)c;
accum-=(u32)*borrow;
*borrow=(u32)((accum & 0x00010000) ? 1 : 0);    /* New borrow */
*a=(u16)(accum & 0xFFFF);
return;
}

/*******************
** ShiftMantLeft1 **
********************
** Shift a vector of 16-bit numbers left 1 bit.  Also provides
** a carry bit, which is shifted in at the beginning, and
** shifted out at the end.
*/
static void ShiftMantLeft1(u16 *carry,
                        u16 *mantissa)
{
int i;          /* Index */
int new_carry;
u16 accum;      /* Temporary holding placed */

for(i=INTERNAL_FPF_PRECISION-1;i>=0;i--)
{       accum=mantissa[i];
        new_carry=accum & 0x8000;       /* Get new carry */
        accum=accum<<1;                 /* Do the shift */
        if(*carry)
                accum|=1;               /* Insert previous carry */
        *carry=new_carry;
        mantissa[i]=accum;              /* Return shifted value */
}
return;
}

/********************
** ShiftMantRight1 **
*********************
** Shift a mantissa right by 1 bit.  Provides carry, as
** above
*/
static void ShiftMantRight1(u16 *carry,
                        u16 *mantissa)
{
int i;          /* Index */
int new_carry;
u16 accum;

for(i=0;i<INTERNAL_FPF_PRECISION;i++)
{       accum=mantissa[i];
        new_carry=accum & 1;            /* Get new carry */
        accum=accum>>1;
        if(*carry)
                accum|=0x8000;
        *carry=new_carry;
        mantissa[i]=accum;
}
return;
}


/*****************************
** StickyShiftMantRight **
******************************
** This is a shift right of the mantissa with a "sticky bit".
** I.E., if a carry of 1 is shifted out of the least significant
** bit, the least significant bit is set to 1.
*/
static void StickyShiftRightMant(InternalFPF *ptr,
                        int amount)
{
int i;          /* Index */
u16 carry;      /* Self-explanatory */
u16 *mantissa;

mantissa=ptr->mantissa;

if(ptr->type!=IFPF_IS_ZERO)     /* Don't bother shifting a zero */
{
        /*
        ** If the amount of shifting will shift everyting
        ** out of existence, then just clear the whole mantissa
        ** and set the lowmost bit to 1.
        */
        if(amount>=INTERNAL_FPF_PRECISION * 16)
        {
                for(i=0;i<INTERNAL_FPF_PRECISION-1;i++)
                        mantissa[i]=0;
                mantissa[INTERNAL_FPF_PRECISION-1]=1;
        }
        else
                for(i=0;i<amount;i++)
                {
                        carry=0;
                        ShiftMantRight1(&carry,mantissa);
                        if(carry)
                                mantissa[INTERNAL_FPF_PRECISION-1] |= 1;
                }
}
return;
}


/**************************************************
**         POST ARITHMETIC PROCESSING            **
**  (NORMALIZE, ROUND, OVERFLOW, AND UNDERFLOW)  **
**************************************************/

/**************
** normalize **
***************
** Normalize an internal-representation number.  Normalization
** discards empty most-significant bits.
*/
static void normalize(InternalFPF *ptr)
{
u16     carry;

/*
** As long as there's a highmost 0 bit, shift the significand
** left 1 bit.  Each time you do this, though, you've
** gotta decrement the exponent.
*/
while ((ptr->mantissa[0] & 0x8000) == 0)
{
        carry = 0;
        ShiftMantLeft1(&carry, ptr->mantissa);
        ptr->exp--;
}
return;
}

/****************
** denormalize **
*****************
** Denormalize an internal-representation number.  This means
** shifting it right until its exponent is equivalent to
** minimum_exponent. (You have to do this often in order
** to perform additions and subtractions).
*/
static void denormalize(InternalFPF *ptr,
                int minimum_exponent)
{
long exponent_difference;

if (IsMantissaZero(ptr->mantissa))
{
        vexxx_printf("Error:  zero significand in denormalize\n");
}

exponent_difference = ptr->exp-minimum_exponent;
if (exponent_difference < 0)
{
        /*
        ** The number is subnormal
        */
        exponent_difference = -exponent_difference;
        if (exponent_difference >= (INTERNAL_FPF_PRECISION * 16))
        {
                /* Underflow */
                SetInternalFPFZero(ptr, ptr->sign);
        }
        else
        {
                ptr->exp+=exponent_difference;
                StickyShiftRightMant(ptr, exponent_difference);
        }
}
return;
}


/*********************
** RoundInternalFPF **
**********************
** Round an internal-representation number.
** The kind of rounding we do here is simplest...referred to as
** "chop".  "Extraneous" rightmost bits are simply hacked off.
*/
void RoundInternalFPF(InternalFPF *ptr)
{
/* int i; */

if (ptr->type == IFPF_IS_NORMAL ||
        ptr->type == IFPF_IS_SUBNORMAL)
{
        denormalize(ptr, MIN_EXP);
        if (ptr->type != IFPF_IS_ZERO)
        {

                /* clear the extraneous bits */
                ptr->mantissa[3] &= 0xfff8;
/*              for (i=4; i<INTERNAL_FPF_PRECISION; i++)
                {
                        ptr->mantissa[i] = 0;
                }
*/
                /*
                ** Check for overflow
                */
/*              Does not do anything as ptr->exp is a short and MAX_EXP=37268
		if (ptr->exp > MAX_EXP)
                {
                        SetInternalFPFInfinity(ptr, ptr->sign);
                }
*/
        }
}
return;
}

/*******************************************************
**  ARITHMETIC OPERATIONS ON INTERNAL REPRESENTATION  **
*******************************************************/

/***************
** choose_nan **
****************
** Called by routines that are forced to perform math on
** a pair of NaN's.  This routine "selects" which NaN is
** to be returned.
*/
static void choose_nan(InternalFPF *x,
                InternalFPF *y,
                InternalFPF *z,
                int intel_flag)
{
int i;

/*
** Compare the two mantissas,
** return the larger.  Note that we will be emulating
** an 80387 in this operation.
*/
for (i=0; i<INTERNAL_FPF_PRECISION; i++)
{
        if (x->mantissa[i] > y->mantissa[i])
        {
                my_memmove((void *)x,(void *)z,sizeof(InternalFPF));
                return;
        }
        if (x->mantissa[i] < y->mantissa[i])
        {
                my_memmove((void *)y,(void *)z,sizeof(InternalFPF));
                return;
        }
}

/*
** They are equal
*/
if (!intel_flag)
        /* if the operation is addition */
        my_memmove((void *)x,(void *)z,sizeof(InternalFPF));
else
        /* if the operation is multiplication */
        my_memmove((void *)y,(void *)z,sizeof(InternalFPF));
return;
}


/**********************
** AddSubInternalFPF **
***********************
** Adding or subtracting internal-representation numbers.
** Internal-representation numbers pointed to by x and y are
** added/subtracted and the result returned in z.
*/
static void AddSubInternalFPF(uchar operation,
                InternalFPF *x,
                InternalFPF *y,
                InternalFPF *z)
{
int exponent_difference;
u16 borrow;
u16 carry;
int i;
InternalFPF locx,locy;  /* Needed since we alter them */

/*
** Following big switch statement handles the
** various combinations of operand types.
*/
switch ((x->type * IFPF_TYPE_COUNT) + y->type)
{
case ZERO_ZERO:
        my_memmove((void *)x,(void *)z,sizeof(InternalFPF));
        if (x->sign ^ y->sign ^ operation)
        {
                z->sign = 0; /* positive */
        }
        break;

case NAN_ZERO:
case NAN_SUBNORMAL:
case NAN_NORMAL:
case NAN_INFINITY:
case SUBNORMAL_ZERO:
case NORMAL_ZERO:
case INFINITY_ZERO:
case INFINITY_SUBNORMAL:
case INFINITY_NORMAL:
        my_memmove((void *)x,(void *)z,sizeof(InternalFPF));
        break;


case ZERO_NAN:
case SUBNORMAL_NAN:
case NORMAL_NAN:
case INFINITY_NAN:
        my_memmove((void *)y,(void *)z,sizeof(InternalFPF));
        break;

case ZERO_SUBNORMAL:
case ZERO_NORMAL:
case ZERO_INFINITY:
case SUBNORMAL_INFINITY:
case NORMAL_INFINITY:
        my_memmove((void *)y,(void *)z,sizeof(InternalFPF));
        z->sign ^= operation;
        break;

case SUBNORMAL_SUBNORMAL:
case SUBNORMAL_NORMAL:
case NORMAL_SUBNORMAL:
case NORMAL_NORMAL:
        /*
        ** Copy x and y to locals, since we may have
        ** to alter them.
        */
        my_memmove((void *)&locx,(void *)x,sizeof(InternalFPF));
        my_memmove((void *)&locy,(void *)y,sizeof(InternalFPF));

        /* compute sum/difference */
        exponent_difference = locx.exp-locy.exp;
        if (exponent_difference == 0)
        {
                /*
                ** locx.exp == locy.exp
                ** so, no shifting required
                */
                if (locx.type == IFPF_IS_SUBNORMAL ||
                  locy.type == IFPF_IS_SUBNORMAL)
                        z->type = IFPF_IS_SUBNORMAL;
                else
                        z->type = IFPF_IS_NORMAL;

                /*
                ** Assume that locx.mantissa > locy.mantissa
                */
                z->sign = locx.sign;
                z->exp= locx.exp;
        }
        else
                if (exponent_difference > 0)
                {
                        /*
                        ** locx.exp > locy.exp
                        */
                        StickyShiftRightMant(&locy,
                                 exponent_difference);
                        z->type = locx.type;
                        z->sign = locx.sign;
                        z->exp = locx.exp;
                }
                else    /* if (exponent_difference < 0) */
                {
                        /*
                        ** locx.exp < locy.exp
                        */
                        StickyShiftRightMant(&locx,
                                -exponent_difference);
                        z->type = locy.type;
                        z->sign = locy.sign ^ operation;
                        z->exp = locy.exp;
                }

                if (locx.sign ^ locy.sign ^ operation)
                {
                        /*
                        ** Signs are different, subtract mantissas
                        */
                        borrow = 0;
                        for (i=(INTERNAL_FPF_PRECISION-1); i>=0; i--)
                                Sub16Bits(&borrow,
                                        &z->mantissa[i],
                                        locx.mantissa[i],
                                        locy.mantissa[i]);

                        if (borrow)
                        {
                                /* The y->mantissa was larger than the
                                ** x->mantissa leaving a negative
                                ** result.  Change the result back to
                                ** an unsigned number and flip the
                                ** sign flag.
                                */
                                z->sign = locy.sign ^ operation;
                                borrow = 0;
                                for (i=(INTERNAL_FPF_PRECISION-1); i>=0; i--)
                                {
                                        Sub16Bits(&borrow,
                                                &z->mantissa[i],
                                                0,
                                                z->mantissa[i]);
                                }
                        }
                        else
                        {
                                /* The assumption made above
                                ** (i.e. x->mantissa >= y->mantissa)
                                ** was correct.  Therefore, do nothing.
                                ** z->sign = x->sign;
                                */
                        }

                        if (IsMantissaZero(z->mantissa))
                        {
                                z->type = IFPF_IS_ZERO;
                                z->sign = 0; /* positive */
                        }
                        else
                                if (locx.type == IFPF_IS_NORMAL ||
                                         locy.type == IFPF_IS_NORMAL)
                                {
                                        normalize(z);
                                }
                }
                else
                {
                        /* signs are the same, add mantissas */
                        carry = 0;
                        for (i=(INTERNAL_FPF_PRECISION-1); i>=0; i--)
                        {
                                Add16Bits(&carry,
                                        &z->mantissa[i],
                                        locx.mantissa[i],
                                        locy.mantissa[i]);
                        }

                        if (carry)
                        {
                                z->exp++;
                                carry=0;
                                ShiftMantRight1(&carry,z->mantissa);
                                z->mantissa[0] |= 0x8000;
                                z->type = IFPF_IS_NORMAL;
                        }
                        else
                                if (z->mantissa[0] & 0x8000)
                                        z->type = IFPF_IS_NORMAL;
        }
        break;

case INFINITY_INFINITY:
        SetInternalFPFNaN(z);
        break;

case NAN_NAN:
        choose_nan(x, y, z, 1);
        break;
}

/*
** All the math is done; time to round.
*/
RoundInternalFPF(z);
return;
}


/************************
** MultiplyInternalFPF **
*************************
** Two internal-representation numbers x and y are multiplied; the
** result is returned in z.
*/
static void MultiplyInternalFPF(InternalFPF *x,
                        InternalFPF *y,
                        InternalFPF *z)
{
int i;
int j;
u16 carry;
u16 extra_bits[INTERNAL_FPF_PRECISION];
InternalFPF locy;       /* Needed since this will be altered */
/*
** As in the preceding function, this large switch
** statement selects among the many combinations
** of operands.
*/
switch ((x->type * IFPF_TYPE_COUNT) + y->type)
{
case INFINITY_SUBNORMAL:
case INFINITY_NORMAL:
case INFINITY_INFINITY:
case ZERO_ZERO:
case ZERO_SUBNORMAL:
case ZERO_NORMAL:
        my_memmove((void *)x,(void *)z,sizeof(InternalFPF));
        z->sign ^= y->sign;
        break;

case SUBNORMAL_INFINITY:
case NORMAL_INFINITY:
case SUBNORMAL_ZERO:
case NORMAL_ZERO:
        my_memmove((void *)y,(void *)z,sizeof(InternalFPF));
        z->sign ^= x->sign;
        break;

case ZERO_INFINITY:
case INFINITY_ZERO:
        SetInternalFPFNaN(z);
        break;

case NAN_ZERO:
case NAN_SUBNORMAL:
case NAN_NORMAL:
case NAN_INFINITY:
        my_memmove((void *)x,(void *)z,sizeof(InternalFPF));
        break;

case ZERO_NAN:
case SUBNORMAL_NAN:
case NORMAL_NAN:
case INFINITY_NAN:
        my_memmove((void *)y,(void *)z,sizeof(InternalFPF));
        break;


case SUBNORMAL_SUBNORMAL:
case SUBNORMAL_NORMAL:
case NORMAL_SUBNORMAL:
case NORMAL_NORMAL:
        /*
        ** Make a local copy of the y number, since we will be
        ** altering it in the process of multiplying.
        */
        my_memmove((void *)&locy,(void *)y,sizeof(InternalFPF));

        /*
        ** Check for unnormal zero arguments
        */
        if (IsMantissaZero(x->mantissa) || IsMantissaZero(y->mantissa))
                SetInternalFPFInfinity(z, 0);

        /*
        ** Initialize the result
        */
        if (x->type == IFPF_IS_SUBNORMAL ||
            y->type == IFPF_IS_SUBNORMAL)
                z->type = IFPF_IS_SUBNORMAL;
        else
                z->type = IFPF_IS_NORMAL;

        z->sign = x->sign ^ y->sign;
        z->exp = x->exp + y->exp ;
        for (i=0; i<INTERNAL_FPF_PRECISION; i++)
        {
                z->mantissa[i] = 0;
                extra_bits[i] = 0;
        }

        for (i=0; i<(INTERNAL_FPF_PRECISION*16); i++)
        {
                /*
                ** Get rightmost bit of the multiplier
                */
                carry = 0;
                ShiftMantRight1(&carry, locy.mantissa);
                if (carry)
                {
                        /*
                        ** Add the multiplicand to the product
                        */
                        carry = 0;
                        for (j=(INTERNAL_FPF_PRECISION-1); j>=0; j--)
                                Add16Bits(&carry,
                                        &z->mantissa[j],
                                        z->mantissa[j],
                                        x->mantissa[j]);
                }
                else
                {
                        carry = 0;
                }

                /*
                ** Shift the product right.  Overflow bits get
                ** shifted into extra_bits.  We'll use it later
                ** to help with the "sticky" bit.
                */
                ShiftMantRight1(&carry, z->mantissa);
                ShiftMantRight1(&carry, extra_bits);
        }

        /*
        ** Normalize
        ** Note that we use a "special" normalization routine
        ** because we need to use the extra bits. (These are
        ** bits that may have been shifted off the bottom that
        ** we want to reclaim...if we can.
        */
        while ((z->mantissa[0] & 0x8000) == 0)
        {
                carry = 0;
                ShiftMantLeft1(&carry, extra_bits);
                ShiftMantLeft1(&carry, z->mantissa);
                z->exp--;
        }

        /*
        ** Set the sticky bit if any bits set in extra bits.
        */
        if (IsMantissaZero(extra_bits))
        {
                z->mantissa[INTERNAL_FPF_PRECISION-1] |= 1;
        }
        break;

case NAN_NAN:
        choose_nan(x, y, z, 0);
        break;
}

/*
** All math done...do rounding.
*/
RoundInternalFPF(z);
return;
}


/**********************
** DivideInternalFPF **
***********************
** Divide internal FPF number x by y.  Return result in z.
*/
static void DivideInternalFPF(InternalFPF *x,
                        InternalFPF *y,
                        InternalFPF *z)
{
int i;
int j;
u16 carry;
u16 extra_bits[INTERNAL_FPF_PRECISION];
InternalFPF locx;       /* Local for x number */

/*
** As with preceding function, the following switch
** statement selects among the various possible
** operands.
*/
switch ((x->type * IFPF_TYPE_COUNT) + y->type)
{
case ZERO_ZERO:
case INFINITY_INFINITY:
        SetInternalFPFNaN(z);
        break;

case ZERO_SUBNORMAL:
case ZERO_NORMAL:
        if (IsMantissaZero(y->mantissa))
        {
                SetInternalFPFNaN(z);
                break;
        }

case ZERO_INFINITY:
case SUBNORMAL_INFINITY:
case NORMAL_INFINITY:
        SetInternalFPFZero(z, x->sign ^ y->sign);
        break;

case SUBNORMAL_ZERO:
case NORMAL_ZERO:
        if (IsMantissaZero(x->mantissa))
        {
                SetInternalFPFNaN(z);
                break;
        }

case INFINITY_ZERO:
case INFINITY_SUBNORMAL:
case INFINITY_NORMAL:
        SetInternalFPFInfinity(z, 0);
        z->sign = x->sign ^ y->sign;
        break;

case NAN_ZERO:
case NAN_SUBNORMAL:
case NAN_NORMAL:
case NAN_INFINITY:
        my_memmove((void *)x,(void *)z,sizeof(InternalFPF));
        break;

case ZERO_NAN:
case SUBNORMAL_NAN:
case NORMAL_NAN:
case INFINITY_NAN:
        my_memmove((void *)y,(void *)z,sizeof(InternalFPF));
        break;

case SUBNORMAL_SUBNORMAL:
case NORMAL_SUBNORMAL:
case SUBNORMAL_NORMAL:
case NORMAL_NORMAL:
        /*
        ** Make local copy of x number, since we'll be
        ** altering it in the process of dividing.
        */
        my_memmove((void *)&locx,(void *)x,sizeof(InternalFPF));

        /*
        ** Check for unnormal zero arguments
        */
        if (IsMantissaZero(locx.mantissa))
        {
                if (IsMantissaZero(y->mantissa))
                        SetInternalFPFNaN(z);
                else
                        SetInternalFPFZero(z, 0);
                break;
        }
        if (IsMantissaZero(y->mantissa))
        {
                SetInternalFPFInfinity(z, 0);
                break;
        }

        /*
        ** Initialize the result
        */
        z->type = x->type;
        z->sign = x->sign ^ y->sign;
        z->exp = x->exp - y->exp +
                        ((INTERNAL_FPF_PRECISION * 16 * 2));
        for (i=0; i<INTERNAL_FPF_PRECISION; i++)
        {
                z->mantissa[i] = 0;
                extra_bits[i] = 0;
        }

        while ((z->mantissa[0] & 0x8000) == 0)
        {
                carry = 0;
                ShiftMantLeft1(&carry, locx.mantissa);
                ShiftMantLeft1(&carry, extra_bits);

                /*
                ** Time to subtract yet?
                */
                if (carry == 0)
                        for (j=0; j<INTERNAL_FPF_PRECISION; j++)
                        {
                                if (y->mantissa[j] > extra_bits[j])
                                {
                                        carry = 0;
                                        goto no_subtract;
                                }
                                if (y->mantissa[j] < extra_bits[j])
                                        break;
                        }
                /*
                ** Divisor (y) <= dividend (x), subtract
                */
                carry = 0;
                for (j=(INTERNAL_FPF_PRECISION-1); j>=0; j--)
                        Sub16Bits(&carry,
                                &extra_bits[j],
                                extra_bits[j],
                                y->mantissa[j]);
                carry = 1;      /* 1 shifted into quotient */
        no_subtract:
                ShiftMantLeft1(&carry, z->mantissa);
                z->exp--;
        }
        break;

case NAN_NAN:
        choose_nan(x, y, z, 0);
        break;
}

/*
** Math complete...do rounding
*/
RoundInternalFPF(z);
}

/**********************
** LongToInternalFPF **
** Int32ToInternalFPF **
***********************
** Convert a signed (long) 32-bit integer into an internal FPF number.
*/
/* static void LongToInternalFPF(long mylong, */
static void Int32ToInternalFPF(int32 mylong,
                InternalFPF *dest)
{
int i;          /* Index */
u16 myword;     /* Used to hold converted stuff */
/*
** Save the sign and get the absolute value.  This will help us
** with 64-bit machines, since we use only the lower 32
** bits just in case. (No longer necessary after we use int32.)
*/
/* if(mylong<0L) */
if(mylong<(int32)0)
{       dest->sign=1;
        mylong=(int32)0-mylong;
}
else
        dest->sign=0;
/*
** Prepare the destination floating point number
*/
dest->type=IFPF_IS_NORMAL;
for(i=0;i<INTERNAL_FPF_PRECISION;i++)
        dest->mantissa[i]=0;

/*
** See if we've got a zero.  If so, make the resultant FP
** number a true zero and go home.
*/
if(mylong==0)
{       dest->type=IFPF_IS_ZERO;
        dest->exp=0;
        return;
}

/*
** Not a true zero.  Set the exponent to 32 (internal FPFs have
** no bias) and load the low and high words into their proper
** locations in the mantissa.  Then normalize.  The action of
** normalizing slides the mantissa bits into place and sets
** up the exponent properly.
*/
dest->exp=32;
myword=(u16)((mylong >> 16) & 0xFFFFL);
dest->mantissa[0]=myword;
myword=(u16)(mylong & 0xFFFFL);
dest->mantissa[1]=myword;
normalize(dest);
return;
}

#if 1
/************************
** InternalFPFToString **
*************************
** FOR DEBUG PURPOSES
** This routine converts an internal floating point representation
** number to a string.  Used in debugging the package.
** Returns length of converted number.
** NOTE: dest must point to a buffer big enough to hold the
**  result.  Also, this routine does append a null (an effect
**  of using the sprintf() function).  It also returns
**  a length count.
** NOTE: This routine returns 5 significant digits.  Thats
**  about all I feel safe with, given the method of
**  conversion.  It should be more than enough for programmers
**  to determine whether the package is properly ported.
*/
static int InternalFPFToString(char *dest,
                InternalFPF *src)
{
InternalFPF locFPFNum;          /* Local for src (will be altered) */
InternalFPF IFPF10;             /* Floating-point 10 */
InternalFPF IFPFComp;           /* For doing comparisons */
int msign;                      /* Holding for mantissa sign */
int expcount;                   /* Exponent counter */
int ccount;                     /* Character counter */
int i,j,k;                      /* Index */
u16 carryaccum;                 /* Carry accumulator */
u16 mycarry;                    /* Local for carry */

/*
** Check first for the simple things...Nan, Infinity, Zero.
** If found, copy the proper string in and go home.
*/
switch(src->type)
{
        case IFPF_IS_NAN:
                my_memcpy(dest,"NaN",3);
                return(3);

        case IFPF_IS_INFINITY:
                if(src->sign==0)
                        my_memcpy(dest,"+Inf",4);
                else
                        my_memcpy(dest,"-Inf",4);
                return(4);

        case IFPF_IS_ZERO:
                if(src->sign==0)
                        my_memcpy(dest,"+0",2);
                else
                        my_memcpy(dest,"-0",2);
                return(2);
}

/*
** Move the internal number into our local holding area, since
** we'll be altering it to print it out.
*/
my_memcpy((void *)&locFPFNum,(void *)src,sizeof(InternalFPF));

/*
** Set up a floating-point 10...which we'll use a lot in a minute.
*/
/* LongToInternalFPF(10L,&IFPF10); */
Int32ToInternalFPF((int32)10,&IFPF10);

/*
** Save the mantissa sign and make it positive.
*/
msign=src->sign;

/* src->sign=0 */ /* bug, fixed Nov. 13, 1997 */
(&locFPFNum)->sign=0;

expcount=0;             /* Init exponent counter */

/*
** See if the number is less than 10.  If so, multiply
** the number repeatedly by 10 until it's not.   For each
** multiplication, decrement a counter so we can keep track
** of the exponent.
*/

while(1)
{       AddSubInternalFPF(1,&locFPFNum,&IFPF10,&IFPFComp);
        if(IFPFComp.sign==0) break;
        MultiplyInternalFPF(&locFPFNum,&IFPF10,&IFPFComp);
        expcount--;
        my_memcpy((void *)&locFPFNum,(void *)&IFPFComp,sizeof(InternalFPF));
}
/*
** Do the reverse of the above.  As long as the number is
** greater than or equal to 10, divide it by 10.  Increment the
** exponent counter for each multiplication.
*/

while(1)
{
        AddSubInternalFPF(1,&locFPFNum,&IFPF10,&IFPFComp);
        if(IFPFComp.sign!=0) break;
        DivideInternalFPF(&locFPFNum,&IFPF10,&IFPFComp);
        expcount++;
        my_memcpy((void *)&locFPFNum,(void *)&IFPFComp,sizeof(InternalFPF));
}

/*
** About time to start storing things.  First, store the
** mantissa sign.
*/
ccount=1;               /* Init character counter */
if(msign==0)
        *dest++='+';
else
        *dest++='-';

/*
** At this point we know that the number is in the range
** 10 > n >=1.  We need to "strip digits" out of the
** mantissa.  We do this by treating the mantissa as
** an integer and multiplying by 10. (Not a floating-point
** 10, but an integer 10.  Since this is debug code and we
** could care less about speed, we'll do it the stupid
** way and simply add the number to itself 10 times.
** Anything that makes it to the left of the implied binary point
** gets stripped off and emitted.  We'll do this for
** 5 significant digits (which should be enough to
** verify things).
*/
/*
** Re-position radix point
*/
carryaccum=0;
while(locFPFNum.exp>0)
{
        mycarry=0;
        ShiftMantLeft1(&mycarry,locFPFNum.mantissa);
        carryaccum=(carryaccum<<1);
        if(mycarry) carryaccum++;
        locFPFNum.exp--;
}

while(locFPFNum.exp<0)
{
        mycarry=0;
        ShiftMantRight1(&mycarry,locFPFNum.mantissa);
        locFPFNum.exp++;
}

for(i=0;i<6;i++)
        if(i==1)
        {       /* Emit decimal point */
                *dest++='.';
                ccount++;
        }
        else
        {       /* Emit a digit */
                *dest++=('0'+carryaccum);
                ccount++;

                carryaccum=0;
                my_memcpy((void *)&IFPF10,
                        (void *)&locFPFNum,
                        sizeof(InternalFPF));

                /* Do multiply via repeated adds */
                for(j=0;j<9;j++)
                {
                        mycarry=0;
                        for(k=(INTERNAL_FPF_PRECISION-1);k>=0;k--)
                                Add16Bits(&mycarry,&(IFPFComp.mantissa[k]),
                                        locFPFNum.mantissa[k],
                                        IFPF10.mantissa[k]);
                        carryaccum+=mycarry ? 1 : 0;
                        my_memcpy((void *)&locFPFNum,
                                (void *)&IFPFComp,
                                sizeof(InternalFPF));
                }
        }

/*
** Now move the 'E', the exponent sign, and the exponent
** into the string.
*/
*dest++='E';

/* sprint is supposed to return an integer, but it caused problems on SunOS
 * with the native cc. Hence we force it.
 * Uwe F. Mayer
 */
if (expcount < 0) {
     *dest++ = '-';
     expcount =- expcount;
}
else *dest++ = ' ';

*dest++ = (char)(expcount + '0');
*dest++ = 0;

ccount += 3;
/*
** All done, go home.
*/
return(ccount);

}

#endif



////////////////////////////////////////////////////////////////////////
static 
void* AllocateMemory ( unsigned long n, int* p )
{
  *p = 0;
  void* r = (void*) (*serviceFn)(2,n);
  return r;
}
static 
void FreeMemory ( void* p, int* zz )
{
  *zz = 0;
  // free(p);
}



/**************
** DoEmFloat **
***************
** Perform the floating-point emulation routines portion of the
** CPU benchmark.  Returns the operations per second.
*/
static 
void DoEmFloat(void)
{
EmFloatStruct *locemfloatstruct;        /* Local structure */
InternalFPF *abase;             /* Base of A array */
InternalFPF *bbase;             /* Base of B array */
InternalFPF *cbase;             /* Base of C array */
ulong tickcount;                /* # of ticks */
char *errorcontext;             /* Error context string pointer */
int systemerror;                /* For holding error code */
ulong loops;                    /* # of loops */

/*
** Link to global structure
*/
EmFloatStruct global_emfloatstruct;
 global_emfloatstruct.adjust = 0;
 global_emfloatstruct.request_secs = 0;
 global_emfloatstruct.arraysize = 100;
 global_emfloatstruct.loops = 1;
 global_emfloatstruct.emflops = 0.0;
locemfloatstruct=&global_emfloatstruct;

/*
** Set the error context
*/
errorcontext="CPU:Floating Emulation";


abase=(InternalFPF *)AllocateMemory(locemfloatstruct->arraysize*sizeof(InternalFPF),
		&systemerror);

bbase=(InternalFPF *)AllocateMemory(locemfloatstruct->arraysize*sizeof(InternalFPF),
		&systemerror);

cbase=(InternalFPF *)AllocateMemory(locemfloatstruct->arraysize*sizeof(InternalFPF),
		&systemerror);

/*
** Set up the arrays
*/
SetupCPUEmFloatArrays(abase,bbase,cbase,locemfloatstruct->arraysize);

 loops=100;
	       tickcount=DoEmFloatIteration(abase,bbase,cbase,
			locemfloatstruct->arraysize,
			loops);

FreeMemory((void *)abase,&systemerror);
FreeMemory((void *)bbase,&systemerror);
FreeMemory((void *)cbase,&systemerror);

return;
}

//////////////////
void entry ( HWord(*f)(HWord,HWord) )
{
  serviceFn = f;
  vexxx_printf("starting\n");
  DoEmFloat();
  (*serviceFn)(0,0);
}
