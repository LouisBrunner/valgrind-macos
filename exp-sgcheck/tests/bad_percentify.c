
/* This demonstrates a stack overrun bug that exp-ptrcheck found while
   running Valgrind itself (self hosting).  As at 12 Sept 08 this bug
   is still in Valgrind. */

#include <stdio.h>
#include <assert.h>
#include <stdarg.h>

typedef  unsigned long long int  ULong;
typedef    signed long long int  Long;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  signed char             Char;
typedef  char                    HChar;
typedef unsigned long            UWord;
typedef   signed long            Word;



typedef  unsigned char  Bool;
#define  True   ((Bool)1)
#define  False  ((Bool)0)

#define VG_(_str) VG_##_str


/* ---------------------------------------------------------------------
   vg_sprintf, copied from m_libcprint.c
   ------------------------------------------------------------------ */
UInt
VG_(debugLog_vprintf) ( 
   void(*send)(HChar,void*), 
   void* send_arg2,
   const HChar* format, 
   va_list vargs
                        );

/* ---------------------------------------------------------------------
   printf() and friends
   ------------------------------------------------------------------ */
typedef
   struct { Int fd; Bool is_socket; }
   OutputSink;


OutputSink VG_(log_output_sink) = {  2, False }; /* 2 = stderr */
 
/* Do the low-level send of a message to the logging sink. */
static
void send_bytes_to_logging_sink ( OutputSink* sink, HChar* msg, Int nbytes )
{
   fwrite(msg, 1, nbytes, stdout);
   fflush(stdout);
}


/* --------- printf --------- */

typedef 
   struct {
      HChar       buf[512];
      Int         buf_used;
      OutputSink* sink;
   } 
   printf_buf_t;

// Adds a single char to the buffer.  When the buffer gets sufficiently
// full, we write its contents to the logging sink.
static void add_to__printf_buf ( HChar c, void *p )
{
   printf_buf_t *b = (printf_buf_t *)p;
   
   if (b->buf_used > sizeof(b->buf) - 2 ) {
      send_bytes_to_logging_sink( b->sink, b->buf, b->buf_used );
      b->buf_used = 0;
   }
   b->buf[b->buf_used++] = c;
   b->buf[b->buf_used]   = 0;
   assert(b->buf_used < sizeof(b->buf));
}

__attribute__((noinline))
static UInt vprintf_to_buf ( printf_buf_t* b,
                             const HChar *format, va_list vargs )
{
   UInt ret = 0;
   if (b->sink->fd >= 0 || b->sink->fd == -2) {
      ret = VG_(debugLog_vprintf) 
               ( add_to__printf_buf, b, format, vargs );
   }
   return ret;
}

__attribute__((noinline))
static UInt vprintf_WRK ( OutputSink* sink,
                          const HChar *format, va_list vargs )
{
   printf_buf_t myprintf_buf
      = { "", 0, sink };
   UInt ret;
   ret = vprintf_to_buf(&myprintf_buf, format, vargs);
   // Write out any chars left in the buffer.
   if (myprintf_buf.buf_used > 0) {
      send_bytes_to_logging_sink( myprintf_buf.sink,
                                  myprintf_buf.buf,
                                  myprintf_buf.buf_used );
   }
   return ret;
}

__attribute__((noinline))
UInt VG_(vprintf) ( const HChar *format, va_list vargs )
{
   return vprintf_WRK( &VG_(log_output_sink), format, vargs );
}

__attribute__((noinline))
UInt VG_(printf) ( const HChar *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs, format);
   ret = VG_(vprintf)(format, vargs);
   va_end(vargs);
   return ret;
}

static Bool toBool ( Int x ) {
   Int r = (x == 0) ? False : True;
   return (Bool)r;
}

__attribute__((noinline))
static Int local_strlen ( const HChar* str )
{
   Int i = 0;
   while (str[i] != 0) i++;
   return i;
}

__attribute__((noinline))
static HChar local_toupper ( HChar c )
{
   if (c >= 'a' && c <= 'z')
      return c + ('A' - 'a'); 
   else
      return c;
}


/*------------------------------------------------------------*/
/*--- A simple, generic, vprintf implementation.           ---*/
/*------------------------------------------------------------*/

/* -----------------------------------------------
   Distantly derived from:

      vprintf replacement for Checker.
      Copyright 1993, 1994, 1995 Tristan Gingold
      Written September 1993 Tristan Gingold
      Tristan Gingold, 8 rue Parmentier, F-91120 PALAISEAU, FRANCE

   (Checker itself was GPL'd.)
   ----------------------------------------------- */

/* Some flags.  */
#define VG_MSG_SIGNED    1 /* The value is signed. */
#define VG_MSG_ZJUSTIFY  2 /* Must justify with '0'. */
#define VG_MSG_LJUSTIFY  4 /* Must justify on the left. */
#define VG_MSG_PAREN     8 /* Parenthesize if present (for %y) */
#define VG_MSG_COMMA    16 /* Add commas to numbers (for %d, %u) */
#define VG_MSG_ALTFORMAT 32 /* Convert the value to alternate format */

/* Copy a string into the buffer. */
static __attribute__((noinline))
UInt myvprintf_str ( void(*send)(HChar,void*),
                     void* send_arg2,
                     Int flags, 
                     Int width, 
                     HChar* str, 
                     Bool capitalise )
{
#  define MAYBE_TOUPPER(ch) (capitalise ? local_toupper(ch) : (ch))
   UInt ret = 0;
   Int i, extra;
   Int len = local_strlen(str);

   if (width == 0) {
      ret += len;
      for (i = 0; i < len; i++)
          send(MAYBE_TOUPPER(str[i]), send_arg2);
      return ret;
   }

   if (len > width) {
      ret += width;
      for (i = 0; i < width; i++)
         send(MAYBE_TOUPPER(str[i]), send_arg2);
      return ret;
   }

   extra = width - len;
   if (flags & VG_MSG_LJUSTIFY) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ', send_arg2);
   }
   ret += len;
   for (i = 0; i < len; i++)
      send(MAYBE_TOUPPER(str[i]), send_arg2);
   if (!(flags & VG_MSG_LJUSTIFY)) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ', send_arg2);
   }

#  undef MAYBE_TOUPPER
   return ret;
}


/* Copy a string into the buffer, escaping bad XML chars. */
static 
UInt myvprintf_str_XML_simplistic ( void(*send)(HChar,void*),
                                    void* send_arg2,
                                    HChar* str )
{
   UInt   ret = 0;
   Int    i;
   Int    len = local_strlen(str);
   HChar* alt;

   for (i = 0; i < len; i++) {
      switch (str[i]) {
         case '&': alt = "&amp;"; break;
         case '<': alt = "&lt;"; break;
         case '>': alt = "&gt;"; break;
         default:  alt = NULL;
      }

      if (alt) {
         while (*alt) {
            send(*alt, send_arg2);
            ret++;
            alt++;
         }
      } else {
         send(str[i], send_arg2);
         ret++;
      }
   }

   return ret;
}


/* Write P into the buffer according to these args:
 *  If SIGN is true, p is a signed.
 *  BASE is the base.
 *  If WITH_ZERO is true, '0' must be added.
 *  WIDTH is the width of the field.
 */
static 
UInt myvprintf_int64 ( void(*send)(HChar,void*), 
                       void* send_arg2,
                       Int flags, 
                       Int base, 
                       Int width, 
                       Bool capitalised,
                       ULong p )
{
   HChar  buf[40];
   Int    ind = 0;
   Int    i, nc = 0;
   Bool   neg = False;
   HChar* digits = capitalised ? "0123456789ABCDEF" : "0123456789abcdef";
   UInt   ret = 0;

   if (base < 2 || base > 16)
      return ret;
 
   if ((flags & VG_MSG_SIGNED) && (Long)p < 0) {
      p   = - (Long)p;
      neg = True;
   }

   if (p == 0)
      buf[ind++] = '0';
   else {
      while (p > 0) {
         if (flags & VG_MSG_COMMA && 10 == base &&
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
         /* assert(ind < 39); */
         if (ind > 39) {
            buf[39] = 0;
            break;
         }
         buf[ind] = (flags & VG_MSG_ZJUSTIFY) ? '0': ' ';
      }
   }

   /* Reverse copy to buffer.  */
   ret += ind;
   for (i = ind -1; i >= 0; i--) {
      send(buf[i], send_arg2);
   }
   if (width > 0 && (flags & VG_MSG_LJUSTIFY)) {
      for(; ind < width; ind++) {
         ret++;
         /* Never pad with zeroes on RHS -- changes the value! */
         send(' ', send_arg2);
      }
   }
   return ret;
}


/* A simple vprintf().  */
/* EXPORTED */
__attribute__((noinline))
UInt
VG_(debugLog_vprintf) ( 
   void(*send)(HChar,void*), 
   void* send_arg2,
   const HChar* format, 
   va_list vargs
)
{
   UInt ret = 0;
   Int  i;
   Int  flags;
   Int  width;
   Int  n_ls = 0;
   Bool is_long, caps;

   /* We assume that vargs has already been initialised by the 
      caller, using va_start, and that the caller will similarly
      clean up with va_end.
   */

   for (i = 0; format[i] != 0; i++) {
      if (format[i] != '%') {
         send(format[i], send_arg2);
         ret++;
         continue;
      }
      i++;
      /* A '%' has been found.  Ignore a trailing %. */
      if (format[i] == 0)
         break;
      if (format[i] == '%') {
         /* '%%' is replaced by '%'. */
         send('%', send_arg2);
         ret++;
         continue;
      }
      flags = 0;
      n_ls  = 0;
      width = 0; /* length of the field. */
      while (1) {
         switch (format[i]) {
         case '(':
            flags |= VG_MSG_PAREN;
            break;
         case ',':
         case '\'':
            /* If ',' or '\'' follows '%', commas will be inserted. */
            flags |= VG_MSG_COMMA;
            break;
         case '-':
            /* If '-' follows '%', justify on the left. */
            flags |= VG_MSG_LJUSTIFY;
            break;
         case '0':
            /* If '0' follows '%', pads will be inserted. */
            flags |= VG_MSG_ZJUSTIFY;
            break;
         case '#':
            /* If '#' follows '%', alternative format will be used. */
            flags |= VG_MSG_ALTFORMAT;
            break;
         default:
            goto parse_fieldwidth;
         }
         i++;
      }
     parse_fieldwidth:
      /* Compute the field length. */
      while (format[i] >= '0' && format[i] <= '9') {
         width *= 10;
         width += format[i++] - '0';
      }
      while (format[i] == 'l') {
         i++;
         n_ls++;
      }

      //   %d means print a 32-bit integer.
      //  %ld means print a word-size integer.
      // %lld means print a 64-bit integer.
      if      (0 == n_ls) { is_long = False; }
      else if (1 == n_ls) { is_long = ( sizeof(void*) == sizeof(Long) ); }
      else                { is_long = True; }

      switch (format[i]) {
         case 'o': /* %o */
            if (flags & VG_MSG_ALTFORMAT) {
               ret += 2;
               send('0',send_arg2);
            }
            if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 8, width, False,
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 8, width, False,
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'd': /* %d */
            flags |= VG_MSG_SIGNED;
            if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, Long)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, Int)));
            break;
         case 'u': /* %u */
            if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'p':
            if (format[i+1] == 'S') {
               i++;
               /* %pS, like %s but escaping chars for XML safety */
               /* Note: simplistic; ignores field width and flags */
               char *str = va_arg (vargs, char *);
               if (str == (char*) 0)
                  str = "(null)";
               ret += myvprintf_str_XML_simplistic(send, send_arg2, str);
            } else {
               /* %p */
               ret += 2;
               send('0',send_arg2);
               send('x',send_arg2);
               ret += myvprintf_int64(send, send_arg2, flags, 16, width, True,
                                      (ULong)((UWord)va_arg (vargs, void *)));
            }
            break;
         case 'x': /* %x */
         case 'X': /* %X */
            caps = toBool(format[i] == 'X');
            if (flags & VG_MSG_ALTFORMAT) {
               ret += 2;
               send('0',send_arg2);
               send('x',send_arg2);
            }
            if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 16, width, caps,
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 16, width, caps,
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'c': /* %c */
            ret++;
            send(va_arg (vargs, int), send_arg2);
            break;
         case 's': case 'S': { /* %s */
            char *str = va_arg (vargs, char *);
            if (str == (char*) 0) str = "(null)";
            ret += myvprintf_str(send, send_arg2, 
                                 flags, width, str, format[i]=='S');
            break;
         }

//         case 'y': { /* %y - print symbol */
//            Char buf[100];
//            Char *cp = buf;
//            Addr a = va_arg(vargs, Addr);
//
//            if (flags & VG_MSG_PAREN)
//               *cp++ = '(';
//            if (VG_(get_fnname_w_offset)(a, cp, sizeof(buf)-4)) {
//               if (flags & VG_MSG_PAREN) {
//                  cp += local_strlen(cp);
//                  *cp++ = ')';
//                  *cp = '\0';
//               }
//               ret += myvprintf_str(send, send_arg2, flags, width, buf, 0);
//            }
//            break;
//         }
         default:
            break;
      }
   }
   return ret;
}


static void add_to__sprintf_buf ( HChar c, void *p )
{
   HChar** b = p;
   *(*b)++ = c;
}

UInt VG_(vsprintf) ( HChar* buf, const HChar *format, va_list vargs )
{
   Int ret;
   HChar* sprintf_ptr = buf;

   ret = VG_(debugLog_vprintf) 
            ( add_to__sprintf_buf, &sprintf_ptr, format, vargs );
   add_to__sprintf_buf('\0', &sprintf_ptr);

   assert(local_strlen(buf) == ret);

   return ret;
}

UInt VG_(sprintf) ( HChar* buf, const HChar *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs,format);
   ret = VG_(vsprintf)(buf, format, vargs);
   va_end(vargs);
   return ret;
}



/* ---------------------------------------------------------------------
   percentify()
   ------------------------------------------------------------------ */

/* This part excerpted from coregrind/m_libcbase.c */

// Percentify n/m with d decimal places.  Includes the '%' symbol at the end.
// Right justifies in 'buf'.
__attribute__((noinline))
void VG_percentify(ULong n, ULong m, UInt d, Int n_buf, HChar buf[]) 
{
   Int i, len, space;
   ULong p1;
   HChar fmt[32];

   if (m == 0) {
      // Have to generate the format string in order to be flexible about
      // the width of the field.
      VG_(sprintf)(fmt, "%%-%ds", n_buf);
      // fmt is now "%<n_buf>s" where <d> is 1,2,3...
      VG_(sprintf)(buf, fmt, "--%");
      return;
   }
   
   p1 = (100*n) / m;
    
   if (d == 0) {
      VG_(sprintf)(buf, "%lld%%", p1);
   } else {
      ULong p2;
      UInt  ex;
      switch (d) {
      case 1: ex = 10;    break;
      case 2: ex = 100;   break;
      case 3: ex = 1000;  break;
      default: assert(0);
      /* was: VG_(tool_panic)("Currently can only handle 3 decimal places"); */
      }
      p2 = ((100*n*ex) / m) % ex;
      // Have to generate the format string in order to be flexible about
      // the width of the post-decimal-point part.
      VG_(sprintf)(fmt, "%%lld.%%0%dlld%%%%", d);
      // fmt is now "%lld.%0<d>lld%%" where <d> is 1,2,3...
      VG_(sprintf)(buf, fmt, p1, p2);
   }

   len = local_strlen(buf);
   space = n_buf - len;
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}


/*------------------------------------------------------------*/
/*--- Stats                                                ---*/
/*------------------------------------------------------------*/

/* This part excerpted from coregrind/m_translate.c */

static UInt n_SP_updates_fast            = 0;
static UInt n_SP_updates_generic_known   = 0;
static UInt n_SP_updates_generic_unknown = 0;

__attribute__((noinline))
void VG_print_translation_stats ( void )
{
   HChar buf[6];
   UInt n_SP_updates = n_SP_updates_fast + n_SP_updates_generic_known
                                         + n_SP_updates_generic_unknown;
   VG_percentify(n_SP_updates_fast, n_SP_updates, 1, 6, buf);
   VG_(printf)(
      "translate:            fast SP updates identified: %'u (%s)\n",
      n_SP_updates_fast, buf );

   VG_percentify(n_SP_updates_generic_known, n_SP_updates, 1, 6, buf);
   VG_(printf)(
      "translate:   generic_known SP updates identified: %'u (%s)\n",
      n_SP_updates_generic_known, buf );

   VG_percentify(n_SP_updates_generic_unknown, n_SP_updates, 1, 6, buf);
   VG_(printf)(
      "translate: generic_unknown SP updates identified: %'u (%s)\n",
      n_SP_updates_generic_unknown, buf );
}



int main ( void )
{
  VG_print_translation_stats();
  return 0;
}
