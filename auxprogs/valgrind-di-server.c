
/*--------------------------------------------------------------------*/
/*--- A simple debuginfo server for Valgrind.                      ---*/
/*---                                         valgrind-di-server.c ---*/
/*--------------------------------------------------------------------*/

/* To build for an x86_64-linux host:
      gcc -g -Wall -O -o valgrind-di-server \
         auxprogs/valgrind-di-server.c -Icoregrind -Iinclude \
         -IVEX/pub -DVGO_linux -DVGA_amd64

   To build for an x86 (32-bit) host
      The same, except change -DVGA_amd64 to -DVGA_x86
*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2017 Mozilla Foundation

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Julian Seward <jseward@acm.org> */

/* This code works (just), but it's a mess.  Cleanups (also for
   coregrind/m_debuginfo/image.c):

   * Build this file for the host arch, not the target.  But how?
     Even Tromey had difficulty figuring out how to do that.

   * Change the use of pread w/ fd to FILE*, for the file we're
     serving.  Or, at least, put a loop around the pread uses
     so that it works correctly in the case where pread reads more
     than zero but less than we asked for.

   * CRC3 request/response: pass session-IDs back and forth and
     check them

   * Check that all error cases result in a FAIL frame being returned.

   * image.c: don't assert in cases where a FAIL frame is returned;
     instead cause the debuginfo reading to fail gracefully.  (Not
     sure how to do this)

   * Improve diagnostic printing

   * image.c: do we need to do VG_(write_socket) ?  Will it work
     just to use ordinary VG_(write) ?

   * Both files: document the reason for setting TCP_NODELAY

   * Add a command line argument saying where the served-from
     directory is -- changes clo_serverpath.

   * Fix up (common up) massive code duplication between client and
     server.

   * Tidy up the LZO source files; integrate properly in the build
     system.
*/

/*---------------------------------------------------------------*/

/* Include valgrind headers before system headers to avoid problems
   with the system headers #defining things which are used as names
   of structure members in vki headers. */

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"    // For VG_BUGS_TO
#include "pub_core_vki.h"           // Avoids warnings from 
                                    // pub_core_libcfile.h
#include "pub_core_libcfile.h"      // For VG_CLO_DEFAULT_LOGPORT

/* Needed to get a definition for pread() from unistd.h */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <stdlib.h>
#include <signal.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <netinet/tcp.h>

#include "../coregrind/m_debuginfo/minilzo.h"

/*---------------------------------------------------------------*/

/* The default allowable number of concurrent connections. */
#define  M_CONNECTIONS_DEFAULT 50
/* The maximum allowable number of concurrent connections. */
#define  M_CONNECTIONS_MAX     5000

/* The maximum allowable number of concurrent connections. */
unsigned M_CONNECTIONS = 0;

static const char* clo_serverpath = ".";


/*---------------------------------------------------------------*/

__attribute__ ((noreturn))
static void panic ( const char* str )
{
   fprintf(stderr,
           "\nvalgrind-di-server: the "
           "'impossible' happened:\n   %s\n", str);
   fprintf(stderr,
           "Please report this bug at: %s\n\n", VG_BUGS_TO);
   exit(1);
}

__attribute__ ((noreturn))
static void my_assert_fail ( const char* expr, const char* file, int line, const char* fn )
{
   fprintf(stderr,
           "\nvalgrind-di-server: %s:%d (%s): Assertion '%s' failed.\n",
           file, line, fn, expr );
   fprintf(stderr,
           "Please report this bug at: %s\n\n", VG_BUGS_TO);
   exit(1);
}

#undef assert

#define assert(expr)                                             \
  ((void) ((expr) ? 0 :					         \
	   (my_assert_fail (VG_STRINGIFY(expr),	                 \
                            __FILE__, __LINE__,                  \
                            __PRETTY_FUNCTION__), 0)))


/*---------------------------------------------------------------*/

/* Allocate some memory. Return iff successful. */
static void *my_malloc(size_t amount)
{
  void *p = malloc(amount ?: 1);

  if (p == NULL) {
     fprintf(stderr, "Memory allocation failed; cannot continue.\n");
     exit(1);
  }
  return p;
}

/*---------------------------------------------------------------*/

/* Holds the state that we need to track, for each connection. */
typedef
   struct {
      // is this entry in use?
      Bool in_use;
      // socket descriptor to communicate with client.  Initialised as
      // soon as this entry is created.
      int  conn_sd;
      // fd for the file that we are connected to.  Zero if not
      // currently connected to any file.
      int   file_fd;
      ULong file_size;
      // Session ID
      ULong session_id;
      // How many bytes and chunks sent?
      ULong stats_n_rdok_frames;
      ULong stats_n_read_unz_bytes; // bytes via READ (uncompressed)
      ULong stats_n_read_z_bytes;   // bytes via READ (compressed)
   }
   ConnState;

/* The state itself. */
static int       conn_count = 0;
static ConnState *conn_state;

/* Issues unique session ID values. */
static ULong next_session_id = 1;


/*---------------------------------------------------------------*/

// Code that is duplicated with the client :-(

/* The following Adler-32 checksum code is taken from zlib-1.2.3, which
   has the following copyright notice. */
/*
Copyright notice:

 (C) 1995-2004 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu

If you use the zlib library in a product, we would appreciate *not*
receiving lengthy legal documents to sign. The sources are provided
for free but without warranty of any kind.  The library has been
entirely written by Jean-loup Gailly and Mark Adler; it does not
include third-party code.

If you redistribute modified sources, we would appreciate that you include
in the file ChangeLog history information documenting your changes. Please
read the FAQ for more information on the distribution of modified source
versions.
*/

/* Update a running Adler-32 checksum with the bytes buf[0..len-1] and
   return the updated checksum. If buf is NULL, this function returns
   the required initial value for the checksum. An Adler-32 checksum is
   almost as reliable as a CRC32 but can be computed much faster. */
static
UInt adler32( UInt adler, const UChar* buf, UInt len )
{
#  define BASE 65521UL    /* largest prime smaller than 65536 */
#  define NMAX 5552
   /* NMAX is the largest n such that 
      255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */

#  define DO1(buf,i)  {adler += (buf)[i]; sum2 += adler;}
#  define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#  define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#  define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#  define DO16(buf)   DO8(buf,0); DO8(buf,8);

   /* The zlib sources recommend this definition of MOD if the
      processor cannot do integer division in hardware. */
#  define MOD(a) \
      do { \
          if (a >= (BASE << 16)) a -= (BASE << 16); \
          if (a >= (BASE << 15)) a -= (BASE << 15); \
          if (a >= (BASE << 14)) a -= (BASE << 14); \
          if (a >= (BASE << 13)) a -= (BASE << 13); \
          if (a >= (BASE << 12)) a -= (BASE << 12); \
          if (a >= (BASE << 11)) a -= (BASE << 11); \
          if (a >= (BASE << 10)) a -= (BASE << 10); \
          if (a >= (BASE << 9)) a -= (BASE << 9); \
          if (a >= (BASE << 8)) a -= (BASE << 8); \
          if (a >= (BASE << 7)) a -= (BASE << 7); \
          if (a >= (BASE << 6)) a -= (BASE << 6); \
          if (a >= (BASE << 5)) a -= (BASE << 5); \
          if (a >= (BASE << 4)) a -= (BASE << 4); \
          if (a >= (BASE << 3)) a -= (BASE << 3); \
          if (a >= (BASE << 2)) a -= (BASE << 2); \
          if (a >= (BASE << 1)) a -= (BASE << 1); \
          if (a >= BASE) a -= BASE; \
      } while (0)
#  define MOD4(a) \
      do { \
          if (a >= (BASE << 4)) a -= (BASE << 4); \
          if (a >= (BASE << 3)) a -= (BASE << 3); \
          if (a >= (BASE << 2)) a -= (BASE << 2); \
          if (a >= (BASE << 1)) a -= (BASE << 1); \
          if (a >= BASE) a -= BASE; \
      } while (0)

    UInt sum2;
    UInt n;

    /* split Adler-32 into component sums */
    sum2 = (adler >> 16) & 0xffff;
    adler &= 0xffff;

    /* in case user likes doing a byte at a time, keep it fast */
    if (len == 1) {
        adler += buf[0];
        if (adler >= BASE)
            adler -= BASE;
        sum2 += adler;
        if (sum2 >= BASE)
            sum2 -= BASE;
        return adler | (sum2 << 16);
    }

    /* initial Adler-32 value (deferred check for len == 1 speed) */
    if (buf == NULL)
        return 1L;

    /* in case short lengths are provided, keep it somewhat fast */
    if (len < 16) {
        while (len--) {
            adler += *buf++;
            sum2 += adler;
        }
        if (adler >= BASE)
            adler -= BASE;
        MOD4(sum2);             /* only added so many BASE's */
        return adler | (sum2 << 16);
    }

    /* do length NMAX blocks -- requires just one modulo operation */
    while (len >= NMAX) {
        len -= NMAX;
        n = NMAX / 16;          /* NMAX is divisible by 16 */
        do {
            DO16(buf);          /* 16 sums unrolled */
            buf += 16;
        } while (--n);
        MOD(adler);
        MOD(sum2);
    }

    /* do remaining bytes (less than NMAX, still just one modulo) */
    if (len) {                  /* avoid modulos if none remaining */
        while (len >= 16) {
            len -= 16;
            DO16(buf);
            buf += 16;
        }
        while (len--) {
            adler += *buf++;
            sum2 += adler;
        }
        MOD(adler);
        MOD(sum2);
    }

    /* return recombined sums */
    return adler | (sum2 << 16);

#  undef MOD4
#  undef MOD
#  undef DO16
#  undef DO8
#  undef DO4
#  undef DO2
#  undef DO1
#  undef NMAX
#  undef BASE
}


/* A frame.  The first 4 bytes of |data| give the kind of the frame,
   and the rest of it is kind-specific data. */
typedef  struct { UChar* data; SizeT n_data; }  Frame;


static void write_UInt_le ( /*OUT*/UChar* dst, UInt n )
{
   Int i;
   for (i = 0; i <= 3; i++) {
      dst[i] = (UChar)(n & 0xFF);
      n >>= 8;
   }
}

static UInt read_UInt_le ( UChar* src )
{
   UInt r = 0;
   Int i;
   for (i = 3; i >= 0; i--) {
      r <<= 8;
      r += (UInt)src[i];
   }
   return r;
}

static void write_ULong_le ( /*OUT*/UChar* dst, ULong n )
{
   Int i;
   for (i = 0; i <= 7; i++) {
      dst[i] = (UChar)(n & 0xFF);
      n >>= 8;
   }
}

static ULong read_ULong_le ( UChar* src )
{
   ULong r = 0;
   Int i;
   for (i = 7; i >= 0; i--) {
      r <<= 8;
      r += (ULong)src[i];
   }
   return r;
}

static Frame* mk_Frame_asciiz ( const char* tag, const char* str )
{
   assert(strlen(tag) == 4);
   Frame* f = calloc(sizeof(Frame), 1);
   size_t n_str = strlen(str);
   f->n_data = 4 + n_str + 1;
   f->data = calloc(f->n_data, 1);
   memcpy(&f->data[0], tag, 4);
   memcpy(&f->data[4], str, n_str);
   assert(f->data[4 + n_str] == 0);
   return f;
}

static Bool parse_Frame_noargs ( Frame* fr, const HChar* tag )
{
   assert(strlen(tag) == 4);
   if (!fr || !fr->data) return False;
   if (fr->n_data < 4) return False;
   if (memcmp(&fr->data[0], tag, 4) != 0) return False;
   if (fr->n_data != 4) return False;
   return True;
}

static Bool parse_Frame_asciiz ( Frame* fr, const HChar* tag,
                                 /*OUT*/UChar** str )
{
   assert(strlen(tag) == 4);
   if (!fr || !fr->data) return False;
   if (fr->n_data < 4) return False;
   if (memcmp(&fr->data[0], tag, 4) != 0) return False;
   if (fr->n_data < 5) return False; // else there isn't even enough
                                     // space for the terminating zero
   /* Find the terminating zero and ensure it's right at the end
      of the data.  If not, the frame is malformed. */
   SizeT i = 4;
   while (True) {
      if (i >= fr->n_data) break;
      if (fr->data[i] == 0) break;
      i++;
   }
   assert(i <= fr->n_data);
   if (i == fr->n_data-1 && fr->data[i] == 0) {
      *str = &fr->data[4];
      return True;
   } else {
      return False;
   }
}

static Frame* mk_Frame_le64 ( const HChar* tag, ULong n1 )
{
   assert(strlen(tag) == 4);
   Frame* f = calloc(sizeof(Frame), 1);
   f->n_data = 4 + 1*8;
   f->data = calloc(f->n_data, 1);
   memcpy(&f->data[0], tag, 4);
   write_ULong_le(&f->data[4 + 0*8], n1);
   return f;
}

static Frame* mk_Frame_le64_le64 ( const HChar* tag, ULong n1, ULong n2 )
{
   assert(strlen(tag) == 4);
   Frame* f = calloc(sizeof(Frame), 1);
   f->n_data = 4 + 2*8;
   f->data = calloc(f->n_data, 1);
   memcpy(&f->data[0], tag, 4);
   write_ULong_le(&f->data[4 + 0*8], n1);
   write_ULong_le(&f->data[4 + 1*8], n2);
   return f;
}

static Bool parse_Frame_le64_le64_le64 ( Frame* fr, const HChar* tag,
                                         /*OUT*/ULong* n1, /*OUT*/ULong* n2,
                                         /*OUT*/ULong* n3 )
{
   assert(strlen(tag) == 4);
   if (!fr || !fr->data) return False;
   if (fr->n_data < 4) return False;
   if (memcmp(&fr->data[0], tag, 4) != 0) return False;
   if (fr->n_data != 4 + 3*8) return False;
   *n1 = read_ULong_le(&fr->data[4 + 0*8]);
   *n2 = read_ULong_le(&fr->data[4 + 1*8]);
   *n3 = read_ULong_le(&fr->data[4 + 2*8]);
   return True;
}

static Frame* mk_Frame_le64_le64_le64_bytes ( 
                 const HChar* tag,
                 ULong n1, ULong n2, ULong n3, ULong n_data,
                 /*OUT*/UChar** data )
{
   assert(strlen(tag) == 4);
   Frame* f = calloc(sizeof(Frame), 1);
   f->n_data = 4 + 3*8 + n_data;
   f->data = calloc(f->n_data, 1);
   memcpy(&f->data[0], tag, 4);
   write_ULong_le(&f->data[4 + 0*8], n1);
   write_ULong_le(&f->data[4 + 1*8], n2);
   write_ULong_le(&f->data[4 + 2*8], n3);
   *data = &f->data[4 + 3*8];
   return f;
}

static void free_Frame ( Frame* fr )
{
   assert(fr && fr->data);
   free(fr->data);
   free(fr);
}


static void set_blocking ( int sd )
{
   int res;
   res = fcntl(sd, F_GETFL);
   res = fcntl(sd, F_SETFL, res & ~O_NONBLOCK);
   if (res != 0) {
      perror("fcntl failed");
      panic("set_blocking");
   }
}


#if 0
static void set_nonblocking ( int sd )
{
   int res;
   res = fcntl(sd, F_GETFL);
   res = fcntl(sd, F_SETFL, res | O_NONBLOCK);
   if (res != 0) {
      perror("fcntl failed");
      panic("set_nonblocking");
   }
}
#endif


/* Tries to read 'len' bytes from fd, blocking if necessary.  Assumes
   fd has been set in blocking mode.  If it returns with the number of
   bytes read < len, it means that either fd was closed, or there was
   an error on it. */
static SizeT my_read ( Int fd, UChar* buf, SizeT len )
{
  //set_blocking(fd);
   SizeT nRead = 0;
   while (1) {
      if (nRead == len) return nRead;
      assert(nRead < len);
      SizeT nNeeded = len - nRead;
      assert(nNeeded > 0);
      SSizeT n = read(fd, &buf[nRead], nNeeded);
      if (n <= 0) return nRead; /* error or EOF */
      nRead += n;
   }
}

/* Tries to write 'len' bytes to fd, blocking if necessary.  Assumes
   fd has been set in blocking mode.  If it returns with the number of
   bytes written < len, it means that either fd was closed, or there was
   an error on it. */
static SizeT my_write ( Int fd, UChar* buf, SizeT len )
{
  //set_nonblocking(fd);
   SizeT nWritten = 0;
   while (1) {
      if (nWritten == len) return nWritten;
      assert(nWritten < len);
      SizeT nStillToDo = len - nWritten;
      assert(nStillToDo > 0);
      SSizeT n = write(fd, &buf[nWritten], nStillToDo);
      if (n < 0) return nWritten; /* error or EOF */
      nWritten += n;
   }
}


static UInt calc_gnu_debuglink_crc32(/*OUT*/Bool* ok, int fd, ULong size)
{
  static const UInt crc32_table[256] =
    {
      0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419,
      0x706af48f, 0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4,
      0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07,
      0x90bf1d91, 0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
      0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856,
      0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
      0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4,
      0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
      0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3,
      0x45df5c75, 0xdcd60dcf, 0xabd13d59, 0x26d930ac, 0x51de003a,
      0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599,
      0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
      0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190,
      0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f,
      0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0x0f00f934, 0x9609a88e,
      0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
      0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed,
      0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
      0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3,
      0xfbd44c65, 0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
      0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a,
      0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5,
      0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa, 0xbe0b1010,
      0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
      0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17,
      0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6,
      0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615,
      0x73dc1683, 0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
      0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1, 0xf00f9344,
      0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
      0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a,
      0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
      0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1,
      0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c,
      0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef,
      0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
      0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe,
      0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31,
      0x2cd99e8b, 0x5bdeae1d, 0x9b64c2b0, 0xec63f226, 0x756aa39c,
      0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
      0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b,
      0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
      0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1,
      0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
      0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45, 0xa00ae278,
      0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7,
      0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66,
      0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
      0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605,
      0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8,
      0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b,
      0x2d02ef8d
    };

      /* Work through the image in 1 KB chunks. */
      UInt  crc      = 0xFFFFFFFF;
      ULong img_szB  = size;
      ULong curr_off = 0;
      while (1) {
         assert(curr_off <= img_szB);
         if (curr_off == img_szB) break;
         ULong avail = img_szB - curr_off;
         assert(avail > 0 && avail <= img_szB);
         if (avail > 65536) avail = 65536;
         UChar buf[65536];
         Int nRead = pread(fd, buf, avail, curr_off);
         if (nRead <= 0) { /* EOF or error on the file; neither should happen */
            *ok = False;
            return 0;
         }
         /* this is a kludge .. we should loop around pread and deal
            with short reads, for whatever reason */
         assert(nRead == avail);
         UInt i;
         for (i = 0; i < (UInt)nRead; i++)
            crc = crc32_table[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);
         curr_off += nRead;
      }
      *ok = True;
      return ~crc & 0xFFFFFFFF;
   }


/*---------------------------------------------------------------*/

/* Handle a transaction for conn_state[conn_no].  There is incoming
   data available; read it and send back an appropriate response.
   Returns a boolean indicating whether the connection has been
   closed; in which case this function does all necessary cleanup and
   leaves conn_state[conn_no] in a not-in-use state. */

static Bool handle_transaction ( int conn_no )
{
   Frame* req = NULL; /* the request frame that we receive */
   Frame* res = NULL; /* the response frame that we send back */

   assert(conn_no >= 0 && conn_no < M_CONNECTIONS);
   assert(conn_state[conn_no].in_use);

   //printf("SERVER: handle_transaction(%d)\n", conn_no); fflush(stdout);

   Int sd = conn_state[conn_no].conn_sd;

   /* Get a frame out of the channel. */
   UChar rd_first8[8];  // adler32; length32
   { Int r = my_read(sd, &rd_first8[0], 8);
     if (r == 0) goto client_closed_conn;
     if (r != 8) goto fail;
   }
   UInt rd_adler = read_UInt_le(&rd_first8[0]);
   UInt rd_len   = read_UInt_le(&rd_first8[4]);
   /* Allocate a Frame to hold the result data, and read into it. */
   // Reject obviously-insane length fields.
   if (rd_len > 4*1024*1024) goto fail;
   assert(req == NULL);
   req = calloc(sizeof(Frame), 1);
   req->n_data = rd_len;
   req->data = calloc(rd_len, 1);
   if (rd_len > 0) {
      Int r = my_read(sd, req->data, req->n_data);
      if (r != rd_len) goto fail;
   }
//printf("SERVER: recv %c%c%c%c\n", req->data[0], req->data[1], req->data[2], req->data[3]); fflush(stdout);

   /* Compute the checksum for the received data, and check it. */
   UInt adler = adler32(0, NULL, 0); // initial value
   adler = adler32(adler, &rd_first8[4], 4);
   if (req->n_data > 0) 
      adler = adler32(adler, req->data, req->n_data);

   if (adler/*computed*/ != rd_adler/*expected*/) goto fail;

   /* Now we have a request frame.  Cook up a response. */
   assert(res == NULL);

   UChar* filename = NULL;
   ULong req_session_id = 0, req_offset = 0, req_len = 0;

   if (parse_Frame_noargs(req, "VERS")) {
      res = mk_Frame_asciiz("VEOK", "Valgrind Debuginfo Server, Version 1");
   }
   else
   if (parse_Frame_noargs(req, "CRC3")) {
      /* FIXME: add a session ID to this request, and check it */
      if (conn_state[conn_no].file_fd == 0) {
         res = mk_Frame_asciiz("CRC3", "FAIL: not connected to file");
      } else {
         Bool ok    = False;
         UInt crc32 = calc_gnu_debuglink_crc32(&ok, 
                                               conn_state[conn_no].file_fd,
                                               conn_state[conn_no].file_size);
         if (ok) {
            res = mk_Frame_le64("CROK", (ULong)crc32);
         } else {
            res = mk_Frame_asciiz("FAIL", "CRC3: I/O error reading file");
         }
      }
   }
   else
   if (parse_Frame_asciiz(req, "OPEN", &filename)) {
      Bool ok = True;
      int  fd = 0;
      if (conn_state[conn_no].file_fd != 0) {
         res = mk_Frame_asciiz("FAIL", "OPEN: already connected to file");
         ok = False;
      }
      if (ok) {
         assert(clo_serverpath);
         fd = open((char*)filename, O_RDONLY);
         if (fd == -1) {
            res = mk_Frame_asciiz("FAIL", "OPEN: cannot open file");
            printf("(%d) SessionID %llu: open failed for \"%s\"\n",
                   conn_count, conn_state[conn_no].session_id, filename );
            ok = False;
         } else {
            assert(fd > 2);
         }
      }
      if (ok) {
         struct stat stat_buf;
         int r = fstat(fd, &stat_buf);
         if (r != 0) {
            res = mk_Frame_asciiz("FAIL", "OPEN: cannot stat file");
            close(fd);
            ok = False;
         }
         if (ok && stat_buf.st_size == 0) {
            res = mk_Frame_asciiz("FAIL", "OPEN: file has zero size");
            close(fd);
            ok = False;
         }
         if (ok) {
            conn_state[conn_no].file_fd   = fd;
            conn_state[conn_no].file_size = stat_buf.st_size;
            assert(res == NULL);
            res = mk_Frame_le64_le64("OPOK", conn_state[conn_no].session_id,
                                             conn_state[conn_no].file_size);
            printf("(%d) SessionID %llu: open successful for \"%s\"\n",
                   conn_count, conn_state[conn_no].session_id, filename );
            fflush(stdout);
         }
      }
   }
   else
   if (parse_Frame_le64_le64_le64(req, "READ", &req_session_id,
                                  &req_offset, &req_len)) {
      /* Because each new connection is associated with its own socket
         descriptor and hence with a particular conn_no, the requested
         session-ID is redundant -- it must be the one associated with
         this slot.  But check anyway. */
      Bool ok = True;
      if (req_session_id != conn_state[conn_no].session_id) {
         res = mk_Frame_asciiz("FAIL", "READ: invalid session ID");
         ok = False;
      }
      /* Check we're connected to a file, and if so range-check the
         request. */
      if (ok && conn_state[conn_no].file_fd == 0) {
         res = mk_Frame_asciiz("FAIL", "READ: no associated file");
         ok = False;
      }
      if (ok && (req_len == 0 || req_len > 4*1024*1024)) {
         res = mk_Frame_asciiz("FAIL", "READ: invalid request size");
         ok = False;
      }
      if (ok && req_len + req_offset > conn_state[conn_no].file_size) {
         res = mk_Frame_asciiz("FAIL", "READ: request exceeds file size");
         ok = False;
      }
      /* Try to read the file. */
      if (ok) {
         /* First, allocate a temp buf and read from the file into it. */
         /* FIXME: what if pread reads short and we have to redo it? */
         UChar* unzBuf = my_malloc(req_len);
         size_t nRead = pread(conn_state[conn_no].file_fd,
                              unzBuf, req_len, req_offset);
         if (nRead != req_len) {
            free_Frame(res);
            res = mk_Frame_asciiz("FAIL", "READ: I/O error reading file");
            ok = False;
         }         
         if (ok) {
            // Now compress it with LZO.  LZO appears to recommend
            // the worst-case output size as (in_len + in_len / 16 + 67).
            // Be more conservative here.
#           define STACK_ALLOC(var,size) \
               lzo_align_t __LZO_MMODEL \
                  var [ ((size) \
                        + (sizeof(lzo_align_t) - 1)) / sizeof(lzo_align_t) ]
            STACK_ALLOC(wrkmem, LZO1X_1_MEM_COMPRESS);
#           undef STACK_ALLOC
            UInt zLenMax = req_len + req_len / 4 + 1024;
            UChar* zBuf = my_malloc(zLenMax);
            lzo_uint zLen = zLenMax;
            Int lzo_rc = lzo1x_1_compress(unzBuf, req_len,
                                          zBuf, &zLen, wrkmem); 
            if (lzo_rc == LZO_E_OK) {
              //printf("XXXXX req_len %u  zLen %u\n", (UInt)req_len, (UInt)zLen);
               assert(zLen <= zLenMax);
               /* Make a frame to put the results in.  Bytes 24 and
                  onwards need to be filled from the compressed data,
                  and 'buf' is set to point to the right bit. */
               UChar* buf = NULL;
               res = mk_Frame_le64_le64_le64_bytes
                 ("RDOK", req_session_id, req_offset, req_len, zLen, &buf);
               assert(res);
               assert(buf);
               memcpy(buf, zBuf, zLen);
               // Update stats
               conn_state[conn_no].stats_n_rdok_frames++;
               conn_state[conn_no].stats_n_read_unz_bytes += req_len;
               conn_state[conn_no].stats_n_read_z_bytes   += zLen;
            } else {
               ok = False;
               free_Frame(res);
               res = mk_Frame_asciiz("FAIL", "READ: LZO failed");
            }
            free(zBuf);
         }
         free(unzBuf);
      }
   }
   else {
      res = mk_Frame_asciiz("FAIL", "Invalid request frame type");
   }

   /* All paths through the above should result in an assignment to |res|. */
   assert(res != NULL);

   /* And send the response frame back to the client. */
   /* What goes on the wire is:
         adler(le32) n_data(le32) data[0 .. n_data-1]
      where the checksum covers n_data as well as data[].
   */
   /* The initial Adler-32 value */
   adler = adler32(0, NULL, 0);

   /* Fold in the length field, encoded as le32. */
   UChar wr_first8[8];
   write_UInt_le(&wr_first8[4], res->n_data);
   adler = adler32(adler, &wr_first8[4], 4);
   /* Fold in the data values */
   adler = adler32(adler, res->data, res->n_data);
   write_UInt_le(&wr_first8[0], adler);

   Int r = my_write(sd, &wr_first8[0], 8);
   if (r != 8) goto fail;
   assert(res->n_data >= 4); // else ill formed -- no KIND field
   r = my_write(sd, res->data, res->n_data);
   if (r != res->n_data) goto fail;

//printf("SERVER: send %c%c%c%c\n", res->data[0], res->data[1], res->data[2], res->data[3]); fflush(stdout);

   /* So, success. */
   free_Frame(req);
   free_Frame(res);
   return False;  /* "connection still in use" */

   // Is there any difference between these?
  client_closed_conn:
  fail:
   if (conn_state[conn_no].conn_sd > 0)
      close(conn_state[conn_no].conn_sd);
   if (conn_state[conn_no].file_fd > 0)
      close(conn_state[conn_no].file_fd);

   if (conn_state[conn_no].stats_n_rdok_frames > 0) {
      printf("(%d) SessionID %llu:   sent %llu frames, "
             "%llu MB (unz), %llu MB (z), ratio %4.2f:1\n",
             conn_count, conn_state[conn_no].session_id,
             conn_state[conn_no].stats_n_rdok_frames,
             conn_state[conn_no].stats_n_read_unz_bytes / 1000000,
             conn_state[conn_no].stats_n_read_z_bytes / 1000000,
             (double)conn_state[conn_no].stats_n_read_unz_bytes
               / (double)conn_state[conn_no].stats_n_read_z_bytes);
      printf("(%d) SessionID %llu: closed\n",
             conn_count, conn_state[conn_no].session_id);

      fflush(stdout);
   }

   memset(&conn_state[conn_no], 0, sizeof(conn_state[conn_no]));
   if (req) free_Frame(req);
   if (res) free_Frame(res);
   return True; /* "connection has been closed" */
}


/*---------------------------------------------------------------*/



#if 0
static void copyout ( char* buf, int nbuf )
{
   int i;
   for (i = 0; i < nbuf; i++) {
      if (buf[i] == '\n') {
         fprintf(stdout, "\n(%d) ", conn_count);
      } else {
         __attribute__((unused)) size_t ignored 
            = fwrite(&buf[i], 1, 1, stdout);
      }
   }
   fflush(stdout);
}

static int read_from_sd ( int sd )
{
   char buf[100];
   int n;

   set_blocking(sd);
   n = read(sd, buf, 99);
   if (n <= 0) return 0; /* closed */
   copyout(buf, n);

   set_nonblocking(sd);
   while (1) {
      n = read(sd, buf, 100);
      if (n <= 0) return 1; /* not closed */
      copyout(buf, n);
   }
}
#endif

static void snooze ( void )
{
   struct timespec req;
   req.tv_sec = 0;
   req.tv_nsec = 200 * 1000 * 1000;
   nanosleep(&req,NULL);
}


/* returns 0 if negative, or > BOUND or invalid characters were found */
static int atoi_with_bound ( const char* str, int bound )
{
   int n = 0;
   while (1) {
      if (*str == 0) 
         break;
      if (*str < '0' || *str > '9')
         return 0;
      n = 10*n + (int)(*str - '0');
      str++;
      if (n >= bound)
         return 0;
   }
   return n;
}


/* returns 0 if invalid, else port # */
static int atoi_portno ( const char* str )
{
   int n = atoi_with_bound(str, 65536);

   if (n < 1024)
      return 0;
   return n;
}


static void usage ( void )
{
   fprintf(stderr, 
      "\n"
      "usage is:\n"
      "\n"
      "   valgrind-di-server [--exit-at-zero|-e] [port-number]\n"
      "\n"
      "   where   --exit-at-zero or -e causes the listener to exit\n"
      "           when the number of connections falls back to zero\n"
      "           (the default is to keep listening forever)\n"
      "\n"
      "           --max-connect=INT can be used to increase the maximum\n"
      "           number of connected processes (default = %d).\n"
      "           INT must be positive and less than %d.\n"
      "\n"
      "           port-number is the default port on which to listen for\n"
      "           connections.  It must be between 1024 and 65535.\n"
      "           Current default is %d.\n"
      "\n"
      ,
      M_CONNECTIONS_DEFAULT, M_CONNECTIONS_MAX, VG_CLO_DEFAULT_LOGPORT
   );
   exit(1);
}


static void banner ( const char* str )
{
   time_t t;
   t = time(NULL);
   printf("valgrind-di-server %s at %s", str, ctime(&t));
   fflush(stdout);
}


static void exit_routine ( void )
{
   banner("exited");
   exit(0);
}


static void sigint_handler ( int signo )
{
   exit_routine();
}


int main (int argc, char** argv) 
{
   int    i, j, res, one;
   int    main_sd, new_sd;
   socklen_t client_len;
   struct sockaddr_in client_addr, server_addr;

   char /*bool*/ exit_when_zero = 0;
   int           port = VG_CLO_DEFAULT_LOGPORT;

   for (i = 1; i < argc; i++) {
      if (0==strcmp(argv[i], "--exit-at-zero")
          || 0==strcmp(argv[i], "-e")) {
         exit_when_zero = 1;
      }
      else if (0 == strncmp(argv[i], "--max-connect=", 14)) {
         M_CONNECTIONS = atoi_with_bound(strchr(argv[i], '=') + 1, 5000);
         if (M_CONNECTIONS <= 0 || M_CONNECTIONS > M_CONNECTIONS_MAX)
            usage();
      }
      else
      if (atoi_portno(argv[i]) > 0) {
         port = atoi_portno(argv[i]);
      }
      else
      usage();
   }

   if (M_CONNECTIONS == 0)   // nothing specified on command line
      M_CONNECTIONS = M_CONNECTIONS_DEFAULT;

   conn_state = my_malloc(M_CONNECTIONS * sizeof conn_state[0]);

   banner("started");
   signal(SIGINT, sigint_handler);

   conn_count = 0;
   memset(conn_state, 0, M_CONNECTIONS * sizeof conn_state[0]);

   /* create socket */
   main_sd = socket(AF_INET, SOCK_STREAM, 0);
   if (main_sd < 0) {
      perror("cannot open socket ");
      panic("main -- create socket");
   }

   /* allow address reuse to avoid "address already in use" errors */

   one = 1;
   if (setsockopt(main_sd, SOL_SOCKET, SO_REUSEADDR, 
		  &one, sizeof(one)) < 0) {
      perror("cannot enable address reuse ");
      panic("main -- enable address reuse");
   }

   /* bind server port */
   server_addr.sin_family      = AF_INET;
   server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   server_addr.sin_port        = htons(port);
  
   if (bind(main_sd, (struct sockaddr *) &server_addr, 
                     sizeof(server_addr) ) < 0) {
      perror("cannot bind port ");
      panic("main -- bind port");
   }

   res = listen(main_sd, M_CONNECTIONS);
   if (res != 0) {
      perror("listen failed ");
      panic("main -- listen");
   }

   Bool do_snooze = False;  
   while (1) {

      if (0 && do_snooze)
         snooze();

      /* Snooze after this iteration, unless something happened. */
      do_snooze = True;

      /* enquire, using poll, whether there is any activity available on
         the main socket descriptor.  If so, someone is trying to
         connect; get the fd and add it to our table thereof. */
      { struct pollfd ufd;
        while (1) {
           ufd.fd      = main_sd;
           ufd.events  = POLLIN;
           ufd.revents = 0;
           res = poll(&ufd, 1, 0/*ms*/ /* 0=return immediately. */);
           if (res == 0) break;

           /* ok, we have someone waiting to connect.  Get the sd. */
           client_len = sizeof(client_addr);
           new_sd = accept(main_sd, (struct sockaddr *)&client_addr, 
                                                       &client_len);
           if (new_sd < 0) {
              perror("cannot accept connection ");
              panic("main -- accept connection");
           }

           /* find a place to put it. */
	   assert(new_sd > 0);
           for (i = 0; i < M_CONNECTIONS; i++)
              if (!conn_state[i].in_use)
                 break;

           if (i >= M_CONNECTIONS) {
              fprintf(stderr, "\n\nMore than %d concurrent connections.\n"
                      "Restart the server giving --max-connect=INT on the\n"
                      "commandline to increase the limit.\n\n",
                      M_CONNECTIONS);
              exit(1);
           }

assert(one == 1);
int ret = setsockopt( new_sd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
assert(ret != -1);

           memset(&conn_state[i], 0, sizeof(conn_state[i]));
           conn_state[i].in_use     = True;
           conn_state[i].conn_sd    = new_sd;
           conn_state[i].file_fd    = 0; /* not known yet */
           conn_state[i].session_id = next_session_id++;
           set_blocking(new_sd);
           conn_count++;
           do_snooze = False;
        } /* while (1) */
      }

      /* We've processed all new connect requests.  Listen for changes
         to the current set of fds.  This requires gathering up all
         the known conn_sd values and doing poll() on them. */
      static struct pollfd *tmp_pollfd;
      if (tmp_pollfd == NULL)
         tmp_pollfd = my_malloc(M_CONNECTIONS * sizeof tmp_pollfd[0]);

      /* And a parallel array which maps entries in tmp_pollfd back to
         entries in conn_state. */
      static int *tmp_pollfd_to_conn_state;
      if (tmp_pollfd_to_conn_state == NULL)
         tmp_pollfd_to_conn_state =
            my_malloc(M_CONNECTIONS * sizeof tmp_pollfd_to_conn_state[0]);

      j = 0;
      for (i = 0; i < M_CONNECTIONS; i++) {
         if (!conn_state[i].in_use)
            continue;
         assert(conn_state[i].conn_sd > 2);
         tmp_pollfd[j].fd      = conn_state[i].conn_sd;
         tmp_pollfd[j].events  = POLLIN /* | POLLHUP | POLLNVAL */;
         tmp_pollfd[j].revents = 0;
         tmp_pollfd_to_conn_state[j] = i;
         j++;
      }

      res = poll(tmp_pollfd, j, 20/*ms*/ /* 0=return immediately. */ );
      if (res < 0) {
         perror("poll(main) failed");
         panic("poll(main) failed");
      }
    
      /* nothing happened. go round again. */
      if (res == 0) {
         continue;
      }

      /* inspect the fds. */
      for (i = 0; i < j; i++) {
 
         if (tmp_pollfd[i].revents & POLLIN) {
            /* We have some activity on tmp_pollfd[i].  We need to
               figure out which conn_state[] entry that corresponds
               to, which is what tmp_pollfd_to_conn_state is for. */
            Int  conn_no  = tmp_pollfd_to_conn_state[i];
            Bool finished = handle_transaction(conn_no);
            if (finished) {
               /* this connection has been closed or otherwise gone
                  bad; forget about it. */
               conn_count--;
               fflush(stdout);
               if (conn_count == 0 && exit_when_zero) {
                  if (0) printf("\n");
                  fflush(stdout);
                  exit_routine();
	       }
            } else {
               // maybe show stats
               if (conn_state[i].stats_n_rdok_frames > 0
                   && (conn_state[i].stats_n_rdok_frames % 1000) == 0) {
                  printf("(%d) SessionID %llu:   sent %llu frames, "
                         "%llu MB (unz), %llu MB (z)\n",
                         conn_count, conn_state[conn_no].session_id,
                         conn_state[conn_no].stats_n_rdok_frames,
                         conn_state[conn_no].stats_n_read_unz_bytes / 1000000,
                         conn_state[conn_no].stats_n_read_z_bytes / 1000000);
                  fflush(stdout);
               }
            }
         }

      } /* for (i = 0; i < j; i++) */
  
      do_snooze = False;

   } /* while (1) */

   /* NOTREACHED */
}

////////////////////////////////////////////////////
#include "../coregrind/m_debuginfo/minilzo-inl.c"

/*--------------------------------------------------------------------*/
/*--- end                                     valgrind-di-server.c ---*/
/*--------------------------------------------------------------------*/
