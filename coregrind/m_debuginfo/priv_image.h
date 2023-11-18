
/*--------------------------------------------------------------------*/
/*--- An abstraction that provides a file-reading mechanism.       ---*/
/*---                                                 priv_image.h ---*/
/*--------------------------------------------------------------------*/

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

#ifndef __PRIV_IMAGE_H
#define __PRIV_IMAGE_H

#include "pub_core_basics.h"    // ULong
#include "priv_misc.h"          // ML_(dinfo_zalloc)

/*------------------------------------------------------------*/
/*--- DiImage -- abstract images                           ---*/
/*------------------------------------------------------------*/

/* The basic type, containing an abstractified memory area that can be
   read from.  This is an abstract type since there can be more than
   one implementation of it. */

/* abstract */
typedef  struct _DiImage  DiImage;

/* an offset in the image */
typedef  ULong  DiOffT;

/* This denotes an invalid DiOffT value.  Except where otherwise
   noted, you must never pass this to any of the ML_(image_*)
   functions -- they will assert.  That does mean though that they can
   be used for signalling other conditions, for example that some
   expected part of the image is missing. */
#define DiOffT_INVALID ((DiOffT)(0xFFFFFFFFFFFFFFFFULL))

/* Create an image from a file in the local filesysem.  Returns NULL
   if it fails, for whatever reason. */
DiImage* ML_(img_from_local_file)(const HChar* fullpath);

DiImage* ML_(img_from_fd)(Int fd, const HChar* fullpath);

/* Create an image by connecting to a Valgrind debuginfo server
   (auxprogs/valgrind-di-server.c).  |filename| contains the object
   name to ask for; it must be a plain filename, not absolute, not a
   path.  |serverAddr| must be of the form either "d.d.d.d" or
   "d.d.d.d:d" where d is one or more digits.  These specify the IPv4
   address and (in the second case) port number for the server.  In
   the first case, port 1500 is assumed. */
DiImage* ML_(img_from_di_server)(const HChar* filename,
                                 const HChar* serverAddr);

/* Free memory allocated for image. */
void ML_(img_free)(DiImage*);

/* Destroy an existing image. */
void ML_(img_done)(DiImage*);

/* Virtual size of the image. */
DiOffT ML_(img_size)(const DiImage* img);

/* Real size of the image. */
DiOffT ML_(img_real_size)(const DiImage* img);

/* Does the section [offset, +size) exist in the image? */
Bool ML_(img_valid)(const DiImage* img, DiOffT offset, SizeT size);

/* Get info out of an image.  If any part of the section denoted by
   [offset, +size) is invalid, does not return. */
void ML_(img_get)(/*OUT*/void* dst,
                  DiImage* img, DiOffT offset, SizeT size);

/* A version of ML_(img_get) that is significantly cheaper when
   fetching a lot of data, at the cost of being more difficult to use.
   Fetches between 1 and |size| bytes from |img| at |offset| and
   places them in |dst|.  |size| must be at least 1.  The number of
   bytes read is returned, and the caller must be able to deal with
   any number between 1 and |size|.  |offset| must be a valid offset
   in the image; if not the function will not return.  This function
   will not read off the end of the image. */
SizeT ML_(img_get_some)(/*OUT*/void* dst,
                        DiImage* img, DiOffT offset, SizeT size);

/* Copy a C string out of the image, into ML_(dinfo_zalloc)'d space.
   The caller owns the string and must free it with ML_(dinfo_free).
   |offset| may be DiOffT_INVALID, in which case this returns NULL. */
HChar* ML_(img_strdup)(DiImage* img, const HChar* cc, DiOffT offset);

/* Do strcmp on two C strings in the image.  Chars are cast to HChar
   before comparison. */
Int ML_(img_strcmp)(DiImage* img, DiOffT off1, DiOffT off2);

/* Do strcmp of a C string in the image vs a normal one.  Chars are
   cast to HChar before comparison. */
Int ML_(img_strcmp_c)(DiImage* img, DiOffT off1, const HChar* str2);

/* Do strncmp of a C string in the image vs a normal one.  Chars are
   cast to HChar before comparison. */
Int ML_(img_strcmp_n)(DiImage* img, DiOffT off1, const HChar* str2, Word n);

/* Do strlen of a C string in the image. */
SizeT ML_(img_strlen)(DiImage* img, DiOffT off);

/* Fetch various sized primitive types from the image.  These operate
   at the endianness and word size of the host. */
UChar  ML_(img_get_UChar) (DiImage* img, DiOffT offset);
UShort ML_(img_get_UShort)(DiImage* img, DiOffT offset);
UInt   ML_(img_get_UInt)  (DiImage* img, DiOffT offset);
ULong  ML_(img_get_ULong) (DiImage* img, DiOffT offset);

/* Calculate the "GNU Debuglink CRC" for the image.  This
   unfortunately has to be done as part of the DiImage implementation
   because it involves reading the entire image, and is therefore
   something that needs to be handed off to the remote server -- since
   to do it otherwise would imply pulling the entire image across the
   connection, making the client/server split pointless. */
UInt ML_(img_calc_gnu_debuglink_crc32)(DiImage* img);

/* Mark compressed part of image defined with (offset, szC).
   szD is length of uncompressed data (should be known before decompression).
   Returns (virtual) position in image from which decompressed data can be
   read. */
DiOffT ML_(img_mark_compressed_part)(DiImage* img, DiOffT offset, SizeT szC,
                                     SizeT szD);


/*------------------------------------------------------------*/
/*--- DiCursor -- cursors for reading images               ---*/
/*------------------------------------------------------------*/

/* A type built on DiImage.  It contains a DiImage and a 'current
   offset' in the image, and is useful for building low level readers
   of images.  In the functions section below, "read" means "read data
   at the cursor without moving it along", and "step" means "read data
   at the cursor and move it along by the size of the item read". */
typedef
   struct { DiImage* img; DiOffT ioff; }
   DiCursor;

/* An invalid cursor.  You can't use it for anything. */
#define DiCursor_INVALID ((DiCursor){NULL,DiOffT_INVALID})

static inline DiCursor mk_DiCursor ( DiImage* img, DiOffT ioff ) {
   return (DiCursor){img, ioff};
}

static inline Bool ML_(cur_is_valid)(DiCursor c) {
   return c.img != NULL;
}


/*------------------------------------------------------------*/
/*--- DiSlice -- subranges within DiImages                 ---*/
/*------------------------------------------------------------*/

/* Another type built on top of DiImage.  It denotes a subrange of an
   image and is useful for representing (eg) exactly the part of an
   image that is a specific ELF section. */
typedef
   struct { DiImage* img; DiOffT ioff; DiOffT szB; }
   DiSlice;

/* A DiSlice can also be INVALID, meaning it does not refer to any
   part of any image. */
#define DiSlice_INVALID ((DiSlice){NULL,DiOffT_INVALID,0})

static inline DiSlice mk_DiSlice ( DiImage* img, DiOffT ioff, DiOffT szB ) {
   return (DiSlice){img, ioff, szB};
}

static inline Bool ML_(sli_is_valid)( DiSlice sli ) {
   return sli.img != NULL;
}

/* Create a slice from a combination of a cursor and a length.  The
   maximum implied offset must not exceed the size of the underlying
   image; this is asserted for. */
static inline DiSlice ML_(sli_from_cur)( DiCursor cur, DiOffT size ) {
   if (ML_(cur_is_valid)(cur)) {
      vg_assert(size != DiOffT_INVALID);
      vg_assert(cur.ioff + size <= ML_(img_size)(cur.img));
      return mk_DiSlice(cur.img, cur.ioff, size);
   } else {
      return DiSlice_INVALID;
   }
}

/* Create a slice which exactly covers the given image. */
static inline DiSlice ML_(sli_from_img)( DiImage* img ) {
   if (img) {
      return mk_DiSlice(img, 0, ML_(img_size)(img));
   } else {
      return DiSlice_INVALID;
   }
}


/*------------------------------------------------------------*/
/*--- Functions that operate on DiCursors                  ---*/
/*------------------------------------------------------------*/

/* Create a cursor from a slice, referring to the first byte of the
   slice. */
static inline DiCursor ML_(cur_from_sli)( DiSlice sl ) {
   if (ML_(sli_is_valid)(sl)) {
      DiCursor c;
      c.img  = sl.img;
      c.ioff = sl.ioff;
      return c;
   } else {
      return DiCursor_INVALID;
   }
}

static inline Bool ML_(cur_cmpLT)( DiCursor c1, DiCursor c2 ) {
   vg_assert(c1.img == c2.img);
   return c1.ioff < c2.ioff;
}
static inline Bool ML_(cur_cmpEQ)( DiCursor c1, DiCursor c2 ) {
   vg_assert(c1.img == c2.img);
   return c1.ioff == c2.ioff;
}
static inline Bool ML_(cur_cmpGT)( DiCursor c1, DiCursor c2 ) {
   vg_assert(c1.img == c2.img);
   return c1.ioff > c2.ioff;
}

static inline DiCursor ML_(cur_plus)( DiCursor c, Long n ) {
   c.ioff += (DiOffT)n;
   return c;
}

/* Asserts that c1 and c2 refer to the same image.  Returns the difference
   in offsets (c1.ioff - c2.ioff). */
static inline Long ML_(cur_minus)( DiCursor c1, DiCursor c2 ) {
   vg_assert(c1.img == c2.img);
   return (Long)(c1.ioff) - (Long)(c2.ioff);
}

static inline SizeT ML_(cur_strlen)( DiCursor c ) {
   return ML_(img_strlen)( c.img, c.ioff );
}

// strdup from the given cursor.  Caller must ML_(dinfo_free) the
// resulting string.
static inline HChar* ML_(cur_read_strdup)( DiCursor c, const HChar* cc ) {
   vg_assert(c.ioff != DiOffT_INVALID);
   HChar* res = ML_(img_strdup)(c.img, cc, c.ioff);
   return res;
}
// strdup from the given cursor and advance it.  Caller must
// ML_(dinfo_free) the resulting string.
static inline HChar* ML_(cur_step_strdup)( DiCursor* c, const HChar* cc ) {
   vg_assert(c->ioff != DiOffT_INVALID);
   HChar* res = ML_(img_strdup)(c->img, cc, c->ioff);
   c->ioff += VG_(strlen)(res) + 1;
   return res;
}

// Fetch an arbitrary number of bytes from the cursor.
static inline void ML_(cur_read_get) ( /*OUT*/void* dst,
                                       DiCursor c, SizeT size) {
   ML_(img_get)(dst, c.img, c.ioff, size);
}

// Fetch an arbitrary number of bytes from the cursor, and advance it.
static inline void ML_(cur_step_get) ( /*OUT*/void* dst,
                                       DiCursor* c, SizeT size) {
   ML_(img_get)(dst, c->img, c->ioff, size);
   c->ioff += size;
}

// memdup from the given cursor.  Caller must ML_(dinfo_free) the
// resulting block.
static inline UChar* ML_(cur_read_memdup)( DiCursor c, SizeT size,
                                           const HChar* cc )
{
   UChar* dst = ML_(dinfo_zalloc)(cc, size);
   if (size > 0)
      ML_(cur_read_get)(dst, c, size);
   return dst;
}

static inline UChar ML_(cur_read_UChar) ( DiCursor c ) {
   UChar r = ML_(img_get_UChar)( c.img, c.ioff );
   return r;
}
static inline UChar ML_(cur_step_UChar)( DiCursor* c ) {
   UChar r = ML_(img_get_UChar)( c->img, c->ioff );
   c->ioff += sizeof(UChar);
   return r;
}

static inline UShort ML_(cur_read_UShort) ( DiCursor c ) {
   UShort r = ML_(img_get_UShort)( c.img, c.ioff );
   return r;
}
static inline UShort ML_(cur_step_UShort) ( DiCursor* c ) {
   UShort r = ML_(img_get_UShort)( c->img, c->ioff );
   c->ioff += sizeof(UShort);
   return r;
}
static inline Short ML_(cur_step_Short) ( DiCursor* c ) {
   return (Short)ML_(cur_step_UShort)( c );
}

static inline UInt ML_(cur_read_UInt) ( DiCursor c ) {
   UInt r = ML_(img_get_UInt)( c.img, c.ioff );
   return r;
}
static inline UInt ML_(cur_step_UInt) ( DiCursor* c ) {
   UInt r = ML_(img_get_UInt)( c->img, c->ioff );
   c->ioff += sizeof(UInt);
   return r;
}
static inline Int ML_(cur_step_Int) ( DiCursor* c ) {
   return (Int)ML_(cur_step_UInt)( c );
}

static inline ULong ML_(cur_read_ULong) ( DiCursor c ) {
   ULong r = ML_(img_get_ULong)( c.img, c.ioff );
   return r;
}
static inline ULong ML_(cur_step_ULong) ( DiCursor* c ) {
   ULong r = ML_(img_get_ULong)( c->img, c->ioff );
   c->ioff += sizeof(ULong);
   return r;
}
static inline Long ML_(cur_step_Long) ( DiCursor* c ) {
   return (Long)ML_(cur_step_ULong)( c );
}

static inline Addr ML_(cur_step_Addr) ( DiCursor* c ) {
   if (sizeof(Addr) == sizeof(UInt)) {
      return ML_(cur_step_UInt)(c);
   } else if  (sizeof(Addr) == sizeof(ULong)) {
      return ML_(cur_step_ULong)(c);
   } else {
      vg_assert(0);
   }
}

#endif /* ndef __PRIV_IMAGE_H */

/*--------------------------------------------------------------------*/
/*--- end                                             priv_image.h ---*/
/*--------------------------------------------------------------------*/
