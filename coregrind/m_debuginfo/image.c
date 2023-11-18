/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- An abstraction that provides a file-reading mechanism.       ---*/
/*---                                                      image.c ---*/
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

/* See the corresponding auxprogs/valgrind-di-server.c for a list of
   cleanups for this file and itself. */

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"     /* VG_(read_millisecond_timer) */
#include "pub_core_libcfile.h"
#include "priv_misc.h"             /* dinfo_zalloc/free/strdup */
#include "priv_image.h"            /* self */

#include "minilzo.h"
#define TINFL_HEADER_FILE_ONLY
#include "tinfl.c"

/* These values (1024 entries of 8192 bytes each) gives a cache
   size of 8MB. */
#define CACHE_ENTRY_SIZE_BITS (12+1)
#define CACHE_N_ENTRIES       1024

#define CACHE_ENTRY_SIZE      (1 << CACHE_ENTRY_SIZE_BITS)

#define COMPRESSED_SLICE_ARRAY_GROW_SIZE 64

/* An entry in the cache. */
typedef
   struct {
      Bool   fromC;  // True === contains decompressed data
      DiOffT off;    // file offset for data[0]
      SizeT  size;   // sizeof(data)
      SizeT  used;   // 1 .. sizeof(data), or 0 to denote not-in-use
      UChar  data[];
   }
   CEnt;

/* Compressed slice */
typedef
   struct {
      DiOffT offD;  // offset of decompressed data
      SizeT  szD;   // size of decompressed data
      DiOffT offC;  // offset of compressed data
      SizeT  szC;   // size of compressed data
   }
   CSlc;

/* Source for files */
typedef
   struct {
      // True: img is of local file.  False: img is from a server.
      Bool  is_local;
      // The fd for the local file, or sd for a remote server.
      Int   fd;
      // The name.  In ML_(dinfo_zalloc)'d space.  Used only for printing
      // error messages; hence it doesn't really matter what this contains.
      HChar* name;
      // The rest of these fields are only valid when using remote files
      // (that is, using a debuginfo server; hence when is_local==False)
      // Session ID allocated to us by the server.  Cannot be zero.
      ULong session_id;
   }
   Source;

struct _DiImage {
   // The source -- how to get hold of the file we are reading
   Source source;
   // Virtual size of the image = real size + size of uncompressed data
   SizeT size;
   // Real size of image
   SizeT real_size;
   // The number of entries used.  0 .. CACHE_N_ENTRIES
   UInt  ces_used;
   // Pointers to the entries.  ces[0 .. ces_used-1] are non-NULL.
   // ces[ces_used .. CACHE_N_ENTRIES-1] are NULL.
   // The non-NULL entries may be arranged arbitrarily.  We expect to use
   // a pseudo-LRU scheme though.
   CEnt* ces[CACHE_N_ENTRIES];

   // Array of compressed slices
   CSlc* cslc;
   // Number of compressed slices used
   UInt  cslc_used;
   // Size of cslc array
   UInt  cslc_size;
};


/* Sanity check code for CEnts. */
static void pp_CEnt(const HChar* msg, CEnt* ce)
{
   VG_(printf)("%s: fromC %s, used %llu, size %llu, offset %llu\n",
               msg, ce->fromC ? "True" : "False",
               (ULong)ce->used, (ULong)ce->size, (ULong)ce->off);
}

static Bool is_sane_CEnt ( const HChar* who, const DiImage* img, UInt i )
{
   vg_assert(img);
   vg_assert(i <= CACHE_N_ENTRIES);

   CEnt* ce = img->ces[i];
   if (!(ce->used <= ce->size)) goto fail;
   if (ce->fromC) {
      // ce->size can be anything, but ce->used must be either the
      // same or zero, in the case that it hasn't been set yet.  
      // Similarly, ce->off must either be above the real_size 
      // threshold, or zero if it hasn't been set yet.
      if (!(ce->off >= img->real_size || ce->off == 0)) goto fail;
      if (!(ce->off + ce->used <= img->size)) goto fail;
      if (!(ce->used == ce->size || ce->used == 0)) goto fail;
   } else {
      if (!(ce->size == CACHE_ENTRY_SIZE)) goto fail;
      if (!(ce->off + ce->used <= img->real_size)) goto fail;
   }
   return True;

 fail:
   VG_(printf)("is_sane_CEnt[%u]: fail: %s\n", i, who);
   pp_CEnt("failing CEnt", ce);
   return False;
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

static UInt read_UInt_le ( const UChar* src )
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

static ULong read_ULong_le ( const UChar* src )
{
   ULong r = 0;
   Int i;
   for (i = 7; i >= 0; i--) {
      r <<= 8;
      r += (ULong)src[i];
   }
   return r;
}


/* Set |sd| to be blocking.  Returns True on success. */
static Bool set_blocking ( int sd )
{
   Int res;
   res = VG_(fcntl)(sd, VKI_F_GETFL, 0/*ignored*/);
   if (res != -1)
      res = VG_(fcntl)(sd, VKI_F_SETFL, res & ~VKI_O_NONBLOCK);
   return (res != -1);
}

/* Tries to read 'len' bytes from fd, blocking if necessary.  Assumes
   fd has been set in blocking mode.  If it returns with the number of
   bytes read < len, it means that either fd was closed, or there was
   an error on it. */
static Int my_read ( Int fd, UChar* buf, Int len )
{
   Int nRead = 0;
   while (1) {
      if (nRead == len) return nRead;
      vg_assert(nRead < len);
      Int nNeeded = len - nRead;
      vg_assert(nNeeded > 0);
      Int n = VG_(read)(fd, &buf[nRead], nNeeded);
      if (n <= 0) return nRead; /* error or EOF */
      nRead += n;
   }
}

/* Tries to write 'len' bytes to fd, blocking if necessary.  Assumes
   fd has been set in blocking mode.  If it returns with the number of
   bytes written < len, it means that either fd was closed, or there was
   an error on it. */
static Int my_write ( Int fd, const UChar* buf, Int len )
{
   Int nWritten = 0;
   while (1) {
      if (nWritten == len) return nWritten;
      vg_assert(nWritten < len);
      Int nStillToDo = len - nWritten;
      vg_assert(nStillToDo > 0);
      Int n = VG_(write_socket)(fd, &buf[nWritten], nStillToDo);
      if (n < 0) return nWritten; /* error or EOF */
      nWritten += n;
   }
}

/* If we lost communication with the remote server, just give up.
   Recovering is too difficult. */
static void give_up__comms_lost(void)
{
   VG_(umsg)("\n");
   VG_(umsg)(
      "Valgrind: debuginfo reader: Lost communication with the remote\n");
   VG_(umsg)(
      "Valgrind: debuginfo server.  I can't recover.  Giving up.  Sorry.\n");
   VG_(umsg)("\n");
   VG_(exit)(1);
   /*NOTREACHED*/
}

static void give_up__image_overrun(void)
{
   VG_(umsg)("\n");
   VG_(umsg)(
      "Valgrind: debuginfo reader: Possibly corrupted debuginfo file.\n");
   VG_(umsg)(
      "Valgrind: I can't recover.  Giving up.  Sorry.\n");
   VG_(umsg)("\n");
   VG_(exit)(1);
   /*NOTREACHED*/
}

/* "Do" a transaction: that is, send the given frame to the server and
   return the frame it sends back.  Caller owns the resulting frame
   and must free it.  A NULL return means the transaction failed for
   some reason. */
static Frame* do_transaction ( Int sd, const Frame* req )
{
   if (0) VG_(printf)("CLIENT: send %c%c%c%c\n",
                      req->data[0], req->data[1], req->data[2], req->data[3]);

   /* What goes on the wire is:
         adler(le32) n_data(le32) data[0 .. n_data-1]
      where the checksum covers n_data as well as data[].
   */
   /* The initial Adler-32 value */
   UInt adler = VG_(adler32)(0, NULL, 0);

   /* Fold in the length field, encoded as le32. */
   UChar wr_first8[8];
   write_UInt_le(&wr_first8[4], req->n_data);
   adler = VG_(adler32)(adler, &wr_first8[4], 4);
   /* Fold in the data values */
   adler = VG_(adler32)(adler, req->data, req->n_data);
   write_UInt_le(&wr_first8[0], adler);

   Int r = my_write(sd, &wr_first8[0], 8);
   if (r != 8) return NULL;
   vg_assert(req->n_data >= 4); // else ill formed -- no KIND field
   r = my_write(sd, req->data, req->n_data);
   if (r != req->n_data) return NULL;

   /* So, the request is sent.  Now get a request of the same format
      out of the channel. */
   UChar rd_first8[8];  // adler32; length32
   r = my_read(sd, &rd_first8[0], 8);
   if (r != 8) return NULL;
   UInt rd_adler = read_UInt_le(&rd_first8[0]);
   UInt rd_len   = read_UInt_le(&rd_first8[4]);
   /* Allocate a Frame to hold the result data, and read into it. */
   // Reject obviously-insane length fields.
   if (rd_len < 4 || rd_len > 4*1024*1024) return NULL;
   Frame* res = ML_(dinfo_zalloc)("di.do_transaction.1", sizeof(Frame));
   res->n_data = rd_len;
   res->data = ML_(dinfo_zalloc)("di.do_transaction.2", rd_len);
   r = my_read(sd, res->data, res->n_data);
   if (r != rd_len) return NULL;

   if (0) VG_(printf)("CLIENT: recv %c%c%c%c\n",
                      res->data[0], res->data[1], res->data[2], res->data[3]);

   /* Compute the checksum for the received data, and check it. */
   adler = VG_(adler32)(0, NULL, 0); // initial value
   adler = VG_(adler32)(adler, &rd_first8[4], 4);
   if (res->n_data > 0)
      adler = VG_(adler32)(adler, res->data, res->n_data);

   if (adler/*computed*/ != rd_adler/*expected*/) return NULL;
   return res;
}

static void free_Frame ( Frame* fr )
{
   vg_assert(fr && fr->data);
   ML_(dinfo_free)(fr->data);
   ML_(dinfo_free)(fr);
}

static Frame* mk_Frame_noargs ( const HChar* tag )
{
   vg_assert(VG_(strlen)(tag) == 4);
   Frame* f = ML_(dinfo_zalloc)("di.mFn.1", sizeof(Frame));
   f->n_data = 4;
   f->data = ML_(dinfo_zalloc)("di.mFn.2", f->n_data);
   VG_(memcpy)(&f->data[0], tag, 4);
   return f;
}

static Frame* mk_Frame_le64_le64_le64 ( const HChar* tag,
                                        ULong n1, ULong n2, ULong n3 )
{
   vg_assert(VG_(strlen)(tag) == 4);
   Frame* f = ML_(dinfo_zalloc)("di.mFlll.1", sizeof(Frame));
   f->n_data = 4 + 3*8;
   f->data = ML_(dinfo_zalloc)("di.mFlll.2", f->n_data);
   VG_(memcpy)(&f->data[0], tag, 4);
   write_ULong_le(&f->data[4 + 0*8], n1);
   write_ULong_le(&f->data[4 + 1*8], n2);
   write_ULong_le(&f->data[4 + 2*8], n3);
   return f;
}

static Frame* mk_Frame_asciiz ( const HChar* tag, const HChar* str )
{
   vg_assert(VG_(strlen)(tag) == 4);
   Frame* f = ML_(dinfo_zalloc)("di.mFa.1", sizeof(Frame));
   SizeT n_str = VG_(strlen)(str);
   f->n_data = 4 + n_str + 1;
   f->data = ML_(dinfo_zalloc)("di.mFa.2", f->n_data);
   VG_(memcpy)(&f->data[0], tag, 4);
   VG_(memcpy)(&f->data[4], str, n_str);
   vg_assert(f->data[4 + n_str] == 0);
   return f;
}

static Bool parse_Frame_le64 ( const Frame* fr, const HChar* tag,
                               /*OUT*/ULong* n1 )
{
   vg_assert(VG_(strlen)(tag) == 4);
   if (!fr || !fr->data) return False;
   if (fr->n_data < 4) return False;
   if (VG_(memcmp)(&fr->data[0], tag, 4) != 0) return False;
   if (fr->n_data != 4 + 1*8) return False;
   *n1 = read_ULong_le(&fr->data[4 + 0*8]);
   return True;
}

static Bool parse_Frame_le64_le64 ( const Frame* fr, const HChar* tag,
                                    /*OUT*/ULong* n1, /*OUT*/ULong* n2 )
{
   vg_assert(VG_(strlen)(tag) == 4);
   if (!fr || !fr->data) return False;
   if (fr->n_data < 4) return False;
   if (VG_(memcmp)(&fr->data[0], tag, 4) != 0) return False;
   if (fr->n_data != 4 + 2*8) return False;
   *n1 = read_ULong_le(&fr->data[4 + 0*8]);
   *n2 = read_ULong_le(&fr->data[4 + 1*8]);
   return True;
}

static Bool parse_Frame_asciiz ( const Frame* fr, const HChar* tag,
                                 /*OUT*/UChar** str )
{
   vg_assert(VG_(strlen)(tag) == 4);
   if (!fr || !fr->data) return False;
   if (fr->n_data < 4) return False;
   if (VG_(memcmp)(&fr->data[0], tag, 4) != 0) return False;
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
   vg_assert(i <= fr->n_data);
   if (i == fr->n_data-1 && fr->data[i] == 0) {
      *str = &fr->data[4];
      return True;
   } else {
      return False;
   }
}

static Bool parse_Frame_le64_le64_le64_bytes (
               const Frame* fr, const HChar* tag,
               /*OUT*/ULong* n1, /*OUT*/ULong* n2, /*OUT*/ULong* n3,
               /*OUT*/UChar** data, /*OUT*/ULong* n_data 
            )
{
   vg_assert(VG_(strlen)(tag) == 4);
   if (!fr || !fr->data) return False;
   if (fr->n_data < 4) return False;
   if (VG_(memcmp)(&fr->data[0], tag, 4) != 0) return False;
   if (fr->n_data < 4 + 3*8) return False;
   *n1 = read_ULong_le(&fr->data[4 + 0*8]);
   *n2 = read_ULong_le(&fr->data[4 + 1*8]);
   *n3 = read_ULong_le(&fr->data[4 + 2*8]);
   *data   = &fr->data[4 + 3*8];
   *n_data = fr->n_data - (4 + 3*8);
   vg_assert(fr->n_data >= 4 + 3*8);
   return True;
}

static DiOffT block_round_down ( DiOffT i )
{
   return i & ((DiOffT)~(CACHE_ENTRY_SIZE-1));
}

/* Is this offset inside this CEnt? */
static inline Bool is_in_CEnt ( const CEnt* cent, DiOffT off )
{
   /* This assertion is checked by set_CEnt, so checking it here has
      no benefit, whereas skipping it does remove it from the hottest
      path. */
   /* vg_assert(cent->used > 0 && cent->used <= cent->size); */
   /* What we want to return is:
        cent->off <= off && off < cent->off + cent->used;
      This is however a very hot path, so here's alternative that uses
      only one conditional branch, using the following transformation,
      where all quantities are unsigned:
              x >= LO && x < LO+N
         -->  x-LO >= 0 && x-LO < LO+N-LO
         -->  x-LO >= 0 && x-LO < N
         -->  x-LO < N
      This is however only valid when the original bounds, that is, LO
      .. LO+N-1, do not wrap around the end of the address space.  That
      is, we require that LO <= LO+N-1.  But that's OK .. we don't
      expect wraparounds in CEnts or for that matter any object
      allocated from C-land.  See Hacker's Delight, Chapter 4.1,
      "Checking Bounds of Integers", for more details.
   */
   return off - cent->off < cent->used;
}

/* Returns pointer to CSlc or NULL */
static inline CSlc* find_cslc ( DiImage* img, DiOffT off )
{
   for (UInt i = 0; i < img->cslc_used; i++) {
      if ( (img->cslc[i].offD <= off)
           && (img->cslc[i].offD + img->cslc[i].szD > off)
         )
         return &img->cslc[i];
   }
   return NULL;
}

/* Allocate a new CEnt, connect it to |img|, and return its index. */
static UInt alloc_CEnt ( DiImage* img, SizeT szB, Bool fromC )
{
   vg_assert(img != NULL);
   vg_assert(img->ces_used < CACHE_N_ENTRIES);
   if (fromC) {
      // szB can be arbitrary
   } else {
      vg_assert(szB == CACHE_ENTRY_SIZE);
   }
   UInt entNo = img->ces_used;
   img->ces_used++;
   vg_assert(img->ces[entNo] == NULL);
   img->ces[entNo] = ML_(dinfo_zalloc)("di.alloc_CEnt.1",
                                       offsetof(CEnt, data) + szB);
   img->ces[entNo]->size = szB;
   img->ces[entNo]->fromC = fromC;
   vg_assert(is_sane_CEnt("alloc_CEnt", img, entNo));
   return entNo;
}

static void realloc_CEnt ( DiImage* img, UInt entNo, SizeT szB, Bool fromC )
{
   vg_assert(img != NULL);
   vg_assert(fromC || szB >= CACHE_ENTRY_SIZE);
   vg_assert(is_sane_CEnt("realloc_CEnt-pre", img, entNo));
   img->ces[entNo] = ML_(dinfo_realloc)("di.realloc_CEnt.1",
                                        img->ces[entNo],
                                        offsetof(CEnt, data) + szB);
}

/* Move the given entry to the top and slide those above it down by 1,
   to make space. */
static void move_CEnt_to_top ( DiImage* img, UInt entNo )
{
   vg_assert(entNo < img->ces_used);
   if (LIKELY(entNo == 1)) {
      CEnt* tmp = img->ces[1];
      img->ces[entNo] = img->ces[0];
      img->ces[0] = tmp;
   } else {
      vg_assert(entNo > 1); // a.k.a. >= 2
      CEnt* tmp = img->ces[entNo];
      img->ces[entNo] = img->ces[entNo-1];
      entNo--;
      img->ces[entNo] = img->ces[entNo-1];
      entNo--;
      while (entNo > 0) {
         img->ces[entNo] = img->ces[entNo-1];
         entNo--;
      }
      img->ces[0] = tmp;
   }
}

/* Set the given entry so that it has a chunk of the file containing
   the given offset.  It is this function that brings data into the
   cache, either by reading the local file or pulling it from the
   remote server. */
static void set_CEnt ( const DiImage* img, UInt entNo, DiOffT off )
{
   SizeT len;
   DiOffT off_orig = off;
   vg_assert(img != NULL);
   vg_assert(img->ces_used <= CACHE_N_ENTRIES);
   vg_assert(entNo < img->ces_used);
   vg_assert(off < img->real_size);
   CEnt* ce = img->ces[entNo];
   vg_assert(ce != NULL);
   /* Compute [off, +len) as the slice we are going to read. */
   off = block_round_down(off);
   len = img->real_size - off;
   if (len > ce->size)
      len = ce->size;
   /* It is conceivable that the 'len > 0' bit could fail if we make
      an image with a zero sized file.  But then no 'get' request on
      that image would be valid. */
   vg_assert(len > 0 && len <= ce->size);
   vg_assert(off + len <= img->real_size);
   vg_assert(off <= off_orig && off_orig < off+len);
   /* So, read  off .. off+len-1  into the entry. */

   if (0) {
      static UInt t_last = 0;
      static ULong nread = 0;
      UInt now = VG_(read_millisecond_timer)();
      UInt delay = now - t_last;
      t_last = now;
      nread += len;
      VG_(printf)("XXXXXXXX (tot %'llu)  read %'lu  offset %'llu  delay %'u\n", 
                  nread, len, off, delay);
   }

   if (img->source.is_local) {
      // Simple: just read it
      SysRes sr = VG_(pread)(img->source.fd, &ce->data[0], (Int)len, off);
      vg_assert(!sr_isError(sr));
   } else {
      // Not so simple: poke the server
      vg_assert(img->source.session_id > 0);
      Frame* req
         = mk_Frame_le64_le64_le64("READ", img->source.session_id, off, len);
      Frame* res = do_transaction(img->source.fd, req);
      free_Frame(req); req = NULL;
      if (!res) goto server_fail;
      ULong  rx_session_id = 0, rx_off = 0, rx_len = 0, rx_zdata_len = 0;
      UChar* rx_data = NULL;
      /* Pretty confusing.  rx_sessionid, rx_off and rx_len are copies
         of the values that we requested in the READ frame just above,
         so we can be sure that the server is responding to the right
         request.  It just copies them from the request into the
         response.  rx_data is the actual data, and rx_zdata_len is
         its compressed length.  Hence rx_len must equal len, but
         rx_zdata_len can be different -- smaller, hopefully.. */
      if (!parse_Frame_le64_le64_le64_bytes
          (res, "RDOK", &rx_session_id, &rx_off,
                        &rx_len, &rx_data, &rx_zdata_len))
         goto server_fail;
      if (rx_session_id != img->source.session_id
          || rx_off != off || rx_len != len || rx_data == NULL)
         goto server_fail;

      //VG_(memcpy)(&ce->data[0], rx_data, len);
      // Decompress into the destination buffer
      // Tell the lib the max number of output bytes it can write.
      // After the call, this holds the number of bytes actually written,
      // and it's an error if it is different.
      lzo_uint out_len = len;
      Int lzo_rc = lzo1x_decompress_safe(rx_data, rx_zdata_len,
                                         &ce->data[0], &out_len,
                                         NULL);
      Bool ok = lzo_rc == LZO_E_OK && out_len == len;
      if (!ok) goto server_fail;

      free_Frame(res); res = NULL;
      goto end_of_else_clause;
     server_fail:
      /* The server screwed up somehow.  Now what? */
      if (res) {
         UChar* reason = NULL;
         if (parse_Frame_asciiz(res, "FAIL", &reason)) {
            VG_(umsg)("set_CEnt (reading data from DI server): fail: "
                      "%s\n", reason);
         } else {
            VG_(umsg)("set_CEnt (reading data from DI server): fail: "
                      "unknown reason\n");
         }
         free_Frame(res); res = NULL;
      } else {
         VG_(umsg)("set_CEnt (reading data from DI server): fail: "
                   "server unexpectedly closed the connection\n");
      }
      give_up__comms_lost();
      /* NOTREACHED */
      vg_assert(0);
     end_of_else_clause:
      {}
   }
   
   ce->off  = off;
   ce->used = len;
   ce->fromC = False;
   vg_assert(ce == img->ces[entNo]);
   vg_assert(is_sane_CEnt("set_CEnt", img, entNo));
}

__attribute__((noinline))
static UChar get_slowcase ( DiImage* img, DiOffT off )
{
   /* Stay sane .. */
   vg_assert(off < img->size);
   vg_assert(img->ces_used <= CACHE_N_ENTRIES);
   UInt i;
   /* Start the search at entry 1, since the fast-case function
      checked slot zero already. */
   for (i = 1; i < img->ces_used; i++) {
      vg_assert(img->ces[i]);
      if (is_in_CEnt(img->ces[i], off))
         break;
   }
   vg_assert(i >= 1);

   if (LIKELY(i < img->ces_used)) {
      // Found it.  Move to the top and stop.
      move_CEnt_to_top(img, i);
      vg_assert(is_in_CEnt(img->ces[0], off));
      return img->ces[0]->data[ off - img->ces[0]->off ];
   }

   vg_assert(i <= img->ces_used);

   // It's not in any entry.  Either allocate a new one or recycle the LRU
   // one.  This is where the presence of compressed sections makes things
   // tricky.  There are 4 cases to consider:
   //
   // (1) not from a compressed slice, we can allocate a new entry
   // (2) not from a compressed slice, we have to recycle the LRU entry
   // (3) from a compressed slice, we can allocate a new entry
   // (4) from a compressed slice, we have to recycle the LRU entry
   //
   // Cases (3) and (4) are complex because we will have to call
   // ML_(img_get_some) to get the compressed data.  But this function is
   // reachable from ML_(img_get_some), so we may re-enter get_slowcase a
   // second time as a result.  Given that the compressed data will be cause
   // only cases (1) and (2) to happen, this guarantees no infinite recursion.
   // It does however mean that we can't carry (in this function invokation)
   // any local copies of the overall cache state across the ML_(img_get_some)
   // call, since it may become invalidated by the recursive call to
   // get_slowcase.

   // First of all, see if it is in a compressed slice, and if so, pull the
   // compressed data into an intermediate buffer.  Given the preceding
   // comment, this is a safe place to do it, since we are not carrying any
   // cache state here apart from the knowledge that the requested offset is
   // not in the cache at all, and the recursive call won't change that fact.

   CSlc* cslc = find_cslc(img, off);
   UChar* cbuf = NULL;
   if (cslc != NULL) {
      SizeT len = 0;
      cbuf = ML_(dinfo_zalloc)("di.image.get_slowcase.cbuf-1", cslc->szC);
      // get compressed data
      while (len < cslc->szC)
         len += ML_(img_get_some)(cbuf + len, img, cslc->offC + len,
                                  cslc->szC - len);
   }

   // Now we can do what we like.
   vg_assert((cslc == NULL && cbuf == NULL) || (cslc != NULL && cbuf != NULL));

   // Note, we can't capture this earlier, for exactly the reasons detailed
   // above.
   UInt ces_used_at_entry = img->ces_used;

   // This is the size of the CEnt that we want to have after allocation or
   // recycling.
   SizeT size = (cslc == NULL) ? CACHE_ENTRY_SIZE : cslc->szD;

   // Cases (1) and (3)
   if (img->ces_used < CACHE_N_ENTRIES) {
      /* Allocate a new cache entry, and fill it in. */
      i = alloc_CEnt(img, size, /*fromC?*/cslc != NULL);
      if (cslc == NULL) {
         set_CEnt(img, i, off);
         img->ces[i]->fromC = False;
         vg_assert(is_sane_CEnt("get_slowcase-case-1", img, i));
         vg_assert(img->ces_used == ces_used_at_entry + 1);
      } else {
         SizeT len = tinfl_decompress_mem_to_mem(
                        img->ces[i]->data, cslc->szD,
                        cbuf, cslc->szC,
                        TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF
                        | TINFL_FLAG_PARSE_ZLIB_HEADER);
         vg_assert(len == cslc->szD); // sanity check on data, FIXME
         vg_assert(cslc->szD == size);
         img->ces[i]->used = cslc->szD;
         img->ces[i]->off = cslc->offD;
         img->ces[i]->fromC = True;
         vg_assert(is_sane_CEnt("get_slowcase-case-3", img, i));
         vg_assert(img->ces_used == ces_used_at_entry + 1);
      }
      vg_assert(img->ces_used == ces_used_at_entry + 1);
      if (i > 0) {
         move_CEnt_to_top(img, i);
         i = 0;
      }
      vg_assert(is_in_CEnt(img->ces[i], off));
      if (cbuf != NULL) {
         ML_(dinfo_free)(cbuf);
      }
      return img->ces[i]->data[ off - img->ces[i]->off ];
   }

   // Cases (2) and (4)
   /* All entries in use.  Recycle the (ostensibly) LRU one.  But try to find
      a non-fromC entry to recycle, though, since discarding and reloading
      fromC entries is very expensive.  The result is that -- unless all
      CACHE_N_ENTRIES wind up being used by decompressed slices, which is
      highly unlikely -- we'll wind up keeping all the decompressed data in
      the cache for its entire remaining life.  We could probably do better
      but it would make the cache management even more complex. */
   vg_assert(img->ces_used == CACHE_N_ENTRIES);

   // Select entry to recycle.
   for (i = CACHE_N_ENTRIES-1; i > 0; i--) {
      if (!img->ces[i]->fromC)
         break;
   }
   vg_assert(i < CACHE_N_ENTRIES);

   realloc_CEnt(img, i, size, /*fromC?*/cslc != NULL);
   img->ces[i]->size = size;
   img->ces[i]->used = 0;
   if (cslc == NULL) {
      set_CEnt(img, i, off);
      img->ces[i]->fromC = False;
      vg_assert(is_sane_CEnt("get_slowcase-case-2", img, i));
   } else {
      SizeT len = tinfl_decompress_mem_to_mem(
                     img->ces[i]->data, cslc->szD,
                     cbuf, cslc->szC,
                     TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF
                     | TINFL_FLAG_PARSE_ZLIB_HEADER);
      vg_assert(len == size);
      img->ces[i]->used = size;
      img->ces[i]->off = cslc->offD;
      img->ces[i]->fromC = True;
      vg_assert(is_sane_CEnt("get_slowcase-case-4", img, i));
   }
   vg_assert(img->ces_used == ces_used_at_entry);
   if (i > 0) {
      move_CEnt_to_top(img, i);
      i = 0;
   }
   vg_assert(is_in_CEnt(img->ces[i], off));
   if (cbuf != NULL) {
      ML_(dinfo_free)(cbuf);
   }
   return img->ces[i]->data[ off - img->ces[i]->off ];
}

// This is called a lot, so do the usual fast/slow split stuff on it. */
static inline UChar get ( DiImage* img, DiOffT off )
{
   /* Most likely case is, it's in the ces[0] position. */
   /* ML_(img_from_local_file) requests a read for ces[0] when
      creating the image.  Hence slot zero is always non-NULL, so we
      can skip this test. */
   if (LIKELY(/* img->ces[0] != NULL && */
              is_in_CEnt(img->ces[0], off))) {
      return img->ces[0]->data[ off - img->ces[0]->off ];
   }
   /* Else we'll have to fish around for it. */
   return get_slowcase(img, off);
}

/* Create an image from a file in the local filesystem.  This is
   relatively straightforward. */
DiImage* ML_(img_from_local_file)(const HChar* fullpath)
{
   SysRes         fd;
   struct vg_stat stat_buf;
   DiOffT         size;

   fd = VG_(open)(fullpath, VKI_O_RDONLY, 0);
   if (sr_isError(fd))
      return NULL;

   if (VG_(fstat)(sr_Res(fd), &stat_buf) != 0) {
      VG_(close)(sr_Res(fd));
      return NULL;
   }

   size = stat_buf.size;
   if (size == 0 || size == DiOffT_INVALID
       || /* size is unrepresentable as a SizeT */
          size != (DiOffT)(SizeT)(size)) {
      VG_(close)(sr_Res(fd));
      return NULL; 
   }

   DiImage* img = ML_(dinfo_zalloc)("di.image.ML_iflf.1", sizeof(DiImage));
   img->source.is_local = True;
   img->source.fd       = sr_Res(fd);
   img->size            = size;
   img->real_size       = size;
   img->ces_used        = 0;
   img->source.name     = ML_(dinfo_strdup)("di.image.ML_iflf.2", fullpath);
   img->cslc            = NULL;
   img->cslc_size       = 0;
   img->cslc_used       = 0;
   /* img->ces is already zeroed out */
   vg_assert(img->source.fd >= 0);

   /* Force the zeroth entry to be the first chunk of the file.
      That's likely to be the first part that's requested anyway, and
      loading it at this point forcing img->cent[0] to always be
      non-empty, thereby saving us an is-it-empty check on the fast
      path in get(). */
   UInt entNo = alloc_CEnt(img, CACHE_ENTRY_SIZE, False/*!fromC*/);
   vg_assert(entNo == 0);
   set_CEnt(img, 0, 0);

   return img;
}

/* As above, but uses fd rather than filename */
DiImage* ML_(img_from_fd)(Int fd, const HChar* fullpath)
{
   struct vg_stat stat_buf;
   DiOffT         size;

   if (VG_(fstat)(fd, &stat_buf) != 0) {
      return NULL;
   }

   size = stat_buf.size;
   if (size == 0 || size == DiOffT_INVALID
       || /* size is unrepresentable as a SizeT */
          size != (DiOffT)(SizeT)(size)) {
      return NULL;
   }

   DiImage* img = ML_(dinfo_zalloc)("di.image.ML_iflf.1", sizeof(DiImage));
   img->source.is_local = True;
   img->source.fd       = fd;
   img->size            = size;
   img->real_size       = size;
   img->ces_used        = 0;
   img->source.name     = ML_(dinfo_strdup)("di.image.ML_iflf.2", fullpath);
   img->cslc            = NULL;
   img->cslc_size       = 0;
   img->cslc_used       = 0;
   /* img->ces is already zeroed out */
   vg_assert(img->source.fd >= 0);

   /* Force the zeroth entry to be the first chunk of the file.
      That's likely to be the first part that's requested anyway, and
      loading it at this point forcing img->cent[0] to always be
      non-empty, thereby saving us an is-it-empty check on the fast
      path in get(). */
   UInt entNo = alloc_CEnt(img, CACHE_ENTRY_SIZE, False/*!fromC*/);
   vg_assert(entNo == 0);
   set_CEnt(img, 0, 0);

   return img;
}



/* Create an image from a file on a remote debuginfo server.  This is
   more complex.  There are lots of ways in which it can fail. */
DiImage* ML_(img_from_di_server)(const HChar* filename,
                                 const HChar* serverAddr)
{
   if (filename == NULL || serverAddr == NULL)
      return NULL;

   /* The filename must be a plain filename -- no slashes at all. */
   if (VG_(strchr)(filename, '/') != NULL)
      return NULL;

   /* Try to connect to the server.  A side effect of this is to parse
      and reject, if syntactically invalid, |serverAddr|.  Reasons why
      this could fail:
      - serverAddr is not of the form d.d.d.d:d or d.d.d.d
      - attempt to connect to that address:port failed
   */
   Int sd = VG_(connect_via_socket)(serverAddr);
   if (sd < 0)
      return NULL;
   if (!set_blocking(sd))
      return NULL;
   Int one = 1;
   Int sr = VG_(setsockopt)(sd, VKI_IPPROTO_TCP, VKI_TCP_NODELAY, 
                            &one, sizeof(one));
   vg_assert(sr == 0);

   /* Ok, we got a connection.  Ask it for version string, so as to be
      reasonably sure we're talking to an instance of
      auxprogs/valgrind-di-server and not to some other random program
      that happens to be listening on that port. */
   Frame* req = mk_Frame_noargs("VERS");
   Frame* res = do_transaction(sd, req);
   if (res == NULL)
      goto fail; // do_transaction failed?!
   UChar* vstr = NULL;
   if (!parse_Frame_asciiz(res, "VEOK", &vstr))
      goto fail; // unexpected response kind, or invalid ID string
   vg_assert(vstr);
   if (VG_(strcmp)("Valgrind Debuginfo Server, Version 1",
                   (const HChar*)vstr) != 0)
      goto fail; // wrong version string
   free_Frame(req);
   free_Frame(res);
   req = NULL;
   res = NULL;

   /* Server seems plausible.  Present it with the name of the file we
      want and see if it'll give us back a session ID for it. */
   req = mk_Frame_asciiz("OPEN", filename);
   res = do_transaction(sd, req);
   if (res == NULL)
      goto fail;
   ULong session_id = 0, size = 0;
   if (!parse_Frame_le64_le64(res, "OPOK", &session_id, &size))
      goto fail;
   free_Frame(req);
   free_Frame(res);
   req = NULL;
   res = NULL;

   /* We have a session ID.  We're ready to roll. */
   DiImage* img = ML_(dinfo_zalloc)("di.image.ML_ifds.1", sizeof(DiImage));
   img->source.is_local   = False;
   img->source.fd         = sd;
   img->source.session_id = session_id;
   img->size              = size;
   img->real_size         = size;
   img->ces_used          = 0;
   img->source.name       = ML_(dinfo_zalloc)("di.image.ML_ifds.2",
                                              20 + VG_(strlen)(filename)
                                                 + VG_(strlen)(serverAddr));
   VG_(sprintf)(img->source.name, "%s at %s", filename, serverAddr);
   img->cslc            = NULL;
   img->cslc_size       = 0;
   img->cslc_used       = 0;

   /* img->ces is already zeroed out */
   vg_assert(img->source.fd >= 0);

   /* See comment on equivalent bit in ML_(img_from_local_file) for
      rationale. */
   UInt entNo = alloc_CEnt(img, CACHE_ENTRY_SIZE, False/*!fromC*/);
   vg_assert(entNo == 0);
   set_CEnt(img, 0, 0);

   return img;

  fail:
   free_Frame(req);
   if (res) {
      UChar* reason = NULL;
      if (parse_Frame_asciiz(res, "FAIL", &reason)) {
         // HACK: if it's just telling us that the file can't
         // be opened, don't print it, else we'll get flooded with
         // such complaints, one for each main object for which there
         // isn't a debuginfo file on the server.
         if (0 != VG_(strcmp)((const HChar*)reason, "OPEN: cannot open file"))
            VG_(umsg)("ML_(img_from_di_server): fail: %s\n", reason);
      } else {
         VG_(umsg)("ML_(img_from_di_server): fail: unknown reason\n");
      }
      free_Frame(res);
   }
   VG_(close)(sd);
   return NULL;
}

DiOffT ML_(img_mark_compressed_part)(DiImage* img, DiOffT offset, SizeT szC,
                                     SizeT szD)
{
   DiOffT ret;
   vg_assert(img != NULL);
   vg_assert(offset + szC <= img->size);

   if (img->cslc_used == img->cslc_size) {
      img->cslc_size += COMPRESSED_SLICE_ARRAY_GROW_SIZE;
      img->cslc = ML_(dinfo_realloc)("di.image.ML_img_mark_compressed_part.1",
                                     img->cslc, img->cslc_size * sizeof(CSlc));
   }

   ret = img->size;
   img->cslc[img->cslc_used].offC = offset;
   img->cslc[img->cslc_used].szC = szC;
   img->cslc[img->cslc_used].offD = img->size;
   img->cslc[img->cslc_used].szD = szD;
   img->size += szD;
   img->cslc_used++;
   return ret;
}

void ML_(img_free)(DiImage* img)
{
   vg_assert(img != NULL);

   /* Free up the cache entries, ultimately |img| itself. */
   UInt i;
   vg_assert(img->ces_used <= CACHE_N_ENTRIES);
   for (i = 0; i < img->ces_used; i++) {
      ML_(dinfo_free)(img->ces[i]);
   }
   /* Take the opportunity to sanity check the rest. */
   for (i = i; i < img->ces_used; i++) {
      vg_assert(img->ces[i] == NULL);
   }
   ML_(dinfo_free)(img->source.name);
   ML_(dinfo_free)(img->cslc);
   ML_(dinfo_free)(img);
}

void ML_(img_done)(DiImage* img)
{
   vg_assert(img != NULL);
   if (img->source.is_local) {
      /* Close the file; nothing else to do. */
      vg_assert(img->source.session_id == 0);
      VG_(close)(img->source.fd);
   } else {
      /* Close the socket.  The server can detect this and will scrub
         the connection when it happens, so there's no need to tell it
         explicitly by sending it a "CLOSE" message, or any such. */
      vg_assert(img->source.session_id != 0);
      VG_(close)(img->source.fd);
   }

   ML_(img_free)(img);
}



DiOffT ML_(img_size)(const DiImage* img)
{
   vg_assert(img != NULL);
   return img->size;
}

DiOffT ML_(img_real_size)(const DiImage* img)
{
   vg_assert(img != NULL);
   return img->real_size;
}

inline Bool ML_(img_valid)(const DiImage* img, DiOffT offset, SizeT size)
{
   vg_assert(img != NULL);
   vg_assert(offset != DiOffT_INVALID);
   return img->size > 0 && offset + size <= (DiOffT)img->size;
}

__attribute__((noinline))
static void ensure_valid_failed (const DiImage* img, DiOffT offset, SizeT size,
                                 const HChar* caller)
{
   VG_(umsg)("Valgrind: debuginfo reader: ensure_valid failed:\n");
   VG_(umsg)("Valgrind:   during call to %s\n", caller);
   VG_(umsg)("Valgrind:   request for range [%llu, +%lu) exceeds\n",
             offset, size);
   VG_(umsg)("Valgrind:   valid image size of %lu for image:\n",
             img->size);
   VG_(umsg)("Valgrind:   \"%s\"\n", img->source.name);
   give_up__image_overrun();
}

/* Check the given range is valid, and if not, shut down the system.
   An invalid range would imply that we're trying to read outside the
   image, which normally means the image is corrupted somehow, or the
   caller is buggy.  Recovering is too complex, and we have
   probably-corrupt debuginfo, so just give up. */
static void ensure_valid(const DiImage* img, DiOffT offset, SizeT size,
                         const HChar* caller)
{
   if (LIKELY(ML_(img_valid)(img, offset, size)))
      return;
   else
      ensure_valid_failed(img, offset, size, caller);
}


void ML_(img_get)(/*OUT*/void* dst,
                  DiImage* img, DiOffT offset, SizeT size)
{
   vg_assert(img != NULL);
   vg_assert(size > 0);
   ensure_valid(img, offset, size, "ML_(img_get)");
   SizeT i;
   for (i = 0; i < size; i++) {
      ((UChar*)dst)[i] = get(img, offset + i);
   }
}

SizeT ML_(img_get_some)(/*OUT*/void* dst,
                        DiImage* img, DiOffT offset, SizeT size)
{
   vg_assert(img != NULL);
   vg_assert(size > 0);
   ensure_valid(img, offset, size, "ML_(img_get_some)");
   UChar* dstU = (UChar*)dst;
   /* Use |get| in the normal way to get the first byte of the range.
      This guarantees to put the cache entry containing |offset| in
      position zero. */
   dstU[0] = get(img, offset);
   /* Now just read as many bytes as we can (or need) directly out of
      entry zero, without bothering to call |get| each time. */
   const CEnt* ce = img->ces[0];
   vg_assert(ce && ce->used >= 1);
   vg_assert(is_in_CEnt(ce, offset));
   SizeT nToCopy = size - 1;
   SizeT nAvail  = (SizeT)(ce->used - (offset + 1 - ce->off));
   vg_assert(nAvail <= ce->used-1);
   if (nAvail < nToCopy) nToCopy = nAvail;
   VG_(memcpy)(&dstU[1], &ce->data[offset + 1 - ce->off], nToCopy);
   return nToCopy + 1;
}


SizeT ML_(img_strlen)(DiImage* img, DiOffT off)
{
   ensure_valid(img, off, 1, "ML_(img_strlen)");
   SizeT i = 0;
   while (get(img, off + i) != 0) i++;
   return i;
}

HChar* ML_(img_strdup)(DiImage* img, const HChar* cc, DiOffT offset)
{
   ensure_valid(img, offset, 1, "ML_(img_strdup)");
   SizeT  len = ML_(img_strlen)(img, offset);
   HChar* res = ML_(dinfo_zalloc)(cc, len+1);
   SizeT  i;
   for (i = 0; i < len; i++) {
      res[i] = get(img, offset+i);
   }
   vg_assert(res[len] == 0);
   return res;
}

Int ML_(img_strcmp)(DiImage* img, DiOffT off1, DiOffT off2)
{
   ensure_valid(img, off1, 1, "ML_(img_strcmp)(first arg)");
   ensure_valid(img, off2, 1, "ML_(img_strcmp)(second arg)");
   while (True) {
      UChar c1 = get(img, off1);
      UChar c2 = get(img, off2);
      if (c1 < c2) return -1;
      if (c1 > c2) return 1;
      if (c1 == 0) return 0;
      off1++; off2++;
   }
}

Int ML_(img_strcmp_c)(DiImage* img, DiOffT off1, const HChar* str2)
{
   ensure_valid(img, off1, 1, "ML_(img_strcmp_c)");
   while (True) {
      UChar c1 = get(img, off1);
      UChar c2 = *(const UChar*)str2;
      if (c1 < c2) return -1;
      if (c1 > c2) return 1;
      if (c1 == 0) return 0;
      off1++; str2++;
   }
}

Int ML_(img_strcmp_n)(DiImage* img, DiOffT off1, const HChar* str2, Word n)
{
   ensure_valid(img, off1, 1, "ML_(img_strcmp_c)");
   while (n) {
      UChar c1 = get(img, off1);
      UChar c2 = *(const UChar*)str2;
      if (c1 < c2) return -1;
      if (c1 > c2) return 1;
      if (c1 == 0) return 0;
      off1++; str2++; --n;
   }
   return 0;
}

UChar ML_(img_get_UChar)(DiImage* img, DiOffT offset)
{
   ensure_valid(img, offset, 1, "ML_(img_get_UChar)");
   return get(img, offset);
}

UShort ML_(img_get_UShort)(DiImage* img, DiOffT offset)
{
   UShort r;
   ML_(img_get)(&r, img, offset, sizeof(r));
   return r;
}

UInt ML_(img_get_UInt)(DiImage* img, DiOffT offset)
{
   UInt r;
   ML_(img_get)(&r, img, offset, sizeof(r));
   return r;
}

ULong ML_(img_get_ULong)(DiImage* img, DiOffT offset)
{
   ULong r;
   ML_(img_get)(&r, img, offset, sizeof(r));
   return r;
}


/*
 * This routine for calculating the CRC for a separate debug file
 * is GPLed code borrowed from GNU binutils.
 */
UInt ML_(img_calc_gnu_debuglink_crc32)(DiImage* img)
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

   vg_assert(img != NULL);

   /* If the image is local, calculate the CRC here directly.  If it's
      remote, forward the request to the server. */
   if (img->source.is_local) {
      /* Work through the image in 1 KB chunks. */
      UInt   crc      = 0xFFFFFFFF;
      DiOffT img_szB  = ML_(img_size)(img);
      DiOffT curr_off = 0;
      while (1) {
         vg_assert(curr_off <= img_szB);
         if (curr_off == img_szB) break;
         DiOffT avail = img_szB - curr_off;
         vg_assert(avail > 0 && avail <= img_szB);
         if (avail > 1024) avail = 1024;
         UChar buf[1024];
         SizeT nGot = ML_(img_get_some)(buf, img, curr_off, avail);
         vg_assert(nGot >= 1 && nGot <= avail);
         UInt i;
         for (i = 0; i < (UInt)nGot; i++)
            crc = crc32_table[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);
         curr_off += nGot;
      }
      return ~crc & 0xFFFFFFFF;
   } else {
      Frame* req = mk_Frame_noargs("CRC3");
      Frame* res = do_transaction(img->source.fd, req);
      if (!res) goto remote_crc_fail;
      ULong crc32 = 0;
      if (!parse_Frame_le64(res, "CROK", &crc32)) goto remote_crc_fail;
      if ((crc32 & ~0xFFFFFFFFULL) != 0) goto remote_crc_fail;
      free_Frame(req);
      free_Frame(res);
      return (UInt)crc32;
     remote_crc_fail:

      // XXXX common this up with the READ diagnostic cases
      if (res) {
         UChar* reason = NULL;
         if (parse_Frame_asciiz(res, "FAIL", &reason)) {
            VG_(umsg)("img_calc_gnu_debuglink_crc32: fail: "
                      "%s\n", reason);
         } else {
            VG_(umsg)("img_calc_gnu_debuglink_crc32: fail: "
                      "unknown reason\n");
         }
      } else {
         VG_(umsg)("img_calc_gnu_debuglink_crc32: fail: "
                   "server unexpectedly closed the connection\n");
      }

      if (req) free_Frame(req);
      if (res) free_Frame(res);
      // FIXME: now what?
      give_up__comms_lost();
      /* NOTREACHED */
      vg_assert(0);
   }
   /*NOTREACHED*/
   vg_assert(0);
}

////////////////////////////////////////////////////
#include "minilzo-inl.c"

/*--------------------------------------------------------------------*/
/*--- end                                                  image.c ---*/
/*--------------------------------------------------------------------*/
