/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2020-2025 Louis Brunner <louis.brunner.fr@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/
/*
 * This file contains mig_strncpy and mig_strncpy_zerofill reimplemented
 * from comments left by Joshua Block in the original file
 * `xnu/libsyscall/mach/mig_strncpy.c`
 *
 * Here they are for reference:
 */
/*
 * mig_strncpy.c - by Joshua Block
 *
 * mig_strncpy -- Bounded string copy.  Does what the library routine strncpy
 * OUGHT to do:  Copies the (null terminated) string in src into dest, a
 * buffer of length len.  Assures that the copy is still null terminated
 * and doesn't overflow the buffer, truncating the copy if necessary.
 *
 * Parameters:
 *
 *     dest - Pointer to destination buffer.
 *
 *     src - Pointer to source string.
 *
 *     len - Length of destination buffer.
 *
 * Result:
 *	length of string copied, INCLUDING the trailing 0.
 *
 * mig_strncpy_zerofill -- Bounded string copy.  Does what the
 * library routine strncpy OUGHT to do:  Copies the (null terminated)
 * string in src into dest, a buffer of length len.  Assures that
 * the copy is still null terminated and doesn't overflow the buffer,
 * truncating the copy if necessary. If the string in src is smaller
 * than given length len, it will zero fill the remaining bytes in dest.
 *
 * Parameters:
 *
 *     dest - Pointer to destination buffer.
 *
 *     src - Pointer to source string.
 *
 *     len - Length of destination buffer.
 *
 * Result:
 *	length of string copied, INCLUDING the trailing 0.
 */

#if defined(VGO_darwin)

#include <mach/mig_errors.h>

int mig_strncpy(char *dest, const char *src, int len)
{
  int i;
  int src_len;

  for (src_len = 0; src[src_len]; ++src_len);

  if (len > src_len) {
    for (i = 0; i < src_len; ++i) {
      dest[i] = src[i];
    }
    dest[i] = '\0';
    return i + 1;
  }

  for (i = 0; i < len; ++i) {
    dest[i] = src[i];
  }
  dest[i - 1] = '\0';
  return i;
}

int mig_strncpy_zerofill(char *dest, const char *src, int len)
{
  int i;
  int src_len;

  for (src_len = 0; src[src_len]; ++src_len);

  if (len > src_len) {
    for (i = 0; i < src_len; ++i) {
      dest[i] = src[i];
    }
    for (; i < len; ++i) {
      dest[i] = '\0';
    }
    return len;
  }

  for (i = 0; i < len; ++i) {
    dest[i] = src[i];
  }
  dest[i - 1] = '\0';
  return len;
}

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
