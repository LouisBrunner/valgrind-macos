/* Test strncpy functions.
   Copyright (C) 1999-2023 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */

/* This code is copied from GNU libc 2.38 string/test-strncpy.c
 * Necessary bits have been copied from other headers in GNU libc.
 * The code has been de-MACROed a bit and made specific for
 * wcsncpy. And re-indented. */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <wchar.h>
#include <unistd.h>

# define BIG_CHAR WCHAR_MAX
# define SMALL_CHAR 1273

size_t page_size;
unsigned char buf1[10240];
unsigned char buf2[10240];

typedef wchar_t *(*impl_t)(wchar_t *, const wchar_t *, size_t);

static void do_one_test(impl_t impl, wchar_t *dst, const wchar_t *src, size_t len, size_t n)
{
    if (impl(dst, src, n) != dst)
    {
        fprintf(stderr, "Wrong result in function wscncpy %p %p", impl(dst, src, n), dst);
        return;
    }

    if (memcmp(dst, src, (len > n ? n : len) * sizeof(wchar_t)) != 0)
    {
        fprintf(stderr, "Wrong result in function wcsncpy");
        return;
    }

    if (n > len)
    {
        size_t i;

        for (i = len; i < n; ++i)
        {
            if (dst[i] != '\0')
            {
                fprintf(stderr, "Wrong result in function wcsncpy");
                return;
            }
        }
    }
}

static void do_test(size_t align1, size_t align2, size_t len, size_t n, int max_char)
{
    size_t i;
    wchar_t *s1, *s2;

    /* For wcsncpy: align1 and align2 here mean alignment not in bytes,
       but in wchar_ts, in bytes it will equal to align * (sizeof (wchar_t)).  */
    align1 &= 7;
    if ((align1 + len) * sizeof(wchar_t) >= page_size)
        return;

    align2 &= 7;
    if ((align2 + len) * sizeof(wchar_t) >= page_size)
        return;

    s1 = (wchar_t *)(buf1) + align1;
    s2 = (wchar_t *)(buf2) + align2;

    for (i = 0; i < len; ++i)
        s1[i] = 32 + 23 * i % (max_char - 32);
    s1[len] = 0;
    for (i = len + 1; (i + align1) * sizeof(wchar_t) < page_size && i < len + 64; ++i)
        s1[i] = 32 + 32 * i % (max_char - 32);
    
    do_one_test(wcsncpy, s2, s1, len, n);
}

int main(void)
{
    wchar_t *bar = calloc(121, sizeof(wchar_t));
    wmemset(bar, 1, 120);

    wchar_t *foo = calloc(256, sizeof(wchar_t));
    wcsncpy(foo, bar, 255);
    
    page_size = sysconf (_SC_PAGE_SIZE);

    size_t i;
    for (i = 1; i < 8; ++i)
    {
        do_test(i, i, 16, 16, SMALL_CHAR);
        do_test(i, i, 16, 16, BIG_CHAR);
        do_test(i, 2 * i, 16, 16, SMALL_CHAR);
        do_test(2 * i, i, 16, 16, BIG_CHAR);
        do_test(8 - i, 2 * i, 1 << i, 2 << i, SMALL_CHAR);
        do_test(2 * i, 8 - i, 2 << i, 1 << i, SMALL_CHAR);
        do_test(8 - i, 2 * i, 1 << i, 2 << i, BIG_CHAR);
        do_test(2 * i, 8 - i, 2 << i, 1 << i, BIG_CHAR);
    }

    for (i = 1; i < 8; ++i)
    {
        do_test(0, 0, 4 << i, 8 << i, SMALL_CHAR);
        do_test(0, 0, 16 << i, 8 << i, SMALL_CHAR);
        do_test(8 - i, 2 * i, 4 << i, 8 << i, SMALL_CHAR);
        do_test(8 - i, 2 * i, 16 << i, 8 << i, SMALL_CHAR);
    }
}
