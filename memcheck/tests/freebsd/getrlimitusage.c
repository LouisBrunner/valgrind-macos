/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2024 The FreeBSD Foundation
 *
 * This software was developed by Konstantin Belousov <kib@FreeBSD.org>
 * under sponsorship from the FreeBSD Foundation.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <sys/param.h>
#define _RLIMIT_IDENT
#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <unistd.h>

static void usage(void)
{
    fprintf(stderr, "usage: getrlimitusage [-e] [-p pid]\n");
    exit(2);
}

int main(int argc, char *argv[])
{
    rlim_t res;
    int c, flags;

    flags = 0;
    while ((c = getopt(argc, argv, "e")) != -1)
    {
        switch (c)
        {
        case 'e':
            flags |= GETRLIMITUSAGE_EUID;
            break;
        case '?':
        default:
            usage();
        }
    }
    argc -= optind;
    argv += optind;
    if (argc != 0)
        usage();

    for (unsigned i = 0;; i++)
    {
        if (getrlimitusage(i, flags, &res) == -1)
        {
            if (errno == ENXIO)
            {
                res = -1;
            }
            else
            {
                if (errno != EINVAL)
                    err(1, "getrlimitusage(%d)", errno);
                break;
            }
        }
        // add some rounding to try to make regtest stable
        // most of these change all the time
        switch (i)
        {
        case 7:
        case 5:
        case 9:
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
            res = 0U;
            break;
        case 10:
            res = res/100000000 * 100000000;
            break;
        default:
             break;
        }
        fprintf(stderr, "%s (%d):\t%jd\n", i < nitems(rlimit_ident) ? rlimit_ident[i] : "unknown", i, (uintmax_t)res
);
    }
    exit(0);
}
