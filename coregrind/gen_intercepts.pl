#!/usr/bin/perl -w

#  This file is part of Valgrind, an extensible x86 protected-mode
#  emulator for monitoring program execution on x86-Unixes.
#
#  Copyright (C) 2000-2004 Julian Seward 
#     jseward@acm.org
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 2 of the
#  License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
#  02111-1307, USA.
#
#  The GNU General Public License is contained in the file COPYING.

use strict;

while(<>) {
  if(/VG_INTERCEPT\s*\(\s*(.*?)\s*,\s*(.*?)\s*\)/) {
    my $ver = $1 . ":" . $2;
    $ver =~ s/\*/\$2A/g;
    $ver =~ s/\+/\$2B/g;
    $ver =~ s/\-/\$2D/g;
    $ver =~ s/\./\$2E/g;
    $ver =~ s/\:/\$3A/g;
    s/VG_INTERCEPT\s*\(\s*(.*?)\s*,\s*(.*?)\s*\)/VG_INTERCEPT($ver)/g;
  } elsif(/VG_INTERCEPT_ALIAS\s*\(\s*(.*?)\s*,\s*(.*?)\s*\)/) {
    my $ver = $1 . ":" . $2;
    $ver =~ s/\*/\$2A/g;
    $ver =~ s/\+/\$2B/g;
    $ver =~ s/\-/\$2D/g;
    $ver =~ s/\./\$2E/g;
    $ver =~ s/\:/\$3A/g;
    s/VG_INTERCEPT_ALIAS\s*\(\s*(.*?)\s*,\s*(.*?)\s*\)/VG_INTERCEPT_ALIAS($ver)/g;
  }
  print $_;
}
