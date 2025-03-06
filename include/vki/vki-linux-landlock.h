/*
   This file is part of Valgrind, a dynamic binary instrumentation framework.

   Copyright (C) 2024 Peter Seiderer <ps.report@gmx.net>

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
#ifndef __VKI_LANDLOCK_H
#define __VKI_LANDLOCK_H

// Derived from linux-6.9.7/include/uapi/linux/landlock.h
struct vki_landlock_ruleset_attr {
	__vki_u64 handled_access_fs;
	__vki_u64 handled_access_net;
};

enum vki_landlock_rule_type {
	VKI_LANDLOCK_RULE_PATH_BENEATH = 1,
	VKI_LANDLOCK_RULE_NET_PORT,
};

#define VKI_LANDLOCK_CREATE_RULESET_VERSION 1

#endif
