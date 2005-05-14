
SUBDIRS      = $(VG_ARCH)     $(VG_OS)     $(VG_PLATFORM)     .
DIST_SUBDIRS = $(VG_ARCH_ALL) $(VG_OS_ALL) $(VG_PLATFORM_ALL) .

EXTRA_DIST = \
	valgrind.h.in \
	vg_profile.c

incincdir = $(includedir)/valgrind

incinc_HEADERS = \
	basic_types.h 		\
	tool.h 			\
	tool_asm.h 		\
	pub_tool_errormgr.h 	\
	pub_tool_execontext.h 	\
	pub_tool_hashtable.h 	\
	pub_tool_mallocfree.h 	\
	pub_tool_replacemalloc.h\
	pub_tool_skiplist.h 	\
	pub_tool_stacktrace.h 	\
	pub_tool_tooliface.h 	\
	valgrind.h

BUILT_SOURCES = valgrind.h
CLEANFILES = valgrind.h
