
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_verbose

EXTRA_DIST = \
	alloc-fns-A.post.exp alloc-fns-A.stderr.exp alloc-fns-A.vgtest \
	alloc-fns-B.post.exp alloc-fns-B.stderr.exp alloc-fns-B.vgtest \
	basic.post.exp basic.stderr.exp basic.vgtest \
	basic2.post.exp basic2.stderr.exp basic2.vgtest \
	big-alloc.post.exp big-alloc.post.exp-64bit \
	big-alloc.stderr.exp big-alloc.vgtest \
	deep-A.post.exp deep-A.stderr.exp deep-A.vgtest \
	deep-B.post.exp deep-B.stderr.exp deep-B.vgtest \
	deep-C.post.exp deep-C.stderr.exp deep-C.vgtest \
	deep-D.post.exp deep-D.stderr.exp deep-D.vgtest \
        culling1.stderr.exp culling1.vgtest \
        culling2.stderr.exp culling2.vgtest \
	custom_alloc.post.exp custom_alloc.stderr.exp custom_alloc.vgtest \
	ignored.post.exp ignored.stderr.exp ignored.vgtest \
	ignoring.post.exp ignoring.stderr.exp ignoring.vgtest \
	insig.post.exp insig.stderr.exp insig.vgtest \
	long-names.post.exp long-names.stderr.exp long-names.vgtest \
	long-time.post.exp long-time.stderr.exp long-time.vgtest \
	malloc_usable.stderr.exp malloc_usable.vgtest \
	new-cpp.post.exp new-cpp.stderr.exp new-cpp.vgtest \
	no-stack-no-heap.post.exp no-stack-no-heap.stderr.exp no-stack-no-heap.vgtest \
	null.post.exp null.stderr.exp null.vgtest \
	one.post.exp one.post.exp2 one.stderr.exp one.vgtest \
	overloaded-new.post.exp overloaded-new.stderr.exp overloaded-new.vgtest \
	peak.post.exp peak.stderr.exp peak.vgtest \
	peak2.post.exp peak2.stderr.exp peak2.vgtest \
	realloc.post.exp realloc.stderr.exp realloc.vgtest \
	thresholds_0_0.post.exp   thresholds_0_0.stderr.exp   thresholds_0_0.vgtest \
	thresholds_0_10.post.exp  thresholds_0_10.stderr.exp  thresholds_0_10.vgtest \
	thresholds_10_0.post.exp  thresholds_10_0.stderr.exp  thresholds_10_0.vgtest \
	thresholds_5_0.post.exp   thresholds_5_0.stderr.exp   thresholds_5_0.vgtest \
	thresholds_5_10.post.exp  thresholds_5_10.stderr.exp  thresholds_5_10.vgtest \
	thresholds_10_10.post.exp thresholds_10_10.stderr.exp thresholds_10_10.vgtest \
	zero1.post.exp zero1.stderr.exp zero1.vgtest \
	zero2.post.exp zero2.stderr.exp zero2.vgtest

check_PROGRAMS = \
	alloc-fns \
	basic \
	big-alloc \
	culling1 culling2 \
	custom_alloc \
	deep \
	ignored \
	ignoring \
	insig \
	long-names \
	long-time \
	malloc_usable \
	new-cpp \
	null \
	one \
	overloaded-new \
	peak \
	realloc \
	thresholds \
	zero

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# C++ tests
new_cpp_SOURCES		= new-cpp.cpp
overloaded_new_SOURCES	= overloaded-new.cpp

