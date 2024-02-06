
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_verbose filter_new_aligned

EXTRA_DIST = \
	alloc-fns-A.post.exp alloc-fns-A.stderr.exp alloc-fns-A.vgtest \
	alloc-fns-B.post.exp alloc-fns-B.stderr.exp alloc-fns-B.vgtest \
	basic.post.exp basic.stderr.exp basic.vgtest \
	basic2.post.exp basic2.stderr.exp basic2.vgtest \
	big-alloc.post.exp big-alloc.post.exp-64bit big-alloc.post.exp-ppc64 \
	big-alloc.stderr.exp big-alloc.vgtest \
	big-alloc.post.exp-x86-freebsd \
	bug469146.post.exp bug469146.stderr.exp bug469146.vgtest \
	deep-A.post.exp deep-A.stderr.exp deep-A.vgtest \
	deep-B.post.exp deep-B.stderr.exp deep-B.vgtest \
	deep-C.post.exp deep-C.stderr.exp deep-C.vgtest \
	deep-D.post.exp deep-D.stderr.exp deep-D.vgtest \
        culling1.stderr.exp culling1.vgtest \
        culling2.stderr.exp culling2.vgtest \
	custom_alloc.post.exp custom_alloc.stderr.exp custom_alloc.vgtest \
	ignored.post.exp ignored.stderr.exp ignored.vgtest \
	ignoring.post.exp ignoring.stderr.exp ignoring.vgtest \
	inlinfomalloc.post.exp inlinfomalloc.stderr.exp inlinfomalloc.vgtest \
	insig.post.exp insig.stderr.exp insig.vgtest \
	long-names.post.exp long-names.stderr.exp long-names.vgtest \
	long-time.post.exp long-time.stderr.exp long-time.vgtest \
	malloc_usable.stderr.exp malloc_usable.vgtest \
	mmapunmap.post.exp mmapunmap.post.exp-ppc64 \
	mmapunmap.stderr.exp mmapunmap.vgtest \
	new-cpp.post.exp new-cpp.stderr.exp new-cpp.vgtest \
	no-stack-no-heap.post.exp no-stack-no-heap.stderr.exp no-stack-no-heap.vgtest \
	null.post.exp null.stderr.exp null.vgtest \
	one.post.exp one.post.exp2 one.stderr.exp one.vgtest \
	overloaded-new.post.exp overloaded-new.post.exp-mips32 \
	overloaded-new.stderr.exp overloaded-new.vgtest \
	pages_as_heap.stderr.exp pages_as_heap.vgtest \
	peak.post.exp peak.stderr.exp peak.vgtest \
	peak2.post.exp peak2.stderr.exp peak2.vgtest \
	realloc.post.exp realloc.stderr.exp realloc.vgtest \
	thresholds_0_0.post.exp \
	thresholds_0_0.stderr.exp   thresholds_0_0.vgtest \
	thresholds_0_10.post.exp    thresholds_0_10.stderr.exp \
	thresholds_0_10.vgtest \
	thresholds_10_0.post.exp \
	thresholds_10_0.stderr.exp  thresholds_10_0.vgtest \
	thresholds_5_0.post.exp  \
	thresholds_5_0.stderr.exp   thresholds_5_0.vgtest \
	thresholds_5_10.post.exp \
	thresholds_5_10.stderr.exp  thresholds_5_10.vgtest \
	thresholds_10_10.post.exp \
	thresholds_10_10.stderr.exp thresholds_10_10.vgtest \
	zero1.post.exp zero1.stderr.exp zero1.vgtest \
	zero2.post.exp zero2.stderr.exp zero2.vgtest

check_PROGRAMS = \
	alloc-fns \
	basic \
	big-alloc \
	bug469146 \
	culling1 culling2 \
	custom_alloc \
	deep \
	ignored \
	ignoring \
	inlinfomalloc \
	insig \
	long-names \
	long-time \
	mmapunmap \
	malloc_usable \
	new-cpp \
	null \
	one \
	pages_as_heap \
	peak \
	realloc \
	thresholds \
	zero

if HAVE_ALIGNED_CXX_ALLOC
check_PROGRAMS += overloaded-new
endif

inlinfomalloc_CFLAGS = $(AM_CFLAGS) -w

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# C++ tests
bug469146_SOURCES	= bug469146.cpp
# -fno-optimize-sibling-calls because otherwise some platforms will have
# tail call optimization which messes up --ignore-fn
bug469146_CXXFLAGS	= $(AM_CXXFLAGS) -O2 -fno-optimize-sibling-calls @FLAG_W_NO_USE_AFTER_FREE@ @FLAG_W_NO_ATTRIBUTES@
new_cpp_SOURCES		= new-cpp.cpp
overloaded_new_SOURCES	= overloaded-new.cpp
# aligned new needs C++17
overloaded_new_CXXFLAGS	= $(AM_CXXFLAGS) -Wno-implicit-exception-spec-mismatch -Wno-missing-exception-spec -std=c++17

# Suppress warnings for issues we are testing for
alloc_fns_CFLAGS	= $(AM_CFLAGS) -Wno-unused-result
big_alloc_CFLAGS	= $(AM_CFLAGS) -Wno-unused-result
culling1_CFLAGS		= $(AM_CFLAGS) -Wno-unused-result
culling2_CFLAGS		= $(AM_CFLAGS) -Wno-unused-result
deep_CFLAGS		= $(AM_CFLAGS) -Wno-unused-result
ignoring_CFLAGS		= $(AM_CFLAGS) -Wno-unused-result
insig_CFLAGS		= $(AM_CFLAGS) -Wno-unused-result
long_names_CFLAGS	= $(AM_CFLAGS) -Wno-unused-result
one_CFLAGS		= $(AM_CFLAGS) -Wno-unused-result
thresholds_CFLAGS	= $(AM_CFLAGS) -Wno-unused-result
realloc_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_FREE_NONHEAP_OBJECT@
