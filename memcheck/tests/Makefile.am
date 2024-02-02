
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .

# Arch-specific tests.
if VGCONF_ARCHS_INCLUDE_X86
SUBDIRS += x86
endif
if VGCONF_ARCHS_INCLUDE_AMD64
SUBDIRS += amd64
endif
if VGCONF_ARCHS_INCLUDE_MIPS32
SUBDIRS += mips32
endif
if VGCONF_ARCHS_INCLUDE_MIPS64
SUBDIRS += mips64
endif
if VGCONF_ARCHS_INCLUDE_PPC32
SUBDIRS += ppc32
endif
if VGCONF_ARCHS_INCLUDE_PPC64
SUBDIRS += ppc64
endif
if VGCONF_ARCHS_INCLUDE_S390X
SUBDIRS += s390x
endif

# OS-specific tests
if VGCONF_OS_IS_LINUX
SUBDIRS += linux
endif
if VGCONF_OS_IS_DARWIN
SUBDIRS += darwin
endif
if VGCONF_OS_IS_SOLARIS
SUBDIRS += solaris
endif
if VGCONF_OS_IS_FREEBSD
SUBDIRS += freebsd
endif

# Platform-specific tests
if VGCONF_PLATFORMS_INCLUDE_X86_LINUX
SUBDIRS += x86-linux
endif
if VGCONF_PLATFORMS_INCLUDE_AMD64_LINUX
SUBDIRS += amd64-linux
endif
if VGCONF_PLATFORMS_INCLUDE_ARM64_LINUX
SUBDIRS += arm64-linux
endif
if VGCONF_PLATFORMS_INCLUDE_X86_SOLARIS
SUBDIRS += x86-solaris
endif
if VGCONF_PLATFORMS_INCLUDE_AMD64_SOLARIS
SUBDIRS += amd64-solaris
endif
if VGCONF_PLATFORMS_INCLUDE_X86_FREEBSD
SUBDIRS += x86-freebsd
endif
if VGCONF_PLATFORMS_INCLUDE_AMD64_FREEBSD
SUBDIRS += amd64-freebsd
endif

DIST_SUBDIRS = x86 amd64 ppc32 ppc64 s390x linux \
		darwin solaris x86-linux amd64-linux arm64-linux \
		x86-solaris amd64-solaris mips32 mips64 \
		freebsd amd64-freebsd x86-freebsd \
		common .

dist_noinst_SCRIPTS = \
	filter_addressable \
	filter_allocs \
	filter_leak_cases_possible \
	filter_leak_cpp_interior \
	filter_xml \
	filter_strchr \
	filter_varinfo3 \
	filter_memcheck \
	filter_malloc_free \
	filter_size_t \
	filter_stanza \
	filter_stanza.awk \
	filter_used_supp \
	filter_stanza_and_size_t

noinst_SCRIPTS = \
	filter_dw4 \
	filter_overlaperror \
	filter_stderr

noinst_HEADERS = leak.h

EXTRA_DIST = \
	accounting.stderr.exp accounting.vgtest \
	addressable.stderr.exp addressable.stdout.exp addressable.vgtest \
	atomic_incs.stderr.exp atomic_incs.vgtest \
	atomic_incs.stdout.exp-32bit atomic_incs.stdout.exp-64bit \
	atomic_incs.stdout.exp-64bit-and-128bit \
	badaddrvalue.stderr.exp \
	badaddrvalue.stdout.exp badaddrvalue.vgtest \
	badfree-2trace.stderr.exp badfree-2trace.vgtest \
	badfree.stderr.exp badfree.vgtest \
	badfree3.stderr.exp badfree3.vgtest \
	badjump.stderr.exp badjump.vgtest \
	badjump2.stderr.exp badjump2.vgtest \
	badjump.stderr.exp-kfail \
	badjump2.stderr.exp-kfail \
	badloop.stderr.exp badloop.vgtest \
	badpoll.stderr.exp badpoll.vgtest \
	badrw.stderr.exp badrw.vgtest badrw.stderr.exp-s390x-mvc \
	big_blocks_freed_list.stderr.exp big_blocks_freed_list.vgtest \
	brk2.stderr.exp brk2.vgtest \
	buflen_check.stderr.exp buflen_check.vgtest \
		buflen_check.stderr.exp-kfail \
	bug155125.stderr.exp bug155125.vgtest \
	bug287260.stderr.exp bug287260.vgtest \
	bug340392.stderr.exp bug340392.vgtest \
	bug401284.stderr.exp bug401284.stdout.exp bug401284.vgtest \
	bug464969_d_demangle.stderr.exp bug464969_d_demangle.vgtest \
		bug464969_d_demangle.stdout.exp \
	bug472219.stderr.exp bug472219.vgtest \
	calloc-overflow.stderr.exp calloc-overflow.vgtest\
	cdebug_zlib.stderr.exp cdebug_zlib.vgtest \
	cdebug_zlib_gnu.stderr.exp cdebug_zlib_gnu.vgtest \
	client-msg.stderr.exp client-msg.vgtest \
	client-msg-as-xml.stderr.exp client-msg-as-xml.vgtest \
	clientperm.stderr.exp \
	clientperm.stdout.exp clientperm.vgtest \
	clireq_nofill.stderr.exp \
	clireq_nofill.stdout.exp clireq_nofill.vgtest \
	clo_redzone_default.vgtest clo_redzone_128.vgtest \
	clo_redzone_default.stderr.exp clo_redzone_128.stderr.exp \
	cond_ld.vgtest cond_ld.stdout.exp cond_ld.stderr.exp-arm \
		cond_ld.stderr.exp-64bit-non-arm \
		cond_ld.stderr.exp-32bit-non-arm \
	cond_st.vgtest cond_st.stdout.exp cond_st.stderr.exp-arm \
		cond_st.stderr.exp-64bit-non-arm \
		cond_st.stderr.exp-32bit-non-arm \
	leak_cpp_interior.stderr.exp leak_cpp_interior.stderr.exp-64bit leak_cpp_interior.vgtest libstdc++.supp \
	custom_alloc.stderr.exp custom_alloc.vgtest \
		custom_alloc.stderr.exp-s390x-mvc \
	custom-overlap.stderr.exp custom-overlap.vgtest \
	cxx17_aligned_new.stderr.exp cxx17_aligned_new.vgtest \
		cxx17_aligned_new.stderr.exp_32 \
		cxx17_aligned_new.stdout.exp \
	sized_aligned_new_delete_args.stderr.exp \
		sized_aligned_new_delete_args.vgtest \
	sized_aligned_new_delete_misaligned1.stderr.exp \
		sized_aligned_new_delete_misaligned1.vgtest \
	sized_aligned_new_delete_misaligned2.stderr.exp \
		sized_aligned_new_delete_misaligned2.vgtest \
	sized_aligned_new_delete_misaligned3.stderr.exp \
		sized_aligned_new_delete_misaligned3.vgtest \
	sized_aligned_new_delete_misaligned1_xml.stderr.exp \
		sized_aligned_new_delete_misaligned1_xml.vgtest \
	sized_aligned_new_delete_misaligned2_xml.stderr.exp \
		sized_aligned_new_delete_misaligned2_xml.vgtest \
	sized_aligned_new_delete_misaligned3_xml.stderr.exp \
		sized_aligned_new_delete_misaligned3_xml.vgtest \
	sized_aligned_new_delete_misaligned1_supp.vgtest \
		sized_aligned_new_delete_misaligned1_supp.supp \
		sized_aligned_new_delete_misaligned1_supp.stderr.exp \
		sized_aligned_new_delete_misaligned1_supp.stderr.exp_32 \
	sized_aligned_new_delete_misaligned2_supp.vgtest \
		sized_aligned_new_delete_misaligned2_supp.supp \
		sized_aligned_new_delete_misaligned2_supp.stderr.exp \
		sized_aligned_new_delete_misaligned2_supp.stderr.exp_32 \
	sized_aligned_new_delete_misaligned3_supp.vgtest \
		sized_aligned_new_delete_misaligned3_supp.supp \
		sized_aligned_new_delete_misaligned3_supp.stderr.exp \
		sized_aligned_new_delete_misaligned3_supp.stderr.exp_32 \
	new_aligned_delete_default.stderr.exp \
	new_aligned_delete_default.stderr.exp_32 \
		new_aligned_delete_default.vgtest \
	new_delete_mismatch_size.stderr.exp \
		new_delete_mismatch_size.vgtest \
	new_delete_mismatch_size_supp.stderr.exp \
		new_delete_mismatch_size_supp.stderr.exp_32 \
		new_delete_mismatch_size_supp.vgtest \
		new_delete_mismatch_size_supp.supp \
	deep-backtrace.vgtest deep-backtrace.stderr.exp \
	demangle.stderr.exp demangle.vgtest \
	big_debuginfo_symbol.stderr.exp big_debuginfo_symbol.vgtest \
	describe-block.stderr.exp describe-block.vgtest \
	descr_belowsp.vgtest descr_belowsp.stderr.exp \
	doublefree.stderr.exp doublefree.vgtest \
	dw4.vgtest dw4.stderr.exp dw4.stderr.exp-solaris dw4.stdout.exp \
	err_disable1.vgtest err_disable1.stderr.exp \
	err_disable2.vgtest err_disable2.stderr.exp \
	err_disable3.vgtest err_disable3.stderr.exp \
	err_disable4.vgtest err_disable4.stderr.exp \
	err_disable_arange1.vgtest err_disable_arange1.stderr.exp \
	erringfds.stderr.exp erringfds.stdout.exp erringfds.vgtest \
	error_counts.stderr.exp error_counts.vgtest \
	errs1.stderr.exp errs1.vgtest \
        exit_on_first_error.stderr.exp \
        exit_on_first_error.vgtest \
        exit_on_first_error_with_xml.stderr.exp \
        exit_on_first_error_with_xml.vgtest \
	exitprog.stderr.exp exitprog.vgtest \
	execve1.stderr.exp execve1.vgtest execve1.stderr.exp-kfail \
	execve2.stderr.exp execve2.vgtest execve2.stderr.exp-kfail \
	file_locking.stderr.exp file_locking.vgtest \
	fprw.stderr.exp fprw.stderr.exp-freebsd fprw.stderr.exp-mips32-be \
		fprw.stderr.exp-mips32-le fprw.vgtest \
		fprw.stderr.exp-freebsd-x86 \
	fwrite.stderr.exp fwrite.vgtest fwrite.stderr.exp-kfail \
	gone_abrt_xml.vgtest gone_abrt_xml.stderr.exp gone_abrt_xml.stderr.exp-solaris \
		gone_abrt_xml.stderr.exp-freebsd \
	holey_buffer_too_small.vgtest holey_buffer_too_small.stdout.exp \
	holey_buffer_too_small.stderr.exp \
	inits.stderr.exp inits.vgtest \
	inline.stderr.exp inline.stdout.exp inline.vgtest \
	inlinfo.stderr.exp inlinfo.stdout.exp inlinfo.vgtest \
	inlinfosupp.stderr.exp inlinfosupp.stdout.exp inlinfosupp.supp inlinfosupp.vgtest \
	inlinfosuppobj.stderr.exp inlinfosuppobj.stdout.exp inlinfosuppobj.supp inlinfosuppobj.vgtest \
	inltemplate.stderr.exp inltemplate.stdout.exp inltemplate.vgtest \
		inltemplate.stderr.exp-old-gcc \
	leak-0.vgtest leak-0.stderr.exp \
	leak-cases-full.vgtest leak-cases-full.stderr.exp \
	leak-cases-possible.vgtest leak-cases-possible.stderr.exp \
	leak-cases-summary.vgtest leak-cases-summary.stderr.exp \
	leak-cycle.vgtest leak-cycle.stderr.exp \
	leak-delta.vgtest leak-delta.stderr.exp \
	leak-pool-0.vgtest leak-pool-0.stderr.exp \
	leak-pool-1.vgtest leak-pool-1.stderr.exp \
	leak-pool-2.vgtest leak-pool-2.stderr.exp \
	leak-pool-3.vgtest leak-pool-3.stderr.exp \
	leak-pool-4.vgtest leak-pool-4.stderr.exp \
	leak-pool-5.vgtest leak-pool-5.stderr.exp \
	leak-autofreepool-0.vgtest leak-autofreepool-0.stderr.exp \
	leak-autofreepool-1.vgtest leak-autofreepool-1.stderr.exp \
	leak-autofreepool-2.vgtest leak-autofreepool-2.stderr.exp \
	leak-autofreepool-4.vgtest leak-autofreepool-4.stderr.exp \
	leak-autofreepool-5.vgtest leak-autofreepool-5.stderr.exp \
	leak-autofreepool-6.vgtest leak-autofreepool-6.stderr.exp \
	leak-autofreepool-5.stderr.exp-freebsd \
	leak-tree.vgtest leak-tree.stderr.exp \
	leak-segv-jmp.vgtest leak-segv-jmp.stderr.exp \
	lks.vgtest lks.stdout.exp lks.supp lks.stderr.exp \
	long_namespace_xml.vgtest long_namespace_xml.stdout.exp \
	long_namespace_xml.stderr.exp \
	long-supps.vgtest long-supps.stderr.exp long-supps.supp \
	mallinfo.stderr.exp mallinfo.vgtest \
	malloc_free_fill.vgtest \
	malloc_free_fill.stderr.exp \
	malloc_usable.stderr.exp malloc_usable.vgtest \
	malloc1.stderr.exp malloc1.vgtest \
	malloc1_ks_none.stderr.exp malloc1_ks_none.vgtest \
	malloc1_ks_alloc.stderr.exp malloc1_ks_alloc.vgtest \
	malloc1_ks_free.stderr.exp malloc1_ks_free.vgtest \
	malloc1_ks_alloc_and_free.stderr.exp malloc1_ks_alloc_and_free.vgtest \
	malloc2.stderr.exp malloc2.vgtest \
	malloc3.stderr.exp malloc3.stdout.exp malloc3.vgtest \
	manuel1.stderr.exp manuel1.stdout.exp manuel1.vgtest \
		manuel1.stderr.exp-freebsd-clang \
	manuel2.stderr.exp manuel2.stderr.exp64 manuel2.stdout.exp \
		manuel2.vgtest \
	manuel3.stderr.exp manuel3.vgtest \
	match-overrun.stderr.exp match-overrun.vgtest match-overrun.supp \
	memalign_test.stderr.exp memalign_test.vgtest \
	memalign_args.vgtest memalign_args.stderr.exp \
		memalign_args.stderr.exp-glibc \
		memalign_args.stderr.exp-darwin \
		memalign_args.stderr.exp-solaris \
	memccpy1.stderr.exp memccpy1.stdout.exp memccpy1.vgtest \
	memccpy2.stderr.exp memccpy2.vgtest \
	memcmptest.stderr.exp memcmptest.stderr.exp2 \
	memcmptest.stdout.exp memcmptest.vgtest \
	memmem.stderr.exp memmem.vgtest \
	mempool.stderr.exp mempool.vgtest \
	mempool2.stderr.exp mempool2.vgtest \
	metadata.stderr.exp metadata.stdout.exp metadata.vgtest \
	mismatches.stderr.exp mismatches.vgtest \
		mismatches.stderr.exp2 \
	mismatches_xml.stderr.exp mismatches_xml.vgtest \
		mismatches_xml.stderr.exp2 \
	mmaptest.stderr.exp mmaptest.vgtest \
	nanoleak_supp.stderr.exp nanoleak_supp.vgtest nanoleak.supp \
	nanoleak_dynsupp.stderr.exp nanoleak_dynsupp.vgtest \
	nanoleak2.stderr.exp nanoleak2.vgtest \
	new_nothrow.stderr.exp new_nothrow.vgtest \
	new_override.stderr.exp new_override.stdout.exp new_override.vgtest \
	noisy_child.vgtest noisy_child.stderr.exp noisy_child.stdout.exp \
	null_socket.stderr.exp null_socket.vgtest \
	origin1-yes.vgtest origin1-yes.stdout.exp origin1-yes.stderr.exp \
		origin1-yes.stderr.exp-freebsd \
	origin2-not-quite.vgtest origin2-not-quite.stdout.exp \
	origin2-not-quite.stderr.exp \
		origin2-not-quite.stderr.exp-freebsd \
	origin3-no.vgtest origin3-no.stdout.exp \
		origin3-no.stderr.exp-freebsd \
	origin3-no.stderr.exp \
	origin4-many.vgtest origin4-many.stdout.exp \
	origin4-many.stderr.exp \
	origin5-bz2.vgtest origin5-bz2.stdout.exp \
	origin5-bz2.stderr.exp-glibc25-x86 \
	origin5-bz2.stderr.exp-glibc25-amd64 \
	origin5-bz2.stderr.exp-glibc25-amd64-b \
	origin5-bz2.stderr.exp-glibc25-x86-b \
	origin5-bz2.stderr.exp-glibc27-ppc64 \
	origin5-bz2.stderr.exp-glibc212-s390x \
	origin5-bz2.stderr.exp-glibc234-s390x \
	origin5-bz2.stderr.exp-glibc218-mips32 \
	origin6-fp.vgtest origin6-fp.stdout.exp \
	origin6-fp.stderr.exp-glibc25-amd64 \
	origin6-fp.stderr.exp-glibc27-ppc64 \
	overlap.stderr.exp overlap.stdout.exp overlap.vgtest \
	partiallydefinedeq.vgtest partiallydefinedeq.stderr.exp \
	partiallydefinedeq.stderr.exp4 \
	partiallydefinedeq.stderr.exp3 \
	partiallydefinedeq.stderr.exp2 \
	partiallydefinedeq.stdout.exp \
	partial_load_ok.vgtest partial_load_ok.stderr.exp \
		partial_load_ok.stderr.exp64 \
		partial_load_ok.stderr.exp-ppc64 \
	partial_load_dflt.vgtest partial_load_dflt.stderr.exp \
		partial_load_dflt.stderr.exp64 \
		partial_load_dflt.stderr.exp-ppc64 \
	partial_load_dflt.stderr.expr-s390x-mvc \
	pdb-realloc.stderr.exp pdb-realloc.vgtest \
	pdb-realloc2.stderr.exp pdb-realloc2.stdout.exp pdb-realloc2.vgtest \
	pipe.stderr.exp pipe.vgtest \
	pointer-trace.vgtest \
	pointer-trace.stderr.exp \
	posix_memalign.stderr.exp posix_memalign.vgtest \
		posix_memalign.stderr.exp-darwin \
	posix_memalign_supp.stderr.exp posix_memalign_supp.vgtest \
		posix_memalign_supp.supp \
	posix_memalign_xml.stderr.exp posix_memalign_xml.vgtest \
	post-syscall.stderr.exp post-syscall.vgtest \
	reach_thread_register.stderr.exp reach_thread_register.vgtest \
		reach_thread_register.stderr.exp-mips32 \
		reach_thread_register.stderr.exp-mips64 \
	realloc_size_zero.stderr.exp realloc_size_zero.stdout.exp-glibc \
		realloc_size_zero.stdout.exp-other \
		realloc_size_zero.vgtest \
	realloc_size_zero_yes.stderr.exp realloc_size_zero_yes.stdout.exp \
		realloc_size_zero_yes.vgtest \
	realloc_size_zero_again_yes.stderr.exp \
		realloc_size_zero_again_yes.stdout.exp \
		realloc_size_zero_again_yes.vgtest \
	realloc_size_zero_no.stderr.exp realloc_size_zero_no.stdout.exp \
		realloc_size_zero_no.vgtest \
	realloc_size_zero_again_no.stderr.exp \
		realloc_size_zero_again_no.stdout.exp \
		realloc_size_zero_again_no.vgtest \
	realloc_size_zero_off.stderr.exp realloc_size_zero_off.stdout.exp \
		realloc_size_zero_off.vgtest \
	realloc_size_zero_mismatch.stderr.exp \
		realloc_size_zero_mismatch.stdout.exp \
		realloc_size_zero_mismatch.vgtest \
	realloc_size_zero_supp.stderr.exp realloc_size_zero_supp.stdout.exp \
		realloc_size_zero_supp.vgtest \
		realloc_size_zero.supp \
	realloc1.stderr.exp realloc1.vgtest \
	realloc2.stderr.exp realloc2.vgtest \
	realloc3.stderr.exp realloc3.vgtest \
	recursive-merge.stderr.exp recursive-merge.vgtest \
	resvn_stack.stderr.exp resvn_stack.vgtest \
	demangle-rust.vgtest demangle-rust.stderr.exp \
	sbfragment.stdout.exp sbfragment.stderr.exp sbfragment.vgtest \
	sem.stderr.exp sem.vgtest \
	sendmsg.stderr.exp sendmsg.stderr.exp-solaris sendmsg.vgtest \
	    sendmsg.stderr.exp-freebsd \
	    sendmsg.stderr.exp-freebsd-x86 \
	sh-mem.stderr.exp sh-mem.vgtest \
	sh-mem-random.stderr.exp sh-mem-random.stdout.exp64 \
	sh-mem-random.stdout.exp sh-mem-random.vgtest \
	sigaltstack.stderr.exp sigaltstack.vgtest \
	sigkill.stderr.exp sigkill.stderr.exp-darwin sigkill.stderr.exp-freebsd sigkill.stderr.exp-mips32 \
	    sigkill.stderr.exp-solaris \
	    sigkill.stderr.exp-glibc-2.28 sigkill.vgtest \
	signal2.stderr.exp signal2.stdout.exp signal2.vgtest \
	sigprocmask.stderr.exp sigprocmask.stderr.exp2 sigprocmask.vgtest \
	sized_delete.stderr.exp sized_delete.stderr.exp-x86 sized_delete.vgtest \
	static_malloc.stderr.exp static_malloc.vgtest \
	stpncpy.vgtest stpncpy.stderr.exp stpncpy.stdout.exp \
	strchr.stderr.exp strchr.stderr.exp2 strchr.stderr.exp3 strchr.vgtest \
	str_tester.stderr.exp str_tester.vgtest \
	supp-dir.vgtest supp-dir.stderr.exp \
	supp_unknown.stderr.exp supp_unknown.vgtest supp_unknown.supp \
	supp_unknown.stderr.exp-kfail \
	supp1.stderr.exp supp1.vgtest \
	supp2.stderr.exp supp2.vgtest \
	supp.supp \
	suppfree.stderr.exp suppfree.supp suppfree.vgtest \
	suppfreecollision.stderr.exp suppfreecollision.supp suppfreecollision.vgtest \
	supponlyobj.stderr.exp supponlyobj.supp supponlyobj.vgtest \
	suppsrc_lineno.stderr.exp suppsrc_lineno.stdout.exp suppsrc_lineno.vgtest \
	suppsrc_lineno.supp \
	suppsrc_sanlineno.stderr.exp suppsrc_sanlineno.stdout.exp suppsrc_sanlineno.vgtest \
	suppsrc_sanlineno.supp \
	suppvarinfo5.stderr.exp suppvarinfo5.supp suppvarinfo5.vgtest \
	test-plo-no.vgtest test-plo-no.stdout.exp \
	    test-plo-no.stderr.exp-le64 test-plo-no.stderr.exp-le32 \
	    test-plo-no.stderr.exp-le32-freebsd test-plo-no.stderr.exp-le64-freebsd \
	    test-plo-no.stderr.exp-le32-freebsd-clang \
	test-plo-yes.vgtest test-plo-yes.stdout.exp \
	    test-plo-yes.stderr.exp-le64 test-plo-yes.stderr.exp-le32 \
	    test-plo-no.stderr.exp-s390x-mvc \
	thread_alloca.stderr.exp thread_alloca.vgtest \
	threadname.vgtest threadname.stderr.exp \
	threadname_xml.vgtest threadname_xml.stderr.exp \
	trivialleak.stderr.exp trivialleak.vgtest trivialleak.stderr.exp2 \
	undef_malloc_args.stderr.exp undef_malloc_args.vgtest \
	unit_libcbase.stderr.exp unit_libcbase.vgtest \
	unit_oset.stderr.exp unit_oset.stdout.exp unit_oset.vgtest \
	varinfo1.vgtest varinfo1.stdout.exp varinfo1.stderr.exp \
		varinfo1.stderr.exp-ppc64 \
		varinfo1.stderr.exp-freebsd \
	varinfo2.vgtest varinfo2.stdout.exp varinfo2.stderr.exp \
		varinfo2.stderr.exp-ppc64 \
	varinfo3.vgtest varinfo3.stdout.exp varinfo3.stderr.exp \
		varinfo3.stderr.exp-ppc64 \
		varinfo3.stderr.exp-freebsd \
	varinfo4.vgtest varinfo4.stdout.exp varinfo4.stderr.exp \
		varinfo4.stderr.exp-ppc64 \
		varinfo4.stderr.exp-freebsd \
	varinfo5.vgtest varinfo5.stdout.exp varinfo5.stderr.exp \
		varinfo5.stderr.exp-ppc64 \
		varinfo5.stderr.exp-freebsd \
	varinfo6.vgtest varinfo6.stdout.exp varinfo6.stderr.exp \
		varinfo6.stderr.exp-ppc64 \
	varinforestrict.vgtest varinforestrict.stderr.exp \
	vcpu_bz2.stdout.exp vcpu_bz2.stderr.exp vcpu_bz2.vgtest \
	vcpu_fbench.stdout.exp vcpu_fbench.stderr.exp vcpu_fbench.vgtest \
	vcpu_fnfns.stdout.exp vcpu_fnfns.stdout.exp-glibc28-amd64 \
	vcpu_fnfns.stdout.exp-darwin vcpu_fnfns.stdout.exp-solaris \
	vcpu_fnfns.stderr.exp vcpu_fnfns.vgtest \
	wcs.vgtest wcs.stderr.exp wcs.stdout.exp \
	wcsncpy.vgtest wcsncpy.stderr.exp \
	wmemcmp.vgtest wmemcmp.stderr.exp \
	wrap1.vgtest wrap1.stdout.exp wrap1.stderr.exp \
	wrap2.vgtest wrap2.stdout.exp wrap2.stderr.exp \
	wrap3.vgtest wrap3.stdout.exp wrap3.stderr.exp \
	wrap4.vgtest wrap4.stdout.exp wrap4.stderr.exp \
	wrap5.vgtest wrap5.stdout.exp wrap5.stderr.exp \
	wrap6.vgtest wrap6.stdout.exp wrap6.stderr.exp \
	wrap7.vgtest wrap7.stdout.exp wrap7.stderr.exp \
	wrap8.vgtest wrap8.stdout.exp wrap8.stderr.exp \
	wrap8.stdout.exp-ppc64 wrap8.stderr.exp-ppc64 \
	wrapmalloc.vgtest wrapmalloc.stdout.exp wrapmalloc.stderr.exp \
	wrapmallocstatic.vgtest wrapmallocstatic.stdout.exp \
	wrapmallocstatic.stderr.exp \
	writev1.stderr.exp writev1.stderr.exp-solaris writev1.vgtest \
	xml1.stderr.exp xml1.stdout.exp xml1.vgtest xml1.stderr.exp-s390x-mvc \
	leak_cpp_interior.stderr.exp-freebsd leak_cpp_interior.stderr.exp-freebsd-32bit

check_PROGRAMS = \
	accounting \
	addressable \
	atomic_incs \
	badaddrvalue badfree badjump badjump2 \
	badloop \
	badpoll \
	badrw \
	big_blocks_freed_list \
	brk2 \
	buflen_check \
	bug155125 \
	bug287260 \
	bug340392 \
	bug401284 \
	bug464969_d_demangle \
	bug472219 \
	calloc-overflow \
	client-msg \
	clientperm \
	clireq_nofill \
	clo_redzone \
	cond_ld_st \
	descr_belowsp \
	leak_cpp_interior \
	custom_alloc \
	custom-overlap \
	demangle \
	demangle-rust \
	big_debuginfo_symbol \
	deep-backtrace \
	describe-block \
	doublefree error_counts errs1 exitprog execve1 execve2 erringfds \
	err_disable1 err_disable2 err_disable3 err_disable4 \
	err_disable_arange1 \
	file_locking \
	fprw fwrite inits inline inlinfo inltemplate \
	holey_buffer_too_small \
	leak-0 \
	leak-cases \
	leak-cycle \
	leak-delta \
	leak-pool \
	leak-autofreepool \
	leak-tree \
	leak-segv-jmp \
	long-supps \
	mallinfo \
	malloc_free_fill \
	malloc_usable malloc1 malloc2 malloc3 manuel1 manuel2 manuel3 \
	match-overrun \
	memalign_test  memcmptest mempool mempool2 mmaptest \
	memalign_args \
	memccpy1 memccpy2 \
	memmem \
	mismatches new_override metadata \
	nanoleak_supp nanoleak2 new_nothrow \
	noisy_child \
	null_socket \
	origin1-yes origin2-not-quite origin3-no \
	origin4-many origin5-bz2 origin6-fp \
	overlap \
	partiallydefinedeq \
	partial_load pdb-realloc pdb-realloc2 \
	pipe pointer-trace \
	posix_memalign \
	post-syscall \
	realloc_size_zero realloc_size_zero_mismatch \
	realloc_size_zero_again \
	realloc1 realloc2 realloc3 \
	recursive-merge \
	resvn_stack \
	sbfragment \
	sendmsg \
	sh-mem sh-mem-random \
	sigaltstack signal2 sigprocmask static_malloc sigkill \
	strchr \
	str_tester \
	supp_unknown supp1 supp2 suppfree \
	test-plo \
	trivialleak \
	thread_alloca \
	undef_malloc_args \
	unit_libcbase unit_oset \
	varinfo1 varinfo2 varinfo3 varinfo4 \
	varinfo5 varinfo5so.so varinfo6 \
	varinforestrict \
	vcpu_fbench vcpu_fnfns \
	wcs \
	xml1 \
	wmemcmp \
	wrap1 wrap2 wrap3 wrap4 wrap5 wrap6 wrap7 wrap7so.so wrap8 \
	wrapmalloc wrapmallocso.so wrapmallocstatic \
	writev1

if !SOLARIS_SUN_STUDIO_AS
# Sun Studio assembler fails on "IDENT too long"
check_PROGRAMS += long_namespace_xml
endif

if DWARF4
check_PROGRAMS += dw4
endif

bug464969_d_demangle_SOURCES = bug464969_d_demangle.cpp
bug464969_d_demangle_CXXFLAGS = $(AM_CXXFLAGS) @FLAG_W_NO_UNINITIALIZED@

if GZ_ZLIB
check_PROGRAMS += cdebug_zlib
cdebug_zlib_SOURCES = cdebug.c
cdebug_zlib_CFLAGS = $(AM_CFLAGS) -g -gz=zlib @FLAG_W_NO_UNINITIALIZED@
endif

if GZ_ZLIB_GNU
check_PROGRAMS += cdebug_zlib_gnu
cdebug_zlib_gnu_SOURCES = cdebug.c
cdebug_zlib_gnu_CFLAGS = $(AM_CFLAGS) -g -gz=zlib-gnu @FLAG_W_NO_UNINITIALIZED@
endif

if HAVE_ALIGNED_CXX_ALLOC
check_PROGRAMS += cxx17_aligned_new sized_aligned_new_delete_args \
	new_aligned_delete_default \
	sized_aligned_new_delete_misaligned1 \
	sized_aligned_new_delete_misaligned2 \
	sized_aligned_new_delete_misaligned3
endif

if HAVE_PTHREAD_BARRIER
check_PROGRAMS += reach_thread_register
endif

if HAVE_FSIZED_DEALLOCATION
check_PROGRAMS += sized_delete new_delete_mismatch_size
endif

if HAVE_GNU_STPNCPY
check_PROGRAMS += stpncpy
endif

if HAVE_PTHREAD_SETNAME_NP
check_PROGRAMS += threadname 
endif

# are there still pre-C99 C compilers?
if HAVE_WCSNCPY
check_PROGRAMS += wcsncpy
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

if VGCONF_PLATFORMS_INCLUDE_ARM_LINUX
#AM_CFLAGS   += -mfloat-abi=softfp
#AM_CXXFLAGS += -mfloat-abi=softfp
endif

if VGCONF_OS_IS_DARWIN
atomic_incs_CFLAGS = $(AM_CFLAGS) -mdynamic-no-pic
else
atomic_incs_CFLAGS = $(AM_CFLAGS)
endif

badpoll_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_STRINGOP_OVERFLOW@
badrw_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
big_blocks_freed_list_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@

if VGCONF_OS_IS_SOLARIS
buflen_check_LDADD	= -lsocket -lnsl
endif

leak_cpp_interior_SOURCES	= leak_cpp_interior.cpp

# Suppress various gcc warnings which are correct, but for things
# we are actually testing for at runtime.
accounting_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_ALLOC_SIZE_LARGER_THAN@
badfree_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_FREE_NONHEAP_OBJECT@
bug155125_CFLAGS	= $(AM_CFLAGS) -Wno-unused-result @FLAG_W_NO_ALLOC_SIZE_LARGER_THAN@
bug472219_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
mallinfo_CFLAGS		= $(AM_CFLAGS) -Wno-deprecated-declarations
malloc3_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_ALLOC_SIZE_LARGER_THAN@
sbfragment_CFLAGS	= $(AM_CFLAGS) -Wno-deprecated-declarations
strchr_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

big_debuginfo_symbol_SOURCES = big_debuginfo_symbol.cpp
big_debuginfo_symbol_CXXFLAGS = $(AM_CXXFLAGS) -std=c++0x

bug340392_CFLAGS        = $(AM_CFLAGS) -O3 @FLAG_W_NO_MAYBE_UNINITIALIZED@

if HAVE_ALIGNED_CXX_ALLOC
cxx17_aligned_new_SOURCES  = cxx17_aligned_new.cpp
cxx17_aligned_new_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17 @FLAG_W_NO_MISMATCHED_NEW_DELETE@
new_aligned_delete_default_SOURCES = new_aligned_delete_default.cpp
new_aligned_delete_default_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17
sized_aligned_new_delete_args_SOURCES = sized_aligned_new_delete_args.cpp
sized_aligned_new_delete_args_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17
sized_aligned_new_delete_misaligned1_SOURCES = sized_aligned_new_delete_misaligned1.cpp
sized_aligned_new_delete_misaligned1_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17
sized_aligned_new_delete_misaligned2_SOURCES = sized_aligned_new_delete_misaligned2.cpp
sized_aligned_new_delete_misaligned2_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17 @FLAG_W_NO_UNUSED_VARIABLE@
sized_aligned_new_delete_misaligned3_SOURCES = sized_aligned_new_delete_misaligned3.cpp
sized_aligned_new_delete_misaligned3_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17 @FLAG_W_NO_UNUSED_VARIABLE@
if COMPILER_IS_CLANG
cxx17_aligned_new_CXXFLAGS += -fsized-deallocation
sized_aligned_new_delete_args_CXXFLAGS += -fsized-deallocation
sized_aligned_new_delete_misaligned1_CXXFLAGS += -fsized-deallocation
sized_aligned_new_delete_misaligned2_CXXFLAGS += -fsized-deallocation
sized_aligned_new_delete_misaligned3_CXXFLAGS += -fsized-deallocation
endif
endif

demangle_SOURCES = demangle.cpp
demangle_rust_SOURCES = demangle-rust.c
demangle_rust_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@
dw4_CFLAGS		= $(AM_CFLAGS) -gdwarf-4 -fdebug-types-section

descr_belowsp_LDADD     = -lpthread
err_disable3_LDADD 	= -lpthread
err_disable4_LDADD 	= -lpthread
err_disable4_CFLAGS	= $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
err_disable4_CFLAGS	+= -D_XOPEN_SOURCE=600
endif

error_counts_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@ @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@

execve1_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

execve2_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

fprw_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@ @FLAG_W_NO_FREE_NONHEAP_OBJECT@ @FLAG_W_NO_USE_AFTER_FREE@

inits_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

inlinfo_CFLAGS = $(AM_CFLAGS) -w

inltemplate_SOURCES = inltemplate.cpp
inltemplate_CXXFLAGS = $(AM_CXXFLAGS) @FLAG_W_NO_UNINITIALIZED@

long_namespace_xml_SOURCES = long_namespace_xml.cpp

malloc_free_fill_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@
malloc1_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@
manuel1_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
manuel3_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

memalign2_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_NON_POWER_OF_TWO_ALIGNMENT@
if VGCONF_OS_IS_SOLARIS
memalign2_CFLAGS	+= -D__EXTENSIONS__
endif

memcmptest_CFLAGS	= $(AM_CFLAGS) -fno-builtin-memcmp

mismatches_SOURCES	= mismatches.cpp
mismatches_CXXFLAGS	= $(AM_CXXFLAGS) @FLAG_W_NO_MISMATCHED_NEW_DELETE@

if HAVE_FSIZED_DEALLOCATION
new_delete_mismatch_size_SOURCES   = new_delete_mismatch_size.cpp
new_delete_mismatch_size_CXXFLAGS  = $(AM_CXXFLAGS) @FLAG_FSIZED_DEALLOCATION@ -std=c++17
endif

new_nothrow_SOURCES 	= new_nothrow.cpp
new_override_SOURCES 	= new_override.cpp

noisy_child_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@

if VGCONF_OS_IS_SOLARIS
null_socket_LDADD	= -lsocket -lnsl
endif

origin1_yes_CFLAGS      = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

origin2_not_quite_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

origin3_no_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

# This requires optimisation in order to get just one resulting error.
origin4_many_CFLAGS	= $(AM_CFLAGS) -O @FLAG_W_NO_USE_AFTER_FREE@ @FLAG_W_NO_UNINITIALIZED@

# Apply -O so as to run in reasonable time.
origin5_bz2_CFLAGS	= $(AM_CFLAGS) -O -Wno-inline
origin6_fp_CFLAGS	= $(AM_CFLAGS) -O

# Don't allow GCC to inline memcpy() and strcpy(),
# because then we can't intercept it
overlap_CFLAGS		= $(AM_CFLAGS) -fno-builtin-memcpy -fno-builtin-strcpy \
			  @FLAG_W_NO_FORTIFY_SOURCE@ @FLAG_W_NO_STRINGOP_OVERFLOW@ @FLAG_W_NO_BUILTIN_MEMCPY_CHK_SIZE@

partial_load_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@
reach_thread_register_CFLAGS	= $(AM_CFLAGS) -O2
reach_thread_register_LDADD	= -lpthread

realloc_size_zero_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_INCOMPATIBLE_POINTER_TYPES_DISCARDS_QUALIFIERS@
realloc_size_zero_mismatch_SOURCES = realloc_size_zero_mismatch.cpp
realloc_size_zero_mismatch_CXXFLAGS = $(AM_CXXFLAGS) @FLAG_W_NO_MISMATCHED_NEW_DELETE@

resvn_stack_CFLAGS      = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

sendmsg_CFLAGS		= $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
sendmsg_CFLAGS		+= -D_XOPEN_SOURCE=600
sendmsg_LDADD		= -lsocket -lnsl
endif

if HAVE_FSIZED_DEALLOCATION
sized_delete_SOURCES   = sized_delete.cpp
sized_delete_CXXFLAGS  = $(AM_CXXFLAGS) @FLAG_FSIZED_DEALLOCATION@
endif

str_tester_CFLAGS	= $(AM_CFLAGS) -Wno-shadow @FLAG_W_NO_STRINGOP_OVERFLOW@ \
			  @FLAG_W_NO_STRINGOP_TRUNCATION@ \
			  @FLAG_W_NO_FORTIFY_SOURCE@ @FLAG_W_NO_SUSPICIOUS_BZERO@ \
			  @FLAG_W_NO_MEMSET_TRANSPOSED_ARGS@ @FLAG_W_NO_STRINGOP_OVERREAD@ \
			  @FLAG_W_NO_BUILTIN_MEMCPY_CHK_SIZE@

supp_CFLAGS             = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

supp_unknown_SOURCES	= badjump.c
supp_unknown_CFLAGS     = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

supp1_SOURCES		= supp.c
supp1_CFLAGS            = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

supp2_SOURCES		= supp.c
supp2_CFLAGS            = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

suppfree_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@
test_plo_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_USE_AFTER_FREE@

thread_alloca_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_FORMAT_OVERFLOW@
thread_alloca_LDADD     = -lpthread
threadname_LDADD 	= -lpthread

vcpu_bz2_CFLAGS		= $(AM_CFLAGS) -O2
vcpu_fbench_CFLAGS	= $(AM_CFLAGS) -O2
vcpu_fnfns_CFLAGS	= $(AM_CFLAGS) -O2
vcpu_fnfns_LDADD	= -lm
wrap6_CFLAGS		= $(AM_CFLAGS) -O2

# To make it a bit more realistic, have some optimisation enabled
# for the varinfo tests.  We still expect sane results.
varinfo1_CFLAGS		= $(AM_CFLAGS) -O @FLAG_W_NO_MAYBE_UNINITIALIZED@
varinfo2_CFLAGS		= $(AM_CFLAGS) -O -Wno-shadow @FLAG_W_NO_MAYBE_UNINITIALIZED@
varinfo3_CFLAGS		= $(AM_CFLAGS) -O @FLAG_W_NO_MAYBE_UNINITIALIZED@
varinfo4_CFLAGS		= $(AM_CFLAGS) -O @FLAG_W_NO_MAYBE_UNINITIALIZED@
varinfo5_CFLAGS		= $(AM_CFLAGS) -O
varinfo6_CFLAGS		= $(AM_CFLAGS) -O @FLAG_W_NO_MAYBE_UNINITIALIZED@

# Build shared object for varinfo5
varinfo5_SOURCES        = varinfo5.c
varinfo5_DEPENDENCIES   = varinfo5so.so
if VGCONF_OS_IS_DARWIN
 varinfo5_LDADD         = `pwd`/varinfo5so.so
 varinfo5_LDFLAGS       = $(AM_FLAG_M3264_PRI)
else
 varinfo5_LDADD         = varinfo5so.so
 varinfo5_LDFLAGS       = $(AM_FLAG_M3264_PRI) \
				-Wl,-rpath,$(top_builddir)/memcheck/tests
endif

varinfo5so_so_SOURCES   = varinfo5so.c
varinfo5so_so_CFLAGS    = $(AM_CFLAGS) -fpic -O -Wno-shadow @FLAG_W_NO_MAYBE_UNINITIALIZED@
if VGCONF_OS_IS_DARWIN
 varinfo5so_so_LDFLAGS  = -fpic $(AM_FLAG_M3264_PRI) -dynamic \
				-dynamiclib -all_load
else
 varinfo5so_so_LDFLAGS  = -fpic $(AM_FLAG_M3264_PRI) -shared \
				-Wl,-soname -Wl,varinfo5so.so
endif
varinforestrict_CFLAGS	= $(AM_CFLAGS) -O0 -g @FLAG_W_NO_MAYBE_UNINITIALIZED@

# Build shared object for wrap7
wrap7_SOURCES           = wrap7.c
wrap7_DEPENDENCIES      = wrap7so.so
if VGCONF_OS_IS_DARWIN
 wrap7_LDADD            = `pwd`/wrap7so.so
 wrap7_LDFLAGS          = $(AM_FLAG_M3264_PRI)
else
 wrap7_LDADD            = wrap7so.so
 wrap7_LDFLAGS          = $(AM_FLAG_M3264_PRI) \
				-Wl,-rpath,$(top_builddir)/memcheck/tests
endif

wrap7so_so_SOURCES      = wrap7so.c
wrap7so_so_CFLAGS       = $(AM_CFLAGS) -fpic
if VGCONF_OS_IS_DARWIN
 wrap7so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -dynamic \
				-dynamiclib -all_load
else
 wrap7so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared \
				-Wl,-soname -Wl,wrap7so.so
endif

# Build shared object for wrapmalloc
wrapmalloc_SOURCES           = wrapmalloc.c
wrapmalloc_DEPENDENCIES      = wrapmallocso.so
if VGCONF_OS_IS_DARWIN
 wrapmalloc_LDADD            = `pwd`/wrapmallocso.so
 wrapmalloc_LDFLAGS          = $(AM_FLAG_M3264_PRI)
else
 wrapmalloc_LDADD            = wrapmallocso.so
 wrapmalloc_LDFLAGS          = $(AM_FLAG_M3264_PRI) \
				-Wl,-rpath,$(top_builddir)/memcheck/tests
endif

wrapmallocso_so_SOURCES      = wrapmallocso.c
wrapmallocso_so_CFLAGS       = $(AM_CFLAGS) -fpic
if VGCONF_OS_IS_DARWIN
 wrapmallocso_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -dynamic \
				-dynamiclib -all_load
else
 wrapmallocso_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared \
				-Wl,-soname -Wl,wrapmallocso.so
endif

writev1_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_STRINGOP_OVERFLOW@ @FLAG_W_NO_STRINGOP_OVERREAD@
xml1_CFLAGS             = $(AM_CFLAGS) -D_GNU_SOURCE @FLAG_W_NO_UNINITIALIZED@ @FLAG_W_NO_USE_AFTER_FREE@

