# Valgrind for macOS ![Build](https://github.com/LouisBrunner/valgrind-macos/workflows/Build/badge.svg)

This repository contains a version of Valgrind including a few patches to improve support for the macOS platform. It is maintained by [Louis Brunner](https://github.com/LouisBrunner).

## Status

Valgrind now builds and works on every macOS version

Note that some features are still in progress:

- crash when using wqthread (used in certain UI frameworks)
- using threads and signals is undefined

It is currently tested on 10.14.6 and 10.15.4.

Checkout the [`patches`](https://github.com/LouisBrunner/valgrind-macos/commits/patches) branch for a list of patches that can be directly applied to the upstream Valgrind.

### macOS 11 and later

Due to changes on how

### Apple Silicon

There is currently no easy way to get Valgrind working on arm64 due to difference on how the XNU kernel treat arm64 and amd64 binaries.

## Usage

In case you already have Valgrind installed, you might need to `brew remove` it first.

In order to use this version, first tap this repository:

```sh
brew tap LouisBrunner/valgrind
```

Then, install `valgrind`:

```sh
brew install --HEAD LouisBrunner/valgrind/valgrind
```

You can now use `valgrind` as normal.

Note: in case of failures during the build, [make sure you have the latest Xcode/CLI tools installed](https://github.com/LouisBrunner/valgrind-macos/issues/6#issuecomment-667587385).

### Update

Any `brew upgrade` will now correctly rebuild the latest `LouisBrunner/valgrind` instead of the upstream one (which doesn't support latest macOS versions).

```sh
brew upgrade --fetch-HEAD LouisBrunner/valgrind/valgrind
```

## TODO

- pthread and signals blocking (re-enable tests) [patch in progess]
- wqthread broken (see #4) [patch in progress]
- drd thread related crash on 10.15
- `-UNHANDLED` messages
- Run regtest in parallel [patch in progess]
- macOS 11 and later leak tracking [patch in progess]
- Apple Silicon support [on hold]

## Tests

Some tests are blocking and were therefore disabled on macOS:

- `none/tests/pselect_alarm`
- `none/tests/pth_term_signal`

### Linux (Ubuntu 18.04)

These errors seem to come from the CI environment itself (as they show with or without my changes).

```
== 719 tests, 18 stderr failures, 0 stdout failures, 0 stderrB failures, 0 stdoutB failures, 2 post failures ==
memcheck/tests/leak_cpp_interior         (stderr)
memcheck/tests/linux/sys-preadv2_pwritev2 (stderr)
memcheck/tests/overlap                   (stderr)
helgrind/tests/tc18_semabuse             (stderr)
helgrind/tests/tc20_verifywrap           (stderr)
drd/tests/bar_bad                        (stderr) // non-deterministic
drd/tests/tc18_semabuse                  (stderr)
massif/tests/new-cpp                     (post)
massif/tests/overloaded-new              (post)
none/tests/fdleak_cmsg                   (stderr)
none/tests/fdleak_creat                  (stderr)
none/tests/fdleak_dup                    (stderr)
none/tests/fdleak_dup2                   (stderr)
none/tests/fdleak_fcntl                  (stderr)
none/tests/fdleak_ipv4                   (stderr)
none/tests/fdleak_open                   (stderr)
none/tests/fdleak_pipe                   (stderr)
none/tests/fdleak_socketpair             (stderr)
none/tests/rlimit64_nofile               (stderr)
none/tests/rlimit_nofile                 (stderr)
```

should be (according to the official builds)

```
== 789 tests, 1 stderr failure, 0 stdout failures, 0 stderrB failures, 0 stdoutB failures, 0 post failures ==
memcheck/tests/linux/sys-preadv2_pwritev2 (stderr)
```

### macOS (10.14.4)

```
OUTDATED
```

### macOS (10.15.1)

```
== 603 tests, 266 stderr failures, 14 stdout failures, 0 stderrB failures, 0 stdoutB failures, 32 post failures ==
memcheck/tests/accounting                (stderr)
memcheck/tests/addressable               (stderr)
memcheck/tests/amd64/insn-pmovmskb       (stderr)
memcheck/tests/amd64/sh-mem-vec128-plo-no (stderr)
memcheck/tests/amd64/sh-mem-vec128-plo-yes (stderr)
memcheck/tests/amd64/sh-mem-vec256-plo-no (stderr)
memcheck/tests/amd64/sh-mem-vec256-plo-yes (stderr)
memcheck/tests/badjump2                  (stderr)
memcheck/tests/big_blocks_freed_list     (stderr)
memcheck/tests/bug155125                 (stderr)
memcheck/tests/bug287260                 (stderr)
memcheck/tests/cdebug_zlib               (stderr)
memcheck/tests/cdebug_zlib_gnu           (stderr)
memcheck/tests/client-msg-as-xml         (stderr)
memcheck/tests/client-msg                (stderr)
memcheck/tests/clientperm                (stderr)
memcheck/tests/darwin/deep_badparam      (stdout)
memcheck/tests/darwin/pth-supp           (stderr)
memcheck/tests/darwin/scalar             (stderr)
memcheck/tests/darwin/scalar_nocancel    (stderr)
memcheck/tests/deep-backtrace            (stderr)
memcheck/tests/demangle                  (stderr)
memcheck/tests/descr_belowsp             (stderr)
memcheck/tests/fprw                      (stderr)
memcheck/tests/gone_abrt_xml             (stderr)
memcheck/tests/inlinfo                   (stderr)
memcheck/tests/inlinfosupp               (stderr)
memcheck/tests/inlinfosuppobj            (stderr)
memcheck/tests/leak-autofreepool-0       (stderr)
memcheck/tests/leak-autofreepool-1       (stderr)
memcheck/tests/leak-autofreepool-2       (stderr)
memcheck/tests/leak-autofreepool-4       (stderr)
memcheck/tests/leak-autofreepool-5       (stderr)
memcheck/tests/leak-autofreepool-6       (stderr)
memcheck/tests/leak-cases-full           (stderr)
memcheck/tests/leak-cases-summary        (stderr)
memcheck/tests/leak-delta                (stderr)
memcheck/tests/leak_cpp_interior         (stderr)
memcheck/tests/lks                       (stderr)
memcheck/tests/manuel1                   (stderr)
memcheck/tests/memalign_test             (stderr)
memcheck/tests/memcmptest                (stderr)
memcheck/tests/mismatches                (stderr)
memcheck/tests/origin1-yes               (stderr)
memcheck/tests/origin2-not-quite         (stderr)
memcheck/tests/origin3-no                (stderr)
memcheck/tests/origin4-many              (stderr)
memcheck/tests/origin5-bz2               (stderr)
memcheck/tests/origin6-fp                (stderr)
memcheck/tests/overlap                   (stderr)
memcheck/tests/post-syscall              (stderr)
memcheck/tests/sem                       (stderr)
memcheck/tests/static_malloc             (stderr)
memcheck/tests/strchr                    (stderr)
memcheck/tests/supp1                     (stderr)
memcheck/tests/supp2                     (stderr)
memcheck/tests/supp_unknown              (stderr)
memcheck/tests/supponlyobj               (stderr)
memcheck/tests/suppvarinfo5              (stderr)
memcheck/tests/test-plo-no               (stderr)
memcheck/tests/thread_alloca             (stderr)
memcheck/tests/varinfo1                  (stderr)
memcheck/tests/varinfo2                  (stderr)
memcheck/tests/varinfo3                  (stderr)
memcheck/tests/varinfo4                  (stderr)
memcheck/tests/varinfo5                  (stderr)
memcheck/tests/varinfo6                  (stderr)
memcheck/tests/wrap6                     (stdout)
memcheck/tests/wrapmalloc                (stdout)
memcheck/tests/wrapmallocstatic          (stdout)
memcheck/tests/xml1                      (stderr)
helgrind/tests/annotate_hbefore          (stderr)
helgrind/tests/annotate_rwlock           (stderr)
helgrind/tests/annotate_smart_pointer    (stderr)
helgrind/tests/bug322621                 (stderr)
helgrind/tests/cond_timedwait_invalid    (stderr)
helgrind/tests/free_is_write             (stderr)
helgrind/tests/hg01_all_ok               (stderr)
helgrind/tests/hg02_deadlock             (stderr)
helgrind/tests/hg03_inherit              (stderr)
helgrind/tests/hg04_race                 (stderr)
helgrind/tests/hg05_race2                (stderr)
helgrind/tests/hg06_readshared           (stderr)
helgrind/tests/locked_vs_unlocked1_fwd   (stderr)
helgrind/tests/locked_vs_unlocked1_rev   (stderr)
helgrind/tests/locked_vs_unlocked2       (stderr)
helgrind/tests/locked_vs_unlocked3       (stderr)
helgrind/tests/pth_destroy_cond          (stderr)
helgrind/tests/rwlock_race               (stderr)
helgrind/tests/rwlock_test               (stderr)
helgrind/tests/stackteardown             (stderr)
helgrind/tests/tc01_simple_race          (stderr)
helgrind/tests/tc02_simple_tls           (stderr)
helgrind/tests/tc03_re_excl              (stderr)
helgrind/tests/tc04_free_lock            (stderr)
helgrind/tests/tc05_simple_race          (stderr)
helgrind/tests/tc06_two_races            (stderr)
helgrind/tests/tc06_two_races_xml        (stderr)
helgrind/tests/tc07_hbl1                 (stderr)
helgrind/tests/tc08_hbl2                 (stderr)
helgrind/tests/tc09_bad_unlock           (stderr)
helgrind/tests/tc10_rec_lock             (stderr)
helgrind/tests/tc11_XCHG                 (stderr)
helgrind/tests/tc12_rwl_trivial          (stderr)
helgrind/tests/tc13_laog1                (stderr)
helgrind/tests/tc14_laog_dinphils        (stderr)
helgrind/tests/tc15_laog_lockdel         (stderr)
helgrind/tests/tc16_byterace             (stderr)
helgrind/tests/tc17_sembar               (stderr)
helgrind/tests/tc18_semabuse             (stderr)
helgrind/tests/tc19_shadowmem            (stderr)
helgrind/tests/tc21_pthonce              (stderr)
helgrind/tests/tc22_exit_w_lock          (stderr)
helgrind/tests/tc23_bogus_condwait       (stderr)
helgrind/tests/tc24_nonzero_sem          (stderr)
drd/tests/annotate_barrier               (stderr)
drd/tests/annotate_barrier_xml           (stderr)
drd/tests/annotate_hb_err                (stderr)
drd/tests/annotate_hb_race               (stderr)
drd/tests/annotate_hbefore               (stderr)
drd/tests/annotate_ignore_read           (stderr)
drd/tests/annotate_ignore_rw             (stderr)
drd/tests/annotate_ignore_rw2            (stderr)
drd/tests/annotate_ignore_write          (stderr)
drd/tests/annotate_ignore_write2         (stderr)
drd/tests/annotate_order_1               (stderr)
drd/tests/annotate_order_2               (stderr)
drd/tests/annotate_order_3               (stderr)
drd/tests/annotate_publish_hg            (stderr)
drd/tests/annotate_rwlock                (stderr)
drd/tests/annotate_rwlock_hg             (stderr)
drd/tests/annotate_sem                   (stderr)
drd/tests/annotate_smart_pointer         (stderr)
drd/tests/annotate_smart_pointer2        (stderr)
drd/tests/annotate_spinlock              (stderr)
drd/tests/annotate_static                (stderr)
drd/tests/annotate_trace_memory          (stderr)
drd/tests/annotate_trace_memory_xml      (stderr)
drd/tests/atomic_var                     (stderr)
drd/tests/bug-235681                     (stderr)
drd/tests/circular_buffer                (stderr)
drd/tests/concurrent_close               (stderr)
drd/tests/custom_alloc                   (stderr)
drd/tests/custom_alloc_fiw               (stderr)
drd/tests/dlopen                         (stdout)
drd/tests/dlopen                         (stderr)
drd/tests/fork-parallel                  (stderr)
drd/tests/fork-serial                    (stderr)
drd/tests/fp_race                        (stderr)
drd/tests/fp_race2                       (stderr)
drd/tests/fp_race_xml                    (stderr)
drd/tests/free_is_write                  (stderr)
drd/tests/free_is_write2                 (stderr)
drd/tests/hg01_all_ok                    (stderr)
drd/tests/hg02_deadlock                  (stderr)
drd/tests/hg03_inherit                   (stderr)
drd/tests/hg04_race                      (stderr)
drd/tests/hg05_race2                     (stderr)
drd/tests/hg06_readshared                (stderr)
drd/tests/hold_lock_1                    (stderr)
drd/tests/hold_lock_2                    (stderr)
drd/tests/linuxthreads_det               (stderr)
drd/tests/memory_allocation              (stderr)
drd/tests/monitor_example                (stderr)
drd/tests/new_delete                     (stderr)
drd/tests/pth_broadcast                  (stderr)
drd/tests/pth_cancel_locked              (stderr)
drd/tests/pth_cleanup_handler            (stderr)
drd/tests/pth_cond_destroy_busy          (stderr)
drd/tests/pth_cond_race                  (stderr)
drd/tests/pth_cond_race2                 (stderr)
drd/tests/pth_cond_race3                 (stderr)
drd/tests/pth_create_chain               (stderr)
drd/tests/pth_detached                   (stderr)
drd/tests/pth_detached2                  (stderr)
drd/tests/pth_detached3                  (stderr)
drd/tests/pth_inconsistent_cond_wait     (stderr)
drd/tests/pth_mutex_reinit               (stderr)
drd/tests/pth_once                       (stderr)
drd/tests/pth_process_shared_mutex       (stderr)
drd/tests/pth_uninitialized_cond         (stderr)
drd/tests/read_and_free_race             (stderr)
drd/tests/recursive_mutex                (stderr)
drd/tests/rwlock_race                    (stderr)
drd/tests/rwlock_test                    (stderr)
drd/tests/rwlock_type_checking           (stderr)
drd/tests/sem_open                       (stderr)
drd/tests/sem_open2                      (stderr)
drd/tests/sem_open3                      (stderr)
drd/tests/sem_open_traced                (stderr)
drd/tests/sigalrm                        (stderr)
drd/tests/sigaltstack                    (stderr)
drd/tests/str_tester                     (stderr)
drd/tests/tc01_simple_race               (stderr)
drd/tests/tc02_simple_tls                (stderr)
drd/tests/tc03_re_excl                   (stderr)
drd/tests/tc04_free_lock                 (stderr)
drd/tests/tc05_simple_race               (stderr)
drd/tests/tc06_two_races                 (stderr)
drd/tests/tc07_hbl1                      (stdout)
drd/tests/tc07_hbl1                      (stderr)
drd/tests/tc08_hbl2                      (stdout)
drd/tests/tc08_hbl2                      (stderr)
drd/tests/tc09_bad_unlock                (stderr)
drd/tests/tc10_rec_lock                  (stderr)
drd/tests/tc11_XCHG                      (stdout)
drd/tests/tc11_XCHG                      (stderr)
drd/tests/tc12_rwl_trivial               (stderr)
drd/tests/tc13_laog1                     (stderr)
drd/tests/tc15_laog_lockdel              (stderr)
drd/tests/tc16_byterace                  (stderr)
drd/tests/tc17_sembar                    (stderr)
drd/tests/tc19_shadowmem                 (stderr)
drd/tests/tc21_pthonce                   (stdout)
drd/tests/tc21_pthonce                   (stderr)
drd/tests/tc22_exit_w_lock               (stderr)
drd/tests/tc23_bogus_condwait            (stderr)
drd/tests/threaded-fork-vcs              (stderr)
drd/tests/threaded-fork                  (stderr)
drd/tests/tls_threads                    (stderr)
drd/tests/trylock                        (stderr)
drd/tests/unit_bitmap                    (stderr)
drd/tests/unit_vc                        (stderr)
massif/tests/alloc-fns-A                 (post)
massif/tests/alloc-fns-B                 (post)
massif/tests/basic                       (post)
massif/tests/basic2                      (post)
massif/tests/big-alloc                   (post)
massif/tests/culling1                    (stderr)
massif/tests/culling2                    (stderr)
massif/tests/custom_alloc                (post)
massif/tests/deep-A                      (post)
massif/tests/deep-B                      (stderr)
massif/tests/deep-B                      (post)
massif/tests/deep-C                      (stderr)
massif/tests/deep-C                      (post)
massif/tests/deep-D                      (post)
massif/tests/ignored                     (post)
massif/tests/ignoring                    (post)
massif/tests/inlinfomalloc               (post)
massif/tests/insig                       (post)
massif/tests/long-names                  (post)
massif/tests/long-time                   (post)
massif/tests/mmapunmap                   (post)
massif/tests/new-cpp                     (post)
massif/tests/null                        (post)
massif/tests/one                         (post)
massif/tests/overloaded-new              (post)
massif/tests/pages_as_heap               (stdout)
massif/tests/pages_as_heap               (stderr)
massif/tests/peak                        (post)
massif/tests/peak2                       (stderr)
massif/tests/peak2                       (post)
massif/tests/realloc                     (stderr)
massif/tests/realloc                     (post)
massif/tests/thresholds_0_0              (post)
massif/tests/thresholds_0_10             (post)
massif/tests/thresholds_10_0             (post)
massif/tests/thresholds_10_10            (post)
massif/tests/thresholds_5_0              (post)
massif/tests/thresholds_5_10             (post)
massif/tests/zero1                       (post)
massif/tests/zero2                       (post)
dhat/tests/acc                           (stderr)
dhat/tests/basic                         (stderr)
dhat/tests/big                           (stderr)
dhat/tests/empty                         (stderr)
dhat/tests/sig                           (stderr)
dhat/tests/single                        (stderr)
none/tests/allexec32                     (stdout) // non-deterministic
none/tests/allexec64                     (stdout) // non-deterministic
none/tests/amd64/sse4-64                 (stdout)
none/tests/async-sigs                    (stderr)
none/tests/bug234814                     (stdout)
none/tests/bug234814                     (stderr)
none/tests/coolo_sigaction               (stdout)
none/tests/darwin/rlimit                 (stderr)
none/tests/empty-exe                     (stderr)
none/tests/faultstatus                   (stderr)
none/tests/fdleak_cmsg                   (stderr)
none/tests/fdleak_creat                  (stderr)
none/tests/fdleak_dup                    (stderr)
none/tests/fdleak_dup2                   (stderr)
none/tests/fdleak_fcntl                  (stderr)
none/tests/fdleak_ipv4                   (stderr)
none/tests/fdleak_open                   (stderr)
none/tests/fdleak_pipe                   (stderr)
none/tests/fdleak_socketpair             (stderr)
none/tests/ioctl_moans                   (stderr)
none/tests/mmap_fcntl_bug                (stderr)
none/tests/nocwd                         (stdout)
none/tests/nocwd                         (stderr)
none/tests/pth_2sig                      (stderr) // non-deterministic
none/tests/pth_cancel1                   (stderr)
none/tests/pth_cancel2                   (stderr)
none/tests/require-text-symbol-2         (stderr)
none/tests/rlimit_nofile                 (stderr)
none/tests/scripts/shell                 (stderr)
none/tests/syscall-restart1              (stderr)
none/tests/syslog                        (stderr)
```
