# Valgrind for macOS ![Build](https://github.com/LouisBrunner/valgrind-macos/workflows/Build/badge.svg)

This repository contains a version of Valgrind including a few patches to improve support for the macOS platform. It is maintained by [Louis Brunner](https://github.com/LouisBrunner).

## Status

| Version                     | x86 | amd64 | arm64 | ppc    |
| --------------------------- | --- | ----- | ----- | ------ |
| macOS 10.13 and earlier[^1] | ✅  | ✅    | -     | ❌[^3] |
| macOS 10.14 (Mojave)        | ✅  | ✅    | -     | -      |
| macOS 10.15 (Catalina)      | ✅  | ✅    | -     | -      |
| macOS 11 (Big Sur)          | -   | ✅    | ✅    | -      |
| macOS 12 (Monterey)         | -   | ✅    | ✅    | -      |
| macOS 13 (Ventura)          | -   | ✅    | ✅    | -      |
| macOS 14 (Sonoma)           | -   | ✅    | ✅    | -      |
| macOS 15 (Sequoia)          | -   | ✅    | ~[^2] | -      |
| macOS 26 (Tahoe)            | -   | ✅    | ✅    | -      |

[^1]: Supported as part of upstream Valgrind.
[^2]: macOS 15 arm64 is experimental ([#123](https://github.com/LouisBrunner/valgrind-macos/issues/123))
[^3]: PowerPC is unsupported ([#62](https://github.com/LouisBrunner/valgrind-macos/issues/62))

Note that every version from macOS 10.12 onwards currently has the following issues:

- using threads and signals together is undefined (crashes, hanging, etc), note: a few tests were disabled because of that
- drd crashes on 10.15 (probably onwards)

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

It is possible that Homebrew shows you the following error message afterwards:

```bash
error: Invalid usage: --HEAD is not supported with HOMEBREW_NO_INSTALL_FROM_API unset! To resolve please run:
  export HOMEBREW_NO_INSTALL_FROM_API=1
  brew tap Homebrew/core
and retry this command.
```

If so, just execute both commands and retry the installation as mentioned above.

You can now use `valgrind` as normal.

Note: in case of failures during the build, [make sure you have the latest Xcode/CLI tools installed](https://github.com/LouisBrunner/valgrind-macos/issues/6#issuecomment-667587385).

### Update

Any `brew upgrade` will now correctly rebuild the latest `LouisBrunner/valgrind` instead of the upstream one (which doesn't support the latest macOS versions).

```sh
brew upgrade --fetch-HEAD LouisBrunner/valgrind/valgrind
```

## Tests

Some tests are hanging and were therefore disabled on macOS:

- `none/tests/pselect_alarm` (amd64)
- `none/tests/pth_term_signal` (amd64 & arm64)
- `memcheck/tests/sigaltstack` (arm64)
- `memcheck/tests/thread_alloca` (amd64)

### Linux (Ubuntu 24.04)

These errors seem to come from the CI environment itself (as they show with or without my changes).

```
== 857 tests, 20 stderr failures, 1 stdout failure, 0 stderrB failures, 0 stdoutB failures, 0 post failures ==
none/tests/fdleak_cmsg                   (stderr)
none/tests/fdleak_cmsg_supp              (stderr)
none/tests/fdleak_cmsg_xml               (stderr)
none/tests/fdleak_creat                  (stderr)
none/tests/fdleak_creat_sup              (stderr)
none/tests/fdleak_creat_xml              (stderr)
none/tests/fdleak_dup                    (stderr)
none/tests/fdleak_dup2                   (stderr)
none/tests/fdleak_dup2_xml               (stderr)
none/tests/fdleak_dup_xml                (stderr)
none/tests/fdleak_fcntl                  (stderr)
none/tests/fdleak_fcntl_xml              (stderr)
none/tests/fdleak_ipv4                   (stderr)
none/tests/fdleak_open                   (stderr)
none/tests/fdleak_pipe                   (stderr)
none/tests/fdleak_socketpair             (stderr)
none/tests/linux/getdents_filter         (stdout)
none/tests/rlimit64_nofile               (stderr)
none/tests/rlimit_nofile                 (stderr)
none/tests/track_high                    (stderr)
none/tests/track_yes                     (stderr)
```

should be (according to the official Fedora x86_64 builds)

```
== 835 tests, 4 stderr failures, 0 stdout failures, 0 stderrB failures, 0 stdoutB failures, 0 post failures ==
memcheck/tests/descr_belowsp             (stderr)
helgrind/tests/tc22_exit_w_lock          (stderr)
drd/tests/std_thread2                    (stderr)
drd/tests/tls_threads                    (stderr)
```

See [here](https://builder.sourceware.org/buildbot/#/builders?tags=%2Bvalgrind) for details.

### macOS

See the `macos-VERSION-expected.txt` in [`.github/`](.github/) for more details about which tests pass on which version.

Some tests are a bit flaky and might fail randomly, see [`.github/flaky-tests.txt`](.github/flaky-tests.txt) for more details.

## Contributing

### Suppressions

Valgrind is a very thorough program and can often report false positives. There are wide range of reasons why those come up (e.g. Valgrind not tracking some OS-specific part of the memory). Moreover, Valgrind might also report issues inside standard libraries, which are relevant for maintainers of such projects, but not to the end-user.

While fixing those issues would be better, it isn't always possible. This is why Valgrind supports a system called "suppressions", which is a special file format instructing which errors to ignore so the end-user doesn't see them.

Because some of those errors only show in special conditions, you might be asked to provide a "suppressions" file to be added to the repository. You can also keep those local to the project you are debugging and use them when running Valgrind.

To create a "suppressions" file, simply run:

```bash
valgrind YOUR_VALGRIND_OPTIONS --gen-suppressions=all YOUR_PROGRAM YOUR_PROGRAM_ARGS
# for example:
valgrind --trace-syscalls=yes --gen-suppressions=all ls -la
# if you want to choose on a case-by-case basis instead of generating all the suppressions, you can do:
valgrind --trace-syscalls=yes --gen-suppressions=yes ls -la
```

You will then see a few extra entries in your output, they will look something like that:

```
{
   <insert_a_suppression_name_here>
   Memcheck:Cond
   fun:_platform_strlen
   fun:_mh_execute_header
   fun:(below main)
}
```

or

```
{
   <insert_a_suppression_name_here>
   Memcheck:Param
   write(buf)
   fun:write$NOCANCEL
   obj:/dev/ttys002
   fun:_swrite
   fun:__sflush
   fun:__sfvwrite
   fun:puts
   fun:_mh_execute_header
   fun:(below main)
}
```

You can then add them to a new file, e.g. one called `my.supp`, replace the name `<insert_a_suppression_name_here>` with a description of the issue for later use. You can also use `#` to comment out lines, for documentation or disabling specific suppressions. Check any of the `.supp` files in this repository for examples, e.g. [`darwin19.supp`](darwin19.supp).

## Acknowledgements

- [tyrael9](https://github.com/tyrael9): for their work around porting Valgrind to iOS armv7 which was used as part of the port to arm64 (see [here](https://github.com/tyrael9/valgrind-ios))
