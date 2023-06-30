# Valgrind for macOS ![Build](https://github.com/LouisBrunner/valgrind-macos/workflows/Build/badge.svg)

This repository contains a version of Valgrind including a few patches to improve support for the macOS platform. It is maintained by [Louis Brunner](https://github.com/LouisBrunner).

## Status

| Version                     | x86 | amd64 | arm64  | ppc    |
| --------------------------- | --- | ----- | ------ | ------ |
| macOS 10.13 and earlier[^1] | ✅  | ✅    | -      | ❌[^3] |
| macOS 10.14 (Mojave)        | ✅  | ✅    | -      | -      |
| macOS 10.15 (Catalina)      | ✅  | ✅    | -      | -      |
| macOS 11 (Big Sur)          | -   | ✅    | ❌[^2] | -      |
| macOS 12 (Monterey)         | -   | ✅    | ❌[^2] | -      |
| macOS 13 (Ventura)          | -   | ✅    | ❌[^2] | -      |

[^1]: Supported as part of upstream Valgrind.
[^2]: Apple Silicon support in progress ([#56](https://github.com/LouisBrunner/valgrind-macos/issues/56))
[^3]: PowerPC is unsupported ([#62](https://github.com/LouisBrunner/valgrind-macos/issues/62))

Note that every version from macOS 10.12 onwards currently has the following issues:

- crash when using wqthread which is used in certain UI frameworks, especially Apple's, e.g. CoreFoundation ([#4](https://github.com/LouisBrunner/valgrind-macos/issues/4))
- using threads and signals together is undefined (crashes, hanging, etc), note: a few tests were disabled because of that
- drd thread related crash on 10.15 (probably onwards)
- lots of `-UNHANDLED` messages on macOS 12 and earlier

<!-- Checkout the [`patches`](https://github.com/LouisBrunner/valgrind-macos/commits/patches) branch for a list of patches that can be directly applied to the upstream Valgrind. -->

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

Any `brew upgrade` will now correctly rebuild the latest `LouisBrunner/valgrind` instead of the upstream one (which doesn't support latest macOS versions).

```sh
brew upgrade --fetch-HEAD LouisBrunner/valgrind/valgrind
```

## Tests

Some tests are hanging and were therefore disabled on macOS:

- `none/tests/pselect_alarm`
- `none/tests/pth_term_signal`

### Linux (Ubuntu 20.04)

These errors seem to come from the CI environment itself (as they show with or without my changes).

```
== 730 tests, 13 stderr failures, 0 stdout failures, 0 stderrB failures, 0 stdoutB failures, 0 post failures ==
memcheck/tests/overlap                   (stderr)
drd/tests/pth_mutex_signal               (stderr)
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

### macOS

See `.github/macos-VERSION-expected.txt` for more details about which tests pass on which version.

<!-- TODO: split the flaky tests into a common, different files for easier reg tracking -->

## Acknowledgements

- [tyrael9](https://github.com/tyrael9): for their work around porting Valgrind to iOS armv7 which was used as part of the port to M1 (see [here](https://github.com/tyrael9/valgrind-ios))
