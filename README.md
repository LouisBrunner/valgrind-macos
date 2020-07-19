# Valgrind for macOS ![Build](https://github.com/LouisBrunner/valgrind-macos/workflows/Build/badge.svg)

This repository contains a version of Valgrind including a few patches to improve support for the macOS platform. It is maintained by [Louis Brunner](https://github.com/LouisBrunner).

Basic iOS support is in progress based on: https://github.com/tyrael9/valgrind-ios.

## Status

Valgrind now builds and works on every macOS version

Note that some features are still in progress:

- crash when using wqthread (used in certain UI frameworks)
- using threads and signals is undefined

It is currently tested on 10.14.6 and 10.15.4.

Checkout the [`patches`](https://github.com/LouisBrunner/valgrind-macos/commits/patches) branch for a list of patches that can be directly applied to the upstream Valgrind.

### macOS 11 and later

Due to changes on how macOS bundles and loads system dylibs, Valgrind is currently unable to track memory allocation correctly and thus to report memory leaks on macOS 11 and later.

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
