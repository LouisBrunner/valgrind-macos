# Valgrind for macOS ![Build](https://github.com/LouisBrunner/valgrind-macos/workflows/Build/badge.svg)

This repository contains a version of Valgrind including a few patches to improve support for the macOS platform. It is maintained by [Louis Brunner](https://github.com/LouisBrunner).

## Status

| Version                     | x86 | amd64 | arm64  | ppc    |
| --------------------------- | --- | ----- | ------ | ------ |
| macOS 10.13 and earlier[^1] | ✅  | ✅    | -      | ❌[^3] |
| macOS 10.14 (Mojave)        | ✅  | ✅    | -      | -      |
| macOS 10.15 (Catalina)      | ✅  | ✅    | -      | -      |
| macOS 11 (Big Sur)          | -   | ✅    | ✅     | -      |
| macOS 12 (Monterey)         | -   | ✅    | ✅     | -      |
| macOS 13 (Ventura)          | -   | ✅    | ✅     | -      |
| macOS 14 (Sonoma)           | -   | ✅    | ✅     | -      |
| macOS 15 (Sequoia)          | -   | ✅    | ✅     | -      |

[^1]: Supported as part of upstream Valgrind.
[^3]: PowerPC is unsupported ([#62](https://github.com/LouisBrunner/valgrind-macos/issues/62))

Note that every version from macOS 10.12 onwards currently has the following issues:

- crash when using wqthread which is used in certain UI frameworks, especially Apple's, e.g. CoreFoundation ([#4](https://github.com/LouisBrunner/valgrind-macos/issues/4))
- using threads and signals together is undefined (crashes, hanging, etc), note: a few tests were disabled because of that
- drd crashes on 10.15 (probably onwards)
- dhat crashes (seen macOS 14 arm64)
- loads of `-UNHANDLED` messages

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

### Linux (Ubuntu 24.04)

These errors seem to come from the CI environment itself (as they show with or without my changes).

```
== 834 tests, 3 stderr failures, 0 stdout failures, 0 stderrB failures, 0 stdoutB failures, 0 post failures ==
none/tests/log-track-fds                 (stderr)
none/tests/track-fds-exec-children       (stderr)
none/tests/xml-track-fds                 (stderr)
```

should be (according to the official Fedora x86_64 builds)

```
== 815 tests, 0 stderr failures, 0 stdout failures, 0 stderrB failures, 0 stdoutB failures, 0 post failures ==
```

See [here](https://builder.sourceware.org/buildbot/#/builders?tags=%2Bvalgrind) for details.

### macOS

See `.github/macos-VERSION-expected.txt` for more details about which tests pass on which version.

Some tests are a bit flaky and might fail randomly, see `.github/flaky-tests.txt` for more details.

## Acknowledgements

- [tyrael9](https://github.com/tyrael9): for their work around porting Valgrind to iOS armv7 which was used as part of the port to M1 (see [here](https://github.com/tyrael9/valgrind-ios))
