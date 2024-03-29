# Changelog

<!-- markdownlint-disable MD024 -->

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.2.0] - 2022-10-11

### Added

- Added `lfiles` module.
- Added functions `llists:enumerate/1`, `llists:enumerate/2`, `llists:uniq/1`
  and `llists:uniq/2`.

### Deprecated

- The `llist_utils:enumerate/1` function is now replaced by
  `llists:enumerate/1` and should no longer be used.

## [1.1.0] - 2019-4-19

### Added

- Added functions `llists_utils:combinations/2`,
  `llists_utils:combinations/3`, `llists_utils:cycle/1`,
  `llists_utils:permutations/2`, `llists_utils:permutations/3`,
  `llists_utils:choice/1`, `llists_utils:enumerate/1`, `llists_utils:random/0`
  and `llists_utils:random/1`.

## [1.0.0] - 2019-3-22

### Added

- Initial release.

[unreleased]: https://github.com/jkrukoff/llists/compare/v1.2.0...HEAD
[1.2.0]: https://github.com/jkrukoff/llists/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/jkrukoff/llists/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/jkrukoff/llists/releases/tag/v1.0.0
