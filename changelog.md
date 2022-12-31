# Changelog

## Unreleased

### Changed

- Significantly refactor and improve syntax code, including refactoring various
  "hacks"
    - It now only looks for expressions and statements where they are expected
      to be; please open an issue if certain syntax elements aren't highlighted
      correctly.
- Make the syntax sync behaviour more consistent
- Various small performance improvements

### Removed

- Removed generics in interface methods (I'm not sure why I added these in the
  first place)

### Fixed

- Fix issue with slice of pointer type not highlighting user-defined types
  correctly

## Version 0.1.0 - 2022/12/26

- First version. This syntax is still being developed and may have breaking
  changes.
