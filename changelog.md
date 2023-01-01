# Changelog

## Version 0.2.2 - 2023/01/01

### Added

- Added support for short declaration in `select` statements, e.g. `case a :=
  <-c:`

### Fixed

- Fixed `range` not highlighting correctly

## Version 0.2.1 - 2023/01/01

### Changed

- Generate comments are now contained correctly

### Fixed

- Generic parentheses, brackets, and braces are no longer contained thus
  allowing them to match at the top-level, fixing some odd highlighting issues
- Fixed `make()` of slice not highlighting other arguments correctly
- Fixed `make()` of slice of functions not highlighting multi-value returns
  correctly
- Fixed slice literals whose item type is a function with a return type
- Fixed nested slice literals
- Fixed comments in certain places where expressions are expected (e.g. function
  arguments)

## Version 0.2.0 - 2022/12/31

### Changed

- Significantly refactor and improve syntax code, including replacing various
  "hacks" with better solutions
    - It now only looks for expressions and statements where they are expected
      to be; please open an issue if certain syntax elements aren't highlighted
      correctly.
- Make the syntax sync behaviour more consistent
- Various small performance improvements

### Removed

- Removed generics in interface methods (I'm not sure why I added these in the
  first place)

### Fixed

- Fix slice of pointer types not highlighting user-defined types correctly
  correctly
- Fix incorrect highlighting of identifiers after case statements in normal
  switch statements nested inside type switch statements

## Version 0.1.0 - 2022/12/26

- First version. This syntax is still being developed and may have breaking
  changes.
