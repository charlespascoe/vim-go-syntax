# Changelog

## Unreleased

### Added

- Add folding for interface and declaration blocks

### Fixed

- Properly handle explicit argument indexes for width and precision in string
  format sequences
    - E.g. `%0[1]*[2]b` and `%[1]*d`
- Fix `chan` highlighting in multi return and function type parameters without names
    - E.g. in `func Something() (bool, chan int) { ... }`, `bool` and `chan`
      would be highlighted as though they were return variables of type `int`,
      and similarly in `type Something func(bool, chan int)`.


## Version 0.3.0 - 2023/01/18

### Added

- Add support for highlighting typeless structs in map literals

### Fixed

- Fix map literal brace highlighting, including issues with variable names as
  keys being highlighted as labels
- Fixing error message when there's an empty import string
- Fix clash with vim-go's Godoc syntax
- Fixed slice type conversion, e.g. `[]byte("foo")`
    - This was caused by trying to improve support for slice literals of
      functions in 0.2.1. It's not possible to reconcile these two things, and
      so type conversion was preferred and a caveat for the function
      highlighting was added to the readme.
- Fix `\\` in rune literal
- Fix matching certain syntax elements not matching at all outside of functions
- Fix type highlighting after type parameters in type definitions

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
