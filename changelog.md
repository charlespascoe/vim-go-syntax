# Changelog

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

- Fix issue with slice of pointer type not highlighting user-defined types
  correctly
- Fix issue where identifiers after case statements in switch statements nested
  inside type switch statements were highlighted as types

## Version 0.1.0 - 2022/12/26

- First version. This syntax is still being developed and may have breaking
  changes.
