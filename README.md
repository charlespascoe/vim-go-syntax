# Fast, IDE-like Vim Syntax Highlighting for Go

`vim-go-syntax` provides fast, IDE-like Vim syntax highlighting for Go.

Code highlighting should help the user quickly scan and read code, using colour
to convey additional information inferred from grammar of the language. Existing
Vim syntax definitions for Go do a poor job of this, which motivated me to write
`vim-go-syntax` from the ground up using the Go language specification. Not only
does it fix many of the shortfalls of existing syntax definitions, it's more
than 2x faster even with _all_ features enabled!

TODO: Screenshot comparison.

Comparison of `vim-go` (left) with `vim-go-syntax` (right) using my colour
scheme; see `:help group-name` to see how your colour scheme highlights Vim's
default syntax groups. Most syntax highlighting features can be disabled if
you'd prefer - see [Configuration](#configuration).

## Key Features

Key features compared to `vim-go` and the built-in Vim syntax:

- **Correctly display user-defined types and packages**
    - E.g in structs, struct literals, parameters, slices, maps, `var`/`const`
      declarations, type parameters, type assertions, type switches, `make()`,
      `new()`, etc.
- **Correctly highlights imported package names**
- **Godoc highlighting in `vim-go`**
- **Vastly more fine-grain highlight [Configuration](#configuration)**
- **Parentheses, Braces, and Brackets highlighted and individually configurable**
    - E.g. struct and function braces can be different
- **Correctly highlight functions**, including:
    - User-defined types and packages in receiver, parameters, and return types
    - Multiline parameters/return values, including successive parameters of the
      same type
    - Identifier/type differentiation in function types (e.g. `func (int,
      MyType)` vs `func (a, b MyType)`) and return types
- **Correctly handle generics:**
    - Type parameters and their constraints (including user-defined types) are
      highlighted correctly
    - User-defined types in type arguments are highlighted correctly
    - Type arguments don't break function call or struct value highlighting
- **Multiline field chains**, even with comments
- **Correct use of Vim's syntax groups**
    - More consistent with syntax definitions of other languages. See `:help
      group-name` to see your current colour scheme (or `:help :hi` to create
      your own).
- Many other small improvements, including labels, embedded types, invalid rune
  error highlighting, and better string behaviour.

## Configuration

TODO

## Caveats

- Type arguments to types and functions are limited to at most three nested
  generic types, e.g. `val := Type1[Type2[Type3[Foo]]]{ ... }`. The number of
  arguments at each level is unlimited, e.g. `Type1[Type2[A, B, C, ...]]`
- Structs literals (`MyStruct{...}`) can't have a space between the name and the
  braces, despite the fact that Go permits this.
- Type conversion (`MyType(someVal)`) is highlighted like a function call

## What's Next

- [ ] Syntax Folding
- [ ] Possibly integrate with `vim-go` to get actual type information to
  highlight things like type conversion correctly
