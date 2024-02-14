# Fast, 'tree-sitter'-like Vim Syntax Highlighting for Go

`vim-go-syntax` provides fast and feature-rich Vim syntax highlighting for Go,
with results comparable to tree-sitter in Neovim.

> **Feedback is welcome!** This syntax is still fairly new and I'd like feedback
> or issues to improve it. Please see if there's already an issue open or create
> a new one.

Syntax highlighting can make reading code easier by using colour to convey
additional information. Existing Vim syntax definitions aren't able to to
correctly highlight key parts of the syntax, which motivated me to write
`vim-go-syntax` from the ground up using the Go language specification. Not only
does it fix many of the shortfalls of existing syntax definitions, it's more
than 2x faster even with _all_ features enabled!

<a href="Syntax%20Comparison.png?raw=true" target="_blank">
<img
    src="Syntax%20Comparison.png"
    title="Syntax Comparison; vim-go on the left, vim-go-syntax on the right"
/>
</a>

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
- **More customisable [Configuration](#configuration)**
- **Vastly more fine-grained highlighting**
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

Listed in the table below are various options that can be used to customise
highlighting. These can be set at any time before the syntax is loaded, usually
your `.vimrc` is best.

Each option can be:

- A `1` or a `0` to enable/disable the highlighting. For example:

```vim
let g:go_highlight_parens = 0
let g:go_highlight_fields = 1
```

- A syntax group name; see '`:help group-name`' to see what default groups are
  available and how your colour scheme styles them. For example, to make
  function calls look like types:

```vim
let g:go_highlight_function_calls = 'Type'
```

- A highlight description; see '`:help hi`' for syntax and options. For example,
  to make type parameters blue and italic:

```vim
let g:go_highlight_type_parameters = 'ctermfg=4 cterm=italic'
```

If you want even more customisation, you can override the default highlighting
by adding custom highlighting rules to `~/.vim/after/syntax/go.vim` (you'll need
to create this file first). The previous configuration examples could also be
achieved like this:

```vim
" ~/.vim/after/syntax/go.vim

hi clear goParens

hi link goField Identifier

hi link goFuncCall Type

hi goTypeParam ctermfg=4 cterm=italic
```

See `syntax/go.vim` for the names of all syntax and
highlight groups.

Summary of options (see below table for descriptions):

| Configuration Option                         | Default     | Default Group |
| -------------------------------------------- | ----------- | ------------- |
| `g:go_highlight_braces`                      | **Enabled** | `Delimiter`   |
| `g:go_highlight_brackets`                    | **Enabled** | `Delimiter`   |
| `g:go_highlight_builtins`                    | **Enabled** | `Special`     |
| `g:go_highlight_comma`                       |   Disabled  | `Delimiter`   |
| `g:go_highlight_dot`                         | **Enabled** | `Operator`    |
| `g:go_highlight_fields`                      |   Disabled  | `Identifier`  |
| `g:go_highlight_format_strings`              | **Enabled** | `SpecialChar` |
| `g:go_highlight_format_string_errors`        |   Disabled  | `Error`       |
| `g:go_highlight_functions`                   | **Enabled** | `Function`    |
| `g:go_highlight_function_parens`             | **Enabled** | `Delimiter`   |
| `g:go_highlight_function_braces`             | **Enabled** | `Delimiter`   |
| `g:go_highlight_function_calls`              | **Enabled** | `Function`    |
| `g:go_highlight_function_call_parens`        | **Enabled** | `Delimiter`   |
| `g:go_highlight_generate_tags`               | **Enabled** | `PreProc`     |
| `g:go_highlight_rune_literal_error`          | **Enabled** | `Error`       |
| `g:go_highlight_labels`                      | **Enabled** | `Label`       |
| `g:go_highlight_map_brackets`                | **Enabled** | `Delimiter`   |
| `g:go_highlight_operators`                   | **Enabled** | `Operator`    |
| `g:go_highlight_function_parameters`         | **Enabled** | `Identifier`  |
| `g:go_highlight_parens`                      | **Enabled** | `Delimiter`   |
| `g:go_highlight_semicolon`                   |   Disabled  | `Delimiter`   |
| `g:go_highlight_short_variable_declarations` | **Enabled** | `Identifier`  |
| `g:go_highlight_slice_brackets`              | **Enabled** | `Delimiter`   |
| `g:go_highlight_format_strings`              | **Enabled** | `SpecialChar` |
| `g:go_highlight_struct_type_fields`          |   Disabled  | `Identifier`  |
| `g:go_highlight_struct_tags`                 | **Enabled** | `PreProc`     |
| `g:go_highlight_struct_fields`               |   Disabled  | `Identifier`  |
| `g:go_highlight_types`                       | **Enabled** | `Type`        |
| `g:go_highlight_type_parameters`             | **Enabled** | `Identifier`  |
| `g:go_highlight_variable_assignments`        |   Disabled  | `Special`     |
| `g:go_highlight_variable_declarations`       | **Enabled** | `Identifier`  |


- `g:go_highlight_braces`
    - All braces (`{}`). More specific brace options (e.g.
  `g:go_highlight_function_braces`) will take precedence over this option.
- `g:go_highlight_brackets`
    - All brackets (`[]`). More specific bracket options (e.g.
      `g:go_highlight_map_brackets`) will take precedence over this option.
- `g:go_highlight_builtins`
    - Highlight built-in functions differently from other functions.
- `g:go_highlight_comma`
    - Commas.
- `g:go_highlight_dot`
    - Dot operators, e.g. the dot in `foo.bar()`
- `g:go_highlight_fields`
    - Fields in expressions, e.g. `bar` in
  `foo.bar = 123`
- `g:go_highlight_format_strings`
    - Format strings, e.g. `"Hello, %s"`.
- `g:go_highlight_format_string_errors`
    - Invalid format strings. Currently only zero index in format strings, e.g.
      `fmt.Sprintf("%[0]d", 123)` which should be `fmt.Sprintf("%[1]d", 123)`.
- `g:go_highlight_functions`
    - Function declaration names, e.g. `foo` in `func foo() { }`. Also applies
      to method names.
- `g:go_highlight_function_parens`
    - Parentheses around parameter list and receiver type.
- `g:go_highlight_function_braces`
    - The braces of the function block.
- `g:go_highlight_function_calls`
    - Function calls. Turning this off does not affect other syntax elements
      (e.g. generics, function call parentheses).
- `g:go_highlight_function_call_parens`
    - The parentheses of a function call.
- `g:go_highlight_generate_tags`
    - Generate comments, e.g. `//go:generate ...`. Turning this off makes them
      look like regular comments.
- `g:go_highlight_rune_literal_error`
    - Invalid rune literals.
- `g:go_highlight_labels`
    - User-defined labels.
- `g:go_highlight_map_brackets`
    - The brackets in map types, e.g.
  `map[string]int`.
- `g:go_highlight_operators`
    - Operators including assignment, e.g. `+`, `-`, `&&`, `||`, `=`, `:=`, etc.
- `g:go_highlight_function_parameters`
    - Parameter names, e.g. `bar` in `func
  foo(bar int)`
- `g:go_highlight_parens`
    - All parentheses. More specific parenthesis options (e.g.
      `g:go_highlight_function_parens`) will take precedence.
- `g:go_highlight_semicolon`
    - Semicolons.
- `g:go_highlight_short_variable_declarations`
    - Highlight the names of new variables defined by the declaration syntax,
      e.g. `a` and `b` in `a, b := foo()`.
- `g:go_highlight_slice_brackets`
    - The brackets in slice types, e.g. `[]string`.
- `g:go_highlight_format_strings`
    - Format placeholders in in format strings, e.g. `%s` in `fmt.Printf("%s",
      foo)`
- `g:go_highlight_struct_type_fields`
    - Field names in struct types, e.g. `Bar` in `type Foo struct { Bar int }`
- `g:go_highlight_struct_tags`
    - Struct tags, the backtick-delimited strings in structs, e.g. `` `json:bar` ``
      in ``struct { Bar int `json:"bar"` }``.
- `g:go_highlight_struct_fields`
    - Field names in struct literals, e.g. `Bar` in `f := Foo{ Bar: 123 }`.
- `g:go_highlight_types`
    - Type declaration names, e.g. `Foo` in `type Foo struct { Bar int }`. Even
      with this option off, types parameters etc. will still be highlighted
      as types.
- `g:go_highlight_type_parameters`
    - Type parameter names in type parameter lists, e.g. the `T` in the brackets
      of `type Foo[T] { Bar T }`; the `T` in the struct will be highlighted as a
      type.
- `g:go_highlight_variable_assignments`
    - Variable names in assignments, e.g. `a` and `b` in `a, b = foo()`.
- `g:go_highlight_variable_declarations`
    - Names in `var` or `const` assignments, e.g. `a` in `var a []string`. Also
      applies to short variable declarations (i.e. `:=`) unless
      `g:go_highlight_short_variable_declarations` is set.

Other configuration options:

- `g:go_fold_decl_blocks` (default: 1)
    - Fold `var` and `const` declaration blocks.
- `g:go_fold_function_blocks` (default: 1)
    - Fold function blocks.
- `g:go_fold_struct_blocks` (default: 1)
    - Fold struct blocks.
- `g:go_fold_interface_blocks` (default: 1)
    - Fold interface blocks.
- `g:go_syntax_fold` (default: 1)
    - Global fold setting. If set to 0, all folding is disabled. If set to 1,
      each respective `g:go_fold_*` option is used.

## Caveats

- Type arguments to types and functions are limited to at most three nested
  generic types, e.g. `val := Type1[Type2[Type3[Foo]]]{ ... }`. The number of
  arguments at each level is unlimited, e.g. `Type1[Type2[A, B, C, ...]]`
- The type parameters of a generic type embedded within a struct (that is, the
  `A,B,C` in `struct { Foo[A,B,C] }`) can't be spread across multiple lines,
  though this is very uncommon in practical use.
- Structs literals (`MyStruct{...}`) can't have a space between the name and the
  braces, despite the fact that Go permits this.
- Type conversion (`MyType(someVal)`) is highlighted like a function call
- A slice or map literal whose type is a function with multiple return values
  and the type is not in parentheses (e.g. `[]func(a, b int) (c, d Foo){ f1, f2,
  f3 }`) is technically valid Go code, but the `Foo` type will not be highlighted
  correctly.
    - This can easily be avoided by wrapping the type in parentheses to make it
      clear (e.g. `[](func(a, b int) (c, d Foo)){ f1, f2, f3 }`, which is
      highlighted correctly), or preferably use a named type to make the code
      easier to read (e.g. `type MyFunc func(a, b int) (c, d Foo)`, and then use
      `[]MyFunc{ f1, f2, f3 }`, all of which will be highlighted correctly).

## What's Next

- [ ] Possibly integrate with `vim-go` to get actual type information to
  highlight things like type conversion correctly
