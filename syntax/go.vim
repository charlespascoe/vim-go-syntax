" Author:    Charles Pascoe
" License:   MIT (see LICENSE)
" Copyright: 2022 Charles Pascoe

if !exists('main_syntax')
    if exists("b:current_syntax") && !get(g:, 'go_highlight_override_existing_syntax', 1)
        finish
    endif

    syntax sync fromstart

    let b:__vim_go_syntax = 1
endif

syntax clear
syntax case match

" Define 'iskeyword' to include valid UTF-8 multibyte characters, some of which
" are technically supported for identifiers.
" TODO: Check UTF-16
syntax iskeyword @,48-57,_,192-255

" TODO: Syntax Folding

" TODO: Check performance of lookbehinds
" TODO: Check correct use of 'skipempty'

" Struct and Interface need 'extend' so that simple matches (e.g. /struct {/)
" can contain complex nested types by extending the parent match. No other types
" should use extend

" Some things use 'contains=TOP,@Spell' to allow them to behave predictably when
" they are nested within a region with special syntax elements (e.g.
" goVarDeclGroup and goConstDeclGroup) or when imported into other syntaxes.
" The '@Spell' is required to disable spelling in these items.  Only a handful
" of things should use 'transparent'.

" Config Utils {{{

fun s:getconfig(keys, default)
    if len(a:keys) == 0
        return a:default
    else
        return get(g:, a:keys[0], s:getconfig(a:keys[1:], a:default))
    endif
endfun

fun s:HiConfig(group, option_names, opts={})
    " All syntax is highlighted by default, unless turned off by the user
    let l:opt = s:getconfig(a:option_names, get(a:opts, 'default', 1))
    let l:cmd = ''

    if type(l:opt) == v:t_string
        if l:opt =~ '^[[:alnum:]]\+$'
            exec 'hi link '.a:group.' '.l:opt
        else
            exec 'hi '.a:group.' '.l:opt
        endif

        return 1
    elseif !l:opt
        exec 'hi link '.a:group.' '.get(a:opts, 'offgroup', 'NONE')
    endif

    return l:opt
endfun

if get(g:, 'go_highlight_string_spellcheck', 1)
    syntax cluster goStringSpell contains=@Spell
endif

if get(g:, 'go_highlight_comment_spellcheck', 1)
    syntax cluster goCommentSpell contains=@Spell
endif

" }}} Config Utils


" Misc {{{

" 'goWordStart' reduces the number of times each of the 'nextgroups' is checked,
" but also prevents 'goImportedPackages' (a keyword syntax element) from
" overriding matches (e.g. in 'goStructValueField').
syntax match goWordStart /\<\ze\K/ nextgroup=goStructValue,goFuncCall,goImportedPackages

" 'goDotExpr' matches a dot that is found as a part of an expression, whereas
" 'goDot' is used to highlight a dot in non-expression contexts (e.g. the dot
" between a package and a type). 'goDotExpr' significantly improves the
" performance of searching for fields and type assertions.
syntax match goDot     /\./ contained
syntax match goDotExpr /\./ skipwhite skipnl nextgroup=@goDotExpr

" The cluster of items that could follow a dot in an expression
syntax cluster goDotExpr contains=goFuncCall,goTypeAssertion,goField,goStructValue,goDotComment,goEmptyLine

" goDotComment is identical to goComment, except it doesn't break field
" highlighting across multiple lines, e.g.:
"
"   foo.bar.
"       baz.
"       // This is goDotComment instead of a goComment
"       blah
"
" Both 'baz' and 'blah' are correctly highlighted as fields
syntax region goDotComment start=+//+  end=+$+   contained contains=@goCommentSpell,goCommentTodo keepend skipwhite skipnl nextgroup=@goDotExpr
syntax region goDotComment start=+/\*+ end=+\*/+ contained contains=@goCommentSpell,goCommentTodo keepend skipwhite skipnl nextgroup=@goDotExpr

syntax match goField /\K\k*/ contained

" TODO: Only valid operators?
syntax match   goOperator     /[-+*/!:=%&^<>|~]\+/
syntax match   goComma        /,/
syntax match   goSemicolon    /;/
syntax keyword goUnderscore   _

hi link goField      Identifier
hi link goLabel      Label
hi link goOperator   Operator
hi link goDot        goOperator
hi link goComma      goOperator
hi link goSemicolon  goOperator
hi link goUnderscore Special
hi link goDotExpr    goDot
hi link goDotComment goComment

call s:HiConfig('goField',     ['go_highlight_fields'], #{default: 0})
call s:HiConfig('goLabel',     ['go_highlight_labels'])
call s:HiConfig('goOperator',  ['go_highlight_operators'])
call s:HiConfig('goDot',       ['go_highlight_dot',       'go_highlight_separators'], #{default: 0})
call s:HiConfig('goComma',     ['go_highlight_comma',     'go_highlight_separators'], #{default: 0})
call s:HiConfig('goSemicolon', ['go_highlight_semicolon', 'go_highlight_separators'], #{default: 0})

" }}} Misc


" Comments {{{

syntax region  goComment start=+//+  end=+$+   contains=@goCommentSpell,goCommentTodo keepend
syntax region  goComment start=+/\*+ end=+\*/+ contains=@goCommentSpell,goCommentTodo keepend

syntax keyword goCommentTodo     contained TODO FIXME XXX TBD NOTE
syntax region  goGenerateComment start=+//go:generate+ end=+$+   containedin=goComment

hi link goCommentTodo     Todo
hi link goComment         Comment
hi link goGenerateComment PreProc

call s:HiConfig('goGenerateComment', ['go_highlight_generate_tags'], #{offgroup: 'goComment'})

" }}} Comments


" Literals {{{

" Strings

syntax region goString       matchgroup=goStringEnds start='"' skip=/\\\\\|\\"/ end='"\|$' oneline contains=@goStringSpell,goStringEscape,goDoubleQuoteEscape,goStringFormat
syntax match  goStringEscape /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\"])/ contained
syntax match  goStringFormat /\v\%%(\%|[-+# 0]*%([1-9]\d*|\*)?%(\.%(\d+|\*)?)?%(\[\d+\])?[EFGOTUXbcdefgopqstvxf])/ contained

" 'goInvalidRuneLiteral' is a loose match for all single-quote sequences; they
" are highlighted as errors. If they contain a valid 'goRuneLiteral' or the
" cursor is present at the end, then the 'goRuneLiteral' highlighting will
" override the 'goInvalidRuneLiteral' highlighting and thus look like a string.
syntax region goInvalidRuneLiteral start=+'+ skip=+\\'+ end=+'+ keepend oneline contains=goRuneLiteral
syntax match  goRuneLiteral        /\v'%(.*%#|[^\\]|\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\']))'/ contained contains=goRuneLiteralEscape
syntax match  goRuneLiteralEscape  /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\'])/ contained

syntax region goRawString matchgroup=goRawStringEnds start='`' end='`' keepend

" Numbers

" TODO: Highlight all forms of invalid number formatting? E.g. underscores in
" certain places
" TODO: Highlight floats differently
syntax match goNumber /\v<[0-9][0-9_]*%(\.[0-9_]*)?%([eE][-+]?[0-9][0-9_]*)?i?/ contains=goNumberDecimalExp
syntax match goNumber /\v\.[0-9][0-9_]*%([eE][-+]?[0-9][0-9_]*)?i?/            contains=goNumberDecimalExp

syntax match goNumber /\c0b[01_]\+/  contained
syntax match goNumber /\c0o[0-7_]\+/ contained
syntax match goNumber /\v\c0x[0-9a-f_]*%(\.[0-9a-f_]*)?%([pP][-+]?[0-9a-f][0-9a-f_]*)?i?/ contained contains=goNumberHexExp

" 'goNumberZeroLeader' searches for '0' so that the above three don't have to,
" improving match performance
syntax match goNumberZeroLeader  /\ze\<0/     nextgroup=goNumber

syntax match goNumberSpecialChar /[_i]/     contained containedin=goNumber
syntax match goNumberType        /\c0[box]/ contained containedin=goNumber
syntax match goNumberError       /_\{2,\}/  contained containedin=goNumber

" Exponent markers
syntax match goNumberDecimalExp  /\ce/      contained
syntax match goNumberHexExp      /\cp/      contained

" Other

syntax keyword goBooleanTrue  true
syntax keyword goBooleanFalse false

syntax keyword goNil          nil

" Highlighting

hi link goString             String
hi link goStringEnds         goString
hi link goStringEscape       SpecialChar
hi link goStringFormat       SpecialChar

hi link goInvalidRuneLiteral Error
hi link goRuneLiteral        Character
hi link goRuneLiteralEscape  goStringFormat

hi link goRawString          String
hi link goRawStringEnds      goRawString

hi link goNumber             Number
hi link goNumberType         SpecialChar
hi link goNumberError        Error
hi link goNumberDecimalExp   SpecialChar
hi link goNumberHexExp       goNumberDecimalExp
hi link goNumberSpecialChar  SpecialChar

hi link goBooleanTrue        Boolean
hi link goBooleanFalse       Boolean

hi link goNil                Constant

call s:HiConfig('goStringFormat',       ['go_highlight_format_strings'],     #{offgroup: 'goString'})
call s:HiConfig('goInvalidRuneLiteral', ['go_highlight_rune_literal_error'], #{offgroup: 'goRuneLiteral'})
" TODO: Config for highlighting special chars in numbers

" }}} Literals


" Simple Blocks {{{

syntax region goBracketBlock matchgroup=goBrackets start='\[' end='\]' transparent extend
syntax region goParenBlock   matchgroup=goParens   start='('  end=')'  transparent extend
syntax region goBraceBlock   matchgroup=goBraces   start='{'  end='}'  transparent extend

hi link goBraces   Delimiter
hi link goBrackets Delimiter
hi link goParens   Delimiter

call s:HiConfig('goBraces',   ['go_highlight_braces'])
call s:HiConfig('goBrackets', ['go_highlight_brackets'])
call s:HiConfig('goParens',   ['go_highlight_parens'])

" }}} Simple Blocks


" Constants and Variables {{{

" TODO Slice/map assignment?
syntax match goVarAssign       /\<\K\k*\%(\.\K\k*\)*\%(\s*,\s*\%(\K\k*\%(\.\K\k*\)*\)\?\)*\ze\s*\%(<<\|>>\|&^\|[-+*/%&|^]\)\?=[^=]/ contains=goComma,goUnderscore,goVarStructAssign contained
syntax match goShortVarDecl    /\<\K\k*\%(\s*,\s*\%(\K\k*\)\?\)*\ze\s*:=/                                                           contains=goComma,goUnderscore                   contained
syntax match goVarStructAssign /\<\K\k*\%(\.\K\k*\)\+/ contained contains=goDotExpr

" TODO: Should these be skipempty instead of skipnl?
syntax keyword goConstDecl const skipnl skipwhite nextgroup=goVarIdentifier,goConstDeclGroup
syntax keyword goVarDecl   var   skipnl skipwhite nextgroup=goVarIdentifier,goVarDeclGroup

syntax region goVarDeclGroup   matchgroup=goVarDeclParens   start='(' end=')' contained contains=TOP,@Spell
syntax region goConstDeclGroup matchgroup=goConstDeclParens start='(' end=')' contained contains=TOP,@Spell

syntax match goVarIdentifier /\<\K\k*/ contained skipwhite nextgroup=goVarComma,@goType
" TODO: Is it worth supporting comments here?
syntax match goVarComma /,/ contained skipwhite skipnl nextgroup=goVarIdentifier

" goVarGroupIdentifier finds positions inside a var/const declaration group
" (e.g. 'const (...)') that may be followed by an identifier. Prevents
" goVarIdentifier from matching in the wrong places.
syntax match goVarGroupIdentifier /^\ze\s/         contained containedin=goConstDeclGroup,goVarDeclGroup skipwhite nextgroup=goVarIdentifier
syntax match goVarGroupIdentifier /[(;]\@1<=\ze\s/ contained containedin=goConstDeclGroup,goVarDeclGroup skipwhite nextgroup=goVarIdentifier
syntax match goVarGroupIdentifier /[(;]\@1<=\ze\K/ contained containedin=goConstDeclGroup,goVarDeclGroup nextgroup=goVarIdentifier

syntax keyword goIota iota contained containedin=goConstDeclGroup

hi link goConstDecl          Statement
hi link goVarDecl            Statement

hi link goConstDeclParens    goParens
hi link goVarDeclParens      goParens

hi link goVarIdentifier      Identifier
hi link goVarComma           goComma
hi link goVarGroupIdentifier goVarIdentifier
hi link goShortVarDecl       Identifier

hi link goVarAssign          Special

hi link goIota               Special

let s:assignOrShortDecl = 0

syntax cluster goStatementStartGroup contains=goLabel

call s:HiConfig('goVarIdentifier', ['go_highlight_variable_declarations'])

if s:HiConfig('goVarAssign', ['go_highlight_variable_assignments'], #{default: 0})
    let s:assignOrShortDecl = 1
    syntax cluster goStatementStartGroup add=goVarAssign
endif

if s:HiConfig('goShortVarDecl', ['go_highlight_short_variable_declarations','go_highlight_variable_declarations'])
    let s:assignOrShortDecl = 1
    syntax cluster goStatementStartGroup add=goShortVarDecl
endif

if s:assignOrShortDecl
    " This lookbehind is checked for a lot of characters in the file, which is
    " why goStatementStart is conditional and only added if needed. Splitting
    " this into three makes it slightly faster overall.
    " Note: the pattern /[{;]\@1<=/ seems to be equivalent to /[{;]\@1<=./
    " which is why it had such poor performance and conflict with other
    " patterns; splitting it into two specific patterns works better
    " TODO Should these two also have 'goLabel' in their 'nextgroup' lists?
    " TODO Should labels match under these circumstances?
    syntax match goStatementStart /[{;]\@1<=\ze\s/ contained containedin=goFuncBlock,goSwitchTypeBlock skipwhite nextgroup=@goStatementStartGroup
    syntax match goStatementStart /[{;]\@1<=\ze\K/ contained containedin=goFuncBlock,goSwitchTypeBlock skipwhite nextgroup=@goStatementStartGroup
endif

syntax match goStatementStart /^\ze\s/         contained containedin=goFuncBlock,goSwitchTypeBlock skipwhite nextgroup=@goStatementStartGroup
syntax match goStatementStart /^\ze\K/         contained containedin=goFuncBlock,goSwitchTypeBlock           nextgroup=@goStatementStartGroup

" }}} Constants and Variables


" Packages {{{

syntax keyword goPackage      package
syntax keyword goImport       import skipwhite nextgroup=goImportItem,goImports
syntax region  goImports      matchgroup=goImportParens start='(' end=')' contained contains=goImportItem,goComment
syntax match   goImportItem   /\(\([\._]\|\K\k*\)\s\+\)\?"[^"]*"/ contained contains=goImportString
syntax region  goImportString start='"' end='"' keepend contained

hi link goPackage      Keyword
hi link goImport       Keyword
hi link goImportItem   Special
hi link goImportString goString
hi link goImportParens goParens

" }}} Packages


" Types {{{

syntax cluster goType contains=goPrimitiveTypes,goFuncType,goStructType,goInterfaceType,goMap,goSliceOrArrayType,goChannel,goNonPrimitiveType,goPointer,goTypeParens

syntax match  goPointer /*/ contained nextgroup=@goType

" goTypeParens is used to ensure types within parens are highlighted correctly,
" e.g. the func type in the slice literal `[](func (a, b int) bool){ ... }`
syntax region goTypeParens start='(' end=')' contained contains=@goType

syntax keyword goTypeDecl     type    skipempty skipwhite nextgroup=goTypeDeclName,goTypeDeclGroup
syntax match   goTypeAssign   /=/     contained skipwhite nextgroup=@goType
syntax match   goTypeDeclName /\K\k*/ contained skipempty skipwhite nextgroup=goTypeDeclTypeParams,goTypeAssign,@goType

syntax region  goTypeDeclGroup      matchgroup=goTypeDeclGroupParens start='('  end=')'  contained contains=goTypeDeclName,goComment
syntax region  goTypeDeclTypeParams matchgroup=goTypeParamBrackets   start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=@goType

" goNonPrimitiveType is used for matching the names and packages of
" non-primitive types (i.e. types other than int, bool, string, etc.). Note the
" optional non-capturing group is later in the pattern to avoid backtracking.
syntax match goNonPrimitiveType /\<\K\k*\%(\.\K\k*\)\?\[\?/ contained contains=goPackageName,goTypeArgs
syntax match goPackageName      /\<\K\k*\ze\./              contained nextgroup=goDot

syntax region goTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma

syntax keyword goPrimitiveTypes any bool byte complex128 complex64 error float32 float64 int int8 int16 int32 int64 rune string uint uint8 uint16 uint32 uint64 uintptr

syntax match  goFuncType /func\s*(/ contained skipwhite contains=goFuncTypeParens skipwhite nextgroup=@goType,goFuncTypeMultiReturnType

syntax region goFuncTypeParens          matchgroup=goFuncParens            start='(' end=')' contained contains=goFuncTypeParam,goComma
syntax region goFuncTypeMultiReturnType matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma

syntax keyword goMap map skipempty skipwhite nextgroup=goMapKeyType
syntax region  goMapKeyType matchgroup=goMapBrackets start='\[' end='\]' contained contains=@goType skipwhite nextgroup=@goType

syntax match goSliceOrArrayType /\[\%(\d\+\|\.\.\.\)\?\]/ contained contains=goNumber,goDot skipwhite nextgroup=@goType

" A lookbehind is used to distinguish a new slice value with slice indexing.
" The lookbehind has variable length, so it has a reasonable 20 character limit
syntax match goSliceOrArray /\k\@1<!\[\%(\d\+\|\.\.\.\)\?\]\ze\%(\K\|\[\|(\)/ contains=goNumber,goDot skipwhite nextgroup=goSliceItemType

" Only look to the end of the line for the item type, and let slices etc. extend
" across lines as necessary. Note the first '(' is to match the first paren
" around the type, which is then extended by goTypeParens.
syntax match goSliceItemType /(\|\%(\%(interface\|struct\)\s*{\|[^{()]\)\+/ contained contains=@goType skipwhite nextgroup=goSliceItems

syntax region goSliceItems matchgroup=goSliceBraces start='{' end='}' contained contains=TOP,@Spell

syntax match goChannel /<-chan/ skipwhite contains=goOperator nextgroup=@goType
syntax match goChannel /chan\%(<-\)\?/ skipwhite contains=goOperator nextgroup=@goType


hi link goPointer             goOperator

" goTypeDecl should technically link to Typedef, but it looks a bit odd.
hi link goTypeDecl            Keyword
hi link goTypeParens          goParens
hi link goTypeDeclGroupParens goParens
hi link goTypeDeclName        Type
hi link goTypeParamBrackets   goBrackets
hi link goTypeAssign          goOperator

hi link goPackageName         Special

hi link goNonPrimitiveType    Type
hi link goPrimitiveTypes      Type
hi link goMap                 goPrimitiveTypes
hi link goMapBrackets         Delimiter
hi link goSliceOrArray        Delimiter
hi link goSliceOrArrayType    goSliceOrArray
hi link goSliceBraces         goBraces
hi link goChannel             Type

hi link goFuncType            goFuncDecl
" See 'Functions' for other function highlight groups

call s:HiConfig('goTypeDeclName', ['go_highlight_types'])
call s:HiConfig('goSliceOrArray', ['go_highlight_slice_brackets'])
call s:HiConfig('goMapBrackets',  ['go_highlight_map_brackets'])

" }}} Types


" Functions {{{

" Unfortunately limited to at most 3 nested type args
syntax match  goFuncCall /\v\K\k*\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\(/ contained nextgroup=goFuncCallTypeArgs,goFuncCallArgs
syntax region goFuncCallTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goFuncCallArgs
syntax region goFuncCallArgs     matchgroup=goFuncCallParens    start='('  end=')'  contained contains=TOP,@Spell

syntax keyword goFuncDecl func skipempty skipwhite nextgroup=goMethodReceiver,goFuncName,goFuncParams

syntax match goVariadic  /\.\.\./ contained skipwhite nextgroup=@goType
syntax match goArgSpread /\.\.\./ contained containedin=goFuncCallArgs

" TODO: Should this be "goParams" rather than "goParam"?
syntax match goParam      /\K\k*/ contained skipempty skipwhite nextgroup=goParamComma,goVariadic,@goType
syntax match goParamComma /,/     contained skipempty skipwhite nextgroup=goParam

syntax match goFuncName /\K\k*/ contained skipwhite nextgroup=goFuncTypeParams,goFuncParams

syntax region goFuncTypeParams matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=goFuncParams

" TODO: is skipempty needed?
syntax match goTypeParam      /\K\k*/ contained skipempty skipwhite nextgroup=goTypeParamComma,goTypeConstraint
syntax match goTypeParamComma /,/     contained skipempty skipwhite nextgroup=goTypeParam

" This is a region to allow use of types that have commas (e.g. function
" definitions) or nested type parameters, because they will automatically extend
" the match of the region
" TODO: Specific operators
syntax region goTypeConstraint start='\s'ms=e+1 end=/[,\]]/me=s-1 contained contains=@goType,goOperator

syntax region goFuncParams      matchgroup=goFuncParens start='(' end=')' contained contains=goParam,goComma skipwhite nextgroup=goFuncReturnType,goFuncMultiReturn,goFuncBlock
syntax match  goFuncReturnType  /\s*\zs(\@1<!\%(\%(interface\|struct\)\s*{\|[^{]\)\+{\@1<!/ contained contains=@goType skipempty skipwhite nextgroup=goFuncBlock
syntax region goFuncMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma skipempty skipwhite nextgroup=goFuncBlock
syntax region goFuncBlock       matchgroup=goFuncBraces start='{' end='}' contained contains=TOP,@Spell skipwhite nextgroup=goFuncCallArgs

syntax match  goMethodReceiver /([^,]\+)\ze\s\+\K\k*\s*[[(]/ contained contains=goReceiverBlock skipempty skipwhite nextgroup=goFuncName
syntax region goReceiverBlock matchgroup=goReceiverParens start='(' end=')' contained contains=goParam

syntax match goFuncTypeParam    /\%(^\|[(,]\)\@1<=\s*\zs\%(\K\k*\%(\s*,\%(\s\|\n\)*\K\k*\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType,goVariadic
syntax match goNamedReturnValue /\%(^\|[(,]\)\@1<=\s*\zs\%(\K\k*\%(\s*,\%(\s\|\n\)*\K\k*\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType

syntax keyword goReturn return

hi link goFuncName              Function
hi link goFuncCall              Function
hi link goFuncCallParens        goParens
hi link goFuncDecl              Keyword
hi link goFuncParens            goParens
hi link goFuncBraces            goBraces
hi link goFuncMultiReturnParens goParens

hi link goReceiverParens        goFuncParens

hi link goVariadic              goOperator
hi link goArgSpread             goVariadic

hi link goParam                 Identifier
hi link goParamComma            goComma
hi link goTypeParam             Identifier
hi link goTypeParamComma        goComma


" TODO: What to do with these?
hi link goNamedReturnValue      NONE
hi link goFuncTypeParam         NONE

hi link goReturn                Statement

call s:HiConfig('goFuncCall',   ['go_highlight_function_calls'])
call s:HiConfig('goFuncName',   ['go_highlight_functions'])
call s:HiConfig('goFuncParens', ['go_highlight_function_parens'])
call s:HiConfig('goFuncBraces', ['go_highlight_function_braces'])
call s:HiConfig('goParam',      ['go_highlight_function_parameters'])
call s:HiConfig('goTypeParam',  ['go_highlight_type_parameters'])

" }}} Functions


" Structs and Interfaces {{{

" Note: 'goStructTypeBlock' has 'nextgroup=goStructBlock' to handle anonymous
" struct types

syntax keyword goStructType struct skipempty skipwhite nextgroup=goStructTypeBlock
syntax region  goStructTypeBlock matchgroup=goStructTypeBraces start='{' end='}' extend contained contains=goEmbeddedType,goStructTypeField,goComment,goStructTypeTag,goDot,goSemicolon skipwhite nextgroup=goStructBlock
syntax region  goStructTypeTag start='`' end='`' contained
syntax region  goStructTypeTag start='"' skip='\\"' end='"' oneline contained
syntax match   goStructTypeField /\%(_\|\K\k*\)\%(,\s*\%(_\|\K\k*\)\)*/ contained skipwhite contains=goComma,goUnderscore nextgroup=@goType
syntax match   goEmbeddedType /\*\?\K\k*\%(\.\K\k*\)\?\%#\@1<!$/ contained contains=@goType

" It is technically possible to have a space between a struct name and the
" braces, but it's hard to reliably highlight
syntax match  goStructValue /\v\K\k*\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\{/ contained nextgroup=goStructValueTypeArgs,goStructBlock
syntax region goStructValueTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goStructBlock
syntax region goStructBlock matchgroup=goStructBraces start='{' end='}' contained contains=TOP,@Spell
syntax match  goStructValueField /\<\K\k*\ze:/ contained containedin=goStructBlock

syntax keyword goInterfaceType interface skipempty skipwhite nextgroup=goInterfaceBlock
" TODO: Maybe don't just put goOperator in here and instead use the correct
" symbols
syntax region goInterfaceBlock             matchgroup=goInterfaceBraces       start='{'  end='}'  contained contains=@goType,goOperator,goInterfaceMethod,goComment extend
syntax region goInterfaceMethodTypeParams  matchgroup=goTypeParamBrackets     start='\[' end='\]' contained contains=goTypeParam,goComma                            nextgroup=goInterfaceMethodParams
syntax region goInterfaceMethodParams      matchgroup=goInterfaceMethodParens start='('  end=')'  contained contains=goFuncTypeParam,goComma                        skipwhite nextgroup=@goType,goInterfaceMethodMultiReturn
syntax region goInterfaceMethodMultiReturn matchgroup=goFuncMultiReturnParens start='('  end=')'  contained contains=goNamedReturnValue,goComma
syntax match  goInterfaceMethod            /\v\K\k*\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\(/ contained skipwhite nextgroup=goInterfaceMethodTypeParams,goInterfaceMethodParams

hi link goStructType            Keyword
hi link goStructTypeBraces      goBraces
hi link goStructTypeField       Identifier
hi link goStructTypeTag         PreProc
hi link goStructValue           goNonPrimitiveType
hi link goStructValueField      Identifier
hi link goStructBraces          goBraces

hi link goInterfaceType         goStructType
hi link goInterfaceBraces       goBraces
hi link goInterfaceMethod       goFuncName
hi link goInterfaceMethodParens goFuncParens

call s:HiConfig('goStructTypeTag',    ['go_highlight_struct_tags'])
call s:HiConfig('goStructValueField', ['go_highlight_struct_fields'],      #{default: 0})
call s:HiConfig('goStructTypeField',  ['go_highlight_struct_type_fields'], #{default: 0})

" }}} Structs and Interfaces


" Builtins {{{

syntax keyword goBuiltins append cap close complex copy delete imag len panic print println real recover skipwhite nextgroup=goFuncCallArgs

syntax keyword goMakeBuiltin    make nextgroup=goMakeBlock
syntax region  goMakeBlock      matchgroup=goFuncCallParens start='(' end=')' contained contains=TOP,@Spell
syntax match   goMakeFirstParen /\%(make(\_[[:space:]]*\)\@20<=/ contained skipempty skipwhite nextgroup=@goType containedin=goMakeBlock

syntax keyword goNewBuiltin new skipwhite nextgroup=goNewBlock
syntax region  goNewBlock matchgroup=goFuncCallParens start='(' end=')' contained contains=@goType

hi link goBuiltins    Special
hi link goMakeBuiltin goBuiltins
hi link goNewBuiltin  goBuiltins

call s:HiConfig('goBuiltins', ['go_highlight_builtins'], #{offgroup: 'goFuncCall'})
call s:HiConfig('goFuncCallParens', ['go_highlight_function_call_parens'])

" }}} Builtins


" Flow Control {{{

" 'goStatementStart' is used to avoid searching for 'goLabel' everywhere
syntax match goLabel /\K\k*\ze:/ contained

syntax keyword goIf   if skipempty skipwhite nextgroup=goShortVarDecl
syntax keyword goElse else

syntax keyword goFor         for skipempty skipwhite nextgroup=goShortVarDecl
syntax keyword goForKeywords range break continue

syntax keyword goSwitch         switch skipwhite nextgroup=goShortVarDecl
syntax keyword goSelect         select
syntax keyword goSwitchKeywords case fallthrough default

syntax match  goSwitchTypeCase  /^\s\+case\s/ contained containedin=goSwitchTypeBlock skipwhite nextgroup=@goType
syntax region goSwitchTypeBlock matchgroup=goSwitchTypeBraces start='{' end='}' contained contains=TOP,@Spell

hi link goIf   Conditional
hi link goElse goIf

hi link goFor         Repeat
hi link goForKeywords goFor

hi link goSwitch         Conditional
hi link goSelect         goSwitch
hi link goSwitchKeywords goSwitch

hi link goSwitchTypeBraces goBraces
hi link goSwitchTypeCase   goSwitchKeywords

" }}} Flow Control


" Misc {{{

syntax keyword goKeywords defer go

" goTypeAssertion is a part of the nextgroup list of goDotExpr
syntax region goTypeAssertion matchgroup=goParens start=/(/ end=/)/ contained contains=@goType
syntax match  goTypeAssertion /(type)/ contained contains=goParenBlock,goTypeDecl skipwhite nextgroup=goSwitchTypeBlock

hi link goKeywords Keyword

" }}} Misc

if !exists('main_syntax')
    let b:current_syntax = 'go'
endif

" vim:tw=80:fdm=marker:fmr={{{,}}}:
