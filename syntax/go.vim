" Author:    Charles Pascoe
" License:   MIT (see LICENSE)
" Copyright: 2022 Charles Pascoe

if !exists('main_syntax')
    if exists("b:current_syntax") && !get(g:, "go_highlight_override_existing_syntax", 1)
        finish
    endif

    let b:__vim_go_syntax = 1
endif

syntax clear
syntax case match
syntax sync fromstart

" Define 'iskeyword' to include valid UTF-8 multibyte characters, some of which
" are technically supported for identifiers.
" TODO: Check UTF-16
syntax iskeyword @,48-57,_,192-255

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

let s:cleanup = []

com! -nargs=* GoDeferCleanup call add(s:cleanup, <q-args>)
GoDeferCleanup delcom GoDeferCleanup

fun s:Cleanup()
    for l:cmd in s:cleanup
        exec l:cmd
    endfor
endfun

fun s:getconfig(keys, default)
    if len(a:keys) == 0
        return a:default
    else
        return get(g:, a:keys[0], s:getconfig(a:keys[1:], a:default))
    endif
endfun

fun s:HiConfig(group, option_names, opts={})
    " Most syntax is highlighted by default, unless turned off by the user
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


" TODO: Rethink this approach (use string/array approach)
if s:getconfig(['go_fold_function_blocks', 'go_syntax_fold'], 1)
    com! -nargs=* GoFoldFunc <args> fold
else
    com! -nargs=* GoFoldFunc <args>
endif

GoDeferCleanup delcom GoFoldFunc


if s:getconfig(['go_fold_struct_blocks', 'go_syntax_fold'], 1)
    com! -nargs=* GoFoldStruct <args> fold
else
    com! -nargs=* GoFoldStruct <args>
endif

GoDeferCleanup delcom GoFoldStruct


if s:getconfig(['go_fold_interface_blocks', 'go_syntax_fold'], 1)
    com! -nargs=* GoFoldInterface <args> fold
else
    com! -nargs=* GoFoldInterface <args>
endif

GoDeferCleanup delcom GoFoldInterface


if s:getconfig(['go_fold_decl_blocks', 'go_syntax_fold'], 1)
    com! -nargs=* GoFoldDecl <args> fold
else
    com! -nargs=* GoFoldDecl <args>
endif

GoDeferCleanup delcom GoFoldDecl

" }}} Config Utils


" Misc {{{

" Two top-level clusters to allow regions to specify what syntax they can
" contain
" NOTE: goIota technically shouldn't be here (it should only be a part of a
" const declaration group), but trying to get it to appear only in
" goConstDeclGroup would require compromising the performance of the current
" implementation or duplicating goExpr just for goConstDeclGroup.
syntax cluster goExpr      contains=@goLiteral,goForRange,goDotExpr,goFuncLiteral,goCommaExpr,goOperator,goWordStart,goParenBlock,goBracketBlock,goComment,goIota
syntax cluster goStatement contains=@goExpr,@goFlowControl,goReturn,goSemicolon,goBraceBlock,goStatementStart,goConstDecl,goVarDecl,goTypeDecl,goKeywords

" 'goWordStart' reduces the number of times each of the 'nextgroups' is checked,
" but also prevents 'goImportedPackages' (a keyword syntax element) from
" overriding matches (e.g. in 'goStructLiteralField').
syntax match goWordStart /\<\ze\K/ contained nextgroup=goStructLiteral,goFuncCall,goBuiltins,goMakeBuiltin,goNewBuiltin,goImportedPackages

" 'goDotExpr' matches a dot that is found as a part of an expression, whereas
" 'goDot' is used to highlight a dot in non-expression contexts (e.g. the dot
" between a package and a type). 'goDotExpr' significantly improves the
" performance of searching for fields and type assertions.
syntax match goDot     /\./ contained
syntax match goDotExpr /\./ nextgroup=@goDotExpr skipwhite skipnl

" The cluster of items that could follow a dot in an expression
syntax cluster goDotExpr contains=goFuncCall,goStructLiteral,goTypeAssertion,goField,goDotComment

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
syntax match   goAssign     /=/                  skipwhite nextgroup=@goExpr
syntax match   goOperator   /[-+*/!:=%&^<>|~]\+/ skipwhite nextgroup=@goExpr
syntax match   goCommaExpr  /,/                  skipwhite skipnl nextgroup=@goExpr
syntax match   goComma      /,/                  contained
syntax match   goSemicolon  /;/
syntax keyword goUnderscore _

hi link goField      Identifier
hi link goLabel      Label
hi link goOperator   Operator
hi link goAssign     goOperator
hi link goDot        goOperator
hi link goComma      Delimiter
hi link goCommaExpr  goComma
hi link goSemicolon  Delimiter
hi link goUnderscore Special
hi link goDotExpr    goDot
hi link goDotComment goComment

call s:HiConfig('goField',     ['go_highlight_fields'],    #{default: 0})
call s:HiConfig('goLabel',     ['go_highlight_labels'])
call s:HiConfig('goOperator',  ['go_highlight_operators'])
call s:HiConfig('goDot',       ['go_highlight_dot'])
call s:HiConfig('goComma',     ['go_highlight_comma'],     #{default: 0})
call s:HiConfig('goSemicolon', ['go_highlight_semicolon'], #{default: 0})

" }}} Misc


" Comments {{{

syntax region  goComment start=+//+  end=+$+   contains=@goCommentSpell,goCommentTodo,goDirectiveComment keepend
syntax region  goComment start=+/\*+ end=+\*/+ contains=@goCommentSpell,goCommentTodo keepend

syntax keyword goCommentTodo      contained TODO FIXME XXX TBD NOTE
syntax region  goDirectiveComment start=+//\(line \|extern \| export\|[a-z0-9]\+:[a-z0-9]\+\)+ end=+$+ contained

hi link goComment          Comment
hi link goCommentTodo      Todo
hi link goDirectiveComment PreProc

call s:HiConfig('goGenerateComment', ['go_highlight_generate_tags'], #{offgroup: 'goComment'})

" }}} Comments


" Literals {{{

syntax cluster goLiteral contains=goString,goRawString,goInvalidRuneLiteral,goNumberLeader,goBooleanTrue,goBooleanFalse,goNil,goSliceOrArrayLiteral,goPrimitiveTypes,goStructType,goInterfaceType,goMapLiteral,goSliceOrArrayLiteral

" Strings

syntax region goString       matchgroup=goStringEnds start='"' skip=/\\\\\|\\"/ end='"\|$' oneline contains=@goStringSpell,goStringEscape,goDoubleQuoteEscape,goStringFormat
syntax match  goStringEscape /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\"])/ contained
syntax match  goStringFormat /\v\%%(\%|[-+# 0]*%([1-9]\d*|%(\[\d+\])?\*)?%(\.%(\d+|%(\[\d+\])?\*)?)?%(\[\d+\])?[EFGOTUXbcdefgopqstwvxf])/ contained contains=goStringFormatInvalidIndex

if s:getconfig(['go_highlight_format_string_errors'], 0)
    syntax match goStringFormatInvalidIndex /\[0\]/ contained
    hi     link  goStringFormatInvalidIndex Error
endif

" 'goInvalidRuneLiteral' is a loose match for all single-quote sequences; they
" are highlighted as errors. If they contain a valid 'goRuneLiteral' or the
" cursor is present at the end, then the 'goRuneLiteral' highlighting will
" override the 'goInvalidRuneLiteral' highlighting and thus look like a string.
syntax region goInvalidRuneLiteral start=+'+ skip=+\\.+ end=+'+ keepend oneline contains=goRuneLiteral
syntax match  goRuneLiteral        /\v'%(.*%#|[^\\]|\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\']))'/ contained contains=goRuneLiteralEscape
syntax match  goRuneLiteralEscape  /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\'])/ contained

syntax region goRawString matchgroup=goRawStringEnds start='`' end='`' keepend

" Numbers

" 'goNumberZeroLeader' searches for a digit so that the various number patterns
" don't have to, improving match performance
syntax match goNumberLeader      /\ze\<[0-9]/ nextgroup=goNumber,goNumberTypeBinary,goNumberTypeOctal,goNumberTypeHex
syntax match goNumberLeader      /\ze\.[0-9]/ nextgroup=goNumber

" TODO: Highlight all forms of invalid number formatting? E.g. underscores in
" certain places
" TODO: Highlight floats differently
syntax match goNumber /\v<[0-9][0-9_]*%(\.[0-9_]*)?%([eE][-+]?[0-9][0-9_]*)?i?/ contained contains=goNumberDecimalExp
syntax match goNumber /\v\.[0-9][0-9_]*%([eE][-+]?[0-9][0-9_]*)?i?/             contained contains=goNumberDecimalExp

syntax match goNumberTypeBinary  /\c0b/        contained nextgroup=goNumberBinary
syntax match goNumberBinary      /[01_]\+i\?/  contained

syntax match goNumberTypeOctal   /\c0o/        contained nextgroup=goNumberOctal
syntax match goNumberOctal       /[0-7_]\+i\?/ contained

syntax match goNumberTypeHex     /\c0x/        contained nextgroup=goNumberHex
syntax match goNumberHex         /\v\c[0-9a-f_]*%(\.[0-9a-f_]*)?%([pP][-+]?[0-9a-f][0-9a-f_]*)?i?/ contained contains=goNumberHexExp

syntax match goNumberSpecialChar /[_i]/       contained containedin=goNumber,goNumberBinary,goNumberOctal,goNumberHex
syntax match goNumberError       /_\{2,\}/    contained containedin=goNumber,goNumberBinary,goNumberOctal,goNumberHex

" Exponent markers
syntax match goNumberDecimalExp  /\ce/        contained
syntax match goNumberHexExp      /\cp/        contained

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
hi link goNumberBinary       goNumber
hi link goNumberOctal        goNumber
hi link goNumberHex          goNumber
hi link goNumberType         SpecialChar
hi link goNumberTypeBinary   goNumberType
hi link goNumberTypeOctal    goNumberType
hi link goNumberTypeHex      goNumberType
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
syntax match goVarAssign       /\<\K\k*\%(\.\K\k*\)*\%(\s*,\s*\%(\K\k*\%(\.\K\k*\)*\)\?\)*\ze\s*\%(<<\|>>\|&^\|[-+*/%&|^]\)\?=[^=]/ contained contains=goComma,goUnderscore,goVarStructAssign skipwhite nextgroup=goOperator
syntax match goShortVarDecl    /\<\K\k*\%(\s*,\s*\%(\K\k*\)\?\)*\ze\s*:=/                                                           contained contains=goComma,goUnderscore                   skipwhite nextgroup=goOperator
syntax match goVarStructAssign /\<\K\k*\%(\.\K\k*\)\+/ contained contains=goDotExpr

" TODO: Should these be skipempty instead of skipnl?
syntax keyword goConstDecl const skipwhite skipnl nextgroup=goVarIdentifier,goConstDeclGroup
syntax keyword goVarDecl   var   skipwhite skipnl nextgroup=goVarIdentifier,goVarDeclGroup

" TODO: Remove these once you're certain goVarGroupIdentifier is not needed
" syntax region goVarDeclGroup   matchgroup=goVarDeclParens   start='(' end=')' contained contains=@goExpr,goComment,goSemicolon,goVarGroupIdentifier
" syntax region goConstDeclGroup matchgroup=goConstDeclParens start='(' end=')' contained contains=@goExpr,goComment,goSemicolon,goVarGroupIdentifier,goIota

GoFoldDecl syntax region goVarDeclGroup   matchgroup=goVarDeclParens   start='(' end=')' contained contains=@goExpr,goSemicolon,goVarIdentifier
GoFoldDecl syntax region goConstDeclGroup matchgroup=goConstDeclParens start='(' end=')' contained contains=@goExpr,goSemicolon,goVarIdentifier

" TODO: Is it worth supporting comments in goVarComma??
syntax match goVarIdentifier /\<\K\k*/ contained skipwhite        nextgroup=goVarComma,@goType
syntax match goVarComma      /,/       contained skipwhite skipnl nextgroup=goVarIdentifier

" TODO: Remove these once you're certain goVarGroupIdentifier is not needed
" " goVarGroupIdentifier finds positions inside a var/const declaration group
" " (e.g. 'const (...)') that may be followed by an identifier. Prevents
" " goVarIdentifier from matching in the wrong places.
" syntax match goVarGroupIdentifier /^\ze\s/           contained nextgroup=goVarIdentifier skipwhite
" syntax match goVarGroupIdentifier /[(;]\@1<=\ze\s/   contained nextgroup=goVarIdentifier skipwhite
" syntax match goVarGroupIdentifier /[(;]\@1<=\ze\<\K/ contained nextgroup=goVarIdentifier

syntax keyword goIota iota contained

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

" goStatementStart ideally shouldn't contain goSwitchKeywords, but the without
" this, `default:` will be highlighted using `goLabel` instead of `goSwitchKeywords`.
syntax cluster goStatementStartGroup contains=goLabel,goSwitchKeywords

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
    " this into two makes it slightly faster overall.
    " Note: the pattern /[{;]\@1<=/ seems to be equivalent to /[{;]\@1<=./
    " which is why it had such poor performance and conflict with other
    " patterns; splitting it into two specific patterns works better
    syntax match goStatementStart /[{;]\@1<=\ze\s/   contained skipwhite nextgroup=@goStatementStartGroup
    syntax match goStatementStart /[{;]\@1<=\ze\<\K/ contained skipwhite nextgroup=@goStatementStartGroup
endif

syntax match goStatementStart /^\ze\s/   contained skipwhite nextgroup=@goStatementStartGroup
syntax match goStatementStart /^\ze\<\K/ contained           nextgroup=@goStatementStartGroup

" }}} Constants and Variables


" Packages {{{

syntax keyword goPackage      package
syntax keyword goImport       import skipwhite nextgroup=goImportItem,goImports
syntax region  goImports      matchgroup=goImportParens start='(' end=')' contained contains=goImportItem,goComment
syntax match   goImportItem   /\(\([\._]\|\K\k*\)\s\+\)\?"[^"]*"/         contained contains=goImportString
syntax region  goImportString start='"' end='"' keepend contained

hi link goPackage      Keyword
hi link goImport       Keyword
hi link goImportItem   Special
hi link goImportString goString
hi link goImportParens goParens

" }}} Packages


" Types {{{

syntax cluster goType contains=goPrimitiveTypes,goFuncType,goStructType,goInterfaceType,goMapType,goSliceOrArrayType,goChannel,goNonPrimitiveType,goPointer,goTypeParens

syntax match  goPointer /*/ contained nextgroup=@goType

" goTypeParens is used to ensure types within parens are highlighted correctly,
" e.g. the func type in the slice literal `[](func (a, b int) bool){ ... }`
syntax region goTypeParens start='(' end=')' contained contains=@goType,goComment

syntax keyword goTypeDecl     type              skipwhite skipnl nextgroup=goTypeDeclName,goTypeDeclGroup
syntax match   goTypeDeclName /\K\k*/ contained skipwhite skipnl nextgroup=goTypeDeclTypeParams,goTypeAssign,@goType
syntax match   goTypeAssign   /=/     contained skipwhite        nextgroup=@goType

syntax region goTypeDeclGroup      matchgroup=goTypeDeclGroupParens start='('  end=')'  contained contains=goTypeDeclName,goComment
syntax region goTypeDeclTypeParams matchgroup=goTypeParamBrackets   start='\[' end='\]' contained contains=goTypeParam,goComma,goComment skipwhite skipnl nextgroup=@goType

" goNonPrimitiveType is used for matching the names and packages of
" non-primitive types (i.e. types other than int, bool, string, etc.). Note the
" optional non-capturing group is later in the pattern to avoid backtracking.
syntax match goNonPrimitiveType /\<\K\k*\%(\.\K\k*\)\?\[\?/ contained contains=goPackageName,goTypeArgs
syntax match goPackageName      /\<\K\k*\ze\./              contained nextgroup=goDot

syntax region goTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma,goComment

syntax keyword goPrimitiveTypes any bool byte complex128 complex64 error float32 float64 int int8 int16 int32 int64 rune string uint uint8 uint16 uint32 uint64 uintptr contained

syntax match  goFuncType /func\s*(/ contained contains=goFuncTypeParens skipwhite nextgroup=@goType,goFuncTypeMultiReturnType

syntax region goFuncTypeParens          matchgroup=goFuncParens            start='(' end=')' contained contains=goFuncParam,goComma,goComment
syntax region goFuncTypeMultiReturnType matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma,goComment

syntax keyword goMapType map contained skipwhite skipempty nextgroup=goMapTypeKeyType
syntax region  goMapTypeKeyType matchgroup=goMapBrackets start='\[' end='\]' contained contains=@goType skipwhite nextgroup=@goType

syntax keyword goMapLiteral map contained skipwhite skipempty nextgroup=goMapLiteralKeyType
syntax region  goMapLiteralKeyType matchgroup=goMapBrackets start='\[' end='\]' contained contains=@goType skipwhite nextgroup=goMapLiteralValueType
" See comment for goSliceOrArrayLiteralType, which serves the same function as goMapLiteralValueType
syntax region  goMapLiteralValueType start='\S' end='\ze[{(]\|$' contained contains=goSliceMapLiteralTypeMatch skipwhite skipnl nextgroup=goMapLiteralItems
syntax region  goMapLiteralItems matchgroup=goMapBraces start='{' end='}' contained contains=goStructLiteralBlock,@goExpr

syntax match goSliceOrArrayType /\[\%(\d\+\|\.\.\.\)\?\]/ contained contains=goNumber,goDot skipwhite nextgroup=@goType

" A lookbehind is used to distinguish a slice/array literal with slice indexing
syntax match goSliceOrArrayLiteral /\k\@1<!\[[0-9.]*\]\ze\%(\*\|\K\|\[\|(\)/ contained contains=goNumber,goDot skipwhite nextgroup=goSliceLiteralType

" goSliceOrArrayLiteralType allows matching complex types for slice literals
" such as a slice of functions without parentheses, e.g. "[]func(a, b Foo) Bar {
" f1, f2, f3 }", which is technically valid, albeit hard to read. The use of a
" region allows the contained matches (goSliceMapLiteralTypeMatch) to extend the
" region as necessary, allowing the type to contain braces, such as "[]struct{X,
" Y int}{ ... }"
syntax region goSliceLiteralType start='\S' end='\ze[{()]\|$' contained contains=goSliceMapLiteralTypeMatch skipwhite skipnl nextgroup=goSliceItems
syntax match  goSliceMapLiteralTypeMatch /(\|\%(\%(interface\|struct\)\s*{\|[^{()]\)\+/ contained contains=@goType

syntax region goSliceItems matchgroup=goSliceBraces start='{' end='}' contained contains=goStructLiteralBlock,@goExpr

syntax match goChannel    /<-chan/        contained contains=goChannelDir skipwhite nextgroup=@goType
syntax match goChannel    /chan\%(<-\)\?/ contained contains=goChannelDir skipwhite nextgroup=@goType
syntax match goChannelDir /<-/            contained

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
hi link goMapType             goPrimitiveTypes
hi link goMapLiteral          goPrimitiveTypes
hi link goMapBrackets         Delimiter
hi link goSliceOrArrayLiteral Delimiter
hi link goSliceOrArrayType    goSliceOrArrayLiteral
hi link goSliceBraces         goBraces
hi link goMapBraces           goBraces
hi link goChannel             Type
hi link goChannelDir          goOperator

hi link goFuncType            Keyword
" See 'Functions' for other function highlight groups

call s:HiConfig('goTypeDeclName', ['go_highlight_types'])
call s:HiConfig('goSliceOrArrayLiteral', ['go_highlight_slice_brackets'])
call s:HiConfig('goMapBrackets',  ['go_highlight_map_brackets'])

" }}} Types


" Functions {{{

" Unfortunately limited to at most 3 nested type args
syntax match  goFuncCall /\v\K\k*\ze%(\(|\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\]\()/ contained nextgroup=goFuncCallTypeArgs,goFuncCallArgs
syntax region goFuncCallTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma,goComment nextgroup=goFuncCallArgs
syntax region goFuncCallArgs     matchgroup=goFuncCallParens    start='('  end=')'  contained contains=@goExpr,goComment,goArgSpread

syntax keyword goFuncDecl    func           skipwhite skipempty nextgroup=goFuncName,goMethodReceiver
syntax keyword goFuncLiteral func contained skipwhite skipempty nextgroup=goFuncName,goFuncParams

syntax match goVariadic  /\.\.\./ contained skipwhite nextgroup=@goType
syntax match goArgSpread /\.\.\./ contained

syntax match goFuncName /\K\k*/ contained skipwhite nextgroup=goFuncTypeParams,goFuncParams

syntax region goFuncTypeParams matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=goTypeParam,goComma,goComment nextgroup=goFuncParams

syntax match goTypeParam      /\K\k*/ contained skipwhite skipempty nextgroup=goTypeParamComma,goTypeConstraint
syntax match goTypeParamComma /,/     contained skipwhite skipempty nextgroup=goTypeParam

" This is a region to allow use of types that have commas (e.g. function
" definitions) or nested type parameters, because they will automatically extend
" the match of the region
syntax region goTypeConstraint start='\s'ms=e+1 end=/[,\]]/me=s-1 contained contains=@goType,goTypeConstraintSymbols
syntax match  goTypeConstraintSymbols /[~|]/ contained

" This is odd, but the \s*\zs at the start seems to ensure that the (\@1<!
" negative lookbehind works as desired (i.e. to not steal a match from
" goFuncMultiReturn); look into this further and try to remove this.
syntax match  goFuncReturnType  /\s*\zs(\@1<!\%(\%(interface\|struct\)\s*{\|[^{]\)\+/ contained contains=@goType skipwhite skipempty nextgroup=goFuncBlock

syntax region goFuncParams      matchgroup=goFuncParens            start='(' end=')' contained contains=goFuncParam,goComma,goComment skipwhite           nextgroup=goFuncReturnType,goFuncMultiReturn,goFuncBlock
syntax region goFuncMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma,goComment  skipwhite skipempty nextgroup=goFuncBlock
syntax region goMethodReceiver  matchgroup=goReceiverParens        start='(' end=')' contained contains=goFuncParam,goComment         skipwhite skipempty nextgroup=goFuncName

GoFoldFunc syntax region goFuncBlock matchgroup=goFuncBraces start='{' end='}' contained contains=@goStatement skipwhite nextgroup=goFuncCallArgs

" Previous versions (delete later)
" syntax match goFuncParam    /\%(^\|[(,]\)\@1<=\s*\zs\%(\%(\K\k*\s*,\%(\s\|\n\)*\)*\%(chan\>\)\@!\K\k*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType,goVariadic
" syntax match goFuncParam    /\%(^\|[(,]\)\@1<=\s*\zs\%(\%(\K\k*\s*,\%(\s\|\n\)*\)*\%(chan\>\)\@!\K\k*\)\%(\s*,\?\%(\s\|\n\)*\%#\|\s\+\ze[^,]\)/ contained contains=goComma,goUnderscore skipwhite nextgroup=@goType,goVariadic
" syntax match goFuncParam    /\%(^\|[(,]\)\@1<=\s*\zs\%(\%(\K\k*\s*,\%(\s\|\n\)*\)*\%(chan\>\)\@!\K\k*\)\%(\s*,\?\%(\s\|\n\)*\%#\ze)\|\s\+\ze[^,]\)/ contained contains=goComma,goUnderscore skipwhite nextgroup=@goType,goVariadic

" TODO: Peformance: Figure out how to eliminate at least the first \ze in
" '\ze)', because it more than doubles the time it takes to match this regex.
" ')\@1<=' didn't work for some reason (i.e. when typing a parameter name, it
" was highlighted as a type).
"
" goFuncParam: Assume the user is typing a parameter name (i.e. avoid
" highlighting parameter names as types until proven otherwise).
"                                                                      conditional group allows skipping directly to type, e.g. func(SomeType)                                                                                              "
"                                                       ┌───────────────────────────────────────────────────────────────────────────────────────────────────┐                                                                               "
syntax match goFuncParam        /\%(^\|[(,]\)\@1<=\s*\zs\%(\%(\%(\K\k*\s*,\%(\s\|\n\)*\)*\%(chan\>\)\@!\K\k*\)\%(\s*,\?\%(\s\|\n\)*\%#\ze)\|\s\+\ze[^,]\)\)\?/ contained contains=goComma,goUnderscore skipwhite nextgroup=@goType,goVariadic
"                                └──────────────────┘      │          └──────────────┘  │└────────────┘         │└────────────────┘      │  │           │                                                                                   "
"                               Param must be preceded     │            comma/ws/nl     │ 'chan' a type,        │   comma/ws/nl          │  │           │                                                                                   "
"                               by start of line, '(',     │                            │ not param name        │                        │  │           │                                                                                   "
"                                 or ',' followed by       └────────────────────────────┘                       └────────────────────────┘  └───────────┘                                                                                   "
"                                     whitespace            zero or more previous params                         if this matches, then we    otherwise if this                                                                              "
"                                                           (e.g. 'a, b, ' in 'a, b, c')                         have one or more params,     matches, we have                                                                              "
"                                                                                                                 then cursor, then close    params then type,                                                                              "
"                                                                                                                      paren, e.g.:                e.g.:                                                                                    "
"                                                                                                                      (a, b, c, |)              (a, b foo)                                                                                 "
" The above diagrams can be found in the Monodraw file goFuncParam_Diagrams.monopic

" goFuncParam: Assume the user is typing a type (i.e. avoid highlighting custom
" types as return value names until proven otherwise)
syntax match goNamedReturnValue /\%(^\|[(,]\)\@1<=\s*\zs\%(\%(\K\k*\s*,\%(\s\|\n\)*\)*\%(chan\>\)\@!\K\k*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType

syntax keyword goReturn return contained

hi link goFuncName              Function
hi link goFuncCall              Function
hi link goFuncCallParens        goParens
hi link goFuncDecl              goFuncType
hi link goFuncLiteral           goFuncDecl
hi link goFuncParens            goParens
hi link goFuncBraces            goBraces
hi link goFuncMultiReturnParens goParens

hi link goReceiverParens        goFuncParens

hi link goVariadic              goOperator
hi link goArgSpread             goVariadic

hi link goTypeParam             Identifier
hi link goTypeParamComma        goComma

hi link goFuncParam             Identifier
hi link goNamedReturnValue      NONE

hi link goReturn                Statement

call s:HiConfig('goFuncCall',   ['go_highlight_function_calls'])
call s:HiConfig('goFuncName',   ['go_highlight_functions'])
call s:HiConfig('goFuncParens', ['go_highlight_function_parens'])
call s:HiConfig('goFuncBraces', ['go_highlight_function_braces'])
call s:HiConfig('goFuncParam',  ['go_highlight_function_parameters'])
call s:HiConfig('goTypeParam',  ['go_highlight_type_parameters'])

" }}} Functions


" Structs and Interfaces {{{

" Note: 'goStructTypeBlock' has 'nextgroup=goStructLiteralBlock' to handle
" anonymous struct type literals

syntax keyword goStructType struct contained skipempty skipwhite nextgroup=goStructTypeBlock
syntax region  goStructTypeBlock matchgroup=goStructTypeBraces start='{' end='}' extend contained contains=goEmbeddedType,goStructTypeField,goComment,goStructTypeTag,goDot,goSemicolon skipwhite nextgroup=goStructLiteralBlock
syntax region  goStructTypeTag   start='`'            end='`' contained
syntax region  goStructTypeTag   start='"' skip='\\"' end='"' contained oneline
syntax match   goStructTypeField /\%(_\|\K\k*\)\%(,\s*\%(_\|\K\k*\)\)*/ contained contains=goComma,goUnderscore skipwhite nextgroup=@goType
syntax match   goEmbeddedType    /\*\?\K\k*\%(\.\K\k*\)\?\%(\[.*\]\)\?\%#\@1<!$/  contained contains=@goType

" It is technically possible to have a space between a struct name and the
" braces, but it's hard to reliably highlight
syntax match  goStructLiteral /\v\K\k*\ze%(\{|\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\]\{)/ contained nextgroup=goStructLiteralTypeArgs,goStructLiteralBlock
syntax region goStructLiteralTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma,goComment nextgroup=goStructLiteralBlock

" goStructLiteralBlock contains itself to 1) prevent weird highlighting while
" typing, and 2) allow slice literals of slices of structs to highlight
" correctly
GoFoldStruct syntax region goStructLiteralBlock matchgroup=goStructBraces start='{' end='}' contained contains=goStructLiteralField,goComma,@goExpr,goStructLiteralBlock

syntax match   goStructLiteralField /\<\K\k*\ze:/ contained nextgroup=goStructLiteralColon
syntax match   goStructLiteralColon /:/           contained

syntax keyword goInterfaceType interface contained skipwhite skipempty nextgroup=goInterfaceBlock
GoFoldInterface syntax region  goInterfaceBlock matchgroup=goInterfaceBraces start='{' end='}' contained contains=@goType,goTypeConstraintSymbols,goInterfaceMethod,goComment extend

syntax match   goInterfaceMethod            /\K\k*\ze(/ contained skipwhite nextgroup=goInterfaceMethodParams
syntax region  goInterfaceMethodParams      matchgroup=goInterfaceMethodParens start='(' end=')' contained contains=goFuncParam,goComma,goComment skipwhite nextgroup=@goType,goInterfaceMethodMultiReturn
syntax region  goInterfaceMethodMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma,goComment

hi link goStructType            Keyword
hi link goStructTypeBraces      goBraces
hi link goStructTypeField       Identifier
hi link goStructTypeTag         PreProc
hi link goStructLiteral         goNonPrimitiveType
hi link goStructLiteralField    Identifier
hi link goStructLiteralColon    goSemicolon
hi link goStructLiteralComma    goComma
hi link goStructBraces          goBraces

hi link goInterfaceType         goStructType
hi link goTypeConstraintSymbols goOperator
hi link goInterfaceBraces       goBraces
hi link goInterfaceMethod       goFuncName
hi link goInterfaceMethodParens goFuncParens

call s:HiConfig('goStructTypeTag',      ['go_highlight_struct_tags'])
call s:HiConfig('goStructLiteralField', ['go_highlight_struct_fields'],      #{default: 0})
call s:HiConfig('goStructTypeField',    ['go_highlight_struct_type_fields'], #{default: 0})

" }}} Structs and Interfaces


" Builtins {{{

" `make` and `new` are matched by `goMakeBuiltin` and `goNewBuiltin` so that the
" paren block that follows can have custom highlighting for types
syntax keyword goBuiltins append cap close complex copy delete imag len max min panic print println real recover skipwhite nextgroup=goFuncCallArgs

syntax keyword goMakeBuiltin make skipwhite nextgroup=goMakeBlock
syntax region  goMakeBlock   matchgroup=goFuncCallParens start='(' end=')' contained contains=@goType,goMakeArguments,goComment
syntax region  goMakeArguments start=',' end='\ze)' contained contains=@goExpr,gComment

syntax keyword goNewBuiltin new skipwhite nextgroup=goNewBlock
syntax region  goNewBlock   matchgroup=goFuncCallParens start='(' end=')' contained contains=@goType,goComment

hi link goBuiltins    Special
hi link goMakeBuiltin goBuiltins
hi link goNewBuiltin  goBuiltins

call s:HiConfig('goBuiltins', ['go_highlight_builtins'], #{offgroup: 'goFuncCall'})
call s:HiConfig('goFuncCallParens', ['go_highlight_function_call_parens'])

" }}} Builtins


" Flow Control {{{

syntax cluster goFlowControl contains=goGoto,goIf,goElse,goFor,goForRange,goForKeywords,goSwitch,goCase,goSelect,goSwitchKeywords

" 'goStatementStart' is used to avoid searching for 'goLabel' everywhere
syntax match   goLabel           /\K\k*\ze:/ contained
syntax keyword goGoto            goto        contained skipwhite nextgroup=goGotoLabel
syntax match   goGotoLabel       /\K\k*/     contained

syntax keyword goIf              if     contained skipwhite skipempty nextgroup=goShortVarDecl
syntax keyword goElse            else   contained

syntax keyword goFor             for    contained skipwhite skipempty nextgroup=goShortVarDecl
syntax keyword goForRange        range contained
syntax keyword goForKeywords     break continue contained

syntax keyword goSwitch          switch contained skipwhite           nextgroup=goShortVarDecl
syntax keyword goSelect          select contained
syntax keyword goCase            case   contained skipwhite           nextgroup=goShortVarDecl
syntax keyword goSwitchKeywords  fallthrough default contained

syntax match   goSwitchTypeCase  /^\s\+case\s/ contained skipwhite nextgroup=@goType
syntax region  goSwitchTypeBlock matchgroup=goSwitchTypeBraces start='{' end='}' contained contains=goSwitchTypeCase,goSwitchTypeBlockNestedBraces,@goStatement

" goSwitchTypeBlockNestedBraces prevents goSwitchTypeCase from matching "case" in a regular nested switch statement
syntax region  goSwitchTypeBlockNestedBraces matchgroup=goBraces start='{' end='}' contained contains=@goStatement

hi link goGoto             Statement
hi link goGotoLabel        goLabel

hi link goIf               Conditional
hi link goElse             goIf

hi link goFor              Repeat
hi link goForRange         goFor
hi link goForKeywords      goFor

hi link goSwitch           Conditional
hi link goSelect           goSwitch
hi link goCase             goSwitch
hi link goSwitchKeywords   goSwitch

hi link goSwitchTypeBraces goBraces
hi link goSwitchTypeCase   goSwitchKeywords

" }}} Flow Control


" Misc {{{

syntax keyword goKeywords defer go contained

" goTypeAssertion is a part of the nextgroup list of goDotExpr
syntax region goTypeAssertion matchgroup=goParens start=/(/ end=/)/ contained contains=@goType
syntax match  goTypeAssertion /(type)/ contained contains=goParenBlock skipwhite nextgroup=goSwitchTypeBlock

hi link goKeywords      Keyword
hi link goTypeAssertion Special

" }}} Misc


" Vim-Go Compatibility {{{

" NOTE: The code in this section is a collection of verbatim extracts from
" https://github.com/fatih/vim-go/blob/master/syntax/go.vim, included here to
" ensure compatibility with vim-go's code coverage and debugging features. Refer
" to https://github.com/fatih/vim-go/blob/master/LICENSE for details on vim-go's
" source license.

hi def link goSameId Search
hi def link goDiagnosticError SpellBad
hi def link goDiagnosticWarning SpellRare

" TODO(bc): is it appropriate to define text properties in a syntax file?
" The highlight groups need to be defined before the text properties types
" are added, and when users have syntax enabled in their vimrc after
" filetype plugin on, the highlight groups won't be defined when
" ftplugin/go.vim is executed when the first go file is opened.
" See https://github.com/fatih/vim-go/issues/2658.
if has('textprop')
    if empty(prop_type_get('goSameId'))
        call prop_type_add('goSameId', {'highlight': 'goSameId'})
    endif
    if empty(prop_type_get('goDiagnosticError'))
        call prop_type_add('goDiagnosticError', {'highlight': 'goDiagnosticError'})
    endif
    if empty(prop_type_get('goDiagnosticWarning'))
        call prop_type_add('goDiagnosticWarning', {'highlight': 'goDiagnosticWarning'})
    endif
endif

" :GoCoverage commands
hi def link goCoverageNormalText Comment
hi def      goCoverageCovered    ctermfg=green guifg=#A6E22E
hi def      goCoverageUncover    ctermfg=red guifg=#F92672

hi def link goDeclsFzfKeyword        Keyword
hi def link goDeclsFzfFunction       Function
hi def link goDeclsFzfSpecialComment SpecialComment
hi def link goDeclsFzfComment        Comment

" :GoDebug commands
if exists('*go#config#HighlightDebug') && go#config#HighlightDebug()
    hi def GoDebugBreakpoint term=standout ctermbg=117 ctermfg=0 guibg=#BAD4F5  guifg=Black
    hi def GoDebugCurrent    term=reverse  ctermbg=12  ctermfg=7 guibg=DarkBlue guifg=White
endif

" }}} Vim-Go Compatibility


call s:Cleanup()

if !exists('main_syntax')
    let b:current_syntax = 'go'
endif

" vim:tw=80:fdm=marker:fmr={{{,}}}:
