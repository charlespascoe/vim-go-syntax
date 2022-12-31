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
if s:getconfig(['g:go_fold_function_blocks', 'go_syntax_fold'], 1)
    com! -nargs=* GoFoldFunc <args> fold
else
    com! -nargs=* GoFoldFunc <args>
endif

GoDeferCleanup delcom GoFoldFunc


if s:getconfig(['g:go_fold_struct_blocks', 'go_syntax_fold'], 1)
    com! -nargs=* GoFoldStruct <args> fold
else
    com! -nargs=* GoFoldStruct <args>
endif

GoDeferCleanup delcom GoFoldStruct

" }}} Config Utils


" Misc {{{

" Two top-level clusters to allow regions to specify what syntax they can
" contain
syntax cluster goExpr      contains=@goLiteral,goDotExpr,goFuncLiteral,goCommaExpr,goOperator,goWordStart,goParenBlock,goBracketBlock
syntax cluster goStatement contains=@goExpr,@goFlowControl,goReturn,goSemicolon,goBraceBlock,goComment,goStatementStart,goConstDecl,goVarDecl,goTypeDecl,goKeywords

syntax match goIdentifier /\K\k*/ contained nextgroup=goDotExpr skipwhite

" 'goWordStart' reduces the number of times each of the 'nextgroups' is checked,
" but also prevents 'goImportedPackages' (a keyword syntax element) from
" overriding matches (e.g. in 'goStructLiteralField').
syntax match goWordStart /\<\ze\K/ contained nextgroup=goStructLiteral,goFuncCall,goBuiltins,goMakeBuiltin,goNewBuiltin,goImportedPackages,goIdentifier

" 'goDotExpr' matches a dot that is found as a part of an expression, whereas
" 'goDot' is used to highlight a dot in non-expression contexts (e.g. the dot
" between a package and a type). 'goDotExpr' significantly improves the
" performance of searching for fields and type assertions.
syntax match goDot     /\./ contained
syntax match goDotExpr /\./ contained nextgroup=@goDotExpr skipwhite skipnl

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
syntax match   goAssign     /=/                            skipwhite nextgroup=@goExpr
syntax match   goOperator   /[-+*/!:=%&^<>|~]\+/ contained skipwhite nextgroup=@goExpr
syntax match   goCommaExpr  /,/                  contained skipwhite skipnl nextgroup=@goExpr
syntax match   goComma      /,/                  contained
syntax match   goSemicolon  /;/                  contained
syntax keyword goUnderscore _                    contained

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

syntax cluster goLiteral contains=goString,goRawString,goInvalidRuneLiteral,goNumberLeader,goBooleanTrue,goBooleanFalse,goNil,goSliceOrArrayLiteral,goPrimitiveTypes,goStructType,goInterfaceType,goMap,goSliceOrArrayLiteral

" Strings

syntax region goString       matchgroup=goStringEnds start='"' skip=/\\\\\|\\"/ end='"\|$' contained oneline contains=@goStringSpell,goStringEscape,goDoubleQuoteEscape,goStringFormat
syntax match  goStringEscape /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\"])/ contained
syntax match  goStringFormat /\v\%%(\%|[-+# 0]*%([1-9]\d*|\*)?%(\.%(\d+|\*)?)?%(\[\d+\])?[EFGOTUXbcdefgopqstvxf])/ contained

" 'goInvalidRuneLiteral' is a loose match for all single-quote sequences; they
" are highlighted as errors. If they contain a valid 'goRuneLiteral' or the
" cursor is present at the end, then the 'goRuneLiteral' highlighting will
" override the 'goInvalidRuneLiteral' highlighting and thus look like a string.
syntax region goInvalidRuneLiteral start=+'+ skip=+\\'+ end=+'+ keepend oneline contains=goRuneLiteral contained
syntax match  goRuneLiteral        /\v'%(.*%#|[^\\]|\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\']))'/ contained contains=goRuneLiteralEscape
syntax match  goRuneLiteralEscape  /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\'])/ contained

syntax region goRawString matchgroup=goRawStringEnds start='`' end='`' keepend contained

" Numbers

" TODO: Highlight all forms of invalid number formatting? E.g. underscores in
" certain places
" TODO: Highlight floats differently
syntax match goNumber /\v<[0-9][0-9_]*%(\.[0-9_]*)?%([eE][-+]?[0-9][0-9_]*)?i?/ contained contains=goNumberDecimalExp
syntax match goNumber /\v\.[0-9][0-9_]*%([eE][-+]?[0-9][0-9_]*)?i?/             contained contains=goNumberDecimalExp

syntax match goNumber /\c0b[01_]\+/  contained
syntax match goNumber /\c0o[0-7_]\+/ contained
syntax match goNumber /\v\c0x[0-9a-f_]*%(\.[0-9a-f_]*)?%([pP][-+]?[0-9a-f][0-9a-f_]*)?i?/ contained contains=goNumberHexExp

" 'goNumberZeroLeader' searches for a digit so that the above three don't have to,
" improving match performance
syntax match goNumberLeader      /\ze\<[0-9]/ contained nextgroup=goNumber
syntax match goNumberLeader      /\ze\.[0-9]/ contained nextgroup=goNumber

syntax match goNumberSpecialChar /[_i]/       contained containedin=goNumber
syntax match goNumberType        /\c0[box]/   contained containedin=goNumber
syntax match goNumberError       /_\{2,\}/    contained containedin=goNumber

" Exponent markers
syntax match goNumberDecimalExp  /\ce/        contained
syntax match goNumberHexExp      /\cp/        contained

" Other

syntax keyword goBooleanTrue  true  contained
syntax keyword goBooleanFalse false contained

syntax keyword goNil          nil   contained

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

syntax region goBracketBlock matchgroup=goBrackets start='\[' end='\]' contained transparent extend
syntax region goParenBlock   matchgroup=goParens   start='('  end=')'  contained transparent extend
syntax region goBraceBlock   matchgroup=goBraces   start='{'  end='}'  contained transparent extend

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

syntax region goVarDeclGroup   matchgroup=goVarDeclParens   start='(' end=')' contained contains=@goExpr,goComment,goSemicolon,goVarIdentifier
syntax region goConstDeclGroup matchgroup=goConstDeclParens start='(' end=')' contained contains=@goExpr,goComment,goSemicolon,goVarIdentifier,goIota

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

syntax cluster goType contains=goPrimitiveTypes,goFuncType,goStructType,goInterfaceType,goMap,goSliceOrArrayType,goChannel,goNonPrimitiveType,goPointer,goTypeParens

syntax match  goPointer /*/ contained nextgroup=@goType

" goTypeParens is used to ensure types within parens are highlighted correctly,
" e.g. the func type in the slice literal `[](func (a, b int) bool){ ... }`
syntax region goTypeParens start='(' end=')' contained contains=@goType

syntax keyword goTypeDecl     type              skipwhite skipempty nextgroup=goTypeDeclName,goTypeDeclGroup
syntax match   goTypeDeclName /\K\k*/ contained skipwhite skipempty nextgroup=goTypeDeclTypeParams,goTypeAssign,@goType
syntax match   goTypeAssign   /=/     contained skipwhite           nextgroup=@goType

syntax region goTypeDeclGroup      matchgroup=goTypeDeclGroupParens start='('  end=')'  contained contains=goTypeDeclName,goComment
syntax region goTypeDeclTypeParams matchgroup=goTypeParamBrackets   start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=@goType

" goNonPrimitiveType is used for matching the names and packages of
" non-primitive types (i.e. types other than int, bool, string, etc.). Note the
" optional non-capturing group is later in the pattern to avoid backtracking.
syntax match goNonPrimitiveType /\<\K\k*\%(\.\K\k*\)\?\[\?/ contained contains=goPackageName,goTypeArgs
syntax match goPackageName      /\<\K\k*\ze\./              contained nextgroup=goDot

syntax region goTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma

syntax keyword goPrimitiveTypes any bool byte complex128 complex64 error float32 float64 int int8 int16 int32 int64 rune string uint uint8 uint16 uint32 uint64 uintptr contained

syntax match  goFuncType /func\s*(/ contained contains=goFuncTypeParens skipwhite nextgroup=@goType,goFuncTypeMultiReturnType

syntax region goFuncTypeParens          matchgroup=goFuncParens            start='(' end=')' contained contains=goFuncTypeParam,goComma
syntax region goFuncTypeMultiReturnType matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma

syntax keyword goMap map contained skipwhite skipempty nextgroup=goMapKeyType
syntax region  goMapKeyType matchgroup=goMapBrackets start='\[' end='\]' contained contains=@goType skipwhite nextgroup=@goType

syntax match goSliceOrArrayType /\[\%(\d\+\|\.\.\.\)\?\]/ contained contains=goNumber,goDot skipwhite nextgroup=@goType

" A lookbehind is used to distinguish a slice/array literal with slice indexing
syntax match goSliceOrArrayLiteral /\k\@1<!\[\%(\d\+\|\.\.\.\)\?\]\ze\%(\*\|\K\|\[\|(\)/ contained contains=goNumber,goDot skipwhite nextgroup=goSliceItemType

" Only look to the end of the line for the item type, and let slices etc. extend
" across lines as necessary. Note the first '(' is to match the first paren
" around the type, which is then extended by goTypeParens.
syntax match goSliceItemType /(\|\%(\%(interface\|struct\)\s*{\|[^{()]\)\+/ contained contains=@goType skipwhite nextgroup=goSliceItems

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
hi link goMap                 goPrimitiveTypes
hi link goMapBrackets         Delimiter
hi link goSliceOrArrayLiteral Delimiter
hi link goSliceOrArrayType    goSliceOrArrayLiteral
hi link goSliceBraces         goBraces
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
syntax region goFuncCallTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goFuncCallArgs
syntax region goFuncCallArgs     matchgroup=goFuncCallParens    start='('  end=')'  contained contains=@goExpr

syntax keyword goFuncDecl    func           skipwhite skipempty nextgroup=goFuncName,goMethodReceiver
syntax keyword goFuncLiteral func contained skipwhite skipempty nextgroup=goFuncName,goFuncParams

syntax match goVariadic  /\.\.\./ contained skipwhite nextgroup=@goType
syntax match goArgSpread /\.\.\./ contained containedin=goFuncCallArgs

" TODO: Should this be "goParams" rather than "goParam"?
syntax match goParam      /\K\k*/ contained skipwhite skipempty nextgroup=goParamComma,goVariadic,@goType
syntax match goParamComma /,/     contained skipwhite skipempty nextgroup=goParam

syntax match goFuncName /\K\k*/ contained skipwhite nextgroup=goFuncTypeParams,goFuncParams

syntax region goFuncTypeParams matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=goFuncParams

" TODO: is skipempty needed?
syntax match goTypeParam      /\K\k*/ contained skipwhite skipempty nextgroup=goTypeParamComma,goTypeConstraint
syntax match goTypeParamComma /,/     contained skipwhite skipempty nextgroup=goTypeParam

" This is a region to allow use of types that have commas (e.g. function
" definitions) or nested type parameters, because they will automatically extend
" the match of the region
syntax region goTypeConstraint start='\s'ms=e+1 end=/[,\]]/me=s-1 contained contains=@goType,goTypeConstraintSymbols
syntax match  goTypeConstraintSymbols /[~|]/ contained

syntax match  goFuncReturnType  /\s*\zs(\@1<!\%(\%(interface\|struct\)\s*{\|[^{]\)\+{\@1<!/ contained contains=@goType skipwhite skipempty nextgroup=goFuncBlock

syntax region goFuncParams      matchgroup=goFuncParens            start='(' end=')' contained contains=goParam,goComma            skipwhite           nextgroup=goFuncReturnType,goFuncMultiReturn,goFuncBlock
syntax region goFuncMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma skipwhite skipempty nextgroup=goFuncBlock
syntax region goMethodReceiver  matchgroup=goReceiverParens        start='(' end=')' contained contains=goFuncTypeParam            skipwhite skipempty nextgroup=goFuncName

GoFoldFunc syntax region goFuncBlock matchgroup=goFuncBraces start='{' end='}' contained contains=@goStatement skipwhite nextgroup=goFuncCallArgs

syntax match goFuncTypeParam    /\%(^\|[(,]\)\@1<=\s*\zs\%(\K\k*\%(\s*,\%(\s\|\n\)*\K\k*\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType,goVariadic
syntax match goNamedReturnValue /\%(^\|[(,]\)\@1<=\s*\zs\%(\K\k*\%(\s*,\%(\s\|\n\)*\K\k*\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType

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

" Note: 'goStructTypeBlock' has 'nextgroup=goStructLiteralBlock' to handle
" anonymous struct type literals

syntax keyword goStructType struct contained skipempty skipwhite nextgroup=goStructTypeBlock
syntax region  goStructTypeBlock matchgroup=goStructTypeBraces start='{' end='}' extend contained contains=goEmbeddedType,goStructTypeField,goComment,goStructTypeTag,goDot,goSemicolon skipwhite nextgroup=goStructLiteralBlock
syntax region  goStructTypeTag   start='`'            end='`' contained
syntax region  goStructTypeTag   start='"' skip='\\"' end='"' contained oneline
syntax match   goStructTypeField /\%(_\|\K\k*\)\%(,\s*\%(_\|\K\k*\)\)*/ contained contains=goComma,goUnderscore skipwhite nextgroup=@goType
syntax match   goEmbeddedType    /\*\?\K\k*\%(\.\K\k*\)\?\%#\@1<!$/     contained contains=@goType

" It is technically possible to have a space between a struct name and the
" braces, but it's hard to reliably highlight
syntax match  goStructLiteral /\v\K\k*\ze%(\{|\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\]\{)/ contained nextgroup=goStructLiteralTypeArgs,goStructLiteralBlock
syntax region goStructLiteralTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goStructLiteralBlock

GoFoldStruct syntax region goStructLiteralBlock matchgroup=goStructBraces start='{' end='}' contained contains=goStructLiteralField,goComma,@goExpr

syntax match goStructLiteralField /\<\K\k*\ze:/ contained nextgroup=goStructLiteralColon
syntax match goStructLiteralColon /:/           contained

syntax keyword goInterfaceType interface contained skipwhite skipempty nextgroup=goInterfaceBlock
syntax region  goInterfaceBlock matchgroup=goInterfaceBraces start='{' end='}' contained contains=@goType,goTypeConstraintSymbols,goInterfaceMethod,goComment extend

syntax match   goInterfaceMethod            /\K\k*\ze(/ contained skipwhite nextgroup=goInterfaceMethodParams
syntax region  goInterfaceMethodParams      matchgroup=goInterfaceMethodParens start='('  end=')'  contained contains=goFuncTypeParam,goComma skipwhite nextgroup=@goType,goInterfaceMethodMultiReturn
syntax region  goInterfaceMethodMultiReturn matchgroup=goFuncMultiReturnParens start='('  end=')'  contained contains=goNamedReturnValue,goComma

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

syntax keyword goBuiltins append cap close complex copy delete imag len panic print println real recover contained skipwhite nextgroup=goFuncCallArgs

syntax keyword goMakeBuiltin make contained skipwhite nextgroup=goMakeBlock
syntax region  goMakeBlock   matchgroup=goFuncCallParens start='(' end=')' contained contains=@goType,@goExpr

syntax keyword goNewBuiltin new contained skipwhite nextgroup=goNewBlock
syntax region  goNewBlock   matchgroup=goFuncCallParens start='(' end=')' contained contains=@goType

hi link goBuiltins    Special
hi link goMakeBuiltin goBuiltins
hi link goNewBuiltin  goBuiltins

call s:HiConfig('goBuiltins', ['go_highlight_builtins'], #{offgroup: 'goFuncCall'})
call s:HiConfig('goFuncCallParens', ['go_highlight_function_call_parens'])

" }}} Builtins


" Flow Control {{{

syntax cluster goFlowControl contains=goIf,goElse,goFor,goForKeywords,goSwitch,goSelect,goSwitchKeywords

" 'goStatementStart' is used to avoid searching for 'goLabel' everywhere
syntax match   goLabel           /\K\k*\ze:/ contained

syntax keyword goIf              if     contained skipwhite skipempty nextgroup=goShortVarDecl
syntax keyword goElse            else   contained

syntax keyword goFor             for    contained skipwhite skipempty nextgroup=goShortVarDecl
syntax keyword goForKeywords     range break continue contained

syntax keyword goSwitch          switch contained skipwhite           nextgroup=goShortVarDecl
syntax keyword goSelect          select contained
syntax keyword goSwitchKeywords  case fallthrough default contained

syntax match   goSwitchTypeCase  /^\s\+case\s/ contained skipwhite nextgroup=@goType
syntax region  goSwitchTypeBlock matchgroup=goSwitchTypeBraces start='{' end='}' contained contains=goSwitchTypeCase,@goStatement

hi link goIf               Conditional
hi link goElse             goIf

hi link goFor              Repeat
hi link goForKeywords      goFor

hi link goSwitch           Conditional
hi link goSelect           goSwitch
hi link goSwitchKeywords   goSwitch

hi link goSwitchTypeBraces goBraces
hi link goSwitchTypeCase   goSwitchKeywords

" }}} Flow Control


" Misc {{{

syntax keyword goKeywords defer go contained

" goTypeAssertion is a part of the nextgroup list of goDotExpr
syntax region goTypeAssertion matchgroup=goParens start=/(/ end=/)/ contained contains=@goType
syntax match  goTypeAssertion /(type)/ contained contains=goParenBlock,goTypeDecl skipwhite nextgroup=goSwitchTypeBlock

hi link goKeywords Keyword

" }}} Misc

call s:Cleanup()

if !exists('main_syntax')
    let b:current_syntax = 'go'
endif

" vim:tw=80:fdm=marker:fmr={{{,}}}:
