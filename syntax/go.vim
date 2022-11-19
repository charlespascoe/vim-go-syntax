if exists("b:current_syntax") && !get(g:, 'go_highlight_override_existing_syntax', 1)
  finish
endif

syntax clear
syntax sync fromstart
syntax case match

" TODO: Add support for defining multiple types at once
" (https://go.dev/ref/spec#Underlying_types)
" TODO: Simplify basic keywords into one syntax group (maybe?)

" TODO: Maybe have highlighting for built-in functions etc?
" TODO: Check performance of lookbehinds
" TODO: Check correct use of 'skipempty'

" Notes on use of extend:
" - Struct and Interface need them so that simple matches (e.g. /struct {/) can
"   contain complex nested types
" - No other types should use extend

" Note on use of transparent: Most things use contains=TOP to allow them to
" behave predictably when they are nested within a region with special syntax
" elements (e.g. goVarDeclGroup and goConstDeclGroup). Only a handful of things
" should use 'transparent', particularly not top-level items.

syntax match   goDot        /\./
syntax match   goSemicolon  /;/
syntax match   goComma      /,/
syntax match   goOperator   /[-+*/!:=%&^<>|~]\+/
syntax keyword goUnderscore _

" Comments {{{

syntax keyword goCommentTodo     contained TODO FIXME XXX TBD NOTE
syntax region  goComment         start=+//+ end=+$+ contains=goCommentTodo keepend
syntax region  goComment         start=+/\*+ end=+\*/+ contains=goCommentTodo fold keepend
syntax match   goGenerateComment +//go:generate.*$+

" }}} Comments


" Literals {{{

syntax region goString             start='"' skip=/\\\\\|\\"/ end='"\|$' oneline contains=goStringEscape,goDoubleQuoteEscape,goStringFormat
syntax match  goStringEscape       /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\"])/ contained
syntax region goInvalidRuneLiteral start=+'+ skip=+\\'+ end=+'+ oneline keepend contains=goRuneLiteral
" TODO: Highlight escapes
syntax match goRuneLiteral /\v'%([^\\]|\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\']))'/ contained
syntax region goRawString start='`' end='`'
" TODO: Proper number matching
" TODO: Fix numbers matching in int64 etc.
syntax match goNumber /\<\c\d\+\%(\.\d\+\)\?\%(e[-+]\d+\)\?\>/
syntax keyword goNil nil
syntax keyword goBooleanTrue true
syntax keyword goBooleanFalse false
" TODO: float formatting, flags (https://pkg.go.dev/fmt)
syntax match goStringFormat /\v\%%([%EFGOTUXbcdefgopqstvxf])/ contained

" }}} Literals


" Simple Blocks {{{

syntax region goBracketBlock matchgroup=goBrackets start='\[' end='\]' transparent extend
syntax region goParenBlock   matchgroup=goParens   start='('  end=')'  transparent extend
syntax region goBraceBlock   matchgroup=goBraces   start='{'  end='}'  transparent extend

" }}}


" Constants and Variables {{{

syntax keyword goConstDecl const skipempty skipwhite nextgroup=goVarIdentifier,goConstDeclGroup
syntax keyword goVarDecl   var   skipempty skipwhite nextgroup=goVarIdentifier,goVarDeclGroup

syntax region goVarDeclGroup   matchgroup=goVarDeclParens   start='(' end=')' contained contains=TOP
syntax region goConstDeclGroup matchgroup=goConstDeclParens start='(' end=')' contained contains=TOP

syntax match goVarIdentifier      /\<\K\k*/ contained skipwhite nextgroup=@goType
syntax match goVarGroupIdentifier /\%(\%(^\|;\|\%(const\|var\)\s\+(\?\)\s*\)\@40<=\K\k*/ contained containedin=goConstDeclGroup,goVarDeclGroup skipwhite nextgroup=@goType

syntax keyword goIota iota contained containedin=goConstDeclGroup

" TODO: Is it possible to reduce duplication here? Remember performance!
" NOTE: goShortVarDecl currently doesn't work inside one-line functions,
" e.g func() { a, b := f(); return a }
syntax match goShortVarDecl       /^\s*\zs\K\k*\%(\s*,\s*\%(\K\k*\)\?\)*\ze\s*:=/ contains=goComma,goUnderscore
syntax match goInlineShortVarDecl /\K\k*\%(\s*,\s*\%(\K\k*\)\?\)*\ze\s*:=/        contains=goComma,goUnderscore contained

" }}} Constants and Variables


" Packages {{{

syntax keyword goPackage package
syntax keyword goImport import skipwhite nextgroup=goImportItem,goImports
syntax region  goImports matchgroup=goImportParens start='(' end=')' contained contains=goImportItem,goComment
syntax match   goImportItem /\(\([\._]\|\K\k*\)\s\+\)\?"[^"]*"/ contained contains=@NoSpell,goString

" }}} Packages


" Types {{{

syntax match  goPointer /*/ contained nextgroup=@goType
syntax region goTypeParens start='(' end=')' contained contains=@goType

syntax keyword goTypeDecl type skipempty skipwhite nextgroup=goTypeDeclName,goTypeDeclGroup
syntax region  goTypeDeclGroup matchgroup=goTypeDeclGroupParens start='(' end=')' contained contains=goTypeDeclName,goComment
syntax match   goTypeDeclName /\K\k*/ contained skipempty skipwhite nextgroup=goTypeDeclTypeParams,goTypeAssign,@goType
syntax region  goTypeDeclTypeParams matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=@goType
syntax match   goTypeAssign /=/ contained skipwhite nextgroup=@goType

syntax cluster goType contains=goSimpleBuiltinTypes,goFuncType,goStructType,goInterfaceType,goMap,goSliceOrArrayType,goChannel,goNonPrimitiveType,goPointer,goTypeParens

syntax match goNonPrimitiveType /\%(\K\k*\.\)*\K\k*\[\?/ contained contains=goPackageName,goDot,goTypeArgs
syntax match goPackageName /[\.[:keyword:]]\@1<!\K\k*\ze\./ contained

" TODO: Try to reduce type arg declarations
syntax region goTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma

syntax keyword goSimpleBuiltinTypes any bool byte complex128 complex64 error float32 float64 int int8 int16 int32 int64 rune string uint uint8 uint16 uint32 uint64 uintptr

" TODO: Can function types have type params?
syntax match  goFuncType /func\s*(/ contained skipwhite contains=goFuncTypeParens skipwhite nextgroup=@goType,goFuncTypeMultiReturnType
syntax region goFuncTypeParens matchgroup=goFuncParens start='(' end=')' contained contains=goFuncTypeParam,goComma
syntax region goFuncTypeMultiReturnType matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma

syntax keyword goMap map skipempty skipwhite nextgroup=goMapKeyType
syntax region  goMapKeyType matchgroup=goMapBrackets start='\[' end='\]' contained contains=@goType skipwhite nextgroup=@goType

syntax match goSliceOrArrayType /\[\%(\d\+\|\.\.\.\)\?\]/ contained contains=goNumber,goDot skipwhite nextgroup=@goType

" A lookbehind is used to distinguish a new slice value with slice indexing.
" The lookbehind has variable length, so it has a reasonable 20 character limit
syntax match goSliceOrArray /\k\@<!\[\%(\d\+\|\.\.\.\)\?\]\ze\%(\K\|\[\|(\)/ contains=goNumber,goDot skipwhite nextgroup=goSliceItemType

" Only look to the end of the line for the item type, and let slices etc. extend
" across lines as necessary. Note the first '(' is to match the first paren
" around the type, which is then extended by goTypeParens.
syntax match goSliceItemType /(\|\%(\%(interface\|struct\)\s*{\|[^{()]\)\+/ contained contains=@goType skipwhite nextgroup=goSliceItems

syntax region goSliceItems matchgroup=goSliceBraces start='{' end='}' contained contains=TOP

" syntax match goChannel /<-chan\|chan\%(<-\)\?/ contains=goOperator skipwhite nextgroup=@goType
" syntax match goChannel /chan/ skipwhite nextgroup=@goType
syntax match goChannel /<-chan/ skipwhite contains=goOperator nextgroup=@goType
syntax match goChannel /chan\%(<-\)\?/ skipwhite contains=goOperator nextgroup=@goType

" }}} Types


" Functions {{{

" Unfortunately limited to at most 3 nested type args
syntax match  goFuncCall /\v<\K\k*\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\(/ nextgroup=goFuncCallTypeArgs,goFuncCallArgs
syntax region goFuncCallTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goFuncCallArgs
syntax region goFuncCallArgs     matchgroup=goFuncCallParens    start='('  end=')'  contained contains=TOP

syntax keyword goFuncDecl func skipempty skipwhite nextgroup=goMethodReceiver,goFuncName,goFuncParams

syntax match goVariadic /\.\.\./ contained skipwhite nextgroup=@goType

syntax match goParam /^\s*\zs\K\k*/               contained skipempty skipwhite nextgroup=goParam,goVariadic,@goType
syntax match goParam /\%([(,]\s*\)\@20<=\zs\K\k*/ contained skipempty skipwhite nextgroup=goParam,goVariadic,@goType

syntax match goFuncName /\K\k*/ contained skipwhite nextgroup=goFuncTypeParams,goFuncParams

syntax region goFuncTypeParams matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=goFuncParams

" TODO: is skipempty needed?
syntax match goTypeParam /\%(\%(^\|[\[,]\)\s*\)\@20<=\zs\K\k*/ contained skipempty skipwhite nextgroup=goTypeParam,goTypeConstraint

" This is a region to allow use of types that have commas (e.g. function
" definitions) or nested type parameters, because they will automatically extend
" the match of the region
" TODO: Specific operators
syntax region goTypeConstraint start='\s'ms=e+1 end=/[,\]]/me=s-1 contained contains=@goType,goOperator


syntax region goFuncParams      matchgroup=goFuncParens start='(' end=')' contained contains=goParam,goComma skipwhite nextgroup=goFuncReturnType,goFuncMultiReturn,goFuncBlock
syntax match  goFuncReturnType  /\s*\zs(\@<!\%(\%(interface\|struct\)\s*{\|[^{]\)\+{\@<!/ contained contains=@goType skipempty skipwhite nextgroup=goFuncBlock
syntax region goFuncMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma skipempty skipwhite nextgroup=goFuncBlock
" syntax region goFuncMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=@goType,goComma skipempty skipwhite nextgroup=goFuncBlock
syntax region goFuncBlock matchgroup=goFuncBraces start='{' end='}' contained contains=TOP skipwhite nextgroup=goFuncCallArgs


syntax match  goMethodReceiver /([^,]\+)\ze\s\+\K\k*\s*(/ contained contains=goReceiverBlock skipempty skipwhite nextgroup=goFuncName
syntax region goReceiverBlock matchgroup=goReceiverParens start='(' end=')' contained contains=goParam

" TODO: Check performance of the backtracking on these
" These are both the same, only defined separately for highlighting purposes
syntax match goNamedReturnValue /\%(^\|[(,]\)\@<=\s*\zs\%(\K\k*\%(\s*,\%(\s\|\n\)*\K\k*\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType
syntax match goFuncTypeParam    /\%(^\|[(,]\)\@<=\s*\zs\%(\K\k*\%(\s*,\%(\s\|\n\)*\K\k*\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType

syntax keyword goReturn return

" }}} Functions


" Structs and Interfaces {{{

syntax keyword goStructType struct skipempty skipwhite nextgroup=goStructTypeBlock
syntax region  goStructTypeBlock matchgroup=goStructTypeBraces start='{' end='}' extend contained contains=goEmbeddedType,goStructTypeField,goComment,goStructTypeTag,goDot,goSemicolon
syntax region  goStructTypeTag start='`' end='`' contained
syntax region  goStructTypeTag start='"' skip='\\"' end='"' contained
syntax match   goStructTypeField /\%(_\|\K\k*\)\%(,\s*\%(_\|\K\k*\)\)*/ contained skipwhite contains=goComma,goUnderscore nextgroup=@goType
syntax match   goEmbeddedType /\*\?\%(\K\k*\.\)\?\K\k*\%#\@<!$/ contained contains=@goType

" It is technically possible to have a space between a struct name and the
" braces, but it's hard to reliably highlight
syntax match goStructValue /\v<%(\K\k*\.)*\K\k*\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\{/ contains=goPackageName,goDot nextgroup=goStructValueTypeArgs,goStructBlock
syntax region goStructValueTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goStructBlock
syntax region goStructBlock matchgroup=goStructBraces start='{' end='}' contained contains=TOP

" Interfaces

syntax keyword goInterfaceType interface skipempty skipwhite nextgroup=goInterfaceBlock
" TODO: Maybe don't just put goOperator in here and instead use the correct
" symbols
syntax region goInterfaceBlock matchgroup=goInterfaceBraces start='{' end='}' contained extend contains=@goType,goOperator,goInterfaceMethod,goComment
syntax match  goInterfaceMethod /\K\k*\ze\s*(/ contained skipwhite nextgroup=goInterfaceMethodParams
syntax region goInterfaceMethodParams matchgroup=goInterfaceMethodParens start='(' end=')' contained contains=goFuncTypeParam,goComma skipwhite nextgroup=@goType,goInterfaceMethodMultiReturn
syntax region goInterfaceMethodMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma

" }}} Structs and Interfaces


" Make and New {{{

syntax keyword goMakeBuiltin make nextgroup=goMakeBlock
syntax region  goMakeBlock matchgroup=goParens start='(' end=')' contained contains=TOP
" TODO: Fix this (multiline)
syntax match goFirstParen /\%(make(\)\@5<=/ contained skipempty skipwhite nextgroup=@goType containedin=goMakeBlock
" syntax region goMakeType start='\%(\<make(\n\?\s*\)\@40<=' end=',\|$' contained containedin=goMakeBlock
"contains=@goType
" hi link goMakeType Error

syntax keyword goNewBuiltin new skipwhite nextgroup=goNewBlock
syntax region  goNewBlock matchgroup=goParens start='(' end=')' contained contains=@goType

" }}} Make and New

" TODO: Field access?

" If {{{

" TODO: Figure out how to remove goInlineShortVarDecl; this could simplify if,
" for, and switch
syntax keyword goIf if skipempty skipwhite nextgroup=goInlineShortVarDecl
syntax keyword goElse else

" For
syntax keyword goFor for skipempty skipwhite nextgroup=goInlineShortVarDecl
syntax keyword goRepeat range break continue

" Switch and Select
syntax keyword goSwitch switch skipwhite nextgroup=goInlineShortVarDecl
syntax keyword goSwitchKeywords case fallthrough default select

" }}} If

" Labels TODO

" Misc {{{
" TODO: Make this a catch-all for various keywords
" TODO: Is "range" technically an operator?
syntax keyword goKeywords defer go range
" This has to use a lookbehind, otherwise goDot steals the dot
syntax region goTypeAssertion matchgroup=goParens start=/\.\@1<=(/ end=/)/ contains=@goType,goTypeDecl

" }}} Misc

" TODO: Statement vs Keyword?

"Highlighting

fun s:getconfig(prefix, keys, default)
    if len(a:keys) == 0
        return a:default
    else
        return get(g:, a:prefix.a:keys[0], s:getconfig(a:prefix, a:keys[1:], a:default))
    endif
endfun

fun s:HiConfig(group, options)
    let l:opt = s:getconfig('go_highlight_', a:options, 1)
    let l:cmd = ''

    if type(l:opt) == v:t_string
        if l:opt =~ '^[[:alnum:]]\+$'
            exec 'hi link '.a:group.' '.l:opt
        else
            exec 'hi '.a:group.' '.l:opt
        endif
    elseif !l:opt
        exec 'hi link '.a:group.' NONE'
    endif
endfun

call s:HiConfig('goBraces',     ['braces'])
call s:HiConfig('goBrackets',   ['brackets'])
call s:HiConfig('goParens',     ['parens'])
call s:HiConfig('goComma',      ['comma','separators'])
call s:HiConfig('goDot',        ['dot','separators'])
call s:HiConfig('goSemicolon',  ['semicolon','separators'])
call s:HiConfig('goFuncName',   ['functions'])
call s:HiConfig('goFuncParens', ['function_parens','parens'])
call s:HiConfig('goFuncBraces', ['function_braces','braces'])
call s:HiConfig('goFuncCall',   ['function_calls'])


hi def link goBraces    Delimiter
hi def link goBrackets  Delimiter
hi def link goParens    Delimiter


hi def link goFuncName       Function
hi def link goFuncParens     Delimiter
hi def link goReceiverParens goFuncParens
hi def link goFuncCall       Type
hi def link goFuncCallParens Delimiter
hi def link goParam          Identifier
" hi def link goFuncTypeParam goParam
hi def link goFuncTypeParam NONE

hi def link goGenerateComment PreProc
hi def link goOperator        Operator
hi def link goVarIdentifier   Identifier
hi def link goStringFormat    SpecialChar
hi def link goTypeDeclName    Typedef

" Constants and Literals

hi def link goBooleanFalse Boolean
hi def link goBooleanTrue Boolean
hi def link goString String
" TODO: Link floats to Float
hi def link goNumber Number
hi def link goNil Constant
hi def link goRawString String

" Package and Imports
" hi link goImport Include TODO: Is this correct?
hi def link goImport Keyword

" Types

hi def link goSimpleBuiltinTypes Type

" Functions

hi def link goFuncDecl Keyword
" hi def link goFuncName Function
hi def link goFuncType goFuncDecl

" Structs

hi def link goStructType Keyword
hi def link goStructTypeTag PreProc
hi def link goStructTypeBraces goBraces

hi def link goStringEscape Special
hi def link goConstDecl    Statement
hi def link goVarDecl      Statement

" Declarations

" hi def link goVarIdentifier      Identifier
hi def link goInlineShortVarDecl goVarIdentifier
hi def link goShortVarDecl       goVarIdentifier
hi def link goVarGroupIdentifier goVarIdentifier

hi def link goConstDeclParens goParens
hi def link goVarDeclParens   goParens



hi def link goIf                 Conditional
hi def link goReturn             Statement
hi def link goTypeDecl           Keyword
hi def link goInterfaceType      goStructType
hi def link goComment            Comment
hi def link goCommentTodo        Todo

" TODO: Figure out what this should be

hi def link goUnderscore Special

hi def link goRepeat Repeat
hi def link goFor goRepeat

hi def link goRuneLiteral         Character
hi def link goMap                 goSimpleBuiltinTypes
hi def link goElse                Conditional
hi def link goTypeAssign          goOperator
hi def link goTypeDeclGroupParens goParens

" " Keep this, but have an option to change it to 'Constant'
" hi link goInvalidRuneLiteral Error

" hi def link goDelimiters Noise
hi def link goDot       goOperator
hi def link goComma     goOperator
hi def link goSemicolon goOperator


hi def link goPointer          goOperator
hi def link goSliceOrArray     Special
hi def link goSliceOrArrayType Special
hi def link goChannel          Type
hi def link goIota             Special
hi def link goKeywords         Keyword
hi def link goPackage          Keyword
hi def link goSwitch           Conditional
hi def link goSwitchKeywords   Conditional
hi def link goNonPrimitiveType Type
hi def link goPackageName      Special
hi def link goVariadic         goOperator
hi def link goStructValue      goNonPrimitiveType

hi def link goBuiltins          Special
hi def link goNewBuiltin        goBuiltins
hi def link goMakeBuiltin       goBuiltins
hi def link goTypeParamBrackets Special

hi def link goForBraces       goBraces
hi def link goIfBraces        goBraces
hi def link goSliceBraces     goBraces
hi def link goStructBraces    goBraces
hi def link goInterfaceBraces goBraces
hi def link goFuncBraces      goBraces

hi def link goMapBrackets goBrackets

hi def link goFuncCallParens        goParens
hi def link goFuncMultiReturnParens goParens
hi def link goImportParens          goParens

hi def link goImportItem Special
hi def link goTypeParens goParens

hi def link goInterfaceMethod       goFuncName
hi def link goInterfaceMethodParens goFuncParens


" These groups are just used for structural purposes and don't really need to be
" highlighted, hence no "def link"

hi link goFirstParen         NONE
hi link goFuncReturnType     NONE
hi link goInvalidRuneLiteral NONE
hi link goNamedReturnValue   NONE
hi link goSliceItemType      NONE
hi link goStructTypeField    NONE
hi link goTypeConstraint     NONE
hi def link goTypeParam          Special

let b:current_syntax = 'go'

" vim:tw=80:fdm=marker:fmr={{{,}}}:
