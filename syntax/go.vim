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
"   contain complext nested types
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


" Structs {{{

" TODO: goStruct or goStructType?
syntax keyword goStructType struct skipempty skipwhite nextgroup=goStructTypeBlock
syntax region  goStructTypeBlock matchgroup=goStructTypeBraces start='{' end='}' extend contained contains=goEmbeddedType,goStructTypeField,goComment,goStructTypeTag,goDot,goSemicolon
syntax region  goStructTypeTag start='`' end='`' contained
syntax region  goStructTypeTag start='"' skip='\\"' end='"' contained
syntax match   goStructTypeField /\%(_\|\K\k*\)\%(,\s*\%(_\|\K\k*\)\)*/ contained skipwhite contains=goComma,goUnderscore nextgroup=@goType
" TODO: Highlight pointer for pointer embedded types
syntax match goEmbeddedType /\K\k*\%#\@<!$/ contained

" It is techically possible to have a space between a struct name and the
" braces, but it causes odd behaviour elsewhere
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

" }}} Structs


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

" Define defaults for non-standard groups that some users have
hi def link Brackets     Delimiter
hi def link Braces       Delimiter
hi def link Parens       Delimiter
hi def link FunctionCall Special
" hi def link Noise NONE
" hi def link Parameters NONE

" Constants and Literals

hi link goBooleanFalse Boolean
hi link goBooleanTrue Boolean
hi link goString String
" TODO: Link floats to Float
hi link goNumber Number
hi link goNil Constant
hi link goRawString String

" Package and Imports
" hi link goImport Include TODO: Is this correct?
hi link goImport Keyword

" Types

hi link goSimpleBuiltinTypes Type

" Functions

hi link goFuncDecl Keyword
hi link goFuncName Function
hi link goFuncType goFuncDecl

" Structs

if get(g:, 'go_highlight_struct_correctly', 0)
    hi link goStructType Structure
else
    hi link goStructType Keyword
endif

hi link goStructTypeTag PreProc
hi link goStructTypeBraces goBraces

hi link goStringEscape Special
hi link goConstDecl    Keyword
hi link goVarDecl      Keyword
hi link goOperator     Operator


hi link goStringFormat       SpecialChar
hi link goShortVarDecl       Identifier
hi link goInlineShortVarDecl goShortVarDecl
hi link goIf                 Conditional
hi link goReturn             Statement
hi link goTypeDecl           Keyword
hi link goTypeDeclName       Typedef
hi link goInterfaceType      goStructType
hi link goComment            Comment
hi link goGenerateComment    PreProc
hi link goCommentTodo        Todo

" TODO: Figure out what this should be

hi link goUnderscore Special

hi link goRepeat Repeat
hi link goFor goRepeat

hi link goRuneLiteral         Character
hi link goMap                 goSimpleBuiltinTypes
hi link goElse                Conditional
hi link goTypeAssign          Operator
hi link goTypeDeclGroupParens Parens

" " Keep this, but have an option to change it to 'Constant'
" hi link goInvalidRuneLiteral Error

hi def link goNoise Noise
hi link goDot goNoise
hi link goComma goNoise
hi link goSemicolon goNoise


hi link goPointer          Operator
hi link goSliceOrArray     Special
hi link goSliceOrArrayType Special
hi link goEmbeddedType     Special
hi link goChannel          Type
hi link goIota             Special
hi link goKeywords         Keyword
hi link goPackage          goKeywords
hi link goSwitch           goKeywords
hi link goSwitchKeywords   goKeywords
hi link goNonPrimitiveType Type
hi link goPackageName      Special
hi link goVariadic         Operator
hi link goStructValue      goNonPrimitiveType

hi link goBuiltins          Special
hi link goNewBuiltin        goBuiltins
hi link goMakeBuiltin       goBuiltins
hi link goTypeParamBrackets Special


hi def link goBraces   Braces
hi def link goBrackets Brackets
hi def link goParens   Parens

hi link goForBraces       goBraces
hi link goFuncBraces      goBraces
hi link goIfBraces        goBraces
hi link goInterfaceBraces goBraces
hi link goSliceBraces     goBraces
hi link goStructBraces    goBraces

hi link goMapBrackets goBrackets

hi link goFuncCallParens        goParens
hi link goFuncMultiReturnParens goParens
hi link goImportParens          goParens

hi def link FunctionParens Parens

hi link goFuncParens     FunctionParens
hi link goReceiverParens FunctionParens

hi def link goFuncCall FunctionCall

" TODO: Should this be "goParams" rather than "goParam"?
hi link goParam Parameters

" TODO: This isn't standard
hi link goImportItem Special
hi link goTypeParens goParens

hi link goInterfaceMethod       Identifier
hi link goInterfaceMethodParens FunctionParens




hi link goConstDeclParens goParens
hi link goVarDeclParens   goParens




" These groups are just used for structural purposes and don't really need to be
" highlighted, hence no "def link"

hi link goVarIdentifier      NONE
" hi link goVarIdentifier      Identifier
hi link goVarGroupIdentifier goVarIdentifier
hi link goFirstParen         NONE
hi link goFuncReturnType     NONE
hi link goFuncTypeParam      NONE
hi link goInvalidRuneLiteral NONE
hi link goNamedReturnValue   NONE
hi link goSliceItemType      NONE
hi link goStructTypeField    NONE
hi link goTypeConstraint     NONE
hi link goTypeParam          NONE

let b:current_syntax = 'go'

" vim:tw=80:fdm=marker:fmr={{{,}}}:
