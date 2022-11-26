if !exists('main_syntax')
    if exists("b:current_syntax") && !get(g:, 'go_highlight_override_existing_syntax', 1)
        finish
    endif

    syntax sync fromstart

    let b:__vim_go_syntax = 1
endif

syntax clear
syntax case match

" iskeyword includes colon to allow goStructValueField to take precedence over
" goImportedPackages; if not for this, syntax keywords would frequently take
" precedence over pattern matches. The two negative effects of this are:
"   1) not being able to use \K and \k, which isn't a problem for Go
"   2) 'default:' keyword must include the colon (see goSwitchKeywords)
" TODO: Try find a way to remove this (see goStructValueField)
syntax iskeyword @,48-57,_,192-255,:

" TODO: Syntax Folding

" TODO: Add support for defining multiple types at once
" (https://go.dev/ref/spec#Underlying_types)

" TODO: Maybe have highlighting for built-in functions etc?
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

" 'goDotExpr' matches a dot that is found as a part of an expression, whereas
" 'goDot' is used to highlight a dot in non-expression contexts (e.g. the dot
" between a package and a type). 'goDotExpr' significantly improves the
" performance of searching for fields and type assertions.
syntax match goDot     /\./     contained
syntax match goDotExpr /\./     skipwhite skipempty nextgroup=goFuncCall,goTypeAssertion,goField,goStructValue,goEmptyLine
syntax match goField   /\<\w\+/ contained

" 'goEmptyLine' is used to prevent odd highlighting behaviour when the current
" line ends in a dot while the user is typing (see 'nextgroup' of 'goDotExpr')
syntax match goEmptyLine /^$/ contained

" TODO: Only valid operators?
syntax match   goOperator     /[-+*/!:=%&^<>|~]\+/
syntax match   goComma        /,/
syntax match   goSemicolon    /;/
syntax keyword goUnderscore   _

hi link goOperator   Operator
hi link goDot        goOperator
hi link goDotExpr    goDot
hi link goComma      goOperator
hi link goSemicolon  goOperator
hi link goUnderscore Special
hi link goField      Identifier

call s:HiConfig('goField',     ['go_highlight_fields'], #{default: 0})
call s:HiConfig('goOperator',  ['go_highlight_operators'])
call s:HiConfig('goDot',       ['go_highlight_dot','go_highlight_separators'])
call s:HiConfig('goComma',     ['go_highlight_comma','go_highlight_separators'])
call s:HiConfig('goSemicolon', ['go_highlight_semicolon','go_highlight_separators'])

" }}} Misc


" Comments {{{

syntax keyword goCommentTodo     contained TODO FIXME XXX TBD NOTE
syntax region  goComment         start=+//+  end=+$+   contains=@goCommentSpell,goCommentTodo keepend
syntax region  goComment         start=+/\*+ end=+\*/+ contains=@goCommentSpell,goCommentTodo keepend
syntax match   goGenerateComment +//go:generate.*$+

hi link goCommentTodo     Todo
hi link goComment         Comment
hi link goGenerateComment PreProc

call s:HiConfig('goGenerateComment', ['go_highlight_generate_tags'], #{offgroup: 'goComment'})

" }}} Comments


" Literals {{{

syntax region goString       start='"' skip=/\\\\\|\\"/ end='"\|$' oneline contains=@goStringSpell,goStringEscape,goDoubleQuoteEscape,goStringFormat
syntax match  goStringEscape /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\"])/ contained
syntax match  goStringFormat /\v\%%(\%|[-+# 0]*%([1-9]\d*|\*)?%(\.%(\d+|\*)?)?[EFGOTUXbcdefgopqstvxf])/ contained

" 'goInvalidRuneLiteral' is a loose match for all single-quote sequences; they
" are highlighted as errors. If they contain a valid 'goRuneLiteral' or the
" cursor is present at the end, then the 'goRuneLiteral' highlighting will
" override the 'goInvalidRuneLiteral' highlighting and thus look like a string.
syntax region goInvalidRuneLiteral start=+'+ skip=+\\'+ end=+'+ keepend oneline contains=goRuneLiteral
syntax match  goRuneLiteral        /\v'%(.*%#|[^\\]|\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\']))'/ contained contains=goRuneLiteralEscape
syntax match  goRuneLiteralEscape  /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\'])/ contained

syntax region goRawString start='`' end='`' keepend

" TODO: Proper number matching
" TODO: Fix numbers matching in int64 etc.
syntax match goNumber /\<\c\d\+\%(\.\d\+\)\?\%(e[-+]\d+\)\?\>/

syntax keyword goBooleanTrue  true
syntax keyword goBooleanFalse false

syntax keyword goNil nil

hi link goString             String
hi link goStringEscape       SpecialChar
hi link goStringFormat       SpecialChar

hi link goInvalidRuneLiteral Error
hi link goRuneLiteral        Character
hi link goRuneLiteralEscape  goStringFormat

hi link goRawString          String

hi link goNumber             Number

hi link goBooleanTrue        Boolean
hi link goBooleanFalse       Boolean

hi link goNil                Constant

call s:HiConfig('goStringFormat',       ['go_highlight_format_strings'], #{offgroup: 'goString'})
call s:HiConfig('goInvalidRuneLiteral', ['go_highlight_rune_literal_error'])

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

" goVarAssign and goShortVarDecl

let s:assignOrShortDecl = 0

if s:HiConfig('goVarAssign', ['go_highlight_variable_assignments'], #{default: 0})
    " TODO: Only valid operators?
    syntax match goVarAssign /\<\w\+\%(\s*,\s*\%(\w\+\)\?\)*\ze\s*[-+*/!%&^<>|~]*=/ contains=goComma,goUnderscore contained
    let s:assignOrShortDecl = 1
endif

if s:HiConfig('goShortVarDecl', ['go_highlight_short_variable_declarations','go_highlight_variable_declarations'])
    syntax match goShortVarDecl /\<\w\+\%(\s*,\s*\%(\w\+\)\?\)*\ze\s*:=/ contains=goComma,goUnderscore contained
    let s:assignOrShortDecl = 1
endif

if s:assignOrShortDecl
    " This lookbehind is checked for every character, which is why
    " goStatementStart is conditional and only added if needed. Splitting this
    " into two seems to make it slightly faster overall.
    syntax match goStatementStart /[{;]\@1<=/ contained containedin=goFuncBlock,goSwitchTypeBlock skipwhite nextgroup=goVarAssign,goShortVarDecl
    syntax match goStatementStart /^\ze\s/    contained containedin=goFuncBlock,goSwitchTypeBlock skipwhite nextgroup=goVarAssign,goShortVarDecl
endif

syntax keyword goConstDecl const skipempty skipwhite nextgroup=goVarIdentifier,goConstDeclGroup
syntax keyword goVarDecl   var   skipempty skipwhite nextgroup=goVarIdentifier,goVarDeclGroup

syntax region goVarDeclGroup   matchgroup=goVarDeclParens   start='(' end=')' contained contains=TOP,@Spell
syntax region goConstDeclGroup matchgroup=goConstDeclParens start='(' end=')' contained contains=TOP,@Spell

syntax match goVarIdentifier      /\<\w\+/ contained skipwhite nextgroup=@goType
syntax match goVarGroupIdentifier /\%(^\|;\|\%(const\|var\)\s\+(\)\@20<=\s*\zs\w\+/ contained containedin=goConstDeclGroup,goVarDeclGroup skipwhite nextgroup=@goType

syntax keyword goIota iota contained containedin=goConstDeclGroup

hi link goConstDecl          Statement
hi link goVarDecl            Statement

hi link goConstDeclParens    goParens
hi link goVarDeclParens      goParens

hi link goVarIdentifier      Identifier
hi link goVarGroupIdentifier goVarIdentifier
hi link goShortVarDecl       Identifier
hi link goShortVarDecl goShortVarDecl

hi link goVarAssign          Special

hi link goIota               Special

call s:HiConfig('goVarIdentifier', ['go_highlight_variable_declarations'])

" }}} Constants and Variables


" Packages {{{

syntax keyword goPackage      package
syntax keyword goImport       import skipwhite nextgroup=goImportItem,goImports
syntax region  goImports      matchgroup=goImportParens start='(' end=')' contained contains=goImportItem,goComment
syntax match   goImportItem   /\(\([\._]\|\w\+\)\s\+\)\?"[^"]*"/ contained contains=goImportString
syntax region  goImportString start='"' end='"' keepend contained

hi link goPackage      Keyword
hi link goImport       Keyword
hi link goImportItem   Special
hi link goImportString goString
hi link goImportParens goParens

" }}} Packages


" Types {{{

syntax match  goPointer /*/ contained nextgroup=@goType
syntax region goTypeParens start='(' end=')' contained contains=@goType

syntax keyword goTypeDecl type skipempty skipwhite nextgroup=goTypeDeclName,goTypeDeclGroup
syntax region  goTypeDeclGroup matchgroup=goTypeDeclGroupParens start='(' end=')' contained contains=goTypeDeclName,goComment
syntax match   goTypeDeclName /\w\+/ contained skipempty skipwhite nextgroup=goTypeDeclTypeParams,goTypeAssign,@goType
syntax region  goTypeDeclTypeParams matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=@goType
syntax match   goTypeAssign /=/ contained skipwhite nextgroup=@goType

syntax cluster goType contains=goSimpleBuiltinTypes,goFuncType,goStructType,goInterfaceType,goMap,goSliceOrArrayType,goChannel,goNonPrimitiveType,goPointer,goTypeParens

syntax match goNonPrimitiveType /\<\w\+\%(\.\w\+\)\?\[\?/ contained contains=goPackageName,goDot,goTypeArgs
syntax match goPackageName /\<\w\+\ze\./ contained nextgroup=goDot

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
syntax match goSliceOrArray /\w\@1<!\[\%(\d\+\|\.\.\.\)\?\]\ze\%(\w\|\[\|(\)/ contains=goNumber,goDot skipwhite nextgroup=goSliceItemType

" Only look to the end of the line for the item type, and let slices etc. extend
" across lines as necessary. Note the first '(' is to match the first paren
" around the type, which is then extended by goTypeParens.
syntax match goSliceItemType /(\|\%(\%(interface\|struct\)\s*{\|[^{()]\)\+/ contained contains=@goType skipwhite nextgroup=goSliceItems

syntax region goSliceItems matchgroup=goSliceBraces start='{' end='}' contained contains=TOP,@Spell

syntax match goChannel /<-chan/ skipwhite contains=goOperator nextgroup=@goType
syntax match goChannel /chan\%(<-\)\?/ skipwhite contains=goOperator nextgroup=@goType


hi link goPointer               goOperator

" goTypeDecl should technically link to Typedef, but it looks a bit odd.
hi link goTypeDecl              Keyword
hi link goTypeParens            goParens
hi link goTypeDeclGroupParens   goParens
hi link goTypeDeclName          Typedef
hi link goTypeParamBrackets     goBrackets
hi link goTypeAssign            goOperator

hi link goPackageName           Special

hi link goNonPrimitiveType      Type
hi link goSimpleBuiltinTypes    Type
hi link goMap                   goSimpleBuiltinTypes
hi link goMapBrackets           Delimiter
hi link goSliceOrArray          Delimiter
hi link goSliceOrArrayType      goSliceOrArray
hi link goSliceBraces           goBraces
hi link goChannel               Type

hi link goFuncType              goFuncDecl
" See 'Functions' for other function highlight groups

call s:HiConfig('goTypeDeclName', ['go_highlight_types'])
call s:HiConfig('goSliceOrArray', ['go_highlight_slice_brackets'])
call s:HiConfig('goMapBrackets',  ['go_highlight_map_brackets'])

" }}} Types


" Functions {{{

" Unfortunately limited to at most 3 nested type args
" TODO: Figure out a better alternative to the long containedin (some kind of
" expression group?)
syntax match  goFuncCall /\v<\w+\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\(/ contained containedin=goFuncBlock,goSwitchTypeBlock,goStructBlock,goVarDeclGroup,goConstDeclGroup,goFuncCallArgs,goMakeBlock nextgroup=goFuncCallTypeArgs,goFuncCallArgs
syntax region goFuncCallTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goFuncCallArgs
syntax region goFuncCallArgs     matchgroup=goFuncCallParens    start='('  end=')'  contained contains=TOP,@Spell

syntax keyword goFuncDecl func skipempty skipwhite nextgroup=goMethodReceiver,goFuncName,goFuncParams

syntax match goVariadic  /\.\.\./ contained skipwhite nextgroup=@goType
syntax match goArgSpread /\.\.\./ contained containedin=goFuncCallArgs

" TODO: Should this be "goParams" rather than "goParam"?
syntax match goParam /^\s*\zs\w\+/               contained skipempty skipwhite nextgroup=goParam,goVariadic,@goType
syntax match goParam /\%([(,]\s*\)\@20<=\zs\w\+/ contained skipempty skipwhite nextgroup=goParam,goVariadic,@goType

syntax match goFuncName /\w\+/ contained skipwhite nextgroup=goFuncTypeParams,goFuncParams

syntax region goFuncTypeParams matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=goTypeParam,goComma nextgroup=goFuncParams

" TODO: is skipempty needed?
syntax match goTypeParam /\%(\%(^\|[\[,]\)\s*\)\@20<=\zs\w\+/ contained skipempty skipwhite nextgroup=goTypeParam,goTypeConstraint

" This is a region to allow use of types that have commas (e.g. function
" definitions) or nested type parameters, because they will automatically extend
" the match of the region
" TODO: Specific operators
syntax region goTypeConstraint start='\s'ms=e+1 end=/[,\]]/me=s-1 contained contains=@goType,goOperator

syntax region goFuncParams      matchgroup=goFuncParens start='(' end=')' contained contains=goParam,goComma skipwhite nextgroup=goFuncReturnType,goFuncMultiReturn,goFuncBlock
syntax match  goFuncReturnType  /\s*\zs(\@<!\%(\%(interface\|struct\)\s*{\|[^{]\)\+{\@<!/ contained contains=@goType skipempty skipwhite nextgroup=goFuncBlock
syntax region goFuncMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma skipempty skipwhite nextgroup=goFuncBlock
" syntax region goFuncMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=@goType,goComma skipempty skipwhite nextgroup=goFuncBlock
syntax region goFuncBlock matchgroup=goFuncBraces start='{' end='}' contained contains=TOP,@Spell skipwhite nextgroup=goFuncCallArgs

syntax match  goMethodReceiver /([^,]\+)\ze\s\+\w\+\s*(/ contained contains=goReceiverBlock skipempty skipwhite nextgroup=goFuncName
syntax region goReceiverBlock matchgroup=goReceiverParens start='(' end=')' contained contains=goParam

syntax match goFuncTypeParam    /\%(^\|[(,]\)\@1<=\s*\zs\%(\w\+\%(\s*,\%(\s\|\n\)*\w\+\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType,goVariadic
syntax match goNamedReturnValue /\%(^\|[(,]\)\@1<=\s*\zs\%(\w\+\%(\s*,\%(\s\|\n\)*\w\+\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType

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
hi link goTypeParam             Identifier


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

syntax keyword goStructType struct skipempty skipwhite nextgroup=goStructTypeBlock
syntax region  goStructTypeBlock matchgroup=goStructTypeBraces start='{' end='}' extend contained contains=goEmbeddedType,goStructTypeField,goComment,goStructTypeTag,goDot,goSemicolon
syntax region  goStructTypeTag start='`' end='`' contained
syntax region  goStructTypeTag start='"' skip='\\"' end='"' oneline contained
syntax match   goStructTypeField /\%(_\|\w\+\)\%(,\s*\%(_\|\w\+\)\)*/ contained skipwhite contains=goComma,goUnderscore nextgroup=@goType
syntax match   goEmbeddedType /\*\?\w\+\%(\.\w\+\)\?\%#\@1<!$/ contained contains=@goType

" It is technically possible to have a space between a struct name and the
" braces, but it's hard to reliably highlight
syntax match  goStructValue /\v<\w+%(\.\w+)?\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\{/ contains=goPackageName,goDot nextgroup=goStructValueTypeArgs,goStructBlock
syntax region goStructValueTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goStructBlock
syntax region goStructBlock matchgroup=goStructBraces start='{' end='}' contained contains=TOP,@Spell
syntax match  goStructValueField /\<\w\+\ze:/ contained containedin=goStructBlock

syntax keyword goInterfaceType interface skipempty skipwhite nextgroup=goInterfaceBlock
" TODO: Maybe don't just put goOperator in here and instead use the correct
" symbols
syntax region goInterfaceBlock matchgroup=goInterfaceBraces start='{' end='}' contained extend contains=@goType,goOperator,goInterfaceMethod,goComment
syntax match  goInterfaceMethod /\<\w\+\ze\s*(/ contained skipwhite nextgroup=goInterfaceMethodParams
syntax region goInterfaceMethodParams matchgroup=goInterfaceMethodParens start='(' end=')' contained contains=goFuncTypeParam,goComma skipwhite nextgroup=@goType,goInterfaceMethodMultiReturn
syntax region goInterfaceMethodMultiReturn matchgroup=goFuncMultiReturnParens start='(' end=')' contained contains=goNamedReturnValue,goComma

hi link goStructType       Keyword
hi link goStructTypeBraces goBraces
hi link goStructTypeField  NONE
hi link goStructTypeTag    PreProc
hi link goStructValue      goNonPrimitiveType
hi link goStructValueField Identifier
hi link goStructBraces     goBraces

hi link goInterfaceType         goStructType
hi link goInterfaceBraces       goBraces
hi link goInterfaceMethod       goFuncName
hi link goInterfaceMethodParens goFuncParens

call s:HiConfig('goStructTypeTag',    ['go_highlight_struct_tags'])
call s:HiConfig('goStructValueField', ['go_highlight_struct_fields'], #{default: 1})

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

" }}} Builtins

" TODO: Field access?

" Flow Control {{{

" TODO: Figure out how to remove goShortVarDecl; this could simplify if,
" for, and switch
syntax keyword goIf   if skipempty skipwhite nextgroup=goShortVarDecl
syntax keyword goElse else

syntax keyword goFor         for skipempty skipwhite nextgroup=goShortVarDecl
syntax keyword goForKeywords range break continue

syntax keyword goSwitch         switch skipwhite nextgroup=goShortVarDecl
syntax keyword goSelect         select
syntax keyword goSwitchKeywords case fallthrough
" This is an unfortunate side-effect of setting 'syntax iskeyword' to include
" colon to make 'goStructValueField' matching fast and correct.
" TODO: Figure out how to correctly highlight goStructValueField that doesn't
" involve changing 'syntax iskeyword'
syntax keyword goSwitchKeywords default[:]


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


" Labels {{{

" TODO: Figure out a better alternative to the long containedin (some kind of
" expression group?)
syntax match goLabel /^\w\+\ze:/ contained containedin=goFuncBlock,goSwitchTypeBlock

hi link goLabel Label

call s:HiConfig('goLabel', ['go_highlight_labels'])

" }}} Labels


" Misc {{{

" TODO: Make this a catch-all for various keywords
syntax keyword goKeywords defer go

" goTypeAssertion is a part of the nextgroup list of goDotExpr
syntax region goTypeAssertion matchgroup=goParens start=/(/ end=/)/ contained contains=@goType,goTypeDecl
syntax match  goTypeAssertion /(type)/ contained contains=goParenBlock,goTypeDecl skipwhite nextgroup=goSwitchTypeBlock

hi link goKeywords Keyword

" }}} Misc

if !exists('main_syntax')
    let b:current_syntax = 'go'
endif

" vim:tw=80:fdm=marker:fmr={{{,}}}:
