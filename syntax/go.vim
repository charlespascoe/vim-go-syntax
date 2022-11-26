if !exists('main_syntax')
    if exists("b:current_syntax") && !get(g:, 'go_highlight_override_existing_syntax', 1)
        finish
    endif

    syntax sync fromstart
endif

let b:__vim_go_syntax = 1

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

fun s:getconfig(prefix, keys, default)
    if len(a:keys) == 0
        return a:default
    else
        return get(g:, a:prefix.a:keys[0], s:getconfig(a:prefix, a:keys[1:], a:default))
    endif
endfun

fun s:HiConfig(group, option_names, opts={})
    " All syntax is highlighted by default, unless turned off by the user
    let l:opt = s:getconfig('go_highlight_', a:option_names, get(a:opts, 'default', 1))
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

call s:HiConfig('goField',     ['fields'], #{default: 0})
call s:HiConfig('goOperator',  ['operators'])
call s:HiConfig('goDot',       ['dot','separators'])
call s:HiConfig('goComma',     ['comma','separators'])
call s:HiConfig('goSemicolon', ['semicolon','separators'])

hi def link goOperator   Operator
hi def link goDot        goOperator
hi def link goDotExpr    goDot
hi def link goComma      goOperator
hi def link goSemicolon  goOperator
hi def link goUnderscore Special
hi def link goField      Identifier

" }}} Misc


" Comments {{{

syntax keyword goCommentTodo     contained TODO FIXME XXX TBD NOTE
syntax region  goComment         start=+//+  end=+$+   contains=@goCommentSpell,goCommentTodo keepend
syntax region  goComment         start=+/\*+ end=+\*/+ contains=@goCommentSpell,goCommentTodo keepend
syntax match   goGenerateComment +//go:generate.*$+

call s:HiConfig('goGenerateComment', ['generate_tags'], #{offgroup: 'goComment'})

hi def link goCommentTodo     Todo
hi def link goComment         Comment
hi def link goGenerateComment PreProc

" }}} Comments


" Literals {{{

syntax region goString       start='"' skip=/\\\\\|\\"/ end='"\|$' oneline contains=@goStringSpell,goStringEscape,goDoubleQuoteEscape,goStringFormat
syntax match  goStringEscape /\v\\%(\o{3}|x\x{2}|u\x{4}|U\x{8}|[abfnrtv\\"])/ contained

" TODO: float formatting, flags (https://pkg.go.dev/fmt)
syntax match  goStringFormat /\v\%%([%EFGOTUXbcdefgopqstvxf])/ contained

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

call s:HiConfig('goStringFormat',       ['format_strings'], #{offgroup: 'goString'})
call s:HiConfig('goInvalidRuneLiteral', ['rune_literal_error'])

hi def link goString             String
hi def link goStringEscape       SpecialChar
hi def link goStringFormat       SpecialChar

hi def link goInvalidRuneLiteral Error
hi def link goRuneLiteral        Character
hi def link goRuneLiteralEscape  goStringFormat

hi def link goRawString          String

hi def link goNumber             Number

hi def link goBooleanTrue        Boolean
hi def link goBooleanFalse       Boolean

hi def link goNil                Constant

" }}} Literals


" Simple Blocks {{{

syntax region goBracketBlock matchgroup=goBrackets start='\[' end='\]' transparent extend
syntax region goParenBlock   matchgroup=goParens   start='('  end=')'  transparent extend
syntax region goBraceBlock   matchgroup=goBraces   start='{'  end='}'  transparent extend

call s:HiConfig('goBraces',   ['braces'])
call s:HiConfig('goBrackets', ['brackets'])
call s:HiConfig('goParens',   ['parens'])

hi def link goBraces   Delimiter
hi def link goBrackets Delimiter
hi def link goParens   Delimiter

" }}} Simple Blocks


" Constants and Variables {{{

" goVarAssign and goShortVarDecl

let s:assignOrShortDecl = 0

if s:HiConfig('goVarAssign', ['variable_assignments'], #{default: 0})
    " TODO: Only valid operators?
    syntax match goVarAssign /\<\w\+\%(\s*,\s*\%(\w\+\)\?\)*\ze\s*[-+*/!%&^<>|~]*=/ contains=goComma,goUnderscore contained
    let s:assignOrShortDecl = 1
endif

if s:HiConfig('goShortVarDecl', ['short_variable_declarations','variable_declarations'])
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

call s:HiConfig('goVarIdentifier', ['variable_declarations'])

hi def link goConstDecl          Statement
hi def link goVarDecl            Statement

hi def link goConstDeclParens    goParens
hi def link goVarDeclParens      goParens

hi def link goVarIdentifier      Identifier
hi def link goVarGroupIdentifier goVarIdentifier
hi def link goShortVarDecl       Identifier
hi def link goShortVarDecl goShortVarDecl

hi def link goVarAssign          Special

hi def link goIota               Special

" }}} Constants and Variables


" Packages {{{

syntax keyword goPackage      package
syntax keyword goImport       import skipwhite nextgroup=goImportItem,goImports
syntax region  goImports      matchgroup=goImportParens start='(' end=')' contained contains=goImportItem,goComment
syntax match   goImportItem   /\(\([\._]\|\w\+\)\s\+\)\?"[^"]*"/ contained contains=goImportString
syntax region  goImportString start='"' end='"' keepend contained

hi def link goPackage      Keyword
hi def link goImport       Keyword
hi def link goImportItem   Special
hi def link goImportString goString
hi def link goImportParens goParens

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


call s:HiConfig('goTypeDeclName', ['types'])
call s:HiConfig('goSliceOrArray', ['slice_brackets'])
call s:HiConfig('goMapBrackets',  ['map_brackets'])

hi def link goPointer               goOperator

" goTypeDecl should technically link to Typedef, but it looks a bit odd.
hi def link goTypeDecl              Keyword
hi def link goTypeParens            goParens
hi def link goTypeDeclGroupParens   goParens
hi def link goTypeDeclName          Typedef
hi def link goTypeParamBrackets     goBrackets
hi def link goTypeAssign            goOperator

hi def link goPackageName           Special

hi def link goNonPrimitiveType      Type
hi def link goSimpleBuiltinTypes    Type
hi def link goMap                   goSimpleBuiltinTypes
hi def link goMapBrackets           Delimiter
hi def link goSliceOrArray          Delimiter
hi def link goSliceOrArrayType      goSliceOrArray
hi def link goSliceBraces           goBraces
hi def link goChannel               Type

hi def link goFuncType              goFuncDecl
" See 'Functions' for other function highlight groups

" }}} Types


" Functions {{{

" Unfortunately limited to at most 3 nested type args
" TODO: Figure out a better alternative to the long containedin (some kind of
" expression group?)
syntax match  goFuncCall /\v<\w+\ze%(\[\s*\n?%(,\n|[^\[\]]|\[\s*\n?%(,\n|[^\[\]]|\[[^\[\]]*\])*\])*\])?\(/ contained containedin=goFuncBlock,goSwitchTypeBlock,goStructBlock,goVarDeclGroup,goConstDeclGroup,goFuncCallArgs,goMakeBlock nextgroup=goFuncCallTypeArgs,goFuncCallArgs
syntax region goFuncCallTypeArgs matchgroup=goTypeParamBrackets start='\[' end='\]' contained contains=@goType,goUnderscore,goComma nextgroup=goFuncCallArgs
syntax region goFuncCallArgs     matchgroup=goFuncCallParens    start='('  end=')'  contained contains=TOP,@Spell

syntax keyword goFuncDecl func skipempty skipwhite nextgroup=goMethodReceiver,goFuncName,goFuncParams

syntax match goVariadic /\.\.\./ contained skipwhite nextgroup=@goType

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

" goNamedReturnValue and goFuncTypeParam are the same but defined separately for highlighting purposes
syntax match goNamedReturnValue /\%(^\|[(,]\)\@1<=\s*\zs\%(\w\+\%(\s*,\%(\s\|\n\)*\w\+\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType
syntax match goFuncTypeParam    /\%(^\|[(,]\)\@1<=\s*\zs\%(\w\+\%(\s*,\%(\s\|\n\)*\w\+\)*\s\+\)\?\ze[^,]/ contained contains=goComma skipwhite nextgroup=@goType

syntax keyword goReturn return

call s:HiConfig('goFuncCall',   ['function_calls'])
call s:HiConfig('goFuncName',   ['functions'])
call s:HiConfig('goFuncParens', ['function_parens'])
call s:HiConfig('goFuncBraces', ['function_braces'])
call s:HiConfig('goParam',      ['function_parameters'])
call s:HiConfig('goTypeParam',  ['type_parameters'])

hi def link goFuncName              Function
hi def link goFuncCall              Function
hi def link goFuncCallParens        goParens
hi def link goFuncDecl              Keyword
hi def link goFuncParens            goParens
hi def link goFuncBraces            goBraces
hi def link goFuncMultiReturnParens goParens

hi def link goReceiverParens        goFuncParens

hi def link goVariadic              goOperator

hi def link goParam                 Identifier
hi def link goTypeParam             Identifier


" TODO: What to do with these?
hi def link goNamedReturnValue      NONE
hi def link goFuncTypeParam         NONE

hi def link goReturn                Statement

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

call s:HiConfig('goStructTypeTag',    ['struct_tags'])
call s:HiConfig('goStructValueField', ['struct_fields'], #{default: 1})

hi def link goStructType       Keyword
hi def link goStructTypeBraces goBraces
hi def link goStructTypeField  NONE
hi def link goStructTypeTag    PreProc
hi def link goStructValue      goNonPrimitiveType
hi def link goStructValueField Identifier
hi def link goStructBraces     goBraces

hi def link goInterfaceType         goStructType
hi def link goInterfaceBraces       goBraces
hi def link goInterfaceMethod       goFuncName
hi def link goInterfaceMethodParens goFuncParens

" }}} Structs and Interfaces


" Builtins {{{

syntax keyword goBuiltins append cap close complex copy delete imag len panic print println real recover skipwhite nextgroup=goFuncCallArgs

syntax keyword goMakeBuiltin    make nextgroup=goMakeBlock
syntax region  goMakeBlock      matchgroup=goFuncCallParens start='(' end=')' contained contains=TOP,@Spell
syntax match   goMakeFirstParen /\%(make(\_[[:space:]]*\)\@20<=/ contained skipempty skipwhite nextgroup=@goType containedin=goMakeBlock

syntax keyword goNewBuiltin new skipwhite nextgroup=goNewBlock
syntax region  goNewBlock matchgroup=goFuncCallParens start='(' end=')' contained contains=@goType

call s:HiConfig('goBuiltins', ['builtins'], #{offgroup: 'goFuncCall'})

hi def link goBuiltins    Special
hi def link goMakeBuiltin goBuiltins
hi def link goNewBuiltin  goBuiltins

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

hi def link goIf   Conditional
hi def link goElse goIf

hi def link goFor         Repeat
hi def link goForKeywords goFor

hi def link goSwitch         Conditional
hi def link goSelect         goSwitch
hi def link goSwitchKeywords goSwitch

hi def link goSwitchTypeBraces goBraces
hi def link goSwitchTypeCase   goSwitchKeywords

" }}} Flow Control


" Labels {{{

" TODO: Figure out a better alternative to the long containedin (some kind of
" expression group?)
syntax match goLabel /^\w\+\ze:/ contained containedin=goFuncBlock,goSwitchTypeBlock

call s:HiConfig('goLabel', ['labels'])

hi def link goLabel Label

" }}} Labels


" Misc {{{

" TODO: Make this a catch-all for various keywords
syntax keyword goKeywords defer go

" goTypeAssertion is a part of the nextgroup list of goDotExpr
syntax region goTypeAssertion matchgroup=goParens start=/(/ end=/)/ contained contains=@goType,goTypeDecl
syntax match  goTypeAssertion /(type)/ contained contains=goParenBlock,goTypeDecl skipwhite nextgroup=goSwitchTypeBlock

hi def link goKeywords Keyword

" }}} Misc

if !exists('main_syntax')
    let b:current_syntax = 'go'
endif

" vim:tw=80:fdm=marker:fmr={{{,}}}:
