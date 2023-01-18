" Author:    Charles Pascoe
" License:   MIT (see LICENSE)
" Copyright: 2022 Charles Pascoe
" Syntax highlighting for vim-go godoc buffers

if exists("b:current_syntax") && !get(g:, "go_highlight_override_existing_syntax", 1)
    finish
endif

syntax clear

let b:current_syntax = 'godoc'

let main_syntax = 'godoc'
syntax include @go syntax/go.vim
unlet main_syntax

syntax match godocTypeLine /^\%1l.*$/ contains=@godocType,@go

syntax cluster godocType contains=godocUntyped,goVarDecl,goConstDecl,goTypeDecl,goPackage,godocFuncDecl,godocField,godocVarConst

syntax match  godocFuncDecl                  /^func\ze\s*\%(\%(\K\k*\.\)\?\K\k*\)\?(/ skipwhite nextgroup=godocFuncDeclPackage,goFuncName,godocReceiverBlock
syntax match  godocFuncDeclPackage     contained /\K\k*\ze\.\K\k*/ contains=goPackageName nextgroup=godocMethodDot
syntax region godocReceiverBlock       contained matchgroup=godocReceiverParens start='(' end=')' contains=@goType nextgroup=godocMethodDot
syntax match  godocMethodDot           contained /\./ nextgroup=goFuncName

syntax keyword godocField              contained field   skipwhite nextgroup=godocFieldIdentifier
syntax match   godocFieldIdentifier    contained /\K\k*/ skipwhite nextgroup=@goType

syntax keyword godocVarConst           contained var const             skipwhite nextgroup=godocVarConstIdentifier
syntax keyword godocUntyped            contained untyped               skipwhite nextgroup=@goType
syntax match   godocVarConstIdentifier contained /\K\k*\%(\.\K\k*\)\?/ contains=goPackageName skipwhite nextgroup=@goType


hi link godocUntyped            Type
hi link godocVarConst           goVarDecl
hi link godocVarConstIdentifier goVarIdentifier
hi link godocFuncDecl           goFuncDecl
hi link godocReceiverParens     goReceiverParens
hi link godocField              Keyword
hi link godocFieldIdentifier    Identifier
hi link godocMethodDot          goDot
