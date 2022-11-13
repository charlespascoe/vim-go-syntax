" Add syntax highlighting to vim-go documentation

syntax clear

syntax include @go syntax/go.vim

syntax match godocTypeLine /^\%1l.*$/ contains=@godocType

syntax cluster godocType contains=goVarDecl,goConstDecl,goTypeDecl,goPackage,godocFuncDecl,godocField

syntax match  godocFuncDecl /^func\ze\s*\%(\%(\K\k*\.\)\?\K\k*\)\?(/ skipwhite nextgroup=godocFuncDeclPackage,goFuncName,godocReceiverBlock
syntax match  godocFuncDeclPackage /\K\k*\ze\.\K\k*/ contained contains=goPackageName nextgroup=godocMethodDot
syntax region godocReceiverBlock matchgroup=godocReceiverParens start='(' end=')' contained contains=@goType nextgroup=godocMethodDot
syntax match  godocMethodDot /\./ contained nextgroup=goFuncName

syntax keyword godocField field contained skipwhite nextgroup=godocFieldIdentifier
syntax match   godocFieldIdentifier /\K\k*/ contained skipwhite nextgroup=@goType

hi link godocFuncDecl        goFunc
hi link godocReceiverParens  goReceiverParens
hi link godocField           Keyword
hi link godocFieldIdentifier Identifier
hi link godocMethodDot       goDot

let b:current_syntax = 'godoc'
