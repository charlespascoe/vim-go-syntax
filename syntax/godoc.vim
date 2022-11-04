" Intended to add syntax highlighting to vim-go documentation

syntax include @go syntax/go.vim

syntax match godocTypeLine /^\%1l.*$/ contains=@go
