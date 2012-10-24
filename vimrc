set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
let g:vundle_default_git_proto = 'git'
call vundle#rc()

Bundle 'gmarik/vundle'

" Language plugins
Bundle "empanda/vim-varnish"
Bundle "Glench/Vim-Jinja2-Syntax"
Bundle 'jnwhiteh/vim-golang'
Bundle "rodjek/vim-puppet"
Bundle "tpope/vim-markdown"

" Other plugins
Bundle "godlygeek/tabular"
Bundle "kien/ctrlp.vim"
Bundle "Lokaltog/vim-powerline"
Bundle "mileszs/ack.vim"
Bundle "scrooloose/syntastic"
Bundle "tpope/vim-fugitive"

" Personal bundles
Bundle 'mitchellh/vim-misc'

" Turn on filetype plugin and indent loading so that loading the
" vim-misc stuff below loads the proper files.
filetype plugin indent on

" Load in my custom vimrc if it exists
if filereadable(expand('~/.vim/bundle/vim-misc/vimrc.vim'))
    source ~/.vim/bundle/vim-misc/vimrc.vim
endif
