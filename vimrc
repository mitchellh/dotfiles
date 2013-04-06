" Installation Instructions
"	1. Place file in home directory as .vimrc
"	2. Run the following command in terminal
"		mkdir .vim .vim/bundle .vim/backup .vim/swap .vim/cache .vim/undo; git clone https://github.com/gmarik/vundle.git .vim/bundle/vundle
"	3. Launch Vim and Run
"		:BundleInstall
"	5. Restart Vim
set nocompatible
filetype off

if has("win32")
    " We're on Windows.
    let g:vim_home_path = "~/vimfiles"
else
    " We're on some POSIX system, hopefully.
    let g:vim_home_path = "~/.vim"
endif

execute "set rtp+=" . g:vim_home_path . "/bundle/vundle/"
let g:vundle_default_git_proto = 'https'
call vundle#rc(g:vim_home_path. "/bundle")

Bundle 'gmarik/vundle'

" Language plugins
Bundle "bryanjswift/vim-rust"
Bundle "empanda/vim-varnish"
Bundle "groenewege/vim-less"
Bundle "Glench/Vim-Jinja2-Syntax"
Bundle 'jnwhiteh/vim-golang'
Bundle "kchmck/vim-coffee-script"
Bundle "PProvost/vim-ps1"
Bundle "rodjek/vim-puppet"
Bundle "tpope/vim-markdown"

" Other plugins
Bundle "godlygeek/tabular"
Bundle "kien/ctrlp.vim"
Bundle "Lokaltog/vim-easymotion"
Bundle "Lokaltog/vim-powerline"
Bundle "mileszs/ack.vim"
Bundle "scrooloose/syntastic"
Bundle "tpope/vim-eunuch"
Bundle "tpope/vim-fugitive"

" Personal bundles
Bundle 'mitchellh/vim-misc'

" Turn on filetype plugin and indent loading so that loading the
" vim-misc stuff below loads the proper files.
filetype plugin indent on

" Load in my custom vimrc if it exists
if filereadable(expand(g:vim_home_path . "/bundle/vim-misc/vimrc.vim"))
    execute "source " . g:vim_home_path . "/bundle/vim-misc/vimrc.vim"
endif
