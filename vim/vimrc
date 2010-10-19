syntax on
filetype plugin indent on
colorscheme desert
set tabstop=4
set incsearch
set shiftwidth=4
set softtabstop=4
set ruler
set ls=2
set nocompatible
set hlsearch
set wrap
set autoindent
set textwidth=0
set showcmd
set showmatch
set ruler
set shell=bash

" Set backup directory
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp

" highlight current line
set cul
" adjust color
hi CursorLine term=none cterm=none ctermbg=7

set bg=dark

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif
