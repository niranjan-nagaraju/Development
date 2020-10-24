execute pathogen#infect()
syntax on
filetype plugin indent on
colorscheme gruvbox

" Tabs = 4 columns
set tabstop=4
set shiftwidth=4
set softtabstop=4

" Directly jump to matching prefixes as soon as letters are entered
set incsearch

" Case-insensitive search
set ignorecase

" Highlight search string
set hlsearch
set ruler
set ls=2
set nocompatible
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

set bg=dark

" highlight current line
set cul
" adjust color
hi CursorLine term=none cterm=none ctermbg=7


" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" for vimdiff, Open all folds
"  Set a large number for context
"    context being the number of unfolded lines
"
" Alternately zR inside vimdiff works as well.
" Courtesy: Superuser(http://superuser.com/questions/198779/make-vimdiff-show-entire-file)
if &diff " only for diff mode/vimdiff
      set diffopt=filler,context:1000000 " filler is default and inserts empty lines for sync
endif 


" enable scrollwheel inside vim in gnu screen
set mouse=n             " hold shift to copy xterm
set ttymouse=xterm2     " necessary for gnu screen & mouse

