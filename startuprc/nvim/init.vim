" ------ Plugins (vimplug) ----
call plug#begin('~/.config/nvim/plugged')
" Tools
    Plug 'junegunn/goyo.vim'
    Plug 'vifm/vifm.vim'
    Plug 'junegunn/limelight.vim'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
	Plug 'scrooloose/nerdtree' , { 'on': 'NERDTreeToggle' }
	Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Syntax
    Plug 'tpope/vim-markdown'
    Plug 'ap/vim-css-color' "Displays a preview of colors with CSS 
    Plug 'vim-scripts/fountain.vim'
	Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
	Plug 'ryanoasis/vim-devicons' " Needed for nerdtree-syntax-highlight
	Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
" Color-schemes
    Plug 'morhetz/gruvbox' "My favorite theme
    Plug 'kristijanhusak/vim-hybrid-material'
    Plug 'NLKNguyen/papercolor-theme'
    Plug 'ajh17/Spacegray.vim'
    Plug 'chriskempson/base16-vim'
	Plug 'vim-airline/vim-airline'
call plug#end()


" ----- General settings -----
syntax on
filetype plugin indent on
colorscheme gruvbox

" Tabs = 4 columns
set ts=4
set tabstop=4
set shiftwidth=4
set softtabstop=4
set noexpandtab
autocmd FileType python setlocal tabstop=4 noexpandtab
autocmd FileType scala setlocal tabstop=4 noexpandtab

" coc config
let g:coc_global_extensions = [
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-tsserver',
  \ 'coc-eslint', 
  \ 'coc-prettier', 
  \ 'coc-json', 
  \ 'coc-clangd',
  \ 'coc-fzf-preview',
  \ 'coc-java',
  \ 'coc-markdownlint',
  \ 'coc-python',
  \ 'coc-actions',
  \ ]

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
set shell=zsh

" Set backup directory
set backup
set backupdir=~/.config/nvim/backup
set directory=~/.config/nvim/tmp

set bg=dark

" highlight current line
set cul
" adjust color
hi CursorLine ctermbg=Blue term=None cterm=None

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


" folding
" enable folding
" set foldmethod=indent

" enable scrollwheel inside vim in gnu screen
set mouse=n             " hold shift to copy xterm



