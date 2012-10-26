set nocompatible

call pathogen#infect() 

set mouse=a
set number
set autoindent
filetype plugin indent on
syntax on
syntax keyword cTodo contained DEBUG
au BufEnter *.md setf markdown

set wildmode=longest,list,full
set wildmenu
set wildignore+=*.hi

set exrc
set secure

set vb

set tabstop=4
set shiftwidth=4
set scrolloff=2

set ignorecase
set smartcase
set gdefault
set incsearch
set hlsearch
nnoremap <leader><space> :noh<cr>

let &sh='bash'

set background=dark
colorscheme wombat

if has('gui_running')
	set guifont=Inconsolata\ Medium\ 11
	set guioptions-=T
endif
