set nocompatible

call pathogen#infect() 

set mouse=a
set number
set autoindent
filetype plugin indent on
syntax on
syntax keyword cTodo contained DEBUG

set wildignore+=*.hi

set vb

set tabstop=4
set shiftwidth=4
set scrolloff=2

let &sh='bash'

set background=dark
colorscheme wombat

if has('gui_running')
	set guifont=Inconsolata\ Medium\ 11
	set guioptions-=T
endif
