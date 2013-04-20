" vim: fdm=marker
set nocompatible

call pathogen#infect() 

" Behavior {{{1
set mouse=a
set vb
let &sh='bash'

set clipboard=autoselect
if has('unnamedplus')
	set clipboard+=unnamedplus
end

" http://stackoverflow.com/a/2969052/230170
au FocusLost,TabLeave * call feedkeys("\<C-\>\<C-n>")

" Appearance {{{1
set number
set scrolloff=2

" Colors and GUI {{{2
set background=dark
colorscheme darkspectrum

if has('gui_running')
	set guifont=Inconsolata\ Medium\ 11
	set guioptions-=T
endif

" Hidden characters {{{2
set listchars=tab:▸\ ,eol:¬
nmap <leader>l :set list!<cr>

" Syntax {{{1
filetype plugin indent on
syntax on
syntax keyword cTodo contained DEBUG
au BufEnter *.md setf markdown

" Indentation {{{1
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent

" Tab completion {{{1
set wildmode=longest,list,full
set wildmenu
set wildignore+=*.hi

" Per-directory configuration {{{1
set exrc
set secure

" Search {{{1
set ignorecase
set smartcase
set gdefault
set incsearch
set hlsearch
nnoremap <leader><space> :noh<cr>

" Mappings {{{1
imap jj <Esc>
noremap <Up> <C-w>W
noremap <Down> <C-w>w
noremap <Left> <NOP>
noremap <Right> <NOP>

" Plugins {{{1
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
let g:ctrlp_working_path_mode = 'a'
