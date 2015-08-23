" vim: fdm=marker noet
set nocompatible

" Vundle {{{1
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'vim-scripts/darkspectrum'
Bundle 'nanotech/jellybeans.vim'
Bundle 'Pychimp/vim-luna'
Bundle 'jnurmine/Zenburn'
Bundle 'altercation/vim-colors-solarized'
Bundle 'chriskempson/base16-vim'
Bundle 'vim-scripts/winter.vim'
Bundle 'vim-scripts/biogoo.vim'
Bundle 'fuqinho/vim-colors-graycier'
Bundle 'nice/sweater'
Bundle 'skammer/vim-css-color'
Bundle 'tpope/vim-fugitive'
Bundle 'ludovicchabant/vim-lawrencium'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-sleuth'
Bundle 'mileszs/ack.vim'
Bundle 'rking/ag.vim'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'godlygeek/tabular'
"Bundle 'Lokaltog/powerline'
"Bundle 'bling/vim-airline'
Bundle 'kien/ctrlp.vim'
"Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-commentary'
Bundle 'scrooloose/syntastic'
Bundle 'gregsexton/gitv'
Bundle 'vim-scripts/taglist.vim'
Bundle 'sfiera/vim-emacsmodeline'
Bundle 'bkad/CamelCaseMotion'
Bundle 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'rust-lang/rust.vim'
Plugin 'honza/dockerfile.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-speeddating'
Plugin 'tpope/vim-repeat'
Plugin 'justinmk/vim-sneak'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'jaxbot/semantic-highlight.vim'
Plugin 'cespare/vim-toml'
Plugin 'derekwyatt/vim-scala'
Plugin 'dag/vim-fish'

"call pathogen#infect() 

" Plugin settings {{{1
let g:airline_theme='solarized'
let g:airline_powerline_fonts=1
let g:airline_linecolumn_prefix=''
let g:airline_fugitive_prefix=''

let g:ctrlp_working_path_mode = 'a'
"set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
nmap <leader>st :SyntasticToggleMode<cr>
nmap <leader>sc :SyntasticCheck<cr>

nmap <leader>ss :SemanticHighlightToggle<cr>

nmap <leader>t :TlistToggle<cr>
let Tlist_Ctags_Cmd = '/usr/local/bin/ctags'

nmap <leader>gg :GitGutterSignsToggle<cr>
nmap <leader>gh :GitGutterLineHighlightsToggle<cr>

let g:LatexBox_Folding=1
let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_latexmk_async=1

" Behavior {{{1
set mouse=a
set vb
let &sh='bash'

set clipboard=autoselect
if has('gui_macvim')
	set clipboard=unnamed
elseif has('unnamedplus')
	set clipboard+=unnamedplus
end

" http://stackoverflow.com/a/2969052/230170
au FocusLost,TabLeave * call feedkeys("\<C-\>\<C-n>")

" Appearance {{{1
set number
if version >= 703
	set relativenumber
end
set scrolloff=2
set laststatus=2

" Colors and GUI {{{2
set background=dark
colorscheme desert

if has('gui_running')
	if has('mac')
		set guifont=Inconsolata:h14
	else
		set guifont=Monospace\ 11
	endif
	colorscheme darkspectrum
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

set spellfile=~/.vim/spell/en.utf-8.add

" Indentation {{{1
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent
if version >= 703
	set colorcolumn=80
end

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
nmap <leader>m :make 
nmap <leader>mm :make<cr>

