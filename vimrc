" vim: fdm=marker noet
set nocompatible

" Vundle {{{1
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" Editing {{{2
Plugin 'godlygeek/tabular'
Plugin 'jaxbot/semantic-highlight.vim'
Plugin 'justinmk/vim-sneak'
Plugin 'sfiera/vim-emacsmodeline'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-sleuth'
Plugin 'tpope/vim-speeddating'
Plugin 'tpope/vim-surround'

" Tools {{{2
Plugin 'airblade/vim-gitgutter'
Plugin 'gregsexton/gitv'
Plugin 'kien/ctrlp.vim'
Plugin 'ludovicchabant/vim-lawrencium'
Plugin 'mattn/gist-vim'
Plugin 'mattn/webapi-vim'
Plugin 'mileszs/ack.vim'
Plugin 'rking/ag.vim'
Plugin 'scrooloose/syntastic'
Plugin 'taglist.vim'
Plugin 'tpope/vim-fugitive'

" Colors {{{2
Plugin 'altercation/vim-colors-solarized'
Plugin 'biogoo.vim'
Plugin 'chriskempson/base16-vim'
Plugin 'darkspectrum'
Plugin 'fuqinho/vim-colors-graycier'
Plugin 'jnurmine/Zenburn'
Plugin 'nanotech/jellybeans.vim'
Plugin 'nice/sweater'
Plugin 'Pychimp/vim-luna'
Plugin 'winter.vim'

" File type support {{{2
Plugin 'cespare/vim-toml'
Plugin 'dag/vim-fish'
Plugin 'derekwyatt/vim-scala'
Plugin 'elixir-lang/vim-elixir'
Plugin 'fatih/vim-go'
Plugin 'honza/dockerfile.vim'
Plugin 'jceb/vim-orgmode'
Plugin 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'pearofducks/ansible-vim'
Plugin 'rust-lang/rust.vim'
Plugin 'skammer/vim-css-color'
Plugin 'tpope/vim-markdown'

" Plugin settings {{{1

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

