let mapleader="," 

" GENERAL SANITY STUFF {{{
set background=dark        " make dark
syntax on                  " turn on syntax highlighting
syntax enable              " something?
set clipboard+=unnamedplus " enable copying to clipboard
set foldenable             " Enable folding
set foldlevelstart=10      " open most folds by default
set foldmethod=indent      " fold based on indent level
set modelines=1            " tell vim to look for file specific settings (used for this config to make it fold on load)
set encoding=utf-8 nobomb  " For those sweet sweet iamges
set nu                     " Turn on line nums
set expandtab              " Expand tabs to spaces
set hidden                 " buffer remembers undo history and marks
set noerrorbells           " Disable error bells
set noshowmode             " hide current mode (liteline handles it)
set shiftwidth=2           " The # of spaces for indenting
set softtabstop=2          " Tab key results in 2 spaces
set incsearch              " search as characters are entered
set hlsearch               " highlight matches
filetype plugin indent on  " Detect files for indenting?
" }}}

" DIRECTORIES {{{
set wildignore+=*/bower_components/*,*/node_modules/*
set backupdir=~/.config/nvim/backups
set directory=~/.config/nvim/swaps
set undodir=~/.config/nvim/undo
"}}}

" LEADER SHORTCUTS {{{
nnoremap <leader>m :NERDTreeToggle<CR>  
nnoremap <space> za                     
nnoremap <leader>u :UndotreeToggle<CR>  
"fzf commands
nnoremap <Leader>ag :Ag<CR>
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>fb :Buffers<CR>
"}}}

" PLUGINGS {{{
call plug#begin('~/.local/share/nvim/plugged')
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'junegunn/goyo.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'ryanoasis/vim-devicons'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'itchyny/lightline.vim'
Plug 'mbbill/undotree'
Plug 'chriskempson/base16-vim' 
Plug 'airblade/vim-gitgutter'
Plug 'Yggdroot/indentLine'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
call plug#end()
"}}}

" PLUGIN CONFIG {{{

" Use fzf plug instead of ctrl p? I guess.

if isdirectory('/usr/local/opt/fzf')
  Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
endif

" start deoplete at startup
let g:deoplete#enable_at_startup = 1

" FZF SETUP  This is the default extra key bindings
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" Default fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~40%' }

" LiteLine
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'component': {
      \   'readonly': '%{&readonly?"⭤":""}',
      \ }
      \ }"

" NERDTree
let g:NERDTreeDirArrows=0
let g:WebDevIconsNerdTreeAfterGlyphPadding = ''
let g:WebDevIconsUnicodeGlphDoubleWidth = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1
"}}}

"COLORS AND HILIGHTING THINGS {{{
set termguicolors     " enable true colors support
colorscheme base16-oceanicnext
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
"}}}

" enable folding for this file on load
" vim:foldmethod=marker:foldlevel=0
