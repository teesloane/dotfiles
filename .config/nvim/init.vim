let mapleader="," 
syntax on
syntax enable

" ------ My Junk -------"
set background=dark       " make dark
set foldenable            " Enable folding
set foldlevelstart=10     " open most folds by default
set foldmethod=indent     " fold based on indent level
set encoding=utf-8 nobomb " For those sweet sweet iamges
set nu                    " Turn on line nums
set expandtab             " Expand tabs to spaces
set hidden                " buffer remembers undo history and marks
set noerrorbells          " Disable error bells
set noshowmode            " hide current mode (liteline handles it)
set shiftwidth=2          " The # of spaces for indenting
set softtabstop=2         " Tab key results in 2 spaces
set incsearch             " search as characters are entered
set hlsearch              " highlight matches
filetype plugin indent on " Detect files for indenting?

" --- directory things --- "
set wildignore+=*/bower_components/*,*/node_modules/*
set backupdir=~/.config/nvim/backups
set directory=~/.config/nvim/swaps
set undodir=~/.config/nvim/undo


" --- LEADER SHORTCUTS --- "
nnoremap <leader>m :NERDTreeToggle<CR>
nnoremap <space> za
nnoremap <leader>u :UndotreeToggle<CR>


" -- Plugins -- "
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

" Use fzf plug instead of ctrl p? I guess.
if isdirectory('/usr/local/opt/fzf')
  Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
  Plug 'junegunn/fzf.vim'
endif

call plug#end()
" -- End Plugins -- "


" -- Start Plugins Config-- "
let g:deoplete#enable_at_startup = 1

nnoremap <Leader>fa :Ag<CR>
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>fb :Buffers<CR>


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

" Colors "
set termguicolors     " enable true colors support
colorscheme base16-oceanicnext

" Highlights must be done at the end of file
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
