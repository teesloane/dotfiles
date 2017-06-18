" Welcome to my vim config which I stole form the internet and don't really
" use anyway

"*****************************************************************************
""  Setup
"*****************************************************************************"

" GENERAL SANITY STUFF {{{
let mapleader="," 
syntax on                       " turn on syntax highlighting
syntax enable                   " something?
set background=dark             " make dark
set clipboard+=unnamedplus      " enable copying to clipboard
set foldenable                  " Enable folding
set foldlevelstart=10           " open most folds by default
set foldmethod=indent           " fold based on indent level
set modelines=1                 " tell vim to look for file specific settings (used for this config to make it fold on load)
set encoding=utf-8 nobomb       " For those sweet sweet iamges
set nu                          " Turn on line nums
set expandtab                   " Expand tabs to spaces
set hidden                      " buffer remembers undo history and marks
set noerrorbells                " Disable error bells
set noshowmode                  " hide current mode (liteline handles it)
set shiftwidth=2                " The # of spaces for indenting
set softtabstop=2               " Tab key results in 2 spaces
set incsearch                   " search as characters are entered
set hlsearch                    " highlight matches
set autoindent
set nobackup                    " LIVE LIFE ON THE EDGE
set noswapfile                  " WHO KNWOS WHO CARES?
hi Normal guibg=NONE ctermbg=NONE
let g:jsx_ext_required = 0      " FOR THE JSX
filetype plugin indent on       " Detect files for indenting?
" }}}

" DIRECTORIES {{{
set wildignore+=*/bower_components/*,*/node_modules/*
set directory=~/.config/nvim/swaps
set undodir=~/.config/nvim/undo
"}}}


"*****************************************************************************
"" Mappings
"*****************************************************************************

" LEADER SHORTCUTS {{{
nnoremap <leader>m :NERDTreeToggle<CR>  
nnoremap <space> za                     " for the folding I think
nnoremap <leader>u :UndotreeToggle<CR>  

"" Split
noremap <Leader>h :<C-u>split<CR>
noremap <Leader>v :<C-u>vsplit<CR>

"" Git
noremap <Leader>ga :Gwrite<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <Leader>gsh :Gpush<CR>
noremap <Leader>gll :Gpull<CR>
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gb :Gblame<CR>
noremap <Leader>gd :Gvdiff<CR>
noremap <Leader>gr :Gremove<CR>

" session management
nnoremap <leader>so :OpenSession<Space>
nnoremap <leader>ss :SaveSession<Space>
nnoremap <leader>sd :DeleteSession<CR>
nnoremap <leader>sc :CloseSession<CR>

"" Buffer nav
noremap <leader>z :bp<CR>
noremap <leader>q :bp<CR>
noremap <leader>x :bn<CR>
noremap <leader>w :bn<CR>

"" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

"" Close buffer
noremap <leader>c :bd<CR>

"" Vmap for maintain Visual Mode after shifting > and <
vmap < <gv
vmap > >gv

"" Tabs
nnoremap <Tab> gt
nnoremap <S-Tab> gT
nnoremap <silent> <S-t> :tabnew<CR>

"" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

"" Opens an edit command with the path of the currently edited file filled in
noremap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

"" Opens a tab edit command with the path of the currently edited file filled
noremap <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

"fzf commands
nnoremap <Leader>ag :Ag<CR>
nnoremap <Leader>ff :GitFiles<CR>
nnoremap <Leader>af :Files<CR>
nnoremap <Leader>fb :Buffers<CR>
"NERDTree 
nnoremap <Leader>fm :NERDTreeFind<CR>


" Remap some stuff so other stuff isn't so bad right?
command! W w                    " Remap W to w 
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall
"}}}

"*****************************************************************************
"" Plugins
"*****************************************************************************
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
Plug 'pangloss/vim-javascript'
Plug 'chriskempson/base16-vim' 
Plug 'airblade/vim-gitgutter'
Plug 'fatih/vim-go'
Plug 'Yggdroot/indentLine'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'elmcast/elm-vim'
Plug 'mxw/vim-jsx'
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
let g:deoplete#file#enable_buffer_path = 1  "autocomplete path relative to file not pwd

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

" width of goyo mode
let g:goyo_width = 110

" NERDTree
let g:NERDTreeDirArrows=0
let g:WebDevIconsNerdTreeAfterGlyphPadding = ''
let g:WebDevIconsUnicodeGlphDoubleWidth = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1

let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 50
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
"}}}

"COLORS AND HILIGHTING THINGS {{{
set termguicolors     " enable true colors support
colorscheme base16-eighties
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
highlight link xmlEndTag xmlTag   "make jsx tags not suck
"}}}

" MOST OF THIS STUFF IS STOLEN FROM VIM BOOSTRAP
" LANGUAGE STUFF {{{

" elm / elm-vim
let g:elm_setup_keybindings = 0
let g:elm_format_autosave = 1
let g:polyglot_disabled = ['elm'] "disable vim polyglot for elm

"}}}
