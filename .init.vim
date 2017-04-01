
" - -- General Sanity --- "
let mapleader=","
syntax on
syntax enable

set foldenable 		" Enable folding
set encoding=utf-8	" For those sweet sweet iamges
set nu    		" Turn on line nums
set expandtab  	 	" Expand tabs to spaces
set hidden 		" buffer remembers undo history and marks
set noerrorbells 	" Disable error bells
set noshowmode 		" hide current mode (liteline handles it)
set shiftwidth=2 	" The # of spaces for indenting
set softtabstop=2	" Tab key results in 2 spaces
set incsearch           " search as characters are entered
set hlsearch            " highlight matches
set foldlevelstart=10   " open most folds by default
set foldmethod=indent   " fold based on indent level


let g:deoplete#enable_at_startup = 1


" --- Key shorts --- "
nnoremap <leader>m :NERDTreeToggle<CR>
nnoremap <space> za
nnoremap j gj
nnoremap k gk 
nnoremap <leader>u :UndotreeToggle<CR>


" -- Plugins -- "
call plug#begin('~/.local/share/nvim/plugged')

Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'junegunn/goyo.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'ryanoasis/vim-devicons'
Plug 'Valloric/YouCompleteMe'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'itchyny/lightline.vim'
Plug 'ayu-theme/ayu-vim'
Plug 'mbbill/undotree'
call plug#end()

" -- End Plugins -- "

" NERDTree
let g:NERDTreeLimitedSyntax = 1
let g:NERDTreeDirArrowExpandable = ''
let g:NERDTreeDirArrowCollapsible = ''
let g:WebDevIconsNerdTreeAfterGlyphPadding = ' '
let g:WebDevIconsUnicodeGlyphDoubleWidth = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1

"undo anything forever"

if has("persistent_undo")
    set undodir=~/.undodir/
    set undofile
endif

" Colors " 
set termguicolors     " enable true colors support
let ayucolor="light"  " for light version of theme
let ayucolor="mirage" " for mirage version of theme
let ayucolor="dark"   " for dark version of theme
colorscheme ayu

