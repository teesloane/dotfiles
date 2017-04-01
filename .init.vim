
" - -- General Sanity --- "
let mapleader=","
syntax on
syntax enable

set foldenable          " Enable folding
set encoding=utf-8      " For those sweet sweet iamges
set nu                  " Turn on line nums
set expandtab           " Expand tabs to spaces
set hidden              " buffer remembers undo history and marks
set noerrorbells        " Disable error bells
set noshowmode          " hide current mode (liteline handles it)
set shiftwidth=2        " The # of spaces for indenting
set softtabstop=2       " Tab key results in 2 spaces
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
 NORMAL  init.vim                                      unix | utf-8 | vim   26%   19:45
