syntax on
set number
set nocompatible

set history=50

set virtualedit=block
set whichwrap=b,s,[,],<,>

"--- search ---"
set ignorecase
set smartcase
set wrapscan
set incsearch
set hlsearch


set list
set listchars=tab:^\ ,trail:~

highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=white
match ZenkakuSpace /　/

set autoindent
