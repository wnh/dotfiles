
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" My plugins
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'


Plugin 'chriskempson/base16-vim'
Plugin 'altercation/vim-colors-solarized'

call vundle#end()            " required
filetype plugin indent on    " required

let g:airline_left_sep  = ' '
let g:airline_right_sep = ' '
"let g:airline_theme = 'dark'
let g:airline_theme = 'light'

set background=dark laststatus=2

command W w

