
set nocompatible              " be iMproved, required
filetype off                  " required
set t_Co=256
set timeoutlen=50

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" My plugins
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
"Plugin 'scrooloose/nerdcommenter'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'
Plugin 'godlygeek/tabular'
Plugin 'Raimondi/delimitMate'
Plugin 'vim-scripts/dbext.vim'

Plugin 'joonty/vdebug'
Plugin 'majutsushi/tagbar'


Plugin 'chriskempson/base16-vim'
Plugin 'altercation/vim-colors-solarized'

call vundle#end()            " required
filetype plugin indent on    " required
filetype plugin on

let g:airline_left_sep  = ' '
let g:airline_right_sep = ' '

colorscheme base16-default
let g:airline_theme='base16'

set background=dark laststatus=2

command W w

set number
set nowrap
set listchars=tab:»\ ,trail:·

au BufNewFile,BufRead *.install set filetype=php
au BufNewFile,BufRead *.module  set filetype=php
au BufNewFile,BufRead *.inc     set filetype=php
au BufNewFile,BufRead *.test    set filetype=php


au FileType css setlocal list
au FileType php setlocal makeprg=php\ -l\ %
au FileType php setlocal errorformat=%m\ in\ %f\ on\ line\ %l,%-GErrors\ parsing\ %f,%-G

set tabstop=2 shiftwidth=2 expandtab

set grepprg=ack


nnoremap <C-S-n> :cnext<CR>

function! GrepForLastSearch()
  execute "grep " . shellescape(@/) . " **"
endfunction
command GrepForLastSearch call GrepForLastSearch()

set complete-=t
nnoremap ,m :make<CR>

function! ClearTrailingWhitespace()
  execute "%s/\\s\\+$//"
endfunction!

command Trail call ClearTrailingWhitespace()



set previewheight=25
command Gci Gcommit


