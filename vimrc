
set nocompatible              " be iMproved, required
filetype off                  " required
set timeoutlen=50
set t_Co=256
let base16colorspace=256

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
"Plugin 'Valloric/YouCompleteMe'

Plugin 'joonty/vdebug'
Plugin 'majutsushi/tagbar'
Plugin 'Shougo/unite.vim'
Plugin 'tpope/vim-dispatch'

Plugin 'ervandew/supertab'
Plugin 'SerVer/ultisnips'

"Plugin 'terryma/vim-multiple-cursors'
Plugin 'mattn/emmet-vim'

Plugin 'chriskempson/base16-vim'
Plugin 'altercation/vim-colors-solarized'

Plugin 'closetag.vim'

call vundle#end()            " required
filetype plugin indent on    " required
filetype plugin on

let g:airline_left_sep  = ' '
let g:airline_right_sep = ' '

colorscheme base16-default
let g:airline_theme='base16'

set background=dark laststatus=2

command W w
command Vsp vsp
command Sp sp

" set tab completion mode
" 1st -> complete to the longest match
" 2nd -> show a list of all completions
" 3rd -> start cycling full completions
set wildmode=longest,list,full
set wildmenu

set number nowrap cursorline
set listchars=tab:»\ ,trail:·

au BufNewFile,BufRead *.install set filetype=php
au BufNewFile,BufRead *.module  set filetype=php
au BufNewFile,BufRead *.inc     set filetype=php
au BufNewFile,BufRead *.test    set filetype=php

au BufNewFile,BufRead Guardfile set filetype=ruby

au FileType css setlocal list
au FileType php setlocal makeprg=php\ -l\ %
au FileType php setlocal errorformat=%m\ in\ %f\ on\ line\ %l,%-GErrors\ parsing\ %f,%-G
au FileType php setlocal list
au FileType make setlocal list noexpandtab tabstop=4

set hlsearch
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


" Quickfix window's default height
set previewheight=25
command Gci Gcommit

nnoremap <F2> :set paste!<CR>

let g:vdebug_options= {
\    "break_on_open" : 1,
\    "watch_window_style" : 'compact',
\}

function! GrepCurrentWord()
  let current = expand('<cword>')
  execute ":grep -Q '". current . "'"
endfunction!
nnoremap <F8> :call GrepCurrentWord()<CR>

call unite#filters#matcher_default#use(['matcher_fuzzy'])


let g:vdebug_options['server'] = "0.0.0.0"
let g:vdebug_options['path_maps'] = {
\    '/home/acro/accounts/barcodestalk/barcodestalk/wwwroot': '/home/wharding/work/barcodestalk/barcodestalk/wwwroot'
\}


let g:tagbar_type_php  = {
  \ 'ctagstype' : 'php',
  \ 'kinds'     : [
    \ 'i:interfaces',
    \ 'c:classes',
    \ 'd:constant definitions',
    \ 'f:functions',
    \ 'j:javascript functions:1'
  \ ]
\ }

