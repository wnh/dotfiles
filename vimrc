
set nocompatible              " be iMproved, required
filetype off                  " required
set timeoutlen=50
set t_Co=256
let base16colorspace=256
syntax on 

set encoding=utf-8            " The encoding displayed.
setglobal fileencoding=utf-8  " The encoding written to file.

set cscopetag

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
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-fugitive'
Plugin 'godlygeek/tabular'
Plugin 'Raimondi/delimitMate'
Plugin 'vim-scripts/dbext.vim'
"Plugin 'Valloric/YouCompleteMe'
Plugin 'mileszs/ack.vim'

Plugin 'joonty/vdebug'
Plugin 'majutsushi/tagbar'
Plugin 'Shougo/unite.vim'
Plugin 'tpope/vim-dispatch'

Plugin 'scrooloose/syntastic'

Plugin 'ervandew/supertab'
Plugin 'SerVer/ultisnips'

"Plugin 'terryma/vim-multiple-cursors'
Plugin 'mattn/emmet-vim'

Plugin 'digitaltoad/vim-jade'

Plugin 'chriskempson/base16-vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'groenewege/vim-less'
"Plugin 'mitechie/govim'
Plugin 'fatih/vim-go'

Plugin 'raichoo/haskell-vim'
Plugin 'rust-lang/rust.vim'

Plugin 'closetag.vim'

Plugin 'hashivim/vim-terraform'


call vundle#end()            " required
filetype plugin indent on    " required
filetype plugin on

let g:airline_left_sep  = ' '
let g:airline_right_sep = ' '

colorscheme base16-default
let g:airline_theme='base16'

set background=dark laststatus=2

set diffopt+=vertical

command W w
command Vsp vsp
command Sp sp

" Emmet makes :E ambigious, put it back
command E Explore

" set tab completion mode
" 1st -> complete to the longest match
" 2nd -> show a list of all completions
" 3rd -> start cycling full completions
set wildmode=longest,list,full
set wildmenu

set number nowrap cursorline
set listchars=tab:»\ ,trail:·

au BufNewFile,BufRead *.tf      set filetype=terraform
au BufNewFile,BufRead *.tfstate set filetype=terraform

au BufRead,BufNewFile *.go      set filetype=go
au BufNewFile,BufRead *.less    set filetype=less

let g:go_fmt_command = "goimports"

au BufNewFile,BufRead Guardfile set filetype=ruby

au FileType css setlocal list
au FileType php setlocal makeprg=php\ -l\ %
au FileType php setlocal errorformat=%m\ in\ %f\ on\ line\ %l,%-GErrors\ parsing\ %f,%-G
au FileType php setlocal list
au FileType make setlocal list noexpandtab tabstop=4

au FileType javascript setlocal tabstop=4 shiftwidth=4 expandtab
au BufRead,BufNewFile *.jade set filetype=jade

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


let NERDTreeIgnore=['\.pyc$', '\~$']

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|node_modules)$',
  \ }

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

vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

set exrc
set secure

