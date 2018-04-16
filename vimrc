
set nocompatible              " be iMproved, required


set timeoutlen=50
set t_Co=256 
let base16colorspace=256
syntax on

let mapleader=","

set encoding=utf-8            " The encoding displayed.
setglobal fileencoding=utf-8  " The encoding written to file.

set cscopetag "use cscope instead of ctags

filetype plugin indent on    " required
filetype plugin on

execute pathogen#infect()

let g:airline_left_sep  = ' ' " remove annoying custom patched font requrements for airline
let g:airline_right_sep = ' '

colorscheme base16-default-dark
let g:airline_theme='base16'

set background=dark 
set laststatus=2 " always show the status line

set diffopt+=vertical

command W w " remap some often accidentally uppercased stuff
command Vsp vsp
command Sp sp

" set tab completion mode
" 1st -> complete to the longest match
" 2nd -> show a list of all completions
" 3rd -> start cycling full completions
set wildmode=longest,list,full
set wildmenu " adds  a better tab completion UI

set number nowrap cursorline
set listchars=tab:»\ ,trail:·

au BufWritePre * call AutoFormatSave()

au BufNewFile,BufRead *.tf      set filetype=terraform
au BufNewFile,BufRead *.tfstate set filetype=terraform

" fastlane config files
au BufNewFile,BufRead Appfile   set filetype=ruby
au BufNewFile,BufRead Fastfile  set filetype=ruby
au BufNewFile,BufRead Matchfile set filetype=ruby

au BufNewFile,BufRead *.pug     set filetype=haml

au BufRead,BufNewFile *.go      set filetype=go
au BufNewFile,BufRead *.less    set filetype=less
au BufNewFile,BufRead *.glsl    set filetype=glsl
au BufNewFile,BufRead *.h       set filetype=c

let g:go_fmt_command = "goimports"

au BufNewFile,BufRead Guardfile set filetype=ruby

au FileType css setlocal list
au FileType c setlocal noexpandtab tabstop=8
au FileType make setlocal list noexpandtab tabstop=8
au FileType go setlocal noexpandtab tabstop=4
au FileType markdown setlocal list tabstop=4 shiftwidth=4

au FileType javascript setlocal tabstop=4 shiftwidth=4 expandtab
au BufRead,BufNewFile *.jade set filetype=jade

au FileType go setlocal tabstop=4 shiftwidth=4 noexpandtab
au FileType python setlocal tabstop=4 shiftwidth=4 noexpandtab

set hlsearch
set tabstop=2 shiftwidth=2 expandtab
set grepprg=ack


" set hlsearch on at the start. The rules for this are weird
set hlsearch

nnoremap <C-S-n> :cnext<CR>

"TODO debug why these dont work
nnoremap <leader>m :make<CR>
nnoremap <leader>s :w<CR>

function! GrepForLastSearch()
  execute "grep " . shellescape(@/) . " **"
endfunction
command GrepForLastSearch call GrepForLastSearch()


set complete-=t " remove tag completion, usually too much stuff in there

function! ClearTrailingWhitespace()
  execute "%s/\\s\\+$//"
endfunction!

command Trail call ClearTrailingWhitespace()



set previewheight=25   " Quickfix window's default height
command Gci Gcommit

nnoremap <F2> :set paste!<CR>

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

vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

set exrc " allow loading of .vim files from inside a project directory
set secure




command E Explore
