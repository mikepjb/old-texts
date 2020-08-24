func! EnsureManager()
  let plugPath = expand("~/.vim/autoload/plug.vim")
  if !filereadable(plugPath)
    echo 'downloading plugin manager'
    echo system("curl -fLo " . plugPath . " --create-dirs "
          \. "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
  endif
endfunc
call EnsureManager()
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
" Plug 'mikepjb/vim-pair' " for now let's try without autoclosing brackets
Plug 'mikepjb/vim-fold', { 'for': ['css', 'markdown', 'javascript'] }
Plug 'tpope/vim-fireplace', { 'for': ['clojure'] }
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim' 
Plug 'maxmellon/vim-jsx-pretty'
Plug 'Quramy/tsuquyomi'
" Plug 'w0rp/ale' " use over lsc?
Plug 'natebosch/vim-lsc' " used for clj-kondo and eslint?

call plug#end()

syn on
filetype plugin indent on
set nocompatible nu autowrite hidden shiftwidth=2 tabstop=2 gdefault mouse=a
set clipboard=unnamed,unnamedplus expandtab smarttab ignorecase smartcase
set iskeyword+=- path+=** wildmenu noswapfile textwidth=79
set stl=--\ %1*%F%m%r%h%w%*\ %=\ %y\ -\ [%l,%c]\ [%L,%p%%] showtabline=1
if v:version >= 800 | set shortmess+=c | endif
set hlsearch cot+=preview
set fillchars=stlnc:\-,stl:\-,vert:\|
setglobal grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ \ %l%m
if executable('rg')
  setglobal grepprg=rg\ -s\ --vimgrep
elseif has('unix')
  " . will search for everything, remove if you want .clj etc
  setglobal grepprg=grep\ -rn\ $*\ .\ /dev/null
endif
setglobal tags=./tags;
runtime macros/matchit.vim
let g:ftplugin_sql_omni_key = '<Nop>' " ctrl+c is for escape, not completion.

let mapleader= ' '
nnoremap Y y$
nnoremap Q @q
nnoremap gb :Gblame<cr>
nnoremap <C-q> :quit<cr>
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
nnoremap <leader>h :LSClientShowHover<CR>
if executable('selecta')
  nnoremap <leader>f :call SelectaCommand("find * -type f", "", ":e")<cr>
else
  nnoremap <leader>f :find<space>
endif
nnoremap <leader>g :grep<space>
nnoremap <leader>i :e ~/src/rollout/rollout<cr>
nnoremap <leader>l :e ~/.log.md<cr>
nnoremap <leader>b :b<space>

nnoremap <Tab> :bnext<cr>
nnoremap <S-Tab> :bprevious<cr>

imap <C-c> <esc>
map <C-h> <C-w><C-h>
map <C-j> <C-w><C-j>
map <C-k> <C-w><C-k>
map <C-l> <C-w><C-l>
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-d> <Delete>
inoremap <C-l> <space>=><space>
nnoremap gF :ALEFix<cr>
nnoremap gj :ALENextWrap<cr>
nnoremap gk :ALEPreviousWrap<cr>
nnoremap g1 :ALEFirst<cr>
nnoremap g0 :ALEStopAllLSPs<cr>
nnoremap gq :copen<cr>

nnoremap <silent><expr> <C-G> (v:count ? ':<C-U>:call <SID>save_change_marks()\|edit\|call <SID>restore_change_marks()<CR>' : '')
      \ . ':nohlsearch'.(has('diff')?'\|diffupdate':'')
      \ . '<CR><C-L>'

command! TrimWhitespace :%s/\s\+$//e
command! ClearPrefixWhitespace :%s/^\s\+//g
command! PrettifyJSON :%!python -m json.tool
command! PrettifyXML  :%!xmllint --format -
command! JackInCljs :CljEval (figwheel.main.api/cljs-repl "dev")<cr>
nnoremap zS :echo join(reverse(map(synstack(line('.'), col('.')), 'synIDattr(v:val,"name")')),' ')<cr>

colorscheme rollout

augroup clojure
  au Syntax clojure nmap <buffer>  gd <Plug>FireplaceDjump
augroup end

let g:closetag_filenames = '*.html,*.xhtml,*.md,*.js,*.vue'

" Run a given vim command on the results of fuzzy selecting from a given shell
" command. See usage below.
function! SelectaCommand(choice_command, selecta_args, vim_command)
  try
    let selection = system(a:choice_command . " | selecta " . a:selecta_args)
  catch /Vim:Interrupt/
    " Swallow the ^C so that the redraw below happens; otherwise there will be
    " leftovers from selecta on the screen
    redraw!
    return
  endtry
  redraw!
  exec a:vim_command . " " . selection
endfunction
