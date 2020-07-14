func! EnsureManager()
  let plugPath = expand("~/.config/nvim/autoload/plug.vim")
  if !filereadable(plugPath)
    echo 'downloading plugin manager'
    echo system("curl -fLo " . plugPath . " --create-dirs "
          \. "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
  endif
endfunc
call EnsureManager()
call plug#begin('~/.config/nvim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dadbod', { 'for': 'sql' }
Plug 'mikepjb/vim-pair'
Plug 'mikepjb/vim-fold', { 'for': ['css', 'markdown', 'javascript'] }

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

Plug 'tpope/vim-fireplace', { 'for': ['clojure'] }

Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim' 
Plug 'maxmellon/vim-jsx-pretty'

Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf', { 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'natebosch/vim-lsc'

call plug#end()

syn on
filetype plugin indent on
set nocompatible nu autowrite hidden shiftwidth=2 tabstop=2 gdefault mouse=a
set clipboard=unnamed,unnamedplus expandtab smarttab ignorecase smartcase
set shortmess+=c iskeyword+=- path+=** wildmenu noswapfile textwidth=79
set hlsearch
set wildignore+=**/node_modules/**,**/cljs/**.js*,**/cljs-out/*,**/target/*
" set completeopt=menu,menuone,noinsert,noselect
set stl=--\ %1*%F%m%r%h%w%*\ %=\ %y\ -\ [%l,%c]\ [%L,%p%%] showtabline=1
set fillchars=stlnc:\-,stl:\-,vert:\|
setglobal grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ \ %l%m
if executable('rg')
  setglobal grepprg=rg\ -s\ --vimgrep
elseif has('unix')
  " . will search for everything, remove if you want .clj etc
  setglobal grepprg=grep\ -rn\ $*\ .\ /dev/null
endif
if !has('nvim')
  set scl=number
endif
setglobal tags=./tags;
runtime macros/matchit.vim
let g:ftplugin_sql_omni_key = '<Nop>' " ctrl+c is for escape, not completion.


let mapleader= ' '
nnoremap R :call Reload()<CR>
nnoremap Y y$
nnoremap Q @q
nnoremap gb :Gblame<cr>
nnoremap <C-q> :quit<cr>
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
nnoremap <leader>h :LSClientShowHover<CR>
nnoremap <leader>f :find<space>
nnoremap <leader>g :grep<space>
nnoremap <leader>i :e ~/src/rollout/rollout<cr>
nnoremap <leader>l :e ~/.log.md<cr>
nnoremap <leader>b :b<space>

nnoremap <Tab> :bnext<cr>
nnoremap <S-Tab> :bprevious<cr>
nnoremap <leader>d :bd<cr>

" conflicts with neovim terminal <CR> on finish
" nnoremap <CR> :!elm make src/Main.elm --output elm.js<CR>
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

" Use <C-L> to:
"   - redraw
"   - clear 'hlsearch'
"   - update the current diff (if any)
" Use {count}<C-L> to:
"   - reload (:edit) the current buffer
nnoremap <silent><expr> <C-G> (v:count ? ':<C-U>:call <SID>save_change_marks()\|edit\|call <SID>restore_change_marks()<CR>' : '')
      \ . ':nohlsearch'.(has('diff')?'\|diffupdate':'')
      \ . '<CR><C-L>'

if !exists('*Reload') " do not redefine self
  function! Reload()
    if has('nvim')
      botright 20 split | terminal ~/src/rollout/rollout
    else
      execute '!~/src/rollout/rollout'
    endif
    source ~/.config/nvim/init.vim
    " does not update the first time.. 2 times reload shows the change though..
    " colorscheme rollout
  endfunction
endif

command! TrimWhitespace :%s/\s\+$//e
command! ClearPrefixWhitespace :%s/^\s\+//g
command! PrettifyJSON :%!python -m json.tool
command! PrettifyXML  :%!xmllint --format -
command! JackInCljs :CljEval (figwheel.main.api/cljs-repl "dev")<cr>
nnoremap zS :echo join(reverse(map(synstack(line('.'), col('.')), 'synIDattr(v:val,"name")')),' ')<cr>

colorscheme rollout

set cot+=preview

augroup clojure
  au Syntax clojure nmap <buffer>  gd <Plug>FireplaceDjump
augroup end

let g:ale_linters = {
  \ 'javascript': ['prettier', 'eslint'],
  \ 'vue': ['eslint', 'stylelint']
  \ }
let g:ale_fixers = {
  \ 'javascript': ['prettier', 'eslint'],
  \ 'vue': ['prettier', 'eslint', 'stylelint']
  \ }
let g:ale_linter_aliases = {'vue': ['css', 'javascript']}
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
let g:ale_fix_on_save = 1
let g:ale_lint_delay = 0
let g:ale_set_quickfix = 0
let g:ale_set_loclist = 0

" if executable('tailwindcss-language-server')
"   let g:tailwind_server_config = {
"     \ 'html': {
"     \   'command': 'tailwindcss-language-server --stdio',
"     \   'suppress_stderr': v:false,
"     \   },
"     \ 'javascript': {
"     \   'command': 'tailwindcss-language-server --stdio',
"     \   'suppress_stderr': v:false,
"     \   },
"     \ }
"   let g:lsc_enable_autocomplete  = v:true
" 
"   let g:lsc_server_commands = g:tailwind_server_config
" endif

" if executable('javascript-typescript-langserver')
"   let g:javascript_server_config = {
"     \ 'html': {
"     \   'command': 'javascript-typescript-stdio',
"     \   'suppress_stderr': v:true,
"     \   },
"     \ 'javascript': {
"     \   'command': 'javascript-typescript-stdio',
"     \   'suppress_stderr': v:true,
"     \   },
"     \ }
"   let g:lsc_enable_autocomplete  = v:true
" 
"   let g:lsc_server_commands = g:javascript_server_config
" endif

let g:lsc_auto_map = {
 \  'GoToDefinition': 'gd',
 \  'FindReferences': 'gr',
 \  'Rename': 'gR',
 \  'ShowHover': 'K',
 \  'Completion': 'omnifunc',
 \}

func! GoErrInline()
  let wordUnderCursor = expand("<cword>")
  exec "normal! ^"
  if wordUnderCursor == "if"
    exec "normal! k^"
  elseif wordUnderCursor != "err"
    echom "no err conditional found nearby, doing nothing."
  endif
  exec "normal! \"gD\"zdd^W\"gPa; "
endfunc

augroup go
  " remove all previous commands in this group
  au!
  let g:go_template_autocreate = 0 " do not pause ages creating a template.
  au Syntax go nnoremap gD :GoDecls<cr>
  au Syntax go nnoremap gI :GoImports<cr>
  au Syntax go nnoremap gt :GoTest<cr>
  au Syntax go nnoremap M :call GoErrInline()<cr>
augroup end

function! SwapLanguageServer(config)
  LSClientDisable
  for [filetype, config] in items(a:config)
    call RegisterLanguageServer(filetype, config)
  endfor
  LSClientEnable
endfunction
command! Tailwind :call SwapLanguageServer(g:tailwind_server_config)
command! JS :call SwapLanguageServer(g:javascript_server_config)

let g:closetag_filenames = '*.html,*.xhtml,*.md,*.js,*.vue'
