hi clear
if exists("syntax_on")
  syntax reset
endif
set background=dark

let g:colors_name = "rollout"

hi Normal ctermfg=NONE " ex line text & some special chars :? etc?
hi Constant ctermfg=5
hi PreProc ctermfg=1
hi Repeat ctermfg=1
hi Ignore ctermfg=1
hi Number ctermfg=2
hi Function ctermfg=6
hi String ctermfg=7
hi Search ctermbg=8 ctermfg=NONE
hi QuickFixLine ctermbg=0 ctermfg=NONE
hi Identifier ctermfg=3
hi Keyword ctermfg=1
hi Special ctermfg=3
hi Preproc ctermfg=1
hi Type ctermfg=2
hi Comment ctermfg=8
hi TODO ctermfg=4 ctermbg=NONE
hi LineNr ctermfg=8 ctermbg=NONE
hi SignColumn ctermfg=8 ctermbg=NONE
hi CursorLine ctermfg=7 ctermbg=NONE
hi Visual ctermfg=NONE ctermbg=0
hi Error ctermfg=1 ctermbg=NONE
hi ErrorMsg ctermfg=1 ctermbg=NONE
hi WarningMsg ctermfg=1 ctermbg=NONE
hi NonText ctermfg=1 ctermbg=NONE
hi Delimiter ctermfg=6 ctermbg=NONE
hi MatchParen ctermfg=3 ctermbg=NONE
hi Operator ctermfg=1 ctermbg=NONE
hi PMenu ctermfg=15 ctermbg=0
hi PMenuSel ctermfg=6 ctermbg=NONE
hi PMenuSbar ctermfg=6 ctermbg=0
hi PMenuThumb ctermfg=NONE ctermbg=6
hi Folded ctermfg=7 ctermbg=0
hi Quote ctermfg=6 ctermbg=8
hi FoldedColumn ctermfg=6 ctermbg=8
hi StatusLine ctermfg=15 ctermbg=NONE cterm=NONE
hi StatusLineNC ctermfg=8 ctermbg=NONE cterm=NONE
hi VertSplit ctermfg=7 ctermbg=NONE cterm=NONE
hi User1 ctermfg=2 ctermbg=NONE cterm=NONE

hi! htmlTagName ctermfg=6
hi! htmlSpecialTagName ctermfg=6
hi! htmlTag ctermfg=NONE
hi! htmlTagN ctermfg=NONE
hi! htmlEndTag ctermfg=NONE
hi! link htmlH1 Normal
hi! link jsonQuote Keyword
hi! link jsonKeyword Keyword

" Common groups that link to default highlighting.
" You can specify other highlighting easily.
hi link String	Constant
hi link Character	Constant
hi link Number	Constant
hi link Boolean	Constant
hi link Float		Number
hi link Conditional	Repeat
hi link Label		Statement
hi link Keyword	Statement
hi link Exception	Statement
hi link Include	PreProc
hi link Define	PreProc
hi link Macro		PreProc
hi link PreCondit	PreProc
hi link StorageClass	Type
hi link Structure	Type
hi link Typedef	Type
hi link Tag		Special
hi link SpecialChar	Special
hi link Delimiter	Special
hi link SpecialComment Special
hi link Debug		Special

hi ALEError ctermbg=0 ctermfg=NONE cterm=NONE
hi ALEWarning ctermbg=0 ctermfg=NONE cterm=NONE

set stl=--\ %1*%F%m%r%h%w%*\ %=\ %y\ -\ [%l,%c]\ [%L,%p%%] showtabline=1
