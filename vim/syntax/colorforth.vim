" Hide control characters when not inserting on that line.
set concealcursor=nvc
set conceallevel=2
set isprint=32-126
set display+=uhex

" Syntax highlighting only needs to go as far back as the most
" recent control code.
syntax sync match groupthere "[\x81-\x8B]"


" Define styles for the various colors.
highlight forthDefine   ctermbg=white ctermfg=darkred     " red
highlight forthCompile  ctermbg=white ctermfg=darkgreen   " green
highlight forthExecute  ctermbg=white ctermfg=darkyellow  " yellow
highlight forthMacro    ctermbg=white ctermfg=darkcyan    " cyan
highlight forthComment  ctermbg=white ctermfg=black       " white


" Map control codes to styles.
syntax match forthControl  "[\x81-\x8B]" conceal
syntax match forthDefine   "\%x81\_p\+" contains=forthControl  " red
syntax match forthCompile  "\%x82\_p\+" contains=forthControl  " green
syntax match forthExecute  "\%x83\_p\+" contains=forthControl  " yellow
syntax match forthMacro    "\%x86\_p\+" contains=forthControl  " cyan
syntax match forthComment  "\%x87\_p\+" contains=forthControl  " white
syntax match forthComment  "\%^\_p\+"                          " white


" Insert control codes.
nmap <buffer> <LocalLeader>r i<C-V>X81<Esc>  " red
nmap <buffer> <LocalLeader>g i<C-V>X82<Esc>  " green
nmap <buffer> <LocalLeader>y i<C-V>X83<Esc>  " yellow
nmap <buffer> <LocalLeader>c i<C-V>X86<Esc>  " cyan
nmap <buffer> <LocalLeader>w i<C-V>X87<Esc>  " white

" Delete control codes.
nmap <buffer> <LocalLeader>d :s/[\x81-\x8B]//g<CR>


let b:current_syntax = "colorforth"
