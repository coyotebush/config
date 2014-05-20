"set ts=2 sts=2 sw=2 et
set lbr
set conceallevel=2
set cc=
set spell

noremap <buffer> <silent> k gk
noremap <buffer> <silent> j gj
noremap <buffer> <silent> 0 g0
noremap <buffer> <silent> $ g$

" specifically for Latex-Suite
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
"au BufWritePost *.tex silent call Tex_CompileLatex()
"au BufWritePost *.tex silent !pkill -USR1 xdvi.bin
let g:Tex_ItemStyle_snum='\item '

