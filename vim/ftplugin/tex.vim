
" specifically for Latex-Suite
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
au BufWritePost *.tex silent call Tex_CompileLatex()
au BufWritePost *.tex silent !pkill -USR1 xdvi.bin
let g:Tex_ItemStyle_snum='\item '
