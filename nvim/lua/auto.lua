vim.cmd(
[[
au BufRead,BufNewFile *.ms setl ft=nroff spell
au BufRead,BufNewFile *.ms nnoremap j gj
au BufRead,BufNewFile *.ms nnoremap k gk
au BufRead,BufNewFile *.md nnoremap j gj
au BufRead,BufNewFile *.md nnoremap k gk
au BufRead,BufNewFile *.md setl spell breakindent
au BufRead,BufNewFile *.html nnoremap j gj
au BufRead,BufNewFile *.html nnoremap k gk
au BufRead,BufNewFile *.html setl tabstop=2 shiftwidth=2
au BufRead,BufNewFile *.amber setl ft=amber tabstop=2 shiftwidth=2 list
au BufRead,BufNewFile *.gcss setl ft=css expandtab shiftwidth=2 list
au BufRead,BufNewFile *.bib set ft=rbib
au BufRead,BufNewFile *.sh set ft=bash
au BufRead,BufNewFile [Nn]eomuttrc,.neomuttrc,.neomutt/*muttrc,.config/neomutt/*muttrc setfiletype neomuttrc
au BufRead,BufNewFile neomutt-*-\w\+,neomutt[[:alnum:]_-]\{6\} setfiletype mail
au BufRead,BufNewFile * setl formatoptions-=c formatoptions+=j 
]]
) -- made with vimauto