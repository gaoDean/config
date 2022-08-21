
syn match nroffInline /\$.\{-}\$/
syn match nroffHeaderArg /\%(\.[sml]h\)\@<=.*/

hi link nroffHeaderArg GruvboxPurpleBold
hi link nroffInline PreProc

hi link nroffDefSpecial PreProc
hi link nroffRequest GruvboxBlueBold
