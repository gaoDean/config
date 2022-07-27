syn match rbibOther ".*"
syn match rbibKey "%[A-Z]\s"
syn match rbibT "%T.*" contains=rbibKey
syn match rbibO "%O.*" contains=rbibKey
hi def link rbibO rbibT
hi def link rbibOther Comment
hi def link rbibKey Identifier
hi def link rbibT PreProc
