:function! g:LookupKanjiAtCursor()
:   normal yl
:   let char = getreg()
:   let result = ''
:   redir => result
:   silent execute '!./lookup' char
:   redir END
:   echo result
:endfunction

:function! g:LookupKanjiAtCursor_VerbOnly()
:   normal yl
:   let char = getreg()
:   let result = ''
:   redir => result
:   silent execute '!v=1 ./lookup' char
:   redir END
:   echo result
:endfunction

:map <F3> :call g:LookupKanjiAtCursor()<CR>
:map <F4> :call g:LookupKanjiAtCursor_VerbOnly()<CR>
