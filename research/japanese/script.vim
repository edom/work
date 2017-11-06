:function! g:LookupSelection()
:   let string = getreg()
:   let result = ''
:   redir => result
:   silent execute '!./lookup' string
:   redir END
:   echo result
:endfunction

:function! g:LookupSelection_All()
:   let string = getreg()
:   let result = ''
:   redir => result
:   silent execute '!u=1 ./lookup' string
:   redir END
:   echo result
:endfunction

:function! g:LookupSelection_VerbOnly()
:   let string = getreg()
:   let result = ''
:   redir => result
:   silent execute '!v=1 ./lookup' string
:   redir END
:   echo result
:endfunction

" Look up in dictionary:
" (Normal mode) character under cursor
" (Visual mode) selected string
:map <F3> yl:call g:LookupSelection()<CR>
:vmap <F3> y:call g:LookupSelection_All()<CR>

" Look up character under cursor in dictionary, returning verbs only.
:map <F4> yl:call g:LookupSelection_VerbOnly()<CR>

" Copy to clipboard:
" (Normal mode) character under cursor
" (Visual mode) selected string
:map <F5> <Esc><Esc><Esc>"+yl
:vmap <F5> "+y
