

function! NewlineToNewline()
  :s/\\n//g
endfunction

command! NewlineToNewline call NewlineToNewline()
