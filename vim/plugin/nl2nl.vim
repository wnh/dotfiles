

function! NewlineToNewline()
  :s/\\n/
endfunction

command! NewlineToNewline call NewlineToNewline()