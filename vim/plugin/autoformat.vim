

function! AutoFormatSave()
  if exists("b:fmt_prog")
    let l:out = system(b:fmt_prog, getline(1, '$'))
    if v:shell_error == 0
      let l:tmpname = tempname()
      call writefile(split(l:out, '\n'), l:tmpname)
      call rename(l:tmpname, expand('%'))
      silent edit!
      call delete(l:tmpname)
    else
      echo "Error formatting" 
    endif
  endif

endfunction
