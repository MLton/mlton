structure FileDesc: FILE_DESC =
   struct
      open Posix.IO Posix.FileSys

      type t = file_desc

      fun move{from, to} =
	 if from <> to
	    then (dup2{old = from, new = to}
		  ; close from)
	 else ()
   end
   
