structure FileDesc: FILE_DESC =
   struct
      open Posix.IO Posix.FileSys

      type t = file_desc

      fun move {from, to} =
	 if from <> to
	    then (dup2 {old = from, new = to}
		  ; close from)
	 else ()

      fun fluidLet (d1, d2, f) =
	 let
	    val copy = dup d1
	    val _ = dup2 {old = d2, new = d1}
	 in
	    DynamicWind.wind (f, fn () => move {from = copy, to = d1})
	 end
   end
   
