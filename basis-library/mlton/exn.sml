structure MLtonExn: MLTON_EXN =
   struct
      open Primitive.Exn

      type t = exn

      val history: t -> string list =
	 if keepHistory
	    then (setInitExtra ([]: extra)
		  ; setExtendExtra (op ::)
		  ; extra)
	 else fn _ => []

      local
	 val message = Primitive.Stdio.print
      in
	 fun topLevelHandler exn =
	    (message "unhandled exception: "
	     ; let
		  fun loop e =
		     case e of
			Fail s => message s
		      | IO.Io {cause, function, name, ...} => 
			  (message function
			   ; message " "
			   ; message name
			   ; message ": "
			   ; loop cause)
		      | PosixError.SysErr (s, _) => message s
		      | _ => message (exnName e)
	       in
		  loop exn
	       end
	     ; message "\n"
	     ; (case history exn of
		   [] => ()
		 | l =>
		      (message "with history:\n"
		       ; (List.app
			  (fn s => (message "\t"; message s; message "\n"))
			  l)))
	     ; MLtonProcess.exit 1)
	    handle _ => (message "Toplevel handler raised exception.\n"
			 ; Primitive.halt 1)
      end

      val _ = Primitive.Exn.setTopLevelHandler topLevelHandler
   end
