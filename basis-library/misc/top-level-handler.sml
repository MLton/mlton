val _ =
   let
      val message = Primitive.Stdio.print
      fun handler exn =
	 (message "unhandled exception: "
	  ; let
	       fun loop e =
		  case e of
		     Fail s => (message "Fail "; message s)
		   | IO.Io {cause, function, ...} => (message "IO "
						      ; message function
						      ; message ": "
						      ; loop cause)
		   | _ => message (exnName exn)
	    in
	       loop exn
	    end
	  ; message "\n"
	  ; (case MLton.Exn.history exn of
		[] => ()
	      | l =>
		   (message "with history:\n"
		    ; (List.app
		       (fn s => (message "\t"; message s; message "\n"))
		       l)))
	  ; let open OS.Process
	    in exit failure 
	    end) handle _ =>
	 (message "Toplevel handler raised exception.\n"
	  ; Primitive.halt 1)
   in Primitive.Exn.setTopLevelHandler handler
   end
