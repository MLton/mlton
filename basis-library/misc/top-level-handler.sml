val _ =
   let
      val message = Primitive.Stdio.print
      fun handler exn =
	 (message "unhandled exception "
	  ; (case exn of
		Fail s => (message "Fail "; message s)
	      | _ => message (exnName exn))
	  ; message "\n"
	  ; let open OS.Process
	    in exit failure 
	    end) handle _ => (message "Toplevel handler raised exception.\n"
			      ; Primitive.halt 1)
   in Primitive.Exn.setTopLevelHandler handler
   end
