val _ =
   let
      val message = Primitive.Stdio.print
      fun handler exn =
	 let
	    val msg =
	       case exn of
		  Fail s => concat ["Fail ", s]
		| _ => exnName exn
	 in (message "unhandled exception "
	     ; message msg
	     ; message "\n"
	     ; let open OS.Process
	       in exit failure 
	       end) handle _ => (message "Toplevel handler raised exception.\n"
				 ; Primitive.halt 1)
	 end
   in Primitive.Exn.setTopLevelHandler handler
   end
