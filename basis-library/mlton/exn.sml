structure MLtonExn =
   struct
      open Primitive.Exn

      type t = exn
	 
      val addExnMessager = General.addExnMessager

      val history: t -> string list =
	 if keepHistory
	    then (setInitExtra (NONE: extra)
		  ; setExtendExtra (fn e =>
				    case e of
				       NONE => SOME (MLtonCallStack.current ())
				     | SOME _ => e)
		  ; fn e => (case extra e of
				NONE => []
			      | SOME cs =>
				   (* The tl gets rid of the anonymous function
				    * passed to setExtendExtra above.
				    *)
				   tl (MLtonCallStack.toStrings cs)))
	 else fn _ => []

      local
	 val message = Primitive.Stdio.print
      in
	 fun 'a topLevelHandler (exn: exn): 'a =
	    (message (concat ["unhandled exception: ", exnMessage exn, "\n"])
	     ; (case history exn of
		   [] => ()
		 | l =>
		      (message "with history:\n"
		       ; (List.app (fn s => message (concat ["\t", s, "\n"]))
			  l)))
	     ; Exit.exit Exit.Status.failure)
	    handle _ => (message "Toplevel handler raised exception.\n"
			 ; Primitive.halt Exit.Status.failure
			 (* The following raise is unreachable, but must be there
			  * so that the expression is of type 'a.
			  *)
			 ; raise Fail "bug")
      end
   end
