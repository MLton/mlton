structure MLtonExn =
   struct
      open Primitive.Exn

      type t = exn

      val addExnMessager = General.addExnMessager

      val history: t -> string list =
	 if keepHistory
	    then (setInitExtra ([]: extra)
		  ; setExtendExtra (op ::)
		  ; extra)
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
	     ; MLtonProcess.exit MLtonProcess.Status.failure)
	    handle _ => (message "Toplevel handler raised exception.\n"
			 ; Primitive.halt MLtonProcess.Status.failure
			 ; raise Fail "bug")
      end
   end
