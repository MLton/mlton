structure Exn: MLTON_EXN =
   struct
      open Primitive.Exn

      type t = exn

      val history: t -> string list =
	 if keepHistory
	    then (
		  (* In setInitExtra f, f cannot contain any free variables,
		   * since implement-exceptions will move it to the top of the
		   * program.
		   *)
		  setInitExtra (fn () => (ref []): extra)
		  ; setRaise (fn (s, e) =>
			      let
				 val r = extra e
			      in
				 r := s :: !r
			      end)
		  ; ! o extra)
	 else fn _ => []

      local
	 val message = Primitive.Stdio.print
      in
	 fun topLevelHandler exn =
	    (message "unhandled exception: "
	     ; let
		  fun loop e =
		     case e of
			Fail s => 
			  (message "Fail "; message s)
		      | IO.Io {name, function, cause, ...} => 
			  (message "IO "
			   ; message function
			   ; message " on "
			   ; message name
			   ; message ": "
			   ; loop cause)
		      | PosixError.SysErr (s, _) =>
			   (message "SysErr "; message s)
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
	     ; Process.exit 1)
	    handle _ => (message "Toplevel handler raised exception.\n"
			 ; Primitive.halt 1)
      end

      val _ = Primitive.Exn.setTopLevelHandler topLevelHandler
   end
