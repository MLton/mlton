structure MLtonExn =
   struct
      open Primitive.Exn

      type t = exn

      val history: t -> string list =
	 if keepHistory
	    then (setInitExtra ([]: extra)
		  ; setExtendExtra (op ::)
		  ; extra)
	 else fn _ => []

      val rec exnMessage: t -> string =
	 fn Fail s => concat ["Fail: ", s]
	  | IO.Io {cause, function, name, ...} => 
	       concat ["Io: ", function, " \"", name, "\" failed with ",
		       exnMessage cause]
	  | PosixError.SysErr (s, eo) =>
	       concat ["SysErr: ", s,
		       case eo of
			  NONE => ""
			| SOME e => concat [" [", PosixError.errorName e, "]"]]
	  | e => exnName e
	    
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
	     ; MLtonProcess.exit MLtonProcess.failure)
	    handle _ => (message "Toplevel handler raised exception.\n"
			 ; Primitive.halt MLtonProcess.failure
			 ; raise Fail "bug")
      end
   end

structure General: GENERAL =
   struct
      open General

      val exnMessage = MLtonExn.exnMessage
   end

structure GeneralGlobal: GENERAL_GLOBAL = General
open GeneralGlobal
