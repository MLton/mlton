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
   end
