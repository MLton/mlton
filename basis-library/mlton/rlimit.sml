structure MLtonRlimit: MLTON_RLIMIT =
   struct
      open Primitive.MLton.Rlimit

      val get =
	 fn (r: resource) =>
	 (PosixError.checkResult (get r)
	  ; {hard = getHard (),
	     soft = getSoft ()})

      val set =
	 fn (r: resource, {hard, soft}) =>
	 PosixError.checkResult (set (r, hard, soft))
   end
