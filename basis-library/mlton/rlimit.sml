structure MLtonRlimit: MLTON_RLIMIT =
   struct
      open Primitive.MLton.Rlimit

      val get =
	 fn (r: t) =>
	 PosixError.SysCall.syscall
	 (fn () =>
	  (get r, fn () => 
	   {hard = getHard (),
	    soft = getSoft ()}))

      val set =
	 fn (r: t, {hard, soft}) =>
	 PosixError.SysCall.simple
	 (fn () => set (r, hard, soft))
   end
