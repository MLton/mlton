(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

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
