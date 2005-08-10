(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
