(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure Real =
   struct
      open Real
	 
      val fromLargeInt: IntInf.int -> real =
	 fn _ => raise Fail "Real.fromLargeInt"
      val toLargeInt: IEEEReal.rounding_mode -> real -> IntInf.int =
	 fn _ => fn _ => raise Fail "Real.toLargeInt"
   end
