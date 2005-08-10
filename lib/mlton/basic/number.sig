(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature NUMBER =
   sig
      include INTEGER
      structure I : INTEGER

      val / : t * t -> t 
      val inverse : t -> t
      val ln : t -> t
      val exp : t -> t
      val log : t * t -> t
      val log2 : t -> t

      val fromReal : real -> t
	 
      (* Rational Specific *)
      val numerator : t -> I.t
      val denominator : t -> I.t
   end
