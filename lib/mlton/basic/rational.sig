(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature RATIONAL =
   sig
      include ORDERED_FIELD

      structure I : INTEGER

      val isInt : t -> bool

      val make : I.t * I.t -> t

      val fromInt : I.t -> t
      val toInt : t -> I.t
      val numerator : t -> I.t
      val denominator : t -> I.t
	 
(*      val toInt : t -> int
      val intTo : int -> t
      val intIntTo : int * int -> t
	 *)
      val toReal : t -> real
   end
