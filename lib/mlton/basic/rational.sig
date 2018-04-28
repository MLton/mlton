(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
