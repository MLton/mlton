(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
