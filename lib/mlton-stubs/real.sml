(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Real =
   struct
      open Real

      val fromLargeInt: IntInf.int -> real =
         fn _ => raise Fail "Real.fromLargeInt"
      val toLargeInt: IEEEReal.rounding_mode -> real -> IntInf.int =
         fn _ => fn _ => raise Fail "Real.toLargeInt"
   end
