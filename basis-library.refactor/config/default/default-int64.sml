(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Int = Int64
type int = Int.int

functor CharAddToFromInt(type char
                         val fromInt64 : Int64.int -> char
                         val toInt64 : char -> Int64.int) =
   struct
      val fromInt = fromInt64
      val toInt = toInt64
   end
functor IntAddToFromInt(type int
                        val fromInt64 : Int64.int -> int
                        val toInt64 : int -> Int64.int) =
   struct
      val fromInt = fromInt64
      val toInt = toInt64
   end
functor WordAddToFromInt(type word
                         val fromInt64 : Int64.int -> word
                         val toInt64 : word -> Int64.int
                         val toInt64X : word -> Int64.int) =
   struct
      val fromInt = fromInt64
      val toInt = toInt64
      val toIntX = toInt64X
   end
