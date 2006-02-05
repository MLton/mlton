(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Int = Int32
type int = Int.int

functor CharAddToFromInt(type char
                         val fromInt32 : Int32.int -> char
                         val toInt32 : char -> Int32.int) =
   struct
      val fromInt = fromInt32
      val toInt = toInt32
   end
functor IntAddToFromInt(type int
                        val fromInt32 : Int32.int -> int
                        val toInt32 : int -> Int32.int) =
   struct
      val fromInt = fromInt32
      val toInt = toInt32
   end
functor WordAddToFromInt(type word
                         val fromInt32 : Int32.int -> word
                         val toInt32 : word -> Int32.int
                         val toInt32X : word -> Int32.int) =
   struct
      val fromInt = fromInt32
      val toInt = toInt32
      val toIntX = toInt32X
   end
