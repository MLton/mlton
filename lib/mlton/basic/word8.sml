(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Word8:
   sig
      include WORD

      val stringToVector: string -> t vector
      val vectorToString: t vector -> string
   end =
   struct
      open Pervasive.Word8
      structure Z = FixWord (Pervasive.Word8)
      open Z

      val equals: t * t -> bool = op =

      fun vectorToString v =
         Pervasive.CharVector.tabulate (Pervasive.Vector.length v, fn i =>
                                        toChar (Pervasive.Vector.sub (v, i)))

      fun stringToVector s =
         Pervasive.Vector.tabulate (Pervasive.String.size s, fn i =>
                                    fromChar (Pervasive.String.sub (s, i)))
   end
