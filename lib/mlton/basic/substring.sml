(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Substring: SUBSTRING =
   struct
      open Pervasive.Substring

      type t = substring

      val length = size

      val substring =
         fn (s, {start, length}) => substring (s, start, length)

      val base =
         fn ss => let val (s, start, length) = base ss
                  in (s, {start = start, length = length})
                  end

      val toString = string

      val layout = String1.layout o toString

      fun endOf ss =
         let
            val (_, {start, length}) = base ss
         in
            start + length
         end
   end
