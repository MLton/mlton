(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FUNCTION =
   sig
      val curry: ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
      val compose: ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
      val layout: ('a -> 'b) -> Layout.t
      val output: ('a -> 'b) * TextIO.outstream -> unit
      val seq: ('a -> 'b) * ('b -> 'c) -> ('a -> 'c)
      val seq3: ('a -> 'b) * ('b -> 'c) * ('c -> 'd) -> ('a -> 'd)
      val uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
   end
