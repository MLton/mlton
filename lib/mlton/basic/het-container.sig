(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature HET_CONTAINER =
   sig
      type t

      val new: unit -> {make: 'a -> t,
                        pred: t -> bool,
                        peek: t -> 'a option}
   end
