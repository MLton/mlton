(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Set() :
   sig
      type 'a t

      val make: {equal: 'a * 'a -> bool,
                  output: 'a * Out.t -> unit} ->
         {empty: 'a t,
          isEmpty: 'a t -> bool,
          forall: 'a t * ('a -> bool) -> bool,
          equal: 'a t * 'a t -> bool,
          ...}
   end

fun make{equal, output} =
   let
      val empty = []
      fun isEmpty [] = true
        | isEmpty _ = false
      val forall = List.forall
   in {empty = empty,
       isEmpty = isEmpty,
