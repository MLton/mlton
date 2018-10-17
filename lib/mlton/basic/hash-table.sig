(* Copyright (C) 2017 Jason Carr
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
signature HASH_TABLE =
   sig
      type ('a, 'b) t

      val insertIfNew: ('a, 'b) t * 'a * (unit -> 'b) * ('b -> unit) -> 'b
      val layout: ('a * 'b -> Layout.t) -> ('a, 'b) t -> Layout.t
      val lookupOrInsert: ('a, 'b) t * 'a * (unit -> 'b) -> 'b
      val new: {equals: 'a * 'a -> bool,
                hash: 'a -> word} -> ('a, 'b) t
      val peek: ('a, 'b) t * 'a -> 'b option
      val size: ('a, 'b) t -> int
      val stats: unit -> Layout.t
   end

functor TestHashTable (S: HASH_TABLE): sig end =
struct

open S

val _ = Assert.assert("TestHashTable", fn () => true)
end
