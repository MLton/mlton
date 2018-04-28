(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Escape: ESCAPE =
struct

type 'a t = 'a -> exn

fun escape (e, a) = raise (e a)

fun new (f: 'a t -> 'a): 'a =
   let exception E of 'a
   in f E handle E a => a
   end

end
