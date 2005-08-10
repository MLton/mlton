(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
