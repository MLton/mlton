(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(*-------------------------------------------------------------------*)
(*                              Iterate                              *)
(*-------------------------------------------------------------------*)

structure Iterate: ITERATE =
struct
   
fun iterate(start, isTerm,next) =
   let fun loop s = if isTerm s then s else loop(next s)
   in loop start
   end
   
fun whileDo(test, f) = iterate((),not o test, f)
   
fun repeatUntil(f, test) = (f() ; iterate((), test, f))
   
end
