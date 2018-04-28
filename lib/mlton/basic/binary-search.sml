(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BinarySearch: BINARY_SEARCH =
struct

(* Based on page 38 of Programming Pearls, by Jon Bentley. *)
fun 'a search (a: 'a array, f: 'a -> order): int option =
   let 
      fun loop (min: int, max: int): int option =
         if min > max
            then NONE
         else
            let val mid = Int.quot (min + max, 2)
            in case f (Array.sub (a, mid)) of
               LESS => loop (min, mid - 1)
             | EQUAL => SOME mid
             | GREATER => loop (mid + 1, max)
            end
   in loop (0, Array.length a - 1)
   end

fun 'a largest (a: 'a array, f: 'a -> bool): int option =
   let 
      fun loop(min, max, res: int option): int option =
         if min > max
            then res
         else
            let val mid = Int.quot(min + max, 2)
            in if f(Array.sub(a, mid))
                  then loop(mid + 1, max, SOME mid)
               else loop(min, mid - 1, res)
            end
   in loop(0, Array.length a - 1, NONE)
   end

fun 'a smallest(a: 'a array, f: 'a -> bool): int option =
   let 
      fun loop(min, max, res: int option): int option =
         if min > max
            then res
         else
            let val mid = Int.quot(min + max, 2)
            in if f(Array.sub(a, mid))
                  then loop(min, mid - 1, SOME mid)
               else loop(mid + 1, max, res)
            end
   in loop(0, Array.length a - 1, NONE)
   end

end
