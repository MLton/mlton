(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure FixedPoint: FIXED_POINT =
struct

fun fix{start, step, equals} =
   let
      fun loop s =
         let val s' = step s
         in if equals(s, s')
               then s
            else loop s'
         end
   in loop start
   end

fun fix' (f: (unit -> unit) -> unit) =
   let
      fun loop() =
         let val changed = ref false
         in f(fn () => changed := true);
            if !changed
               then loop()
            else ()
         end
   in loop()
   end


end
