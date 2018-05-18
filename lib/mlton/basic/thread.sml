(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Thread: THREAD =
struct

open MLton.Thread

fun generate (f: ('a -> unit) -> unit): unit -> 'a option =
   let
      val paused: 'a option t option ref = ref NONE
      val gen: unit t option ref = ref NONE
      fun return (a: 'a option): unit =
         switch (fn t' =>
                 let
                    val _ = gen := SOME t'
                    val t = valOf (!paused)
                    val _ = paused := NONE
                 in
                    prepare (t, a)
                 end)
      val _ =
         gen := SOME (new (fn () => (f (return o SOME)
                                     ; return NONE)))
   in fn () => switch (fn t => (paused := SOME t
                                ; prepare (valOf (!gen), ())))
   end

end
