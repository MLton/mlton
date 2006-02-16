(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure DynamicWind: DYNAMIC_WIND =
struct

fun try (f: unit -> 'a, k: 'a -> 'b, h: exn -> 'b) =
   let
      datatype t =
         A of 'a
       | E of exn
   in
      case A (f ()) handle e => E e of
         A a => k a
       | E e => h e
   end
         
fun wind (thunk, cleanup: unit -> unit) =
   try (thunk, fn a => (cleanup (); a), fn e => (cleanup (); raise e))

end

