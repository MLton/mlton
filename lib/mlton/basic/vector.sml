(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Vector =
   let
      structure V = Vector (local
                               open Pervasive.Vector
                            in
                               type 'a t = 'a vector
                               exception New = Size
                               val length = length
                               val sub = sub
                               val unfoldi = MLton.Vector.unfoldi
                               val unsafeSub = Unsafe.Vector.sub
                            end)
   in
      struct
         open V

         type 'a vector = 'a t

         (* The built-in concat is faster in MLton because it can use
          * Vector.fromArray.
          * See src/basis-library/arrays-and-vectors/sequence.fun.
          *)
         val concat = Pervasive.Vector.concat
      end
   end
