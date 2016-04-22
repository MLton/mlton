(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
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

         val new0 = fn () => #[]

         val new1 = fn x => #[x]

         val new2 = fn (x0, x1) => #[x0, x1]

         val new3 = fn (x0, x1, x2) => #[x0, x1, x2]

         val new4 = fn (x0, x1, x2, x3) => #[x0, x1, x2, x3]

         val new5 = fn (x0, x1, x2, x3, x4) => #[x0, x1, x2, x3, x4]

         val new6 = fn (x0, x1, x2, x3, x4, x5) => #[x0, x1, x2, x3, x4, x5]
      end
   end
