(* Copyright (C) 2006-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure One:
   sig
      type 'a t

      val make: (unit -> 'a) -> 'a t
      val use: 'a t * ('a -> 'b) -> 'b
   end =
   struct
      datatype 'a t = T of {more: unit -> 'a,
                            static: 'a,
                            staticIsInUse: bool ref}

      fun make f = T {more = f,
                      static = f (),
                      staticIsInUse = ref false}

      fun use (T {more, static, staticIsInUse}, f) =
         let
            val () = Primitive.MLton.Thread.atomicBegin ()
            val b = ! staticIsInUse
            val d =
               if b then
                  (Primitive.MLton.Thread.atomicEnd ();
                   more ())
               else
                  (staticIsInUse := true;
                   Primitive.MLton.Thread.atomicEnd ();
                   static)
        in
           DynamicWind.wind (fn () => f d,
                             fn () => if b then () else staticIsInUse := false)
        end
   end
