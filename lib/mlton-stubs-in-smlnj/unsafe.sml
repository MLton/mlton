(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ConvertMonoArray (A: UNSAFE_MONO_ARRAY) =
   struct
      open A

      open OpenInt32
      val create = fn i => create (toInt i)
      val sub = fn (a, i) => sub (a, toInt i)
      val update = fn (a, i, x) => update (a, toInt i, x)
   end

functor ConvertMonoVector (V: sig
                               type vector
                               type elem
                               val sub: vector * Int31.int -> elem
                              end) =
   struct
      open V

      val sub = fn (a, i) => sub (a, OpenInt32.toInt i)
   end

structure Unsafe =
   struct
      open OpenInt32
      open Unsafe

      structure Array =
         struct
            open Array

            val create = fn (i, x) => create (toInt i, x)
            val sub = fn (a, i) => sub (a, toInt i)
            val update = fn (a, i, x) => update (a, toInt i, x)
         end

      structure Vector =
         struct
            open Vector

            val sub = fn (a, i) => sub (a, toInt i)
         end

      structure CharVector = ConvertMonoVector (CharVector)
      structure Word8Vector = ConvertMonoVector (Word8Vector)
      structure CharArray = ConvertMonoArray (CharArray)
      structure Word8Array = ConvertMonoArray (Word8Array)
      structure Real64Array = ConvertMonoArray (Real64Array)
   end
