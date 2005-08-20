(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Vector
   (V: sig
          type 'a vector
          type 'a elem
          val maxLen: Int31.int 
          val tabulate: Int31.int * (Int31.int -> 'a elem) -> 'a vector 
          val length: 'a vector -> Int31.int 
          val sub: ('a vector * Int31.int) -> 'a elem
          val mapi: ((Int31.int * 'a elem) -> 'b elem) -> 'a vector -> 'b vector 
          val appi: ((Int31.int * 'a elem) -> unit) -> 'a vector -> unit 
          val foldli:
             ((Int31.int * 'a elem * 'b) -> 'b) -> 'b -> 'a vector -> 'b 
          val foldri:
             ((Int31.int * 'a elem * 'b) -> 'b) -> 'b -> 'a vector -> 'b 
       end) =
   struct
      open V OpenInt32

      val maxLen = fromInt maxLen
      fun tabulate (n, f) = V.tabulate (toInt n, f o fromInt)
      fun length (v: 'a vector) = fromInt (V.length v)
      fun sub (v, i) = V.sub (v, toInt i)
      fun convertSlice (v: 'a vector, i, io) = (v, toInt i, toIntOpt io)
      local
         fun make f g v = f (fn (i, e) => g (fromInt i, e)) v
      in
         val mapi = fn z => make mapi z
         val appi = fn z => make appi z
      end
      local
         fun make fold f a v =
            fold (fn (i, e, a) => f (fromInt i, e, a)) a v
      in
         val foldli = fn z => make foldli z
         val foldri = fn z => make foldri z
      end
   end

structure Vector =
   let
      structure V = Vector (open Pervasive.Vector
                            type 'a elem = 'a)
   in struct open Vector V end
   end

functor MonoVector (V: MONO_VECTOR) =
   struct
      structure V' = Vector (open V
                             type 'a vector = vector
                             type 'a elem = elem
                             (* These rebindings are because of an SML/NJ bug. *)
                             val appi = appi
                             val length = length
                             val mapi = mapi
                             val sub = sub
                             val tabulate = tabulate)
      open V V'
      local open V
      in type vector = vector
         type elem = elem
      end
   end

structure CharVector = MonoVector (CharVector)
structure RealVector = MonoVector (RealVector)
structure Word8Vector = MonoVector (Word8Vector)
