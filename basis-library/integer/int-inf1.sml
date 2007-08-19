(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INT_INF1 =
   sig
      include INT_INF0

      val fromInt: Int.int -> int
      val fromLarge: LargeInt.int -> int
      val toInt: int -> Int.int
      val toLarge: int -> LargeInt.int
   end

structure Primitive = struct

open Primitive

structure IntInf : INT_INF1 =
   struct
      structure I = Primitive.IntInf

      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = I.fromInt8
             val fInt16 = I.fromInt16
             val fInt32 = I.fromInt32
             val fInt64 = I.fromInt64
             val fIntInf = I.fromIntInf)
      in
         val fromInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = I.fromInt8
             val fInt16 = I.fromInt16
             val fInt32 = I.fromInt32
             val fInt64 = I.fromInt64
             val fIntInf = I.fromIntInf)
      in
         val fromLarge = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = I.toInt8
             val fInt16 = I.toInt16
             val fInt32 = I.toInt32
             val fInt64 = I.toInt64
             val fIntInf = I.toIntInf)
      in
         val toInt = S.f
      end
      local
         structure S =
            LargeInt_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = I.toInt8
             val fInt16 = I.toInt16
             val fInt32 = I.toInt32
             val fInt64 = I.toInt64
             val fIntInf = I.toIntInf)
      in
         val toLarge = S.f
      end

   end

end
