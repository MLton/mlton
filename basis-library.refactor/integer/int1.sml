(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INT_FROM_TO_ARG =
   sig
      type int
      (* Overflow checking, signed interp. *)
      val fromInt8: Primitive.Int8.int -> int
      val fromInt16: Primitive.Int16.int -> int
      val fromInt32: Primitive.Int32.int -> int
      val fromInt64: Primitive.Int64.int -> int
      val fromIntInf: Primitive.IntInf.int -> int
      (* Overflow checking. *)
      val toInt8: int -> Primitive.Int8.int
      val toInt16: int -> Primitive.Int16.int
      val toInt32: int -> Primitive.Int32.int
      val toInt64: int -> Primitive.Int64.int
      val toIntInf: int -> Primitive.IntInf.int
   end

signature INT_FROM_TO_RES =
   sig
      type int
      val fromInt: Int.int -> int
      val fromLarge: LargeInt.int -> int
      val toInt: int -> Int.int
      val toLarge: int -> LargeInt.int
   end

functor IntFromTo(I: INT_FROM_TO_ARG): INT_FROM_TO_RES where type int = I.int =
   struct
      open I
         
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

structure Primitive = struct
open Primitive

structure Int8 = struct 
                    open Int8
                    local 
                       structure S = IntFromTo (Primitive.Int8)
                    in
                       open S
                    end
                 end
structure Int16 = struct 
                     open Int16
                     local 
                        structure S = IntFromTo (Primitive.Int16)
                     in
                        open S
                     end
                  end
structure Int32 = struct 
                     open Int32
                     local 
                        structure S = IntFromTo (Primitive.Int32)
                     in
                        open S
                     end
                  end
structure Int64 = struct 
                     open Int64
                     local 
                        structure S = IntFromTo (Primitive.Int64)
                     in
                        open S
                     end
                  end
structure IntInf = struct 
                      open IntInf
                      local 
                         structure S = IntFromTo (Primitive.IntInf)
                      in
                         open S
                      end
                   end
end
