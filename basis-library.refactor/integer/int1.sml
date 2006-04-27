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
      (* Lowbits or sign-extend. *)
      val fromInt8Unsafe: Primitive.Int8.int -> int
      val fromInt16Unsafe: Primitive.Int16.int -> int
      val fromInt32Unsafe: Primitive.Int32.int -> int
      val fromInt64Unsafe: Primitive.Int64.int -> int
      val fromIntInfUnsafe: Primitive.IntInf.int -> int
      (* Overflow checking, signed interp. *)
      val fromInt8: Primitive.Int8.int -> int
      val fromInt16: Primitive.Int16.int -> int
      val fromInt32: Primitive.Int32.int -> int
      val fromInt64: Primitive.Int64.int -> int
      val fromIntInf: Primitive.IntInf.int -> int
      (* Lowbits or sign-extend. *)
      val toInt8Unsafe: int -> Primitive.Int8.int
      val toInt16Unsafe: int -> Primitive.Int16.int
      val toInt32Unsafe: int -> Primitive.Int32.int
      val toInt64Unsafe: int -> Primitive.Int64.int
      val toIntInfUnsafe: int -> Primitive.IntInf.int
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

      val fromIntUnsafe: Int.int -> int
      val fromInt: Int.int -> int
      val fromLargeIntUnsafe: LargeInt.int -> int
      val fromLargeUnsafe: LargeInt.int -> int
      val fromLargeInt: LargeInt.int -> int
      val fromLarge: LargeInt.int -> int

      val toIntUnsafe: int -> Int.int
      val toInt: int -> Int.int
      val toLargeIntUnsafe: int -> LargeInt.int
      val toLargeUnsafe: int -> LargeInt.int
      val toLargeInt: int -> LargeInt.int
      val toLarge: int -> LargeInt.int
   end

functor IntFromTo(I: INT_FROM_TO_ARG): INT_FROM_TO_RES where type int = I.int =
   struct
      open I
         
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = I.fromInt8Unsafe
             val fInt16 = I.fromInt16Unsafe
             val fInt32 = I.fromInt32Unsafe
             val fInt64 = I.fromInt64Unsafe
             val fIntInf = I.fromIntInfUnsafe)
      in
         val fromIntUnsafe = S.f
      end
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
             val fInt8 = I.fromInt8Unsafe
             val fInt16 = I.fromInt16Unsafe
             val fInt32 = I.fromInt32Unsafe
             val fInt64 = I.fromInt64Unsafe
             val fIntInf = I.fromIntInfUnsafe)
      in
         val fromLargeIntUnsafe = S.f
         val fromLargeUnsafe = fromLargeIntUnsafe
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
         val fromLargeInt = S.f
         val fromLarge = fromLargeInt
      end

      local
         structure S =
            Int_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = I.toInt8Unsafe
             val fInt16 = I.toInt16Unsafe
             val fInt32 = I.toInt32Unsafe
             val fInt64 = I.toInt64Unsafe
             val fIntInf = I.toIntInfUnsafe)
      in
         val toIntUnsafe = S.f
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
             val fInt8 = I.toInt8Unsafe
             val fInt16 = I.toInt16Unsafe
             val fInt32 = I.toInt32Unsafe
             val fInt64 = I.toInt64Unsafe
             val fIntInf = I.toIntInfUnsafe)
      in
         val toLargeIntUnsafe = S.f
         val toLargeUnsafe = toLargeIntUnsafe
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
         val toLargeInt = S.f
         val toLarge = toLargeInt
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
