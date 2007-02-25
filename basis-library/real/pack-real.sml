(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PackReal (S: sig
                        type real
                        val realSize: int
                        val isBigEndian: bool
                        val subArr: Word8.word array * C_Ptrdiff.t -> real
                        val subArrRev: Word8.word array * C_Ptrdiff.t -> real
                        val subVec: Word8.word vector * C_Ptrdiff.t -> real
                        val subVecRev: Word8.word vector * C_Ptrdiff.t -> real
                        val update: Word8.word array * C_Ptrdiff.t * real -> unit
                        val updateRev: Word8.word array * C_Ptrdiff.t * real -> unit
                     end): PACK_REAL =
struct

open S

val bytesPerElem = Int.div (realSize, 8)

val (subA, subV, updA) =
   if isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
      then (subArr, subVec, update)
   else (subArrRev, subVecRev, updateRev)

fun offset (i, n) =
   let
      val i = Int.* (bytesPerElem, i)
      val () =
         if Primitive.Controls.safe
            andalso (Int.geu (Int.+ (i, Int.- (bytesPerElem, 1)), n))
            then raise Subscript
            else ()
   in
      C_Ptrdiff.fromInt i
   end
   handle Overflow => raise Subscript

fun update (a, i, r) =
   let
      val i = offset (i, Word8Array.length a)
      val a = Word8Array.toPoly a
   in
      updA (a, i, r)
   end

fun toBytes (r: real): Word8Vector.vector =
   let
      val a = Array.arrayUninit bytesPerElem
   in
      (updA (a, 0, r)
       ; Word8Vector.fromPoly (Array.vector a))
   end

local
   fun make (sub, length, toPoly) (s, i) =
      let
         val i = offset (i, length s)
         val s = toPoly s
      in
         sub (s, i)
      end
in
   val subArr = make (subA, Word8Array.length, Word8Array.toPoly)
   val subVec = make (subV, Word8Vector.length, Word8Vector.toPoly)
end

fun fromBytes v = subVec (v, 0)

end

structure PackReal32Big: PACK_REAL =
   PackReal (val realSize = Real32.realSize
             val isBigEndian = true
             open Primitive.PackReal32)
structure PackReal32Little: PACK_REAL =
   PackReal (val realSize = Real32.realSize
             val isBigEndian = false
             open Primitive.PackReal32)
structure PackReal64Big: PACK_REAL =
   PackReal (val realSize = Real64.realSize
             val isBigEndian = true
             open Primitive.PackReal64)
structure PackReal64Little: PACK_REAL =
   PackReal (val realSize = Real64.realSize
             val isBigEndian = false
             open Primitive.PackReal64)
local
   local
      structure S =
         Real_ChooseRealN
         (type 'a t = int
          val fReal32 = Real32.realSize
          val fReal64 = Real64.realSize)
   in
      val realSize = S.f
   end
   structure PackReal =
      struct
         type real = Real.real
         local
            structure S =
               Real_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subArr
                val fReal64 = Primitive.PackReal64.subArr)
         in
            val subArr = S.f
         end
         local
            structure S =
               Real_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subArrRev
                val fReal64 = Primitive.PackReal64.subArrRev)
         in
            val subArrRev = S.f
         end
         local
            structure S =
               Real_ChooseRealN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subVec
                val fReal64 = Primitive.PackReal64.subVec)
         in
            val subVec = S.f
         end
         local
            structure S =
               Real_ChooseRealN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subVecRev
                val fReal64 = Primitive.PackReal64.subVecRev)
         in
            val subVecRev = S.f
         end
         local
            structure S =
               Real_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fReal32 = Primitive.PackReal32.update
                val fReal64 = Primitive.PackReal64.update)
         in
            val update = S.f
         end
         local
            structure S =
               Real_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fReal32 = Primitive.PackReal32.updateRev
                val fReal64 = Primitive.PackReal64.updateRev)
         in
            val updateRev = S.f
         end

      end
in
structure PackRealBig: PACK_REAL =
   PackReal (val realSize = realSize
             val isBigEndian = true
             open PackReal)
structure PackRealLittle: PACK_REAL =
   PackReal (val realSize = realSize
             val isBigEndian = false
             open PackReal)
end
local
   local
      structure S =
         LargeReal_ChooseRealN
         (type 'a t = int
          val fReal32 = Real32.realSize
          val fReal64 = Real64.realSize)
   in
      val realSize = S.f
   end

   structure PackLargeReal =
      struct
         type real = LargeReal.real
         local
            structure S =
               LargeReal_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subArr
                val fReal64 = Primitive.PackReal64.subArr)
         in
            val subArr = S.f
         end
         local
            structure S =
               LargeReal_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subArrRev
                val fReal64 = Primitive.PackReal64.subArrRev)
         in
            val subArrRev = S.f
         end
         local
            structure S =
               LargeReal_ChooseRealN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subVec
                val fReal64 = Primitive.PackReal64.subVec)
         in
            val subVec = S.f
         end
         local
            structure S =
               LargeReal_ChooseRealN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fReal32 = Primitive.PackReal32.subVecRev
                val fReal64 = Primitive.PackReal64.subVecRev)
         in
            val subVecRev = S.f
         end
         local
            structure S =
               LargeReal_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fReal32 = Primitive.PackReal32.update
                val fReal64 = Primitive.PackReal64.update)
         in
            val update = S.f
         end
         local
            structure S =
               LargeReal_ChooseRealN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fReal32 = Primitive.PackReal32.updateRev
                val fReal64 = Primitive.PackReal64.updateRev)
         in
            val updateRev = S.f
         end

      end
in
structure PackLargeRealBig: PACK_REAL =
   PackReal (val realSize = realSize
             val isBigEndian = true
             open PackLargeReal)
structure PackLargeRealLittle: PACK_REAL =
   PackReal (val realSize = realSize
             val isBigEndian = false
             open PackLargeReal)
end
