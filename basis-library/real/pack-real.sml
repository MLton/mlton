(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PackRealArg (S: sig
                           type real
                           type word
                           val subArr: Word8.word array * SeqIndex.int -> word
                           val subVec: Word8.word vector * SeqIndex.int -> word
                           val update: Word8.word array * SeqIndex.int * word -> unit
                           val bswap: word -> word
                           val castFromWord: word -> real
                           val castToWord: real -> word
                        end) =
struct

open S

val subArrRev = castFromWord o bswap o subArr
val subVecRev = castFromWord o bswap o subVec
fun updateRev (a, i, r) = update (a, i, bswap (castToWord r))

val subArr = castFromWord o subArr
val subVec = castFromWord o subVec
val update = fn (a, i, r) => update (a, i, castToWord r)

end

structure PackReal32Arg =
   PackRealArg (open Primitive.PackReal32
                open Primitive.PackWord32
                val bswap = Word32.bswap)
structure PackReal64Arg =
   PackRealArg (open Primitive.PackReal64
                open Primitive.PackWord64
                val bswap = Word64.bswap)
structure PackRealArg =
   struct
      type real = Real.real
      local
         structure S =
            Real_ChooseRealN
            (type 'a t = int
             val fReal32 = Real32.realSize
             val fReal64 = Real64.realSize)
      in
         val realSize = S.f
      end
      local
         structure S =
            Real_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subArr
             val fReal64 = PackReal64Arg.subArr)
      in
         val subArr = S.f
      end
      local
         structure S =
            Real_ChooseRealN
            (type 'a t = Word8.word vector * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subVec
             val fReal64 = PackReal64Arg.subVec)
      in
         val subVec = S.f
      end
      local
         structure S =
            Real_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int * 'a -> unit
             val fReal32 = PackReal32Arg.update
             val fReal64 = PackReal64Arg.update)
      in
         val update = S.f
      end
      local
         structure S =
            Real_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subArrRev
             val fReal64 = PackReal64Arg.subArrRev)
      in
         val subArrRev = S.f
      end
      local
         structure S =
            Real_ChooseRealN
            (type 'a t = Word8.word vector * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subVecRev
             val fReal64 = PackReal64Arg.subVecRev)
      in
         val subVecRev = S.f
      end
      local
         structure S =
            Real_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int * 'a -> unit
             val fReal32 = PackReal32Arg.updateRev
             val fReal64 = PackReal64Arg.updateRev)
      in
         val updateRev = S.f
      end
   end
structure PackLargeRealArg =
   struct
      type real = LargeReal.real
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = int
             val fReal32 = Real32.realSize
             val fReal64 = Real64.realSize)
      in
         val realSize = S.f
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subArr
             val fReal64 = PackReal64Arg.subArr)
      in
         val subArr = S.f
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = Word8.word vector * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subVec
             val fReal64 = PackReal64Arg.subVec)
      in
         val subVec = S.f
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int * 'a -> unit
             val fReal32 = PackReal32Arg.update
             val fReal64 = PackReal64Arg.update)
      in
         val update = S.f
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subArrRev
             val fReal64 = PackReal64Arg.subArrRev)
      in
         val subArrRev = S.f
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = Word8.word vector * SeqIndex.int -> 'a
             val fReal32 = PackReal32Arg.subVecRev
             val fReal64 = PackReal64Arg.subVecRev)
      in
         val subVecRev = S.f
      end
      local
         structure S =
            LargeReal_ChooseRealN
            (type 'a t = Word8.word array * SeqIndex.int * 'a -> unit
             val fReal32 = PackReal32Arg.updateRev
             val fReal64 = PackReal64Arg.updateRev)
      in
         val updateRev = S.f
      end
   end

functor PackReal (S: sig
                        type real
                        val realSize: int
                        val isBigEndian: bool
                        val subArr: Word8.word array * SeqIndex.int -> real
                        val subVec: Word8.word vector * SeqIndex.int -> real
                        val update: Word8.word array * SeqIndex.int * real -> unit
                        val subArrRev: Word8.word array * SeqIndex.int -> real
                        val subVecRev: Word8.word vector * SeqIndex.int -> real
                        val updateRev: Word8.word array * SeqIndex.int * real -> unit
                     end): PACK_REAL =
struct

open S

val bytesPerElem = Int.div (realSize, 8)

fun offset (i, n) = 
   let
      val i' = Int.* (bytesPerElem, i)
      val () =
         if Primitive.Controls.safe
            andalso (Int.geu (Int.+ (i', Int.- (bytesPerElem, 1)), n))
            then raise Subscript
            else ()
   in
      SeqIndex.fromInt i
   end
   handle Overflow => raise Subscript


val (subA, subV, updA) =
   if isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
      then (subArr, subVec, update)
   else (subArrRev, subVecRev, updateRev)

fun update (a, i, r) =
   let
      val i = offset (i, Word8Array.length a)
      val a = Word8Array.toPoly a
   in
      updA (a, i, r)
   end

local
   fun make (sub, length, toPoly) (av, i) =
      let
         val i = offset (i, length av)
      in
         sub (toPoly av, i)
      end
in
   val subArr = make (subA, Word8Array.length, Word8Array.toPoly)
   val subVec = make (subV, Word8Vector.length, Word8Vector.toPoly)
end

fun toBytes (r: real): Word8Vector.vector =
   let
      val a = Array.arrayUninit bytesPerElem
   in
      (updA (a, 0, r)
       ; Word8Vector.fromPoly (Array.vector a))
   end

fun fromBytes v = subVec (v, 0)

end

structure PackReal32Big: PACK_REAL =
   PackReal (open Real32
             open PackReal32Arg
             val isBigEndian = true)
structure PackReal32Little: PACK_REAL =
   PackReal (open Real32
             open PackReal32Arg
             val isBigEndian = false)
structure PackReal32Host: PACK_REAL =
   PackReal (open Real32
             open PackReal32Arg
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian)
structure PackReal64Big: PACK_REAL =
   PackReal (open Real64
             open PackReal64Arg
             val isBigEndian = true)
structure PackReal64Little: PACK_REAL =
   PackReal (open Real64
             open PackReal64Arg
             val isBigEndian = false)
structure PackReal64Host: PACK_REAL =
   PackReal (open Real64
             open PackReal64Arg
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian)
structure PackRealBig: PACK_REAL =
   PackReal (open Real
             open PackRealArg
             val isBigEndian = true)
structure PackRealLittle: PACK_REAL =
   PackReal (open Real
             open PackRealArg
             val isBigEndian = false)
structure PackRealHost: PACK_REAL =
   PackReal (open Real
             open PackRealArg
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian)
structure PackLargeRealBig: PACK_REAL =
   PackReal (open LargeReal
             open PackLargeRealArg
             val isBigEndian = true)
structure PackLargeRealLittle: PACK_REAL =
   PackReal (open LargeReal
             open PackLargeRealArg
             val isBigEndian = false)
structure PackLargeRealHost: PACK_REAL =
   PackReal (open LargeReal
             open PackLargeRealArg
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian)
