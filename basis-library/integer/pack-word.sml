(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PackWord (S: sig
                        type word
                        val wordSize: int
                        val isBigEndian: bool
                        val subArr: Word8.word array * SeqIndex.int -> word
                        val subVec: Word8.word vector * SeqIndex.int -> word
                        val update: Word8.word array * SeqIndex.int * word -> unit
                        val bswap: word -> word
                        val toLarge: word -> LargeWord.word
                        val toLargeX: word -> LargeWord.word
                        val fromLarge: LargeWord.word -> word
                     end): PACK_WORD =
struct

open S

val bytesPerElem = Int.div (wordSize, 8)

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

val subArrRev = bswap o subArr
val subVecRev = bswap o subVec
fun updateRev (a, i, w) = update (a, i, bswap w)

val (subA, subV, updA) =
   if isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
      then (subArr, subVec, update)
   else (subArrRev, subVecRev, updateRev)

fun update (a, i, w) =
   let
      val i = offset (i, Word8Array.length a)
      val a = Word8Array.toPoly a
   in
      updA (a, i, fromLarge w)
   end

local
   fun make (sub, length, toPoly) (av, i) =
      let
         val i = offset (i, length av)
      in
         sub (toPoly av, i)
      end
in
   val subArr = toLarge o (make (subA, Word8Array.length, Word8Array.toPoly))
   val subArrX = toLargeX o (make (subA, Word8Array.length, Word8Array.toPoly))
   val subVec = toLarge o (make (subV, Word8Vector.length, Word8Vector.toPoly))
   val subVecX = toLargeX o (make (subV, Word8Vector.length, Word8Vector.toPoly))
end

end

structure PackWord8Big: PACK_WORD =
   PackWord (val isBigEndian = true
             open Primitive.PackWord8
             open Word8)
structure PackWord8Little: PACK_WORD =
   PackWord (val isBigEndian = false
             open Primitive.PackWord8
             open Word8)
structure PackWord8Host: PACK_WORD =
   PackWord (val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open Primitive.PackWord8
             open Word8)
structure PackWord16Big: PACK_WORD =
   PackWord (val isBigEndian = true
             open Primitive.PackWord16
             open Word16)
structure PackWord16Little: PACK_WORD =
   PackWord (val isBigEndian = false
             open Primitive.PackWord16
             open Word16)
structure PackWord16Host: PACK_WORD =
   PackWord (val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open Primitive.PackWord16
             open Word16)
structure PackWord32Big: PACK_WORD =
   PackWord (val isBigEndian = true
             open Primitive.PackWord32
             open Word32)
structure PackWord32Little: PACK_WORD =
   PackWord (val isBigEndian = false
             open Primitive.PackWord32
             open Word32)
structure PackWord32Host: PACK_WORD =
   PackWord (val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open Primitive.PackWord32
             open Word32)
structure PackWord64Big: PACK_WORD =
   PackWord (val isBigEndian = true
             open Primitive.PackWord64
             open Word64)
structure PackWord64Little: PACK_WORD =
   PackWord (val isBigEndian = false
             open Primitive.PackWord64
             open Word64)
structure PackWord64Host: PACK_WORD =
   PackWord (val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open Primitive.PackWord64
             open Word64)
local
   structure PackWord =
      struct
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word array * SeqIndex.t -> 'a
                val fWord8 = Primitive.PackWord8.subArr
                val fWord16 = Primitive.PackWord16.subArr
                val fWord32 = Primitive.PackWord32.subArr
                val fWord64 = Primitive.PackWord64.subArr)
         in
            val subArr = S.f
         end
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word vector * SeqIndex.t -> 'a
                val fWord8 = Primitive.PackWord8.subVec
                val fWord16 = Primitive.PackWord16.subVec
                val fWord32 = Primitive.PackWord32.subVec
                val fWord64 = Primitive.PackWord64.subVec)
         in
            val subVec = S.f
         end
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word array * SeqIndex.t * 'a -> unit
                val fWord8 = Primitive.PackWord8.update
                val fWord16 = Primitive.PackWord16.update
                val fWord32 = Primitive.PackWord32.update
                val fWord64 = Primitive.PackWord64.update)
         in
            val update = S.f
         end
      end
in
structure PackWordBig: PACK_WORD =
   PackWord (val isBigEndian = true
             open PackWord
             open Word)
structure PackWordLittle: PACK_WORD =
   PackWord (val isBigEndian = false
             open PackWord
             open Word)
structure PackWordHost: PACK_WORD =
   PackWord (val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PackWord
             open Word)
end
local
   structure PackLargeWord =
      struct
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word array * SeqIndex.t -> 'a
                val fWord8 = Primitive.PackWord8.subArr
                val fWord16 = Primitive.PackWord16.subArr
                val fWord32 = Primitive.PackWord32.subArr
                val fWord64 = Primitive.PackWord64.subArr)
         in
            val subArr = S.f
         end
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word vector * SeqIndex.t -> 'a
                val fWord8 = Primitive.PackWord8.subVec
                val fWord16 = Primitive.PackWord16.subVec
                val fWord32 = Primitive.PackWord32.subVec
                val fWord64 = Primitive.PackWord64.subVec)
         in
            val subVec = S.f
         end
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word array * SeqIndex.t * 'a -> unit
                val fWord8 = Primitive.PackWord8.update
                val fWord16 = Primitive.PackWord16.update
                val fWord32 = Primitive.PackWord32.update
                val fWord64 = Primitive.PackWord64.update)
         in
            val update = S.f
         end
      end
in
structure PackLargeWordBig: PACK_WORD =
   PackWord (val isBigEndian = true
             open PackLargeWord
             open LargeWord)
structure PackLargeWordLittle: PACK_WORD =
   PackWord (val isBigEndian = false
             open PackLargeWord
             open LargeWord)
structure PackLargeWordHost: PACK_WORD =
   PackWord (val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PackLargeWord
             open LargeWord)
end
