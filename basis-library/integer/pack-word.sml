(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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
                        val subArrRev: Word8.word array * C_Ptrdiff.t -> word
                        val subVec: Word8.word vector * SeqIndex.int -> word
                        val subVecRev: Word8.word vector * C_Ptrdiff.t -> word
                        val update: Word8.word array * SeqIndex.int * word -> unit
                        val updateRev: Word8.word array * C_Ptrdiff.t * word -> unit
                        val toLarge: word -> LargeWord.word
                        val toLargeX: word -> LargeWord.word
                        val fromLarge: LargeWord.word -> word
                     end): PACK_WORD =
struct

open S

val bytesPerElem = Int.div (wordSize, 8)

fun offsetForC (i, n) =
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

fun offsetForML (i, n) = 
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

val (subA, subV, updA, offset) =
   if isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
      then (subArr, subVec, update, offsetForML)
   else (subArrRev, subVecRev, updateRev, offsetForC)

fun update (a, i, w) =
   let
      val i = offset (i, Word8Array.length a)
      val a = Word8Array.toPoly a
   in
      updA (a, i, fromLarge w)
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
   PackWord (val wordSize = Word8.wordSize
             val isBigEndian = true
             open PrimitiveFFI.PackWord8
             open Primitive.PackWord8
             open Word8)
structure PackWord8Little: PACK_WORD =
   PackWord (val wordSize = Word8.wordSize
             val isBigEndian = false
             open PrimitiveFFI.PackWord8
             open Primitive.PackWord8
             open Word8)
structure PackWord8Host: PACK_WORD =
   PackWord (val wordSize = Word8.wordSize
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PrimitiveFFI.PackWord8
             open Primitive.PackWord8
             open Word8)
structure PackWord16Big: PACK_WORD =
   PackWord (val wordSize = Word16.wordSize
             val isBigEndian = true
             open PrimitiveFFI.PackWord16
             open Primitive.PackWord16
             open Word16)
structure PackWord16Little: PACK_WORD =
   PackWord (val wordSize = Word16.wordSize
             val isBigEndian = false
             open PrimitiveFFI.PackWord16
             open Primitive.PackWord16
             open Word16)
structure PackWord16Host: PACK_WORD =
   PackWord (val wordSize = Word16.wordSize
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PrimitiveFFI.PackWord16
             open Primitive.PackWord16
             open Word16)
structure PackWord32Big: PACK_WORD =
   PackWord (val wordSize = Word32.wordSize
             val isBigEndian = true
             open PrimitiveFFI.PackWord32
             open Primitive.PackWord32
             open Word32)
structure PackWord32Little: PACK_WORD =
   PackWord (val wordSize = Word32.wordSize
             val isBigEndian = false
             open PrimitiveFFI.PackWord32
             open Primitive.PackWord32
             open Word32)
structure PackWord32Host: PACK_WORD =
   PackWord (val wordSize = Word32.wordSize
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PrimitiveFFI.PackWord32
             open Primitive.PackWord32
             open Word32)
structure PackWord64Big: PACK_WORD =
   PackWord (val wordSize = Word64.wordSize
             val isBigEndian = true
             open PrimitiveFFI.PackWord64
             open Primitive.PackWord64
             open Word64)
structure PackWord64Little: PACK_WORD =
   PackWord (val wordSize = Word64.wordSize
             val isBigEndian = false
             open PrimitiveFFI.PackWord64
             open Primitive.PackWord64
             open Word64)
structure PackWord64Host: PACK_WORD =
   PackWord (val wordSize = Word64.wordSize
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PrimitiveFFI.PackWord64
             open Primitive.PackWord64
             open Word64)
local
   local
      structure S =
         Word_ChooseWordN
         (type 'a t = int
          val fWord8 = Word8.wordSize
          val fWord16 = Word16.wordSize
          val fWord32 = Word32.wordSize
          val fWord64 = Word64.wordSize)
   in
      val wordSize = S.f
   end
   structure PackWord =
      struct
         type word = Word.word
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subArr
                val fWord16 = PrimitiveFFI.PackWord16.subArr
                val fWord32 = PrimitiveFFI.PackWord32.subArr
                val fWord64 = PrimitiveFFI.PackWord64.subArr)
         in
            val subArr = S.f
         end
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subArrRev
                val fWord16 = PrimitiveFFI.PackWord16.subArrRev
                val fWord32 = PrimitiveFFI.PackWord32.subArrRev
                val fWord64 = PrimitiveFFI.PackWord64.subArrRev)
         in
            val subArrRev = S.f
         end
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subVec
                val fWord16 = PrimitiveFFI.PackWord16.subVec
                val fWord32 = PrimitiveFFI.PackWord32.subVec
                val fWord64 = PrimitiveFFI.PackWord64.subVec)
         in
            val subVec = S.f
         end
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subVecRev
                val fWord16 = PrimitiveFFI.PackWord16.subVecRev
                val fWord32 = PrimitiveFFI.PackWord32.subVecRev
                val fWord64 = PrimitiveFFI.PackWord64.subVecRev)
         in
            val subVecRev = S.f
         end
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fWord8 = PrimitiveFFI.PackWord8.update
                val fWord16 = PrimitiveFFI.PackWord16.update
                val fWord32 = PrimitiveFFI.PackWord32.update
                val fWord64 = PrimitiveFFI.PackWord64.update)
         in
            val update = S.f
         end
         local
            structure S =
               Word_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fWord8 = PrimitiveFFI.PackWord8.updateRev
                val fWord16 = PrimitiveFFI.PackWord16.updateRev
                val fWord32 = PrimitiveFFI.PackWord32.updateRev
                val fWord64 = PrimitiveFFI.PackWord64.updateRev)
         in
            val updateRev = S.f
         end
      end
in
structure PackWordBig: PACK_WORD =
   PackWord (val wordSize = Word.wordSize
             val isBigEndian = true
             open PackWord
             open Word)
structure PackWordLittle: PACK_WORD =
   PackWord (val wordSize = Word.wordSize
             val isBigEndian = false
             open PackWord
             open Word)
structure PackWordHost: PACK_WORD =
   PackWord (val wordSize = Word.wordSize
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PackWord
             open Word)
end
local
   local
      structure S =
         LargeWord_ChooseWordN
         (type 'a t = int
          val fWord8 = Word8.wordSize
          val fWord16 = Word16.wordSize
          val fWord32 = Word32.wordSize
          val fWord64 = Word64.wordSize)
   in
      val wordSize = S.f
   end
   structure PackLargeWord =
      struct
         type word = Word.word
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subArr
                val fWord16 = PrimitiveFFI.PackWord16.subArr
                val fWord32 = PrimitiveFFI.PackWord32.subArr
                val fWord64 = PrimitiveFFI.PackWord64.subArr)
         in
            val subArr = S.f
         end
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subArrRev
                val fWord16 = PrimitiveFFI.PackWord16.subArrRev
                val fWord32 = PrimitiveFFI.PackWord32.subArrRev
                val fWord64 = PrimitiveFFI.PackWord64.subArrRev)
         in
            val subArrRev = S.f
         end
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subVec
                val fWord16 = PrimitiveFFI.PackWord16.subVec
                val fWord32 = PrimitiveFFI.PackWord32.subVec
                val fWord64 = PrimitiveFFI.PackWord64.subVec)
         in
            val subVec = S.f
         end
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word vector * C_Ptrdiff.t -> 'a
                val fWord8 = PrimitiveFFI.PackWord8.subVecRev
                val fWord16 = PrimitiveFFI.PackWord16.subVecRev
                val fWord32 = PrimitiveFFI.PackWord32.subVecRev
                val fWord64 = PrimitiveFFI.PackWord64.subVecRev)
         in
            val subVecRev = S.f
         end
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fWord8 = PrimitiveFFI.PackWord8.update
                val fWord16 = PrimitiveFFI.PackWord16.update
                val fWord32 = PrimitiveFFI.PackWord32.update
                val fWord64 = PrimitiveFFI.PackWord64.update)
         in
            val update = S.f
         end
         local
            structure S =
               LargeWord_ChooseWordN
               (type 'a t = Word8.word array * C_Ptrdiff.t * 'a -> unit
                val fWord8 = PrimitiveFFI.PackWord8.updateRev
                val fWord16 = PrimitiveFFI.PackWord16.updateRev
                val fWord32 = PrimitiveFFI.PackWord32.updateRev
                val fWord64 = PrimitiveFFI.PackWord64.updateRev)
         in
            val updateRev = S.f
         end
      end
in
structure PackLargeWordBig: PACK_WORD =
   PackWord (val wordSize = LargeWord.wordSize
             val isBigEndian = true
             open PackLargeWord
             open LargeWord)
structure PackLargeWordLittle: PACK_WORD =
   PackWord (val wordSize = LargeWord.wordSize
             val isBigEndian = false
             open PackLargeWord
             open LargeWord)
structure PackLargeWordHost: PACK_WORD =
   PackWord (val wordSize = LargeWord.wordSize
             val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
             open PackLargeWord
             open LargeWord)
end
