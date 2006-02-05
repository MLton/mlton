(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PackWord32 (val isBigEndian: bool): PACK_WORD =
struct

val bytesPerElem: int = 4

val isBigEndian = isBigEndian

val (sub, up, subV) =
   if isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
      then (Primitive.Word8Array.subWord,
            Primitive.Word8Array.updateWord,
            Primitive.Word8Vector.subWord)
   else (Primitive.Word8Array.subWordRev,
         Primitive.Word8Array.updateWordRev,
         Primitive.Word8Vector.subWordRev)

fun start (i, n) = 
   let
      val i = Int.* (bytesPerElem, i)
      val _ =
         if Primitive.safe
            andalso Primitive.Int.geu (Int.+ (i, Int.- (bytesPerElem, 1)), n)
            then raise Subscript
         else ()
   in
      i
   end handle Overflow => raise Subscript

local
   fun make (sub, length, toPoly) (av, i) =
      let
         val _ = start (i, length av)
      in
         Word.toLarge (sub (toPoly av, i))
      end
in
   val subArr = make (sub, Word8Array.length, Word8Array.toPoly)
   val subArrX = subArr
   val subVec = make (subV, Word8Vector.length, Word8Vector.toPoly)
   val subVecX = subVec
end

fun update (a, i, w) =
   let
      val a = Word8Array.toPoly a
      val _ = start (i, Array.length a)
   in
      up (a, i, Word.fromLarge w)
   end

end

structure PackWord32Big = PackWord32 (val isBigEndian = true)
structure PackWord32Little = PackWord32 (val isBigEndian = false)
structure PackWord32Host = 
   PackWord32(val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian)
