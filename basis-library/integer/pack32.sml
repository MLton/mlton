(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Pack (S: sig
		    val isBigEndian: bool
		 end): PACK_WORD =
struct

open S

val bytesPerElem: int = 4

val isBigEndian = isBigEndian

val (sub, up, subV) =
   if isBigEndian = Primitive.MLton.Platform.Arch.isBigEndian
      then (Primitive.Word8Array.subWord,
	    Primitive.Word8Array.updateWord,
	    Primitive.Word8Vector.subWord)
   else (Primitive.Word8Array.subWordRev,
	 Primitive.Word8Array.updateWordRev,
	 Primitive.Word8Vector.subWordRev)

fun start (i, n) = 
   let
      val i = bytesPerElem * i 
      val _ =
	 if Primitive.safe andalso Int.geu (i + (bytesPerElem - 1), n)
	    then raise Subscript
	 else ()
   in
      i
   end handle Overflow => raise Subscript

local
   fun make (sub, length) (av, i) =
      let
	 val _ = start (i, length av)
      in
	 sub (av, i)
      end
in
   val subArr = make (sub, Word8Array.length)
   val subArrX = subArr
   val subVec = make (subV, Word8Vector.length)
   val subVecX = subVec
end

fun update (a, i, w) =
   let
      val _ = start (i, Array.length a)
   in
      up (a, i, w)
   end

end

structure Pack32Big = Pack (val isBigEndian = true
			    open Primitive.Word8Array)
structure Pack32Little = Pack (val isBigEndian = false
			       open Primitive.Word8Array)
