(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Pack (val isBigEndian: bool): PACK_WORD =
   struct
      val bytesPerElem: int = 4
      val isBigEndian = isBigEndian

      fun revWord (w: word): word =
	 let
	    open Word
	 in
	    orb (orb (orb (andb (>> (w, 0w8), 0wxFF00),
			   andb(<< (w, 0w8), 0wxFF0000)),
		      >> (w, 0w24)),
		 << (w, 0w24))
	 end
      
      fun maybeRev w =
	 if isBigEndian = Primitive.MLton.Platform.isBigEndian
	    then w
	 else revWord w

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
	       maybeRev (sub (av, i))
	    end
      in
	 val subArr = make (Primitive.Word8Array.subWord, Word8Array.length)
	 val subArrX = subArr
	 val subVec = make (Primitive.Word8Vector.subWord, Word8Vector.length)
	 val subVecX = subVec
      end

      fun update (a, i, w) =
	 let
	    val _ = start (i, Array.length a)
	 in
	    Primitive.Word8Array.updateWord (a, i, maybeRev w)
	 end
   end

structure Pack32Big = Pack (val isBigEndian = true)
structure Pack32Little = Pack (val isBigEndian = false)
