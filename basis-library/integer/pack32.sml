(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

functor Pack32 (val isBigEndian: bool
		val shift3: Word.word
		val shift2: Word.word
		val shift1: Word.word
		val shift0: Word.word): PACK_WORD_EXTRA =
   struct
      structure LW = LargeWord
      structure W8 = Word8

      val bytesPerElem: int = 4
      val isBigEndian = isBigEndian

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
	 fun make (s, length) (av, i) =
	    let
	       val i = start (i, length av)
	       fun sub (i, shift: Word.word): LW.word =
		  LW.<< (W8.toLargeWord (s (av, i)), shift)
	    in LW.orb (LW.orb (LW.orb (sub (i, shift0),
				       sub (i +? 1, shift1)),
			       sub (i +? 2, shift2)),
		       sub (i +? 3, shift3))
	    end
      in
	 val subArr = make (Array.unsafeSub, Array.length)
	 val subArrX = subArr
	 val subVec = make (Vector.unsafeSub, Vector.length)
	 val subVecX = subVec
      end

      fun update (a, i, w) =
	 let
	    val i = start (i, Array.length a)
	    fun update (i, shift) =
	       Array.unsafeUpdate (a, i, W8.fromLargeWord (LW.>> (w, shift)))
	 in
	    update (i, shift0)
	    ; update (i +? 1, shift1)
	    ; update (i +? 2, shift2)
	    ; update (i +? 3, shift3)
	 end
   end

structure Pack32Big =
   Pack32 (val isBigEndian = true
	   val shift3: word = 0w0
	   val shift2: word = 0w8
	   val shift1: word = 0w16
	   val shift0: word = 0w24)

(* structure Pack32Little =
 *    Pack32 (val isBigEndian = false
 * 	   val shift0: word = 0w0
 * 	   val shift1: word = 0w8
 * 	   val shift2: word = 0w16
 * 	   val shift3: word = 0w24)
 *)

(* Depends on being on a little-endian machine. *)
structure Pack32Little: PACK_WORD =
   struct
      val start = Pack32Big.start
      val _ = if Primitive.isLittleEndian
		 then ()
	      else Primitive.halt 1
      val bytesPerElem: int = 4
      val isBigEndian = false

      local
	 fun make (sub, length) (av, i) =
	    let
	       val _ = start (i, length av)
	    in
	       sub (av, i)
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
	    Primitive.Word8Array.updateWord (a, i, w)
	 end
   end
