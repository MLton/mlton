(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Word: WORD32 =
   struct
      structure Int = Pervasive.Int
      type int = Int.int
      open Pervasive.Word MLton.Word

      type t = word
  
      val layout = Layout.str o toString

      val equals = op =

(*       val equals =
 * 	 Trace.trace2 ("Word.equals", layout, layout, Bool.layout) equals
 *)

(*       val toIntX =
 * 	 Trace.trace ("Word.toIntX", layout, Layout.str o Int.toString) toIntX
 *)
	 
      val fromChar = Word8.toWord o Byte.charToByte
      val toChar = Byte.byteToChar o Word8.fromWord

      fun nthBitIsSet (w: t, n: int): bool =
	 0w1 = andb (0w1, >> (w, fromInt n))
	 
      fun fromWord8s (f: int -> Word8.t): t =
	 let
	    fun w (i, shift) =
	       Pervasive.Word.<< (Word8.toWord (f i), shift)
	 in orb (orb (w (0, 0w0), w (1, 0w8)),
		 orb (w (2, 0w16), w (3, 0w24)))
	 end

      local
	 val wordSize = fromInt wordSize
      in
	 fun rotateLeft (w: t, n: t) =
	    let val l = n mod wordSize
	       val r = wordSize - l
	    in orb (<< (w, l), >> (w, r))
	    end
      end

      val fromWord = fn x => x
      val toWord = fn x => x
      val toWordX = fn x => x

      val fromWord8 = Word8.toWord
      val toWord8 = Word8.fromWord
   end

structure ZWord = Word
