(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Word8 =
   Word
   (structure P = Primitive.Word8
    open P
       
    val wordSize: int = 8
    val wordSizeWord : Primitive.Word32.word = 0w8

    val highBit: word = 0wx80
    val allOnes: word = 0wxFF
    val zero: word = 0w0

    val {compare, min, max} = Util.makeCompare(op <)

    fun << (i, n) 
      = if Primitive.Word32.>=(n ,wordSizeWord)
	  then zero
	  else P.<<(i, n)

    fun >> (i, n) 
      = if Primitive.Word32.>=(n, wordSizeWord)
	  then zero
	  else P.>>(i, n)

    fun ~>> (i, n) 
      = if Primitive.Word32.<(n, wordSizeWord)
	  then P.~>>(i, n)
	  else P.~>>(i, Primitive.Word32.-(wordSizeWord, 0w1))
       )


