(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Word32: WORD_EXTRA =
   Word
   (structure P = Primitive.Word32
    open P
       
    val wordSize: int = 32
    val wordSizeWord: word = 0w32
    val zero: word = 0w0

    local
       fun id x = x
    in
       val toLargeWord = id
       val toLargeWordX = id
       val fromLargeWord = id
    end

    fun highBitSet w = Int.<(toIntX w, 0)

    (* This assumes that Words and Ints have the same number of bits.
     * toInt w is supposed to treat w as unsigned.  Thus, if the high bit is
     * set in w, it will be unrepresentable as a twos-complement integer with
     * the same number of bits.
     *)
    fun toInt w =
       if Primitive.safe andalso highBitSet w
	  then raise Overflow
       else toIntX w

    val {compare, min, max} = Util.makeCompare(op <)

    fun << (i, n) 
      = if n >= wordSizeWord
	  then zero
	  else P.<<(i, n)

    fun >> (i, n) 
      = if n >= wordSizeWord
	  then zero
	  else P.>>(i, n)

    fun ~>> (i, n) 
      = if n < wordSizeWord
	  then P.~>>(i, n)
	  else P.~>>(i, wordSizeWord - 0w1)
       )

structure WordGlobal: WORD_GLOBAL = Word
open WordGlobal


