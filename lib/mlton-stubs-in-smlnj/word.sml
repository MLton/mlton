(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure LargeWord: WORD = Word32

signature WORD =
   sig
      eqtype word

      val * : word * word -> word 
      val + : word * word -> word 
      val - : word * word -> word 
      val < : word * word -> bool 
      val << : word * Word32.word -> word 
      val <= : word * word -> bool 
      val > : word * word -> bool 
      val >= : word * word -> bool 
      val >> : word * Word32.word -> word 
      val ~ : word -> word
      val ~>> : word * Word32.word -> word 
      val andb: word * word -> word 
      val compare: word * word -> order 
      val div: word * word -> word 
      val fmt: StringCvt.radix -> word -> string 
      val fromInt: Int32.int -> word 
      val fromLarge: LargeWord.word -> word 
      val fromLargeInt: IntInf.int -> word
      val fromLargeWord: LargeWord.word -> word 
      val fromString: string -> word option 
      val max: word * word -> word
      val min: word * word -> word 
      val mod: word * word -> word
      val notb: word -> word 
      val orb: word * word -> word 
      val scan:
         StringCvt.radix
         -> (char, 'a) StringCvt.reader
         -> (word, 'a) StringCvt.reader
      val toInt: word -> Int32.int 
      val toIntX: word -> Int32.int 
      val toLarge: word -> LargeWord.word
      val toLargeInt: word -> IntInf.int 
      val toLargeIntX: word -> IntInf.int 
      val toLargeWord: word -> LargeWord.word 
      val toLargeWordX: word -> LargeWord.word 
      val toLargeX: word -> LargeWord.word
      val toString: word -> string 
      val wordSize: Int32.int 
      val xorb: word * word -> word 
   end

functor FixWord (W: PERVASIVE_WORD): WORD =
   struct
      local
         open W
      in
         type word = word
         val op * = op *
         val op + = op +
         val op - = op -
         val op < = op <
         val op <= = op <=
         val op > = op >
         val op >= = op >=
         val ~ = ~
         val andb = andb
         val compare = compare
         val op div = op div
         val fromString = fromString
         val max = max
         val min = min
         val op mod = op mod
         val notb = notb
         val orb = orb
         val scan = scan
         val xorb = xorb
      end

      val wordSize = Pervasive.Int32.fromInt W.wordSize

      local
         fun fix (f: word * Word31.word -> word)
            (w: word, w': Word32.word): word =
            f (w, Word31.fromLargeWord w')
      in
         val << = fix W.<<
         val >> = fix W.>>
         val ~>> = fix W.~>>
      end
      val fromInt = W.fromLargeInt o Pervasive.Int32.toLarge
      val fromLarge = W.fromLargeWord o LargeWord.toLargeWord
      val fromLargeInt = W.fromLargeInt
      val fromLargeWord = fromLarge
      val toInt = Pervasive.Int32.fromLarge o W.toLargeInt
      val toIntX = Pervasive.Int32.fromLarge o W.toLargeIntX
      val toLarge = LargeWord.fromLargeWord o W.toLargeWord
      val toLargeInt = W.toLargeInt
      val toLargeIntX = W.toLargeIntX
      val toLargeWord = toLarge
      val toLargeWordX = LargeWord.fromLargeWord o W.toLargeWordX
      val toLargeX = toLargeWordX
      local
         (* Bug in SML/NJ -- they use lower instead of upper case. *)
         val toUpper = Pervasive.String.translate (Char.toString o Char.toUpper)
      in
         fun fmt r i = toUpper (W.fmt r i)
         val toString = toUpper o W.toString
      end

      fun ~ (w: word) = fromInt 0 - w
   end

structure Word8 = FixWord (Pervasive.Word8)
structure Word32 = FixWord (Pervasive.Word32)
structure Word16 = Word32
structure Word64 = FixWord (LargeWord)
structure Word = Word32
structure SysWord = Word32
structure LargeWord = Word64

(* Dummy implementation that will not be used at run-time. *)
structure PackWord32Little = struct
   val bytesPerElem = 0
   val isBigEndian = false
   fun subVec _ = raise Fail "PackWord32Little.subVec"
   fun subVecX _ = raise Fail "PackWord32Little.subVecX"
   fun subArr _ = raise Fail "PackWord32Little.subArr"
   fun subArrX _ = raise Fail "PackWord32Little.subArrX"
   fun update _ = raise Fail "PackWord32Little.update"
end

(* Dummy implementation that will not be used at run-time. *)
structure PackWord64Little = struct
   val bytesPerElem = 0
   val isBigEndian = false
   fun subVec _ = raise Fail "PackWord64Little.subVec"
   fun subVecX _ = raise Fail "PackWord64Little.subVecX"
   fun subArr _ = raise Fail "PackWord64Little.subArr"
   fun subArrX _ = raise Fail "PackWord64Little.subArrX"
   fun update _ = raise Fail "PackWord64Little.update"
end
