(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(*
 * IntInf.int's either have a bottom bit of 1, in which case the top 31
 * bits are the signed integer, or else the bottom bit is 0, in which case
 * they point to an vector of Word.word's.  The first word is either 0,
 * indicating that the number is positive, or 1, indicating that it is
 * negative.  The rest of the vector contains the `limbs' (big digits) of
 * the absolute value of the number, from least to most significant.
 *)
structure IntInf: INT_INF_EXTRA =
   struct
      structure Word = Word32
	 
      datatype rep =
	 Big of Word.word Vector.vector
       | Small of Int.int
	 
      structure Prim = Primitive.IntInf
      type bigInt = Prim.int
      local
	 open Int
      in
	 val op < = op <
	 val op <= = op <=
	 val op > = op >
	 val op >= = op >=
	 val op + = op +
	 val op - = op -
      end
      type smallInt = int
	 
      (* bigIntConstant is just to make it easy to spot where the bigInt
       * constants are in this module.
       *)
      fun bigIntConstant x = x
      val zero = bigIntConstant 0
      val one = bigIntConstant 1
      val negOne = bigIntConstant ~1
	 
      (* Check if an IntInf.int is small (i.e., a fixnum). *)
      fun isSmall (i: bigInt): bool =
	 0w0 <> Word.andb (Prim.toWord i, 0w1)

      (* Check if two IntInf.int's are both small (i.e., fixnums).
       * This is a gross hack, but uses only one test.
       *)
      fun areSmall (i: bigInt, i': bigInt) =
	 0w0 <> Word.andb (Prim.toWord i, Word.andb (Prim.toWord i', 0w1))
	 
      (*
       * Return the number of `limbs' in a bigInt.
       * If arg is big, then |arg| is in [ 2^ (32 (x-1)), 2^ (32 x) )
       * where x is size arg.  If arg is small, then it is in
       * [ - 2^30, 2^30 ).
       *)
      fun bigSize (arg: bigInt): smallInt =
	 Vector.length (Prim.toVector arg) -? 1
      fun size (arg: bigInt): smallInt =
	 if isSmall arg
	    then 1
	 else bigSize arg

      val bytesPerWord = 0w4
      (*
       * Reserve heap space for a bignum bigInt with room for size + extra
       * `limbs'.  The reason for splitting this up is that extra is intended
       * to be a constant, and so can be combined at compile time with the 0w4
       * below.
       *)
      fun reserve (size: smallInt, extra: smallInt): word = 
	 Word.* (bytesPerWord,
		 Word.+ (Word.fromInt size,
			 Word.+ (0w4, (* counter, size, header, sign words *)
				 Word.fromInt extra)))

      (*
       * Given a fixnum bigInt, return the Word.word which it
       * represents.
       * NOTE: it is an ERROR to call stripTag on an argument
       * which is a bignum bigInt.
       *)
      fun stripTag (arg: bigInt): Word.word =
	 Word.~>> (Prim.toWord arg, 0w1)

      (*
       * Given a Word.word, add the tag bit in so that it looks like
       * a fixnum bigInt.
       *)
      fun addTag (argw: Word.word): Word.word =
	 Word.orb (Word.<< (argw, 0w1), 0w1)

      (*
       * Given a fixnum bigInt, change the tag bit to 0.
       * NOTE: it is an ERROR to call zeroTag on an argument
       * which is a bignum bigInt.
       *)
      fun zeroTag (arg: bigInt): Word.word =
	 Word.andb (Prim.toWord arg, 0wxFFFFFFFE)

      (*
       * Given a Word.word, set the tag bit back to 1.
       *)
      fun incTag (argw: Word.word): Word.word =
	 Word.orb (argw, 0w1)

      (*
       * badw is the fixnum bigInt (as a word) whose negation and
       * absolute value are not fixnums.  badv is the same thing
       * with the tag stripped off.
       * negBad is the negation (and absolute value) of that bigInt.
       *)
      val badw: Word.word = 0wx80000001 (* = Prim.toWord ~0x40000000 *)
      val badv: Word.word = 0wxC0000000 (* = stripTag ~0x40000000 *)
      val negBad: bigInt = bigIntConstant 0x40000000

      (*
       * Given two Word.word's, check if they have the same `sign' bit.
       *)
      fun sameSign (lhs: Word.word, rhs: Word.word): bool =
	 Word.toIntX (Word.xorb (lhs, rhs)) >= 0

      (*
       * Given a bignum bigint, test if it is (strictly) negative.
       * Note: it is an ERROR to call bigIsNeg on an argument
       * which is a fixnum bigInt.
       *)
      fun bigIsNeg (arg: bigInt): bool =
	 Primitive.Vector.sub (Prim.toVector arg, 0) <> 0w0

      (*
       * Convert a smallInt to a bigInt.
       *)
      fun bigFromInt (arg: smallInt): bigInt =
	 let
	    val argv = Word.fromInt arg
	    val ans = addTag argv
	 in
	    if sameSign (argv, ans)
	       then Prim.fromWord ans
	    else let val space = Primitive.Array.array 2
		     val (isneg, abs) = if arg < 0
					   then (0w1, Word.- (0w0, argv))
					else (0w0, argv)
		     val _ = Primitive.Array.update (space, 0, isneg)
		     val _ = Primitive.Array.update (space, 1, abs)
		     val space = Primitive.Vector.fromArray space
		 in 
		    Prim.fromVector space
		 end
	 end

      fun rep x =
	 if isSmall x
	    then Small (Word.toIntX (stripTag x))
	 else Big (Prim.toVector x)
	    
      (*
       * Convert a bigInt to a smallInt, raising overflow if it
       * is too big.
       *)
      fun bigToInt (arg: bigInt): smallInt =
	 if isSmall arg
	    then Word.toIntX (stripTag arg)
	 else if bigSize arg <> 1
		 then raise Overflow
	      else let val arga = Prim.toVector arg
		       val argw = Primitive.Vector.sub (arga, 1)
		   in if Primitive.Vector.sub (arga, 0) <> 0w0
			 then if Word.<= (argw, 0wx80000000)
				 then Word.toIntX (Word.- (0w0, argw))
			      else raise Overflow
		      else if Word.< (argw, 0wx80000000)
			      then Word.toIntX argw
			   else raise Overflow
		   end

      fun bigFromInt64 (i: Int64.int): bigInt =
	 if Int64.<= (~0x40000000, i) andalso Int64.<= (i, 0x3FFFFFFF)
	    then Prim.fromWord (addTag (Word.fromInt (Int64.toInt i)))
	 else
	    let
	       fun doit (i: Int64.int, isNeg): bigInt =
		  if Int64.<= (i, 0xFFFFFFFF)
		     then
			let
			   val a = Primitive.Array.array 2
			   val _ = Array.update (a, 0, isNeg)
			   val _ = Array.update (a, 1, Int64.toWord i)
			in
			   Prim.fromVector (Vector.fromArray a)
			end
		  else
		     let
			val a = Primitive.Array.array 3
			val _ = Array.update (a, 0, isNeg)
			val r = Int64.rem (i, 0x100000000)
			val _ = Array.update (a, 1, Int64.toWord r)
			val q = Int64.quot (i, 0x100000000)
			val _ = Array.update (a, 2, Int64.toWord q)
		     in
			Prim.fromVector (Vector.fromArray a)
		     end
	    in
	       if Int64.>= (i, 0)
		  then doit (i, 0w0)
	       else
		  if i = valOf Int64.minInt
		     then ~0x8000000000000000
		  else doit (Int64.~? i, 0w1)
	    end
		
      fun bigToInt64 (arg: bigInt): Int64.int =
	 case rep arg of
	    Small i => Int64.fromInt i
	  | Big v => 
	       if Vector.length v > 3
		 then raise Overflow
	      else let
		      val sign = Primitive.Vector.sub (v, 0)
		      val w1 = Primitive.Vector.sub (v, 1)
		      val w2 = Primitive.Vector.sub (v, 2)
		   in
		      if Word.> (w2, 0wx80000000)
			 then raise Overflow
		      else if w2 = 0wx80000000
			      then if w1 = 0w0 andalso sign = 0w1
				      then valOf Int64.minInt
				   else raise Overflow				      
			   else
			      let
				 val n =
				    Int64.+?
				    (Primitive.Int64.fromWord w1,
				     Int64.*? (Primitive.Int64.fromWord w2,
					       0x100000000))
			      in
				 if sign = 0w1
				    then Int64.~ n
				 else n
			      end
		   end
			 
      (*
       * bigInt negation.
       *)
      fun bigNegate (arg: bigInt): bigInt =
	 if isSmall arg
	    then let val argw = Prim.toWord arg
		 in if argw = badw
		       then negBad
		    else Prim.fromWord (Word.- (0w2, argw))
		 end
	 else Prim.~ (arg, reserve (bigSize arg, 1))

      val dontInline: (unit -> 'a) -> 'a =
	 fn f =>
	 let
	    val rec recur: int -> 'a =
	       fn i =>
	       if i = 0
		  then f ()
	       else (ignore (recur (i - 1))
		     ; recur (i - 2))
	 in
	    recur 0
	 end
	    
      (*
       * bigInt multiplication.
       *)
      local 
	 val carry: Word.word ref = ref 0w0
      in
	 fun bigMul (lhs: bigInt, rhs: bigInt): bigInt =
	    let
	       val res =
		  if areSmall (lhs, rhs)
		     then let
			     val lhsv = stripTag lhs
			     val rhs0 = zeroTag rhs
			     val ans0 = Prim.smallMul (lhsv, rhs0, carry)
			  in
			     if (! carry) = Word.~>> (ans0, 0w31)
				then SOME (Prim.fromWord (incTag ans0))
			     else NONE
			  end
		  else NONE
	    in
	       case res of
		  NONE =>
		     dontInline
		     (fn () =>
		      Prim.* (lhs, rhs, reserve (size lhs +? size rhs, 0)))
		| SOME i => i
	    end
      end

      (*
       * bigInt quot.
       * Round towards 0 (bigRem returns the remainder).
       * Note, if size num < size den, then the answer is 0.
       * The only non-trivial case here is num being - den,
       * and small, but in that case, although den may be big, its
       * size is still 1.  (den cannot be 0 in this case.)
       * The space required for the shifted numerator limbs is <= nsize + 1.
       * The space required for the shifted denominator limbs is <= dsize
       * The space required for the quotient limbs is <= 1 + nsize - dsize.
       * Thus the total space for limbs is <= 2*nsize + 2 (and one extra
       * word for the isNeg flag).
       *)
      fun bigQuot (num: bigInt, den: bigInt): bigInt =
	 if areSmall (num, den)
	    then let val numv = stripTag num
		     val denv = stripTag den
		 in if numv = badv andalso denv = Word.fromInt ~1
		       then negBad
		    else let val numi = Word.toIntX numv
			     val deni = Word.toIntX denv
			     val ansi = Int.quot (numi, deni)
			     val answ = Word.fromInt ansi
			 in Prim.fromWord (addTag answ)
			 end
		 end
	 else let val nsize = size num
		  val dsize = size den
	      in if nsize < dsize
		    then zero
		 else if den = zero
			 then raise Div
		      else
			 Prim.quot
			 (num, den,
			  Word.* (Word.* (0w2, bytesPerWord),
				  Word.+ (Word.fromInt nsize, 0w3)))
	      end

      (*
       * bigInt rem.
       * Sign taken from numerator, quotient is returned by bigQuot.
       * Note, if size num < size den, then the answer is 0.
       * The only non-trivial case here is num being - den,
       * and small, but in that case, although den may be big, its
       * size is still 1.  (den cannot be 0 in this case.)
       * The space required for the shifted numerator limbs is <= nsize + 1.
       * The space required for the shifted denominator limbs is <= dsize
       * The space required for the quotient limbs is <= 1 + nsize - dsize.
       * Thus the total space for limbs is <= 2*nsize + 2 (and one extra
       * word for the isNeg flag).
       *)
      fun bigRem (num: bigInt, den: bigInt): bigInt =
	 if areSmall (num, den)
	    then let val numv = stripTag num
		     val numi = Word.toIntX numv
		     val denv = stripTag den
		     val deni = Word.toIntX denv
		     val ansi = Int.rem (numi, deni)
		     val answ = Word.fromInt ansi
		 in Prim.fromWord (addTag answ)
		 end
	 else let val nsize = size num
		  val dsize = size den
	      in if nsize < dsize
		    then num
		 else if den = zero
			 then raise Div
		      else
			 Prim.rem
			 (num, den, Word.* (Word.* (0w2, bytesPerWord),
					    Word.+ (Word.fromInt nsize, 0w3)))
	      end

      (*
       * bigInt addition.
       *)
      fun bigPlus (lhs: bigInt, rhs: bigInt): bigInt =
	 let
	    val res =
	       if areSmall (lhs, rhs)
		  then let val ansv = Word.+ (stripTag lhs, stripTag rhs)
			   val ans = addTag ansv
		       in if sameSign (ans, ansv)
			     then SOME (Prim.fromWord ans)
			  else NONE
		       end
	       else NONE
	 in
	    case res of
	       NONE => 
		  dontInline
		  (fn () =>
		   Prim.+ (lhs, rhs, reserve (Int.max (size lhs, size rhs), 1)))
	     | SOME i => i
	 end

      (*
       * bigInt subtraction.
       *)
      fun bigMinus (lhs: bigInt, rhs: bigInt): bigInt =
	 let
	    val res =
	       if areSmall (lhs, rhs)
		  then
		     let
			val ansv = Word.- (stripTag lhs, stripTag rhs)
			val ans = addTag ansv
		     in
			if sameSign (ans, ansv)
			   then SOME (Prim.fromWord ans)
			else NONE
		     end
	       else NONE
	 in
	    case res of
	       NONE =>
		  dontInline
		  (fn () =>
		   Prim.- (lhs, rhs, reserve (Int.max (size lhs, size rhs), 1)))
	     | SOME i => i
	 end

      (*
       * bigInt compare.
       *)
      fun bigCompare (lhs: bigInt, rhs: bigInt): order =
	 if areSmall (lhs, rhs)
	    then Int.compare (Word.toIntX (Prim.toWord lhs),
			      Word.toIntX (Prim.toWord rhs))
	 else Int.compare (Prim.compare (lhs, rhs), 0)


      (*
       * bigInt comparisions.
       *)
      local
	 fun makeTest (smallTest: smallInt * smallInt -> bool)
	    (lhs: bigInt, rhs: bigInt): bool =
	    if areSmall (lhs, rhs)
	       then smallTest (Word.toIntX (Prim.toWord lhs),
			       Word.toIntX (Prim.toWord rhs))
	    else smallTest (Prim.compare (lhs, rhs), 0)
      in
	 val bigGT = makeTest (op >)
	 val bigGE = makeTest (op >=)
	 val bigLE = makeTest (op <=)
	 val bigLT = makeTest (op <)
      end

      (*
       * bigInt abs.
       *)
      fun bigAbs (arg: bigInt): bigInt =
	 if isSmall arg
	    then let val argw = Prim.toWord arg
		 in if argw = badw
		       then negBad
		    else if Word.toIntX argw < 0
			    then Prim.fromWord (Word.- (0w2, argw))
			 else arg
		 end
	 else if bigIsNeg arg
		 then Prim.~ (arg, reserve (bigSize arg, 1))
	      else arg

      (*
       * bigInt min.
       *)
      fun bigMin (lhs: bigInt, rhs: bigInt): bigInt =
	 if bigLE (lhs, rhs)
	    then lhs
	 else rhs

      (*
       * bigInt max.
       *)
      fun bigMax (lhs: bigInt, rhs: bigInt): bigInt =
	 if bigLE (lhs, rhs)
	    then rhs
	 else lhs

      (*
       * bigInt sign.
       *)
      fun bigSign (arg: bigInt): smallInt =
	 if isSmall arg
	    then Int.sign (Word.toIntX (stripTag arg))
	 else if bigIsNeg arg
		 then ~1
	      else 1

      (*
       * bigInt sameSign.
       *)
      fun bigSameSign (lhs: bigInt, rhs: bigInt): bool =
	 bigSign lhs = bigSign rhs

      (*
       * bigInt gcd.
       * based on code from PolySpace.
       *)
      local
	 open Int

	 fun mod2 x = Word.toIntX (Word.andb (Word.fromInt x, 0w1))
	 fun div2 x = Word.toIntX (Word.>> (Word.fromInt x, 0w1))
	    
	 fun gcdInt (a, b, acc) =
	    case (a, b) of
	       (0, _) => b * acc
	     | (_, 0) => a * acc
	     | (_, 1) => acc
	     | (1, _) => acc
	     | _ => 
		  if a = b
		     then a * acc
		  else
		     let
			val a_2 = div2 a
			val a_r2 = mod2 a
			val b_2 = div2 b
			val b_r2 = mod2 b
		     in
			if 0 = a_r2
			   then
			      if 0 = b_r2
				 then gcdInt (a_2, b_2, acc + acc)
			      else gcdInt (a_2, b, acc)
			else
			   if 0 = b_r2
			      then gcdInt (a, b_2, acc)
			   else
			      if a >= b
				 then gcdInt (div2 (a - b), b, acc)
			      else gcdInt (a, div2 (b - a), acc)
		     end
		  
      in
	 fun bigGcd (lhs: bigInt, rhs: bigInt): bigInt =
	    if areSmall (lhs, rhs)
	       then
		  Prim.fromWord
		  (addTag
		   (Word.fromInt
		    (gcdInt (Int.abs (Word.toIntX (stripTag lhs)),
			     Int.abs (Word.toIntX (stripTag rhs)),
			     1))))
	    else Prim.gcd (lhs, rhs, reserve (max (size lhs, size rhs), 0))
      end

      (*
       * bigInt toString and fmt.
       * dpc is the maximum number of digits per `limb'.
       *)
      local
	 open StringCvt

	 fun cvt {base: smallInt,
		  dpc: word,
		  smallCvt: smallInt -> string}
	    (arg: bigInt)
	    : string =
	    if isSmall arg
	       then smallCvt (Word.toIntX (stripTag arg))
	    else Prim.toString (arg, base,
				Word.+
				(reserve (0, 0),
				 Word.+ (0w2, (* sign character *)
					 Word.* (dpc,
						 Word.fromInt (bigSize arg)))))
	 val binCvt = cvt {base = 2, dpc = 0w32, smallCvt = Int.fmt BIN}
	 val octCvt = cvt {base = 8, dpc = 0w11, smallCvt = Int.fmt OCT}
	 val hexCvt = cvt {base = 16, dpc = 0w8, smallCvt = Int.fmt HEX}
      in
	 val bigToString = cvt {base = 10,
				dpc = 0w10,
				smallCvt = Int.toString}
	 fun bigFmt radix =
	    case radix of
	       BIN => binCvt
	     | OCT => octCvt
	     | DEC => bigToString
	     | HEX => hexCvt
      end

      (*
       * bigInt scan and fromString.
       *)
      local
	 open StringCvt

	 (*
	  * We use Word.word to store chunks of digits.
	  * smallToInf converts such a word to a fixnum bigInt.
	  * Thus, it can only represent values in [- 2^30, 2^30).
	  *)
	 fun smallToBig (arg: Word.word): bigInt =
	    Prim.fromWord (addTag arg)
	    
	    
	 (*
	  * Given a char, if it is a digit in the appropriate base,
	  * convert it to a word.  Otherwise, return NONE.
	  * Note, both a-f and A-F are accepted as hexadecimal digits.
	  *)
	 fun binDig (ch: char): Word.word option =
	    case ch of
	       #"0" => SOME 0w0
	     | #"1" => SOME 0w1
	     | _ => NONE

	 local
	    val op <= = Char.<=
	 in
	    fun octDig (ch: char): Word.word option =
	       if #"0" <= ch andalso ch <= #"7"
		  then SOME (Word.fromInt (ord ch -? ord #"0"))
	       else NONE
		  
	    fun decDig (ch: char): Word.word option =
	       if #"0" <= ch andalso ch <= #"9"
		  then SOME (Word.fromInt (ord ch -? ord #"0"))
	       else NONE
		  
	    fun hexDig (ch: char): Word.word option =
	       if #"0" <= ch andalso ch <= #"9"
		  then SOME (Word.fromInt (ord ch -? ord #"0"))
	       else if #"a" <= ch andalso ch <= #"f"
		       then SOME (Word.fromInt (ord ch -? (ord #"a" - 0xa)))
		    else if #"A" <= ch andalso ch <= #"F"
			    then SOME (Word.fromInt
				       (ord ch -? (ord #"A" - 0xA)))
			 else
			    NONE
	 end
      
	 (*
	  * Given a digit converter and a char reader, return a digit
	  * reader.
	  *)
	 fun toDigR (charToDig: char -> Word.word option,
		     cread: (char, 'a) reader)
	    (state: 'a)
	    : (Word.word * 'a) option =
	    case cread state of
	       NONE => NONE
	     | SOME (ch, state') =>
		  case charToDig ch of
		     NONE => NONE
		   | SOME dig => SOME (dig, state')
			
	 (*
	  * A chunk represents the result of processing some digits.
	  * more is a bool indicating if there might be more digits.
	  * shift is base raised to the number-of-digits-seen power.
	  * chunk is the value of the digits seen.
	  *)
	 type chunk = {
		       more: bool,
		       shift: Word.word,
		       chunk: Word.word
		       }
	    
	 (*
	  * Given the base, the number of digits per chunk,
	  * a char reader and a digit reader, return a chunk reader.
	  *)
	 fun toChunkR (base: Word.word,
		       dpc: smallInt,
		       dread: (Word.word, 'a) reader)
	    : (chunk, 'a) reader =
	    let fun loop {left: smallInt,
			  shift: Word.word,
			  chunk: Word.word,
			  state: 'a}
	       : chunk * 'a =
	       if left <= 0
		  then ({more = true,
			 shift = shift,
			 chunk = chunk },
			state)
	       else
		  case dread state of
		     NONE => ({more = false,
			       shift = shift,
			       chunk = chunk},
			      state)
		   | SOME (dig, state') =>
			loop {
			      left = left - 1,
			      shift = Word.* (base, shift),
			      chunk = Word.+ (Word.* (base,
						      chunk),
					      dig),
			      state = state'
			      }
		fun reader (state: 'a): (chunk * 'a) option =
		   case dread state of
		      NONE => NONE
		    | SOME (dig, next) =>
			 SOME (loop {left = dpc - 1,
				     shift = base,
				     chunk = dig,
				     state = next})
	    in reader
	    end
	 
	 (*
	  * Given a chunk reader, return an unsigned reader.
	  *)
	 fun toUnsR (ckread: (chunk, 'a) reader): (bigInt, 'a) reader =
	    let fun loop (more: bool, ac: bigInt, state: 'a) =
	       if more
		  then case ckread state of
		     NONE => (ac, state)
		   | SOME ({more, shift, chunk}, state') =>
			loop (more,
			      bigPlus (bigMul (smallToBig shift,
					       ac),
				       smallToBig chunk),
			      state')
	       else (ac, state)
		fun reader (state: 'a): (bigInt * 'a) option =
		   case ckread state of
		      NONE => NONE
		    | SOME ({more, chunk, ...}, state') =>
			 SOME (loop (more,
				     smallToBig chunk,
				     state'))
	    in reader
	    end
	 
	 (*
	  * Given a char reader and an unsigned reader, return a signed
	  * reader.  This includes skipping any initial white space.
	  *)
	 fun toSign (cread: (char, 'a) reader, uread: (bigInt, 'a) reader)
	    : (bigInt, 'a) reader =
	    let fun reader (state: 'a): (bigInt * 'a) option =
	       case cread state of
		  NONE => NONE
		| SOME (ch, state') =>
		     if Char.isSpace ch
			then reader state'
		     else let val (isNeg, state'') =
			case ch of
			   #"+" =>
			   (false, state')
			 | #"-" =>
			      (true, state')
			 | #"~" =>
			      (true, state')
			 | _ =>
			      (false, state)
			  in if isNeg
				then case uread state'' of
				   NONE => NONE
				 | SOME (abs, state''') =>
				      SOME (bigNegate abs,
					    state''')
			     else uread state''
			  end
	    in reader
	    end
		  
	 (*
	  * Base-specific conversions from char readers to
	  * bigInt readers.
	  *)
	 local
	    fun reader (base, dpc, dig)
	       (cread: (char, 'a) reader): (bigInt, 'a) reader =
	       let val dread = toDigR (dig, cread)
		  val ckread = toChunkR (base, dpc, dread)
		  val uread = toUnsR ckread
		  val reader = toSign (cread, uread)
	       in reader
	       end
	 in
	    fun binReader z = reader (0w2, 29, binDig) z
	    fun octReader z = reader (0w8, 9, octDig) z
	    fun decReader z = reader (0w10, 9, decDig) z
	    fun hexReader z = reader (0wx10, 7, hexDig) z
	 end	 
      in
	 
	 local fun stringReader (pos, str) =
	    if pos >= String.size str
	       then NONE
	    else SOME (String.sub (str, pos),
		       (pos + 1, str))
	       val reader = decReader stringReader
	 in
	    fun bigFromString str =
	       case reader (0, str) of
		  NONE => NONE
		| SOME (res, _) => SOME res
	 end
      
	 fun bigScan radix =
	    case radix of
	       BIN => binReader
	     | OCT => octReader
	     | DEC => decReader
	     | HEX => hexReader
      end

      local
	 fun isEven (n: int) = Int.mod (Int.abs n, 2) = 0
      in
	 fun pow (i: bigInt, j: int): bigInt =
	    if j < 0
	       then
		  if i = zero
		     then raise Div
		  else if i = one
			  then one
		       else if i = negOne
			       then if isEven j
				       then one
				    else negOne
			    else zero
	    else
	       if j = 0
		  then one
	       else
		  let
		     fun square (n: bigInt): bigInt = bigMul (n, n)
		     (* pow (j) returns (i ^ j) *)
		     fun pow (j: int): bigInt =
			if j <= 0
			   then one
			else if isEven j
				then evenPow j
			     else bigMul (i, evenPow (j - 1))
		     (* evenPow (j) returns (i ^ j), assuming j is even *)
		     and evenPow (j: int): bigInt =
			square (pow (Int.quot (j, 2)))
		  in pow (j)
		  end
      end

      val op + = bigPlus
      val op - = bigMinus
      val op > = bigGT
      val op >= = bigGE
      val op < = bigLT
      val quot = bigQuot
      val rem = bigRem

      fun x div y =
	 if x >= zero
	    then if y > zero
		    then quot (x, y)
		 else if y < zero
			 then if x = zero
				 then zero
			      else quot (x - one, y) - one
		      else raise Div
	 else if y < zero
		 then quot (x, y)
	      else if y > zero
		      then quot (x + one, y) - one
		   else raise Div

      fun x mod y =
	 if x >= zero
	    then if y > zero
		    then rem (x, y)
		 else if y < zero
			 then if x = zero
				 then zero
			      else rem (x - one, y) + (one + y)
		      else raise Div
	 else if y < zero
		 then rem (x, y)
	      else if y > zero
		      then rem (x + one, y) + (y - one)
		   else raise Div

      fun divMod (x, y) = (x div y, x mod y)
      fun quotRem (x, y) = (quot (x, y), rem (x, y))

      (*
       * bigInt log2
       *)
      structure Word =
	 struct
	    open Word
	    fun log2 (w: word): int =
	       let
		  fun loop (n, s, ac): word =
		     if n = 0w1
			then ac
		     else
			let
			   val (n, ac) =
			      if n >= << (0w1, s)
				 then (>> (n, s), ac + s)
			      else (n, ac)
			in
			   loop (n, >> (s, 0w1), ac)
			end
	       in
		  toInt (loop (w, 0w16, 0w0))
	       end
	 end

      local
	 val bitsPerLimb: Int.int = 32
      in
	 fun log2 (n: bigInt): Int.int =
	    if bigLE (n, 0)
	       then raise Domain
	    else
	       case rep n of
		  Big v =>
		     Int.+ (Int.* (bitsPerLimb, Int.- (Vector.length v, 2)),
			    Word.log2 (Vector.sub (v, Int.- (Vector.length v, 1))))
		| Small i => Word.log2 (Word.fromInt i)
      end

      (* 
       * bigInt bit operations.
       *)
      local
	 fun make (wordOp, bigIntOp): bigInt * bigInt -> bigInt =
	    fn (lhs: bigInt, rhs: bigInt) =>
	    if areSmall (lhs, rhs)
	       then
		  let
		     val ansv = wordOp (stripTag lhs, stripTag rhs)
		     val ans = addTag ansv
		  in
		     Prim.fromWord ans
		  end
	    else
	       dontInline
	       (fn () => 
		bigIntOp (lhs, rhs, reserve (Int.max (size lhs, size rhs), 0)))
      in
	 val bigAndb = make (Word.andb, Prim.andb)
	 val bigOrb = make (Word.orb, Prim.orb)
	 val bigXorb = make (Word.xorb, Prim.xorb)
      end

      fun bigNotb (arg: bigInt): bigInt =
	 if isSmall arg
	    then Prim.fromWord (addTag (Word.notb (stripTag arg)))
	 else dontInline (fn () => Prim.notb (arg, reserve (size arg, 0)))

      local
	 val bitsPerLimb : Word.word = 0w32
	 fun shiftSize shift = Word.toIntX (Word.div (shift, bitsPerLimb))
      in
	 fun bigArshift (arg: bigInt, shift: word): bigInt =
	    if shift = 0wx0
	       then arg
	    else Prim.~>> (arg, shift,
			   reserve (Int.max (1, size arg -? shiftSize shift),
				    0))

	 fun bigLshift (arg: bigInt, shift: word): bigInt =
	    if shift = 0wx0
	       then arg
	    else Prim.<< (arg, shift, reserve (size arg +? shiftSize shift, 1))
      end
   
      type int = bigInt
      val abs = bigAbs
      val compare = bigCompare
      val divMod = divMod
      val fmt = bigFmt
      val fromInt = bigFromInt
      val fromInt64 = bigFromInt64
      val fromLarge = fn x => x
      val fromString = bigFromString
      val gcd = bigGcd
      val max = bigMax
      val maxInt = NONE
      val min = bigMin
      val minInt = NONE
      val op * = bigMul
      val op + = bigPlus
      val op - = bigMinus
      val op < = bigLT
      val op <= = bigLE
      val op > = bigGT
      val op >= = bigGE
      val op div = op div
      val op mod = op mod
      val pow = pow
      val precision = NONE
      val quot = bigQuot
      val quotRem = quotRem
      val rem = bigRem
      val rep = rep
      val sameSign = bigSameSign
      val scan = bigScan
      val sign = bigSign
      val toInt = bigToInt
      val toInt64 = bigToInt64
      val toLarge = fn x => x
      val toString = bigToString
      val ~ = bigNegate
      val andb = bigAndb
      val notb = bigNotb
      val orb = bigOrb
      val xorb = bigXorb
      val ~>> = bigArshift
      val << = bigLshift
   end

structure LargeInt = IntInf
