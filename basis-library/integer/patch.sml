(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(* Patch in fromLarge and toLarge now that IntInf is defined.
 *)

structure Int8: INTEGER_EXTRA =
   struct
      open Int8
       
      val fromLarge = fromInt o IntInf.toInt
      val toLarge = IntInf.fromInt o toInt
   end
structure Int16: INTEGER_EXTRA =
   struct
      open Int16
       
      val fromLarge = fromInt o IntInf.toInt
      val toLarge = IntInf.fromInt o toInt
   end
structure Int32: INTEGER_EXTRA =
   struct
      open Int32

      val fromLarge = IntInf.toInt
      val toLarge = IntInf.fromInt
   end
structure Int64: INTEGER_EXTRA =
   struct
      open Int64

      val fromLarge = IntInf.toInt64
      val toLarge = IntInf.fromInt64

      val op * =
	 if Primitive.detectOverflow
	    then fn (i, j) => fromLarge (IntInf.* (toLarge i, toLarge j))
	 else op *?

      (* Must redefine scan because the Integer functor defines it in terms of
       * Int64.*, which wasn't defined yet.
       *)
      fun scan radix reader state =
	 case IntInf.scan radix reader state of
	    NONE => NONE
	  | SOME (i, s) => SOME (fromLarge i, s)
		      
      val fromString = StringCvt.scanString (scan StringCvt.DEC)
   end

structure Int = Int32
structure Position = Int
structure FixedInt = Int
structure LargeInt = IntInf

structure Word8: WORD_EXTRA =
   struct
      open Word8

      val toLargeIntX = IntInf.fromInt o toIntX
      val toLargeInt = IntInf.fromInt o toInt

      fun fromLargeInt (i: IntInf.int): word =
	 fromInt (IntInf.toInt (IntInf.mod (i, 256)))
   end

structure Word16: WORD_EXTRA =
   struct
      open Word16

      val toLargeIntX = IntInf.fromInt o toIntX
      val toLargeInt = IntInf.fromInt o toInt

      fun fromLargeInt (i: IntInf.int): word =
	 fromInt (IntInf.toInt (IntInf.mod (i, 65536)))
   end

structure Word32: WORD32_EXTRA =
   struct
      open Word32

      val toLargeIntX = IntInf.fromInt o toIntX

      fun highBitSet w = w >= 0wx80000000

      fun toLargeInt (w: word): LargeInt.int =
	 if highBitSet w
	    then
	       (* Use 2 * 0x40000000 instead of 0x80000000 so that SML/NJ
		* doesn't complain about an integer constant being too large.
		*)
	       IntInf.+ (IntInf.* (2, 0x40000000), 
			 toLargeIntX (andb (w, 0wx7FFFFFFF)))
	 else toLargeIntX w

      fun toReal (w: word): real =
	 if highBitSet w
	    then
	       Real.+ (2147483648.0, (* 2 ^ 31 *)
		       Real.fromInt (toIntX (andb (w, 0wx7FFFFFFF))))
	 else Real.fromInt (toIntX w)

      local
	 val t32 = IntInf.pow (2, 32)
	 val t31 = IntInf.pow (2, 31)
      in
	 fun fromLargeInt (i: IntInf.int): word =
	    fromInt
	    (let
		open IntInf
		val low32 = i mod t32
	     in
		toInt (if low32 >= t31
			  then low32 - t32
		       else low32)
	     end)
      end
   end

structure Word64: WORD =
   struct
      open Word64

      structure W = Word64

      val t32 = IntInf.pow (2, 32)
      val t64 = IntInf.pow (2, 64)
	 
      fun toLargeInt w =
	 IntInf.+ (Word32.toLargeInt (Word32.fromLarge w),
		   IntInf.<< (Word32.toLargeInt (Word32.fromLarge (>> (w, 0w32))),
			      0w32))

      fun toLargeIntX w =
	 if 0w0 = andb (w, << (0w1, 0w63))
	    then toLargeInt w
	 else IntInf.- (toLargeInt w, t64)

      fun fromLargeInt (i: IntInf.int): word =
	 let
	    val (d, m) = IntInf.divMod (i, t32)
	 in
	    W.orb (W.<< (Word32.toLarge (Word32.fromLargeInt d), 0w32),
		   Word32.toLarge (Word32.fromLargeInt m))
	 end
   end

structure Word = Word32
structure LargeWord = Word64
structure SysWord = Word32
