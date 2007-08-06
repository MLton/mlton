signature PRE_REAL_GLOBAL =
  sig
      type real
      structure Math: MATH where type real = real
  end

signature PRE_REAL =
  sig
      include PRE_REAL_GLOBAL

      val * : real * real -> real
      val *+ : real * real * real -> real
      val *- : real * real * real -> real
      val + : real * real -> real
      val - : real * real -> real
      val / : real * real -> real
      val <  : real * real -> bool
      val <= : real * real -> bool
      val == : real * real -> bool
      val >  : real * real -> bool
      val >= : real * real -> bool
      val ?= : real * real -> bool
      val ~ : real -> real
      val abs: real -> real

      val maxFinite: real
      val minNormalPos: real
      val minPos: real

      val realSize: Primitive.Int32.int
      val precision: Primitive.Int32.int
      val radix: Primitive.Int32.int

      val class: real -> C_Int.t
      val signBit: real -> C_Int.t

      val nextAfterDown: real -> real
      val nextAfterUp: real -> real

      val frexp: real * C_Int.int ref -> real
      val ldexp: real * C_Int.int -> real
      val modf: real * real ref -> real

      val round: real -> real
      val gdtoa: real * C_Int.t * C_Int.t * C_Int.t * C_Int.t ref -> C_String.t
      val strto: Primitive.NullString8.t * C_Int.t -> real

      val fromInt8Unsafe: Primitive.Int8.int -> real
      val fromInt16Unsafe: Primitive.Int16.int -> real
      val fromInt32Unsafe: Primitive.Int32.int -> real
      val fromInt64Unsafe: Primitive.Int64.int -> real

      val fromReal32Unsafe: Primitive.Real32.real -> real
      val fromReal64Unsafe: Primitive.Real64.real -> real

      val fromWord8Unsafe: Primitive.Word8.word -> real
      val fromWord16Unsafe: Primitive.Word16.word -> real
      val fromWord32Unsafe: Primitive.Word32.word -> real
      val fromWord64Unsafe: Primitive.Word64.word -> real

      val toInt8Unsafe: real -> Primitive.Int8.int
      val toInt16Unsafe: real -> Primitive.Int16.int
      val toInt32Unsafe: real -> Primitive.Int32.int
      val toInt64Unsafe: real -> Primitive.Int64.int

      val toReal32Unsafe: real -> Primitive.Real32.real
      val toReal64Unsafe: real -> Primitive.Real64.real

      val toWord8Unsafe: real -> Primitive.Word8.word
      val toWord16Unsafe: real -> Primitive.Word16.word
      val toWord32Unsafe: real -> Primitive.Word32.word
      val toWord64Unsafe: real -> Primitive.Word64.word
  end

signature REAL_GLOBAL =
   sig
     include PRE_REAL_GLOBAL

     val round: real -> Int.int
     val trunc: real -> Int.int 
     val ceil: real -> Int.int
     val floor: real -> Int.int 
   end

signature REAL =
   sig
      include REAL_GLOBAL

      val != : real * real -> bool
      val * : real * real -> real
      val *+ : real * real * real -> real
      val *- : real * real * real -> real
      val + : real * real -> real
      val - : real * real -> real
      val / : real * real -> real
      val <  : real * real -> bool
      val <= : real * real -> bool
      val == : real * real -> bool
      val >  : real * real -> bool
      val >= : real * real -> bool
      val ?= : real * real -> bool
      val ~ : real -> real
      val abs: real -> real
      val checkFloat: real -> real
      val class: real -> IEEEReal.float_class
      val compare: real * real -> order
      val compareReal: real * real -> IEEEReal.real_order
      val copySign: real * real -> real
      val fmt: StringCvt.realfmt -> real -> string
      val fromDecimal: IEEEReal.decimal_approx -> real option
      val fromInt: int -> real
      val fromLarge: IEEEReal.rounding_mode -> LargeReal.real -> real
      val fromLargeInt: LargeInt.int -> real
      val fromManExp: {man: real, exp: int} -> real
      val fromString: string -> real option
      val isFinite: real -> bool
      val isNan: real -> bool
      val isNormal: real -> bool
      val max: real * real -> real
      val maxFinite: real
      val min: real * real -> real
      val minNormalPos: real
      val minPos: real
      val negInf: real
      val nextAfter: real * real -> real
      val posInf: real
      val precision: int
      val radix: int
      val realCeil: real -> real
      val realFloor: real -> real
      val realMod: real -> real
      val realRound: real -> real
      val realTrunc: real -> real
      val rem: real * real -> real
      val sameSign: real * real -> bool
      val scan: (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
      val sign: real -> int
      val signBit: real -> bool
      val split: real -> {whole: real, frac: real}
      val toDecimal: real -> IEEEReal.decimal_approx
      val toInt: IEEEReal.rounding_mode -> real -> int
      val toLarge: real -> LargeReal.real
      val toLargeInt: IEEEReal.rounding_mode -> real -> LargeInt.int
      val toManExp: real -> {man: real, exp: int}
      val toString: real -> string
      val unordered: real * real -> bool
   end

signature REAL_EXTRA =
   sig
      include REAL
      val realSize: Int.int

      val fromWord: word -> real
      val fromLargeWord: LargeWord.word -> real
      val toWord: IEEEReal.rounding_mode -> real -> word
      val toLargeWord: IEEEReal.rounding_mode -> real -> LargeWord.word
   end
