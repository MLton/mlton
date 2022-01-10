signature REAL =
   sig
      type real

      structure Math: MATH where type real = real

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
      val ceil: real -> Int.int
      val checkFloat: real -> real
      val class: real -> IEEEReal.float_class
      val compare: real * real -> order
      val compareReal: real * real -> IEEEReal.real_order
      val copySign: real * real -> real
      val floor: real -> Int.int
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
      val round: real -> Int.int
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
      val trunc: real -> Int.int
      val unordered: real * real -> bool
   end
