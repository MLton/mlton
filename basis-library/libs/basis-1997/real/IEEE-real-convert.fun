(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor IEEERealConvert
        (structure IEEEReal: IEEE_REAL):
        sig
          include IEEE_REAL_1997 
          val >> : IEEEReal.float_class -> float_class
          val << : float_class -> IEEEReal.float_class
          val >>> : IEEEReal.decimal_approx -> decimal_approx
          val <<< : decimal_approx -> IEEEReal.decimal_approx
        end =
  struct
     open IEEEReal

     datatype nan_mode = QUIET | SIGNALLING
     datatype float_class =
        NAN of nan_mode
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL
     val >> =
       fn IEEEReal.NAN => NAN QUIET
        | IEEEReal.INF => INF
        | IEEEReal.ZERO => ZERO
        | IEEEReal.NORMAL => NORMAL
        | IEEEReal.SUBNORMAL => SUBNORMAL
     val << =
       fn NAN _ => IEEEReal.NAN
        | INF => IEEEReal.INF
        | ZERO => IEEEReal.ZERO
        | NORMAL => IEEEReal.NORMAL
        | SUBNORMAL => IEEEReal.SUBNORMAL

     type decimal_approx = {kind: float_class, sign: bool,
                            digits: int list, exp: int}

     val <<< = fn {kind, sign, digits, exp} =>
       {class = << kind, sign = sign, 
        digits = digits, exp = exp}
     val >>> = fn {class, sign, digits, exp} =>
       {kind = >> class, sign = sign, 
        digits = digits, exp = exp}

     val toString = toString o <<<
     val fromString = fn s => 
       Option.map (>>>) (fromString s)
  end

structure IEEEReal1997 = IEEERealConvert(structure IEEEReal = IEEEReal)
