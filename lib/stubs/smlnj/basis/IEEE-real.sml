(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature IEEE_REAL_EXTRA =
   sig
      include IEEE_REAL

      val toGoodFC: Pervasive.IEEEReal.float_class -> float_class
      val toBadFC: float_class -> Pervasive.IEEEReal.float_class
      val toGoodDA: Pervasive.IEEEReal.decimal_approx -> decimal_approx
      val toBadDA: decimal_approx -> Pervasive.IEEEReal.decimal_approx
   end

(* SML/NJ uses an old version of datatype IEEEReal.float_class. *)
structure IEEERealExtra : IEEE_REAL_EXTRA =
   struct
      open IEEEReal

      datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
      type decimal_approx = {class:float_class, digits:int list, exp:int, sign:bool}

      local
         structure P = Pervasive.IEEEReal
      in
         fun toGoodFC c =
            case c of
               P.NAN _ => NAN
             | P.INF => INF
             | P.ZERO => ZERO
             | P.NORMAL => NORMAL
             | P.SUBNORMAL => SUBNORMAL
         fun toBadFC c =
            case c of
               NAN => P.NAN P.QUIET
             | INF => P.INF
             | ZERO => P.ZERO
             | NORMAL => P.NORMAL
             | SUBNORMAL => P.SUBNORMAL
         fun toGoodDA {digits, exp, kind, sign} =
            {class = toGoodFC kind, digits = digits, exp = exp, sign = sign}
         fun toBadDA {class, digits, exp, sign} =
            {digits = digits, exp = exp, kind = toBadFC class, sign = sign}
         val toString = P.toString o toBadDA
         val fromString = (Option.map toGoodDA) o P.fromString
         fun scan r s = Option.map (fn (da,x) => (toGoodDA da,x)) (P.scan r s)
      end

   end

structure IEEEReal : IEEE_REAL = IEEERealExtra
