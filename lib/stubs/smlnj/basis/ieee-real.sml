(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* SML/NJ uses an old version of datatype IEEEReal.float_class. *)
signature IEEE_REAL =
   sig
      exception Unordered
      datatype real_order = EQUAL | GREATER | LESS | UNORDERED
      datatype float_class = INF | NAN | NORMAL | SUBNORMAL | ZERO
      datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
      val setRoundingMode : rounding_mode -> unit
      val getRoundingMode : unit -> rounding_mode
      type decimal_approx = {digits:int list, exp:int, kind:float_class, sign:bool}
      val toString : decimal_approx -> string
      val fromString : string -> decimal_approx option
      val scan : (char,'a) StringCvt.reader -> (decimal_approx,'a) StringCvt.reader
   end

structure IEEEReal : IEEE_REAL =
   struct
      open IEEEReal

      datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
      type decimal_approx = {digits:int list, exp:int, kind:float_class, sign:bool}

      local
         structure P = Pervasive.IEEEReal
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
            {digits = digits, exp = exp, kind = toGoodFC kind, sign = sign}
         fun toBadDA {digits, exp, kind, sign} =
            {digits = digits, exp = exp, kind = toBadFC kind, sign = sign}
      in
         val toString = P.toString o toBadDA
         val fromString = (Option.map toGoodDA) o P.fromString
         fun scan r s =
            Option.map (fn (da,x) => (toGoodDA da,x)) (P.scan r s)
      end

   end
