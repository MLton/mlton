(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IEEEReal: IEEE_REAL =
   struct
      exception Unordered
      datatype real_order = LESS | EQUAL | GREATER | UNORDERED

      datatype float_class =
         INF
       | NAN
       | NORMAL
       | SUBNORMAL
       | ZERO

      datatype rounding_mode =
         TO_NEAREST
       | TO_NEGINF
       | TO_POSINF
       | TO_ZERO

      type decimal_approx =
         {class: float_class,
          digits: int list,
          exp: int,
          sign: bool}

      val setRoundingMode = fn _ => raise Fail "IEEEReal.setRoundingMode"
      val getRoundingMode = fn _ => raise Fail "IEEEReal.getRoundingMode"

      fun 'a scan reader (state: 'a) = raise Fail "IEEEReal.scan"

      val fromString = fn _ => raise Fail "IEEEReal.fromString"
      val toString = fn _ => raise Fail "IEEEReal.toString"
   end
