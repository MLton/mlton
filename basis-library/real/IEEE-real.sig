signature IEEE_REAL =
   sig
      exception Unordered

      datatype real_order = LESS | EQUAL | GREATER | UNORDERED

      datatype float_class =
         NAN
       | INF
       | ZERO
       | NORMAL
       | SUBNORMAL

      datatype rounding_mode =
         TO_NEAREST
       | TO_NEGINF
       | TO_POSINF
       | TO_ZERO

      type decimal_approx = {class: float_class,
                             digits: int list,
                             exp: int,
                             sign: bool}

      val fromString: string -> decimal_approx option
      val getRoundingMode: unit -> rounding_mode
      val scan: (char, 'a) StringCvt.reader
                -> (decimal_approx, 'a) StringCvt.reader
      val setRoundingMode: rounding_mode -> unit 
      val toString: decimal_approx -> string 
   end

signature IEEE_REAL_EXTRA =
   sig
      include IEEE_REAL

      val mkClass: ('a -> C_Int.t) -> 'a -> float_class
      val withRoundingMode: rounding_mode * (unit -> 'a) -> 'a
   end
