structure IntInf: INT_INF = Integer(open Pervasive.IntInf
				    fun toIntInf x = x)

structure IntInf =
   struct
      open IntInf

      val fromInt = Trace.trace("IntInf.fromInt", Int.layout, layout) fromInt
      val toInt = Trace.trace("IntInf.toInt", layout, Int.layout) toInt
   end

structure LargeInt = IntInf
