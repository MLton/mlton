structure Itimer: MLTON_ITIMER =
   struct
      structure Prim = Primitive.Itimer
	 
      datatype t = Prof | Real | Virtual

      val signal =
	 fn Prof => PosixPrimitive.Signal.prof
	  | Real => PosixPrimitive.Signal.alrm
	  | Virtual => PosixPrimitive.Signal.vtalrm

      val toInt =
	 fn Prof => Prim.prof
	  | Real => Prim.real
	  | Virtual => Prim.virtual

      fun set (t, {interval = Time.T {sec = s1, usec = u1},
		   value = Time.T {sec = s2, usec = u2}}) =
	 Prim.set (toInt t, s1, u1, s2, u2)
   end
