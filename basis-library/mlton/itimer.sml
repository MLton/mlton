structure Itimer: MLTON_ITIMER =
   struct
      structure Prim = Primitive.Itimer
	 
      datatype which = Prof | Real | Virtual

      val whichSignal =
	 fn Prof => PosixPrimitive.Signal.prof
	  | Real => PosixPrimitive.Signal.alrm
	  | Virtual => PosixPrimitive.Signal.vtalrm

      val whichToInt =
	 fn Prof => Prim.prof
	  | Real => Prim.real
	  | Virtual => Prim.virtual

      fun set (which,
	       {interval = Time.T {sec = s1, usec = u1},
		value = Time.T {sec = s2, usec = u2}}) =
	 Prim.set (whichToInt which, s1, u1, s2, u2)
   end
