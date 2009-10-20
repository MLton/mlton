functor Basic(structure Real : REAL
              structure Pack : PACK_REAL
              where type real = Real.real) =
   struct
      open Real
      val () = print "  Reported\n"
      val () = print ("    precision:    " ^ Int.toString precision ^ "\n")
      val {man=_, exp} = toManExp maxFinite
      val () = print ("    max exponent: " ^ Int.toString exp ^ "\n")
      val {man=_, exp} = toManExp minPos
      val () = print ("    min exponent: " ^ Int.toString exp ^ "\n")
      val () = print ("    subnormal:    yes\n")
      
      (* Now let's compute the actual mantissa *)
      val zero = fromInt 0
      val one = fromInt 1
      val two = fromInt 2
      
      fun precision eq x =
         if eq (x+one, x) then 0 else
         Int.+ (1, precision eq (x+x))
      fun maxExp inf x =
         if inf (x+x) then 1 else
         Int.+ (1, maxExp inf (x+x))
      fun minExp zero x =
         if zero (x / two) then 1 else
         Int.- (minExp zero (x / two), 1)
      fun deNorm zero =
         if zero (minPos * (one + one / two) - minPos) then "no" else "yes"
      
      val xprecision = precision == one
      val xmaxExp = maxExp (not o isFinite) one
      val xminExp = minExp (fn z => == (z, zero)) one
      val xdeNorm = deNorm (fn z => == (z, zero))
      
      val () = print "  Actual\n"
      val () = print ("    precision:    " ^ Int.toString xprecision ^ "\n")
      val () = print ("    max exponent: " ^ Int.toString xmaxExp ^ "\n")
      val () = print ("    min exponent: " ^ Int.toString xminExp ^ "\n")
      val () = print ("    subnormal:    " ^ xdeNorm ^ "\n")
      
      val a = Word8Array.array (Pack.bytesPerElem, 0w0)
      fun id x = (Pack.update (a, 0, x); Pack.subArr (a, 0))
      val xprecision = precision (fn (x, y) => == (id x, id y)) one
      val xmaxExp = maxExp (not o isFinite o id) one
      val xminExp = minExp (fn z => == (id z, zero)) one
      val xdeNorm = deNorm (fn z => == (id z, zero))
      
      val () = print "  Exported\n"
      val () = print ("    precision:    " ^ Int.toString xprecision ^ "\n")
      val () = print ("    max exponent: " ^ Int.toString xmaxExp ^ "\n")
      val () = print ("    min exponent: " ^ Int.toString xminExp ^ "\n")
      val () = print ("    subnormal:    " ^ xdeNorm ^ "\n")
   end

val () = print "Real32\n"
structure Z = Basic(structure Real = Real32
                    structure Pack = PackReal32Little)
val () = print "Real64\n"
structure Z = Basic(structure Real = Real64
                    structure Pack = PackReal64Big)
