functor Basic(structure Real : REAL
              structure Pack : PACK_REAL
              where type real = Real.real) =
   struct
      open Real
      val () = print "  Reported\n"
      val () = print ("    precision:    " ^ Int.toString precision ^ "\n")
      val {man=_, exp} = toManExp maxFinite
      val () = print ("    max exponent: " ^ Int.toString exp ^ "\n")
      val {man=_, exp} = toManExp minNormalPos
      val () = print ("    min exponent: " ^ Int.toString exp ^ "\n")
      val {man=_, exp} = toManExp minPos
      val () = print ("    min denormal: " ^ Int.toString exp ^ "\n")
      
      (* Now let's compute the actual mantissa *)
      val zero = fromInt 0
      val one = fromInt 1
      val two = fromInt 2
      
      fun precision eq x =
         if eq (x+one, x) then 0 else
         Int.+ (1, precision eq (x+x))
      fun maxExp eq x =
         if eq (x, x+x) then 0 else
         Int.+ (1, maxExp eq (x+x))
      fun lowBit (1, x) = x
        | lowBit (i, x) = lowBit (Int.- (i, 1), x / two)
      fun minExp eq x =
         if not (eq (x, (x / two) * two)) orelse eq (x, zero) then 1 else
         Int.- (minExp eq (x / two), 1)
      
      val eq = ==
      val xprecision = precision eq one
      val lastBit = one + lowBit (xprecision, one)
      val xmaxExp = maxExp eq one
      val xminNormalExp = minExp eq lastBit
      val xminExp = minExp eq one
      
      val () = print "  Actual\n"
      val () = print ("    precision:    " ^ Int.toString xprecision ^ "\n")
      val () = print ("    max exponent: " ^ Int.toString xmaxExp ^ "\n")
      val () = print ("    min exponent: " ^ Int.toString xminNormalExp ^ "\n")
      val () = print ("    min denormal: " ^ Int.toString xminExp ^ "\n")
      
      val a = Word8Array.array (Pack.bytesPerElem, 0w0)
      fun id x = (Pack.update (a, 0, x); Pack.subArr (a, 0))
      
      val eq = fn (x, y) => == (id x, id y)
      val xprecision = precision eq one
      val lastBit = one + lowBit (xprecision, one)
      val xmaxExp = maxExp eq one
      val xminNormalExp = minExp eq lastBit
      val xminExp = minExp eq one
      
      val () = print "  Exported\n"
      val () = print ("    precision:    " ^ Int.toString xprecision ^ "\n")
      val () = print ("    max exponent: " ^ Int.toString xmaxExp ^ "\n")
      val () = print ("    min exponent: " ^ Int.toString xminNormalExp ^ "\n")
      val () = print ("    min denormal: " ^ Int.toString xminExp ^ "\n")
   end

val () = print "Real32\n"
structure Z = Basic(structure Real = Real32
                    structure Pack = PackReal32Little)
val () = print "Real64\n"
structure Z = Basic(structure Real = Real64
                    structure Pack = PackReal64Big)
