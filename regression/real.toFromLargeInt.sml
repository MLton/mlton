fun mkInt i n = if n = 0
		  then i
		  else mkInt (IntInf.+ (IntInf.* (i, IntInf.fromInt 10),i)) 
                             (n - 1)

val mkInt = fn i => fn n => mkInt (IntInf.fromInt i) n

fun check i n mode = 
  let
    val _ = IEEEReal.setRoundingMode mode
    val _ = print "check\t"
    val _ = print (Int.toString i)
    val _ = print "\t"
    val _ = print (Int.toString n)
    val _ = print "\t"
    val _ = print (case mode of
		     IEEEReal.TO_NEAREST => "TO_NEAREST"
		   | IEEEReal.TO_NEGINF => "TO_NEGINF"
		   | IEEEReal.TO_POSINF => "TO_POSINF"
		   | IEEEReal.TO_ZERO => "TO_ZERO")
    val _ = print "\n"
    fun doit i =
      let
	val r = Real.fromLargeInt i
	val i' = Real.toLargeInt mode r
      in
	print (IntInf.toString i);
	print "\n";
	print (Real.fmt (StringCvt.FIX (SOME 1)) r);
	print "\n";
	print (IntInf.toString i');
	print "\n"
      end
    val i = mkInt i n
  in
    doit i;
    doit (IntInf.~ i);
    print "\n"
  end

val _ = check 2 17 IEEEReal.TO_NEAREST
val _ = check 2 17 IEEEReal.TO_NEGINF
val _ = check 2 17 IEEEReal.TO_POSINF
val _ = check 2 17 IEEEReal.TO_ZERO
