functor Test (structure PackReal: PACK_REAL
	      structure Real: REAL
	      sharing type PackReal.real = Real.real) =
struct
   
val _ =
   if List.all (fn r =>
		let
		   val r = valOf (Real.fromString r)
		   val v = PackReal.toBytes r
		   val _ =
		      print (concat ["r = ", Real.fmt StringCvt.EXACT r, "\t"])
		   val _ =
		      Vector.app
		      (fn w => print (concat [" ", Word8.toString w]))
		      v
		   val _ = print "\n"
		in 
		   Real.== (r, PackReal.fromBytes v)
		end)
      ["~100.0", "~1.1", "~0.12345", "0.0", "1.0", "123E6"]
      then ()
   else raise Fail "failure"
      
end

structure Z = Test (structure PackReal = PackReal32Big
		    structure Real = Real32)
structure Z = Test (structure PackReal = PackReal32Little
		    structure Real = Real32)
structure Z = Test (structure PackReal = PackReal64Big
		    structure Real = Real64)
structure Z = Test (structure PackReal = PackReal64Little
		    structure Real = Real64)
