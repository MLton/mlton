functor Test (structure PackReal: PACK_REAL
              structure Real: REAL
              val tests: Real.real list
              sharing type PackReal.real = Real.real) =
struct
   
val _ =
   if List.all (fn r =>
                let
                   val v = PackReal.toBytes r
                   val _ =
                      print (concat ["r = ", Real.fmt StringCvt.EXACT r, "\t"])
                   val _ =
                      Word8Vector.app
                      (fn w =>
                       let
                          val s = Word8.toString w
                       in
                          print (if String.size s = 1
                                    then concat ["0", s]
                                 else s)
                       end)
                      v
                   val _ = print "\n"
                in 
                   Real.== (r, PackReal.fromBytes v)
                end)
      tests
      then ()
   else raise Fail "failure"
      
end

val real32Tests =
   let
      open Real32
   in
      [negInf,
       ~100.0,
       ~1.1,
       ~0.12345,
       ~0.0,
       0.0,
       minPos,
       minNormalPos,
       1.0,
       2.0,
       123E6,
       maxFinite, 
       posInf]
   end

val real64Tests =
   let
      open Real64
   in
      [negInf,
       ~100.0,
       ~1.1,
       ~0.12345,
       ~0.0,
       0.0,
       minPos,
       minNormalPos,
       1.0,
       2.0,
       123E6,
       maxFinite, 
       posInf]
   end

structure Z = Test (structure PackReal = PackReal32Big
                    structure Real = Real32
                    val tests = real32Tests)
structure Z = Test (structure PackReal = PackReal32Little
                    structure Real = Real32
                    val tests = real32Tests)
structure Z = Test (structure PackReal = PackReal64Big
                    structure Real = Real64
                    val tests = real64Tests)
structure Z = Test (structure PackReal = PackReal64Little
                    structure Real = Real64
                    val tests = real64Tests)
