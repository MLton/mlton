structure Real32 =
  Real
  (structure P = Primitive.Real32
   open P
   fun fromLarge m r =
      IEEEReal.withRoundingMode (m, fn () => P.fromLarge r)

   val realToWord: real -> word =
      fn r => Pack32Little.subVec (PackReal32Little.toBytes r, 0)
	 
   val wordToReal: word -> real =
      let
	 val a = Word8Array.array (4, 0w0)
      in
	 fn w =>
	 let
	    val _ = Pack32Little.update (a, 0, w)
	 in
	    PackReal32Little.subArr (a, 0)
	 end
      end

   fun nextAfterUp r = wordToReal (Word.+ (realToWord r, 0w1))
   fun nextAfterDown r = wordToReal (Word.- (realToWord r, 0w1))
  )
