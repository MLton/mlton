structure StringCvt =
   struct
      open StringCvt

      open OpenInt32

      datatype realfmt =
	 EXACT
       | FIX of Pervasive.Int32.int option 
       | GEN of Pervasive.Int32.int option 
       | SCI of Pervasive.Int32.int option 

      fun padLeft c i = StringCvt.padLeft c (toInt i)
      fun padRight c i = StringCvt.padRight c (toInt i)
   end
