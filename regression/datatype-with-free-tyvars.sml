fun 'a f (x1: 'a, x2: 'a, aToString: 'a -> string): unit =
   let
      datatype 'b t = T of 'a * 'b
      and u = U of int t
      val y1: int t = T (x1, 13)
      val _: u = U y1
      val y2 = T (x2, "foo")
      fun 'b g (T (a, b), bToString: 'b -> string): unit =
         print (concat [aToString a, " ", bToString b, "\n"])
      val _ = g (y1, Int.toString)
      val _ = g (y2, fn s => s)
   in
      ()
   end

val _ = f (true, false, Bool.toString)
