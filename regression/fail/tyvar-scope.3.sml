val _ =
   fn () =>
   let
      exception E of 'a
      val 'a f = fn z => z
   in
      ()
   end
