fun die(): 'a =
   let exception Die
   in raise Die
   end

val _ =
   let val _: string = die()
      val _: real = die()
      val _: bool = die()
   in ()
   end handle _ => ()
