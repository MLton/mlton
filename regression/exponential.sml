fun f x = x
fun toy () =
   let
      fun g y = f f f f f f f f f f f f f f f f f f f f f f f f f f f f f f f f y
   in g 3; g 4
   end
val _ = toy ()
