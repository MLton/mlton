val _ =
   let val x = ref []
   in (!x = [1], !x = [true])
   end
