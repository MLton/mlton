type int = Int.int
   
signature MLTON_ARRAY =
   sig
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b array
   end
