type int = Int.int

signature MLTON_VECTOR =
   sig
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b vector
   end
   
