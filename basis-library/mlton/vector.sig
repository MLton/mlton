type int = Int.int

signature MLTON_VECTOR =
   sig
      val unfold: int * 'a * ('a -> 'b * 'a) -> 'b vector
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b vector
   end
   
