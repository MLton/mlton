type int = Int.int

signature MLTON_VECTOR =
   sig
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a vector
   end
   
