type int = Int.int
   
signature MLTON_ARRAY =
   sig
      val unfold: int * 'a * ('a -> 'b * 'a) -> 'b array
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b array
   end
