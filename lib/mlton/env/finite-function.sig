type int = Int.t
   
signature FINITE_FUNCTION = 
   sig
      type ('a, 'b) t
	 
      val foreach: ('a, 'b) t * ('a * 'b -> unit) -> unit
      val lookup: ('a, 'b) t * 'a -> 'b
      val size: ('a, 'b) t -> int
      val toFunction: ('a, 'b) t -> 'a -> 'b
   end

