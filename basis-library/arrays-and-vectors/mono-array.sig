signature MONO_ARRAY =
   sig
      eqtype array
      type elem
	 
      structure Vector: MONO_VECTOR
	 
      val app: (elem -> unit) -> array -> unit 
      val appi: ((int * elem) -> unit) -> (array * int * int option) -> unit 
      val array: (int * elem) -> array 
      val copy: {src: array,
		 si: int,
		 len: int option,
		 dst: array,
		 di: int} -> unit 
      val copyVec: {src: Vector.vector,
		    si: int,
		    len: int option,
		    dst: array,
		    di: int} -> unit 
      val extract: (array * int * int option) -> Vector.vector 
      val foldl: ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
      val foldli:
	 ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b 
      val foldr: ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
      val foldri:
	 ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b 
      val fromList: elem list -> array 
      val length: array -> int 
      val maxLen: int 
      val modify: (elem -> elem) -> array -> unit
      val modifyi: ((int * elem) -> elem) -> (array * int * int option) -> unit 
      val sub: (array * int) -> elem 
      val tabulate: (int * (int -> elem)) -> array 
      val update: (array * int * elem) -> unit 
   end
