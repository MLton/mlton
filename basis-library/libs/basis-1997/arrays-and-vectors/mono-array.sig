signature MONO_ARRAY_1997 =
   sig
      eqtype array
      type elem
      structure Vector: MONO_VECTOR_1997
      val maxLen: int
      val array: (int * elem) -> array
      val fromList: elem list -> array
      val tabulate: (int * (int -> elem)) -> array
      val length: array -> int
      val sub: (array * int) -> elem
      val update: (array * int * elem) -> unit
      val extract: (array * int * int option) -> Vector.vector
      val copy: {src: array, si: int, len: int option,
                 dst: array, di: int} -> unit
      val copyVec: {src: Vector.vector, si: int, len: int option, 
                    dst: array, di: int} -> unit
      val appi: ((int * elem) -> unit) -> (array * int * int option) -> unit
      val app: (elem -> unit) -> array -> unit
      val foldli: ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b
      val foldri: ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b
      val foldl: ((elem * 'b) -> 'b) -> 'b -> array -> 'b
      val foldr: ((elem * 'b) -> 'b) -> 'b -> array -> 'b
      val modifyi: ((int * elem) -> elem) -> (array * int * int option) -> unit
      val modify: (elem -> elem) -> array -> unit
   end
