signature MONO_ARRAY2_1997 = 
   sig
      eqtype array
      type elem
      type region = {base: array, 
                     row: int, col: int, 
                     nrows: int option, ncols: int option}
      datatype traversal = datatype Array2.traversal
      structure Vector: MONO_VECTOR_1997
      val array: (int * int * elem) -> array
      val fromList: elem list list -> array
      val tabulate: traversal -> (int * int * ((int * int) -> elem)) -> array
      val sub: (array * int * int) -> elem
      val update: (array * int * int * elem) -> unit
      val dimensions: array -> (int * int)
      val nCols: array -> int
      val nRows: array -> int
      val row: (array * int) -> Vector.vector
      val column: (array * int) -> Vector.vector
      val copy: {src: region, dst: array, dst_row: int, dst_col: int} -> unit
      val appi: Array2.traversal -> ((int * int * elem) -> unit) -> region -> unit
      val app: Array2.traversal -> (elem -> unit) -> array -> unit
      val modifyi: Array2.traversal -> ((int * int * elem) -> elem) -> region -> unit
      val modify: Array2.traversal -> (elem -> elem) -> array -> unit
      val foldi: Array2.traversal -> ((int * int * elem * 'b) -> 'b) -> 'b -> region -> 'b
      val fold: Array2.traversal -> ((elem * 'b) -> 'b) -> 'b -> array -> 'b
   end
