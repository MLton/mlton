signature MONO_ARRAY2 =
   sig
      eqtype array

      type elem
      type vector

      type region = {base: array,
                     row: int,
                     col: int,
                     nrows: int option,
                     ncols: int option}

      datatype traversal = datatype Array2.traversal

      val array: int * int * elem -> array 
      val fromList: elem list list -> array 
      val tabulate: traversal -> int * int * (int * int -> elem) -> array 
      val sub: array * int * int -> elem 
      val update: array * int * int * elem -> unit 
      val dimensions: array -> int * int 
      val nCols: array -> int 
      val nRows: array -> int 
      val row: array * int -> vector 
      val column: array * int -> vector 
      val copy: {src: region, dst: array, dst_row: int, dst_col: int} -> unit
      val appi: traversal -> (int * int * elem -> unit) -> region -> unit 
      val app: traversal -> (elem -> unit) -> array -> unit 
      val foldi: traversal -> (int * int * elem * 'b -> 'b) -> 'b -> region -> 'b 
      val fold: traversal -> (elem * 'b -> 'b) -> 'b -> array -> 'b
      val modifyi: traversal -> (int * int * elem -> elem) -> region -> unit 
      val modify: traversal -> (elem -> elem) -> array -> unit 
   end
