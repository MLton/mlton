signature ARRAY2 =
   sig
      eqtype 'a array

      type 'a region = {base: 'a array,
                        row: int,
                        col: int,
                        nrows: int option,
                        ncols: int option}

      datatype traversal = RowMajor | ColMajor

      val array: int * int * 'a -> 'a array 
      val fromList: 'a list list -> 'a array 
      val tabulate: traversal -> (int * int * (int * int -> 'a)) -> 'a array 
      val sub: 'a array * int * int -> 'a 
      val update: 'a array * int * int * 'a -> unit 
      val dimensions: 'a array -> int * int
      val nRows: 'a array -> int 
      val nCols: 'a array -> int 
      val row: 'a array * int -> 'a vector 
      val column: 'a array * int -> 'a vector 
      val copy: {src: 'a region,
                 dst: 'a array,
                 dst_row: int,
                 dst_col: int} -> unit
      val appi: traversal -> (int * int * 'a -> unit) -> 'a region -> unit 
      val app: traversal -> ('a -> unit) -> 'a array -> unit 
      val foldi: traversal -> (int * int * 'a * 'b -> 'b) -> 'b -> 'a region -> 'b 
      val fold: traversal -> ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
      val modifyi: traversal -> (int * int * 'a -> 'a) -> 'a region -> unit 
      val modify: traversal -> ('a -> 'a) -> 'a array -> unit 
   end
