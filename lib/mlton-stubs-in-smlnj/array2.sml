functor Array2
   (Array2:
    sig
       eqtype 'a array
       type 'a elem
       type 'a region = {base: 'a array,
			 row: int,
			 col: int,
			 nrows: int option,
			 ncols: int option}

       datatype traversal = RowMajor | ColMajor

       val app: traversal -> ('a -> unit) -> 'a array -> unit 
       val appi: traversal -> (int * int * 'a -> unit) -> 'a region -> unit 
       val array: int * int * 'a -> 'a array 
       val column: ('a array * int) -> 'a vector 
       val copy: {src: 'a region, dst: 'a array, dst_row: int, dst_col: int} -> unit
       val dimensions: 'a array -> (int * int) 
       val fold: traversal -> ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
       val foldi: traversal -> (int * int * 'a * 'b -> 'b) -> 'b -> 'a region -> 'b 
       val fromList: 'a list list -> 'a array 
       val modify: traversal -> ('a -> 'a) -> 'a array -> unit 
       val modifyi: traversal -> (int * int * 'a -> 'a) -> 'a region -> unit 
       val nCols: 'a array -> int 
       val nRows: 'a array -> int 
       val row: ('a array * int) -> 'a vector 
       val sub: 'a array * int * int -> 'a 
       val tabulate: traversal -> (int * int * (int * int -> 'a)) -> 'a array 
       val update: 'a array * int * int * 'a -> unit 
    end) =
   struct
      open Array2 OpenInt32

      type int = Int32.int
      type 'a region = {base: 'a array,
			row: int,
			col: int,
			nrows: int option,
			ncols: int option}

      fun toRegion{base, row, col, nrows, ncols}: 'a Array2.region =
	 {base = base,
	  row = toInt row,
	  col = toInt col,
	  nrows = toIntOpt nrows,
	  ncols = toIntOpt ncols}

      val array = fn (r, c, x) => array(toInt r, toInt c, x)
      val tabulate =
	 fn t => fn (r, c, f) =>
	 tabulate t (toInt r, toInt c, fn (r, c) => f(fromInt r, fromInt c))
      val sub = fn (a, r, c) => sub(a, toInt r, toInt c)
      val update = fn (a, r, c, x) => update(a, toInt r, toInt c, x)
      val dimensions = fn a => let val (r, c) = dimensions a
			       in (fromInt r, fromInt c)
			       end
      val nCols = fn a => fromInt(nCols a)
      val nRows = fn a => fromInt(nRows a)
      val row = fn (a, r) => row(a, toInt r)
      val column = fn (a, c) => column(a, toInt c)
      val copy =
	 fn {src, dst, dst_row, dst_col} =>
	 copy{src = toRegion src,
	      dst = dst,
	      dst_row = toInt dst_row,
	      dst_col = toInt dst_col}
      val appi =
	 fn t => fn f => fn r =>
	 appi t (fn (r, c, x) => f(fromInt r, fromInt c, x)) (toRegion r)
      val modifyi =
	 fn t => fn f => fn r =>
	 modifyi t (fn (r, c, x) => f(fromInt r, fromInt c, x)) (toRegion r)
      val foldi =
	 fn t => fn f => fn b => fn r =>
	 foldi t (fn (r, c, x, y) => f(fromInt r, fromInt c, x, y))
	 b (toRegion r)
   end

structure Array2 =
   let 
      structure A = Array2(open Array2
			   type 'a elem = 'a)
   in struct open Array2 A end
   end
