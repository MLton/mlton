structure Array2: ARRAY2 =
struct

open Array2

type 'a t = 'a array

fun toList a =
   let
      fun loop(r, ac) =
	 if r = ~1
	    then ac
	 else loop(r - 1,
		   let
		      fun loop(c, ac) =
			 if c = ~1
			    then ac
			 else loop(c - 1, sub(a, r, c) :: ac)
		   in loop(nCols a - 1, [])
		   end :: ac)
   in loop(nRows a - 1, [])
   end
   
fun layout f a = List.layout (List.layout f) (toList a)

fun wholeRegion a : 'a region =
   {base = a, row = 0, col = 0, nrows = NONE, ncols = NONE}

(* All of the following stuff is here because NJ doesn't implement Array2.copy.
 *)
fun checkSliceMax(start: int, num: int option, max: int): int =
   case num of
      NONE => if start < 0 orelse start > max
		 then raise Subscript
	      else max
    | SOME num =>
	 if start < 0 orelse num < 0 orelse start > max - num
	    then raise Subscript
	 else start + num

fun checkRegion{base, row, col, nrows, ncols} =
   let val (rows, cols) = dimensions base
   in {stopRow = checkSliceMax(row, nrows, rows),
       stopCol = checkSliceMax(col, ncols, cols)}
   end

fun copy{src = src as {base, row, col, nrows, ncols},
	 dst, dst_row, dst_col} =
   let val {stopRow, stopCol} = checkRegion src
      val nrows = stopRow - row
      val ncols = stopCol - col
      val _ = checkRegion{base = dst, row = dst_row, col = dst_col,
			  nrows = SOME nrows, ncols = SOME ncols}
      fun for(start, stop, f) =
	 let fun loop i = if i = stop then () else (f i; loop(i + 1))
	 in loop start
	 end
      fun forDown(start, stop, f) =
	 let fun loop i = if i < start then () else (f i; loop(i - 1))
	 in loop(stop - 1)
	 end
      val forRows = if row <= dst_row then forDown else for
      val forCols = if col <= dst_col then for else forDown
   in forRows(0, nrows, fn r =>
	      forCols(0, ncols, fn c =>
		      update(dst, dst_row + r, dst_col + c,
			     sub(base, row + r, col + c))))
   end

fun foralli(a, f) =
   let exception False
   in (appi RowMajor (fn (r, c, x) =>
		      if f(r, c, sub(a, r, c))
			 then ()
		      else raise False)
       (wholeRegion a)
       ; true)
      handle False => false
   end

fun equals(a, a', f) =
   nRows a = nRows a'
   andalso nCols a = nCols a'
   andalso foralli(a, fn (r, c, x) => f(x, sub(a', r, c)))

fun forall(a, f) = foralli(a, f o #3)

fun tabulate(r, c, f) = Pervasive.Array2.tabulate RowMajor (r, c, f)

fun foreachi(a, f) =
   foldi RowMajor (fn (r, c, a, ()) => f(r, c, a)) () (wholeRegion a)

fun foreach(a, f) = foreachi(a, f o #3)
   
fun copy a = tabulate(nRows a, nCols a, fn (r, c) => sub(a, r, c))

fun new(r, c, x) = tabulate(r, c, fn _ => x)
   
end

