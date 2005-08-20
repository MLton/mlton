open Array2

fun printa a =
   let val (rows, cols) = dimensions a
      fun loopRows r =
         if r = rows
            then ()
         else (let
                  fun loopCols c =
                     if c = cols
                        then ()
                     else (print(Int.toString(sub(a, r, c))) ;
                           print " " ;
                           loopCols(c + 1))
               in loopCols 0
               end;
               print "\n";
               loopRows(r + 1))
   in loopRows 0
   end

val a1 = array(4, 5, 13)
val _ = (printa a1;
         modifyi RowMajor (fn (x, y, _) => x + y)
         {base = a1, row = 0, col = 0, nrows = NONE, ncols = NONE};
         printa a1)

val a2 = fromList[[1, 2], [3, 4], [5, 6]]
val _ = printa a2

fun bogus l = (fromList l; false) handle Size => true

val _ = (bogus[[1], [2, 3]];
         bogus[[], [1]])
   
val a3 =
   let val r = ref 0
   in tabulate RowMajor (3, 3, fn _ => (r := !r + 1 ; !r))
   end
val _ = printa a3

val a4 =
   let val r = ref 0
   in tabulate ColMajor (3, 3, fn _ => (r := !r + 1 ; !r))
   end
val _ = printa a4
