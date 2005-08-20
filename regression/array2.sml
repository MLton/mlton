open Array2

val x = ref 0

val i2s = Int.toString
   
fun test trv =
   let
      val a =
         tabulate trv
         (3, 4, fn (r, c) =>
          (x := !x + 1
           ; concat["(", i2s r, ", ", i2s c, ", ", i2s(!x), ")"]))
      val _ = app trv (fn s => (print s; print "\n")) a
   in ()
   end

val _ = (test RowMajor; test ColMajor)

(* Check that Size is correctly raised when constructing large arrays. *)
val m = valOf Int.maxInt

val _ =
   (array (m, 2, 13)
    ; print "FAIL")
   handle Size => print "OK"
      
val _ =
   (tabulate RowMajor (m, 2, fn _ => 13)
    ; print "FAIL")
   handle Size => print "OK\n"
