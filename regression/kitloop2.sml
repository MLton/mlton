(*kitloop2.sml*)

(* A tail-recursive loop which gives stack overflow, unless one
   uses storage mode analsysis  *)

val x =
let 
  val N = 500
  val I = 1000

  type int_pair = int * int
  val maxint = 2000
  val zero = (0,0)
  fun is_zero(0,0) = true
    | is_zero _ = false

  fun sub (m,n) =
      if n=0 then (m-1, maxint)
      else (m, n-1)

  fun loop (x as (m,n)) =
      if is_zero x then x
      else loop(sub x)

  fun loop' p = (loop p; "\ndone\n")

in
  print "\nlooping...\n";
  print (loop'(maxint,maxint))
end
