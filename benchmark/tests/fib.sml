val rec fib =
   fn 0 => 0
    | 1 => 1
    | n => fib (n - 1) + fib (n - 2)

structure Main =
   struct
      fun doit () =
	 if 701408733 <> fib 44
	    then raise Fail "bug"
	 else ()
   end
