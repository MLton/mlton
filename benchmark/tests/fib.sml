val rec fib =
   fn 0 => 0
    | 1 => 1
    | n => fib (n - 1) + fib (n - 2)

structure Main =
   struct
      fun doit n =
         if n = 0
            then ()
         else let
                 val _ = if 165580141 <> fib 41
                            then raise Fail "bug"
                         else ()
              in
                 doit (n - 1)
              end
   end
