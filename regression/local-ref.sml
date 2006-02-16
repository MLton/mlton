
local
   val c = ref 0
in
   fun fib n
     = let
          val _ = if !c mod 5 = 0
                    then print (concat [Int.toString (!c),
                                        "th invocation of fib\n"])
                    else ()
          val _ = c := !c + 1
       in
          case n
            of 0 => 1
             | 1 => 1
             | n => (fib (n-1)) + (fib (n-2))
       end
end

val n = fib 5 

val _ = print (concat ["fib(5) = ", Int.toString n, "\n"])
