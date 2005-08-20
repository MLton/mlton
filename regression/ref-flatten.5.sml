datatype t =
   A of int ref * int
 | B

val n = 100

val a = Array.tabulate (n, fn i =>
                        case i mod 3 of
                           0 => B
                         | 1 => A (ref 13, 14)
                         | 2 => A (ref 15, 16))

datatype t =
   A' of int ref * int
 | B'

val a' =
   Array.tabulate (n, fn i =>
                   case Array.sub (a, i) of
                      B => B'
                    | A (r, n) => A' (r, n + 1))

val _ = Array.app (fn A (r, n) => r := 17 + n + !r  | B => ()) a

val _ =
   case Array.sub (a', 1) of
      A' (r, n) => print (concat [Int.toString (!r + n), "\n"])
    | B' => ()
