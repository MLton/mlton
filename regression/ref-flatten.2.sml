datatype t =
   A of Word16.word ref * Word16.word ref * int list
 | B

val a = Array.tabulate (100, fn i =>
                        let
                           val l = [100 + i, 2, 3]
                        in
                           case i mod 2 of
                              0 => A (ref 0w13, ref 0w123, l)
                            | 1 => B
                        end)

val _ =
   Array.app
   (fn B => ()
     | A (r, r', l) => (r := !r + Word16.fromLarge (LargeWord.fromInt (hd l))
                        ; r' := !r' + !r))
   a

val A (w, w', _) = Array.sub (a, 0)

val _ = print (concat [Word16.toString (!w), " ",
                       Word16.toString (!w'), "\n"])
         
                           
                                      
