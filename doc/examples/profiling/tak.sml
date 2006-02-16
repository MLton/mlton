structure Tak =
   struct
      fun tak1 (x, y, z) =
         let
            fun tak2 (x, y, z) =
               if y >= x
                  then z
               else
                  tak1 (tak2 (x - 1, y, z),
                        tak2 (y - 1, z, x),
                        tak2 (z - 1, x, y))
         in
            if y >= x
               then z
            else
               tak1 (tak2 (x - 1, y, z),
                     tak2 (y - 1, z, x),
                     tak2 (z - 1, x, y))
         end
   end

val rec f =
   fn 0 => ()
    | ~1 => print "this branch is not taken\n"
    | n => (Tak.tak1 (18, 12, 6) ; f (n-1))

val _ = f 5000

fun uncalled () = ()
