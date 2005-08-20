fun f r =
   case #1 (!r) of
      3 => r := (5, 6)
    | _ => f (ref (7, 8))
      
val r = ref (1, 2)
val _ = r := (3, 4)
val _ = f r
val _ = print (concat [Int.toString (#1 (!r)), " ",
                       Int.toString (#2 (!r)), "\n"])
