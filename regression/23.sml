fun f l =
   case l of
      [] => f l
    | _ :: l => f l
   
val _ = f [13]
