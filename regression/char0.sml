fun f c =
   case c of
      #"a" => ()
    | _ => raise Fail "bug"

val _ = f #"a"
val _ = f #"a"

       
