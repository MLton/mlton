(* No warning according to page 28 of defn *)
val x::l = [] 

structure S =
   struct
      val x :: l = []
   end

val _ =
   let
      val x::l = []  (* But this should generate a warning. *)
   in
      ()
   end

val _ =
   case 13 of
      1 => ()

val _ =
   case 13 of
      1 => 2
    | 1 => 3
    | 1 => 4

val NONE = NONE

fun f _ 1 = ()
  | f 1 _ = ()

fun f _ _ = ()
  | f 1 2 = ()
  | f 3 4 = ()

val _ = fn 13 => ()

val _ = () handle _ => () | x => ()
