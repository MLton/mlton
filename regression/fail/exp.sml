val _ = 1 andalso true

val _ = true andalso 1

val _ = 13 14

val _ = (op +) (1, true)

val _ =
   case "foo" of
      13 => 14

val _ = 13: bool

val _ = + 1 2

val _ = 1 handle _ => "foo"

val _ = () handle _: int => ()

val _ = if "foo" then () else ()

val _ = if true then 1 else "foo"

val _ = [1, 2, "foo"]

val _ = 1 orelse true

val _ = true orelse 1

val _ = raise 13

val _ = "foo" + "bar"

val _ = while "foo" do ()

val _ =
   fn _: int => ()
    | _: bool => ()

val _ =
   fn 1 => "foo"
    | 2 => true
      
