(* Fails because the signature means that it's treated as 3 topdecs. *)
val x = ref nil
signature S = sig end
val _ = 1 :: !x
