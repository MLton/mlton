(*
 * This example tests that a variable type containing unknown types is not
 * mistakenly generalized when matching a signature.
 *)
structure S:
   sig
      val f: 'a option -> 'a option
   end =
   struct
      val f =
         let
            val r = ref NONE
         in
            fn z => (!r before (r := z))
         end
   end

val _ = S.f (SOME 13)

val _ =
   case S.f (SOME (fn z => z)) of
      NONE => 15
    | SOME f => f 17
