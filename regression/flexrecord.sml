(* flexrecord.sml *)

(* Checks type inference for flexible records. *)

fun f(r as {...}) =
let
    fun g() = r
in
    [r, {a=1}]
end;
