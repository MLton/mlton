(* flexrecord.sml *)

(* Checks type inference for flexible records. *)

fun f(r as {...}) =
let
    fun g() = r
in
    [r, {a=1}]
end;

val _ =
   fn x =>
   let
      val _: string = #1 x
      fun id z = z
      fun g () =
	 let
	    val (_, a) = x
	 in a
	 end
   in
      g ()
   end
