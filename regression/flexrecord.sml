(* flexrecord.sml *)

(* Checks type inference for flexible records. *)

(* flexrecord1 *)
fun f(r as {...}) =
let
    fun g() = r
in
    [r, {a=1}]
end;
(* flexrecord1 *)

(* flexrecord2 *)
val _ =
   let
      val g = #foo
      val _ = g {foo = 13}
      val _ = g {foo = "yes"}
   in
      ()
   end
(* flexrecord2 *)

(* flexrecord3 *)
val _ =
   let
      val g = #foo
      val _ = g {foo = 13, goo = 1.0}
      val _ = g {foo = "yes", goo = 1.0}
   in
      ()
   end
(* flexrecord3 *)

(* flexrecord4 *)
val _ =
   let
      val g = #foo
      val _ = g {foo = 13, goo = 1.0}
      val _ = g {foo = "yes", goo = false}
   in
      ()
   end
(* flexrecord4 *)

(* flexrecord5 *)
val _ =
   let
      val f = #foo
      val g = fn h => fn y => h (f y)
      val h = fn x => f x
      val _ = f {foo=0, bar=1}
   in
      ()
   end
(* flexrecord5 *)

(* flexrecord6 *)
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
(* flexrecord6 *)

(* flexrecord7 *)
val _ =
   let
      fun f r = { a = #a r, b = #b r }
      val _ = f { a = 0.0, b = 0.0 }
   in
      ()
   end
(* flexrecord7 *)

(* flexrecord8 *)
val f = #foo
val g = (fn x => x) f
val _ = f {foo=0, bar=1}
(* flexrecord8 *)

(* flexrecord9 *)
structure S =
   struct
      val f = #foo
   end

val _ = S.f {foo=1, goo=2}
(* flexrecord9 *)
