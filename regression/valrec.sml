val rec ((((((((x)))))))) = fn () => ()

val _ = x ()

local
   val 'a rec f = fn () => ()
in
end

local
   val x = "Hello, world!\n"
      
   val x = "BUG\n"
   and rec f = fn () => print x
in
   val _ = f ()
end

local
   val x = "BUG\n"

   val x = "Hello, world!\n"
   and rec f = fn y => print y
in
   val _ = f x
end

local
   val rec rec f = fn () => ()
in
end

local
   val rec f = fn () => ()
   and rec g = fn () => ()
in
end

(* valrec.sml *)

(* Checks parsing, scoping, typing and dynamic semantics of "val rec". *)

fun x x = x
val 1 = x 1;

val a = fn x => x
val a = 1
and rec b = fn x => a(b(c(d(e(f(g x))))))
and c : 'a -> 'a as d : 'a -> 'a = fn x => x
and rec e as f as g = fn x => x
and h : 'b -> 'b : 'b -> 'b = fn x => x;

val x =
let
    val rec LESS = fn x => x	(* will raise Bind *)
    and NONE as SOME = fn x => x
    val SOME = 1;
in
    raise Fail "should not get here!"
end handle Bind => ();
