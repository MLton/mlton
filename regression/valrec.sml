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
val _ =
   case x 1 of
      1 => ()
    | _ => raise Fail "bug"

val a = fn x => x
val a = 1
and rec b = fn x => a(b(c(d(e(f(g x))))))
and c : 'a -> 'a as d : 'a -> 'a = fn x => x
and rec e as f as g = fn x => x
and h : 'b -> 'b : 'b -> 'b = fn x => x;
