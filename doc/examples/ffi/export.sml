val e = _export "f": (int * real * char -> char) -> unit;
val _ = e (fn (i, r, _) =>
           (print (concat ["i = ", Int.toString i,
                           "  r = ", Real.toString r, "\n"])
            ; #"g"))
val g = _import "g": unit -> unit;
val _ = g ()
val _ = g ()

val e = _export "f2": (Word8.word -> word array) -> unit;
val _ = e (fn w =>
           Array.tabulate (10, fn _ => Word.fromLargeWord (Word8.toLargeWord w)))
val g2 = _import "g2": unit -> word array;
val a = g2 ()
val _ = print (concat ["0wx", Word.toString (Array.sub (a, 0)), "\n"])

val e = _export "f3": (unit -> unit) -> unit;
val _ = e (fn () => print "hello\n");
val g3 = _import "g3": unit -> unit;
val _ = g3 ()

(* This example demonstrates mutual recursion between C and SML. *)
val e = _export "f4": (int -> unit) -> unit;
val g4 = _import "g4": int -> unit;
val _ = e (fn i => if i = 0 then () else g4 (i - 1))
val _ = g4 13

val (_, zzzSet) = _symbol "zzz" alloc: (unit -> int) * (int -> unit);
val () = zzzSet 42
val g5 = _import "g5": unit -> unit;
val _ = g5 ()

val _ = print "success\n"

