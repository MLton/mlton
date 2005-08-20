(*
 * This is based on
 *   Functional Unparsing
 *   BRICS Technical Report RS 98-12
 *   Olivier Danvy, May 1998
 *)

signature FORMAT =
   sig
      type ('a, 'b) t

      val eol: ('a, 'a) t
      val format: (string, 'a) t -> 'a
      val int: ('a, int -> 'a) t
      val list: ('a, 'b -> 'a) t -> ('a, 'b list -> 'a) t
      val lit: string -> ('a, 'a) t 
      val new: ('b -> string) -> ('a, 'b -> 'a) t
      val o: ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t
      val string: ('a, string -> 'a) t
   end

structure Format:> FORMAT =
struct

type ('a, 'b) t = (string list -> 'a) * string list -> 'b

val new: ('b -> string) -> ('a, 'b -> 'a) t =
   fn toString => fn (k, ss) => fn b => k (toString b :: ss)

val lit: string -> ('a, 'a) t = fn s => fn (k, ss) => k (s :: ss)

val eol: ('a, 'a) t = fn z => lit "\n" z
   
val format: (string, 'a) t -> 'a = fn f => f (concat o rev, [])
   
val int: ('a, int -> 'a) t = fn z => new Int.toString z
   
val list: ('a, 'b -> 'a) t -> ('a, 'b list -> 'a) t =
   fn f => fn (k, ss) =>
   fn [] => k ("[]" :: ss)
    | x :: xs =>
         let
            fun loop xs ss =
               case xs of
                  [] => k ("]" :: ss)
                | x :: xs => f (loop xs, ", " :: ss) x
         in f (loop xs, "[" :: ss) x
         end

val op o: ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t =
   fn (f, g) => fn (k, ss) => f (fn ss => g (k, ss), ss)

val string: ('a, string -> 'a) t = fn z => new (fn s => s) z
   
end

open Format

val _ =
   if
      "abc" = format (lit "abc")
      andalso "abc" = format string "abc"
      andalso "abc" = format (lit "a" o lit "b" o lit "c")
      andalso "abc" = format (string o string o string) "a" "b" "c"
      andalso "[a, b, c]" = format (list string) ["a", "b", "c"]
      andalso "[1, 2, 3]" = format (list int) [1, 2, 3]
      then ()
   else raise Fail "bug"
