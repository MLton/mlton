(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * This is based on
 *   Functional Unparsing
 *   BRICS Technical Report RS 98-12
 *   Olivier Danvy, May 1998
 *)

structure Format:> FORMAT =
struct

type ('a, 'b) t = (string list -> 'a) * string list -> 'b

val new: ('b -> string) -> ('a, 'b -> 'a) t =
   fn toString => fn (k, ss) => fn b => k (toString b :: ss)

val lit: string -> ('a, 'a) t = fn s => fn (k, ss) => k (s :: ss)

val eol: ('a, 'a) t = fn z => lit "\n" z

(* val concat =
 *    Trace.trace ("Format.concat", List.layout String.layout, String.layout) concat
 *)

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
