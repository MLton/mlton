(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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

functor TestFormat (S: FORMAT): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestFormat", fn () => 
    "abc" = format (lit "abc")
    andalso "abc" = format string "abc"
    andalso "abc" = format (lit "a" o lit "b" o lit "c")
    andalso "abc" = format (string o string o string) "a" "b" "c"
    andalso "[a, b, c]" = format (list string) ["a", "b", "c"]
    andalso "[1, 2, 3]" = format (list int) [1, 2, 3])

end
