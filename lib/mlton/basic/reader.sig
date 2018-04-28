(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature READER =
   sig
      type ('a, 's) t = 's -> ('a * 's) option

      val all: ('a, 's) t -> ('a list, 's) t
      val char: (char, 'a) t * char -> (unit, 'a) t
      val firstN: ('a, 's) t * Int.t -> ('a list, 's) t 
      val readFromString:
         ((char, Int.t) t -> ('a, Int.t) t) * string -> 'a option
      val map: ('a, 's) t * ('a -> 'b) -> ('b, 's) t
      val mapFail: ('a, 's) t * ('a -> 'b option) -> ('b, 's) t
      val or: ('a, 's) t list -> ('a, 's) t
      val seq2: ('a, 's) t * ('b, 's) t -> ('a * 'b, 's) t
      val seq3: ('a, 's) t * ('b, 's) t * ('c, 's) t -> ('a * 'b * 'c, 's) t
      val seq4:
         ('a, 's) t * ('b, 's) t * ('c, 's) t * ('d, 's) t
         -> ('a * 'b * 'c * 'd, 's) t
      val seq5:
         ('a, 's) t * ('b, 's) t * ('c, 's) t * ('d, 's)t * ('e,'s) t
         -> ('a * 'b * 'c * 'd * 'e, 's) t
      val stringOfLength: (char, 'a) t * Int.t -> (string, 'a) t
   end
