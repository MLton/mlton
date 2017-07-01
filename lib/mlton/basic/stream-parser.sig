(* Copyright (C) 2017 Jason Carr.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STREAM_PARSER = 
   sig
      type 'a t
      structure State:
         sig
            type t
         end
      structure Location:
         sig
            type t = {line: int, column: int}
         end
      structure Info:
         sig
            type t = string
         end

      val parse: 'a t * char Stream.t -> 'a
      val parseWithFile: 'a t * File.t * char Stream.t -> 'a

      (*
       * infix 1 <|> >>=
       * infix 2 <&>
       * infix  3 <*> <* *> 
       * infixr 4 <$> <$$> <$$$> <$
       *)
      val >>= : 'a t * ('a -> 'b t) -> 'b t
      val <*> : ('a -> 'b) t * 'a t -> 'b t
      val <$> : ('a -> 'b) * 'a t -> 'b t
      (* replace the result of a parser witih a constant *)
      val <$ : 'b * 'a t -> 'b t
      (* map over pairs of parsers, joining their results together *)
      val <$$> : ('a * 'b -> 'c) * ('a t * 'b t) -> 'c t
      val <$$$> : ('a * 'b * 'c -> 'd) * ('a t * 'b t * 'c t) -> 'd t
      (* match both parsers, and discard the right or left result respectively *)
      val <* : 'a t * 'b t -> 'a t
      val *> : 'a t * 'b t -> 'b t
      (* try both parsers, take the result of the first success *)
      val <|> : 'a t * 'a t -> 'a t 
      (* try both parsers, fail if either fails, or take the last success *)
      val <&> : 'a t * 'a t -> 'a t 
      structure Ops : sig
         val >>= : 'a t * ('a -> 'b t) -> 'b t
         val <*> : ('a -> 'b) t * 'a t -> 'b t
         val <$> : ('a -> 'b) * 'a t -> 'b t
         val <$ : 'b * 'a t -> 'b t
         val <$$> : ('a * 'b -> 'c) * ('a t * 'b t) -> 'c t
         val <$$$> : ('a * 'b * 'c -> 'd) * ('a t * 'b t * 'c t) -> 'd t
         val <* : 'a t * 'b t -> 'a t
         val *> : 'a t * 'b t -> 'b t
         val <|> : 'a t * 'a t -> 'a t 
         val <&> : 'a t * 'a t -> 'a t 
      end

      val pure: 'a -> 'a t
      (* succeeds if any of the parsers succeed *)
      val any: 'a t list -> 'a t 
      (* matches the given character *)
      val char: char -> char t
      (* composes two parsers in turn, the characters used for the second come
       * from the first *)
      val compose : char t * 'a t -> 'a t
      (* if the parser fails, it will fail as a cut *)
      val cut: 'a t -> 'a t 
      (* delays a stream lazily, for recursive combinations *)
      val delay: (unit -> 'a t) -> 'a t
      (* succeeds if all of the parsers succeed and combines their results *)
      val each: 'a t list -> 'a list t 
      (* fail with a specified error message *)
      val fail: string -> 'a t
      (* as fail, but also cuts at the next choice point *)
      val failCut: string -> 'a t
      (* succeeds if and only if its argument fails to parse*)
      val failing: 'a t -> unit t
      (* return the parser representation of a given reader *)
      val fromReader: (State.t -> ('a * State.t) option)-> 'a t
      (* returns the info for the parse, usually a filename *)
      val info: Info.t t
      (* returns the current source location of the parser *)
      val location: Location.t t
      (* succeeds for each time the parser succeeds in succession *)
      val many: 'a t -> 'a list t
      (* as many, but one or more, rather than zero or more times *)
      val many1: 'a t -> 'a list t
      (* as many, but with failures of the second parser before each parse from the first *)
      val manyFailing: ('a t * 'b t) -> 'a list t
      (* manyFailing, specialized to the case that p is next *)
      val manyCharsFailing: 'a t -> char list t
      (* gets the next char of input *)
      val next: char t
      (* succeeds if the first parser succeeds and the second fails 
       * the second parser will never consume any input *)
      val notFollowedBy: 'a t * 'b t -> 'a t
      (* succeeds with SOME if the parser succeeded and NONE otherwise *)
      val optional: 'a t -> 'a option t
      (* run a parser but consume no input *)
      val peek: 'a t -> 'a t
      (* succeeds if the parser succeeded with an input that passed the predicate*) 
      val sat: 'a t * ('a -> bool) -> 'a t
      (* as many, but an instance of the second parser separates each instance*)
      val sepBy: 'a t * 'b t -> 'a list t
      (* as many1, but an instance of the second parser separates each instance*)
      val sepBy1: 'a t * 'b t -> 'a list t
      (* matches the given string *)
      val string: string -> string t
      (* returns a reader representation of the parser *)
      val toReader: 'a t -> State.t -> ('a * State.t) option

   end
