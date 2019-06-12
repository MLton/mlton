(* Copyright (C) 2017,2019 Jason Carr, Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PARSE =
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

      val parseFile: 'a t * File.t -> 'a Result.t
      val parseStream: 'a t * char Stream.t -> 'a Result.t
      val parseString: 'a t * string -> 'a Result.t

      (*
       * infix 1 <|> >>=
       * infix  3 <*> <* *>
       * infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?> 
       *)
      val >>= : 'a t * ('a -> 'b t) -> 'b t
      val <*> : ('a -> 'b) t * 'a t -> 'b t
      val <$> : ('a -> 'b) * 'a t -> 'b t
      (* replace the result of a parser with a constant *)
      val <$ : 'b * 'a t -> 'b t
      (* map over pairs of parsers, joining their results together *)
      val <$$> : ('a * 'b -> 'c) * ('a t * 'b t) -> 'c t
      val <$$$> : ('a * 'b * 'c -> 'd) * ('a t * 'b t * 'c t) -> 'd t
      val <$$$$> : ('a * 'b * 'c * 'd -> 'e) * ('a t * 'b t * 'c t * 'd t) -> 'e t
      val <$?> : ('a -> 'b option) * 'a t -> 'b t
      (* match both parsers, and discard the right or left result respectively *)
      val <* : 'a t * 'b t -> 'a t
      val *> : 'a t * 'b t -> 'b t
      (* try both parsers, take the result of the first success *)
      val <|> : 'a t * 'a t -> 'a t
      (* try both parsers, fail if either fails, or take the last success *)
      structure Ops : sig
         val >>= : 'a t * ('a -> 'b t) -> 'b t
         val <*> : ('a -> 'b) t * 'a t -> 'b t
         val <$> : ('a -> 'b) * 'a t -> 'b t
         val <$ : 'b * 'a t -> 'b t
         val <$$> : ('a * 'b -> 'c) * ('a t * 'b t) -> 'c t
         val <$$$> : ('a * 'b * 'c -> 'd) * ('a t * 'b t * 'c t) -> 'd t
         val <$$$$> : ('a * 'b * 'c * 'd -> 'e) * ('a t * 'b t * 'c t * 'd t) -> 'e t
         val <$?> : ('a -> 'b option) * 'a t -> 'b t
         val <* : 'a t * 'b t -> 'a t
         val *> : 'a t * 'b t -> 'b t
         val <|> : 'a t * 'a t -> 'a t
      end

      val pure: 'a -> 'a t
      (* succeeds if any of the parsers succeed *)
      val any: 'a t list -> 'a t
      (* matches the given character *)
      val char: char -> char t
      (* delays a stream lazily, for recursive combinations *)
      val delay: (unit -> 'a t) -> 'a t
      (* succeeds if all of the parsers succeed and combines their results *)
      val each: 'a t list -> 'a list t
      (* fail with a specified error message *)
      val fail: string -> 'a t
      (* succeeds if and only if its argument fails to parse *)
      val failing: 'a t -> unit t
      (* return the parser representation of a given reader *)
      val fromReader: (State.t -> ('a * State.t) option) -> 'a t
      (* return the parser corresponding to a scanner *)
      val fromScan: ((char, State.t) StringCvt.reader -> ('a, State.t) StringCvt.reader) -> 'a t
      (* returns the current source location of the parser *)
      val location: Location.t t
      (* succeeds for each time the parser succeeds in succession *)
      val many: 'a t -> 'a list t
      (* as many, but one or more, rather than zero or more times *)
      val many1: 'a t -> 'a list t
      (* as many, but with failures of the second parser before each parse from the first *)
      val manyFailing: ('a t * 'b t) -> 'a list t
      (* manyFailing, specialized to the case that p is "next" *)
      val manyCharsFailing: 'a t -> char list t
      (* sets the parser name, to build better error messages *)
      val named: string * 'a t -> 'a t
      (* gets the next char of input *)
      val next: char t
      (* as sat, specialized to "next" *)
      val nextSat: (char -> bool) -> char t
      (* succeeds if the first parser succeeds and the second fails
       * the second parser will never consume any input *)
      val notFollowedBy: 'a t * 'b t -> 'a t
      (* succeeds with SOME if the parser succeeded and NONE otherwise *)
      val optional: 'a t -> 'a option t
      (* parse an integer, as Integer.scan StringCVT.DEC *)
      val int: int t
      (* run a parser but consume no input *)
      val peek: 'a t -> 'a t
      (* succeeds if the parser succeeded with an input that passed the predicate *)
      val sat: 'a t * ('a -> bool) -> 'a t
      (* as many, but an instance of the second parser separates each instance *)
      val sepBy: 'a t * 'b t -> 'a list t
      (* as many1, but an instance of the second parser separates each instance *)
      val sepBy1: 'a t * 'b t -> 'a list t
      val space: char t
      val spaces: char list t
      val str: string -> string t
      (* returns a reader representation of the parser *)
      val toReader: 'a t -> State.t -> ('a * State.t) option

      (* The following parsers always (and only) consume spaces before
       * performing a `char` or `str`.
       *)
      val bool: bool t
      val cbrack: 'a t -> 'a t
      (* parse first field of a record (not preceded by `,`) *)
      val ffield: string * 'a t -> 'a t
      (* parse SML-style keyword (not followed by alphanum id character) *)
      val kw: string -> unit t
      (* parse `List.layout` (not `Layout.list`) *)
      val list: 'a t -> 'a list t
      val listOpt: 'a t -> 'a list t
      (* a Standard ML comment, with support for nesting *)
      val mlComment: char t
      (* may contain any number of spaces or comments *)
      val mlSpaces: char list t
      (* parse next field of a record (preceded by `,`) *)
      val nfield: string * 'a t -> 'a t
      (* parse `Option.layout` *)
      val option: 'a t -> 'a option t
      val paren: 'a t -> 'a t
      (* parse SML-style symbol (not followed by symbolic id character) *)
      val sym: string -> unit t
      (* parse `Vector.layout` (not `Layout.vector`) *)
      val vector: 'a t -> 'a vector t
      val vectorOpt: 'a t -> 'a vector t
   end
