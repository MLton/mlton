signature STREAM_PARSER_STRUCTS = 
    sig
      structure Stream: STREAM
      structure File: FILE
    end 

signature STREAM_PARSER = 
   sig
      include STREAM_PARSER_STRUCTS

      type 'b t
      type state

      type location = {line: int, column: int}
      type info = string
      val parse: 'b t * char Stream.t -> 'b
      val parseWithFile: 'b t * File.t * char Stream.t -> 'b

      val pure: 'b -> 'b t
      (*
       * infix 1 <|> >>=
       * infix 2 <&>
       * infix  3 <*> <* *> 
       * infixr 4 <$> <$$> <$$$> <$
       *)
      val >>= : 'a t * ('a -> 'b t) -> 'b t
      val <*> : ('b -> 'c) t * 'b t -> 'c t
      val <$> : ('b -> 'c) * 'b t -> 'c t
      val <$ : 'c * 'b t -> 'c t
      val <$$> : ('b * 'c -> 'd) * ('b t * 'c t) -> 'd t
      val <$$$> : ('b * 'c * 'd -> 'e) * ('b t * 'c t * 'd t) -> 'e t
      val <* : 'b t * 'c t -> 'b t
      val *> : 'b t * 'c t -> 'c t
      val <|> : 'b t * 'b t -> 'b t 
      val <&> : 'b t * 'b t -> 'b t 
      structure Ops : sig
         val >>= : 'a t * ('a -> 'b t) -> 'b t
         val <*> : ('b -> 'c) t * 'b t -> 'c t
         val <$> : ('b -> 'c) * 'b t -> 'c t
         val <$ : 'c * 'b t -> 'c t
         val <$$> : ('b * 'c -> 'd) * ('b t * 'c t) -> 'd t
         val <$$$> : ('b * 'c * 'd -> 'e) * ('b t * 'c t * 'd t) -> 'e t
         val <* : 'b t * 'c t -> 'b t
         val *> : 'b t * 'c t -> 'c t
         val <|> : 'b t * 'b t -> 'b t 
         val <&> : 'b t * 'b t -> 'b t 
      end


      (* succeeds if any of the parsers succeed, effectively folding with <|> *)
      val any: 'b t list -> 'b t 
      (* matches the given character *)
      val char: char -> char t
      (* if the parser fails, it will fail as a cut *)
      val cut: 'b t -> 'b t 
      (* delays a stream lazily, for recursive combinations *)
      val delay: (unit -> 'b t) -> 'b t
      (* succeeds if all of the parsers succeed and puts them together in a list*)
      val each: 'b t list -> 'b list t 
      (* fail with a specified error message *)
      val fail: string -> 'b t
      (* as fail, but also cuts at the next choice point *)
      val failCut: string -> 'b t
      (* succeeds if and only if its argument fails to parse*)
      val failing: 'b t -> unit t
      (* return the parser representation of a given reader *)
      val fromReader: (state -> ('b * state) option)-> 'b t
      (* returns the info for the parse, usually a filename *)
      val info: info t
      (* returns the current source location of the parser *)
      val location: location t
      (* succeeds for each time the parser succeeds in succession *)
      val many: 'b t -> 'b list t
      (* as many, but one or more, rather than zero or more times *)
      val many1: 'b t -> 'b list t
      (* gets the next char of input *)
      val next: char t
      (* succeeds if the first parser succeeds and the second fails 
       * the second parser will never consume any input *)
      val notFollowedBy: 'b t * 'c t -> 'b t
      (* succeeds with SOME if the parser succeeded and NONE otherwise *)
      val optional: 'b t -> 'b option t
      (* run a parser but consume no input *)
      val peek: 'b t -> 'b t
      (* succeeds if the parser succeeded with an input that passed the predicate*) 
      val sat: 'b t * ('b -> bool) -> 'b t
      (* as many, but an instance of the second parser separates each instance*)
      val sepBy: 'b t * 'c t -> 'b list t
      (* as many1, but an instance of the second parser separates each instance*)
      val sepBy1: 'b t * 'c t -> 'b list t
      (* matches the given string *)
      val string: string -> string t
      (* returns a reader representation of the parser *)
      val toReader: 'b t -> state -> ('b * state) option

   end
