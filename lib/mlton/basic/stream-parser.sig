signature STREAM_PARSER_STRUCTS = 
    sig
      structure Stream: STREAM
      structure File: FILE
    end 

signature STREAM_PARSER = 
    sig
       include STREAM_PARSER_STRUCTS

       type 'b t

       type location = {line: int, column: int}
       type info = File.t
       val parse: 'b t * File.t * char Stream.t -> 'b
       exception Parse of string

       val pure: 'b -> 'b t
       (*
        * infixr 1 <|>
        * infixr 2 <&>
        * infix  3 <*> <* *> 
        * infixr 4 <$> <$$> <$$$> <$
        *)
       val <*> : ('b -> 'c) t * 'b t -> 'c t
       val <$> : ('b -> 'c) * 'b t -> 'c t
       val <$ : 'c * 'b t -> 'c t
       val <$$> : ('b * 'c -> 'd) * ('b t * 'c t) -> 'd t
       val <$$$> : ('b * 'c * 'd -> 'e) * ('b t * 'c t * 'd t) -> 'e t
       val <* : 'b t * 'c t -> 'b t
       val *> : 'b t * 'c t -> 'c t
       val <|> : 'b t * 'b t -> 'b t 
       val <&> : 'b t * 'b t -> 'b t 

       (* delays a stream lazily, for recursive combinations *)
       val delay: (unit -> 'b t) -> 'b t
       (* run a parser but consume no input *)
       val peek: 'b t -> 'b t
       (* gets the next char of input *)
       val next: char t
       (* suceeds if the parser suceeded with an input that passed the predicate*) 
       val sat: 'b t * ('b -> bool) -> 'b t
       (* succeeds if and only if its argument fails to parse*)
       val failing: 'b t -> unit t
       (* succeeds if the first parser succeeds and the second fails *)
       (* the second parser will never consume any input *)
       val notFollowedBy: 'b t * 'c t -> 'b t
       (* succeeds if any of the parsers succeed, effectively folding with <|> *)
       val any: 'b t list -> 'b t 
       (* succeeds for each time the parser succeeds in succession *)
       val many: 'b t -> 'b list t
       (* as many, but one or more, rather than zero or more times *)
       val many1: 'b t -> 'b list t
       (* as many, but an instance of the second parser separates each instance*)
       val sepBy: 'b t * 'c t -> 'b list t
       (* as many1, but an instance of the second parser separates each instance*)
       val sepBy1: 'b t * 'c t -> 'b list t
       (* suceed with SOME if the parser succeeded and NONE otherwise *)
       val optional: 'b t -> 'b option t
       (* fail with a specified error message *)
       val fail: string -> 'b t
       (* matches the given character *)
       val char: char -> char t
       (* matches the given string *)
       val string: string -> string t

    end
