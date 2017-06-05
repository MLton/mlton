signature STREAM_PARSER_STRUCTS = 
    sig
      structure Stream: STREAM
    end 

signature STREAM_PARSER = 
    sig
      include STREAM_PARSER_STRUCTS

      type 'b t

      val parse: 'b t * char Stream.t -> 'b
      exception Parse of string

      val pure: 'b -> 'b t
      (*
       * infix 2 <|>
       * infix 3 <*> <* *> <$
       * infixr 4 <$> <$$> <$$$> 
       *)
      val <*> : ('b -> 'c) t * 'b t -> 'c t
      val <$> : ('b -> 'c) * 'b t -> 'c t
      val <$ : 'c * 'b t -> 'c t
      val <$$> : ('b * 'c -> 'd) * ('b t * 'c t) -> 'd t
      val <$$$> : ('b * 'c * 'd -> 'e) * ('b t * 'c t * 'd t) -> 'e t
      val <* : 'b t * 'c t -> 'b t
      val *> : 'b t * 'c t -> 'c t
      val <|> : 'b t * 'b t -> 'b t 
      
      val next: char t
      val sat: 'b t * ('b -> bool) -> 'b t
      val any: 'b t list -> 'b t 
      val many: 'b t -> 'b list t
      val optional: 'b t -> 'b option t
      val fail: string -> 'b t
      
      val char: char -> char t
      val string: string -> string t

    end
