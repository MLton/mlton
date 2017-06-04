signature STREAM_TRANSFORMER_STRUCTS = 
    sig
      structure Stream: STREAM
    end 

signature STREAM_TRANSFORMER = 
    sig
      include STREAM_TRANSFORMER_STRUCTS

      type ('a, 'b) t

      val parse: ('a, 'b) t * 'a Stream.t -> 'b
      exception Parse of string

      val pure: 'b -> ('a, 'b) t
      (*
       * infix 3 <*> <* *> <$
       * infixr 4 <$> <$$> <$$$>
       *)
      val <*> : ('a, 'b -> 'c) t * ('a, 'b) t -> ('a, 'c) t
      val <$> : ('b -> 'c) * ('a, 'b) t -> ('a, 'c) t
      val <$ : 'c * ('a, 'b) t -> ('a, 'c) t
      val <$$> : ('b * 'c -> 'd) * (('a, 'b) t * ('a, 'c) t) -> ('a, 'd) t
      val <$$$> : ('b * 'c * 'd -> 'e) * (('a, 'b) t * ('a, 'c) t * ('a, 'd) t) -> ('a, 'e) t
      val <* : ('a, 'b) t * ('a, 'c) t -> ('a, 'b) t
      val *> : ('a, 'b) t * ('a, 'c) t -> ('a, 'c) t
      
      val one: ('a, 'a) t
      val any: ('a, 'b) t list -> ('a, 'b) t 
      val many: ('a, 'b) t -> ('a, 'b list) t
      val optional: ('a, 'b) t -> ('a, 'b option) t
      
      val equals: ''a -> (''a, ''a) t
      val string: string -> (char, string) t
    end
