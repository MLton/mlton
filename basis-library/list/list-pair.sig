signature LIST_PAIR =
   sig
      val all: ('a * 'b -> bool) -> 'a list * 'b list -> bool 
      val app: ('a * 'b -> unit) -> 'a list * 'b list -> unit 
      val exists: ('a * 'b -> bool) -> 'a list * 'b list -> bool 
      val foldl: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c 
      val foldr: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c 
      val map: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list 
      val unzip: ('a * 'b) list -> 'a list * 'b list
      val zip: 'a list * 'b list -> ('a * 'b) list 
   end
