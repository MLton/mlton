signature LIST_PAIR =
   sig
      exception UnequalLengths
      val zip: 'a list * 'b list -> ('a * 'b) list 
      val zipEq: 'a list * 'b list -> ('a * 'b) list 
      val unzip: ('a * 'b) list -> 'a list * 'b list
      val app: ('a * 'b -> unit) -> 'a list * 'b list -> unit 
      val appEq: ('a * 'b -> unit) -> 'a list * 'b list -> unit 
      val map: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list 
      val mapEq: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list 
      val foldl: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c 
      val foldr: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c 
      val foldlEq: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c 
      val foldrEq: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c 
      val all: ('a * 'b -> bool) -> 'a list * 'b list -> bool 
      val exists: ('a * 'b -> bool) -> 'a list * 'b list -> bool 
      val allEq: ('a * 'b -> bool) -> 'a list * 'b list -> bool 
   end
