signature LIST_GLOBAL =
   sig
      datatype list = datatype list

      exception Empty

      val @ : 'a list * 'a list -> 'a list 
      val app: ('a -> unit) -> 'a list -> unit 
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b 
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b 
      val hd: 'a list -> 'a 
      val length: 'a list -> int 
      val map: ('a -> 'b) -> 'a list -> 'b list 
      val null: 'a list -> bool 
      val rev: 'a list -> 'a list 
      val tl: 'a list -> 'a list 
   end

signature LIST =
   sig
      include LIST_GLOBAL

      val all: ('a -> bool) -> 'a list -> bool 
      val collate: ('a * 'a -> order) -> 'a list * 'a list -> order
      val concat: 'a list list -> 'a list 
      val drop: 'a list * int -> 'a list 
      val exists: ('a -> bool) -> 'a list -> bool 
      val filter: ('a -> bool) -> 'a list -> 'a list 
      val find: ('a -> bool) -> 'a list -> 'a option 
      val getItem: 'a list -> ('a * 'a list) option 
      val last: 'a list -> 'a 
      val mapPartial: ('a -> 'b option) -> 'a list -> 'b list 
      val nth: 'a list * int -> 'a 
      val partition: ('a -> bool) -> 'a list -> 'a list * 'a list
      val revAppend: 'a list * 'a list -> 'a list 
      val tabulate: int * (int -> 'a) -> 'a list 
      val take: 'a list * int -> 'a list 
   end
