type int = Pervasive.Int.int
      
signature FOLD_STRUCTS = 
   sig
      type 'a t
      type 'a elt

      val fold: 'a t * 'b * ('a elt * 'b -> 'b) -> 'b
   end

signature FOLD = 
   sig
      include FOLD_STRUCTS

      val exists: 'a t * ('a elt -> bool) -> bool
      val first: 'a t -> 'a elt
      val foldi: 'a t * 'b * (int * 'a elt * 'b -> 'b) -> 'b
      val forall: 'a t * ('a elt -> bool) -> bool
      val foralli: 'a t * (int * 'a elt -> bool) -> bool
      val foreachi: 'a t * (int * 'a elt -> unit) -> unit
      val foreach: 'a t * ('a elt -> unit) -> unit
      val index: 'a t * ('a elt -> bool) -> int option
      val isEmpty: 'a t -> bool
      val keepAll: 'a t * ('a elt -> bool) -> 'a elt list
      val keepAllMap: 'a t * ('a elt -> 'b option) -> 'b list
      val last: 'a t -> 'a elt
      val layout: ('a elt -> Layout.t) -> 'a t -> Layout.t
      val length: 'a t -> int
      val lookup: 'a t * ('a elt -> bool) -> 'a elt
      val map: 'a t * ('a elt -> 'b) -> 'b list
      val mapi: 'a t * (int * 'a elt -> 'b) -> 'b list
      val nth: 'a t * int -> 'a elt
      val peek: 'a t * ('a elt -> bool) -> 'a elt option
      val peeki: 'a t * (int * 'a elt -> bool) -> (int * 'a elt) option
      val peekMap: 'a t * ('a elt -> 'b option) -> 'b option
      val peekMapi: 'a t * ('a elt -> 'b option) -> (int * 'b) option
      val removeAll: 'a t * ('a elt -> bool) -> 'a elt list
      val subset: 'a t * ('a elt -> bool) -> 'a elt list
   end
