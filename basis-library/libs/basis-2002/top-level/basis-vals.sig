signature BASIS_2002_VALS = 
   sig
      (* Top-level values *)
      val ! : 'a ref -> 'a
      val := : 'a ref * 'a -> unit
      val @ : ('a list * 'a list) -> 'a list
      val ^ : string * string -> string
      val app : ('a -> unit) -> 'a list -> unit
      val before : 'a * unit -> 'a
      val ceil : real -> int 
      val chr : int -> char
      val concat : string list -> string
      val exnMessage : exn -> string
      val exnName : exn -> string
      val explode : string -> char list
      val floor : real -> int 
      val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
      val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b 
      val getOpt : ('a option * 'a) -> 'a
      val hd : 'a list -> 'a
      val ignore : 'a -> unit
      val isSome : 'a option -> bool
      val implode : char list -> string
      val length : 'a list -> int
      val map : ('a -> 'b) -> 'a list -> 'b list
      val not : bool -> bool
      val null : 'a list -> bool
      val o : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
      val ord : char -> int
      val print : string -> unit
      val real : int -> real
(*
      val ref : 'a -> 'a ref
*)
      val rev : 'a list -> 'a list
      val round : real -> int
      val size : string -> int
      val str : char -> string
      val substring : string * int * int -> string
      val tl : 'a list -> 'a list
      val trunc : real -> int 
(*
      val use : string -> unit
*)
      val valOf : 'a option -> 'a 
      val vector : 'a list -> 'a vector

      val = : ''a * ''a -> bool
      val <> : ''a * ''a -> bool
   end
