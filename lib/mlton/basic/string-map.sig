signature STRING_MAP = 
   sig
      type 'a t

      val clear: 'a t -> unit
      val domain: 'a t -> string list
      val foreach: 'a t * ('a -> unit) -> unit
      val keepAll: 'a t * ('a -> bool) -> string list
      val lookup: 'a t * string -> 'a
      val new: (unit -> 'a) -> 'a t
   end
