type int = Int.t
   
signature RESIZABLE_ARRAY =
   sig
      include ARRAY

      val addToEnd: 'a t * 'a -> unit
      val deleteLast: 'a t -> 'a
      val empty: unit -> 'a t
      val subOption: 'a t * int -> 'a option
   end
