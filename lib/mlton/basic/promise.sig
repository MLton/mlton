signature PROMISE =
   sig
      type 'a t
	 
      exception Force

      val delay: (unit -> 'a) -> 'a t
      val force: 'a t -> 'a
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val lazy: (unit -> 'a) -> (unit -> 'a)
      val reset: 'a t * (unit -> 'a) -> unit
   end
