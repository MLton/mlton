signature PROMISE =
   sig
      type 'a t
	 
      exception Force

      val delay: (unit -> 'a) -> 'a t
      val force: 'a t -> 'a
      val lazy: (unit -> 'a) -> (unit -> 'a)
   end
