signature CLEARABLE_PROMISE =
   sig
      type 'a t

      exception Force

      val clear: 'a t -> unit
      val delay: (unit -> 'a) -> 'a t
      val force: 'a t -> 'a
   end
