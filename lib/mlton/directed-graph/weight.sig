signature WEIGHT = 
   sig
      include BOUNDED_ORDER
      val zero : t
      val + : t * t -> t
      val input : In.t -> t
      val output : t * Out.t -> unit
   end
