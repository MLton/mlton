signature APPEND_REVERSE =
   sig
      structure L: LIST
      structure I: INTEGER
      sharing L.I = I
	 
      type 'a t
      val empty: unit -> 'a t
      val isEmpty: 'a t -> bool
      val length: 'a t -> I.t
      val destruct: 'a t -> ('a * 'a t) option
      val appendReverse: 'a t * 'a L.t -> 'a t
   end
