signature ORDERED_UNIQUE_SET_STRUCTS = 
   sig
      structure Element: ORDER
   end

signature ORDERED_UNIQUE_SET = 
   sig
      include ORDERED_UNIQUE_SET_STRUCTS

      type t
	 
      val fromList: Element.t list -> t

      (* O(1) *)
      val equals: t * t -> bool
   end
