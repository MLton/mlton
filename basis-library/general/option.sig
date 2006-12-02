signature OPTION_GLOBAL =
   sig
      datatype 'a option = NONE | SOME of 'a 

      exception Option

      val getOpt: 'a option * 'a -> 'a 
      val isSome: 'a option -> bool 
      val valOf: 'a option -> 'a 
   end

signature OPTION =
   sig
      include OPTION_GLOBAL

      val app: ('a -> unit) -> 'a option -> unit
      val compose: ('a -> 'b) * ('c -> 'a option) -> 'c -> 'b option 
      val composePartial: ('a -> 'b option) * ('c -> 'a option) -> 'c -> 'b option 
      val filter: ('a -> bool) -> 'a -> 'a option 
      val join: 'a option option -> 'a option 
      val map: ('a -> 'b) -> 'a option -> 'b option 
      val mapPartial: ('a -> 'b option) -> 'a option -> 'b option 
   end
