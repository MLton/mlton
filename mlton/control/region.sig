(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature REGION_STRUCTS = 
   sig
   end

signature REGION = 
   sig
      include REGION_STRUCTS
      
      datatype t = T of {left: int,
			 right: int}

      val bogus: t
      val layout: t -> Layout.t
      val left: t -> int
      val list: 'a list * ('a -> t) -> t
      val right: t -> int

      structure Wrap:
	 sig
	    type region
	    type 'a t
	    val region: 'a t -> region
	    val node: 'a t -> 'a
	    val makeRegion: 'a * region -> 'a t
	    val makeRegion':  'a * int * int -> 'a t
	    val make: 'a -> 'a t
	    val dest: 'a t -> 'a * region
	    val left: 'a t -> int
	    val right: 'a t -> int
	 end
      sharing type Wrap.region = t
   end
