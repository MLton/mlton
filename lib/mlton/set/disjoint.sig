signature DISJOINT_SET =
   sig
      type 'a t
	 
      (* Each set is associated with a single value.
       * When unions occur, one of the values is chosen.
       *)

      val canUnion: 'a t * 'a t * ('a * 'a -> 'a option) -> bool
      val equals: 'a t * 'a t -> bool
      val isRepresentative: 'a t -> bool
      val representative: 'a t -> 'a t
      val setValue: 'a t * 'a -> unit
      val singleton: 'a -> 'a t
      val union: 'a t * 'a t -> unit
      val value: 'a t -> 'a
   end
