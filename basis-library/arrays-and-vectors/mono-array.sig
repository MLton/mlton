signature MONO_ARRAY =
   sig
      eqtype array
      type elem
      type vector
      val maxLen: int 
      val array: int * elem -> array 
      val fromList: elem list -> array
      val tabulate: int * (int -> elem) -> array
      val length: array -> int
      val sub: array * int -> elem
      val update: array * int * elem -> unit
      val vector: array -> vector
      val copy: {src: array, dst: array, di: int} -> unit
      val copyVec: {src: vector, dst: array, di: int} -> unit
      val appi: (int * elem -> unit) -> array -> unit
      val app: (elem -> unit) -> array -> unit
      val modifyi: (int * elem -> elem) -> array -> unit
      val modify: (elem -> elem) -> array -> unit
      val foldli: (int * elem * 'b -> 'b) -> 'b -> array -> 'b
      val foldri: (int * elem * 'b -> 'b) -> 'b -> array -> 'b
      val foldl: (elem * 'b -> 'b) -> 'b -> array -> 'b
      val foldr: (elem * 'b -> 'b) -> 'b -> array -> 'b
      val findi: (int * elem -> bool) -> array -> (int * elem) option
      val find: (elem -> bool) -> array -> elem option
      val exists: (elem -> bool) -> array -> bool
      val all: (elem -> bool) -> array -> bool
      val collate: (elem * elem -> order) -> array * array -> order
   end

signature MONO_ARRAY_EXTRA =
   sig
      include MONO_ARRAY
      type vector_slice
      structure MonoArraySlice: MONO_ARRAY_SLICE_EXTRA 
	where type elem = elem
	  and type array = array
	  and type vector = vector
	  and type vector_slice = vector_slice

      val concat: array list -> array
      val duplicate: array -> array
      val extract: array * int * int option -> vector (* Deprecated *)
      val fromPoly: elem Array.array -> array
      val rawArray: int -> array
      val toList: array -> elem list
      val toPoly: array -> elem Array.array
      val unfoldi: int * 'a * (int * 'a -> elem * 'a) -> array
      val unsafeSub: array * int -> elem
      val unsafeUpdate: array * int * elem -> unit
   end
