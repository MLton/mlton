signature MONO_ARRAY =
   sig
      eqtype array
      type elem
      type vector

      val all: (elem -> bool) -> array -> bool
      val app: (elem -> unit) -> array -> unit
      val appi: (int * elem -> unit) -> array -> unit
      val array: int * elem -> array 
      val collate: (elem * elem -> order) -> array * array -> order
      val copy: {src: array, dst: array, di: int} -> unit
      val copyVec: {src: vector, dst: array, di: int} -> unit
      val exists: (elem -> bool) -> array -> bool
      val find: (elem -> bool) -> array -> elem option
      val findi: (int * elem -> bool) -> array -> (int * elem) option
      val foldl: (elem * 'b -> 'b) -> 'b -> array -> 'b
      val foldli: (int * elem * 'b -> 'b) -> 'b -> array -> 'b
      val foldr: (elem * 'b -> 'b) -> 'b -> array -> 'b
      val foldri: (int * elem * 'b -> 'b) -> 'b -> array -> 'b
      val fromList: elem list -> array
      val length: array -> int
      val maxLen: int 
      val modify: (elem -> elem) -> array -> unit
      val modifyi: (int * elem -> elem) -> array -> unit
      val sub: array * int -> elem
      val tabulate: int * (int -> elem) -> array
      val update: array * int * elem -> unit
      val vector: array -> vector
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

      val arrayUninit: int -> array

      val concat: array list -> array
      val duplicate: array -> array
      val fromPoly: elem Array.array -> array
      val toList: array -> elem list
      val toPoly: array -> elem Array.array
      val unfoldi: int * 'a * (int * 'a -> elem * 'a) -> array * 'a
      val unsafeSub: array * int -> elem
      val unsafeUpdate: array * int * elem -> unit
   end
