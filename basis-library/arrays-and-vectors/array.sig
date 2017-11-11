signature ARRAY_GLOBAL =
   sig
      type 'a array = 'a Array.array
   end

signature ARRAY =
   sig
      include ARRAY_GLOBAL

      type 'a vector = 'a Vector.vector

      val all: ('a -> bool) -> 'a array -> bool
      val app: ('a -> unit) -> 'a array -> unit 
      val appi: (int * 'a -> unit) -> 'a array -> unit 
      val array: int * 'a -> 'a array 
      val collate: ('a * 'a -> order) -> 'a array * 'a array -> order
      val copy: {src: 'a array, dst: 'a array, di: int} -> unit 
      val copyVec: {src: 'a vector, dst: 'a array, di: int} -> unit 
      val exists: ('a -> bool) -> 'a array -> bool
      val find: ('a -> bool) -> 'a array -> 'a option
      val findi: (int * 'a -> bool) -> 'a array -> (int * 'a) option
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a array -> 'b 
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a array -> 'b 
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
      val fromList: 'a list -> 'a array 
      val length: 'a array -> int 
      val maxLen: int 
      val modify: ('a -> 'a) -> 'a array -> unit 
      val modifyi: (int * 'a -> 'a) -> 'a array -> unit 
      val sub: 'a array * int -> 'a 
      val tabulate: int * (int -> 'a) -> 'a array 
      val update: 'a array * int * 'a -> unit 
      val vector: 'a array -> 'a vector
   end

signature ARRAY_EXTRA =
   sig
      include ARRAY

      structure ArraySlice: ARRAY_SLICE_EXTRA 

      val alloc: int -> 'a array
      val uninitIsNop: 'a array -> bool
      val uninit: 'a array * int -> unit
      val unsafeAlloc: int -> 'a array
      val unsafeArray: int * 'a -> 'a array
      val unsafeCopy: {dst: 'a array, di: int, src: 'a array} -> unit
      val unsafeCopyVec: {dst: 'a array, di: int, src: 'a vector} -> unit
      val unsafeSub: 'a array * int -> 'a
      val unsafeUninit: 'a array * int -> unit
      val unsafeUpdate: 'a array * int * 'a -> unit

      val concat: 'a array list -> 'a array
      val duplicate: 'a array -> 'a array
      val toList: 'a array -> 'a list
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a array * 'b
      val unfold: int * 'b * ('b -> 'a * 'b) -> 'a array * 'b

      structure Raw:
         sig
            type 'a rawarr
            val alloc: int -> 'a rawarr
            val length: 'a rawarr -> int
            val uninit: 'a rawarr * int -> unit
            val uninitIsNop: 'a rawarr -> bool
            val unsafeAlloc: int -> 'a rawarr
            val unsafeToArray: 'a rawarr -> 'a array
            val unsafeUninit: 'a rawarr * int -> unit
         end
   end
