structure Array =
   struct
      type 'a array = 'a array
   end

signature ARRAY_SLICE_GLOBAL =
   sig
   end

signature ARRAY_SLICE =
   sig
      include ARRAY_SLICE_GLOBAL

      type 'a slice

      val all: ('a -> bool) -> 'a slice -> bool
      val app : ('a -> unit) -> 'a slice -> unit
      val appi: (int * 'a -> unit) -> 'a slice -> unit
      val base: 'a slice -> 'a Array.array * int * int
      val collate: ('a * 'a -> order) -> 'a slice * 'a slice -> order
      val copy: {src: 'a slice, dst: 'a Array.array, di: int} -> unit
      val copyVec: {di: int,
                    dst: 'a Array.array,
                    src: 'a VectorSlice.slice} -> unit
      val exists: ('a -> bool) -> 'a slice -> bool
      val find: ('a -> bool) -> 'a slice -> 'a option
      val findi: (int * 'a -> bool) -> 'a slice -> (int * 'a) option
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val full: 'a Array.array -> 'a slice
      val getItem: 'a slice -> ('a * 'a slice) option
      val isEmpty: 'a slice -> bool
      val length: 'a slice -> int
      val modify : ('a -> 'a) -> 'a slice -> unit
      val modifyi: (int * 'a -> 'a) -> 'a slice -> unit
      val slice: 'a Array.array * int * int option -> 'a slice
      val sub: 'a slice * int -> 'a
      val subslice: 'a slice * int * int option -> 'a slice
      val update: 'a slice * int * 'a -> unit
      val vector: 'a slice -> 'a Vector.vector
   end

signature ARRAY_SLICE_EXTRA =
   sig
      include ARRAY_SLICE

      val concat: 'a slice list -> 'a array
      val toList: 'a slice -> 'a list
      val slice': 'a array * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSlice': 'a array * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSlice: 'a array * int * int option -> 'a slice
      val sub': 'a slice * SeqIndex.int -> 'a
      val unsafeSub': 'a slice * SeqIndex.int -> 'a
      val unsafeSub: 'a slice * int -> 'a
      val unsafeSubslice': 'a slice * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSubslice: 'a slice * int * int option -> 'a slice
      val update': 'a slice * SeqIndex.int * 'a -> unit
      val unsafeUpdate': 'a slice * SeqIndex.int * 'a -> unit
      val unsafeUpdate: 'a slice * int * 'a -> unit
   end
