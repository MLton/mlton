signature VECTOR_SLICE_GLOBAL =
   sig

   end

signature VECTOR_SLICE =
   sig
      include VECTOR_SLICE_GLOBAL

      type 'a vector
      type 'a slice
      val length: 'a slice -> int
      val sub: 'a slice * int -> 'a
      val full: 'a vector -> 'a slice
      val slice: 'a vector * int * int option -> 'a slice
      val subslice: 'a slice * int * int option -> 'a slice
      val base: 'a slice -> 'a vector * int * int
      val vector: 'a slice -> 'a vector
      val concat: 'a slice list -> 'a vector
      val isEmpty: 'a slice -> bool
      val getItem: 'a slice -> ('a * 'a slice) option
      val appi: (int * 'a -> unit) -> 'a slice -> unit
      val app: ('a -> unit) -> 'a slice -> unit
      val mapi: (int * 'a -> 'b) -> 'a slice -> 'b vector
      val map: ('a -> 'b) -> 'a slice -> 'b vector
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val findi: (int * 'a -> bool) -> 'a slice -> (int * 'a) option
      val find: ('a -> bool) -> 'a slice -> 'a option
      val exists: ('a -> bool) -> 'a slice -> bool
      val all: ('a -> bool) -> 'a slice -> bool
      val collate: ('a * 'a -> order) -> 'a slice * 'a slice -> order
   end

signature VECTOR_SLICE_EXTRA =
   sig
      include VECTOR_SLICE

      val copy: 'a slice -> 'a vector
      val toList: 'a slice -> 'a list

      val unsafeSub: 'a slice * int -> 'a
      val unsafeSlice: 'a vector * int * int option -> 'a slice
      val unsafeSubslice: 'a slice * int * int option -> 'a slice

      val isPrefix: ('a * 'a -> bool) -> 'a vector -> 'a slice -> bool
      val isSubsequence: ('a * 'a -> bool) -> 'a vector -> 'a slice -> bool
      val isSuffix: ('a * 'a -> bool) -> 'a vector -> 'a slice -> bool
   end
