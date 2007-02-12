structure Vector =
   struct
      type 'a vector = 'a vector
   end

signature VECTOR_SLICE_GLOBAL =
   sig
   end

signature VECTOR_SLICE =
   sig
      include VECTOR_SLICE_GLOBAL

      type 'a slice

      val length: 'a slice -> int
      val sub: 'a slice * int -> 'a
      val full: 'a Vector.vector -> 'a slice
      val slice: 'a Vector.vector * int * int option -> 'a slice
      val subslice: 'a slice * int * int option -> 'a slice
      val base: 'a slice -> 'a Vector.vector * int * int
      val vector: 'a slice -> 'a Vector.vector
      val concat: 'a slice list -> 'a Vector.vector
      val isEmpty: 'a slice -> bool
      val getItem: 'a slice -> ('a * 'a slice) option
      val appi: (int * 'a -> unit) -> 'a slice -> unit
      val app: ('a -> unit) -> 'a slice -> unit
      val mapi: (int * 'a -> 'b) -> 'a slice -> 'b Vector.vector
      val map: ('a -> 'b) -> 'a slice -> 'b Vector.vector 
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
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

      val unsafeSub': 'a slice * SeqIndex.int -> 'a
      val unsafeSub: 'a slice * int -> 'a
      val unsafeSlice': 'a Vector.vector * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSlice: 'a Vector.vector * int * int option -> 'a slice
      val unsafeSubslice': 'a slice * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSubslice: 'a slice * int * int option -> 'a slice

      (* Used to implement Substring/String functions *)
      val concatWith: 'a Vector.vector -> 'a slice list -> 'a Vector.vector
      val triml: int -> 'a slice -> 'a slice
      val trimr: int -> 'a slice -> 'a slice
      val isPrefix: ('a * 'a -> bool) -> 'a Vector.vector -> 'a slice -> bool
      val isSubvector: ('a * 'a -> bool) -> 'a Vector.vector -> 'a slice -> bool
      val isSuffix: ('a * 'a -> bool) -> 'a Vector.vector -> 'a slice -> bool
      val splitl: ('a -> bool) -> 'a slice -> 'a slice * 'a slice
      val splitr: ('a -> bool) -> 'a slice -> 'a slice * 'a slice
      val splitAt: 'a slice * int -> 'a slice * 'a slice
      val dropl: ('a -> bool) -> 'a slice -> 'a slice
      val dropr: ('a -> bool) -> 'a slice -> 'a slice
      val takel: ('a -> bool) -> 'a slice -> 'a slice
      val taker: ('a -> bool) -> 'a slice -> 'a slice
      val position: ('a * 'a -> bool) -> 
                    'a Vector.vector -> 'a slice -> 'a slice * 'a slice
      val span: ''a slice * ''a slice -> ''a slice
      val translate: ('a -> 'b Vector.vector) -> 'a slice -> 'b Vector.vector
      val tokens: ('a -> bool) -> 'a slice -> 'a slice list
      val fields: ('a -> bool) -> 'a slice -> 'a slice list

      val toList: 'a slice -> 'a list
   end
