signature MONO_VECTOR_SLICE =
   sig
      type elem
      type slice
      type vector

      val all: (elem -> bool) -> slice -> bool
      val app: (elem -> unit) -> slice -> unit
      val appi: (int * elem -> unit) -> slice -> unit
      val base: slice -> vector * int * int
      val collate: (elem * elem -> order) -> slice * slice -> order
      val concat: slice list -> vector
      val exists: (elem -> bool) -> slice -> bool
      val find : (elem -> bool) -> slice -> elem option
      val findi: (int * elem -> bool) -> slice -> (int * elem) option
      val foldl: (elem * 'b -> 'b) -> 'b -> slice -> 'b
      val foldli: (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
      val foldr: (elem * 'b -> 'b) -> 'b -> slice -> 'b
      val foldri: (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
      val full: vector -> slice
      val getItem: slice -> (elem * slice) option
      val isEmpty: slice -> bool
      val length: slice -> int
      val map: (elem -> elem) -> slice -> vector
      val mapi: (int * elem -> elem) -> slice -> vector
      val slice: vector * int * int option -> slice
      val sub: slice * int -> elem
      val subslice: slice * int * int option -> slice
      val vector: slice -> vector
   end

signature MONO_VECTOR_SLICE_EXTRA =
   sig
      include MONO_VECTOR_SLICE

      val concatWith: vector -> slice list -> vector
      val dropl: (elem -> bool) -> slice -> slice
      val dropr: (elem -> bool) -> slice -> slice
      val fields: (elem -> bool) -> slice -> slice list
      val isPrefix: (elem * elem -> bool) -> vector -> slice -> bool
      val isSubvector: (elem * elem -> bool) -> vector -> slice -> bool
      val isSuffix: (elem * elem -> bool) -> vector -> slice -> bool
      val position: (elem * elem -> bool) -> vector -> slice -> slice * slice
      val splitAt: slice * int -> slice * slice
      val splitl: (elem -> bool) -> slice -> slice * slice
      val splitr: (elem -> bool) -> slice -> slice * slice
      val takel: (elem -> bool) -> slice -> slice
      val taker: (elem -> bool) -> slice -> slice
      val toList: slice -> elem list
      val tokens: (elem -> bool) -> slice -> slice list
      val translate: (elem -> vector) -> slice -> vector
      val triml: int -> slice -> slice
      val trimr: int -> slice -> slice
      val unsafeSlice: vector * int * int option -> slice
      val unsafeSub: slice * int -> elem
      val unsafeSubslice: slice * int * int option -> slice
   end

signature EQTYPE_MONO_VECTOR_SLICE_EXTRA =
   sig
      include MONO_VECTOR_SLICE_EXTRA

      val fromPoly: elem VectorSlice.slice -> slice
      val span: slice * slice -> slice
      val toPoly: slice -> elem VectorSlice.slice
   end
