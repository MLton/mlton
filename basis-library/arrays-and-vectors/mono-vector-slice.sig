signature MONO_VECTOR_SLICE =
   sig
      type elem
      type slice
      type vector
	 
      val length: slice -> int
      val sub: slice * int -> elem
      val full: vector -> slice
      val slice: vector * int * int option -> slice
      val subslice: slice * int * int option -> slice
      val base: slice -> vector * int * int
      val vector: slice -> vector
      val concat: slice list -> vector
      val isEmpty: slice -> bool
      val getItem: slice -> (elem * slice) option
      val appi: (int * elem -> unit) -> slice -> unit
      val app: (elem -> unit) -> slice -> unit
      val mapi: (int * elem -> elem) -> slice -> vector
      val map: (elem -> elem) -> slice -> vector
      val foldli: (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
      val foldr: (elem * 'b -> 'b) -> 'b -> slice -> 'b
      val foldl: (elem * 'b -> 'b) -> 'b -> slice -> 'b
      val foldri: (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
      val findi: (int * elem -> bool) -> slice -> (int * elem) option
      val find : (elem -> bool) -> slice -> elem option
      val exists: (elem -> bool) -> slice -> bool
      val all: (elem -> bool) -> slice -> bool
      val collate: (elem * elem -> order) -> slice * slice -> order
   end

signature MONO_VECTOR_SLICE_EXTRA =
   sig
      include MONO_VECTOR_SLICE

      val unsafeSub: slice * int -> elem
      val unsafeSlice: vector * int * int option -> slice
      val unsafeSubslice: slice * int * int option -> slice

      (* Used to implement Substring/String functions *)
      val concatWith: vector -> slice list -> vector
      val triml: int -> slice -> slice
      val trimr: int -> slice -> slice
      val isPrefix: (elem * elem -> bool) -> vector -> slice -> bool
      val isSubvector: (elem * elem -> bool) -> vector -> slice -> bool
      val isSuffix: (elem * elem -> bool) -> vector -> slice -> bool
      val splitl: (elem -> bool) -> slice -> slice * slice
      val splitr: (elem -> bool) -> slice -> slice * slice
      val splitAt: slice * int -> slice * slice
      val dropl: (elem -> bool) -> slice -> slice
      val dropr: (elem -> bool) -> slice -> slice
      val takel: (elem -> bool) -> slice -> slice
      val taker: (elem -> bool) -> slice -> slice
      val position: (elem * elem -> bool) -> 
                    vector -> slice -> slice * slice
      val translate: (elem -> vector) -> slice -> vector
      val tokens: (elem -> bool) -> slice -> slice list
      val fields: (elem -> bool) -> slice -> slice list

      val toList: slice -> elem list
      val fromPoly: elem VectorSlice.slice -> slice
      val toPoly: slice -> elem VectorSlice.slice
   end

signature EQTYPE_MONO_VECTOR_SLICE_EXTRA =
   sig
      include MONO_VECTOR_SLICE_EXTRA
      val span: slice * slice -> slice
   end
