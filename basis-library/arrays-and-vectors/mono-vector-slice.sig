signature MONO_VECTOR_SLICE =
   sig
      type elem
      type vector
      type slice
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
