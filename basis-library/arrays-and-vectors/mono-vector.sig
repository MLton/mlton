signature MONO_VECTOR =
   sig
      type vector
      type elem

      val all: (elem -> bool) -> vector -> bool
      val app: (elem -> unit) -> vector -> unit 
      val appi: (int * elem -> unit) -> vector -> unit 
      val collate: (elem * elem -> order) -> vector * vector -> order
      val concat: vector list -> vector 
      val exists: (elem -> bool) -> vector -> bool
      val find: (elem -> bool) -> vector -> elem option
      val findi: (int * elem -> bool) -> vector -> (int * elem) option
      val foldl: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldli: (int * elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldr: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldri: (int * elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val fromList: elem list -> vector 
      val length: vector -> int 
      val map: (elem -> elem) -> vector -> vector 
      val mapi: (int * elem -> elem) -> vector -> vector 
      val maxLen: int 
      val sub: vector * int -> elem 
      val tabulate: int * (int -> elem) -> vector 
      val update: vector * int * elem -> vector
   end

signature MONO_VECTOR_EXTRA_PRE = 
   sig
      include MONO_VECTOR

      type array

      val unsafeFromArray: array -> vector
      val unsafeSub: vector * int -> elem

      val append: vector * vector -> vector
      val concatWith: vector -> vector list -> vector
      val duplicate: vector -> vector
      val fields: (elem -> bool) -> vector -> vector list
      val isPrefix: (elem * elem -> bool) -> vector -> vector -> bool
      val isSubvector: (elem * elem -> bool) -> vector -> vector -> bool
      val isSuffix: (elem * elem -> bool) -> vector -> vector -> bool
      val toList: vector -> elem list
      val tokens: (elem -> bool) -> vector -> vector list
      val translate: (elem -> vector) -> vector -> vector
      val unfoldi: int * 'a * (int * 'a -> elem * 'a) -> vector * 'a
      val vector: int * elem -> vector
   end

signature MONO_VECTOR_EXTRA =
   sig
      include MONO_VECTOR_EXTRA_PRE

      structure MonoVectorSlice: MONO_VECTOR_SLICE_EXTRA 
         where type elem = elem
           and type vector = vector
   end

signature EQTYPE_MONO_VECTOR_EXTRA =
   sig
      include MONO_VECTOR_EXTRA_PRE

      structure MonoVectorSlice: EQTYPE_MONO_VECTOR_SLICE_EXTRA 
         where type elem = elem
           and type vector = vector

      val fromPoly: elem Vector.vector -> vector 
      val toPoly: vector -> elem Vector.vector
   end
