signature MONO_VECTOR =
   sig
      type vector
      type elem
      val maxLen: int 
      val fromList: elem list -> vector 
      val tabulate: int * (int -> elem) -> vector 
      val length: vector -> int 
      val sub: vector * int -> elem 
      val update: vector * int * elem -> vector
      val concat: vector list -> vector 
      val appi: (int * elem -> unit) -> vector -> unit 
      val app: (elem -> unit) -> vector -> unit 
      val mapi: (int * elem -> elem) -> vector -> vector 
      val map: (elem -> elem) -> vector -> vector 
      val foldli: (int * elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldri: (int * elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldl: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldr: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val findi: (int * elem -> bool) -> vector -> (int * elem) option
      val exists: (elem -> bool) -> vector -> bool
      val all: (elem -> bool) -> vector -> bool
      val collate: (elem * elem -> order) -> vector * vector -> order
   end

signature MONO_VECTOR_EXTRA_PRE = 
   sig
      include MONO_VECTOR

      val unsafeSub: vector * int -> elem

      (* Used to implement Substring/String functions *)
      val concatWith: vector -> vector list -> vector
      val isPrefix: (elem * elem -> bool) -> vector -> vector -> bool
      val isSubvector: (elem * elem -> bool) -> vector -> vector -> bool
      val isSuffix: (elem * elem -> bool) -> vector -> vector -> bool
      val translate: (elem -> vector) -> vector -> vector
      val tokens: (elem -> bool) -> vector -> vector list
      val fields: (elem -> bool) -> vector -> vector list

      val duplicate: vector -> vector
      val fromArray: elem array -> vector
      val toList: vector -> elem list
      val unfoldi: int * 'a * (int * 'a -> elem * 'a) -> vector
      val vector: int * elem -> vector
      (* Depreciated *)
      val extract: vector * int * int option -> vector
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
   end
