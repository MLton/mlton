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

      (* Depreciated *)
      val extract: vector * int * int option -> vector 
   end

signature MONO_VECTOR_EXTRA =
   sig
      include MONO_VECTOR
      structure MonoVectorSlice: MONO_VECTOR_SLICE_EXTRA 
	where type elem = elem
	  and type vector = vector

      val copy: vector -> vector
      val fromArray: elem array -> vector
      val toList: vector -> elem list
      val unfoldi: int * 'a * (int * 'a -> elem * 'a) -> vector
      val vector: int * elem -> vector

      val unsafeSub: vector * int -> elem

      val isPrefix: (elem * elem -> bool) -> vector -> vector -> bool
      val isSubsequence: (elem * elem -> bool) -> vector -> vector -> bool
      val isSuffix: (elem * elem -> bool) -> vector -> vector -> bool
   end

signature CONCRETE_MONO_VECTOR = MONO_VECTOR 
  where type vector = elem Vector.vector
    and type MonoVectorSlice.vector = elem Vector.vector
    and type MonoVectorSlice.vector = elem Vector.VectorSlice.slice
signature CONCRETE_MONO_VECTOR_EXTRA = MONO_VECTOR_EXTRA 
  where type vector = elem Vector.vector
    and type MonoVectorSlice.vector = elem Vector.vector
    and type MonoVectorSlice.vector = elem Vector.VectorSlice.slice

(*
(* The only difference between CONCRETE_MONO_VECTOR and MONO_VECTOR is that
 * the former specifies the type of vector.  I couldn't figure out a way to do
 * this in SML using sharing/with, so I had to duplicate the signature.
 *)
signature CONCRETE_MONO_VECTOR =
   sig
      type elem
      type vector = elem Vector.vector
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

      (* Depreciated *)
      val extract: vector * int * int option -> vector 
   end

signature CONCRETE_MONO_VECTOR_EXTRA =
   sig
      include CONCRETE_MONO_VECTOR
      structure MonoVectorSlice: MONO_VECTOR_SLICE where type elem = elem
	                                             and type vector = vector
   end
*)
(* Depreciated *)
(*
signature EQTYPE_MONO_VECTOR =
   sig
      eqtype elem
      type vector = elem Vector.vector
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

      (* Depreciated *)
      val extract: vector * int * int option -> vector 
   end
*)