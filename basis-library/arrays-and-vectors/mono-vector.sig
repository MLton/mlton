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
