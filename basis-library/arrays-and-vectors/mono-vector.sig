signature MONO_VECTOR =
   sig
      type vector
      type elem
      val maxLen: int 
      val fromList: elem list -> vector 
      val tabulate: int * (int -> elem) -> vector 
      val length: vector -> int 
      val sub: vector * int -> elem 
      val extract: vector * int * int option -> vector 
      val concat: vector list -> vector 
      val mapi: (int * elem -> elem) -> vector * int * int option -> vector 
      val map: (elem -> elem) -> vector -> vector 
      val appi: (int * elem -> unit) -> vector * int * int option -> unit 
      val app: (elem -> unit) -> vector -> unit 
      val foldli:
	 (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a 
      val foldri:
	 (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a 
      val foldl: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldr: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
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
      val extract: vector * int * int option -> vector 
      val concat: vector list -> vector 
      val mapi: (int * elem -> elem) -> vector * int * int option -> vector 
      val map: (elem -> elem) -> vector -> vector 
      val appi: (int * elem -> unit) -> vector * int * int option -> unit 
      val app: (elem -> unit) -> vector -> unit 
      val foldli:
	 (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a 
      val foldri:
	 (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a 
      val foldl: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldr: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
   end

signature EQTYPE_MONO_VECTOR =
   sig
      type elem
      type vector = elem Vector.vector
      val maxLen: int 
      val fromList: elem list -> vector 
      val tabulate: int * (int -> elem) -> vector 
      val length: vector -> int 
      val sub: vector * int -> elem 
      val extract: vector * int * int option -> vector 
      val concat: vector list -> vector 
      val mapi: (int * elem -> elem) -> vector * int * int option -> vector 
      val map: (elem -> elem) -> vector -> vector 
      val appi: (int * elem -> unit) -> vector * int * int option -> unit 
      val app: (elem -> unit) -> vector -> unit 
      val foldli:
	 (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a 
      val foldri:
	 (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a 
      val foldl: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
      val foldr: (elem * 'a -> 'a) -> 'a -> vector -> 'a 
   end
