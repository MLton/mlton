signature VECTOR_GLOBAL =
   sig
      eqtype 'a vector
   end

signature VECTOR =
   sig
      include VECTOR_GLOBAL

      val maxLen: int 
      val fromList: 'a list -> 'a vector 
      val tabulate: int * (int -> 'a) -> 'a vector 
      val length: 'a vector -> int 
      val sub: 'a vector * int -> 'a 
      val update: 'a vector * int * 'a -> 'a vector
      val concat: 'a vector list -> 'a vector 
      val appi: (int * 'a -> unit) -> 'a vector -> unit 
      val app: ('a -> unit) -> 'a vector -> unit 
      val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector 
      val map: ('a -> 'b) -> 'a vector -> 'b vector 
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b 
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b 
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b 
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
      val findi: (int * 'a -> bool) -> 'a vector -> (int * 'a) option
      val find: ('a -> bool) -> 'a vector -> 'a option
      val exists: ('a -> bool) -> 'a vector -> bool
      val all: ('a -> bool) -> 'a vector -> bool
      val collate: ('a * 'a -> order) -> 'a vector * 'a vector -> order
   end

signature VECTOR_EXTRA =
   sig
      include VECTOR
      structure VectorSlice: VECTOR_SLICE_EXTRA where type 'a vector = 'a vector

      val vector: int * 'a -> 'a vector
      val fromArray: 'a array -> 'a vector
      val toList: 'a vector -> 'a list
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b vector
      val unsafeSub: 'a vector * int -> 'a

      (* Depreciated *)
      val checkSlice: 'a vector * int * int option -> int
      (* Depreciated *)
      val extract: 'a vector * int * int option -> 'a vector
   end
