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
      val extract: 'a vector * int * int option -> 'a vector 
      val concat: 'a vector list -> 'a vector 
      val mapi : (int * 'a -> 'b) -> 'a vector * int * int option -> 'b vector 
      val map: ('a -> 'b) -> 'a vector -> 'b vector 
      val appi: (int * 'a -> unit) -> 'a vector * int * int option -> unit 
      val app: ('a -> unit) -> 'a vector -> unit 
      val foldli :
	 (int * 'a * 'b -> 'b) -> 'b -> 'a vector * int * int option -> 'b 
      val foldri :
	 (int * 'a * 'b -> 'b) -> 'b -> 'a vector * int * int option -> 'b 
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b 
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
   end

signature VECTOR_EXTRA =
   sig
      include VECTOR

      val checkSlice: 'a vector * int * int option -> int
      val fromArray: 'a array -> 'a vector
      val unfold: int * 'a * ('a -> 'b * 'a) -> 'b vector
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b vector
      val unsafeSub: 'a vector * int -> 'a
   end
