signature ARRAY_GLOBAL =
   sig
      eqtype 'a array
   end

signature ARRAY =
   sig
      include ARRAY_GLOBAL

      type 'a vector

      val app: ('a -> unit) -> 'a array -> unit 
      val appi: (int * 'a -> unit) -> 'a array * int * int option -> unit 
      val array: int * 'a -> 'a array 
      val copy:
	 {src: 'a array, si: int, len: int option, dst: 'a array, di: int}
	 -> unit 
      val copyVec:
	 {src: 'a vector, si: int, len: int option, dst: 'a array, di: int}
	 -> unit 
      val extract: 'a array * int * int option -> 'a vector 
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a array -> 'b 
      val foldli:
	 (int * 'a * 'b -> 'b) -> 'b -> 'a array * int * int option -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a array -> 'b 
      val foldri:
	 (int * 'a * 'b -> 'b) -> 'b -> 'a array * int * int option -> 'b
      val fromList: 'a list -> 'a array 
      val length: 'a array -> int 
      val maxLen: int 
      val modify: ('a -> 'a) -> 'a array -> unit 
      val modifyi: (int * 'a -> 'a) -> 'a array * int * int option -> unit 
      val sub: 'a array * int -> 'a 
      val tabulate: int * (int -> 'a) -> 'a array 
      val update: 'a array * int * 'a -> unit 
   end

signature ARRAY_EXTRA =
   sig
      include ARRAY

      val checkSlice: 'a array * int * int option -> int
      val checkSliceMax: int * int option * int -> int
      val prefixToList: 'a array * int -> 'a list
      val toList: 'a array -> 'a list
      val unsafeSub: 'a array * int -> 'a
      val unsafeUpdate: 'a array * int * 'a -> unit
   end
