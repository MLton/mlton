signature VECTOR_GLOBAL =
   sig
      type 'a vector = 'a vector
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
      structure VectorSlice: VECTOR_SLICE_EXTRA 

      val unsafeFromArray: 'a array -> 'a vector
      val unsafeSub: 'a vector * int -> 'a

      (* Used to implement Substring/String functions *)
      val concatWith: 'a vector -> 'a vector list -> 'a vector
      val isPrefix: ('a * 'a -> bool) -> 'a vector -> 'a vector -> bool
      val isSubvector: ('a * 'a -> bool) -> 'a vector -> 'a vector -> bool
      val isSuffix: ('a * 'a -> bool) -> 'a vector -> 'a vector -> bool
      val translate: ('a -> 'b vector) -> 'a vector -> 'b vector
      val tokens: ('a -> bool) -> 'a vector -> 'a vector list
      val fields: ('a -> bool) -> 'a vector -> 'a vector list

      val append: 'a vector * 'a vector -> 'a vector
      val create: int -> {done: unit -> 'a vector,
                          sub: int -> 'a,
                          update: int * 'a -> unit}
      val duplicate: 'a vector -> 'a vector
      val tabulate': SeqIndex.int * (SeqIndex.int -> 'a) -> 'a vector 
      val toList: 'a vector -> 'a list
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a vector * 'b
      val vector: int * 'a -> 'a vector
   end
