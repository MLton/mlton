type int = Int.t
   
signature RANDOM =
   sig
      val alphaNumString: int -> string
      val bool: unit -> bool
      val int: unit -> int
      (* intRange(i, j)() is uniform in [i, j]
       * It is curried because it does some computation with i and j.
       *)
      val intRange: int * int -> unit -> int
      val list: 'a list -> 'a option
      val nRandom: {list: 'a list, length: int, n: int} -> 'a list
      (* 0.0 <= real() <= 1.0 *)
      val real: unit -> real
      val seed: unit -> Word.t
      val srand: Word.t -> unit
      val useed: unit -> Word.t
      val word: unit -> Word.t
   end
