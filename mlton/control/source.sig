type int = Int.t
   
signature SOURCE =
   sig
      type t

      val currentIndex: t -> int
      val file: t -> File.t
      val indexPosition: t * int -> {column: int, line: int} 
      val new: File.t -> t
      val newline: t * int -> unit
   end
