signature STRING_GLOBAL =
   sig
      eqtype string
	 
      val ^ : string * string -> string 
      val concat: string list -> string 
      val explode: string -> Char.char list 
      val implode: Char.char list -> string 
      val size: string -> int 
      val str: Char.char -> string 
      val substring: string * int * int -> string 
   end

signature STRING =
   sig
      include STRING_GLOBAL

      structure Char: CHAR
	 
      val < : string * string -> bool 
      val <= : string * string -> bool 
      val > : string * string -> bool 
      val >= : string * string -> bool 
      val collate: (Char.char * Char.char -> order) -> string * string -> order
      val compare: string * string -> order
      val extract: string * int * int option -> string
      val fields: (Char.char -> bool) -> string -> string list
      val fromCString: string -> string option 
      val fromString: string -> string option 
      val isPrefix: string -> string -> bool
      val map: (Char.char -> Char.char) -> string -> string 
      val maxSize: int 
      val sub: string * int -> Char.char 
      val toCString: string -> string
      val toString: string -> string 
      val tokens: (Char.char -> bool) -> string -> string list 
      val translate: (Char.char -> string) -> string -> string 
   end

signature STRING_EXTRA =
   sig
      include STRING

      val fromArray: char array -> string
      val new: int * char -> string
      val nullTerm: string -> string
      val tabulate: int * (int -> char) -> string
   end
