signature STRING_GLOBAL =
   sig
      eqtype string
	 
      val size: string -> int 
      val substring: string * int * int -> string 
      val ^ : string * string -> string 
      val concat: string list -> string 
      val str: Char.char -> string 
      val implode: Char.char list -> string 
      val explode: string -> Char.char list 
   end

signature STRING =
   sig
      include STRING_GLOBAL

      eqtype char
	 
      val maxSize: int 
      val sub: string * int -> char 
      val extract: string * int * int option -> string
      val concatWith: string -> string list -> string
      val map: (Char.char -> Char.char) -> string -> string 
      val translate: (Char.char -> string) -> string -> string 
      val tokens: (Char.char -> bool) -> string -> string list 
      val fields: (Char.char -> bool) -> string -> string list
      val isPrefix: string -> string -> bool
      val isSubstring: string -> string -> bool
      val isSuffix: string -> string -> bool
      val compare: string * string -> order
      val collate: (char * char -> order) -> string * string -> order
      val < : string * string -> bool 
      val <= : string * string -> bool 
      val > : string * string -> bool 
      val >= : string * string -> bool 
      val fromString: string -> string option 
      val toString: string -> string 
      val fromCString: string -> string option 
      val toCString: string -> string
   end

signature STRING_EXTRA =
   sig
      include STRING

      val fromArray: char array -> string
      val new: int * char -> string
      val nullTerm: string -> string
      val tabulate: int * (int -> char) -> string
   end
