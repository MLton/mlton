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

      eqtype char
	 
      val < : string * string -> bool 
      val <= : string * string -> bool 
      val > : string * string -> bool 
      val >= : string * string -> bool 
      val collate: (char * char -> order) -> string * string -> order
      val compare: string * string -> order
      val concatWith: string -> string list -> string
      val extract: string * int * int option -> string
      val fields: (Char.char -> bool) -> string -> string list
      val fromCString: string -> string option 
      val fromString: string -> string option 
      val isPrefix: string -> string -> bool
      val isSubstring: string -> string -> bool
      val isSuffix: string -> string -> bool
      val map: (Char.char -> Char.char) -> string -> string 
      val maxSize: int
      val scan: (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
      val sub: string * int -> char 
      val toCString: string -> string
      val toString: string -> string 
      val tokens: (Char.char -> bool) -> string -> string list 
      val translate: (Char.char -> string) -> string -> string 
   end

signature STRING_EXTRA =
   sig
      include STRING

      val fromArray: CharArray.array -> string
      val new: int * char -> string
      val nullTerm: string -> string
      val tabulate: int * (int -> char) -> string
      val toLower: string -> string
   end
