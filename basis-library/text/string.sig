signature STRING_GLOBAL =
   sig
      eqtype char
      eqtype string

      val ^ : string * string -> string 
      val concat: string list -> string 
      val explode: string -> char list 
      val implode: char list -> string 
      val size: string -> int 
      val str: char -> string 
      val substring: string * int * int -> string 
   end

signature STRING =
   sig
      include STRING_GLOBAL


      val < : string * string -> bool 
      val <= : string * string -> bool 
      val > : string * string -> bool 
      val >= : string * string -> bool 
      val collate: (char * char -> order) -> string * string -> order
      val compare: string * string -> order
      val concatWith: string -> string list -> string
      val extract: string * int * int option -> string
      val fields: (char -> bool) -> string -> string list
      val fromCString: String.string -> string option 
      val fromString: String.string -> string option 
      val isPrefix: string -> string -> bool
      val isSubstring: string -> string -> bool
      val isSuffix: string -> string -> bool
      val map: (char -> char) -> string -> string 
      val maxSize: int
      val scan: (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
      val sub: string * int -> char 
      val toCString: string -> String.string
      val toString: string -> String.string 
      val tokens: (char -> bool) -> string -> string list 
      val translate: (char -> string) -> string -> string 
   end

signature STRING_EXTRA =
   sig
      include STRING
      type array
      
      val unsafeFromArray: array -> string

      val new: int * char -> string
      val nullTerm: string -> string
      val tabulate: int * (int -> char) -> string
      val toLower: string -> string
   end
