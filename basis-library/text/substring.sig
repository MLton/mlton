signature SUBSTRING_GLOBAL =
   sig
      type substring
   end

signature SUBSTRING =
   sig
      include SUBSTRING_GLOBAL
      eqtype char
      eqtype string

      val sub: substring * int -> char
      val size: substring -> int
      val base: substring -> string * int * int
      val extract: string * int * int option -> substring
      val substring: string * int * int -> substring
      val full: string -> substring
      val all: string -> substring
      val string: substring -> string
      val isEmpty: substring -> bool
      val getc: substring -> (char * substring) option
      val first: substring -> char option
      val triml: int -> substring -> substring
      val trimr: int -> substring -> substring
      val slice: substring * int * int option -> substring
      val concat: substring list -> string
      val concatWith: string -> substring list -> string
      val explode: substring -> char list
      val isPrefix: string -> substring -> bool
      val isSubstring: string -> substring -> bool
      val isSuffix: string -> substring -> bool
      val compare: substring * substring -> order
      val collate: (char * char -> order) -> substring * substring -> order
      val splitl: (char -> bool) -> substring -> substring * substring
      val splitr: (char -> bool) -> substring -> substring * substring
      val splitAt: substring * int -> substring * substring
      val dropl: (char -> bool) -> substring -> substring
      val dropr: (char -> bool) -> substring -> substring
      val takel: (char -> bool) -> substring -> substring
      val taker: (char -> bool) -> substring -> substring
      val position: string -> substring -> substring * substring
      val span: substring * substring -> substring
      val translate: (char -> string) -> substring -> string
      val tokens: (char -> bool) -> substring -> substring list
      val fields: (char -> bool) -> substring -> substring list
      val app: (char -> unit) -> substring -> unit
      val foldl: (char * 'a -> 'a) -> 'a -> substring -> 'a
      val foldr: (char * 'a -> 'a) -> 'a -> substring -> 'a

(*
      structure String: STRING

      val all: String.string -> substring 
      val app: (String.Char.char -> unit) -> substring -> unit
      val base: substring -> String.string * int * int
      val collate:
	 (String.Char.char * String.Char.char -> order)
	 -> substring * substring -> order 
      val compare: substring * substring -> order 
      val concat: substring list -> String.string 
      val dropl: (String.Char.char -> bool) -> substring -> substring 
      val dropr: (String.Char.char -> bool) -> substring -> substring 
      val explode: substring -> String.Char.char list 
      val extract: String.string * int * int option -> substring 
      val fields: (String.Char.char -> bool) -> substring -> substring list
      val first: substring -> String.Char.char option 
      val foldl: (String.Char.char * 'a -> 'a) -> 'a -> substring -> 'a 
      val foldr: (String.Char.char * 'a -> 'a) -> 'a -> substring -> 'a 
      val getc: substring -> (String.Char.char * substring) option 
      val isEmpty: substring -> bool 
      val isPrefix: String.string -> substring -> bool 
      val position: String.string -> substring -> substring * substring 
      val size: substring -> int 
      val slice: substring * int * int option -> substring
      val span: substring * substring -> substring
      val splitAt: substring * int -> substring * substring 
      val splitl:
	 (String.Char.char -> bool) -> substring -> substring * substring 
      val splitr:
	 (String.Char.char -> bool) -> substring -> substring * substring 
      val string: substring -> String.string 
      val sub: substring * int -> char 
      val substring: String.string * int * int -> substring 
      val takel: (String.Char.char -> bool) -> substring -> substring 
      val taker: (String.Char.char -> bool) -> substring -> substring 
      val tokens: (String.Char.char -> bool) -> substring -> substring list
      val translate:
	 (String.Char.char -> String.string) -> substring -> String.string 
      val triml: int -> substring -> substring 
      val trimr: int -> substring -> substring 
*)
   end
