signature SUBSTRING_GLOBAL =
   sig
      type substring
   end

signature SUBSTRING =
   sig
      include SUBSTRING_GLOBAL
      
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
   end
