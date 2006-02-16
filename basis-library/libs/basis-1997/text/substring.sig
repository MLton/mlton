signature SUBSTRING_1997 =
   sig
      structure String: STRING_1997
      type substring
      val base: substring -> (String.string * int * int)
      val string: substring -> String.string
      val extract: (String.string * int * int option) -> substring
      val substring: (String.string * int * int) -> substring
      val all: String.string -> substring
      val isEmpty: substring -> bool
      val getc: substring -> (String.Char.char * substring) option
      val first: substring -> String.Char.char option
      val triml: int -> substring -> substring
      val trimr: int -> substring -> substring
      val slice: (substring * int * int option) -> substring
      val sub: (substring * int) -> String.Char.char
      val size: substring -> int
      val concat: substring list -> String.string
      val explode: substring -> String.Char.char list
      val isPrefix: String.string -> substring -> bool
      val compare: (substring * substring) -> order
      val collate: ((String.Char.char * String.Char.char) -> order)
                   -> (substring * substring) -> order
      val splitl: ((String.Char.char -> bool)
                   -> substring -> (substring * substring))
      val splitr: ((String.Char.char -> bool)
                   -> substring -> (substring * substring))
      val splitAt: (substring * int) -> (substring * substring)
      val dropl: (String.Char.char -> bool) -> substring -> substring
      val dropr: (String.Char.char -> bool) -> substring -> substring
      val takel: (String.Char.char -> bool) -> substring -> substring
      val taker: (String.Char.char -> bool) -> substring -> substring
      val position: String.string -> substring -> (substring * substring)
      val span: (substring * substring) -> substring
      val translate: ((String.Char.char -> String.string)
                      -> substring -> String.string)
      val tokens: (String.Char.char -> bool) -> substring -> substring list
      val fields: (String.Char.char -> bool) -> substring -> substring list
      val foldl: ((String.Char.char * 'a) -> 'a) -> 'a -> substring -> 'a
      val foldr: ((String.Char.char * 'a) -> 'a) -> 'a -> substring -> 'a
      val app: (String.Char.char -> unit) -> substring -> unit
   end
