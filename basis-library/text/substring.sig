signature SUBSTRING_GLOBAL =
   sig
      type substring
   end

signature SUBSTRING =
   sig
      include SUBSTRING_GLOBAL
      eqtype char
      eqtype string

      val app: (char -> unit) -> substring -> unit
      val base: substring -> string * int * int
      val collate: (char * char -> order) -> substring * substring -> order
      val compare: substring * substring -> order
      val concat: substring list -> string
      val concatWith: string -> substring list -> string
      val dropl: (char -> bool) -> substring -> substring
      val dropr: (char -> bool) -> substring -> substring
      val explode: substring -> char list
      val extract: string * int * int option -> substring
      val fields: (char -> bool) -> substring -> substring list
      val first: substring -> char option
      val foldl: (char * 'a -> 'a) -> 'a -> substring -> 'a
      val foldr: (char * 'a -> 'a) -> 'a -> substring -> 'a
      val full: string -> substring
      val getc: substring -> (char * substring) option
      val isEmpty: substring -> bool
      val isPrefix: string -> substring -> bool
      val isSubstring: string -> substring -> bool
      val isSuffix: string -> substring -> bool
      val position: string -> substring -> substring * substring
      val size: substring -> int
      val slice: substring * int * int option -> substring
      val span: substring * substring -> substring
      val splitAt: substring * int -> substring * substring
      val splitl: (char -> bool) -> substring -> substring * substring
      val splitr: (char -> bool) -> substring -> substring * substring
      val string: substring -> string
      val sub: substring * int -> char
      val substring: string * int * int -> substring
      val takel: (char -> bool) -> substring -> substring
      val taker: (char -> bool) -> substring -> substring
      val tokens: (char -> bool) -> substring -> substring list
      val translate: (char -> string) -> substring -> string
      val triml: int -> substring -> substring
      val trimr: int -> substring -> substring
   end

signature SUBSTRING_EXTRA =
   sig
      include SUBSTRING
   end
