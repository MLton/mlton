(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Pervasive.Int.int
   
signature STRING =
   sig
      type t = string

      (* a / b = a ^ "/" ^ b
       * This abbreviation is useful for building pathnames, e.g.
       *    val user = "you"
       *    val bin = "/home"/user/"bin"
       *)
      val / : string * string -> string
      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val ^ : t * t -> t
      val alphabetize: t -> t
      val baseName: t * t -> t
      val compare: t * t -> Relation.t
      val concat: t list -> t
      val contains: t * char -> bool
      val deleteSurroundingWhitespace: t -> t
      val dquote: t (* " *)
      val dropl: t * (char -> bool) -> t
      val dropLast: t -> t
      val dropPrefix: t * int -> t
      val dropSuffix: t * int -> t
      val dropTrailing: t * char -> t
      val empty: t
      val equals: t * t -> bool
      val escapeC: t -> t
      val escapeSML: t -> t
      val explode: t -> char list
      (* extract (s, i, SOME j)
       * returns the substring of s of length j starting at i.
       *)
      val extract: t * int * int option -> t
      val fields: t * (char -> bool) -> t list
      val fold: t * 'a * (char * 'a -> 'a) -> 'a
      val foldi: t * 'a * (int * char * 'a -> 'a) -> 'a
      val foreach: t * (char -> unit) -> unit
      val forall: t * (char -> bool) -> bool
      val fromCString: t -> t option
      val fromChar: char -> t
      val fromCharArray: CharArray.array -> t
      val fromString: t -> t option
      val hash: t -> Word.t
      val implode: char list -> t
      val isEmpty: t -> bool
      val isPrefix: {string: t, prefix: t} -> bool
      val isSubstring: {string: t, substring: t} -> bool
      val isSuffix: {string: t, suffix: t} -> bool
      val keepAll: t * (char -> bool) -> t
      val last: t -> char
      val layout: t -> Layout.t
      val length: t -> int
      val lparen: t (* ( *)
      val make: int * char -> t
      val max: t * t -> t
      val memoize: (t -> 'a) -> t -> 'a
      val memoizeList: (t -> 'a) * (t * 'a) list -> t -> 'a
      val min: t * t -> t
      val newline: t
      val output: t * TextIO.outstream -> unit
      val peek: t * (char -> bool) -> char option
      val peeki: t * (int * char -> bool) -> (int * char) option
      val posToLineCol: t -> int -> {line: int, col: int}
      val prefix: t * int -> t
      val removeTrailing: t * (char -> bool) -> t
      val rparen: t (* ) *)
      val size: t -> int
      (* splits the string into substrings broken at char,
       * e.g. split("foo$bar$baz", #"$") = ["foo", "bar", "baz"]
       *)
      val split: t * char -> t list
      val sub: t * int -> char
      (* beginning at start, with length chars *)
      val substring1: t * {start: int, length: int} -> t
      (* inclusive of start, exclusive of finish *)
      val substring2: t * {start: int, finish: int} -> t
      val substring: t * int * int -> t
      val suffix: t * int -> t
      val tabulate: int * (int -> char) -> t
      val toChar: t -> char
      val toLower: t -> t
      val toString: t -> t
      val toUpper: t -> t
      val tokens: t * (char -> bool) -> t list 
      val translate: t * (char -> t) -> t
   end


functor TestString (S: STRING): sig end =
struct

val _ = print "TestString\n"
   
open S

val _ =
   Assert.assert
   ("String", fn () =>
    dropl("abc", fn c => c = #"a") = "bc"
    andalso "\\000" = escapeC "\000"
    andalso "abc" = removeTrailing ("abc  ", Char.isSpace)
    andalso "" = removeTrailing ("   ", Char.isSpace)
    )

end

