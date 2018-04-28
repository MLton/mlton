(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure String0 =
struct

structure PInt = Pervasive.Int
type int = PInt.int
structure PS = Pervasive.String   
local
   open PS
in
   val op ^ = op ^
   val concat = concat
   val concatWith = fn (ss,s) => concatWith s ss
   val escapeC = toCString
   val escapeSML = toString
   val explode = explode
   val extract = extract
   val fromCString = fromCString
   val fromString = fromString
   val implode = implode
   val maxLength = maxSize
   val size = size
   val sub = sub
   val substring = substring
end
structure Char = Char0

type t = string

val empty = ""

val dquote = "\""
val newline = "\n"
val lparen = "("
val rparen = ")"

val isEmpty =
   fn "" => true
    | _ => false

val length = size

fun last s = sub (s, length s - 1)

fun append (x, y) = x ^ y

fun toChar s = if length s = 1 then sub (s, 0) else Error.bug "String0.toChar"

val fromChar = str

fun contains (s, c) = Pervasive.Char.contains s c

val equals: t * t -> bool = op =

val {compare, min, max, ...} = Relation0.lessEqual {< = PS.<, equals = equals}

fun output (s, out) = Pervasive.TextIO.output (out, s)

val tabulate = CharVector.tabulate

fun make (n, c) = tabulate (n, fn _ => c)

fun substring1 (s, {start, length}) =
   substring (s, start, length)

fun substring2 (s, {start, finish}) =
   substring (s, start, finish-start)

fun prefix (s, len) =
   substring1 (s, {start = 0, length = len})

fun suffix (s, len) =
   substring1 (s, {start = length s - len,
                  length = len})

fun dropPrefix (s,n) =
   substring1 (s, {start=n, length = length s - n})
fun dropSuffix (s,n) =
   substring1 (s, {start=0, length = length s - n})

fun dropFirst s = dropPrefix (s, 1)
fun dropLast s = dropSuffix (s, 1)

fun dropPrefix (s, n) =
   substring2 (s, {start = n, finish = length s})

fun hasPrefix (string, {prefix}) = PS.isPrefix prefix string

fun removeTrailing (s: t, p: char -> bool): t =
   let
      fun loop (i: int) =
         if i < 0
            then i
         else if p (sub (s, i))
                 then loop (i - 1)
              else i
   in substring (s, 0, 1 + (loop (size s - 1)))
   end

fun hasSuffix (string, {suffix}) =
   let
      val n = length string
      val n' = length suffix
      fun loop (i: int, j: int): bool =
         i >= n orelse (Char.equals (sub (string, i), sub (suffix, j))
                        andalso loop (i + 1, j + 1))
   in n' <= n andalso loop (n - n', 0)
   end

fun findSubstring (string: t, {substring: t}) =
   let
      val n = length substring
      val maxIndex = length string - n
      fun loopString i =
         if i > maxIndex
            then NONE
         else
            let
               val start = i
               fun loopSubstring (i, j) =
                  if j >= n
                     then SOME start
                  else
                     if Char.equals (sub (string, i), sub (substring, j))
                        then loopSubstring (i + 1, j + 1)
                     else loopString (i + 1)
            in
               loopSubstring (i, 0)
            end
   in
      loopString 0
   end

val hasSubstring = isSome o findSubstring

fun baseName (x, y) =
   if hasSuffix (x, {suffix = y})
      then dropSuffix (x, size y)
   else Error.bug "String0.baseName"

fun fold (s, b, f) =
   let
      val n = size s
      fun loop (i, b) =
         if i >= n
            then b
         else loop (i + 1, f (sub (s, i), b))
   in loop (0, b)
   end

fun translate (s, f) = PS.translate f s

fun tokens (s, f) = PS.tokens f s
fun fields (s, f) = PS.fields f s

fun split (s, c) = fields (s, fn c' => c = c')

fun dropTrailing (s, c) =
   let
      val n = size s
      fun loop i =
         if PInt.< (i, 0) orelse c <> sub (s, i)
            then i
         else loop (i - 1)
   in dropSuffix (s, n - 1 - loop (n - 1))
   end

fun translateChar (s, f) = translate (s, fromChar o f)

fun toUpper s = translateChar (s, Char.toUpper)
fun toLower s = translateChar (s, Char.toLower)

fun sort (l, f) =
   let
      fun loop l =
         case l of
            [] => []
          | x :: l =>
               let
                  fun loop' l =
                     case l of
                        [] => [x]
                      | x' :: l => if f (x, x')
                                      then x :: x' :: l
                                   else x' :: loop' l
               in loop' (loop l)
               end
   in loop l
   end

fun alphabetize s = implode (sort (explode s, Char.<))

fun fromCharArray (a: CharArray.array): t =
   CharVector.tabulate (CharArray.length a, fn i => CharArray.sub (a, i))

fun toString s = s

fun a / b = concat [a, "/", b]

local
   open PS
in
   val op <= = op <=
   val op < = op <
   val op >= = op >=
   val op > = op >
end

fun rev (s: t): t =
   let
      val n = size s
      val n1 = n - 1
   in
      CharVector.tabulate (n, fn i => sub (s, n1 - i))
   end

val fromListRev = rev o implode

end
