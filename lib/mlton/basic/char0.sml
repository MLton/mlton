(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Char0 : CHAR0 =
struct

structure Array = Pervasive.Array
structure Int = Pervasive.Int
structure String = Pervasive.String
open Pervasive.Char

type t = char

val dash = #"-"
val dquote = #"\""
val newline = #"\n"
val space = #" "
val toInt = ord
val fromInt = chr
val escapeSML = toString
val escapeC = toCString
val toString = String.str
val equals: t * t -> bool = op =
val toWord8 = Byte.charToByte
val fromWord8 = Byte.byteToChar

val {max, min, compare, ...} = Relation0.lessEqual {< = op <, equals = equals}

fun digitToInt (c: t): int option =
   if isDigit c
      then SOME (ord c - ord #"0")
   else NONE

fun fromDigit (d: int): t =
   if let open Int in 0 <= d andalso d < 10 end
      then chr (d + ord #"0")
   else Error.bug "Char0.fromDigit"

fun output (c, out) = TextIO.output (out, toString c)

val numChars = ord maxChar + 1

fun memoize (f: t -> 'a): t -> 'a =
   let val a = Array.tabulate (numChars, f o chr)
   in fn c => Array.sub (a, ord c)
   end

fun toHexDigit (c: t): int =
   if #"0" <= c andalso c <= #"9"
      then ord c - ord #"0"
   else if #"a" <= c andalso c <= #"f"
           then ord c - ord #"a" + 10
        else if #"A" <= c andalso c <= #"F"
                then ord c - ord #"A" + 10
             else Error.bug "Char0.charToHexDigit"

fun fromHexDigit (n: int): char = String.sub ("0123456789ABCDEF", n)

end
