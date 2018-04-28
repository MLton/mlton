(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Mark: MARK =
struct

datatype t =
   T of {string: string,
         pos: int}

fun pos (T {pos, ...}) = pos

local
   val numChars: int = 15
in
   fun layout (T {string, pos}) =
      String.layout (String.substring1 (string, {start = pos, length = numChars}))
end

fun length (T {string, ...}) = String.size string
fun isAtBeginning (m: t): bool = pos m = 0
fun isAtEnd (m: t): bool = pos m = length m - 1
fun diff (m: t, m': t): int = pos m - pos m'

fun fromString s = T {string = s, pos = 0}
fun fromFile f = fromString (File.contents f)

fun beginning (T {string, ...}) = T {string = string, pos = 0}

exception BackwardChars
fun backwardChars (T {string, pos},n) =
   let val pos = pos - n
   in if pos < 0 then raise BackwardChars
      else T {string = string, pos = pos}
   end
fun backwardChar m = backwardChars (m, 1)
val backwardChar = Trace.trace ("Mark.backwardChar", layout, layout) backwardChar

exception ForwardChars
fun forwardChars (T {string, pos},n) =
   let val pos = pos + n
   in if pos > String.size string then raise ForwardChars
      else T {string = string, pos = pos}
   end
fun forwardChar m = forwardChars (m, 1)
val forwardChar = Trace.trace ("Mark.forwardChar", layout, layout) forwardChar

fun charAt (T {string, pos}) = String.sub (string, pos)

fun lookingAtChar (m, c) = Char.equals (charAt m, c)

local
   fun searchChar move (m, c) =
      let
         fun loop m =
            if lookingAtChar (m, c) then m else loop (move m)
      in loop m
      end
in 
   val searchCharForward = searchChar forwardChar
   fun searchCharBackward (m, c) =
      searchChar backwardChar (backwardChar m, c)
end

fun bol m = forwardChar (searchCharBackward (m, #"\n"))
   handle _ => beginning m

fun eol m = searchCharForward (m, #"\n")

fun whatColumn m = diff (m, bol m)

fun numColumns m = diff (eol m, bol m)

local
   fun moveLines move =
      let
         fun moves (m as T {string, pos}, n: int) =
         let
            val c = whatColumn m
            fun loop (m, n) =
               if n <= 0 then forwardChars (m, Int.min (c, numColumns m))
               else loop (move m, n - 1)
         in loop (bol m, n)
         end
          fun move m = moves (m, 1)
      in (move, moves)
      end
in
   val (previousLine, previousLines) = moveLines (bol o backwardChar)
   val (nextLine, nextLines) = moveLines (forwardChar o eol)
end

fun lookingAtString (T {string, pos}, string') =
   let
      val len = String.size string
      val len' = String.size string'
      fun loop (pos, pos') =
         pos < len
         andalso (pos' >= len'
                  orelse (Char.equals (String.sub (string, pos),
                                       String.sub (string', pos'))
                          andalso loop (pos + 1, pos' + 1)))
   in loop (pos, 0)
   end
val lookingAtString =
   Trace.trace2 ("Mark.lookingAtString", layout, String.layout, Bool.layout)
   lookingAtString

exception Search
fun makeSearch move (m, s) =
   let
      fun search m =
         if lookingAtString (m, s)
            then forwardChars (m, String.size s)
         else (search (move m) handle _ => raise Search)
   in search m
   end

val search = makeSearch forwardChar
val searchBackward = makeSearch backwardChar

fun skip p =
   let fun skip m = if p (charAt m) then skip (forwardChar m)
                    else m
   in skip
   end

fun skipUntil p = skip (not o p)

val skipSpaces = skip Char.isSpace
val skipUntilSpace = skipUntil Char.isSpace

fun substring (T {string, pos}, T {pos=pos', ...}) =
   String.substring2 (string, {start=pos, finish=pos'})

fun num (fromString, exn) m =
   let val m = skipSpaces m
      val m' = skipUntilSpace m
   in case fromString (substring (m, m')) of
      NONE => raise exn
    | SOME n => (m',n)
   end

exception Int
val int = num (Int.fromString, Int)

exception Real
val real = num (Real.fromString, Real)

val real = Trace.trace ("Mark.real", layout, Layout.tuple2 (layout, Real.layout)) real

val op < = fn (m, m') => pos m < pos m'

val equals = fn (m, m') => pos m = pos m'

val {>, >=, <=, min, max, compare} =
   Relation.lessEqual {< = op <, equals = op =}

end
