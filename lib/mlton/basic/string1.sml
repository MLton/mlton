(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure String1 =
struct

open String0

structure F = Fold (type 'a t = string
                    type 'a elt = char
                    val fold = fold)
open F

type t = string

val last = String0.last

val layout = Layout.str

fun forall (s, f) =
   let
      val n = length s
      fun loop i =
         i = n
         orelse (f (sub (s, i)) andalso loop (i + 1))
   in
      loop 0
   end

(* This hash function is taken from pages 56-57 of
 * The Practice of Programming by Kernighan and Pike.
 *)
fun hash (s: t): Word.t =
   fold (s, 0w0, fn (c, h) => Word.fromChar c + Word.* (h, 0w31))

fun peek (v, f) =
   let
      val n = length v
      fun loop i =
         if i = n
            then NONE
         else let
                 val x = sub (v, i)
              in
                 if f x
                    then SOME x
                 else loop (i + 1)
              end
   in
      loop 0
   end

fun peeki (v, f) =
   let
      val n = length v
      fun loop i =
         if i = n
            then NONE
         else let
                 val x = sub (v, i)
              in
                 if f (i, x)
                    then SOME (i, x)
                 else loop (i + 1)
              end
   in
      loop 0
   end

fun dropl (s, p) =
   case peeki (s, fn (_, c) => not (p c)) of
      NONE => ""
    | SOME (i, _) => extract (s, i, NONE)

fun deleteSurroundingWhitespace (s: t): t =
   let
      val n = size s
      fun loop (i: int) =
         if PInt.>= (i, n)
            then s
         else
            if Char.isSpace (sub (s, i))
               then loop (i + 1)
            else
               let
                  fun loop (j: int) =
                     let
                        val c = sub (s, j)
                     in
                        if PInt.<= (j, i)
                           then fromChar c
                        else
                           if Char.isSpace c
                              then loop (j - 1)
                           else extract (s, i, SOME (j - i + 1))
                     end
               in
                  loop (n - 1)
               end
   in loop 0
   end

end
