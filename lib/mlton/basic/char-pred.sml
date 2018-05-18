(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

fun contains s =
         let
            val a = Array.array(numChars, false)
            val n = String.size s
            fun loop i =
               if i >= n then ()
               else (Array.update(a, ord(String.sub(s, i)), true)
                     ; loop(i + 1))
         in loop 0
            ; fn c => Array.sub(a, ord c)
         end

      fun notContains s = not o (contains s)

      fun memoize (f: char -> 'a): char -> 'a =
         let val a = Array.tabulate(numChars, f o chr)
         in fn c => Array.sub(a, ord c)
         end

      local
         val not = fn f => memoize(not o f)
         infix or andd
         fun f or g = memoize(fn c => f c orelse g c)
         fun f andd g = memoize(fn c => f c andalso g c)
