(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Char0 =
   struct
      open Char
         
      type char = char
      type string = string

      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> char
             val fInt8 = Char.fromInt8Unsafe
             val fInt16 = Char.fromInt16Unsafe
             val fInt32 = Char.fromInt32Unsafe
             val fInt64 = Char.fromInt64Unsafe
             val fIntInf = Char.fromIntInfUnsafe)
      in
         val chrUnsafe = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = char -> 'a
             val fInt8 = Char.toInt8Unsafe
             val fInt16 = Char.toInt16Unsafe
             val fInt32 = Char.toInt32Unsafe
             val fInt64 = Char.toInt64Unsafe
             val fIntInf = Char.toIntInfUnsafe)
      in
         val ord = S.f
      end

      val minChar:char = #"\000"
      val numChars: int = 256
      val maxOrd: int = 255
      val maxChar:char = #"\255"

      fun succ c =
         if Primitive.Controls.safe andalso c = maxChar
            then raise Chr
         else chrUnsafe (Int.+ (ord c, 1))

      fun pred c =
         if Primitive.Controls.safe andalso c = minChar
            then raise Chr
         else chrUnsafe (Int.- (ord c, 1))

      fun chrOpt c =
         if Primitive.Controls.safe 
            andalso (Int.< (c, 0) orelse Int.> (c, maxOrd))
            (* andalso Int.gtu (c, maxOrd) *)
            then NONE
         else SOME (chrUnsafe c)

      fun chr c =
         case chrOpt c of
            NONE => raise Chr
          | SOME c => c

      structure String = String0

      fun oneOf s =
         let
            val a = Array.array (numChars, false)
            val n = String.size s
            fun loop i =
               if Int.>= (i, n) then ()
               else (Array.update (a, ord (String.sub (s, i)), true)
                     ; loop (Int.+ (i, 1)))
         in loop 0
            ; fn c => Array.sub (a, ord c)
         end
      val contains = oneOf

      fun notOneOf s = not o (oneOf s)
      val notContains = notOneOf

      fun memoize (f: char -> 'a): char -> 'a =
         let val a = Array.tabulate (numChars, f o chr)
         in fn c => Array.sub (a, ord c)
         end
         
      local
         val not = fn f => memoize (not o f)
         infix || &&
         fun f || g = memoize (fn c => f c orelse g c)
         fun f && g = memoize (fn c => f c andalso g c)
      in
         val isLower = oneOf "abcdefghijklmnopqrstuvwxyz"
         val isUpper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
         val isDigit = oneOf "0123456789"
         val isAlpha = isUpper || isLower
         val isHexDigit = isDigit || (oneOf "abcdefABCDEF")
         val isAlphaNum = isAlpha || isDigit
         val isPrint = fn c => #" " <= c andalso c <= #"~"
         val isSpace = oneOf " \t\r\n\v\f"
         val isGraph = (not isSpace) && isPrint
         val isPunct = isGraph && (not isAlphaNum)
         val isCntrl = not isPrint
         val isAscii = fn c => c < #"\128"
      end

      local
         fun make (lower, upper, diff) =
            memoize (fn c => if lower <= c andalso c <= upper
                               then chr (Int.+? (ord c, diff))
                            else c)
         val diff = Int.- (ord #"A", ord #"a")
      in
         val toLower = make (#"A", #"Z", Int.~ diff)
         val toUpper = make (#"a", #"z", diff)
      end
   end
