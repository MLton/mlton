(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Char0 =
   struct
      open Primitive.Int Primitive.Char
	 
      type char = char
      type string = string

      val minChar = #"\000"
      val numChars: int = 256
      val maxOrd: int = 255
      val maxChar = #"\255"

      fun succ c =
	 if Primitive.safe andalso c = maxChar
	    then raise Chr
	 else Primitive.Char.chr (ord c + 1)

      fun pred c =
	 if Primitive.safe andalso c = minChar
	    then raise Chr
	 else Primitive.Char.chr (ord c - 1)

      fun chrOpt c =
	 if Primitive.safe andalso Primitive.Int.gtu (c, maxOrd)
	    then NONE
	 else SOME (Primitive.Char.chr c)

      fun chr c =
	 case chrOpt c of
	    NONE => raise Chr
	  | SOME c => c

      val {compare, ...} = Util.makeCompare (op <)

      structure String = String0

      fun oneOf s =
	 let
	    val a = Array.array (numChars, false)
	    val n = String.size s
	    fun loop i =
	       if Primitive.Int.>= (i, n) then ()
	       else (Array.update (a, ord (String.sub (s, i)), true)
		     ; loop (i + 1))
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
	 infix or andd
	 fun f or g = memoize (fn c => f c orelse g c)
	 fun f andd g = memoize (fn c => f c andalso g c)
      in
	 val isLower = oneOf "abcdefghijklmnopqrstuvwxyz"
	 val isUpper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	 val isDigit = oneOf "0123456789"
	 val isAlpha = isUpper or isLower
	 val isHexDigit = isDigit or (oneOf "abcdefABCDEF")
	 val isAlphaNum = isAlpha or isDigit
	 val isPrint = fn c => #" " <= c andalso c <= #"~"
	 val isSpace = oneOf " \t\r\n\v\f"
	 val isGraph = (not isSpace) andd isPrint
	 val isPunct = isGraph andd (not isAlphaNum)
	 val isCntrl = not isPrint
	 val isAscii = fn c => c < #"\128"
      end

      local
	 fun make (lower, upper, diff) =
	    memoize (fn c => if lower <= c andalso c <= upper
			       then chr (ord c +? diff)
			    else c)
	 val diff = ord #"A" - ord #"a"
      in
	 val toLower = make (#"A", #"Z", ~diff)
	 val toUpper = make (#"a", #"z", diff)
      end
   end

