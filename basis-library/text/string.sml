(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure String: STRING_EXTRA =
   struct
      open String0

      structure Char = Char

      fun explode s =
	 let
	    fun loop (i, l) =
	       if i < 0 then l
	       else loop (i - 1, sub (s, i) :: l)
	 in loop (size s - 1, [])
	 end

      fun translate f s = concat (List.map f (explode s))

      fun isPrefix s s' =
	 let
	    val n = size s
	    val n' = size s'
	    fun loop i =
	       i >= n orelse (sub (s, i) = sub (s', i)
			      andalso loop (i + 1))
	 in n <= n' andalso loop 0
	 end

      local
	 fun make (tokens,name) p s =
	    case StringCvt.scanString (tokens p) s of
	       SOME l => List.map implode l
	     | NONE => raise Fail ("String." ^ name)
      in
	 val tokens = make (Reader.tokens, "tokens")
	 val fields = make (Reader.fields, "fields")
      end
   
      fun collate comp (s, s') =
	 let val n = size s
	    val n' = size s'
	    fun loop i =
	       if i >= n
		  then if i >= n'
			  then EQUAL
		       else LESS
	       else if i >= n'
		       then GREATER
		    else (case comp (sub (s, i), sub (s', i)) of
			     EQUAL => loop (i + 1)
			   | r => r)
	 in loop 0
	 end

      val compare = collate Char.compare

      val {<, <=, >, >=} = Util.makeOrder compare

      val toString = translate Char.toString
      val toCString = translate Char.toCString

      fun scanString scanChar (reader: (char, 'a) StringCvt.reader)
	: (string, 'a) StringCvt.reader =
	 fn state =>
	 case reader state of
	    NONE => SOME ("", state)
	  | _ =>
	       case Reader.list (scanChar reader) state of
		  NONE => NONE
		| SOME ([], _) => NONE
		| SOME (cs, state) => SOME (implode cs, state)

      val fromString = StringCvt.scanString (scanString Char.scan)
      val fromCString = StringCvt.scanString (scanString Char.scanC)

      fun nullTerm s = s ^ "\000"
   end

structure StringGlobal: STRING_GLOBAL = String
open StringGlobal
