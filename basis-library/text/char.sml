(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Char: CHAR_EXTRA =
   struct
      open Char0
	 	       
      fun control reader state =
	 case reader state of
	    NONE => NONE
	  | SOME (c, state) =>
	       if #"@" <= c andalso c <= #"_"
		  then SOME (chr (ord c -? ord #"@"), state)
	       else NONE

      fun formatChar reader state =
	 case reader state of
	    NONE => NONE
	  | SOME (c, state) =>
	       if isSpace c
		  then SOME ((), state)
	       else NONE

      fun formatChars reader =
	 let
	    fun loop state =
	       case formatChar reader state of
		  NONE => state
		| SOME ((), state) => loop state
	 in
	    loop
	 end
		  
      val 'a formatSequences: (char, 'a) StringCvt.reader -> 'a -> 'a =
	 fn reader =>
	 let
	    fun loop state =
	       case reader state of
		  SOME (#"\\", state1) =>
		     (case formatChar reader state1 of
			 NONE => state
		       | SOME ((), state2) =>
			    let
			       val state3 = formatChars reader state2
			    in
			       case reader state3 of
				  SOME (#"\\", state4) => loop state4
				| _ => state
			    end)
		| _ => state
	 in
	    loop
	 end

      fun 'a scan (reader: (char, 'a) StringCvt.reader)
	: (char, 'a) StringCvt.reader =
	 let
	    val escape: (char, 'a) StringCvt.reader =
	       fn state =>
	       case reader state of
		  NONE => NONE
		| SOME (c, state') =>
		     let
			fun yes c = SOME (c, state')
		     in
			case c of
			   #"a" => yes #"\a"
			 | #"b" => yes #"\b"
			 | #"t" => yes #"\t"
			 | #"n" => yes #"\n"
			 | #"v" => yes #"\v"
			 | #"f" => yes #"\f"
			 | #"r" => yes #"\r"
			 | #"\\" => yes #"\\"
			 | #"\"" => yes #"\""
			 | #"^" => control reader state'
			 | #"u" =>
			      Reader.mapOpt chrOpt
			      (StringCvt.digitsExact (StringCvt.HEX, 4) reader)
			      state'
			 | _ => (* 3 decimal digits *)
			      Reader.mapOpt chrOpt
			      (StringCvt.digitsExact (StringCvt.DEC, 3)
			       reader)
			      state
		     end
	    val main: (char, 'a) StringCvt.reader =
	       fn state =>
	       let
		  val state = formatSequences reader state
	       in
		  case reader state of
		     NONE => NONE
		   | SOME (c, state) =>
			if isPrint c
			   then
			      case c of
				 #"\\" => escape state
			       | #"\"" => NONE
			       | _ => SOME (c, formatSequences reader state)
			else NONE
	       end
	 in
	    main
	 end

      val fromString = StringCvt.scanString scan

      fun 'a scanC (reader: (char, 'a) StringCvt.reader)
	: (char, 'a) StringCvt.reader =
	 let
	    val rec escape =
	       fn state =>
	       case reader state of
		  NONE => NONE
		| SOME (c, state') =>
		     let fun yes c = SOME (c, state')
		     in case c of
			#"a" => yes #"\a"
		      | #"b" => yes #"\b"
		      | #"t" => yes #"\t"
		      | #"n" => yes #"\n"
		      | #"v" => yes #"\v"
		      | #"f" => yes #"\f"
		      | #"r" => yes #"\r"
		      | #"?" => yes #"?"
		      | #"\\" => yes #"\\"
		      | #"\"" => yes #"\""
		      | #"'" => yes #"'"
		      | #"^" => control reader state'
		      | #"x" =>
			   Reader.mapOpt chrOpt
			   (StringCvt.digits StringCvt.HEX reader)
			   state'
		      | _ =>
			   Reader.mapOpt chrOpt
			   (StringCvt.digitsPlus (StringCvt.OCT, 3) reader)
			   state
		     end
	    and main =
	       fn NONE => NONE
		| SOME (c, state) =>
		     if isPrint c
			then
			   case c of
			      #"\\" => escape state
			    | _ => SOME (c, state)
		     else NONE
	 in
	    main o reader
	 end

      val fromCString = StringCvt.scanString scanC

      fun padLeft (s: string, n: int): string =
	 let
	    val m = String.size s
	    val diff = n -? m
	 in if Int.> (diff, 0)
	       then String.concat [String.new (diff, #"0"), s]
	    else if diff = 0
		    then s
		 else raise Fail "padLeft"
	 end

      val toString =
	 memoize
	 (fn c =>
	  if isPrint c
	     then
		(case c of
		    #"\\" => "\\\\"
		  | #"\"" => "\\\""
		  | _ => String0.str c)
	  else
	     case c of
		#"\a" => "\\a"
	      | #"\b" => "\\b"
	      | #"\t" => "\\t"
	      | #"\n" => "\\n"
	      | #"\v" => "\\v"
	      | #"\f" => "\\f"
	      | #"\r" => "\\r"
	      | _ =>
		   if c < #" "
		      then (String.concat
			    ["\\^", String0.str (chr (ord c +? ord #"@"))])
		   else String.concat 
		        ["\\", padLeft (Int.fmt StringCvt.DEC (ord c), 3)])
      
      val toCString =
	 memoize
	 (fn c =>
	  if isPrint c
	     then
		(case c of
		    #"\\" => "\\\\"
		  | #"\"" => "\\\""
		  | #"?" => "\\?"
		  | #"'" => "\\'"
		  | _ => String0.str c)
	  else
	     case c of
		#"\a" => "\\a"
	      | #"\b" => "\\b"
	      | #"\t" => "\\t"
	      | #"\n" => "\\n"
	      | #"\v" => "\\v"
	      | #"\f" => "\\f"
	      | #"\r" => "\\r"
	      | _ =>
		   String.concat
		   ["\\", padLeft (Int.fmt StringCvt.OCT (ord c), 3)])
   end

structure CharGlobal: CHAR_GLOBAL = Char
open CharGlobal

