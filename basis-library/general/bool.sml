(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Bool: BOOL =
   struct
      datatype bool = datatype bool

      val not = not

      val toString =
	 fn true => "true"
	  | false => "false"

      fun scan reader state =
	 case reader state of
	    NONE => NONE
	  | SOME(c, state) =>
	       case c of
		  #"f" => (case Reader.reader4 reader state of
			      SOME((#"a", #"l", #"s", #"e"), state) =>
				 SOME(false, state)
			    | _ => NONE)
		| #"t" => (case Reader.reader3 reader state of
			      SOME((#"r", #"u", #"e"), state) =>
				 SOME(true, state)
			    | _ => NONE)
		| _ => NONE
	       
      val fromString = StringCvt.scanString scan
   end

structure BoolGlobal: BOOL_GLOBAL = Bool
open BoolGlobal
