(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature C =
   sig
      (* C char* *)
      structure CS :
	 sig
	    type cs

	    val update: cs * int * char -> unit
	       
	    (* string must be null terminated *)
	    val length: cs -> int

	    (* string must be null terminated *)
	    val toString: cs -> string

	    (* string must be terminated by char *)
	    val extractToChar: cs * char -> string

	    (* extract first n characters of string *)
	    val toStringOfLength: cs * int -> string
	    val toCharArrayOfLength: cs * int -> char array
	    val toWord8ArrayOfLength: cs * int -> word8 array
	 end

      (* NULL terminated char** *)
      structure CSS :
	 sig
	    type css

	    val toList: css -> string list

	    (* extract first n strings from array *)
	    val toArrayOfLength: css * int -> string array

	    val fromList: string list -> string array
	 end
   end
