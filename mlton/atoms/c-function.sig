(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
signature C_FUNCTION_STRUCTS = 
   sig
   end

signature C_FUNCTION = 
   sig
      include C_FUNCTION_STRUCTS

      structure Convention:
	 sig
	    datatype t = Cdecl | Stdcall

	    val layout: t -> Layout.t
	    val toString: t -> string
	 end

      datatype 'a t = T of {args: 'a vector,
			     (* bytesNeeded = SOME i means that the i'th
			      * argument to the function is a word that
			      * specifies the number of bytes that must be
			      * free in order for the C function to succeed.
			      * Limit check insertion is responsible for
			      * making sure that the bytesNeeded is available.
			      *)
			     bytesNeeded: int option,
			     convention: Convention.t,
			     ensuresBytesFree: bool,
			     mayGC: bool,
			     maySwitchThreads: bool,
			     modifiesFrontier: bool,
			     modifiesStackTop: bool,
			     name: string,
			     return: 'a}

      val args: 'a t -> 'a vector
      val bytesNeeded: 'a t -> int option
      val ensuresBytesFree: 'a t -> bool
      val equals: 'a t * 'a t -> bool
      val isOk: 'a t * {isUnit: 'a -> bool} -> bool
      val layout: 'a t * ('a -> Layout.t) -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      val mayGC: 'a t -> bool
      val maySwitchThreads: 'a t -> bool
      val modifiesFrontier: 'a t -> bool
      val modifiesStackTop: 'a t -> bool
      val name: 'a t -> string
      val return: 'a t -> 'a
      val vanilla: {args: 'a vector,
		    name: string,
		    return: 'a} -> 'a t
   end
