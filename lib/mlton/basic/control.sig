(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature CONTROL =
   sig
      val all: unit -> {name: string,
			value: string} list
      val control: {name: string,
		    default: 'a,
		    toString: 'a -> string} -> 'a ref
      val setDefaults: unit -> unit
   end
