(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature RELATION =
   sig
      datatype t = datatype order (* from the pervasive environment *)

      val compare: ('a * 'a -> t)
	 -> {equals: 'a * 'a -> bool,
	     < : 'a * 'a -> bool,
	     > : 'a * 'a -> bool,
	     >= : 'a * 'a -> bool,
	     <= : 'a * 'a -> bool,
	     min: 'a * 'a -> 'a,
	     max: 'a * 'a -> 'a}
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val lexico: t * (unit -> t) -> t
      val lessEqual: {< : 'a * 'a -> bool,
		       equals: 'a * 'a -> bool}
	 -> {> : 'a * 'a -> bool,
	     >= : 'a * 'a -> bool,
	     <= : 'a * 'a -> bool,
	     min: 'a * 'a -> 'a,
	     max: 'a * 'a -> 'a,
	     compare: 'a * 'a -> t}
   end
