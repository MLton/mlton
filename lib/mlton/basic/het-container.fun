(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ExnHetContainer():> HET_CONTAINER =
   struct
      type t = exn

      fun 'a new() =
	 let exception E of 'a
	 in {make = E,
	     pred = fn E _ => true | _ => false,
	     peek = fn E x => SOME x | _ => NONE}
	 end
   end

functor RefHetContainer():> HET_CONTAINER =
   struct
      type t = unit ref * (unit -> unit)

      fun 'a new() =
	 let
	    val id = ref()
	    val r: 'a option ref = ref NONE
	    fun make v = (id, fn () => r := SOME v)
	    fun peek ((id', f): t) =
	       if id = id' then (f(); !r before r := NONE)
	       else NONE
	    fun pred(id', _) = id = id'
	 in {make = make, pred = pred, peek = peek}
	 end
   end
