(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t

signature SCALE_STRUCTS =
   sig
   end

signature SCALE =
   sig
      include SCALE_STRUCTS
	 
      datatype t = One | Two | Four | Eight

      val fromInt: int -> t option
      val layout: t -> Layout.t
      val toString: t -> string
   end
