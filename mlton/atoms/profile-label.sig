(* Copyright (C) 2004-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature PROFILE_LABEL_STRUCTS =
   sig
   end

signature PROFILE_LABEL =
   sig
      type t
	
      val clear: t -> unit
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val new: unit -> t
      val plist: t -> PropertyList.t
      val toString: t -> string
   end
