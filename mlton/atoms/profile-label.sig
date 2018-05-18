(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROFILE_LABEL_STRUCTS =
   sig
   end

signature PROFILE_LABEL =
   sig
      type t

      val clear: t -> unit
      val layout: t -> Layout.t
      val new: unit -> t
      val plist: t -> PropertyList.t
      val toString: t -> string
   end
