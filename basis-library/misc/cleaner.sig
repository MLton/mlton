(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature CLEANER =
   sig
      structure UniqueId: UNIQUE_ID

      type t

      val add: t * UniqueId.t * (unit -> unit) -> unit
      val addNew: t * (unit -> unit) -> unit
      val atExit: t
      val atLoadWorld: t
      val clean: t -> unit
      val new: unit -> t
      val remove: t * UniqueId.t -> unit
   end
