(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature CLEANER =
   sig
      structure UniqueId: UNIQUE_ID

      type t

      val add: t * UniqueId.id * (unit -> unit) -> unit
      val addNew: t * (unit -> unit) -> unit
      val atExit: t
      val atLoadWorld: t
      val clean: t -> unit
      val new: unit -> t
      val remove: t * UniqueId.id -> unit
   end
