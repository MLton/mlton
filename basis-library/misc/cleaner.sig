(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature CLEANER =
   sig
      type t

      val addNew: t * (unit -> unit) -> unit
      val atExit: t
      val atLoadWorld: t
      val clean: t -> unit
      val new: unit -> t
   end
