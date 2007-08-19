(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
