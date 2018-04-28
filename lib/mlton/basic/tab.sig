(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TAB =
   sig
      val make: Out.t * string -> {reset: unit -> unit,
                                    right: unit -> unit,
                                    left: unit -> unit,
                                    indent: unit -> unit}
   end
