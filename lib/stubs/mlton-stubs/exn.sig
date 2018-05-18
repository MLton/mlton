(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_EXN =
   sig
      val addExnMessager: (exn -> string option) -> unit
      val history: exn -> string list

      val defaultTopLevelHandler: exn -> 'a (* does not return *)
      val getTopLevelHandler: unit -> (exn -> unit)
      val setTopLevelHandler: (exn -> unit) -> unit
      val topLevelHandler: exn -> 'a (* does not return *)
   end
