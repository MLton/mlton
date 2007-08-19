(* Copyright (C) 2001-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
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
