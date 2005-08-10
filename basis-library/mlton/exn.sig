(* Copyright (C) 2001-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_EXN =
   sig
      val addExnMessager: (exn -> string option) -> unit
      val history: exn -> string list
      val topLevelHandler: exn -> 'a (* does not return *)
   end
