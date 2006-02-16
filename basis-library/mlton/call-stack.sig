(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_CALL_STACK =
   sig
      type t

      val keep: bool
      val current: unit -> t
      val toStrings: t -> string list
   end
