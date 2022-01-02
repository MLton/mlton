(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature COMMAND_LINE =
   sig
      structure Status:
         sig
            type t = OS.Process.status
         end

      val arguments: unit -> string list
      val make: (string * string list -> unit) -> (unit -> unit)
      val makeMain: (string * string list -> unit) -> (string * string list -> Status.t)
      val name: unit -> string
      val wrapMain: (string * string list -> Status.t) -> (unit -> 'a)
      val usage: {usage: string, msg: string} -> 'a
   end
