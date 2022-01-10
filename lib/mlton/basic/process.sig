(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROCESS =
   sig
      val execute: string * string list -> unit
      val executeWith: string * string list * (In.t * Out.t -> 'a) -> 'a
      val executeWithIn: string * string list * (In.t -> 'a) -> 'a
      val executeWithOut: string * string list * (Out.t -> 'a) -> 'a
      (* Raise Fail exception. *)
      val fail: string -> 'a
      val spawn: {path: string, args: string list} -> Pid.t
      val spawne: {path: string, args: string list, env: string list} -> Pid.t
      val spawnp: {file: string, args: string list} -> Pid.t
      val succeed: unit -> 'a
      val system: string -> unit
      val time: (unit -> unit) -> {system: Time.t, user: Time.t}
      (* Wait for child process to finish.
       * Raise Fail if process terminates with nonzero status.
       *)
      val waitChildPid: Pid.t -> unit
      (* Wait for all Pid.ts in list to finish. *)
      val waits: Pid.t list -> unit
   end
