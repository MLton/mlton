(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FILE =
   sig
      type t = string
      type dir = string
      type file = string

      val appendTo: t * string -> unit
      val base: t -> string
      val canRead: t -> bool
      val canRun: t -> bool
      val canWrite: t -> bool
      val concat: t list * t -> unit
      val contents: t -> string
      val copy: t * t -> unit
      val create: t -> unit (* make an empty file *)
      val dirOf: t -> dir
      val doesExist: t -> bool
      val ensureRead: t -> unit
      val ensureWrite: t -> unit
      val extension: t -> string option
      val fileOf: t -> file
      (* Each line includes the newline. *)
      val foldLines: t * 'a * (string * 'a -> 'a) -> 'a
      val isNewer: t * t -> bool
      val layout: t -> Layout.t
      (* Each line includes the newline. *)
      val lines: t -> string list
      val modTime: t -> Time.t
      val move: {from: t, to: t} -> unit
      val output: t * Out.t -> unit
      val outputContents: t * Out.t -> unit
      val remove: t -> unit
      val sameContents: t * t -> bool
      val size: t -> Position.int
      val suffix: t -> string option
      val temp: {prefix: string, suffix: string} -> t * Out.t
      val toString: t -> string
      val withAppend: t * (Out.t -> 'a) -> 'a
      val withIn: t * (In.t -> 'a) -> 'a
      val withOut: t * (Out.t -> 'a) -> 'a
      val withOutIn: (Out.t -> unit) * (In.t -> 'a) -> 'a
      val withString: string * (t -> 'a) -> 'a
      val withStringIn: string * (In.t -> 'a) -> 'a
      val withTemp: (t -> 'a) -> 'a
      val withTempOut: (Out.t -> unit) * (t -> 'a) -> 'a
      val withTempOut':
         {prefix: string, suffix: string} * (Out.t -> unit) * (t -> 'a) -> 'a
   end
