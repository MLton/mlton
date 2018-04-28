(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DIR =
   sig
      type t = string
      type file = string

      val cd: t -> unit
      val current: unit -> t
      val doesExist: t -> bool
      val inDir: t * (unit -> 'a) -> 'a
      val inTemp: (unit -> 'a) -> 'a
      val isDir: string -> bool
      val layout: t -> Layout.t
      val ls: t -> t list * file list
      val lsDirs: t -> t list
      val lsFiles: t -> file list
      val make: t -> unit
      val remove: t -> unit
      val removeR: t -> unit (* remove recursively, i.e. all contents *)
      val root: t
      val toString: t -> string
   end
