(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FILE_DESC =
   sig
      type t = Posix.FileSys.file_desc

      val close: t -> unit
      val dup: t -> t
      val dup2: {old: t, new: t} -> unit
      val fluidLet: t * t * (unit -> 'a) -> 'a
      val layout: t -> Layout.t
      val move: {from: t, to: t} -> unit
      val pipe: unit -> {infd: t, outfd: t}
      val stderr: t
      val stdin: t
      val stdout: t
   end
