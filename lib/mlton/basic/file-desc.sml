(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure FileDesc: FILE_DESC =
   struct
      open Posix.IO Posix.FileSys

      type t = file_desc

      val toString = SysWord.fmt StringCvt.DEC o fdToWord
      val layout = Layout.str o toString

      fun move {from, to} =
         if from <> to
            then (dup2 {old = from, new = to}
                  ; close from)
         else ()

      fun fluidLet (d1, d2, f) =
         let
            val copy = dup d1
            val _ = dup2 {old = d2, new = d1}
         in
            Exn.finally (f, fn () => move {from = copy, to = d1})
         end
   end
