(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure FileDesc: FILE_DESC =
   struct
      open Posix.IO Posix.FileSys

      type t = file_desc

      val layout = Word.layout o fdToWord
         
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
   
