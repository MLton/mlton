(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Exn:> EXN =
struct

open Exn0

fun layout e =
   let
      open Layout
   in
      case e of
         OS.SysErr (s, _) => str s
       | Fail s => str s
       | IO.Io {cause, function, name, ...} =>
            seq [str (concat [function, " ", name, ": "]), layout cause]
       | _ => seq [str "unhandled exception: ", str (exnName e)]
   end

val toString = Layout.toString o layout

end
