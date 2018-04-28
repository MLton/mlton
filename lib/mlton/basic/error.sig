(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ERROR =
    sig
       val bug: string -> 'a
       val reraise: exn * {prefix: string, suffix: string} -> 'a
       val reraisePrefix: exn * string -> 'a
       val reraiseSuffix: exn * string -> 'a
       val unimplemented: string -> 'a
       val warning: string -> unit
   end
