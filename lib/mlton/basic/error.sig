(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ERROR =
    sig
       val bug: string -> 'a
       val reraise: exn * string -> 'a
       val unimplemented: string -> 'a
       val warning: string -> unit
   end
