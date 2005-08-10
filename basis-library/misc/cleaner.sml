(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure Cleaner: CLEANER =
struct

type t = (unit -> unit) list ref

fun new (): t = ref []

fun addNew (cs, f) = cs := f :: (!cs)

fun clean cs = app (fn c => c () handle _ => ()) (!cs)
   
val atExit = new ()
   
val atLoadWorld = new ()

end
