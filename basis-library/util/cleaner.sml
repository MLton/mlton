(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
