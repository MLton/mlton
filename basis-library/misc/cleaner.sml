(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Cleaner: CLEANER =
struct

structure UniqueId = UniqueId ()
structure Id = UniqueId

type t = (Id.t * (unit -> unit)) list ref

fun new (): t = ref []

fun add (cs, id, f) = cs := (id, f) :: (!cs)

fun addNew (cs, f) = add (cs, Id.new (), f)

fun remove (cs, id) =
   cs :=
   foldl (fn (c as (id', _), cs) => if Id.equals (id, id') then cs else c :: cs)
   [] (!cs)

fun clean cs =
   app (fn (_, c) => c () handle _ => ()) (!cs)
   
val atExit = new ()
val atLoadWorld = new ()

end

structure EmptyCleaner: CLEANER =
struct

structure UniqueId = UniqueId()
structure Id = UniqueId

type t = unit

fun new (): t = ()

fun add _ = ()
fun addNew _ = ()
fun remove _ = ()
fun clean _ = ()

val atExit = new ()
val atLoadWorld = new ()

end
