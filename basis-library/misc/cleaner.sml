(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Cleaner: CLEANER =
struct

structure UniqueId = UniqueId()
structure Id = UniqueId

type t = (Id.id * (unit -> unit)) list ref

fun new (): t = ref []

fun add (cs, id, f) = cs := (id, f) :: (!cs)

fun addNew (cs, f) = add (cs, Id.new(), f)

fun remove (cs, id) =
   cs :=
   foldl (fn (c as (id', _), cs) => if Id.equals(id, id') then cs else c :: cs)
   [] (!cs)

fun clean cs = app (fn (_, c) => c ()) (!cs)
   
val atExit = new ()
val atLoadWorld = new ()
val atSaveWorld = new ()
   
end
