(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Cleaner(): CLEANER =
struct

structure UniqueId = UniqueId()
structure Id = UniqueId

val cleaners: (Id.id * (unit -> unit)) list ref = ref []

fun addCleaner(id, f) = cleaners := (id, f) :: (!cleaners)

fun addNewCleaner f = addCleaner(Id.new(), f)

fun removeCleaner id =
   cleaners :=
   foldl (fn (cleaner as (id', _), cleaners) =>
	  if Id.equals(id, id')
	     then cleaners
	  else cleaner :: cleaners)
   [] (!cleaners)

fun clean() = app (fn (_, cleaner) => cleaner()) (!cleaners)
   
end

structure AtExit = Cleaner()
structure AtSaveWorld = Cleaner()
structure AtLoadWorld = Cleaner()
