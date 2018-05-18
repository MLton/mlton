(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Decs (S: DECS_STRUCTS): DECS =
struct

open S

structure Dec = CoreML.Dec

type dec = CoreML.Dec.t

open AppendList

type t = dec t

fun add (ds, d) = append (ds, single d)

fun layout ds =
   let
      open Layout
   in
      align (Vector.toListMap (toVector ds, Dec.layout))
   end

end
