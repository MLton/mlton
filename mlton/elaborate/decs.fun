(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Decs (S: DECS_STRUCTS): DECS =
struct

open S

type dec = CoreML.Dec.t

open AppendList

type t = dec t

fun add (ds, d) = append (ds, single d)

val fromDec = single

fun toAsts ds =
   Vector.map (toVector ds, CoreML.Dec.toAst)

fun toAst ds = Ast.Dec.makeRegion (Ast.Dec.SeqDec (toAsts ds), Region.bogus)

fun layout ds =
   let
      open Layout
   in
      align (Vector.toListMap (toAsts ds, Ast.Dec.layout))
   end

end
