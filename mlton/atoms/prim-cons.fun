(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PrimCons (S: PRIM_CONS_STRUCTS): PRIM_CONS =
struct

open S

type con = t

val cons = fromString "::"
val falsee = fromString "false"
val nill = fromString "nil"
val reff = fromString "ref"
val truee = fromString "true"

(* exception constructors *)
val bind = fromString "Bind"
val match = fromString "Match"
val overflow = fromString "Overflow"

end
