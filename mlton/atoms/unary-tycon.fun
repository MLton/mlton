(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor UnaryTycon(S: UNARY_TYCON_STRUCTS): UNARY_TYCON = 
struct

open S

datatype t = Ref | Array | Vector

val toTycon =
   fn Ref => Tycon.reff
    | Array => Tycon.array
    | Vector => Tycon.vector

val toString =
   fn Ref => "Ref"
    | Array => "Array"
    | Vector => "Vector"

val equals: t * t -> bool = op =

val layout = Layout.str o toString

end
