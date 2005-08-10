(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
