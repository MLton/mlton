(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
