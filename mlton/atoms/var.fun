(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Var(S: VAR_STRUCTS): VAR = 
struct

open S

structure V = HashId(structure AstId = AstId
		     val noname = "x")
open V
   
end
