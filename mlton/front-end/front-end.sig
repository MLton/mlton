(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature FRONT_END_STRUCTS = 
   sig
      structure Ast: AST
   end

signature FRONT_END = 
   sig
      include FRONT_END_STRUCTS
	 
      val lexAndParse: In.t * Source.t -> Ast.Program.t
   end
