(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

type int = Int.t
   
signature ELABORATE_STRUCTS = 
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      sharing Ast = CoreML.Ast
   end

signature ELABORATE = 
   sig
      include ELABORATE_STRUCTS
      structure Decs: DECS
      structure Env: ELABORATE_ENV
	 
      val elaborateProgram: Ast.Program.t * Env.t -> Decs.t
   end
