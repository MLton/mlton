(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
