(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature ELABORATE_MLBS_STRUCTS = 
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure Decs: DECS
      structure Env: ELABORATE_ENV
      sharing Ast = Env.Ast
      sharing Ast.Tyvar = CoreML.Tyvar
      sharing CoreML = Decs.CoreML = Env.CoreML
      sharing Decs = Env.Decs
   end

signature ELABORATE_MLBS = 
   sig
      include ELABORATE_MLBS_STRUCTS

      val elaborateMLB:
	 Ast.Basdec.t * {addPrim: Env.t -> CoreML.Dec.t list}
	 -> Env.t * (CoreML.Dec.t list * bool) vector
    end
