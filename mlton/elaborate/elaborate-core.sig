(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature ELABORATE_CORE_STRUCTS = 
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure Decs: DECS
      structure Env: ELABORATE_ENV
      sharing Ast = CoreML.Ast
      sharing CoreML = Decs.CoreML = Env.CoreML
   end

signature ELABORATE_CORE = 
   sig
      include ELABORATE_CORE_STRUCTS

      (* Elaborate dec in env, returning Core ML decs. *)
      val elaborateDec: Ast.Dec.t * Env.t -> Decs.t
   end
