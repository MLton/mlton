(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature ELABORATE_CORE_STRUCTS = 
   sig
      structure Ast: AST
      structure ConstType: CONST_TYPE
      structure CoreML: CORE_ML
      structure Decs: DECS
      structure Env: ELABORATE_ENV
      sharing Ast = Env.Ast
      sharing Ast.Tyvar = CoreML.Tyvar
      sharing CoreML = Decs.CoreML = Env.CoreML
   end

signature ELABORATE_CORE = 
   sig
      include ELABORATE_CORE_STRUCTS

      val allowRebindEquals: bool ref
      (* Elaborate dec in env, returning Core ML decs. *)
      val elaborateDec:
	 Ast.Dec.t * {env: Env.t,
		      lookupConstant: string * ConstType.t -> CoreML.Const.t,
		      nest: string list}
	 -> Decs.t
   end
