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
      val elaborateDec: Ast.Dec.t * string list * Env.t -> Decs.t
   end
