(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ELABORATE_STRUCTS = 
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure TypeEnv: TYPE_ENV
      sharing Ast.Record = CoreML.Record
      sharing Ast.SortedRecord = CoreML.SortedRecord
      sharing Ast.Tyvar = CoreML.Tyvar
      sharing CoreML.Atoms = TypeEnv.Atoms
      sharing CoreML.Type = TypeEnv.Type
   end

signature ELABORATE = 
   sig
      include ELABORATE_STRUCTS

      structure Env: ELABORATE_ENV

      val elaborateMLB:
	 Ast.Basdec.t * {addPrim: Env.t -> CoreML.Dec.t list}
	 -> Env.t * (CoreML.Dec.t list * bool) vector
  end
