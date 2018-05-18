(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_MLBS_STRUCTS = 
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure Decs: DECS
      structure Env: ELABORATE_ENV
      sharing Ast = Env.Ast
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
