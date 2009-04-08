(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_CORE_STRUCTS = 
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

signature ELABORATE_CORE = 
   sig
      include ELABORATE_CORE_STRUCTS

      (* Elaborate dec in env, returning Core ML decs. *)
      val elaborateDec: Ast.Dec.t * {env: Env.t, nest: string list} -> Decs.t
      val reportSequenceNonUnit: unit -> unit
      val reportUndeterminedTypes: unit -> unit
   end
