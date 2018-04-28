(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_SIGEXP_STRUCTS = 
   sig
      structure Ast: AST
      structure Env: ELABORATE_ENV
      sharing Ast = Env.Ast
   end

signature ELABORATE_SIGEXP = 
   sig
      include ELABORATE_SIGEXP_STRUCTS

      val elaborateSigexp:
         Ast.Sigexp.t * {env: Env.t, nest: string list}
         -> Env.Interface.t option
   end
