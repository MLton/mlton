(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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

      val elaborateSigexp: Ast.Sigexp.t * Env.t -> Env.Interface.t
   end
