(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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

      val elaborateSigexp: Ast.Sigexp.t * Env.t -> Env.Interface.t option
   end
