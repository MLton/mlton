(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature PRECEDENCE_PARSE_STRUCTS =
   sig
      structure Ast: AST
      structure Env: ELABORATE_ENV
      sharing Ast = Env.Ast
   end

signature PRECEDENCE_PARSE =
   sig
      include PRECEDENCE_PARSE_STRUCTS

      val parseClause: Ast.Pat.t vector * Env.t -> {func: Ast.Var.t,
						    args: Ast.Pat.t vector}
      val parseExp: Ast.Exp.t vector * Env.t -> Ast.Exp.t
      val parsePat: Ast.Pat.t vector * Env.t -> Ast.Pat.t
   end
