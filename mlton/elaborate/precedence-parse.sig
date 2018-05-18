(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRECEDENCE_PARSE_STRUCTS =
   sig
      structure Ast: AST
      structure Env: ELABORATE_ENV
      sharing Ast = Env.Ast
   end

signature PRECEDENCE_PARSE =
   sig
      include PRECEDENCE_PARSE_STRUCTS

      val parseClause:
         Ast.Pat.t vector * Env.t * (unit -> Layout.t)
         -> {args: Ast.Pat.t vector,
             func: Ast.Var.t}
      val parseExp: Ast.Exp.t vector * Env.t * (unit -> Layout.t) -> Ast.Exp.t
      val parsePat: Ast.Pat.t vector * Env.t * (unit -> Layout.t) -> Ast.Pat.t
   end
