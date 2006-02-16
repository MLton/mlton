(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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

      val parseClause:
         Ast.Pat.t vector * Env.t * Region.t * (unit -> Layout.t)
         -> {args: Ast.Pat.t vector,
             func: Ast.Var.t}
      val parseExp: Ast.Exp.t vector * Env.t * (unit -> Layout.t) -> Ast.Exp.t
      val parsePat: Ast.Pat.t vector * Env.t * (unit -> Layout.t) -> Ast.Pat.t
   end
