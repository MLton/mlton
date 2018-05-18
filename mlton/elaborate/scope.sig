(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SCOPE_STRUCTS =
   sig
      structure Ast: AST
   end

signature SCOPE =
   sig
      include SCOPE_STRUCTS

      (* Add free type variables to the val or fun declaration where they are
       * implicitly scoped.
       *)
      val scope: Ast.Dec.t -> Ast.Dec.t
   end
