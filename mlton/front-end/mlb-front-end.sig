(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLB_FRONT_END_STRUCTS = 
   sig
      structure Ast: AST
      structure FrontEnd: FRONT_END
      sharing Ast = FrontEnd.Ast
   end

signature MLB_FRONT_END = 
   sig
      include MLB_FRONT_END_STRUCTS

      val lexAndParseString: String.t -> Ast.Basdec.t 
   end
