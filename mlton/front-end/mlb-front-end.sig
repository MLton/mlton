(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
	 
      val lexAndParseString: String.t -> Ast.Basdec.t * File.t vector
   end
