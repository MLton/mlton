(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature FRONT_END_STRUCTS = 
   sig
      structure Ast: AST
   end

signature FRONT_END = 
   sig
      include FRONT_END_STRUCTS
	 
      val lexAndParseFile: File.t -> Ast.Program.t
   end
