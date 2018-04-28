(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
