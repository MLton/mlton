(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SXML_SIMPLIFY_STRUCTS = 
   sig
      include SXML_TREE
      val shrink: Program.t -> Program.t
      val typeCheck: Program.t -> unit
   end

signature SXML_SIMPLIFY = 
   sig
      include SXML_SIMPLIFY_STRUCTS

      val simplify: Program.t -> Program.t
   end
