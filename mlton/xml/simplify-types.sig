(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature SIMPLIFY_TYPES_STRUCTS = 
   sig
      structure Input: XML_TREE
      structure Output: XML_TREE
      sharing Input.Atoms = Output.Atoms
   end

signature SIMPLIFY_TYPES = 
   sig
      include SIMPLIFY_TYPES_STRUCTS
      
      val simplifyTypes: Input.Program.t -> Output.Program.t
   end
