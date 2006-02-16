(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature XML_SIMPLIFY_STRUCTS = 
   sig
      include XML_TREE
      val shrink: Program.t -> Program.t
      val typeCheck: Program.t -> unit
   end

signature XML_SIMPLIFY = 
   sig
      include XML_SIMPLIFY_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
