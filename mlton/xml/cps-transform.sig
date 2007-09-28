(* Copyright (C) 2007-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CPS_TRANSFORM_STRUCTS = 
   sig
      include SXML_TREE
   end

signature CPS_TRANSFORM = 
   sig
      include CPS_TRANSFORM_STRUCTS

      val doit: Program.t -> Program.t
   end
