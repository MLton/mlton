(* Copyright (C) 2007-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature XML_TRANSFORM_STRUCTS = 
   sig
      include SHRINK
   end

signature XML_TRANSFORM = 
   sig
      include XML_TRANSFORM_STRUCTS

      val transform: Program.t -> Program.t
   end
