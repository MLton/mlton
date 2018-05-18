(* Copyright (C) 2009 Wesley W. Tersptra.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)


signature SSA2_TRANSFORM_STRUCTS = 
   sig
      include SHRINK2
   end

signature SSA2_TRANSFORM = 
   sig
      include SSA2_TRANSFORM_STRUCTS

      val transform2: Program.t -> Program.t
   end
