(* Copyright (C) 2009 Wesley W. Tersptra.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)


signature SSA_TRANSFORM_STRUCTS = 
   sig
      include RESTORE
   end

signature SSA_TRANSFORM = 
   sig
      include SSA_TRANSFORM_STRUCTS

      val transform: Program.t -> Program.t
   end
