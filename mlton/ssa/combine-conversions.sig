(* Copyright (C) 2009 Wesley W. Tersptra.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)


signature COMBINE_CONVERSIONS_STRUCTS = 
   sig
      include SHRINK
   end

signature COMBINE_CONVERSIONS = 
   sig
      include COMBINE_CONVERSIONS_STRUCTS

      val combine: Program.t -> Program.t
   end
