(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature INLINE_STRUCTS = 
   sig
      include SHRINK
   end

signature INLINE = 
   sig
      include INLINE_STRUCTS
      
      val inline: Program.t -> Program.t
   end
