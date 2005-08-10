(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
signature POLYVARIANCE_STRUCTS = 
   sig
      include SHRINK
   end

signature POLYVARIANCE = 
   sig
      include POLYVARIANCE_STRUCTS
      
      val duplicate: Program.t -> Program.t
   end
