(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SIMPLIFY2_STRUCTS = 
   sig
      include SHRINK2
   end

signature SIMPLIFY2 = 
   sig
      include SIMPLIFY2_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
