(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature COMMON_SUBEXP_STRUCTS = 
   sig
      include SHRINK
   end

signature COMMON_SUBEXP = 
   sig
      include COMMON_SUBEXP_STRUCTS
      
      val eliminate: Program.t -> Program.t
   end
